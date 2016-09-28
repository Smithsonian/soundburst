chai = require 'chai'
chai.use require 'chai-spies'
expect = chai.expect
SugarClient = require '../client'
MockPrimus = require './support/mock_primus'

SugarClient.host = 'sugar-api'

describe 'SugarClient', ->
  userClient = sessionClient = callback1 = callback2 = null

  setupEvents = ->
    callback1 = chai.spy()
    callback2 = chai.spy()
    userClient.events =
      event1: [callback1, callback2]
      event2: [callback2]

  originalConsole = console.info
  beforeEach ->
    console.info = ->
    SugarClient.Primus = MockPrimus
    for own key, _ of MockPrimus
      MockPrimus[key] = chai.spy MockPrimus[key]

    userClient = new SugarClient 'user', 'auth'
    sessionClient = new SugarClient()

  afterEach ->
    console.info = originalConsole

  describe 'initializing', ->
    it 'should set userId', ->
      expect(userClient.userId).to.eql 'user'

    it 'should set authToken', ->
      expect(userClient.authToken).to.eql 'auth'

    it 'should start without events', ->
      expect(sessionClient.events).to.eql { }

    it 'should start without subscriptions', ->
      expect(sessionClient.subscriptions).to.eql { }

    describe 'primus', ->
      it 'should connect', ->
        expect(MockPrimus.connect).to.have.been.called

      it 'should listen to outgoing::url', ->
        expect(MockPrimus.on).to.have.been.called.with 'outgoing::url', sessionClient.primusUrl

      it 'should listen to data', ->
        expect(MockPrimus.on).to.have.been.called.with 'data', sessionClient.receiveData

  describe '#primusUrl', ->
    describe 'when logged in', ->
      it 'should set the query string', ->
        baseUrl = { query: null }
        userClient.primusUrl baseUrl
        expect(baseUrl.query).to.eql 'user_id=user&auth_token=auth'

    describe 'when not logged in', ->
      it 'should not set the query string', ->
        baseUrl = { query: null }
        sessionClient.primusUrl baseUrl
        expect(baseUrl.query).to.eql null

  describe '#connect', ->
    it 'should disconnect', ->
      sessionClient.disconnect = chai.spy()
      sessionClient.connect()
      expect(sessionClient.disconnect).to.have.been.called()

    it 'open a new connection', ->
      sessionClient.connect()
      expect(MockPrimus.open).to.have.been.called()

  describe '#disconnect', ->
    it 'should remove invalid subscriptions', ->
      userClient.subscriptions['valid'] = true
      userClient.subscriptions['session:asdf'] = true
      userClient.subscriptions['user:not-me'] = true
      userClient.disconnect()
      expect(userClient.subscriptions).to.eql valid: true

    it 'should reset loggedIn', ->
      userClient.loggedIn = true
      userClient.disconnect()
      expect(userClient.loggedIn).to.eql null

    it 'should reset userKey', ->
      userClient.userKey = 'user:123'
      userClient.disconnect()
      expect(userClient.userKey).to.eql null

    it 'should end the connection', ->
      userClient.disconnect()
      expect(MockPrimus.end).to.have.been.called()

  describe '#receiveData', ->
    describe 'with data', ->
      it 'should emit', ->
        userClient.emit = chai.spy()
        userClient.receiveData foo: 'bar'
        expect(userClient.emit).to.have.been.called.with foo: 'bar'

    describe 'with a connection', ->
      beforeEach ->
        userClient.__subscribeToChannels = chai.spy()
        userClient.receiveData
          type: 'connection'
          loggedIn: 'state'
          userKey: 'key'

      it 'should set loggedIn', ->
        expect(userClient.loggedIn).to.eql 'state'

      it 'should set userKey', ->
        expect(userClient.userKey).to.eql 'key'

      it 'should add user subscription', ->
        expect(userClient.subscriptions).to.include key: true

      it 'should subscribe to channels', (done) ->
        setTimeout ->
          expect(userClient.__subscribeToChannels).to.have.been.called()
          done()
        , 100

  describe '#subscribeTo', ->
    beforeEach ->
      userClient.__subscribeTo = chai.spy()

    it 'should disallow duplicates', ->
      userClient.subscribeTo 'foo'
      userClient.subscribeTo 'foo'
      expect(userClient.__subscribeTo).to.have.been.called.once()

    it 'should store subscriptions', ->
      userClient.subscribeTo 'foo'
      expect(userClient.subscriptions).to.include foo: true

    it 'should send the subscription', ->
      userClient.subscribeTo 'foo'
      expect(userClient.__subscribeTo).to.have.been.called.with 'foo'

  describe '#unsubscribeFrom', ->
    it 'should disallow non-subscribed channels', ->
      userClient.unsubscribeFrom 'foo'
      expect(MockPrimus.write).to.not.have.been.called()

    it 'should remove subscriptions', ->
      userClient.subscriptions.foo = true
      userClient.unsubscribeFrom 'foo'
      expect(userClient.subscriptions).to.not.include foo: true

    it 'should send the unsubscribe', ->
      userClient.subscriptions.foo = true
      userClient.unsubscribeFrom 'foo'
      expect(MockPrimus.write).to.have.been.called.once.with
        action: 'Unsubscribe'
        params:
          channel: 'foo'

  describe '#on', ->
    it 'should add the event', ->
      fn = ->
      userClient.on 'foo', fn
      expect(userClient.events.foo).to.include fn

  describe '#off', ->
    beforeEach -> setupEvents()

    describe 'with a callback', ->
      it 'should remove the callback', ->
        userClient.off 'event1', callback1
        expect(userClient.events.event1).to.eql [callback2]

      it 'should not change other events', ->
        userClient.off 'event2', callback2
        expect(userClient.events.event1).to.eql [callback1, callback2]

    describe 'without a callback', ->
      it 'should remove the event', ->
        userClient.off 'event1'
        expect(userClient.events.event1).to.eql undefined

      it 'should not change other events', ->
        userClient.off 'event2'
        expect(userClient.events.event1).to.eql [callback1, callback2]

  describe '#emit', ->
    beforeEach -> setupEvents()

    it 'should call listeners', ->
      userClient.emit type: 'event1', works: true
      expect(callback1).to.have.been.called.once().with type: 'event1', works: true
      expect(callback2).to.have.been.called.once().with type: 'event1', works: true

    it 'should not call other listeners', ->
      userClient.emit type: 'event2', works: true
      expect(callback1).to.not.have.been.called()

  describe '#__subscribeToChannels', ->
    beforeEach ->
      userClient.subscriptions = { foo: true, bar: true }
      userClient.__subscribeTo = chai.spy()
      userClient.__subscribeToChannels()

    it 'should trigger all subscriptions', ->
      expect(userClient.__subscribeTo).to.have.been.called.with 'foo'
      expect(userClient.__subscribeTo).to.have.been.called.with 'bar'

  describe '#__subscribeTo', ->
    it 'should send the subscription', ->
      userClient.__subscribeTo 'foo'
      expect(MockPrimus.write).to.have.been.called.once.with
        action: 'Subscribe'
        params:
          channel: 'foo'

  describe '#createEvent', ->
    it 'should send the event', ->
      userClient.createEvent 'type', 'channel', works: true
      expect(MockPrimus.write).to.have.been.called.with
        action: 'Event'
        params:
          type: 'type'
          channel: 'channel'
          data: { works: true }
