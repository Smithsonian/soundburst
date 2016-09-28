JSONAPIClient = require('json-api-client')

PATH_TO_API_ROOT = 'https://panoptes.zooniverse.org/api'

DEFAULT_HEADERS = {
  'Content-Type': 'application/json',
  'Accept': 'application/vnd.api+json; version=1' }

client = new JSONAPIClient PATH_TO_API_ROOT, DEFAULT_HEADERS
console.log(client);
client.type('people').get('1').then (person) ->
