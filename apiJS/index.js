// apiClient = require('panoptes-client/lib/api-client');
// auth = require('panoptes-client/lib/auth');
// oauth = require('panoptes-client/lib/oauth');
// talkClient = require('panoptes-client/lib/talk-client');
//
// // Retrieve a Resource by ID
// apiClient.type('subjects').get('3523104')
//     .then(function (subject) {
//         console.log(subject);
//     });
//
//     var foo = apiClient.type('subject_sets').get('1').then(function (response) {
//       console.log(response);
//     });
//     // Retrieve a Resource by ID, skipping local cache
//     // (Any request with query params is passed to the server.)
//     apiClient.type('subjects').get('5967', {})
//         .then(function (subjects) {
//             console.log(subjects);
//         });

var request = require('request');

request({
  method: 'GET',
  url: 'https://panoptes-staging.zooniverse.org/api/projects/3159',
  headers: {
    'Accept': 'application/vnd.api+json; version=1',
    'Content-Type': 'application/json'
  }}, function (error, response, body) {
  console.log('Status:', response.statusCode);
  console.log('Headers:', JSON.stringify(response.headers));
  console.log('Response:', body);
});
