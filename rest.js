// REST API for ICFPC 2016

var fs = require("fs");
var Promise = require("bluebird");

var headers = {
  'X-API-Key': '103-7133f8e2759c5495a88472be2ff6f7c1',
  'Accept-Encoding': 'gzip',
  'Expect': ''
};

var request = require('request-promise').defaults({
  headers: headers,
  gzip: true,
  json: true,
  baseUrl: 'http://2016sv.icfpcontest.org/api/'
});


var args = {
  headers: headers 
};

var options = {
  url: 'snapshot/list',
};

var minDelay = 1000;

var maxProblemsToGet = 5;
var problemsDir = "data"

if (!fs.existsSync(problemsDir)){
  console.log("Creating ", problemsDir, " directory.");
  fs.mkdirSync(problemsDir);
}

var lastTimepoint = Date.now();

request(options)
.then(function(data){

  // console.log("Snapshot List: ", data);
  var latestSnapshot = data.snapshots.sort(function(a,b){
    // Needs to be negative if a < b, needs to be positive if a > b, 0 otherwise
    return b.snapshot_time - a.snapshot_time;
  })[0];
  console.log("Got latest snapshot with time :", latestSnapshot.snapshot_time, new Date(latestSnapshot.snapshot_time));
  return latestSnapshot;

})
.delay(minDelay)
.then(function(latestSnapshot){

  // Call blob snapshot 
  options.url = 'blob/' + latestSnapshot.snapshot_hash;
  return request(options);

})
.then(function(data){

  var problems = data.problems.slice(0,maxProblemsToGet);

  console.log("Found", problems.length, "problems.");

  return Promise.map(
    problems, 
    function(problem){

      var myDelay = minDelay - (Date.now() - lastTimepoint);
      lastTimepoint = Date.now();
      return request({ url: 'blob/' + problem.problem_spec_hash })
        .then(function(data){

          console.log("Saving problem to ", problemsDir + "/" + problem.problem_id + '.input');
          console.log("-------------------------------------------");
          console.log(data);
          console.log("-------------------------------------------");
          var targetFile = problemsDir + "/" + problem.problem_id + '.input';
          fs.writeFile(targetFile, data);
          console.log("Waiting for API to cool down:", myDelay);

        })
        .delay(myDelay);
    }, 
    // Using Bluebird concurrency
    {concurrency: 1}
  );

})
// Must be last thing in the chain - we want all subsequent call to fail if any of them fail
.catch(function(error){
  console.log("I caught a Pokemon.  No.  Error:", error.message);
})
.then(function(){
  console.log("We're finished!");
});


