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

var teamId = "103";

var args = {
  headers: headers 
};

var options = {
  url: 'snapshot/list',
};

var minDelay = 1500;

var maxNewProblemsToGet = 50;


var problemsDir = "data";
var problemsExt = ".in";

if (!fs.existsSync(problemsDir)){
  console.log("Creating ", problemsDir, " directory.");
  fs.mkdirSync(problemsDir);
}

var lastTimepoint = Date.now();


// Get list of team problems
var getTeamProblems = function(problems){
  return problems.filter(function(problem){
    problem.owner === teamId;
  });
};

// Perform analysis on problem effectiveness
var analyzeProblemStrength = function(problems){

  console.log("Following are very strong: ", problems);

};

// Get all latest problems available
var getAllProblems = function(){

  return request(options)
  .then(function(data){

    // console.log("Snapshot List: ", data);
    var latestSnapshot = data.snapshots.sort(function(a,b){
      // Needs to be negative if a < b, needs to be positive if a > b, 0 otherwise
      return b.snapshot_time - a.snapshot_time;
    })[0];
    console.log("Got latest snapshot",latestSnapshot.snapshot_hash, "with time :", latestSnapshot.snapshot_time, new Date(latestSnapshot.snapshot_time));
    return latestSnapshot;

  })
  .delay(minDelay)
  .then(function(latestSnapshot){

    // Call blob snapshot 
    options.url = 'blob/' + latestSnapshot.snapshot_hash;
    return request(options);

  })
  .then(function(data){
    return data.problems;
  });

};


var downloadNewProblems = function(allProblems, maxNewProblemsToGet){

  var problems = allProblems
    // Get all problems which do not exist yet
    .filter(function(problem){
      return !fs.existsSync(problemsDir + "/" + problem.problem_id + problemsExt);
    })
    // Set max problems to get
    .slice(0, maxNewProblemsToGet || allProblems.length);


  console.log("Found", problems.length, "new problems and", allProblems.length - problems.length, " existing problems.");

  return Promise.map(
    problems, 
    function(problem){

      var targetFile = problemsDir + "/" + problem.problem_id + problemsExt;
      var myDelay = 0;
      lastTimepoint = Date.now();
      return request({ url: 'blob/' + problem.problem_spec_hash })
        .then(function(data){

          console.log("Saving problem to ", targetFile);      
          console.log("-------------------------------------------");
          console.log(data);
          console.log("-------------------------------------------");
          fs.writeFile(targetFile, data);
          myDelay = minDelay - (Date.now() - lastTimepoint);
          lastTimepoint = Date.now();
          console.log("Waiting for API to cool down:", myDelay);
        })
        .delay(myDelay);
    }, 
    // Using Bluebird concurrency
    {concurrency: 1}
  );

};



// var teamProblems = getTeamProblems(data.problems);
// analyzeProblemStrength(teamProblems);


getAllProblems()
.then(function(problems){
  downloadNewProblems(problems);
})

