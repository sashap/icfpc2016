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

var myTeamId = "103";

var args = {
  headers: headers 
};

var options = {
  url: 'snapshot/list',
};

var minDelay = 1000;

var maxNewProblemsToGet = 50;


var problemsDir = "data";
var problemsExt = ".in";

if (!fs.existsSync(problemsDir)){
  console.log("Creating ", problemsDir, " directory.");
  fs.mkdirSync(problemsDir);
}

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
      return request({ url: 'blob/' + problem.problem_spec_hash })
        .then(function(data){
        
          console.log("Saving problem to ", targetFile);      
          console.log("-------------------------------------------");
          console.log(data);
          console.log("-------------------------------------------");
          fs.writeFile(targetFile, data);
        })
        .delay(minDelay);
    }, 
    // Using Bluebird concurrency
    {concurrency: 1}
  );

};

// Get list of team problems
var filterTeamProblems = function(problems, teamId){
  return problems.filter(function(problem){
    return problem.owner === ''+teamId;
  });
};

// Perform analysis on problem effectiveness
var analyzeProblemStrength = function(problems, orderKey, orderDesc){

  orderKey = orderKey || "id";
  console.log("Running analysis on ", problems.length, "problems");

  return problems.map(function(problem){

    // s - the size of the solution that produced the problem
    // n - the number of the teams that submitted a perfect solution, plus 1. 
    // The problem-setting team receives (5000 - s) / n points
    // Each team that submitted a perfect solution gets s / n points. 
    // 
    // The scores for the teams with imperfect solutions are calculated such that 
    // the total points earned by all the imperfect solution teams is s / n, 
    // and each team's share is proportional to the resemblance of its solution.

    var totalSolutions = problem.ranking.length;
    var perfectSolutions = 0;
    var allResemblances = 0;
    var partialResemblanceSum = 0;
    var allResemblanceSum = 0;
    problem.ranking.forEach(function(solution){
      if(solution.resemblance >= 1.0) {
        perfectSolutions++;
      } else {
        partialResemblanceSum += solution.resemblance;
      }
      allResemblanceSum+= solution.resemblance;
    });
    var partialSolutions = totalSolutions - perfectSolutions;
    var s = problem.solution_size;
    var n = perfectSolutions + 1;
    var teamScore = (5000 - problem.solution_size) / n;
    var pefectSolutionScore = (s / n ) || 0;

    return {
      id: problem.problem_id,
      hash: problem.problem_spec_hash,
      teamScore: teamScore,
      perfectSolutions: perfectSolutions,
      partialSolutions: partialSolutions,
      perfectSolutionScore: pefectSolutionScore,
      solveRatio: (perfectSolutions / totalSolutions) || 0,
      averagePartialResemblance: (partialResemblanceSum / partialSolutions) || 0,
      averageResemblance: (allResemblanceSum / totalSolutions) || 0,
      s: s,
      n: n,
    };

  }).sort(function(a,b){
    if (orderDesc) {
      return b[orderKey] - a[orderKey]; 
    } else {
      return a[orderKey] - b[orderKey];
    };
    
  });

};


// getAllProblems().then(function(problems){
//   // Download and save new problems
//   downloadNewProblems(problems);
// });


getAllProblems().then(function(problems){
    
  var teamProblems = filterTeamProblems(problems, myTeamId);
  var analyzedProblems = analyzeProblemStrength(teamProblems, 'teamScore', true);

  console.log("Problems Analysis");
  console.log(analyzedProblems);

});
