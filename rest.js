#!/usr/bin/env node

// ICFPC 2016 API Tools
// Authors:
// Sasha Parfenov
// Peter Yao

require('console.table');
var glob = require('glob-fs')({ gitignore: true });
var path = require('path');
var fs = require("fs");
var Promise = require("bluebird");

// Arguments processing
var argv = require('yargs')
  .usage('Usage: $0 <command> [options]')
  .example('$0 solved --desc --full --sort-key teamScore', 'run analysis of our submitted problems and sort results by team score in descending order')
  .demand(1)
  .command('download', 'download new problems into data directory')
  .command('solved', 'run statistics on solved problems')
  .command('created', 'run statistics on created problems')
  .option('api-url', {
    default: 'http://2016sv.icfpcontest.org/api/',
    describe: 'ICFPC API URL',
  })  
  .option('api-key', {
    default: '103-7133f8e2759c5495a88472be2ff6f7c1',
    describe: 'ICFPC Team API Key',
  })  
  .option('team-id', {
    default: 103,
    describe: 'ICFPC Team ID',
  })
  .option('data-dir', {
    default: 'data',
    describe: 'All downloaded and processed data directory',
  })
  .option('problems-dir', {
    default: 'problems',
    describe: 'Team problems directory',
  })
  .option('max-problems', {
    default: 1000,
    describe: 'Maximum problems to download',
  })
  .option('api-delay', {
    default: 1000,
    describe: 'Minimum amount of delay between API calls',
  })
  .option('sort-key', {
    describe: 'Default sort key',
    default: 'teamScore',
  })
  .option('desc', {
    default: false,
    type: 'boolean',
    describe: 'Sort in descending order',
  })
  .option('full', {
    alias: 'fullStats',
    default: false,
    type: 'boolean',
    describe: 'Show full statistics',
  })
  .option('json', {
    default: false,
    type: 'boolean',
    describe: 'Show statistics in JSON format',
  })
  .option('rounded', {
    default: 2,
    describe: 'Show numbers rounded to N decimal places',
  })
  .count('v')
  .alias('v', 'verbose')
  .help('h')
  .alias('h', 'help')
  .argv;
 
var VERBOSE_LEVEL = argv.verbose;
 
function INFO()  { VERBOSE_LEVEL >= 1 && console.log.apply(console, arguments); }
function DEBUG() { VERBOSE_LEVEL >= 2 && console.log.apply(console, arguments); }

DEBUG("Arguments (argv)", argv);


var headers = {
  'X-API-Key': argv.apiKey,
  'Accept-Encoding': 'gzip',
  'Expect': ''
};

var request = require('request-promise').defaults({
  headers: headers,
  gzip: true,
  json: true,
  baseUrl: argv.apiUrl
});

var myTeamId = argv.teamId;



var args = {
  headers: headers 
};

var options = {
  url: 'snapshot/list',
};

var apiDelay = argv.apiDelay;



var maxNewProblemsToGet = argv.maxProblems;

var dataDir = argv.dataDir;
var problemsExt = ".in";

if (!fs.existsSync(dataDir)){
  INFO("Creating ", dataDir, " directory.");
  fs.mkdirSync(dataDir);
}

// Get all latest problems available
var getAllProblems = function(){

  INFO("Getting all problems...");
  return request(options)
  .then(function(data){

    // console.log("Snapshot List: ", data);
    var latestSnapshot = data.snapshots.sort(function(a,b){
      // Needs to be negative if a < b, needs to be positive if a > b, 0 otherwise
      return b.snapshot_time - a.snapshot_time;
    })[0];
    INFO("Got latest snapshot",latestSnapshot.snapshot_hash, "with time :", latestSnapshot.snapshot_time, new Date(latestSnapshot.snapshot_time));
    return latestSnapshot;

  })
  .delay(apiDelay)
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
      return !fs.existsSync(dataDir + "/" + problem.problem_id + problemsExt);
    })
    // Set max problems to get
    .slice(0, maxNewProblemsToGet || allProblems.length);


  console.log("Found", problems.length, "new problems and", allProblems.length - problems.length, " existing problems.");

  return Promise.map(
    problems, 
    function(problem){

      var targetFile = dataDir + "/" + problem.problem_id + problemsExt;
      return request({ url: 'blob/' + problem.problem_spec_hash })
        .then(function(data){
        
          console.log("Saving problem to ", targetFile);      
          console.log("-------------------------------------------");
          console.log(data);
          console.log("-------------------------------------------");
          fs.writeFile(targetFile, data);
        })
        .delay(apiDelay);
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
var analyzeProblemStrength = function(problems, orderKey, orderDesc, fullStats){

  orderKey = orderKey || "id";
  INFO("Running analysis on ", problems.length, "problems");

  var problemToClassMap = getProblemToClassMap(argv.problemsDir);


  return problems.map(function(problem){

    // s - the size of the solution that produced the problem
    // n - the number of the teams that submitted a perfect solution, plus 1. 
    // The problem-setting team receives (5000 - s) / n points
    // Each team that submitted a perfect solution gets s / n points. 
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

    var result = {
      id: problem.problem_id,
      class: problemToClassMap[problem.problem_id],
      teamScore: teamScore.toFixed(argv.rounded),
      solutionSize: s,
      perfectSolutions: perfectSolutions,
      partialSolutions: partialSolutions,
    };

    if (fullStats) {
      result.perfectSolutionScore = pefectSolutionScore.toFixed(argv.rounded);
      result.solveRatio = ((perfectSolutions / totalSolutions) || 0).toFixed(argv.rounded);
      result.averagePartialResemblance = ((partialResemblanceSum / partialSolutions) || 0).toFixed(argv.rounded);
      result.partialResemblanceSum = partialResemblanceSum;
      result.averageResemblance = ((allResemblanceSum / totalSolutions) || 0).toFixed(argv.rounded);
      result.s = s;
      result.n = n;
      result.hash = problem.problem_spec_hash;
    }
    return result;

  }).sort(function(a,b){
    if (orderDesc) {
      return b[orderKey] - a[orderKey]; 
    } else {
      return a[orderKey] - b[orderKey];
    };
    
  });

};

// Creates a map between problem id and a type/class of problem it is
var getProblemToClassMap = function(sourceDir){

  var resultExt = 'result';
  var classExt = 'class'

  var resultFiles = glob.readdirSync(sourceDir + '/*.' + resultExt);
  var classFile;

  var results = {};
  resultFiles.forEach(function(resultFile){

    classFile = path.dirname(resultFile) + '/' +  path.basename(resultFile, resultExt) + classExt;

    try {
      resultFileContents = JSON.parse(fs.readFileSync(resultFile, "utf-8"));
      classFileContents = fs.readFileSync(classFile, "utf-8");
      if (resultFileContents.problem_id && classFileContents) {
        results[resultFileContents.problem_id] = classFileContents;
      }
    } catch(error){
      console.log("Failed to read ", resultFile, "and", classFile);
    }

  });
  return results;


};


// Get solution statistics
var getSolvedProblemsStats = function(problems, orderKey, orderDesc, fullStats) {

  orderKey = orderKey || 'bestResult';
  var resultExt = 'result';
  var bestExt = 'best'

  var problemsStats = analyzeProblemStrength(problems, 'id', false, true);

  return problemsStats.map(function(ps){

    var bestResult = 0;
    var bestFile = path.join(dataDir, ps.id + '.' + bestExt);
    var resultFile = path.join(dataDir, ps.id + '.' + resultExt);
    try{
      if(fs.existsSync(bestFile)){
        bestResult = JSON.parse(fs.readFileSync(bestFile, "utf-8")).resemblance;
      } else {
        bestResult = JSON.parse(fs.readFileSync(resultFile, "utf-8")).resemblance;
      }
    } catch(error){
      DEBUG("Unable to parse ", resultFile, "error:", error);
    }
    
    // Each team that submitted a perfect solution gets s / n points. 
    // The scores for the teams with imperfect solutions are calculated such that 
    // the total points earned by all the imperfect solution teams is s / n, 
    // and each team's share is proportional to the resemblance of its solution.
    
    var teamScore = 0;
    if (bestResult >= 1.0) {
      teamScore = (ps.s / ps.n) || 0;
    } else {
      teamScore = ((ps.s / ps.n) * (bestResult / ps.partialResemblanceSum)) || 0;
    }

    var result = {
      id: ps.id,
      bestResult: bestResult.toFixed(argv.rounded),
      teamScore: teamScore.toFixed(argv.rounded),
      perfectSolutions: ps.perfectSolutions,
      partialSolutions: ps.partialSolutions,
    }

    if (fullStats){
      result.solveRatio = ps.solveRatio;
      result.averagePartialResemblance = ps.averagePartialResemblance;
      result.averageResemblance = ps.averageResemblance;
    }

    return result;

  })
  .sort(function(a,b){
    if (orderDesc) {
      return b[orderKey] - a[orderKey]; 
    } else {
      return a[orderKey] - b[orderKey];
    };
    
  });  ;

};

switch(argv._[0]) {
  case "download":

    getAllProblems().then(function(problems){
      // Download and save new problems
      downloadNewProblems(problems);
    });

    break;

  case "created":

    getAllProblems().then(function(problems){        
      var teamProblems = filterTeamProblems(problems, myTeamId);
      var analyzedProblems = analyzeProblemStrength(teamProblems, argv.sortKey, argv.desc, argv.fullStats);
      INFO("Problems Created by Team ", myTeamId);
      if (argv.json) {
        console.log(analyzedProblems);  
      } else {
        console.table(analyzedProblems);  
      }      
    });

    break;

  case "solved":

    getAllProblems().then(function(problems){
      var solvedProblems = getSolvedProblemsStats(problems);
      INFO("Problems Solved by Team ", myTeamId);
      if (argv.json) {
        console.log(solvedProblems);  
      } else {
        console.table(solvedProblems);  
      }
    });

    break;

  default:
    console.log("Option", argv._[0], " not found  See help with --help.");

}



