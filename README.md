ICFPC 2016 - Team Hydralisk eats taco
=====================================


origami (ocaml)
---------------

To build origami.native

    brew install opam / apt install opam
    opam install zarith extlib cairo2 vg ocurl oasis
    make

Commands

    ./origami.native get_tasks - refresh available tasks
    ./origami.native submit - submit data/*.out
    ./origami.native submit - submit data/*.out
    ./origami.native gen_folds - generates folds based on manually entered edges

to renegerate data/*.out files

    make redo


rest.js (javascript)
--------------------

To configure first run

    npm install

Commands and options for ./rest.js

    Usage: rest.js <command> [options]

    Commands:
      download  download new problems into data directory
      solved    run statistics on solved problems
      created   run statistics on created problems

    Options:
      --api-url            ICFPC API URL
                                     [default: "http://2016sv.icfpcontest.org/api/"]
      --api-key            ICFPC Team API Key
                                   [default: "103-7133f8e2759c5495a88472be2ff6f7c1"]
      --team-id            ICFPC Team ID                              [default: 103]
      --data-dir           All downloaded and processed data directory
                                                                   [default: "data"]
      --problems-dir       Team problems directory             [default: "problems"]
      --max-problems       Maximum problems to download              [default: 1000]
      --api-delay          Minimum amount of delay between API calls [default: 1000]
      --sort-key           Default sort key                   [default: "teamScore"]
      --desc               Sort in descending order       [boolean] [default: false]
      --full, --fullStats  Show full statistics           [boolean] [default: false]
      --json               Show statistics in JSON format [boolean] [default: false]
      --rounded            Show numbers rounded to N decimal places     [default: 2]
      -h, --help           Show help                                       [boolean]

    Examples:
      rest.js solved --desc --full --sort-key   run analysis of our submitted
      teamScore                                 problems and sort results by team
                                                score in descending order




files
-----


Files in the data folder are created using origami or rest.js API calls.

* N.in - problem text
* N.in.png - problem picture
* N.out - proposed solution
* N.sent - submitted solution
* N.result - received answer to solution
* N.done - perfect score marker


