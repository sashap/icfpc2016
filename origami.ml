open ExtLib
open Prelude

open Printf
open Otypes

let get_problem n = Problem.input (sprintf "data/%d.in" n)
let save_solution n x = Std.output_file ~filename:(sprintf "data/%d.out" n) ~text:(Solution.show x)

let gen_folds () =
  let flag = ref true in
  let poly = ref orig in
  while !flag do
    printf "Edge to fold over : ";
    let edge = Line.of_string (read_line ()) in
    poly := Ops.Fold.do_fold !poly edge;
    Render.render_poly !poly "folds.png";
    printf "continue? [1/0] : ";
    flag := (read_int ()) = 1
  done;
  print_string @@ (Solution.show @@ Ops.Fold.build_solution ())

let () =
  Random.self_init ();
  match List.tl @@ Array.to_list Sys.argv with
  | "render"::files ->
    let p = ref None in
    let s = ref None in
    let output = ref None in
    let last = ref "/dev/null" in
    files |> List.iter begin fun f ->
      last := f ^ ".png";
      if String.ends_with f ".out" then (assert (!s = None); s := Some (Solution.input f)) else
      if String.ends_with f ".in" then (assert (!p = None); p := Some (Problem.input f)) else
      if String.ends_with f ".png" then (assert (!output = None); output := Some f) else
      fail "don't know what to do with %S" f
    end;
    let output = Option.default !last !output in
    eprintfn "output to %s" output;
    Render.render ?p:!p ?s:!s output
  | "draw"::ns ->
    ns |> List.iter (fun n ->
    let p = sprintf "data/%s.in" n in
    let p = if Sys.file_exists p then Some (Problem.input p) else None in
    let s = sprintf "data/%s.out" n in
    let s = if Sys.file_exists s then Some (Solution.input s) else None in
    let out = sprintf "data/%s.png" n in
    eprintfn "output to %s" out;
    Render.render ?p ?s out)
  | "classify"::files ->
    files |> List.iter begin fun file ->
      match String.ends_with file ".in" with
      | false -> eprintfn "skipping %s" file
      | true ->
      let perfect = Sys.file_exists (sprintf "%s.perfect_score" @@ String.slice ~last:(-3) file) in
      let p = Problem.input file in
      let r = match p.Problem.shape with
      | [] -> "empty"
      | _::_::_ -> "multiple polygons (holes?)"
      | [x] ->
        let cls =
          match List.length x with
          | 3 ->
            let edges = Poly.edges x in
            if (edges |> List.filter (fun l -> R.eq (Line.length2 l) R.one) |> List.length) = 2 then "origin triangle" else "triangle"
          | 4 ->
            let edges = Poly.edges x in
            let n = edges |> List.filter (fun l -> R.eq (Line.length2 l) R.one) |> List.length in
            if n = 4 then "origin square" else if n >= 2 then "origin quadrangle" else "quadrangle"
          | n -> sprintf "%d vertices" n
        in
        cls
      in
      printfn "%s %s %s" file r (if perfect then "perfect" else "")
    end
  | "bb"::file::[] ->
    let p = Problem.input file in
    let (lo,hi) = Ops.bounding_box p.shape in
    printfn "%s %s" (Pt.show lo) (Pt.show hi)
  | "solve"::"box"::num::a::b::[] ->
    let n = int_of_string num in
    let p = get_problem n in
    let solution = Ops.solve_in_box num p.shape (Pt.of_string a, Pt.of_string b) in
    save_solution n solution
  | "solve"::meth::files ->
    files |> List.iter begin fun file ->
      let p,save =
        match int_of_string file with
        | n -> get_problem n, save_solution n
        | exception _ -> Problem.input file, (fun x -> print_string @@ Solution.show x)
      in
      let solution =
        match meth with
        | "bb" -> Ops.solve_bb file p.shape
        | "best_bb" -> Ops.solve_best_bb file p.shape
        | "single_facet" -> Ops.solve_single_facet file p
        | _ -> assert false
      in
      save solution
    end
  | "rotate"::file::center::angle::[] ->
    let angle = float_of_string angle in
    let sol = Solution.input file in
    print_string @@ Solution.show {sol with dst = Array.map (Pt.rotate (Pt.of_string center) angle) sol.dst}
  | "mirror"::file::a::b::[] ->
    let line = Pt.of_string a, Pt.of_string b in
    let sol = Solution.input file in
    print_string @@ Solution.show {sol with dst = Array.map (Ops.mirror line) sol.dst}
  | "gen"::meth::mirrors::stripes::[] ->
    let meth = match meth with
    | "rect" -> Gen.gen1
    | "v" -> Gen.gen_v
    | "t" -> Gen.gen_t
    | "cross" -> Gen.gen_cross
    | _ -> assert false
    in
    let mirrors = int_of_string mirrors in
    let stripes = int_of_string stripes in
    let s = Gen.random_mirrors mirrors (meth stripes) in
    Render.render ~s "test.png";
    print_string @@ Solution.show s
  | "hello"::[] -> printfn "%s" (Api.get "hello")
  | "get_tasks"::[] -> Api.get_all_tasks ()
  | "submit_s"::[] -> Api.submit_all_solutions ()
  | "submit_s"::l -> Api.submit_solutions l
  | "submit_p"::[] -> Api.submit_problems ()
  | "intersect_edges"::a::b::out::[] ->
    let a = Problem.input a in
    let b = Problem.input b in
    begin match Ops.intersect_edges (List.hd a.shape) (List.hd b.shape) with
    | `Edges e ->
      List.iter (fun x -> printfn "%s" (Line.show x)) e;
      with_open_out_bin out @@ Render.do_render ~skel:e
    | `Outer p ->
      printfn "contains";
      with_open_out_bin out @@ Render.do_render ~shape:[p]
    end
  | "union"::a::b::out::[] ->
    let a = Problem.input a in
    let b = Problem.input b in
    let p = Ops.union (List.hd a.shape) (List.hd b.shape) in
    print_endline @@ Poly.show p;
    with_open_out_bin out @@ Render.do_render ~shape:[p]
  | "is_inside"::file::pt::[] ->
    let p = Problem.input file in
    List.map (Ops.is_inside (Pt.of_string pt)) p.shape |> List.iter (printfn "%B")
  | "resemble"::a::b::[] ->
    let a = Problem.input a in
    let b = Problem.input b in
    printfn "%g" (Ops.resemble a.shape b.shape)
  | "blob"::hash::[] -> print_string @@ Api.get_blob hash
  | "gen_folds"::[] -> gen_folds ()
  | _ -> fail "wat"
