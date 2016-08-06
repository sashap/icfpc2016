open ExtLib
open Prelude

open Printf
open Otypes

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
  | "draw"::n::[] ->
    let p = sprintf "data/%s.in" n in
    let p = if Sys.file_exists p then Some (Problem.input p) else None in
    let s = sprintf "data/%s.out" n in
    let s = if Sys.file_exists s then Some (Solution.input s) else None in
    let out = sprintf "data/%s.png" n in
    eprintfn "output to %s" out;
    Render.render ?p ?s out
  | "bb"::file::[] ->
    let p = Problem.input file in
    let (lo,hi) = Ops.bounding_box p.shape in
    printfn "%s %s" (Pt.show lo) (Pt.show hi)
  | "solve"::meth::file::[] ->
    let p,save =
      match int_of_string file with
      | n -> Problem.input (sprintf "data/%d.in" n), (fun x -> Std.output_file ~filename:(sprintf "data/%d.out" n) ~text:(Solution.show x))
      | exception _ -> Problem.input file, (fun x -> print_string @@ Solution.show x)
    in
    let solution =
      match meth with
      | "bb" -> Ops.solve_bb p.shape
      | "best_bb" -> Ops.solve_best_bb p.shape
      | _ -> assert false
    in
    save solution
  | "rotate"::file::center::angle::[] ->
    let angle = float_of_string angle in
    let sol = Solution.input file in
    print_string @@ Solution.show {sol with dst = Array.map (Pt.rotate (Pt.of_string center) angle) sol.dst}
  | "mirror"::file::a::b::[] ->
    let line = Pt.of_string a, Pt.of_string b in
    let sol = Solution.input file in
    print_string @@ Solution.show {sol with dst = Array.map (Ops.mirror line) sol.dst}
  | "gen"::meth::[] ->
    let meth = match meth with
    | "rect" -> Gen.gen1
    | "v" -> Gen.gen_v
    | _ -> assert false
    in
(*     print_string @@ Solution.show @@ meth 5 *)
    print_string @@ Solution.show @@ Gen.random_mirrors 3 @@ meth (5 + Random.int 20)
  | "hello"::[] -> printfn "%s" (Api.get "hello")
  | "get_tasks"::[] -> Api.get_all_tasks ()
  | "submit_s"::[] -> Api.submit_all_solutions ()
  | "submit_s"::l -> Api.submit_solutions l
  | "submit_p"::[] -> Api.submit_problems ()
  | "union"::a::b::out::[] ->
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
  | "is_inside"::file::pt::[] ->
    let p = Problem.input file in
    List.map (Ops.is_inside (Pt.of_string pt)) p.shape |> List.iter (printfn "%B")
  | "resemble"::a::b::[] ->
    let a = Problem.input a in
    let b = Problem.input b in
    printfn "%g" (Ops.resemble a.shape b.shape)
  | _ -> fail "wat"
