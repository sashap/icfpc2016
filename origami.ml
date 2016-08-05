open ExtLib
open Prelude

open Otypes

let () =
  Random.self_init ();
  match List.tl @@ Array.to_list Sys.argv with
  | "render"::file::[] ->
    Render.render_problem (Problem.input file) (file ^ ".png")
  | "bb"::file::[] ->
    let p = Problem.input file in
    let (lo,hi) = Ops.bounding_box p.shape in
    printfn "%s %s" (Pt.show lo) (Pt.show hi)
  | "solve"::meth::file::[] ->
    let p = Problem.input file in
    let solution =
      match meth with
      | "bb" -> Ops.bounding_box p.shape |> Ops.fold_bb
      | "best_bb" -> Ops.best_box p.shape |> Ops.fold_bb
      | _ -> assert false
    in
    if meth <> "best_bb" then eprintfn "resemblance %g" (Ops.resemble solution.shape p.shape);
    print_string @@ Solution.show solution
  | "hello"::[] -> printfn "%s" (Api.get "hello")
  | "get_tasks"::[] -> Api.get_all_tasks ()
  | "submit_s"::[] -> Api.submit_solutions ()
  | "submit_p"::[] -> Api.submit_problems ()
  | "is_inside"::file::pt::[] ->
    let p = Problem.input file in
    List.map (Ops.is_inside (Pt.of_string pt)) p.shape |> List.iter (printfn "%B")
  | "resemble"::a::b::[] ->
    let a = Problem.input a in
    let b = Problem.input b in
    printfn "%g" (Ops.resemble a.shape b.shape)
  | _ -> fail "wat"
