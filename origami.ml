open ExtLib
open Prelude

open Otypes

let () =
  match List.tl @@ Array.to_list Sys.argv with
  | "render"::file::[] ->
    let p = Problem.input file in
    with_open_out_bin (file ^ ".png") (Render.render p)
  | "bb"::file::[] ->
    let p = Problem.input file in
    let (lo,hi) = Ops.bounding_box p.shape in
    printfn "%s %s" (Pt.show lo) (Pt.show hi)
  | "solve"::meth::file::[] ->
    let p = Problem.input file in
    let solution =
      match meth with
      | "bb" -> Ops.bounding_box p.shape |> Ops.fold_bb
      | _ -> assert false
    in
    print_string @@ Solution.show solution
  | "hello"::[] -> printfn "%s" (Api.get "hello")
  | "get_tasks"::[] -> Api.get_all_tasks ()
  | "submit"::[] -> Api.submit_solutions ()
  | "is_inside"::file::pt::[] ->
    let p = Problem.input file in
    List.map (Ops.is_inside (Pt.of_string pt)) p.shape |> List.iter (printfn "%B")
  | _ -> fail "wat"
