open ExtLib

open Types

let readlni ch = input_line ch |> String.strip |> int_of_string

let read ch =
  let shape =
    List.init (readlni ch) begin fun _ ->
      List.init (readlni ch) (fun _ -> Pt.of_string @@ input_line ch)
    end
  in
  let skel =
    List.init (readlni ch) (fun _ -> Line.of_string @@ input_line ch)
  in
  { Problem.shape; skel }

let () =
  match Action.args with
  | "render"::file::[] ->
    let p = Control.with_open_in_txt file read in
    Control.with_open_out_bin (file ^ ".png") (Render.render p)
  | _ -> Exn.fail "wat"
