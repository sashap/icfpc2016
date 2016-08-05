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
  let _ = { Problem.shape; skel } in
  ()

let () =
  match Action.args with
  | "read"::file::[] -> Control.with_open_in_txt file read
  | _ -> Exn.fail "wat"
