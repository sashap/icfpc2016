open Printf

let printfn fmt = Printf.ksprintf print_endline fmt

let fail ?exn fmt =
  let fails s = match exn with None -> failwith s | Some exn -> failwith (s ^ " : " ^ Printexc.to_string exn) in
  ksprintf fails fmt

let bracket resource destroy k = Std.finally (fun () -> destroy resource) k resource

let with_open_in_txt name = bracket (open_in name) close_in_noerr
let with_open_out_txt name = bracket (open_out name) close_out_noerr
let with_open_in_bin name = bracket (open_in_bin name) close_in_noerr
let with_open_out_bin name = bracket (open_out_bin name) close_out_noerr
