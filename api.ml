open Printf
open Prelude

let () = Curl.global_init Curl.CURLINIT_GLOBALALL

let do_perform h = try Curl.perform h; Curl.CURLE_OK with Curl.CurlException (code,_,_) -> code

let last_run = ref @@ Unix.gettimeofday ()

let sleep sec = ignore (Unix.select [] [] [] sec)

let get api =
  if Unix.gettimeofday () -. !last_run < 1. then sleep 1.;
  last_run := Unix.gettimeofday ();
  let open Curl in
  let h = init () in
  set_useragent h "HET";
  set_nosignal h true;
  set_connecttimeout h 2;
  set_timeout h 10;
  set_followlocation h true;
  set_maxredirs h 1;
  set_ipresolve h IPRESOLVE_V4;
  set_encoding h CURL_ENCODING_GZIP;
  set_protocols h [CURLPROTO_HTTP; CURLPROTO_HTTPS;];
  set_redirprotocols h [CURLPROTO_HTTP; CURLPROTO_HTTPS;];
  set_httpheader h ["Expect:"; "X-API-Key: 103-7133f8e2759c5495a88472be2ff6f7c1"];
  set_url h ("http://2016sv.icfpcontest.org/api/" ^ api);
  let b = Buffer.create 10 in
  set_writefunction h (fun s -> Buffer.add_string b s; String.length s);
  eprintfn "GET %s" api;
  match do_perform h with
  | CURLE_OK when Curl.get_httpcode h = 200 -> Buffer.contents b
  | CURLE_OK -> fail "http %d" (Curl.get_httpcode h)
  | code -> fail "curl %d : %s" (Curl.errno code) (Curl.strerror code)

let get_blob hash = get ("blob/" ^ hash)

let get_snapshot () =
  let open Api_j in
  let { ok; snapshots } = snapshots_of_string @@ get "snapshot/list" in
  assert ok;
  match List.sort (fun a b -> compare b.snapshot_time a.snapshot_time) snapshots with
  | [] -> assert false
  | { snapshot_hash = hash; _ } :: _ -> get_blob hash

let get_state () =
  let filename = "snapshot.json" in
  let s = match Sys.file_exists filename with
  | false -> let text = get_snapshot () in Std.output_file ~filename ~text; text
  | true -> Std.input_file filename
  in
  Api_j.state_of_string s

let get_all_tasks () =
  let open Api_j in
  let st = get_state () in
  st.problems |> List.iter begin fun p ->
    let filename = sprintf "data/%d.in" p.problem_id in
    match Sys.file_exists filename with
    | true -> ()
    | false ->
      eprintfn "getting %s" filename;
      Std.output_file ~filename ~text:(get_blob p.problem_spec_hash)
  end




