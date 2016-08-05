open Printf
open ExtLib
open Prelude

let () = Curl.global_init Curl.CURLINIT_GLOBALALL

let do_perform h = try Curl.perform h; Curl.CURLE_OK with Curl.CurlException (code,_,_) -> code

let last_run = ref @@ Unix.gettimeofday ()

let sleep sec = ignore (Unix.select [] [] [] sec)

let urlencode = function
  | "" -> ""
  | s ->
    let b = Buffer.create 16 in
    s |> String.iter begin function
      | ('a'..'z'|'A'..'Z'|'0'..'9'|'-'|'.'|'_'|'~' as c) -> Buffer.add_char b c
      | c ->
        let chex n = "0123456789ABCDEF".[n] in
        let n = Char.code c in
        Buffer.add_char b '%';
        Buffer.add_char b @@ chex (n / 16);
        Buffer.add_char b @@ chex (n mod 16)
    end;
    Buffer.contents b

let set_form h args =
  let body = String.concat "&" @@ List.map (fun (k,v) -> sprintf "%s=%s" (urlencode k) (urlencode v)) args in
  let open Curl in
  set_post h true;
  set_postfields h body;
  set_postfieldsize h (String.length body)

let get ?post api =
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
  let headers =  ["Expect:"; "X-API-Key: 103-7133f8e2759c5495a88472be2ff6f7c1"] in
  let headers = if Option.is_some post then "Content-Type: application/x-www-form-urlencoded" :: headers else headers in
  set_httpheader h headers;
  begin match post with
  | None -> eprintfn "GET %s" api
  | Some args -> set_form h args
  end;
  set_url h ("http://2016sv.icfpcontest.org/api/" ^ api);
  let b = Buffer.create 10 in
  set_writefunction h (fun s -> Buffer.add_string b s; String.length s);
  match do_perform h with
  | CURLE_OK when Curl.get_httpcode h = 200 -> Buffer.contents b
  | CURLE_OK -> fail "http %d" (Curl.get_httpcode h)
  | code -> fail "curl %d : %s" (Curl.errno code) (Curl.strerror code)

let get_blob hash = get ("blob/" ^ hash)

let get_snapshot () =
  let open Api_j in
  let { ok; snapshots } = snapshots_of_string @@ get "snapshot/list" in
  assert ok;
  match List.sort ~cmp:(fun a b -> compare b.snapshot_time a.snapshot_time) snapshots with
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
  st.problems |> List.iter begin fun (p:problem) ->
    let filename = sprintf "data/%d.in" p.problem_id in
    match Sys.file_exists filename with
    | true -> ()
    | false ->
      eprintfn "getting %s" filename;
      Std.output_file ~filename ~text:(get_blob p.problem_spec_hash)
  end

let send ?sol ?prob s =
  let api,task = match sol, prob with
    | Some s , None -> "solution/submit", ("problem_id", s)
    | None, Some p -> "problem/submit", ("publish_time", p)
    | _ -> failwith "problem *OR* solution!"
  in
  let post = [ task; "solution_spec", s] in
  get ~post api

let different f1 f2 = Std.input_file f1 <> Std.input_file f2

let submit_solutions () =
  let sent = sprintf "data/%s.sent" in
  let out = sprintf "data/%s.out" in
  let result = sprintf "data/%s.result" in
  let perfect = sprintf "data/%s.perfect_score" in
  Sys.readdir "data/" |> Array.to_list |> List.sort
  |> List.filter_map (fun s -> if String.ends_with s ".out" then Some (String.slice ~last:(-4) s) else None)
  |> List.filter (fun s -> not @@ Sys.file_exists @@ sent s || different (out s) (sent s))
  |> List.iter begin fun s ->
    eprintfn "sending %s ..." (out s);
    let sol = Std.input_file @@ out s in
    let res = send ~sol:s sol in
    Std.output_file ~filename:(result s) ~text:res;
    Std.output_file ~filename:(sent s) ~text:sol;
    let rr = (Api_j.solution_of_string res).resemblance in
    if rr > 0.999999 then Std.output_file ~filename:(perfect s) ~text:sol;
    eprintfn "resemblance %f" rr
  end

let submit_problems () =
  let sent = sprintf "problems/%s.sent" in
  let out = sprintf "problems/%s.out" in
  let result = sprintf "problems/%s.result" in
  Sys.readdir "problems/" |> Array.to_list |> List.sort
  |> List.filter_map (fun s -> if String.ends_with s ".out" then Some (String.slice ~last:(-4) s) else None)
  |> List.filter (fun s -> not @@ Sys.file_exists @@ sent s || different (out s) (sent s))
  |> List.iter begin fun s ->
    eprintfn "sending %s ..." (out s);
    let prob = Std.input_file @@ out s in
    let res = send ~prob:s prob in
    Std.output_file ~filename:(result s) ~text:res;
    Std.output_file ~filename:(sent s) ~text:prob;
  end
