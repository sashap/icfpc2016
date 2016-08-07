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

let h = lazy (Curl.init ())

exception Http of int * string

let get ?post api =
  if Unix.gettimeofday () -. !last_run < 1.1 then sleep 1.;
  last_run := Unix.gettimeofday ();
  let open Curl in
  let h = Lazy.force h in
  reset h;
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
  | CURLE_OK -> raise (Http (Curl.get_httpcode h, Buffer.contents b))
  | code -> fail "curl %d : %s" (Curl.errno code) (Curl.strerror code)

let get_blob hash = get ("blob/" ^ hash)

let get_snapshot () =
  let open Api_j in
  let { ok; snapshots } = snapshots_of_string @@ get "snapshot/list" in
  assert ok;
  match List.sort ~cmp:(fun a b -> compare b.snapshot_time a.snapshot_time) snapshots with
  | [] -> assert false
  | { snapshot_hash = hash; _ } :: _ -> get_blob hash

let _get_state () =
  let filename = "snapshot.json" in
  let s = match Sys.file_exists filename with
  | false -> let text = get_snapshot () in Std.output_file ~filename ~text; text
  | true -> Std.input_file filename
  in
  Api_j.state_of_string s

let get_state () = Api_j.state_of_string @@ get_snapshot ()

let get_all_tasks () =
  let open Api_j in
  let st = get_state () in
  let to_get = st.problems |> List.filter begin fun (p:problem) ->
    let filename = sprintf "data/%d.in" p.problem_id in
    not @@ Sys.file_exists filename
  end
  in
  to_get |> List.iteri begin fun i (p:problem) ->
      let filename = sprintf "data/%d.in" p.problem_id in
      eprintfn "getting %d of %d : %s" i (List.length to_get) filename;
      Std.output_file ~filename ~text:(get_blob p.problem_spec_hash)
  end

let base_ts = 1470441600
let get_time p = base_ts + 3600 * p

let send ?sol ?prob s =
  let api,task = match sol, prob with
    | Some s , None -> "solution/submit", ("problem_id", s)
    | None, Some p -> "problem/submit", ("publish_time", sprintf "%d" (get_time p))
    | _ -> failwith "problem *OR* solution!"
  in
  let post = [ task; "solution_spec", s] in
  get ~post api

let different f1 f2 = Std.input_file f1 <> Std.input_file f2

let submit_solutions l =
  let sent = sprintf "data/%s.sent" in
  let out = sprintf "data/%s.out" in
  let result = sprintf "data/%s.result" in
  let perfect = sprintf "data/%s.perfect_score" in
  let best = sprintf "data/%s.best" in
  let block = sprintf "data/%s.block" in
  l
  |> List.filter (fun s -> not @@ Sys.file_exists @@ sent s || different (out s) (sent s))
  |> List.iter begin fun s ->
    match Sys.file_exists @@ block s with
    | true -> eprintfn "skipping %s due to %s" s (block s)
    | false ->
    match Sys.file_exists @@ perfect s with
    | true -> eprintf "skipping %s due to %s" s (perfect s)
    | false ->
    let prev_r =
      match Api_j.solution_of_string @@ Std.input_file (result s) with
      | exception _ -> 0.
      | a -> a.resemblance
    in
    let best_r =
      match Std.input_file (best s) with
      | exception _ -> 0.
      | s -> (Api_j.solution_of_string s).resemblance
    in
    eprintf "sending %s ... %!" (out s);
    let sol = Std.input_file @@ out s in
    match send ~sol:s sol with
    | exception Http (error,message) ->
      eprintfn "HTTP %d %s" error message;
      if error = 403 then
      begin
        Std.output_file ~filename:(result s) ~text:message;
        Std.output_file ~filename:(sent s) ~text:sol;
      end
    | res ->
    Std.output_file ~filename:(result s) ~text:res;
    Std.output_file ~filename:(sent s) ~text:sol;
    let rr = (Api_j.solution_of_string res).resemblance in
    if rr > 0.999999 then Std.output_file ~filename:(perfect s) ~text:sol;
    if rr > best_r then Std.output_file ~filename:(best s) ~text:res;
    let msg = if best_r > 0. then (if rr > best_r then (if rr -. best_r > 0.02 then "IMPROVED " else "improved ") else "") else "new " in
    eprintfn "%sresemblance %g -> %g (prev %g)" msg best_r rr prev_r
  end

let submit_all_solutions () =
  Sys.readdir "data/" |> Array.to_list
  |> List.filter_map (fun s -> if String.ends_with s ".out" then Some (String.slice ~last:(-4) s) else None)
  |> List.sort
  |> submit_solutions

let submit_problems () =
  let sent = sprintf "problems/%s.sent" in
  let out = sprintf "problems/%s.out" in
  let result = sprintf "problems/%s.result" in
  Sys.readdir "problems/" |> Array.to_list |> List.sort
  |> List.filter_map (fun s -> if String.ends_with s ".out" then Some (String.slice ~last:(-4) s) else None)
  |> List.filter (fun s -> not @@ Sys.file_exists @@ sent s || different (out s) (sent s))
  |> List.iter begin fun s ->
    match Std.input_file @@ out s with
    | "" -> ()
    | prob ->
    eprintfn "sending %s at %d ..." (out s) (get_time @@ int_of_string s);
    match send ~prob:(int_of_string s) prob with
    | exception Http (code,message) ->
      eprintfn "HTTP %d %s" code message;
      if code = 403 then
      begin
        Std.output_file ~filename:(result s) ~text:message;
        Std.output_file ~filename:(sent s) ~text:prob;
      end
    | res ->
    Std.output_file ~filename:(result s) ~text:res;
    Std.output_file ~filename:(sent s) ~text:prob;
    let res = Api_j.problem_subm_of_string res in
    assert res.ok;
    printfn "problem #%d (solution size %d) will be published on %d" res.problem_id res.solution_size res.publish_time
  end
