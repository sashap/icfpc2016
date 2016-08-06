open ExtLib
open Otypes

let gen1 n =
  let orig = {x=R.zmake 1 n; y=R.zero} in
  let a = Array.init (n+1) (fun i -> Pt.mul orig (R.int i)) in
  let src = Array.concat [
    a;
    a |> Array.map (fun p -> { p with y = R.one });
  ] in
  let facets = List.init n (fun i -> [i; i+1; n+2+i; n+1+i]) in
  let dst_a = a |> Array.mapi (fun i _ -> Pt.mul orig (R.int ((i mod (n+1)) mod 2))) in
  let dst_b = dst_a |> Array.map (fun p -> { p with y = R.one }) in
  let dst = Array.concat [ dst_a; dst_b ] in
  { src; dst; facets; }

let transform s f = { s with dst = Array.map f s.dst }

let random_mirrors run sln =
  let box = Ops.poly_of_box @@ Ops.bounding_box [Array.to_list sln.dst] in
  let rec loop run points =
    match run with
    | 0 -> points
    | _ ->
    let rnd () = {x=R.random R.two; y=R.random R.one} in
    let a = rnd () in
    let b = rnd () in
    if Ops.is_inside a box || Ops.is_inside b box then
      loop run points
    else
      loop (run - 1) (Array.map (Ops.mirror (a,b)) points)
  in
  { sln with dst = loop run sln.dst }

let gen_v n =
  let w = R.zmake 1 n in
  let orig = {x=w; y=R.zero} in
  let a = Array.init (n+1) (fun i -> Pt.mul orig (R.int i)) in
  let b = a |> Array.map (fun p -> { p with y = R.one }) in
  let c = a |> Array.mapi (fun i p -> { p with y = R.div ((if i mod 2 = 0 then R.sub else R.add) R.one w) R.two } ) in
  let src = Array.concat [ a; b; c ] in
  let facets =
    List.init n (fun i -> [i; i+1; 2*(n+1)+i+1; 2*(n+1)+i]) @
    List.init n (fun i -> [2*(n+1)+i; 2*(n+1)+i+1; (n+1)+i+1; (n+1)+i])
  in
  let dst_a = a |> Array.mapi (fun i _ -> Pt.mul orig (R.int (i mod 2))) in
  let dst_b = c |> Array.mapi (fun i _ -> { x = R.Infix.((R.one + w) / R.two);
    y = R.div ((if i mod 2 = 0 then R.sub else R.add) R.one w) R.two } )
  in
  let dst_c = c |> Array.mapi (fun i p -> { p with x = if i mod 2 = 0 then R.zero else w }) in
  let dst = Array.concat [ dst_a; dst_b; dst_c ] in
  { src; dst; facets; }
