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

let gen_t n =
  let w = R.zmake 1 n in
  let orig = {x=w; y=R.zero} in
  let a = Array.init (n+1) (fun i -> Pt.mul orig (R.int i)) in
  let b = a |> Array.map (fun p -> { p with y = R.one }) in
  let c = a |> Array.mapi (fun i p -> { p with y = R.div ((if i mod 2 = 0 then R.add else R.sub) R.one (R.mul w R.two)) (R.int 4) } ) in
  let d = a |> Array.map (fun p -> { p with y = R.div R.one R.two }) in
  let src = Array.concat [ a; b; c; d;] in
  let facets =
    List.init n (fun i -> [0*(n+1)+i; 0*(n+1)+i+1; 2*(n+1)+i+1; 2*(n+1)+i]) @
    List.init n (fun i -> [2*(n+1)+i; 2*(n+1)+i+1; 3*(n+1)+i+1; 3*(n+1)+i]) @
    List.init n (fun i -> [1*(n+1)+i; 1*(n+1)+i+1; 3*(n+1)+i+1; 3*(n+1)+i])
  in
  let dst_a = a |> Array.mapi (fun i _ -> { x = R.Infix.((R.one + (R.mul w R.two)) / (R.int 4));
    y = R.div ((if i mod 2 = 0 then R.add else R.sub) R.one (R.mul w R.two)) (R.int 4) } )
  in
  let dst_b = a |> Array.mapi (fun i _ -> Pt.mul orig (R.int (i mod 2))) in
  let dst_c = c |> Array.mapi (fun i p -> { p with x = if i mod 2 = 0 then R.zero else w }) in
  let dst_d = d |> Array.mapi (fun i p -> { p with x = if i mod 2 = 0 then R.zero else w }) in
  let dst = Array.concat [ dst_a; dst_b; dst_c; dst_d ] in
  { src; dst; facets; }

let gen_cross n =
  let w = R.zmake 1 n in
  let h = R.div w R.two in
  let orig = {x=w; y=R.zero} in
  let s0 = Array.init (n+1) (fun i -> { (Pt.mul orig (R.int i)) with y = R.one }) in
  let s2 = s0 |> Array.map (fun p -> { p with y = R.(div two (int 3)) }) in
  let s3 = s0 |> Array.map (fun p -> { p with y = R.(div one (int 3)) }) in
  let s5 = s0 |> Array.map (fun p -> { p with y = R.zero }) in
  let s1 = s0 |> Array.mapi (fun i p -> { p with y = ((if i mod 2 = 0 then R.sub else R.add) R.(div (int 5) (int 6)) h) } ) in
  let s4 = s0 |> Array.mapi (fun i p -> { p with y = ((if i mod 2 = 0 then R.sub else R.add) R.(div one (int 6)) h) } ) in
  let src = Array.concat [ s0;s1;s2;s3;s4;s5 ] in
  let facets =
    List.init n (fun i -> [0*(n+1)+i; 0*(n+1)+i+1; 1*(n+1)+i+1; 1*(n+1)+i]) @
    List.init n (fun i -> [1*(n+1)+i; 1*(n+1)+i+1; 2*(n+1)+i+1; 2*(n+1)+i]) @
    List.init n (fun i -> [2*(n+1)+i; 2*(n+1)+i+1; 3*(n+1)+i+1; 3*(n+1)+i]) @
    List.init n (fun i -> [3*(n+1)+i; 3*(n+1)+i+1; 4*(n+1)+i+1; 4*(n+1)+i]) @
    List.init n (fun i -> [4*(n+1)+i; 4*(n+1)+i+1; 5*(n+1)+i+1; 5*(n+1)+i])
  in
  let d0 = s0 |> Array.mapi (fun i _ -> { x = R.Infix.(R.one / R.int 6 + h);
    y = (if i mod 2 = 0 then R.add else R.sub) (R.div R.one R.two) h })
  in
  let d1 = s1 |> Array.mapi (fun i _ -> { x = if i mod 2 = 0 then R.zero else w;
    y = (if i mod 2 = 0 then R.add else R.sub) (R.div R.one R.two) h })
  in
  let d4 = d1 in
  let d2 = s2 |> Array.mapi (fun i p -> { p with x = if i mod 2 = 0 then R.zero else w }) in
  let d3 = s3 |> Array.mapi (fun i p -> { p with x = if i mod 2 = 0 then R.zero else w }) in
  let d5 = s0 |> Array.mapi (fun i _ -> { x = R.sub R.zero R.Infix.(R.one / R.int 6 - h);
    y = (if i mod 2 = 0 then R.add else R.sub) (R.div R.one R.two) h })
  in
  let dst = Array.concat [ d0; d1; d2; d3; d4; d5 ] in
  { src; dst; facets; }
