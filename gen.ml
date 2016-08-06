open ExtLib
open Otypes

let gen1 n =
  let orig0 = {x=R.zmake 1 n; y=R.zero} in
  let a = Array.init (n+1) (fun i -> Pt.mul orig0 (R.int i)) in
  let src = Array.concat [
    a;
    a |> Array.map (fun p -> { p with y = R.one });
  ] in
  let facets = List.init n (fun i -> [i; i+1; n+2+i; n+1+i]) in
  let dst_a = a |> Array.mapi (fun i _ -> Pt.mul orig0 (R.int ((i mod (n+1)) mod 2))) in
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
