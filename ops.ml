open ExtLib
open Otypes

let mirror (l1,l2) pt =
  let open R.Infix in
  let dx = l2.x - l1.x in
  let dy = l2.y - l1.y in
  let a = (R.sqr dx - R.sqr dy) / (R.sqr dx + R.sqr dy) in
  let b = (R.two * (dx * dy)) / (R.sqr dx + R.sqr dy) in
  { x = l1.x + ((a * (pt.x - l1.x)) + (b * (pt.y - l1.y)));
    y = l1.y + ((b * (pt.x - l1.x)) - (a * (pt.y - l1.y)))}

let fold_over_line ln poly =
  List.map begin fun pt ->
    match Line.which_side_of_line ln pt with
    | Left -> mirror ln pt
    | On | Right -> pt
  end poly

let bounding_box shape =
  match shape |> List.concat with
  | [] -> assert false
  | x::xs ->
    List.fold_left (fun (alo,ahi) p -> Pt.lo p alo, Pt.hi p ahi) (x,x) xs

let fold_bb (lo,hi) =
  let bb = Pt.lo {x=R.one;y=R.one} (Pt.sub hi lo) in
  let rec loop acc cur r =
    if R.gt R.one cur then loop (cur::acc) (R.add cur r) r else R.one :: acc
  in
  let x = loop [R.zero] bb.x bb.x in
  let y = loop [R.zero] bb.y bb.y in
  let vs = x |> List.map (fun x -> y |> List.map (fun y -> {x;y})) |> List.concat |> Array.of_list in
  let index p = Array.findi (Pt.eq p) vs in
  let facets =
    vs |> Array.to_list |> List.filter_map begin fun p ->
      let open R.Infix in
      let px = { x = p.x + bb.x; y = p.y } in
      let py = { x = p.x; y = p.y + bb.y } in
      let pp = Pt.add p bb in
      try
        Some [ index p; index px; index pp; index py ]
      with
        _ -> None
    end
  in
  let bb2x = R.mul bb.x R.two in
  let bb2y = R.mul bb.y R.two in
  let src = Array.to_list vs in
  let dst = src |> List.map begin fun p ->
    let x = R.modulo p.x bb2x in
    let y = R.modulo p.y bb2y in
    Pt.add lo {x;y} end
  in
  { src=Array.to_list vs; dst; facets; }
