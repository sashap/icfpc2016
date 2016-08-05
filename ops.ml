open ExtLib
open Otypes
open Prelude

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
    match Line.which_side ln pt with
    | Left -> mirror ln pt
    | On | Right -> pt
  end poly

let bounding_box shape =
  match shape |> List.concat with
  | [] -> assert false
  | x::xs ->
    List.fold_left (fun (alo,ahi) p -> Pt.lo p alo, Pt.hi p ahi) (x,x) xs

(* stupid *)
let poly_of_box (lo,hi) = [ lo; { lo with y = hi.y }; hi; { hi with y = lo.y } ]

let fold_bb (lo,hi) =
  let bb = Pt.lo Pt.one (Pt.sub hi lo) in
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
      match R.eq R.one p.x || R.eq R.one p.y with
      | true -> None
      | false ->
      let px = { x = R.min_ R.one (p.x + bb.x); y = p.y } in
      let py = { x = p.x; y = R.min_ R.one (p.y + bb.y) } in
      let pp = Pt.lo Pt.one (Pt.add p bb) in
      try
        Some [ index p; index px; index pp; index py ]
      with
        _ -> None
    end
  in
  let src = Array.to_list vs in
  let wrap p len =
    let n,m = R.divide p len in
    if R.is_zero m then snd @@ R.divide p (R.mul len R.two) else if (Z.(mod) n (Z.of_int 2)) = Z.zero then m else R.sub len m
  in
  let dst = src |> List.map begin fun p ->
    let x = wrap p.x bb.x in
    let y = wrap p.y bb.y in
    Pt.add lo {x;y} end
  in
  { src=Array.to_list vs; dst; facets; shape = [poly_of_box (lo,hi)]; }

let is_inside p v =
  let v = Array.of_list v in
  let n = Array.length v in
  let r = ref 0 in
  for i = 0 to n - 1 do
    let vv = if i = n - 1 then v.(0) else v.(i+1) in
    let v = v.(i) in
    if R.le v.y p.y then
    begin
      if R.gt vv.y p.y && Line.which_side (v,vv) p = Left then incr r
    end
    else
    begin
      if R.le vv.y p.y && Line.which_side (v,vv) p = Right then decr r
    end
  done;
  !r <> 0

(* ignoring holes *)
let is_inside_shape p = List.exists (is_inside p)

let show_box (a,b) = Printf.sprintf "%s %s" (Pt.show a) (Pt.show b)

let resemble a b =
  let lo,hi = bounding_box @@ List.concat [a;b] in
  let bb = Pt.sub hi lo in
(*   eprintfn "bounded %s" (show_box (lo,hi)); *)
  let nr_inter = ref 0 in
  let nr_union = ref 0 in
  for _ = 0 to 2_000 do
    let pt = { x = R.add lo.x (R.random bb.x); y = R.add lo.y (R.random bb.y) } in
    let a = is_inside_shape pt a in
    let b = is_inside_shape pt b in
    if a && b then incr nr_inter;
    if a || b then incr nr_union
  done;
  float !nr_inter /. float !nr_union

let mult_box (lo,hi) f =
  let bb = Pt.sub hi lo in
  let cc = Pt.div (Pt.add hi lo) R.two in
  let new_bb = Pt.mul bb f in
  let new_cc = Pt.div bb R.two in
  let offset = Pt.sub cc new_cc in
  offset, Pt.add offset new_bb

let best_box shape =
  let box = bounding_box shape in
  let calc box = resemble [poly_of_box box] shape in
  let init = calc box in
  let r = ref init in
  let best = ref box in
  for i = 99 downto 50 do
    let f = R.zmake i 100 in
    let new_box = mult_box box f in
    let r' = calc new_box in
    if r' > !r then
    begin
(*       eprintfn "advance %d %g -> %g %s" i !r r' (show_box new_box); *)
      r := r';
      best := new_box
    end;
  done;
  eprintfn "best_bb: %g -> %g" init !r;
  !best

let get_intersect (a1,b1) (a2,b2) = (*kx+ny=c*)
 let open R.Infix in
 let get_coefs a b =
   let k = b.y - a.y in
   let n = a.x - b.x in
   let c = (k * a.x) + (n * a.y) in
   (k,n,c)
 in
 let get_ord f s = if f > s then f,s else s,f in
 let k1,n1,c1 = get_coefs a1 b1 in
 let k2,n2,c2 = get_coefs a2 b2 in
 match (k1 * n2) - (k2 * n1) with
 | det when det = R.zero -> None
 | det ->
   let x = ((n2*c1) - (n1*c2))/det in
   let y = ((k1*c2) - (k2*c1))/det in
   let x1g,x1s = get_ord a1.x b1.x in
   let y1g,y1s = get_ord a1.y b1.y in
   let x2g,x2s = get_ord a2.x b2.x in
   let y2g,y2s = get_ord a2.y b2.y in
   if (x <= x1g) && (x >= x1s) && (x <= x2g) && (x >= x2s) &&
      (y <= y1g) && (y >= y1s) && (y <= y2g) && (y >= y2s) then
     Some {x;y}
   else
     None
