open ExtLib
open Otypes
open Prelude

let monte_carlo_limit = 2_000
let shuffle_a = 70
let shuffle_limit = 100

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
    | Right -> mirror ln pt
    | _ -> pt
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
  let wrap p len =
    let n,m = R.divide p len in
    if R.is_zero m then snd @@ R.divide p (R.mul len R.two) else if (Z.(mod) n (Z.of_int 2)) = Z.zero then m else R.sub len m
  in
  let dst = vs |> Array.map begin fun p ->
    let x = wrap p.x bb.x in
    let y = wrap p.y bb.y in
    Pt.add lo {x;y} end
  in
  { src=vs; dst; facets; }, [poly_of_box (lo,hi)]

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
  for _ = 0 to monte_carlo_limit do
    let pt = { x = R.add lo.x (R.random bb.x); y = R.add lo.y (R.random bb.y) } in
    let a = is_inside_shape pt a in
    let b = is_inside_shape pt b in
    if a && b then incr nr_inter;
    if a || b then incr nr_union
  done;
  float !nr_inter /. float !nr_union

let mult_box (lo,hi) var f =
  let bb = Pt.sub hi lo in
  let cc = Pt.div (Pt.add hi lo) R.two in
  let new_bb = Pt.mul bb f in
  let new_cc = Pt.div bb R.two in
  let offset = match var with
  | 0 -> Pt.sub cc new_cc
  | 1 -> lo
  | 2 -> Pt.sub hi new_bb
  | 3 -> { x = lo.x; y = (Pt.sub hi new_bb).y }
  | 4 -> { x = (Pt.sub hi new_bb).x; y = lo.y }
  | _ -> assert false
  in
  offset, Pt.add offset new_bb

let best_box shape =
  let box = bounding_box shape in
  let calc box = resemble [poly_of_box box] shape in
  let r = ref @@ calc box in
  let pos = ref (0,0) in
  let best = ref box in
  for i = shuffle_limit downto shuffle_a do
    let f = R.zmake i shuffle_limit in
    for j = 0 to 4 do
      let new_box = mult_box box j f in
      let r' = calc new_box in
      if r' > !r then
      begin
  (*       eprintfn "advance %d %g -> %g %s" i !r r' (show_box new_box); *)
        r := r';
        best := new_box;
        pos := (i,j)
      end;
    done;
  done;
  eprintfn "best_bb: %g i=%d j=%d" !r (fst !pos) (snd !pos);
  !best

let solve_bb shape =
 let (sol,sol_shape) = bounding_box shape |> fold_bb in
 eprintfn "bb: resemblance %g" (try resemble sol_shape shape with _ -> nan);
 sol

let solve_best_bb shape =
  best_box shape |> fold_bb |> fst

let neighbors' l idx =
  try
    let len = List.length l - 1 in
    if len < 3 then None else
      match idx with
      | 0 -> Some (List.last l, List.nth l 1)
      | i when i = len -> Some (List.nth l (i-1),List.hd l)
      | i -> Some (List.nth l (i-1),List.nth l (i+1))
  with _ ->
    None

let neighbors l e =
  try
      let idx, _ = List.findi (fun _ p -> Pt.eq p e) l in
      neighbors' l idx
  with _ ->
    None

let intersect_edges p1 p2 =
  let e1 = ref [] in
  let e2 = ref @@ Poly.edges p2 in
  let ok = ref false in
  Poly.edges p1 |> List.iter begin fun e ->
    let points = ref [] in
    !e2 |> List.iter begin fun c ->
      match Line.get_intersect e c with
      | None -> ()
      | Some p ->
        ok := true;
        tuck points p;
        e2 := (fst c, p) :: (p, snd c) :: List.filter (fun c' -> c <> c') !e2
    end;
    match !points with
    | [] -> tuck e1 e
    | l ->
      let orig = fst e in
      let l = l |> List.map (fun x -> Line.length2 (orig,x), x) |> List.sort ~cmp:(fun a b -> R.compare (fst a) (fst b)) |> List.map snd in
      let (edges,last) = Poly.connect (orig :: l) in
     tuck e1 (last, snd e);
      List.iter (tuck e1) edges
  end;
  match !ok with
  | true -> `Edges (!e1 @ !e2)
  | false ->
  let check l = assert (List.fold_left (fun acc x -> acc = x) (List.hd l) l); List.hd l in
  let p2_in_p1 = check @@ List.map (fun p -> is_inside p p1) p2 in
  let p1_in_p2 = check @@ List.map (fun p -> is_inside p p2) p1 in
  match p1_in_p2, p2_in_p1 with
  | true, false -> `Outer p2
  | false, true -> `Outer p1
  | false, false -> assert false (* disjoint *)
  | true, true -> assert false (* impossible *)

let find_max cmp = function
| [] -> assert false
| x::xs -> List.fold_left (fun acc x -> if cmp x acc > 0 then x else acc) x xs

let union p1 p2 =
  match intersect_edges p1 p2 with
  | `Outer p -> p
  | `Edges e ->
    let (lo,_) = bounding_box @@ List.map (fun (a,b) -> [a;b]) e in
    let (start,_) =
      let points = Points.elements @@ List.fold_left (fun acc (a,b) -> Points.add b (Points.add a acc)) Points.empty e in
      List.fold_left (fun (_,m as acc) p' -> let mp' = Pt.sub p' lo in if Pt.compare mp' m < 0 then p',mp' else acc)
        (List.hd points, Pt.sub (List.hd points) lo) points
    in
    let poly = ref [] in
    let e = ref e in
    let from p = !e |> List.filter_map (fun (a,b) -> if Pt.eq a p then Some b else if Pt.eq b p then Some a else None) in
    let select p =
      tuck poly p;
      let next = from p in
      e := List.filter (fun (a,b) -> not (Pt.eq a p || Pt.eq b p)) !e;
(*       printfn "picked %s remain %d next %s" (Pt.show p) (List.length !e) (String.concat " " @@ List.map Pt.show next); *)
      next
    in
    let rec loop prev last =
      match select last with
      | [] -> !poly
      | l ->
        let to_prev = Pt.sub prev last in
(*         printfn "last %s to_prev %s" (Pt.show last) (Pt.show to_prev); *)
        let p = find_max (fun a b -> compare (Pt.angle to_prev (Pt.sub a last)) (Pt.angle to_prev (Pt.sub b last))) l in
        loop last p
    in
    loop (Pt.sub lo Pt.one) start

let find_start nv1 overt =
  let rec loop lst i =
    match lst with
    | [] -> failwith "not on edge!"
    | v1::[] ->
      let v2 = List.hd overt in
      if Line.is_on_line (v1,v2) nv1 then
        i
      else
        loop [] (i+1)
    | v1::(v2::_ as tl) ->
      if Line.is_on_line (v1,v2) nv1 then
        i
      else
        loop tl (i+1)
  in
  loop overt 0

let get_polygons start (p1,p2 as edge) overt =
  let top = ref [p1] in
  let bot = ref [p2] in
  let rec loop i overt =
    (* Printf.printf "walking: %d on %s\n top: %s\n bot: %s\n" i (Poly.show overt) (Poly.show !top) (Poly.show !bot); *)
    let get_next started l =
      match l with
      | [] -> assert false
      | (pt::overt) ->
      if started then
        if Line.which_side edge pt = Right then
          `Top (mirror edge pt),overt
        else
          `Bot pt, overt
      else
        `No, overt@[pt]
    in
    (match get_next (i > start) overt with
     | `Top p,[] -> top := p2::p::!top; bot:= p1::!bot;
     | `Bot p,[] -> bot := p1::p::!bot; top:= p2::!top;
     | _,[] -> ()
     | `Top p, ovt -> top := p::!top; loop (i+1) ovt
     | `Bot p, ovt -> bot := p::!bot; loop (i+1) ovt
     | `No, ovt -> loop (i+1) ovt);
  in loop 0 overt;
     !top, (List.rev !bot)

  let do_fold outer_vertices _inner_vertices (p1,_p2 as edge) = (* fold leftward *)
    let start = find_start p1 outer_vertices in
    let top_poly, bot_poly = get_polygons start edge outer_vertices in
    union top_poly bot_poly

(* let gen_folds () = *)
  (* let open Printf in *)
  (* (\* let x0y0, x0y1, x1y1, x1y0 = (\\*outer vertexes*\\) *\) *)
  (* (\*   ({x = R.zero; y = R.zero}, *\) *)
  (* (\*    {x = R.zero; y = R.one}, *\) *)
  (* (\*    {x = R.one; y = R.one}, *\) *)
  (* (\*    {x = R.one; y = R.zero}) *\) *)
  (* (\* in *\) *)
  (* (\* let rec loop i v = *\) *)
  (* let cpt_idx = Hashtbl.create 10 in *)
  (* let idx_pt_hist = Hashtbl.create 10 in *)
  (* while (read_int ()) = 1 do *)
  (*   printf "Edge to fold over : "; *)
  (*   let edge = Line.of_string (read_line ()) in *)
  (*   let outer, inner = do_fold orig [] edge in *)
  (*   printf "continue? [1/0] : "; *)
  (* done *)
