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
 eprintfn "resemblance %g" (resemble sol_shape shape);
 sol

let solve_best_bb shape =
  best_box shape |> fold_bb |> fst

let gen_folds () =
  let x0y0, x0y1, x1y1, x1y0 = (*outer vertexes*)
    ({x = R.zero; y = R.zero},
     {x = R.zero; y = R.one},
     {x = R.one; y = R.one},
     {x = R.one; y = R.zero})
  in
  let outer_vertices = [x0y0; x1y0; x1y1; x0y1] in
  (* let outer_edges = ref [(x0y0, x0y1);(x0y1, x1y1);(x1y1, x1y0);(x1y0, x0y0)] in (\* initial square *\) *)

  (* let pick_edge_to_fold overt = *)
  (*   let v1 = Random.int (List.length overt - 1) in *)
  (*   let v2 = v1 + 2 in (\*do proper!*\) *)
  (*   let p1  = R.(random (make (Z.of_int 100) (Z.of_int 100))) in *)
  (*   let p2  = R.(random (make (Z.of_int 100) (Z.of_int 100))) in *)
  (*   let pt1 = Line.get_on_line !! p1 in *)
  (*   let pt2 = Line.get_on_line !! p2 in *)
  (*   (pt1, pt2) *)
  (* in *)
  (* let insert_vertices nv1 nv2 overt = *)
  (*   let nvert = ref [] in *)
  (*   let rec loop lst = *)
  (*     match lst with *)
  (*     | [] -> !nvert *)
  (*     | v1::[] -> *)
  (*       nvert := v1::!nvert; *)
  (*       let v2 = List.hd overt in *)
  (*       if Line.is_on_line (v1,v2) nv1 then *)
  (*         nvert := nv1::!nvert *)
  (*       else if Line.is_on_line (v1,v2) nv2 then *)
  (*         nvert := nv2::!nvert; *)
  (*       !nvert; *)
  (*     | v1::(v2::_ as tl) -> *)
  (*       nvert := v1::!nvert; *)
  (*       if Line.is_on_line (v1,v2) nv1 then *)
  (*         (nvert := nv1::!nvert; loop tl) *)
  (*       else if Line.is_on_line (v1,v2) nv2 then *)
  (*         (nvert := nv2::!nvert; loop tl) *)
  (*       else *)
  (*         loop tl *)
  (*   in *)
  (*   let _ = loop overt in *)
  (*   let nvert = List.rev !nvert in *)
  (*   assert ((List.length overt - List.length nvert) = 2); *)
  (*   nvert *)
  (* in *)
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
  in
  let get_polygons start (p1,p2 as edge) overt =
    let top = ref [p1] in
    let bot = ref [p2] in
    let rec loop i overt =
      let get_next started (pt::overt) =
        if started then
          if Line.which_side edge pt = Right then
            `Top (mirror edge pt),overt
          else
            `Bot pt, overt
        else
          `No, overt@[pt]
      in
      (match get_next (i > start) overt with
       | `Top p,[] -> top := p2::p::!top
       | `Bot p,[] -> bot := p1::p::!bot
       | _,[] -> ()
       | `Top p, ovt -> top := p::!top; loop (i+1) ovt
       | `Bot p, ovt -> bot := p::!bot; loop (i+1) ovt
       | `No, ovt -> loop (i+1) ovt);
    in loop 0 overt;
    (List.rev !top), !bot
  in
  let do_fold src outer_vertices (p1,p2 as edge) edgeidc = (* fold leftward *)
    (*fold and rearange outer vertices*)
    let start = find_start p1 outer_vertices in
    let top_poly, bot_poly = get_polygons start edge outer_vertices in
    let new_poly (p1,p2 as edge) (topf::top) (botf::bot) = (*intersect goes here*)
      let curt = ref topf in
      let curb = ref botf in
      let nxtt = ref @@ List.hd top in
      let nxtb = ref @@ List.hd bot in
      let nouter = ref [p2] in
      let ninner = ref [] in
      assert (Pt.eq topf botf);
      let get_next = function | [] -> None,[] | x::y-> Some x,y in
      let rec loop top bot =
        match get_next top with
        | Some x, ntop ->
          if is_inside x bot_poly then
            ninner := x::!ninner;

        | None, _ -> ()

      in
      (*fold allpoints*)
      ()
    in
    ()
  in
  ()
