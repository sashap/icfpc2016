open Gg
open Vg

open Prelude
open Otypes

(* 1. Define your image *)

let size = Size2.v 200. 200. (* mm *)

let r {a;b} = Z.to_float a /. Z.to_float b
let pt {x;y} = P2.v (r x) (r y)

let path_of_poly = function
| [] -> assert false
| x::xs -> P.close @@ List.fold_left (fun acc z -> P.line (pt z) acc) (P.sub (pt x) P.empty) xs

let box =
  let path = P.rect (Box2.v P2.o (Size2.v 1. 1.)) P.empty in
  I.const (Color.gray 0.2) >> I.cut ~area:(`O { P.o with P.width = 0.001 }) path

let image_of_shape shape =
  let path = List.fold_left (fun acc p -> P.append (path_of_poly p) acc) P.empty shape in
  I.const Color.black >> I.cut ~area:(`O { P.o with P.width = 0.008 }) path

let image_of_skel skel =
  let path = List.fold_left (fun acc (a,b) -> P.line (pt b) @@ P.sub (pt a) acc) P.empty skel in
  I.const Color.red >> I.cut ~area:(`O { P.o with P.width = 0.002 }) path

(* 2. Render *)

let render { Problem.shape; skel } ch =
(*
  let (lo,hi) = Ops.bounding_box shape in
  let bb = Pt.sub hi lo in
  let dim = max (r bb.x) (r bb.x) in
  let view_size = (max dim 1.) *. 1.5 in
  let shift_x = R.Infix.(view_size /. 2. -. (r lo.x +. r (bb.x / R.two))) in
  let shift_y = R.Infix.(view_size /. 2. -. (r lo.y +. r (bb.y / R.two))) in
*)
  let (lo,hi) = Ops.bounding_box ([{x=R.zero;y=R.zero}; {x=R.one;y=R.one}] :: shape) in
  let view_size = 1.25 *. 2. *. List.fold_left max 0. (List.map (fun p -> abs_float @@ 0.5 -. r p) [lo.x; lo.y; hi.x; hi.y]) in
  let shift_x = view_size /. 2. -. 0.5 in
  let shift_y = view_size /. 2. -. 0.5 in

  let view = Box2.v P2.o (Size2.v view_size view_size) in
  let image = List.fold_left (fun a b -> I.blend b a) (I.const Color.white) [box; image_of_shape shape; image_of_skel skel] in
(*
  let xmp = Vgr.xmp ~title:"Polygon" () in
  let target = Vgr_svg.target ~xmp () in
*)
  let res = 300. /. 0.0254 (* 300dpi in dots per meters *) in
  let fmt = `Png (Size2.v res res) in
  let target = Vgr_cairo.stored_target fmt in
  let warn w = Vgr.pp_warning Format.err_formatter w in
  let r = Vgr.create ~warn target (`Channel ch) in
  ignore (Vgr.render r (`Image (size, view, I.move (P2.v shift_x shift_y) image)));
  ignore (Vgr.render r `End)

let render_problem p file = with_open_out_bin file (render p)
let render_poly p file = with_open_out_bin file (render { Problem.shape = [p]; skel = [] })
