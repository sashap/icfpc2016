open Gg
open Vg

open Otypes

(* 1. Define your image *)

let size = Size2.v 200. 200. (* mm *)

let shift = 0.25
let view = let l = (1.+.2.*.shift) in Box2.v P2.o (Size2.v l l)

let r {R.a;b} = float a /. float b
let pt {Pt.x;y} = P2.v (r x) (r y)

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
  ignore (Vgr.render r (`Image (size, view, I.move (P2.v shift shift) image)));
  ignore (Vgr.render r `End)
