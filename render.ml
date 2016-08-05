open Gg
open Vg

open Types

(* 1. Define your image *)

let size = Size2.v 200. 200. (* mm *)
let view = Box2.v (P2.v (-0.5) (-0.5)) (Size2.v 2. 2.)

let r {R.a;b} = float a /. float b
let pt {Pt.x;y} = P2.v (r x) (r y)

let path_of_poly = function
| [] -> assert false
| x::xs -> P.close @@ List.fold_left (fun acc z -> P.line (pt z) acc) (P.sub (pt x) P.empty) xs

let image_of_shape shape =
  let paths = List.map path_of_poly shape in
  let box = P.rect (Box2.v P2.o (Size2.v 1. 1.)) P.empty in
  let path = List.fold_left (fun acc p -> P.append p acc) P.empty paths in
  I.blend
    (I.const Color.black >> I.cut ~area:(`O { P.o with P.width = 0.005 }) path)
    (I.const (Color.gray 0.2) >> I.cut ~area:(`O { P.o with P.width = 0.001 }) box)

(* 2. Render *)

let render shape ch =
  let image = image_of_shape shape in
(*
  let xmp = Vgr.xmp ~title:"Polygon" () in
  let target = Vgr_svg.target ~xmp () in
*)
  let res = 300. /. 0.0254 (* 300dpi in dots per meters *) in
  let fmt = `Png (Size2.v res res) in
  let target = Vgr_cairo.stored_target fmt in
  let warn w = Vgr.pp_warning Format.err_formatter w in
  let r = Vgr.create ~warn target (`Channel ch) in
  ignore (Vgr.render r (`Image (size, view, image)));
  ignore (Vgr.render r `End)
