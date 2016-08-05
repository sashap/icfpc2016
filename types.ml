open Printf
open ExtLib

let atoi s = try int_of_string @@ String.strip s with _ -> Exn.fail "atoi %S" s

(* ratio *)
module R = struct
type t = { a : int; b : int }
let make a b =
  assert (b > 0);
  { a; b }
let zero = {a = 0; b = 1}
let show = function {a;b=1} -> sprintf "%d" a | {a;b} -> sprintf "%d/%d" a b
let of_string s =
  match String.split s "/" with
  | exception _ -> make (atoi s) 1
  | a,b -> make (atoi a) (atoi b)

let norm x y = if x.b = y.b then x,y else {a = x.a * y.b; b = x.b * y.b},{a = y.a * x.b; b = y.b * x.b} (* common denominator *)
(* let simplify x = ? *)
let mul x y = {a = x.a * y.a ; b = x.b * y.b}
let div x y = mul x {a = y.b; b = y.a}
let add x y =
  let x,y = norm x y in
  assert (x.b = y.b);
  {a = x.a + y.a; b = x.b}

let sub x y =
  let x,y = norm x y in
  assert (x.b = y.b);
  {a = x.a - y.a; b = x.b}

let sqr x = mul x x
let eq x y = let x,y = norm x y in x.a = y.a
end

(* point *)
module Pt = struct
type t = { x : R.t; y : R.t }
let show {x;y} = sprintf "%s,%s" (R.show x) (R.show y)
let of_string s =
  try
    let (x,y) = String.split s "," in
    { x = R.of_string x; y = R.of_string y }
  with exn ->
    Exn.fail ~exn "Pt.of_string %S" s
end

module Poly = struct
type t = Pt.t list
let make = function [] -> assert false | x -> x
end

module Line = struct
type t = Pt.t * Pt.t
let make a b = a,b
let of_string s =
  try
    let (a,b) = String.split (String.strip s) " " in
    Pt.of_string a, Pt.of_string b
  with
    exn -> Exn.fail ~exn "Line.of_string %S" s

let length2 ((a,b) : t) = (* (x - x)^2 + (y - y)^2 no sqrt *)
  let open Pt in
  R.add (R.sqr (R.sub a.x b.x)) (R.sqr (R.sub a.y b.y))

let which_side_of_line (a,b) pt = (* -1 - left , 1 - right , 0 - on *)
  let open Pt in
  (R.sub (R.mul (R.sub b.x a.x) (R.sub pt.y a.y)) (R.mul (R.sub pt.x a.x) (R.sub b.y a.y))).a

let is_on_line (a,b) pt =
  which_side_of_line (a,b) pt = 0
end

let mirror (l1,l2) pt =
  let open Pt in
  let dx = R.sub l2.x l1.x in
  let dy = R.sub l2.y l1.y in
  let a = R.div (R.sub (R.sqr dx) (R.sqr dy)) (R.add (R.sqr dx) (R.sqr dy)) in
  let b = R.div (R.mul (R.make 2 1) (R.mul dx dy)) (R.add (R.sqr dx) (R.sqr dy)) in
  { x = R.add l1.x (R.add (R.mul a (R.sub pt.x l1.x)) (R.mul b (R.sub pt.y l1.y)));
    y = R.add l1.y (R.sub (R.mul b (R.sub pt.x l1.x)) (R.mul a (R.sub pt.y l1.y)))}

let fold_over_line ln poly =
  List.map begin fun pt ->
    match Line.which_side_of_line ln pt with
    | -1 -> mirror ln pt
    | _ -> pt
  end poly


module Problem = struct
type t = { shape : Poly.t list; skel : Line.t list; }
end

module Solution = struct
type source = Pt.t list
type facets = int list
type solution = source * facets
type destination = Pt.t list
end
