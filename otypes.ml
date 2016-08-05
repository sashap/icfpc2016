open Printf
open ExtLib

let atoi s = try int_of_string @@ String.strip s with _ -> Exn.fail "atoi %S" s

type ratio = { a : int; b : int }
type point = { x : ratio; y : ratio }

(* ratio *)
module R = struct
type t = ratio
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

module Infix = struct
let (-) = sub
let (+) = add
let ( * ) = mul
let (/) = div
end

end

(* point *)
module Pt = struct
type t = point
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

type side = On | Left | Right

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
  R.Infix.(R.sqr (a.x - b.x) + R.sqr (a.y - b.y))

let which_side_of_line (a,b) pt =
  let r = R.Infix.(((b.x - a.x) * (pt.y - a.y)) - ((pt.x - a.x) * (b.y - a.y))).a in
  match r with
  | 0 -> On
  | -1 -> Left
  | 1 -> Right
  | _ -> assert false

let is_on_line (a,b) pt =
  which_side_of_line (a,b) pt = On
end

module Problem = struct
type t = { shape : Poly.t list; skel : Line.t list; }
end

module Solution = struct
type source = Pt.t list
type facets = int list
type solution = source * facets
type destination = Pt.t list
end
