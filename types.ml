open Printf
open ExtLib

let atoi s = try int_of_string @@ String.strip s with _ -> Exn.fail "atoi %S" s

(* ratio *)
module R = struct
type t = { a : int; b : int }
let make a b =
  assert (b > 0);
  { a; b }
let show = function {a;b=1} -> sprintf "%d" a | {a;b} -> sprintf "%d/%d" a b
let of_string s =
  match String.split s "/" with
  | exception _ -> make (atoi s) 1
  | a,b -> make (atoi a) (atoi b)

let norm x y = if x.b = y.b then x,y else {a = x.a * y.b; b = x.b * y.b},{a = y.a * x.b; b = y.b * x.b} (* common denominator *)
(* let simplify x = ? *)
let mul x y = {a = x.a * y.a ; b = x.b * y.b}
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

let is_on_line (a,b) pt =
  R.eq (R.add (length2 (a,pt)) (length2 (pt,b))) (length2 (a,b))
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
