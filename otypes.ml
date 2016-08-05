open Printf
open ExtLib
open Prelude

let atoi s = try int_of_string @@ String.strip s with _ -> fail "atoi %S" s

type ratio = { a : int; b : int }
type point = { x : ratio; y : ratio }
type solution = { src : point list; dst : point list; facets : int list list; }

(* ratio *)
module R = struct
type t = ratio
let make a b =
  assert (b > 0);
  { a; b }

let int n = make n 1
let zero = int 0
let one = int 1
let two = int 2

let show = function {a;b=1} -> sprintf "%d" a | {a=0;_} -> "0" | {a;b} -> sprintf "%d/%d" a b
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

let modulo x y =
  assert (x.a >= 0);
  let rec loop cur y =
    let next = sub cur y in
    if next.a < 0 then cur else loop next y
  in
  loop x y

let sqr x = mul x x
let eq x y = let x,y = norm x y in x.a = y.a

let gt a b = (sub a b).a > 0
let min_ a b = if gt b a then a else b
let max_ a b = if gt a b then a else b

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
    fail ~exn "Pt.of_string %S" s
let lo p1 p2 = { x = R.min_ p1.x p2.x; y = R.min_ p1.y p2.y }
let hi p1 p2 = { x = R.max_ p1.x p2.x; y = R.max_ p1.y p2.y }
let sub a b = R.Infix.{ x = a.x - b.x; y = a.y - b.y }
let add a b = R.Infix.{ x = a.x + b.x; y = a.y + b.y }
let eq a b = R.eq a.x b.x && R.eq a.y b.y
let one = {x=R.one;y=R.one}
let zero = {x=R.zero;y=R.zero}
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
    exn -> fail ~exn "Line.of_string %S" s

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
type t = solution
let show { src; dst; facets } =
  let io = IO.output_string () in
  IO.printf io "%d\n" (List.length src);
  List.iter (fun p -> IO.printf io "%s\n" (Pt.show p)) src;
  IO.printf io "%d\n" (List.length facets);
  List.iter (fun l -> IO.printf io "%d %s\n" (List.length l) (String.concat " " @@ List.map string_of_int l)) facets;
  List.iter (fun p -> IO.printf io "%s\n" (Pt.show p)) dst;
  IO.close_out io
end
