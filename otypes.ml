open Printf
open ExtLib
open Prelude

let atoi s = try int_of_string @@ String.strip s with _ -> fail "atoi %S" s

type ratio = { a : Z.t; b : Z.t }
type point = { x : ratio; y : ratio }
type solution = { src : point list; dst : point list; facets : int list list; }

let rec gcd a b : Z.t =
  if b = Z.zero then a else gcd b (Z.(mod) a b)

(* ratio *)
module R = struct
open Z
type t = ratio

let simplify ({ a; b } as r) =
  let ({a;b} as r) = if b < Z.zero then { a = Z.neg a; b = Z.neg b } else r in
  if a = Z.zero then { a=Z.zero; b=Z.one } else
  match Z.abs (gcd a b) with
  | n when n = Z.one -> r
  | n -> Z.{ a = a / n; b = b / n }

let make a b =
  assert (b > Z.zero);
  simplify { a; b }

let zmake a b = make (Z.of_int a) (Z.of_int b)

let int n = make n Z.one
let zero = int Z.zero
let one = int Z.one
let two = int @@ Z.of_int 2

let show r =
  match simplify r with
  | {a;b} when b = Z.one -> sprintf "%s" (Z.to_string a)
  | {a;_} when a = Z.zero -> "0"
  | {a;b} -> sprintf "%s/%s" (Z.to_string a) (Z.to_string b)

let of_string s =
  match String.split s "/" with
  | exception _ -> make (Z.of_string s) Z.one
  | a,b -> make (Z.of_string a) (Z.of_string b)

let norm x y = if x.b = y.b then x,y else {a = x.a * y.b; b = x.b * y.b},{a = y.a * x.b; b = y.b * x.b} (* common denominator *)
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

let divide x y =
  assert (x.a >= Z.zero);
  let x,y = norm x y in
  x.a / y.a, { a = (x.a mod y.a); b = x.b }

let sqr x = mul x x
let eq x y = let x,y = norm x y in x.a = y.a
let is_zero x = x.a = Z.zero

let cmp a b = let (a,b) = norm a b in a.a - b.a
let gt a b = cmp a b > Z.zero
let lt a b = cmp a b < Z.zero
let ge a b = cmp a b >= Z.zero
let le a b = cmp a b <= Z.zero

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

let which_side (a,b) pt =
  let r = R.Infix.(((b.x - a.x) * (pt.y - a.y)) - ((pt.x - a.x) * (b.y - a.y))).a in
  if r = Z.zero then
    On
  else if r < Z.zero then
    Right
  else
    Left

let is_on_line (a,b) pt =
  which_side (a,b) pt = On
end

module Problem = struct
type t = { shape : Poly.t list; skel : Line.t list; }

let input file =
  let readlni ch = input_line ch |> String.strip |> int_of_string in
  let read ch =
    let shape =
      List.init (readlni ch) begin fun _ ->
        List.init (readlni ch) (fun _ -> Pt.of_string @@ input_line ch)
      end
    in
    let skel =
      List.init (readlni ch) (fun _ -> Line.of_string @@ input_line ch)
    in
    { shape; skel }
  in
  with_open_in_txt file read

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
