open Printf
open ExtLib
open Prelude

let atoi s = try int_of_string @@ String.strip s with _ -> fail "atoi %S" s

type ratio = { a : Z.t; b : Z.t }
type point = { x : ratio; y : ratio }
type solution = { src : point array; dst : point array; facets : int list list; }

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

let make' a b =
  assert (b > Z.zero);
  { a ; b }

let make a b =
  assert (b > Z.zero);
  simplify { a; b }

let zmake a b = make (Z.of_int a) (Z.of_int b)

let int n = zmake n 1
let zero = int 0
let one = int 1
let two = int 2

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
let map f x = { a = f x.a; b = f x.b; }
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

let to_float {a;b} = Z.to_float a /. Z.to_float b

let compare a b = let (a,b) = norm (simplify a) (simplify b) in Z.sign @@ Z.sub a.a b.a
let gt a b = compare a b > 0
let lt a b = compare a b < 0
let ge a b = compare a b >= 0
let le a b = compare a b <= 0

let min_ a b = if gt b a then a else b
let max_ a b = if gt a b then a else b

let z_random a =
  try
    match Z.to_int a with
    | exception _ -> Z.of_float @@ Random.float @@ Z.to_float a
    | a when a >= 1073741824 -> Z.of_float @@ Random.float @@ float a
    | a -> Z.of_int @@ Random.int a
  with exn -> fail "z_random %S : %s" (Z.to_string a) (Printexc.to_string exn)

let random {a;b} =
  assert (a <> Z.zero);
  let (a,b) =
    if a >= Z.of_int 1_000 then
      a, b
    else
      let n = Z.of_int 1_000 / a in
      Z.mul a n, Z.mul b n
  in
  make (z_random a) b

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
let mul a k = R.Infix.{ x = a.x * k; y = a.y * k }
let div a k = R.Infix.{ x = a.x / k; y = a.y / k }
let dot a b = R.Infix.(a.x * b.x + a.y * b.y)
let cross a b = R.Infix.(a.x * b.y - a.y * b.x)
let eq a b = R.eq a.x b.x && R.eq a.y b.y
let one = {x=R.one;y=R.one}
let zero = {x=R.zero;y=R.zero}
let pi = 4.0 *. atan 1.0
let twopi = 2. *. pi
let angle a b = let f = atan2 (R.to_float @@ cross a b) (R.to_float @@ dot a b) in if f < 0. then twopi +. f else f
let compare a b =
  match R.compare (dot a a) (dot b b) with
  | 0 -> compare (angle a one) (angle b one)
  | n -> n
let rotate center angle pt =
  let angle = angle *. pi /. 180. in
(*   let scale = 1_000_000_000 in *)
  let scale = Z.of_int64 4611686018427387904L in
  let fscale = Z.to_float scale in
  let sin = R.make' (Z.of_float @@ ((sin angle) *. fscale)) scale in
  let cos = R.map Z.sqrt R.Infix.(R.make scale scale - R.map (fun x -> Z.pow x 2) sin) in
(*   let cos = R.make' (Z.of_float @@ ((cos angle) *. fscale)) (Z.of_int64 scale) in *)
  let m = R.Infix.{x = R.simplify (pt.x - center.x); y = R.simplify (pt.y - center.y)} in (*compensate center*)
  let r = R.Infix.{x = R.simplify ((m.x * cos) - (m.y * sin)); y = R.simplify ((m.x * sin) + (m.y * cos))} in (* rotate *)
  {x = R.add r.x center.x; y = R.add r.y center.y}
(*
let normalize a = let {x;y} = a in let n = dot a a in { x = R.div x n; y = R.div y n }
let rotate_to a b =
  let open R.Infix in
  let {x=x1;y=y1} = normalize a in
  let {x=x2;y=y2} = normalize b in
  let a = x1 * x2 + y1 * y2 in
  let b = x2 * y1 - x1 * y2 in
  fun {x;y} -> { x = a * x + b * y; y = a * y - b * x }
*)

end

module Points = Set.Make(Pt)

module Poly = struct
type t = Pt.t list
let make = function [] -> assert false | x -> x
let show l = String.concat " " @@ List.map Pt.show l
let of_string s = String.nsplit s " " |> List.map Pt.of_string
let rotate center angle p = List.map (Pt.rotate center angle) p
let connect p =
  let rec loop acc = function
  | [] -> assert false
  | x::(x2::_ as tl) -> loop ((x,x2)::acc) tl
  | [x] -> acc, x
  in
  loop [] p
let edges p =
  let e,last = connect p in
  List.rev ((last,List.hd p) :: e)
end

let orig = Poly.of_string "0,0 1,0 1,1 0,1"
let orig2 = Poly.of_string "0,0 0,1 1,1 1,0"

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

let vector (a,b) = Pt.sub b a
let eq (a,b) (c,d) = (Pt.eq a c && Pt.eq b d) || (Pt.eq b c && Pt.eq a d)

let length2 ((a,b) : t) = (* (x - x)^2 + (y - y)^2 no sqrt *)
  R.Infix.(R.sqr (a.x - b.x) + R.sqr (a.y - b.y))

let length_imprecise l = R.map Z.sqrt (length2 l)

let is_zero (a,b) = Pt.eq a b
let is_end (a,b) p = Pt.eq a p || Pt.eq b p

let dot a b = Pt.dot (vector a) (vector b)

let show (a,b) = Pt.show a ^ " " ^ Pt.show b

let which_side (a,b) pt =
  let r = R.Infix.(((b.x - a.x) * (pt.y - a.y)) - ((pt.x - a.x) * (b.y - a.y))).a in
  if r = Z.zero then
    On
  else if r < Z.zero then
    Right
  else
    Left

let get_ord f s = if R.gt f s then f,s else s,f
let is_on_line (a,b) pt =
  let open R in
  Pt.eq pt a || Pt.eq pt b ||
    (let xg,xs = get_ord a.x b.x in
     let yg,ys = get_ord a.y b.y in
     which_side (a,b) pt = On && (le pt.x xg) && (ge pt.x xs) && (le pt.y yg) && (ge pt.y ys))

let get_intersect (a1,b1) (a2,b2) = (*kx+ny=c*)
  let open R in
  let open R.Infix in
  let get_coefs (a : point) b =
    let k = b.y - a.y in
    let n = a.x - b.x in
    let c = (mul k a.x) + (mul n a.y) in
    (k,n,c)
  in
  let k1,n1,c1 = get_coefs a1 b1 in
  let k2,n2,c2 = get_coefs a2 b2 in
  match (k1 * n2) - (k2 * n1) with
  | det when R.eq det R.zero -> None
  | det ->
    let x = simplify (((n2*c1) - (n1*c2))/det) in
    let y = simplify (((k1*c2) - (k2*c1))/det) in
    let x1g,x1s = get_ord a1.x b1.x in
    let y1g,y1s = get_ord a1.y b1.y in
    let x2g,x2s = get_ord a2.x b2.x in
    let y2g,y2s = get_ord a2.y b2.y in
    if (le x x1g) && (ge x x1s) && (le x x2g) && (ge x x2s) &&
       (le y y1g) && (ge y y1s) && (le y y2g) && (ge y y2s) then
      Some {x;y}
    else
      None

let get_on_line (a,b) perc = (*get point on line represented by %*)
  let open R.Infix in
  (* let perc = R.make (Z.of_int @@ int_of_float (perc *. 1000000000.)) (Z.of_int 1000000000) in *)
  {x = a.x + ((b.x - a.x) * perc); y = a.y + ((b.y - a.y) * perc)}

end

let readlni ch = input_line ch |> String.strip |> int_of_string
let readln_pt ch = Pt.of_string @@ input_line ch
let readln_poly ch = List.init (readlni ch) (fun _ -> readln_pt ch)

module Problem = struct
type t = { shape : Poly.t list; skel : Line.t list; }

let input file =
  let read ch =
    let shape = List.init (readlni ch) begin fun _ -> readln_poly ch end in
    let skel = List.init (readlni ch) (fun _ -> Line.of_string @@ input_line ch) in
    { shape; skel }
  in
  with_open_in_txt file read

end

module Solution = struct
type t = solution
let show { src; dst; facets; } =
  let facets = List.filter (fun l -> List.length l > 2) facets in
  let io = IO.output_string () in
  IO.printf io "%d\n" (Array.length src);
  Array.iter (fun p -> IO.printf io "%s\n" (Pt.show p)) src;
  IO.printf io "%d\n" (List.length facets);
  List.iter (fun l -> IO.printf io "%d %s\n" (List.length l) (String.concat " " @@ List.map string_of_int l)) facets;
  Array.iter (fun p -> IO.printf io "%s\n" (Pt.show p)) dst;
  IO.close_out io

let input file =
  let read ch =
    let src = Array.init (readlni ch) begin fun _ -> readln_pt ch end in
    let facets = List.init (readlni ch) begin fun _ ->
      match String.nsplit (input_line ch) " " with
      | [] -> assert false
      | n::xs ->
        assert (List.length xs = int_of_string n);
        List.map int_of_string xs
    end in
    let dst = src |> Array.map (fun _ -> readln_pt ch) in
    { src; dst; facets; }
  in
  with_open_in_txt file read

let src s = List.map (List.map (fun i -> s.src.(i))) s.facets
let dst s = List.map (List.map (fun i -> s.dst.(i))) s.facets
end
