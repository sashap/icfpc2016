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
let make x = x
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
end

module Problem = struct
type t = { shape : Poly.t list; skel : Line.t list; }
end
