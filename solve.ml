open Prelude
open Otypes
open Ops

let bb file p =
  let shape = p.Problem.shape in
  let (sol,sol_shape) = bounding_box shape |> fold_bb in
  eprintfn "bb [%s] %g" file (try resemble sol_shape shape with _ -> nan);
  sol

let best_bb file p =
  best_box file p.Problem.shape |> fold_bb |> fst

let in_box file shape box =
  let (sol,sol_shape) = fold_bb box in
  eprintfn "in_box %s [%s]: %g" (show_box box) file (try resemble sol_shape shape with _ -> nan);
  sol

let single_facet file p =
  match classify p with
  | `OriginSquare ->
    eprintfn "single_facet %s" file;
    {src = Array.of_list Otypes.orig; facets = [[0;1;2;3]]; dst = Array.of_list @@ List.hd p.Problem.shape}
  | _ -> failwith "unfitting shape :("

let origin_tri file problem =
  match classify problem with
  | `OriginTriangle ->
    eprintfn "origin_triangle %s" file;
    let p = List.hd problem.shape in
    let edges = Poly.edges p in
    begin match edges |> List.filter (fun l -> not @@ R.eq (Line.length2 l) R.one) with
    | [other] ->
      begin match List.partition (fun p -> not @@ Line.is_end other p) p with
      | [x],[a;b] ->
        { src = Array.of_list orig; facets = [[0;2;1];[0;2;3]]; dst = Array.of_list [a;x;b;x]}
      | _ -> assert false
      end
    | _ -> assert false
    end
  | _ -> failwith "unfitting shape :("

let origin_car_shape file p =
  let h = R.(div one two) in
  match classify p with
  | `QQQ (sharp, right, right2, obtuse) ->
    eprintfn "car_shape %s" file;
    { src = Array.of_list (orig2@[{x=R.zero;y=h};{x=h;y=R.one};{x=h;y=h}]);
      facets = [
        [0;4;6;3];
        [1;4;6];
        [1;5;6];
        [2;5;6;3]
      ];
      dst = [| right; right; right; sharp; right2; right2; obtuse |];
    }
  | _ -> fail "not car-shape"

let auto file p =
  try origin_tri file p with _ ->
  try single_facet file p with _ ->
  try origin_car_shape file p with _ ->
  try best_bb file p with _ ->
  try bb file p with _ -> fail "auto %s failed" file
