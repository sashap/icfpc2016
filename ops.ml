open Otypes

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
