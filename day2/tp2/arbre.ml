type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

type 'a internally_decorated_tree =
  | Leaf of 'a
  | Node of 'a * 'a internally_decorated_tree * 'a internally_decorated_tree

let rec serialise_prefix tree =
  match tree with
  | Leaf(v) -> [v]
  | Node(v, left, right) ->
    let sv = [v] in
    let sl = serialise_prefix left in
    let sr = serialise_prefix right in
    sv @ sl @ sr

let rec serialise_postfix tree =
  match tree with
  | Leaf(v) -> [v]
  | Node(v, left, right) ->
    let sl = serialise_postfix left in
    let sr = serialise_postfix right in
    let sv = [v] in
    sl @ sr @ sv

let rec serialise_infix tree =
  match tree with
  | Leaf(v) -> [v]
  | Node(v, left, right) ->
    let sl = serialise_infix left in
    let sv = [v] in
    let sr = serialise_infix right in
    sl @ sv @ sr

let () =
  let tree1 = Node(1, Leaf, Node(2, Node(3, Leaf, Leaf), Node(4, Leaf, Leaf))) in
  let tree2 = Node("A", Node("B", Leaf, Leaf), Leaf) in

  let result1_prefix = serialise_prefix tree1 in
  let result1_postfix = serialise_postfix tree1 in
  let result1_infix = serialise_infix tree1 in

  let result2_prefix = serialise_prefix tree2 in
  let result2_postfix = serialise_postfix tree2 in
  let result2_infix = serialise_infix tree2 in

  Printf.printf "Tree1 Prefix: [%s]\n" (String.concat ", " (List.map string_of_int result1_prefix));
  Printf.printf "Tree1 Postfix: [%s]\n" (String.concat ", " (List.map string_of_int result1_postfix));
  Printf.printf "Tree1 Infix: [%s]\n" (String.concat ", " (List.map string_of_int result1_infix));

  Printf.printf "Tree2 Prefix: [%s]\n" (String.concat ", " result2_prefix);
  Printf.printf "Tree2 Postfix: [%s]\n" (String.concat ", " result2_postfix);
  Printf.printf "Tree2 Infix: [%s]\n" (String.concat ", " result2_infix);

let tree_height tree =
  let rec calculate_height t =
    match t with
    | Leaf -> 0
    | Node(_, left, right) ->
      let hl = calculate_height left in
      let hr = calculate_height right in
      1 + max hl hr
  in
  calculate_height tree
let test_tree =
  Node(1,
    Node(2, Leaf, Leaf),
    Node(3, Node(4, Leaf, Leaf), Node(5, Node(6, Leaf, Leaf), Leaf))
  )

let result = tree_height test_tree
Printf.printf "Hauteur de l'arbre: %d\n" result


