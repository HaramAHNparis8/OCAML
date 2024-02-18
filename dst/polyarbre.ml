let rec succ_list l =
        match l with
        |[] -> []
        |x :: x1 -> x + 1 :: succ_list (x1)

let rec is_prime_aux n i = 
        if i == 1 then true
        else if n mod i == 0 then false
        else is_prime_aux n (i - 1)

let prime n = is_prime_aux n (n -1)

let rec seq_prime n =
    if n < 2 then ()
    else (
        if prime n then (print_int n; print_string " ");  
        seq_prime (n - 1)  
    )

let rec catalan n =
    if n == 0 then 1
    else (2 * (2 * n - 1) *(catalan(n - 1)) / (n + 1))

let print_catalans f n =
    let rec aux i acc =
        if i > n then acc
        else aux (i + 1) (acc ^ (if acc = "" then "" else " ") ^ string_of_int (f i))
    in
    aux 0 ""

type expr =
    | Literal of int
    | Add of expr * expr
    | Var of string

let rec eval_expr (e : expr) : int = match e with
    | Literal x -> eval_literal x
    | Add (a, b) -> eval_add a b

and eval_literal x = x  

and eval_add e1 e2 = eval_expr e1 + eval_expr e2  

type context = {x : int; y : int; z : int}

let eval_var (cntxt : context) (var : string) : int option =

    match var with
    | "x" -> Some cntxt.x
    | "y" -> Some cntxt.y
    | "z" -> Some cntxt.z
    | _ -> None

let cntxt = {x = 10; y = 20; z = 30}


let _ =
  print_endline (match eval_var cntxt "x" with
                 | Some value -> string_of_int value
                 | None -> "None");
  print_endline (match eval_var cntxt "y" with
                 | Some value -> string_of_int value
                 | None -> "None");
  print_endline (match eval_var cntxt "z" with
                 | Some value -> string_of_int value
                 | None -> "None");
  print_endline (match eval_var cntxt "w" with
                 | Some value -> string_of_int value
                 | None -> "None")

type 'a decorated_binary_tree =
    | Empty
    | Node of 'a * 'a decorated_binary_tree * 'a decorated_binary_tree

let arbre = Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty))

type binary_tree =
    | Empty
    | Node of int * binary_tree * binary_tree

type 'a decorated_binary_tree =
    | Empty
    | Node of 'a * 'a decorated_binary_tree * 'a decorated_binary_tree

let arbre = Node(1, Node(2, Empty, Node(6,Empty,Empty)), Node(3, Empty, Empty))

let rec count_nodes tree = match tree with
    | Empty -> 0
    | Node (_, left, right) -> 1 + count_nodes left + count_nodes right

let rec sum_tree arbre = match arbre with
    | Empty -> 0
    | Node (v, l, r) -> v + sum_tree l + sum_tree r

let rec produit_tree arbre = match arbre with
    | Empty -> 0
    | Node (_, l, r) -> 1 * produit_tree l * produit_tree r

type 'a internally_decorated_tree =
    | Empty
    | Node of 'a * 'a internally_decorated_tree * 'a internally_decorated_tree

let rec map_tree f tree = match tree with
    | Empty -> Empty
    | Node (value, left, right) ->
        Node (f value, map_tree f left, map_tree f right)

let succ tree = map_tree (fun x -> x + 1) tree

let rec map_tree_carre f tree = match tree with
    | Empty -> Empty
    | Node(value, left, right) ->
        Node(f value, map_tree_carre f left, map_tree_carre f right)

let carre  x = x * x

 
let succ_pro tree = map_tree (fun x -> carre x) tree
(* 테스트 트리 생성 *)
let test_tree = Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty))

(* 함수 실행 및 결과 확인 *)
let result_tree = succ test_tree

let res_tree = succ_pro test_tree

let rec search_in_tree t e = match t with
    | Empty -> false
    | Node (value, left, right) ->
        if value = e then true
        else search_in_tree left e || search_in_tree right e

let res_tree_search = search_in_tree test_tree 0


(* 직렬화 함수 정의 *)
let rec serialise_prefix = function
    | Leaf -> []
    | Node (value, left, right) -> [value] @ serialise_prefix left @ serialise_prefix right

let rec serialise_infix = function
    | Leaf -> []
    | Node (value, left, right) -> serialise_infix left @ [value] @ serialise_infix right

let rec serialise_postfix = function
    | Leaf -> []
    | Node (value, left, right) -> serialise_postfix left @ serialise_postfix right @ [value]

let first_example = Node ("a",
                        Node ("b", Leaf,
                            Node ("c", Leaf, Leaf)),
                        Node ("d",
                            Node ("e", Leaf, Leaf), Leaf))

let second_example = Node ("0",
                        Node ("1",
                            Node ("2", Leaf, Leaf),
                            Node ("3", Leaf,
                                Node ("4", Leaf, Leaf))),
                        Node ("5",
                            Node ("6", Leaf, Leaf), Leaf))

(*
let _ =
  print_endline ("Prefix first_example: " ^ String.concat " " (serialise_prefix first_example));
  print_endline ("Prefix second_example: " ^ String.concat " " (serialise_prefix second_example));
  print_endline ("Infix first_example: " ^ String.concat " " (serialise_infix first_example));
  print_endline ("Infix second_example: " ^ String.concat " " (serialise_infix second_example));
  print_endline ("Postfix first_example: " ^ String.concat " " (serialise_postfix first_example));
  print_endline ("Postfix second_example: " ^ String.concat " " (serialise_postfix second_example))
*)
let rec height tree = match tree with
    | Empty -> 0
    | Node (_, left, right) ->
        1 + max (height left) (height right)

let res_h arbre = height first_example;

let rec com_opt_map = match l1, l2 with
    |[], _ | _,[] -> None
    |x1 :: y1 , x2 :: y2 -> (x1, x2) com_opt_map(y1, y2)
