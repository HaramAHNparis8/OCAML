let prod_list l = List.fold_right ( * ) l 1

let rev l = List.fold_right(fun x y -> (List.append y [x])) l []

let rec rev_r l = match l with
    | [] -> []
    | x :: xs -> List.append(rev_r xs) [x]


let rec is_prime_aux(n : int) (i : int) : bool = match i with
    | 1 -> true
    | _ -> if((n mod i )= 0) then false else (is_prime_aux n ( i - 1))

let is_prime = function
    | 0 -> false
    | 1 -> true
    | n -> is_prime_aux n (n - 1)

type 'a arbre =
    | Leaf
    | Node of 'a * ('a arbre) list

let rec count_node (t : 'a arbre) : int = match t with
    | Leaf -> 0
    | Node (_, l, m, r) -> 1 + (count_node l) + (count_node m) + (count_node r)

let example =
        Node (6, [Leaf;Leaf; Leaf; (Node (5,[Leaf]))])
