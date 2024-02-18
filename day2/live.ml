type binary_tree = 
        | Leaf
        | Node of binary_tree * binary_tree

let example_1 = Leaf
let example_2 = Node (Leaf, Leaf)
let example_3 = Node (Node (Leaf, Leaf), Node(Leaf, Leaf))
let example_4 = Node (Leaf, Node(Node(Leaf, Leaf),Leaf))

let rec count_nodes (t : binary_tree) : int = match t with
        | Leaf -> 0
        | Node(l,r) -> 1 + (count_nodes l) + (count_nodes r)

let rec count_nodes_tr(t : binary_tree)(acc : int) : int = 
        match t with
        |Leaf -> acc
        |Node(l,r) -> 
                        (count_nodes_tr l (1 + (count_nodes_tr r acc)))
type 'a decorated_binary_tree =
        | Leaf of 'a
        | Node of 'a * a  decorarted_binary_ tree * decorated_binary_tree
let example_4_1 : int decorated_binary_tree =  
        Node (0, (Leaf 1), Node (2, (Leaf 3),(Leaf 4)))
let example_4_1 : string decorated_binary_tree = 
        Node("A", Node("B", (Leaf "C"), (Leaf "D")),(Leaf F))

let rec tree_sum (t : int decorated_binary) : int =
        match t with
        | Leaf v -> v
        | Node (v,l,r) -> v + (tree_sum l) + (tree_sum r)

let rec tree_sum_tr (t : int decorated_binary_tree)(acc : int) : int
        match t with
        |Leaf v -> v + acc
        |Node (v,l,r) -> tree_sum_tr l (v + (tree_sum_tr r acc))
let rec search_tree : (t : 'a decorated_binary_tree) (e : 'a) : bool
= match t with
    | Leaf v -> (v = e)
    | Node (v, l, r) -> (v = e) || (search_tree l e) || (search_tree r e)
    
