type 'a internally_decorated_tree = 
        |Leaf
        | Node of 'a * 'a internally_decorated_tree * 'a internally_decorated_tree

let rec map_bin_tree f = function 
        |Leaf -> Leaf
        | Node (v,l,r) -> Node (f v, map_bin_tree f l, map_bin_tree f r)

let succ (t: int internally_decorated_tree) : (int internally_decorated_tree) = map_bin_tree (fun x ->(x + 1)) t

let example = Node (1,Leaf,Leaf)
 let rec foldr_bin_tree f i = function 
         |Leaf -> i
         |Node (v,l,r) -> f v (foldr_bin_tree f i l) (foldr_bin_tree f i r)

let sum_tree(x : int internally_decorated_tree) = foldr_bin_tree(fun x y z -> x + y + z) 0 x

let prod_tree(x : int internally_decorated_tree) = foldr_bin_tree(fun x y z -> x * y * z) 1 x 
let search_in_tree(x : int internally_decorated_tree) = foldr_bin_tree(fun x y z -> y != 0 || z != 0) Ture x
