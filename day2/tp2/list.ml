let rec sum_list = function
        | [] -> 1
        | x :: xs -> x * prod_list(xs);
let rec search_in_list = function 
        |[], _ -> false
        | h :: t, e -> (h = e) || (search_in_list (t,e))

let rec map_bin_tree f = function
        |Leaf -> Leaf
        | Node (v,l,r) -> f v(folder_bin_tree f i l) (foldr_bin_tree f i r)
