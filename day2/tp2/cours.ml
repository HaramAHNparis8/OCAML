let map_tr_append f = 
        let rec map_tr_helper f acc = function
                | [] -> acc
                | h :: t -> map_tr_helper f(acc @ [f h]) t in
        map_tr_helper f[]

let rec rev = function
        |[] -> []
        |h :: t -> (rev t) @ [h]

module lifo = struct
        type 'a pile = 'a list;;
        let empty : 'a pile = []

        let is_empty : 'a pille

