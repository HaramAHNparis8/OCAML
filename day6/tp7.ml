 
let rec traverse_list(f: 'a -> 'b)(l: 'a list):('b list) option =
        |[] -> some [];
        |X :: xs -> let* a = l x
                    let* b = traverse_list( f x)
                    return (a :: b)
maybe _mod_list l n = traverse_list;;
(*
let maybe_mod_list x y = match x with
    | (x,y) = (first xs)(first ys) in if y <>0 then Some(x,maybe_mod_list xs) filter (fun x -> x <> None) else if(filter)
*)
let rec maybe_mod_list x y = match x with
        | [] -> []
        | (x :: xs) -> 
                match maybe_mod_t x y with
                    |None -> maybe_mod_t xs y
                    |Some z -> Some z :: maybe_mod_list xs y;;


