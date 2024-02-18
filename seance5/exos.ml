open Monads

let (>>=) = Maybe.(>>=)
let (let*) x f = x >>= f

let nth_t l n = 
    if n < 0 then None else
    let rec nth_aux l n =
        match l with
            | [] -> None 
            | a::l -> if n = 0 then Some a else nth_aux l (n-1)
    in nth_aux l n

let str_of_int_m x = string_of_int x |> Maybe.return

let nth_to_str_m l n =
    let* a = nth_t l n in 
    str_of_int_m a

let pretty (n : int) (x : string) : string Maybe.t = 
    Maybe.return ("The element at index " ^ (string_of_int n) ^ " is: " ^ x)

let nth_to_pretty_str l n = 
    let* a = nth_t l n in 
    let* b = str_of_int_m a in 
    pretty n b

let pretty_print_nth l n = 
    let* a = nth_t l n in 
    let* b = str_of_int_m a in 
    let* c = pretty n b in 
    Maybe.return (print_string c)

let rec comb_opt l m = 
    if (List.length l != List.length m) then None else
    match (l,m) with 
        | x :: xs, y :: ys -> (comb_opt xs ys) >>= (fun l -> Some ((x,y) :: l))
        | ([],[]) -> Some []
        | _ -> None (* should never happen but ok *)

let print_combined l k = 
    let* combined = comb_opt l k in 
    let* pretty = 
        Maybe.return (List.map (function (a,b) -> string_of_int a ^ "<->" ^ string_of_int b) combined) in 
    Maybe.return (List.map print_endline pretty)
let car l = match l with
    | [] -> None 
    | h :: _ -> Some h

let cdr l = match l with 
    | [] -> None 
    | _ :: t -> Some t
