module type Monad = sig
        type 'a t
        val return : 'a -> 'a t
        val bind : 'a t -> ('a -> 'b t) -> 'b t
end

let nth_t l n = 
        if n < 0 then None else
        let rec nth_aux l n = 
          match l with
          |[] -> None
          |a :: l -> if n = 0 then Some a else nth_aux l (n -1)
        in nth_aux l n

let nth_to_str_attempt l n = match (nth_t l n) with
    | None -> None
    | Some x -> Some(string_of_int x);

module Maybe : Monad = struct
    type 'a t = 'a option


let (let*) x f = x >>= f
let nth_to_str_m_1 l n = let* a = nth_t l n in str_of_int_m a
let nth_to_str_m_1 l n =
let* a = nth_t l n in
let* b = str_of_int_m a in print_m b
