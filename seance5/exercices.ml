module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module Maybe : Monad with type 'a t = 'a option = struct 
    type 'a t = 'a option 

    let return x = Some x 

    let ( >>= ) m f = match m with 
        | None -> None 
        | Some x -> f x
end

let str_of_int_m x = string_of_int x |> Maybe.return

let nth_to_str l n =
        Maybe.(nth_t l n >>= fun x -> Maybe.return(string_of_int x))
module Error : Monad with type 'a t = ('a,string) result = struct
        type 'a t = ('a, string) result
        
        let return x = ok x
        
        let ( >>= ) m f = match m with
            |Error s -> Error s
            |ok v -> f v
end
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

let nth_to_str l n =
    let* a = nth_t l n in
    str_of_int_m a

let pretty (n : int)(x : string) : string Maybe.t =
    Maybe.return("The element at index : "^(string_of_int n)^ "is" ^ x)
let 
