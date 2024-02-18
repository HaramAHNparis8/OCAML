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

module Error : Monad with type 'a t = ('a, string) result = struct
    type 'a t = ('a, string) result 

    let return x = Ok x
    
    let ( >>= ) m f = match m with 
        | Error s -> Error s
        | Ok v -> f v

end
