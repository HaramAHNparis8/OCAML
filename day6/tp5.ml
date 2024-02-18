
module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
end

type context = {name : string; surname : string; age : int}

let get_full_name d = d.name ^" " ^ d.surname
let get_age d = string_of_int d.age
let make_pretty n a = 
        "Hi" ^ n ^", you are :" ^ a ^ "years old."
let tell_age y d =
        "In" ^ (string_of_int y) ^" year(s) you'll be " ^ (string_of_int (d.age + y) ^ "!")


module ContextReader : Monad with type 'a t = (context -> 'a) = struct type 'a t = (context -> 'a)
let return x = fun _ -> x
let ( >>= ) x f = fun w -> f (x w) w
end


