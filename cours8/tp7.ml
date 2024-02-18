(*monade 정의하기*)

type context = {name : string; surname : string; age : int}

let get_full_name d = d.name ^ " " ^ d.surname
let get_age d = string_of_int d.age
let make_pretty n a =
        "Hi " ^ n ^ ", you are :" ^a ^ "years old"

let tell_age y d =
        "In" ^ (string_of_int y) ^ "year(s) you'il be " ^ (string_of_int (d.age + y) ^ "!")

let greet d =
    let name = get_full_name d in
    let age = get_age d in
    let ps = make_pretty name age in
    print_endline ps

module ContextReader : Monad with type 'a t = (context -> 'a) = struct
    type 'a t = (context -> 'a)
    let return x = fun _ -> x
    let (>>=) x f = fun w -> f (x w) w
end
