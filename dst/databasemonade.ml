
type ('a, 'e) t = ('a, 'e) result = 
    | Ok of 'a
    | Error of 'e

module Error : Monad with type 'a t = ('a, string) result = struct
    type 'a t = ('a, string) result 

    let return x = Ok x

    let ( >>= ) m f = match m with 
        | Error s -> Error s
        | Ok v -> f v

end

let example_db = (["Alice" ; "Bob"],["Smith" ; "Joe"],["001" ; "002"])

module type Database = sig 
    type database = string list * string list * string list

    val empty : database

    val is_valid : database -> bool 

    val print_db : database -> (unit list, string) t 

    val find_and_print : database -> (string * string * string -> bool) -> (unit list, string) t 
    
    val insert_into_database : string -> string -> string -> database -> (database, string) t

end


module MyDatabase = struct
    type database = string list * string list * string list
    type 'a Error.t = Ok of 'a | Error of string

    let empty : database = ([], [], [])

    let is_valid (names, surnames, ids) =
        let len = List.length names in
        List.length surnames = len && List.length ids = len
    let print_db (names, surnames, ids) =
        let print_student name surname id =
            Printf.printf "Name: %s Surname: %s Student Number: %s\n" name surname id
        in
        try
            List.iteri print_student names surnames ids;
            Ok ()
        with
        | Invalid_argument _ -> Error "List lengths are not equal"
end 
