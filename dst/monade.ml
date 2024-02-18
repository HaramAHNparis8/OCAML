open Maybe

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

let str_of_int_m x = Maybe.return (string_of_int x)



let nth_t l n = 
    if n < 0 then None else
    let rec nth_aux l n =
        match l with
            | [] -> None 
            | a::l -> if n = 0 then Some a else nth_aux l (n-1)
    in nth_aux l n

let nth_to_str n l =
    nth_t l n >>= str_of_int_m

let str_of_pretty n x = Maybe.return (Printf.printf "The element at index %d is : %d" n x)

let nth_to_pretty_str n l =
    nth_t l n >>= str_of_pretty n


let rec comb_opt l k = match l, k with
    | [], [] -> Maybe.return []
    | h1::t1, h2::t2 ->
                    comb_opt t1 t2 >>= fun r ->
                    Maybe.return ((h1, h2) :: r)
    | _, _ -> None

let rec print_combined l1 l2 = match l1, l2 with
    | [], [] -> Maybe.return ()
    | h1 :: t1, h2 :: t2 ->
        Maybe.return (Printf.printf " %d <-> %d\n" h1 h2) >>= fun _ ->
        print_combined t1 t2
    | _, _ -> None

let cdr l = match l with 
    | [] -> None 
    | _ :: t -> Some t
(*끝에 있는 t는 리스트의 요소가 아니라 리스트 *)


let car l = match l with
    | [] -> None
    | h :: _ -> Maybe.return h

let cadr l = match l with
    | [] | [_] -> None
    | _ :: x :: _ -> Maybe.return x

let caddr l = match l with
    | [] | [_] | [_; _] -> None
    | _ :: _ :: x :: _ -> Maybe.return x

let plus_car_cadr_caddr l =
  car l >>= fun a ->
  cadr l >>= fun b ->
  caddr l >>= fun c ->
  Maybe.return (a + b + c)

let check_dups l =
  cadr l >>= fun b ->
  caddr l >>= fun c ->
  Maybe.return (b = c)

let mod_t x y = match y with
    | 0 -> None 
    | _ -> Maybe.return (x mod y)

let rec maybe_mod_list l n = match l with
    | [] -> Maybe.return []
    | h :: t -> 
        match mod_t h n with
        | Some r -> maybe_mod_list t n >>= fun rt -> Maybe.return (r :: rt)
        | None -> None


type 'a tree = 
    | Leaf 
    | Node of 'a * 'a tree * 'a tree 

let left_child t = match t with
    | Leaf -> None 
    | Node (_,l,_) -> Maybe.return l 

let right_child t = match t with
    | Leaf -> None 
    | Node (_,_,r) -> Maybe.return r

let get_val t = match t with 
    | Leaf -> None 
    | Node (v,_,_) -> Maybe.return v

let example_tree = 
    Node (1,
        Node (2, Leaf, Leaf),
        Node (3, 
            Node (4, Leaf, Leaf),
            Node (5, Leaf, Leaf)))


let rec follow_path t path =
  match t, path with
  | Leaf, _ -> None
  | _, [] -> Some []
  | Node (v, l, r), "l" :: tp -> 
      follow_path l tp >>= fun lv -> 
      Maybe.return (v :: lv)
  | Node (v, l, r), "r" :: tp -> 
      follow_path r tp >>= fun rv -> 
      Maybe.return (v :: rv)
  | _ -> None
(*
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

(*
module MyDatabase = struct
    type database = string list * string list * string list

    let empty : database = ([], [], [])

    let is_valid (names, surnames, ids) =
        let len = List.length names in
        List.length surnames = len && List.length ids = len
end *) *)
