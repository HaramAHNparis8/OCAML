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
let nth_t l n = 
    if n < 0 then None else
    let rec nth_aux l n =
        match l with
            | [] -> None 
            | a::l -> if n = 0 then Some a else nth_aux l (n-1)
    in nth_aux l n

let str_of_int_m x = string_of_int x |> Maybe.return

let nth_to_pretty_str l n =
  let rec aux l n current_index =
    match l with
    | [] -> None
    | x :: xs ->
      if current_index = n then
        Some (Printf.sprintf "les element de l'indice %d est: %d" current_index x)
      else
        aux xs n (current_index + 1)
  in
  if n < 0 then None else aux l n 0

let rec comb_opt l k =
  match (l, k) with
  | ([], []) -> Some []
  | (x :: xs, y :: ys) -> 
      (match comb_opt xs ys with
      | Some rest -> Some ((x, y) :: rest)
      | None -> None)
  | _ -> None

let rec print_combined l k =
  match (l, k) with
  | ([], []) -> Some []
  | (x :: xs, y :: ys) -> 
      (match print_combined xs ys with
      | Some rest ->
          print_endline (string_of_int x ^ "<->" ^ string_of_int y);
          Some (() :: rest)
      | None -> None)
  | _ -> None
let car l = match l with
    | [] -> None 
    | h :: _ -> Some h

let cdr l = match l with 
    | [] -> None 
    | _ :: t -> Some t


let cadr lst =
  Maybe.(Some lst >>= fun l ->
  match l with
  | [] -> None      
  | [_] -> None          
  | _ :: x :: _ -> Some x)

let caddr lst =
  Maybe.(Some lst >>= fun l ->
  match l with
  | [] -> None      
  | [_] -> None
  | [_;_] ->None
  | _ :: _ :: x :: _ -> Some x)

let plus_car_cdr_cddr lst =
  Maybe.(
    car lst >>= fun x ->
    cadr lst >>= fun y ->
    caddr lst >>= fun z ->
    return (x + y + z)
  )
let check_dups lst =
  Maybe.(
    car lst >>= fun x ->
    cadr lst >>= fun y ->
    if x = y then return true
    else (
      caddr lst >>= fun z ->
      if x = z then return true
      else if y = z then return true
      else return false
    )
  )

let mod_t x y = match y with
    | 0 -> None 
    | _ -> Some (x mod y)

let maybe_mod_list lst n =
  let rec aux acc lst =
    match lst with
    | [] -> Maybe.return (List.rev acc)
    | x :: xs -> 
        Maybe.(mod_t x n >>= fun r ->
               aux (r :: acc) xs)
  in
  aux [] lst
(*j'ai pas reussi a faire ces exo.
type 'a tree = 
    | Leaf 
    | Node of 'a * 'a tree * 'a tree 

let left_child t = match t with
    | Leaf -> None 
    | Node (_,l,_) -> Some l 

let right_child t = match t with
    | Leaf -> None 
    | Node (_,_,r) -> Some r

let get_val t = match t with 
    | Leaf -> None 
    | Node (v,_,_) -> Some v 

let example_tree = 
    Node (1,
        Node (2, Leaf, Leaf),
        Node (3, 
            Node (4, Leaf, Leaf),
            Node (5, Leaf, Leaf)))
let rec follow_path tree path =
  match tree, path with
  | Leaf, _ -> None                        
  | _, [] -> (match get_val tree with           
              | None -> None
              | Some v -> Some [v])
  | _, "l" :: rest ->                          
      (match left_child tree with
       | None -> None                           
       | Some left ->
           (match follow_path left rest with
            | None -> None
            | Some vals -> 
                (match get_val tree with
                 | None -> None
                 | Some v -> Some (v :: vals))))
  | _, "r" :: rest ->                           
      (match right_child tree with
       | None -> None                           
       | Some right ->
           (match follow_path right rest with
            | None -> None
            | Some vals -> 
                (match get_val tree with
                 | None -> None
                 | Some v -> Some (v :: vals))))
  | _ -> None                                   
*)
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

let example_db_invalid = (["simon"],["01"],[])

type database = string list * string list * string list

module type Database = sig
    
    val empty : database

    val is_valid : database -> bool

    val print_db : database -> unit list Error.t

    val find_and_print : database -> (string * string * string -> bool) -> unit list Error.t

    val insert_into_database : string -> string -> string -> database -> database Error.t

end

module MyDatabase : Database = struct
    let empty = ([], [], [])

    let is_valid (db : database) : bool =
      let (names, surnames, numbers) = db in
      List.length names = List.length surnames && List.length surnames = List.length numbers
    
    let print_db (db : database) : unit list Error.t =
      let (names, surnames, numbers) = db in
      try
    let combined = List.map2 (fun (name, surname) number ->
      Printf.printf "Name: %s Surname: %s Student Number: %s\n" name surname number) 
      (List.combine names surnames) numbers
    in
    Error.return combined
  with Invalid_argument _ ->
    Error "Lists are not of the same length"
    let insert_into_database (name : string) (surname : string) (number : string) (db : database) : database Error.t =
      let (names, surnames, numbers) = db in
      if is_valid db then
      let updated_names = names @ [name] in
      let updated_surnames = surnames @ [surname] in
      let updated_numbers = numbers @ [number] in

      Error.return (updated_names, updated_surnames, updated_numbers)
      else
      Error "Cannot insert into invalid database!"

    let find_and_print (db : database) (f : string * string * string -> bool) : unit list Error.t =
      let (names, surnames, numbers) = db in
      let combined_list = List.combine names (List.combine surnames numbers) in
      let filtered_list = List.filter (fun (name, (surname, number)) -> f (name, surname, number)) combined_list in
      List.iter (fun (name, (surname, number)) ->
      Printf.printf "Name: %s Surname: %s Student Number: %s\n" name surname number) filtered_list;
      Error.return []


end
