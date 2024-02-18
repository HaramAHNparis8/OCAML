let prod_list l = List.fold_right ( * ) l 1

let rev l = List.fold_right(fun x y -> (List.append y [x])) l []

let rec rev_r l = match l with
    | [] -> []
    | x :: xs -> List.append(rev_r xs) [x]


let rec is_prime_aux(n : int) (i : int) : bool = match i with
    | 1 -> true
    | _ -> if((n mod i )= 0) then false else (is_prime_aux n ( i - 1))

let is_prime = function
    | 0 -> false
    | 1 -> true
    | n -> is_prime_aux n (n - 1)

type 'a arbre =
    | Leaf
    | Node of 'a * ('a arbre) list

let rec count_node (t : 'a arbre) : int = match t with
    | Leaf -> 0
    | Node (_, l, m, r) -> 1 + (count_node l) + (count_node m) + (count_node r)

let example =
        Node (6, [Leaf;Leaf; Leaf; (Node (5,[Leaf]))])

module type Stack = sig 

  type 'a t (* a 타입의 요소를 포함하는 스택 *)

  val empty : 'a t (* a 타입의 스택이 비어 있을 때 나타내는 함수 *)

  val is_empty : 'a t -> bool (* empty가 참일 떄 bool 함수를 나타냄 *)

  val push : 'a -> 'a t -> 'a t (* 스택의 맨 위에 요소를 추가하는 함수 *)

  val pop : 'a t -> 'a t option (* 스택에서 맨 위 요소를 제거하고 그 결과로 나오는 스택을 반환 *)

  val peek : 'a t -> 'a option (*  스택의 맨 위에 있는 요소의 값을 반환하는 함수입니다." *)

  val size : 'a t -> int (* ' size는 스택에 있는 요소의 수를 반환하는 함수입니다." *)

end 

module ListStack : Stack = struct
  
  type 'a t = 'a list

  let empty = []

  let is_empty s = s = []

  let push x s = x :: s

  let pop s = match s with
    | [] -> None
    | _ :: t -> Some t

  let peek s = match s with
    | [] -> None
    | h :: _ -> Some h

  let size s = List.length s

end

module type StackExtended = sig
  include Stack

  val from_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list
  val map : ('a -> 'b) -> 'a t -> 'b t
end


module ExtendStack (S: Stack) : StackExtended with type 'a t = 'a S.t = struct
  include S

  let from_list lst = List.fold_right S.push lst S.empty

  let to_list s =
  let rec aux acc s = match S.pop s with
    | None -> List.rev acc
    | Some s' -> match S.peek s with
                 | None -> acc 
                 | Some x -> aux (x :: acc) s'
  in aux [] s


  let map f s =
    let rec aux acc s = match S.pop s with
      | None -> acc
      | Some s' -> aux (S.push (f (Option.get (S.peek s))) acc) s'
    in aux S.empty s
end


module StackTest (S: Stack) = struct
  let test_push_pop x =
    let stack = S.push x S.empty in
    match S.pop stack with
    | Some s -> S.peek s = Some x
    | None -> false

  let test_LIFO () =
    let stack = S.push 2 (S.push 1 S.empty) in
    S.peek stack = Some 2
end

let () =
  let open StackTest(ListStack) in

  (* test_push_pop 테스트 *)
  let test1 = test_push_pop 10 in
  Printf.printf "test_push_pop with 10: %B\n" test1;

  (* test_LIFO 테스트 *)
  let test2 = test_LIFO () in
  Printf.printf "test_LIFO: %B\n" test2;
(* OCaml의 Printf.printf 함수를 사용하여 다양한 타입의 변수를 출력하려면, 해당 타입에 맞는 형식 지정자를 사용해야 합니다. 여기 정수형과 문자형 변수를 출력하기 위한 몇 가지 예시가 있습니다:

정수형 변수 출력:
%d: 10진수 정수를 출력합니다.
%x: 16진수 정수를 출력합니다.
%o: 8진수 정수를 출력합니다.
예시: Printf.printf "정수: %d\n" 정수변수;
문자형 변수 출력:
%c: 하나의 문자를 출력합니다.
예시: Printf.printf "문자: %c\n" 문자변수;
문자열 출력:
%s: 문자열을 출력합니다.
예시: Printf.printf "문자열: %s\n" 문자열변수; *)
(* let () =
  let 정수변수 = 123 in
  let 문자변수 = 'A' in

  Printf.printf "정수: %d\n" 정수변수;
  Printf.printf "문자: %c\n" 문자변수; *)

(*
let carre x = x * x;

let x = 5;  (* 예시로 5를 x의 값으로 정의 *)
let result = carre x
Printf.printf "res carre x : %d\n" result;;
  *)

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
let rec succ_list l =
        match l with
        |[] -> []
        |x :: x1 -> x + 1 :: succ_list (x1)

let rec is_prime_aux n i = 
        if i == 1 then true
        else if n mod i == 0 then false
        else is_prime_aux n (i - 1)

let prime n = is_prime_aux n (n -1)

let rec seq_prime n =
    if n < 2 then ()
    else (
        if prime n then (print_int n; print_string " ");  
        seq_prime (n - 1)  
    )

let rec catalan n =
    if n == 0 then 1
    else (2 * (2 * n - 1) *(catalan(n - 1)) / (n + 1))

let print_catalans f n =
    let rec aux i acc =
        if i > n then acc
        else aux (i + 1) (acc ^ (if acc = "" then "" else " ") ^ string_of_int (f i))
    in
    aux 0 ""

type expr =
    | Literal of int
    | Add of expr * expr
    | Var of string

let rec eval_expr (e : expr) : int = match e with
    | Literal x -> eval_literal x
    | Add (a, b) -> eval_add a b

and eval_literal x = x  

and eval_add e1 e2 = eval_expr e1 + eval_expr e2  

type context = {x : int; y : int; z : int}

let eval_var (cntxt : context) (var : string) : int option =

    match var with
    | "x" -> Some cntxt.x
    | "y" -> Some cntxt.y
    | "z" -> Some cntxt.z
    | _ -> None

let cntxt = {x = 10; y = 20; z = 30}


let _ =
  print_endline (match eval_var cntxt "x" with
                 | Some value -> string_of_int value
                 | None -> "None");
  print_endline (match eval_var cntxt "y" with
                 | Some value -> string_of_int value
                 | None -> "None");
  print_endline (match eval_var cntxt "z" with
                 | Some value -> string_of_int value
                 | None -> "None");
  print_endline (match eval_var cntxt "w" with
                 | Some value -> string_of_int value
                 | None -> "None")

type 'a decorated_binary_tree =
    | Empty
    | Node of 'a * 'a decorated_binary_tree * 'a decorated_binary_tree

let arbre = Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty))

type binary_tree =
    | Empty
    | Node of int * binary_tree * binary_tree

type 'a decorated_binary_tree =
    | Empty
    | Node of 'a * 'a decorated_binary_tree * 'a decorated_binary_tree

let arbre = Node(1, Node(2, Empty, Node(6,Empty,Empty)), Node(3, Empty, Empty))

let rec count_nodes tree = match tree with
    | Empty -> 0
    | Node (_, left, right) -> 1 + count_nodes left + count_nodes right

let rec sum_tree arbre = match arbre with
    | Empty -> 0
    | Node (v, l, r) -> v + sum_tree l + sum_tree r

let rec produit_tree arbre = match arbre with
    | Empty -> 0
    | Node (_, l, r) -> 1 * produit_tree l * produit_tree r

type 'a internally_decorated_tree =
    | Empty
    | Node of 'a * 'a internally_decorated_tree * 'a internally_decorated_tree

let rec map_tree f tree = match tree with
    | Empty -> Empty
    | Node (value, left, right) ->
        Node (f value, map_tree f left, map_tree f right)

let succ tree = map_tree (fun x -> x + 1) tree

let rec map_tree_carre f tree = match tree with
    | Empty -> Empty
    | Node(value, left, right) ->
        Node(f value, map_tree_carre f left, map_tree_carre f right)

let carre  x = x * x

 
let succ_pro tree = map_tree (fun x -> carre x) tree
(* 테스트 트리 생성 *)
let test_tree = Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty))

(* 함수 실행 및 결과 확인 *)
let result_tree = succ test_tree

let res_tree = succ_pro test_tree

let rec search_in_tree t e = match t with
    | Empty -> false
    | Node (value, left, right) ->
        if value = e then true
        else search_in_tree left e || search_in_tree right e

let res_tree_search = search_in_tree test_tree 0


(* 직렬화 함수 정의 *)
let rec serialise_prefix = function
    | Leaf -> []
    | Node (value, left, right) -> [value] @ serialise_prefix left @ serialise_prefix right

let rec serialise_infix = function
    | Leaf -> []
    | Node (value, left, right) -> serialise_infix left @ [value] @ serialise_infix right

let rec serialise_postfix = function
    | Leaf -> []
    | Node (value, left, right) -> serialise_postfix left @ serialise_postfix right @ [value]

let first_example = Node ("a",
                        Node ("b", Leaf,
                            Node ("c", Leaf, Leaf)),
                        Node ("d",
                            Node ("e", Leaf, Leaf), Leaf))

let second_example = Node ("0",
                        Node ("1",
                            Node ("2", Leaf, Leaf),
                            Node ("3", Leaf,
                                Node ("4", Leaf, Leaf))),
                        Node ("5",
                            Node ("6", Leaf, Leaf), Leaf))

(*
let _ =
  print_endline ("Prefix first_example: " ^ String.concat " " (serialise_prefix first_example));
  print_endline ("Prefix second_example: " ^ String.concat " " (serialise_prefix second_example));
  print_endline ("Infix first_example: " ^ String.concat " " (serialise_infix first_example));
  print_endline ("Infix second_example: " ^ String.concat " " (serialise_infix second_example));
  print_endline ("Postfix first_example: " ^ String.concat " " (serialise_postfix first_example));
  print_endline ("Postfix second_example: " ^ String.concat " " (serialise_postfix second_example))
*)
let rec height tree = match tree with
    | Empty -> 0
    | Node (_, left, right) ->
        1 + max (height left) (height right)

let res_h arbre = height first_example;

let rec com_opt_map = match l1, l2 with
    |[], _ | _,[] -> None
    |x1 :: y1 , x2 :: y2 -> (x1, x2) com_opt_map(y1, y2)

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




module type Monoid = sig
  type m
  val ( @ ) : m -> m -> m
  val i : m
end

module IntAdditionMonoid : (Monoid with type m = int) = struct
  type m = int
  let ( @ ) = (+)
  let i = 0
end

module type WriterMonad = sig
  type m
  type 'a t
  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val tell : m -> unit t
  val runWriter : 'a t -> 'a * m
end

module MakeWriterMonad (M : Monoid) : (WriterMonad with type m = M.m) = struct
  type m = M.m
  type 'a t = Writer of 'a * m

  (* Monoid 모듈의 @ 연산자를 위한 래핑 함수 *)
  let mappend a b = M.( @ ) a b

  (* Monad의 return 연산을 정의합니다. *)
  let return x = Writer (x, M.i)

  (* Monad의 >>= (bind) 연산을 정의합니다. *)
  let ( >>= ) (Writer (x, v)) f = 
    match f x with
    | Writer (y, w) -> Writer (y, mappend v w)

  (* 추가 함수: monadic context에 값을 추가합니다. *)
  let tell w = Writer ((), w)

  (* Writer monad의 내부 값을 추출하는 함수 *)
  let runWriter (Writer (x, v)) = (x, v)
end

module AddWriter = MakeWriterMonad(IntAdditionMonoid)




let len lst =
  let open AddWriter in
  let rec aux acc len = function
    | [] -> return (acc, len)
    | x::xs -> tell 1 >>= fun () -> aux (x::acc) (len + 1) xs
  in aux [] 0 lst

(* 예시 사용 *)
let example1 = len [];;
let example2 = len [0; 1; 2; 3; 4; 5; 6; 7; 8; 9];;

let search lst value =
  let open AddWriter in
  let rec aux count = function
    | [] -> return (false, count)
    | x::xs -> 
      tell 1 >>= fun () -> 
      if x = value then return (true, count + 1)
      else aux (count + 1) xs
  in aux 0 lst


module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

type context = {name : string; surname : string; age : int}

module ContextReader : Monad with type 'a t = (context -> 'a) = struct
    type 'a t = (context -> 'a) 

    let return x = fun _ -> x 

    let ( >>= ) x f = fun w -> f (x w) w

end

module type Context = sig type c end 

type ('a, 'c) readerT = Reader of ('c -> 'a)

module type ReaderMonad = sig 
    include Monad
    type c 
    val runReader: 'a t -> c -> 'a
    val local: (c -> c) -> 'a t -> 'a t
    val ask : c t 
end

module MakeReader (C : Context) : (ReaderMonad with type c = C.c) with type 'a t = ('a, C.c) readerT = struct
  type c = C.c 
  type 'a t = ('a, c) readerT 

  (* required to unpack a function insider a reader *)
  let runReader = function | Reader r -> r

  (* use runReader to unpack the constructor and get the function *)
  let ( >>= ) x f = Reader (fun w -> runReader (f (runReader x w)) w)

  let return x = Reader (fun _ -> x)

  let local f m = Reader (fun w -> runReader m (f w))

  let ask = Reader (fun env -> env) 

end

(* ---- *)

module type StateMonad = sig 
    include Monad
    type s 
    val runState: 'a t -> s -> 'a * s
    val get: s t 
    val put : s -> unit t 
end

type ('a, 'b) stateT = State of ('b -> 'a * 'b)
module type State = sig type s end 

module MakeState (S : State) : (StateMonad with type s = S.s) with type 'a t = ('a, S.s) stateT = struct
  
  type s = S.s 
  type 'a t = ('a, S.s) stateT 

  (* required to unpack a function insider a state *)
  let runState = function | State f -> f

  (* use runReader to unpack the constructor and get the function *)
  let ( >>= ) m f = State (fun s -> 
                                let (a, newState) = runState m s in 
                                let b = f a
                                in runState b newState)

  let return x = State (fun s -> (x,s))

  let get = State (fun s -> (s,s)) 

  let put new_s = State (fun _ -> ((), new_s))
end

module MyContext : Context with type c = context = struct
  type c = context
end

module MyReader = MakeReader(MyContext)

let tell_age y =
  let open MyReader in
  ask >>= fun ctx ->
  let future_age = ctx.age + y in
  return (Printf.sprintf "In %d year(s) you'll be %d years old!" y future_age)

let greet =
  let open MyReader in
  ask >>= fun ctx ->
  let greeting = Printf.sprintf "Hi %s %s, you are : %d years old.\n" ctx.name ctx.surname ctx.age in
  return greeting

let greet_and_tell_age_next_year ctx =
  let open MyReader in
  let greet_msg = runReader greet ctx in
  let age_msg = runReader (tell_age 1) ctx in
  Printf.printf "%s%s" greet_msg age_msg

(* 테스트 실행 *)
let () =
  greet_and_tell_age_next_year {name = "Alex"; surname = "Singh"; age = 29}

(*
type three_vars = {x : int; y : int; z : int}

module type ReaderMonad = sig
  type 'a t
  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ask : three_vars t
end

module ThreeVarsReader : ReaderMonad = struct
  type 'a t = three_vars -> 'a

  let return x = fun _ -> x 
  let ( >>= ) m f = fun env -> f (m env) env
  let ask = fun env -> env
  let run r env = r env
end



(* 각 요소에 접근하는 함수들 *)
let get_x =
  let open ThreeVarsReader in
  ask >>= fun env -> return env.x

let get_y =
  let open ThreeVarsReader in
  ask >>= fun env -> return env.y

let get_z =
  let open ThreeVarsReader in
  ask >>= fun env -> return env.z
let sum_all_elements =
  let open ThreeVarsReader in
  get_x >>= fun x ->
  get_y >>= fun y ->
  get_z >>= fun z ->
  return (x + y + z)


let calculate_sum env =
  let result = ThreeVarsReader.run sum_all_elements env in
  Printf.printf "Sum of all elements: %d\n" result

(* 테스트 실행 *)
let () =
  let my_env = {x = 1; y = 2; z = 3} in
  calculate_sum my_env  (* 결과는 1 + 2 + 3 = 6 *) *)
type three_vars = {x : int; y : int; z : int}

module type StateMonad = sig
  type 'a t
  type s
  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val get : s t
  val put : s -> unit t
end

module ThreeVarsState : StateMonad with type s = three_vars = struct
  type 'a t = s -> 'a * s
  type s = three_vars

  let return x = fun s -> (x, s)
  let ( >>= ) m f = fun s -> let (a, newState) = m s in f a newState
  let get = fun s -> (s, s)
  let put newState = fun _ -> ((), newState)
end

let get_x = 
  let open ThreeVarsState in
  get >>= fun s -> return s.x

let get_y = 
  let open ThreeVarsState in
  get >>= fun s -> return s.y

let get_z = 
  let open ThreeVarsState in
  get >>= fun s -> return s.z


  let sum_before_after = 
  let open ThreeVarsState in
  get_x >>= fun x ->
  get_y >>= fun y ->
  get_z >>= fun z ->
  let sum_before = x + y + z in
  put {x = x; y = y; z = 123} >>= fun _ ->
  get_x >>= fun new_x ->
  get_y >>= fun new_y ->
  get_z >>= fun new_z ->
  let sum_after = new_x + new_y + new_z in
  return (sum_before, sum_after)

let runState m s = m s

(* 테스트 실행 *)
let () =
  let initial_state = {x = 10; y = 20; z = 30} in
  let ((sum_before, sum_after), new_state) = runState sum_before_after initial_state in
  Printf.printf "Sum before: %d, Sum after: %d\nNew state: {x = %d; y = %d; z = %d}\n"
    sum_before sum_after new_state.x new_state.y new_state.z

module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module ListMonad : Monad with type 'a t = 'a list = struct 
    type 'a t = 'a list
    let return x = [x] 
    let ( >>= ) x f = List.concat (List.map f x)
end

module type MonadPlus = sig 
    include Monad 
    val mzero : 'a t
    val mplus : 'a t -> 'a t -> 'a t
end 

module ListMonadPlus : MonadPlus with type 'a t = 'a list = struct 
    include ListMonad
    let mzero = [] 
    let mplus = List.append
end 

module MaybeMonadPlus : MonadPlus with type 'a t = 'a option = struct
    type 'a t = 'a option 
    let return x = Some x
    let ( >>= ) x f = match x with
        | None -> None 
        | Some x -> f x 
    let mzero = None 
    let mplus x y = match (x,y) with
    | None, None -> None 
    | Some x, None -> Some x
    | None, Some x -> Some x 
    | Some x, Some _ -> Some x
end

let (let*) x f = ListMonad.(x >>= f)
let triple x = ListMonadPlus.(mplus(return x) (mplus(return x) (return x)))

let res = ListMonad.([1;2;3;4] >>= triple)

let guard b = if b then ListMonad.return () else ListMonadPlus.mzero
let even_num n =
  let* a = List.init n (fun x -> x) in
  let* _ = guard (a mod 2 = 0) in
  ListMonad.return a

let sum_is_odd n =
  let open ListMonadPlus in
  let* a = List.init n (fun x -> x + 1) in
  let* b = List.init n (fun x -> x + 1) in
  let* _ = guard ((a + b) mod 2 <> 0) in
  return (a, b)

let carre x = x * x

let pythagorean_triplets n =
  let open ListMonadPlus in
  let* a = List.init n (fun x -> x + 1)in
  let* b = List.init n (fun x -> x + 1) in
  let* c = List.init n (fun x -> x + 1) in
  let* _ = guard (carre a + carre b =  carre c) in
  return (a,b,c)

let rec chaine_bin n = match n with
    |0 -> [""]
    |_ -> 
          let* r = chaine_bin (n - 1) in
          [r ^ "0"; r ^ "1"]

let rev x =
  let len = String.length x in
  String.init len (fun n -> String.get x (len - n - 1))

let check_1 n =
    let open ListMonadPlus in
    let* a = chaine_bin n in
    let* _ = guard ( a = rev a) in return a


let parse_char c s =
  match s with
  | "" -> MaybeMonadPlus.mzero
  | _ -> 
      if String.length s > 0 && s.[0] = c then
        MaybeMonadPlus.return (String.sub s 1 (String.length s - 1))
      else
        MaybeMonadPlus.mzero


(*
expr := term 
term := factor | factor op expr 
op := '+' | '-'
factor := digit | '(' expr ')'
digit := '0' | '1'

*)
let (<|>) = MaybeMonadPlus.mplus

let digit s =
  let open MaybeMonadPlus in
  mplus (parse_char '0' s) (parse_char '1' s)
let op s =
  let open MaybeMonadPlus in
  mplus (parse_char '+' s) mplus (parse_char '1' s)
let rec expr s = term s

and term s =
  match factor s with
  | Some rest as result ->
      (match op rest with
      | Some rest' -> expr rest'
      | None -> result)
  | None -> None

let factor s =
  match digit s with
  | Some rest -> Some rest
  | None ->
      match parse_char '(' s with
      | Some rest ->
          (match expr rest with
          | Some rest' -> 
              (match parse_char ')' rest' with
              | Some rest'' -> Some rest''
              | None -> None)
          | None -> None)
      | None -> None



and term s =
  match factor s with
  | Some rest as result ->
      (match op rest with
      | Some rest' -> expr rest'
      | None -> result)
  | None -> None
  
let bubble_sort arr =
  let n = Array.length arr in
  for i = 0 to n - 1 do
    for j = 0 to n - i - 2 do
      if arr.(j) > arr.(j + 1) then
        let temp = arr.(j) in
        arr.(j) <- arr.(j + 1);
        arr.(j + 1) <- temp
    done;
  done;
  arr;;

let example_array = [|5; 3; 8; 4; 1|];;
bubble_sort example_array;;

Array.iter (Printf.printf "%d ") example_array;;

let insertion_sort arr =
  let n = Array.length arr in
  for i = 1 to n - 1 do
    let key = arr.(i) in
    let j = ref (i - 1) in
    while !j >= 0 && arr.(!j) > key do
      arr.(!j + 1) <- arr.(!j);
      decr j
    done;

    arr.(!j + 1) <- key
  done;;
  
let example_array = [|5; 3; 8; 4; 1|];;
insertion_sort example_array;;

Array.iter (Printf.printf "%d ") example_array;;

