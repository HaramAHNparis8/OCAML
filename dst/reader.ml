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
