

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

