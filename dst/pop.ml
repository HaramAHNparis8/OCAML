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
