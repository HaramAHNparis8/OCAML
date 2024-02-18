include Mods.ListStack

type abr =
    | Empty 
    | Node of int * abr * abr

(*
module Insert = MakeWriter(Monoid) *)
(*
let insert n lst =
        let open Insert in
        let rec aux acc insert = 
          |[] -> return (acc, insert)
          |x :: x1 -> tell 1 >>= fun () -> aux
          if x > acc
        in aux

*)
let rec search t l = match l with
    | [] -> return([])
    | "l" :: r ->
            let* v = get_val t in
            let* lc = left_child t in
            let* rec_call = search lc r in

            return (v :: rec_call)
    | "r" :: r ->
            let* v = get_val t in
            let* rc = right_child t in
            let* rec_call = search rc r in
            return (v :: rec_call)
    | _ -> None
(*probleme 5*)

(*
let rec serialise_infix = function
    let open Insert in
    | Leaf -> return([])
    | Node (value, left, right) -> serialise_infix left @ [value] @ serialise_infix right
    *)




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
