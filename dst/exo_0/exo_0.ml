
(* exo 0 - 0*)
let diff lst =
  let rec aux n = function
    | [] -> []
    | _::v when n = 0 -> aux (n+1) v
    | v0::v -> (float_of_int n *. v0) :: aux (n+1) v
  in
  aux 0 lst
;;

let res = [1.;2.;1.]
(*testez avec utop sur diff res *)

(*exo 0 - 1 *)
let carre n = n * n


let rec eval_poly lst n =
    match lst with
    | [] -> 0.0
    | l1 ::l2 -> l1 +. n *. (eval_poly l2 n)


let res_2 = eval_poly [-3.; 6. /. 7.; 3.14] 0.5772;;

Printf.printf "%f" res_2;;

