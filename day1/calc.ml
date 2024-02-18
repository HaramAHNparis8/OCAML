type expr =
  |Literal of int
  |Add of expr * expr
  |Var of string
  |Set of string * expr
type context = {mutable x : int; y : int; z : int}
let exemple_contexte: context = {x = 5 ; y = 10 ; z = 20}
  
let rec eval_expr p e = match e with
  |Literal x -> eval_literal p x
  |Add (a,b) -> eval_add p a b
  |Var s -> eval_var p s
  |Set (s, v) -> eval_set p s v 
and eval_literal p t = Some t
and eval_add p a b = match (eval_expr p a, eval_expr p b) with
|(Some k, Some g) -> Some (k + g)
| _ -> None  
and eval_var p s = match s with 
|"x" -> Some p.x
|"y" -> Some p.y
|"z" -> Some p.z
| _  -> None

(* and eval_set p j i =
  match eval_expr p i with
  | Some value -> (match j with
      | "x" -> Some { j with x = value }
      | "y" -> Some { j with y = value }
      | "z" -> Some { j with z = value }
  | None -> None);;
  *)

let rec eval_sequence p L =
  match L with
  | [] -> Some p  
  | expr :: rest ->
    match eval_expr p expr with
    | Some new_context -> eval_sequence new_context rest
    | None -> None  


