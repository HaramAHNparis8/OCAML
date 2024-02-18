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
