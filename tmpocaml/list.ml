
let rec all_true (lst: bool list) : bool =
        match lst with
        | [] -> true
        | x::[] -> if x = true then true else false
        | x::rest -> x && all_true rest

 let rec total l =
    match l with
    | [] -> 0
    | h :: t -> h + total t;;


 let rec all_true l =
         match l with
         | [] -> true
         | x::[] -> if x = true then true else false
         | x::rest -> x && all_true rest;;
 let even x = x mod 2 = 0;;
 
 let rec even2ways l =
         match l with
         | [] -> true
         | x :: [] -> false
         | x1 :: x2 :: rest -> even x1 && even x2 && even2ways rest;;

 let eventure n =
         if n mod 2 == 0 then
                 n
         else
                 0;;

 let rec total_even l =
         match l with
         | [] -> 0
         | n1 :: rest -> eventure n1 + total_even rest;;
 let rec append l1 l2 =
         match l1 with
         | [] -> l2
         | hd :: tl -> hd :: (append tl l2);;
 
 let rec length l1 =
         match l1 with
         | [] -> 0
         | n1 :: rest -> 1 + length rest;;

 let rec reverse l1 =
         match l1 with
         | [] -> l1
         | n1 :: rest -> (reverse rest) @ [n1];;

let rec nth l1 n =
        match l1 with
        | [] -> raise (Failure "list is too short")
        | hd :: td ->
          if n = 0 then hd
          else nth td (n - 1);;


