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


