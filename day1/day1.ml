let f x = x + 2
let g x = x + 5;;
let my_add x y = x + y in my_add 5 10;;

let rec factorial n =
        if n = 0 then 1
        else n * (factorial (n - 1));;

let rec triangular_num n =
        if n = 0 then 0
        else n + (triangular_num(n - 1));;

let triangular_num_tr n =
        let rec tn_helper n acc = 
                if n = 0 then acc
                else (tn_helper[@tailcall]) (n - 1) (acc + n) in
        tn_helper n 0;;

let rec even x = 
        if x = 0 then true else odd (x - 1)
and odd x =
        if x = 0 then false else even (x - 1);;

let is_zero n = match n with
        | 0 -> print_string "l'entree est nulle\n"
        | _ -> print_string "l'entree est pas nulle\n";;

let is_zero_f = function
        | 0 -> print_string "0=0"
        | i -> print_string ((string_of_int i )^" !=0\n");;

type day = Lun | Mar | Mer | Jeu | Sam | Dim;;
let x = Lun;;
