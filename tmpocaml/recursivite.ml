let increment x = x + 1

let add x y = x + y

let addnew (x: int) (y: int) : int =
        x + y

let rec fib n =
        if n < 3 then
                1
        else
                fib(n - 1) + fib (n - 2)




let rec sum_add n =
        if n == 0 then
                0
        else
                n + sum_add(n - 1)

let carre n = n * n

let tri n = n * n * n
let rec cm_fois n nb =
        if nb == 1 then
                n
                
        else 
                n * cm_fois n (nb - 1)

let rec sum_add_carre n =
        if n == 0 then
                0
        else 
                carre(n) + sum_add_carre(n - 1)

let rec sum_add_tri n =
        if n == 0 then
                0
        else
                tri(n) + sum_add_tri(n - 1)


let rec res_fois n = 
        if n == 0 then
                1
        else
                n * res_fois(n - 1)


let rec res_puissance n =
        if n == 0 then
                1
        else
                carre(n) * res_puissance(n - 1)

let rec res_puissance_tri n =
        if n == 0 then
                1
        else
                tri(n) * res_puissance_tri(n - 1)

let rec mul_re n nb =
        if nb == 1 then
          n
        else
          n + mul_re(n) (nb - 1) 

let rec div_re n nb =
        if nb < n then
               0
        else
                1 + div_re(n) (nb - n)


let even x =
        if x mod 2 == 0 then
                x
        else
                0

let rec sum_even n =
        if n == 0 then
                0
        else even(n) + sum_even(n - 1)
    

