 let rec catana n = 
         if n == 0 then 
                 1
         else 
                 catana(n - 1) * 2 *((2 * n) - 1) /(n + 1);

let print_catalans f n =
  let result = ref "" in
  for i = 0 to n do
    result := !result ^ string_of_int (f i)
  done;
  !result

let () =
  let result = print_catalans catalan 5 in
  print_endline result

