let swap a b t =
        let t_b = t.(b) in t.(b) <- t.(a); t.(a) <- t_b;;

type vec = float array

let vec_print v =
  for i = 0 to Array.length v - 1 do
    print_float v.(i); print_newline ()
  done

let v = [|1.;0.|]

let print lst =
    List.map string_of_int lst
        |> String.concat " "
        |> print_endline
;;


type sort_vec = int array


let bubble_sort t =

        let length = Array.length t in

          for max_pos = length - 1 downto 0 do
                  
                  for pos = 0 to max_pos - 1 do
                          
                          if t.(pos) > t.(pos + 1) then swap pos (pos + 1) t 
                        let print =
                                for i = 0 to Array.length t - 1 do
                                        print_int t.(i);
                                       done;;
let t = [|4;3;2;1|]

