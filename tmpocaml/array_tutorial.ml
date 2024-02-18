type vec = float array

let vec_print v =

        for i = 0 to Array.length v - 1 do
                print_float v.(i); print_newline ()
        done

let v = [|1.; 0.|]

let vec_add v1 v2 =

        
        for i = 0 to Array.length v - 1 do
               v1.(i) <-  v1.(i) +. v2.(i);
        done;

        v1
let v1 = [|1.;2.|]
let v2 = [|3.;4.|]

float max = 0;
let max v3 =
       max <- v1.(0);
       for i = 0 to Array.length v - 1 do
               if max < v1.(i) then
                       max <- v1.(i);
       done;
       
       max


let v3 = [|1.;2.;3.|]
        
