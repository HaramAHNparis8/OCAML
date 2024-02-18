
let bubble_sort arr =
  let n = Array.length arr in
  for i = 0 to n - 1 do
    for j = 0 to n - i - 2 do
      if arr.(j) > arr.(j + 1) then
        let temp = arr.(j) in
        arr.(j) <- arr.(j + 1);
        arr.(j + 1) <- temp
    done;
  done;
  arr;;

let example_array = [|5; 3; 8; 4; 1|];;
bubble_sort example_array;;

Array.iter (Printf.printf "%d ") example_array;;

let insertion_sort arr =
  let n = Array.length arr in
  for i = 1 to n - 1 do
    let key = arr.(i) in
    let j = ref (i - 1) in
    while !j >= 0 && arr.(!j) > key do
      arr.(!j + 1) <- arr.(!j);
      decr j
    done;

    arr.(!j + 1) <- key
  done;;
  
let example_array = [|5; 3; 8; 4; 1|];;
insertion_sort example_array;;

Array.iter (Printf.printf "%d ") example_array;;
let rec follow_path t l = match l with
    | [] -> Some []
    | "l" :: r ->
            let* v = get_val t in
            let* lc = left_child t in
            let* rec_call = follow_path lc r in
            return (v :: rec_call)
    | "r" :: r ->
            let* v = get_val t in
            let* rc = right_child t in
            let* rec_call = follow_path rc r in
            return (v :: rec_call)
    | _ -> None


