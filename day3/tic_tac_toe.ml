module MyTicTacToe = 
struct 
type move = 
    | Empty 
    | X 
    | O 

let string_of_move = function
    | Empty -> "_"
    | X -> "X̲"
    | O -> "O̲"

type board = {aa : move; ab : move; ac : move; 
              ba : move; bb : move; bc : move;
              ca : move; cb : move; cc : move;}

let string_of_board (b : board) : string = 
        List.fold_right (fun x y -> (string_of_move x) ^ "|" ^ y) [b.aa; b.ab; b.ac] "" ^ "\n" ^
        List.fold_right (fun x y -> (string_of_move x) ^ "|" ^ y) [b.ba; b.bb; b.bc] "" ^ "\n" ^
        List.fold_right (fun x y -> (string_of_move x) ^ "|" ^ y) [b.ca; b.cb; b.cc] ""

let check_eq = function 
    | [] -> true
    | h :: t -> List.fold_right (fun x y -> (x = h) && y) (h :: t) true


let check_lines (b : board) : bool = 
    (check_eq [b.aa; b.ab; b.ac] && b.aa != Empty) &&
    (check_eq [b.ba; b.bb; b.bc] && b.ba != Empty) &&
    (check_eq [b.ca; b.cb; b.cc] && b.ca != Empty)

let check_collumns (b : board) : bool = 
    (check_eq [b.aa; b.ba; b.ca] && b.aa != Empty) &&
    (check_eq [b.ab; b.bb; b.cb] && b.ab != Empty) &&
    (check_eq [b.ac; b.bc; b.cc] && b.ac != Empty)

let check_diags (b : board) : bool = 
    (check_eq [ b.aa; b.bb; b.cc] && b.bb != Empty) &&
    (check_eq [b.ac; b.bb; b.ca] && b.bb != Empty) 

let winning_config : board -> bool = function 
    | b when ((check_collumns b) || (check_diags b) || (check_lines b)) -> true 
    | _ -> false 

let empty_board = 
    {aa = Empty; ab = Empty; ac = Empty; 
     ba = Empty; bb = Empty; bc = Empty;
     ca = Empty; cb = Empty; cc = Empty;}

let play_move (b : board) (m : move) (s : string) = match s with
    | "aa" -> if (b.aa = Empty) then Some (s, {b with aa = m}) else None
    | "ab" -> if (b.ab = Empty) then Some (s, {b with ab = m}) else None
    | "ac" -> if (b.ac = Empty) then Some (s, {b with ac = m}) else None
    | "ba" -> if (b.ba = Empty) then Some (s, {b with ba = m}) else None
    | "bb" -> if (b.bb = Empty) then Some (s, {b with bb = m}) else None
    | "bc" -> if (b.bc = Empty) then Some (s, {b with bc = m}) else None
    | "ca" -> if (b.ca = Empty) then Some (s, {b with ca = m}) else None
    | "cb" -> if (b.cb = Empty) then Some (s, {b with cb = m}) else None
    | "cc" -> if (b.cc = Empty) then Some (s, {b with cc = m}) else None
    | _ -> None

let possible_moves (b : board) (m : move) = 
    let all_moves = 
        (["aa"; "ab"; "ac";
          "ba"; "bb"; "bc"; 
          "ca"; "cb"; "cc"]) in
    List.map Option.get (List.filter (function x -> x != None) (List.map (function s -> play_move b m s) all_moves))


type game_tree =
    | Tie of board * move * string
    | Winning of board * move * string 
    | Ongoing of board * move * string * game_tree list


let other_player = function
    | X -> O
    | O -> X 
    | Empty -> Empty


let rec make_tree (b : board) (last_player : move) (last_move : string) = match b with
    | b when (winning_config b) -> Winning (b, last_player, last_move)
    | b when (possible_moves b (other_player last_player) = []) -> Tie (b, last_player, last_move) 
    | _ -> let valid_moves = (possible_moves b (other_player last_player)) in 
        Ongoing (b, last_player, last_move, List.map (function s,x -> make_tree x (other_player last_player) s) valid_moves)

let rec win_is_possible m = function
    | Winning (_,x,_) -> x = m 
    | Tie (_,_,_) -> false 
    | Ongoing (_,_,_,l) -> List.fold_right (fun x y -> (win_is_possible m x || y)) l false



let propose_move b last_player last_move =  
    match (make_tree b last_player last_move) with 
    | Winning (_,_,_) -> None 
    | Tie (_,_,_) -> None 
    | Ongoing (_,_,_,l) -> 
        let good_children = List.filter (fun x -> win_is_possible (other_player last_player) x) l 
        and extract_move = function
            | Winning (_,_,s) -> s
            | Tie (_, _, s) -> s
            | Ongoing (_, _, s, _) -> s
        in Some (List.hd (List.fold_right (fun x y -> (extract_move x) :: y) good_children []))
end  
let build_game_tree (board : MyTicTacToe.board) (last_player : MyTicTacToe.move) (last_move : string) : MyTicTacToe.game_tree =
  let rec build_tree (current_board : MyTicTacToe.board) (current_player : MyTicTacToe.move) (current_move : string) : MyTicTacToe.game_tree =
    match current_board with
    | _ when MyTicTacToe.winning_config current_board -> MyTicTacToe.Winning (current_board, last_player, last_move)
    | _ when MyTicTacToe.possible_moves current_board (MyTicTacToe.other_player current_player) = [] ->
      MyTicTacToe.Tie (current_board, last_player, last_move)
    | _ ->
      let valid_moves = MyTicTacToe.possible_moves current_board (MyTicTacToe.other_player current_player) in
      let children = List.map (fun (move, new_board) ->
        build_tree new_board (MyTicTacToe.other_player current_player) move) valid_moves
      in
      MyTicTacToe.Ongoing (current_board, last_player, last_move, children)
  in

  build_tree board last_player last_move
  let rate_children (tree : MyTicTacToe.game_tree) : (string * int) list =
  let rec rate_tree (node : MyTicTacToe.game_tree) : int =
    match node with
    | MyTicTacToe.Winning (_, move, _) when move = MyTicTacToe.O -> 1  (* O's win is good for O *)
    | MyTicTacToe.Winning (_, move, _) when move = MyTicTacToe.X -> -1  (* X's win is bad for O *)
    | MyTicTacToe.Tie (_, _, _) -> 0  (* Tie is neutral *)
    | MyTicTacToe.Ongoing (_, move, _, children) ->
      let child_ratings = List.map rate_tree children in
      if move = MyTicTacToe.O then
        List.fold_left max (-1) child_ratings  (* O wants to maximize score *)
      else
        List.fold_left min 1 child_ratings  (* X wants to minimize score *)
  in

  match tree with
  | MyTicTacToe.Ongoing (_, _, _, children) ->
    List.map (fun (MyTicTacToe.Ongoing (_, _, move, _)) -> (move, rate_tree tree)) children
  | _ -> []

let find_best_move (tree : MyTicTacToe.game_tree) : string option =
  let rated_children = rate_children tree in
  let best_move = List.fold_left (fun acc (move, rating) ->
    if rating > snd acc then (move, rating) else acc) ("", 0) rated_children in

  match best_move with
  | (move, _) -> if move = "" then None else Some move
  | _ -> None


