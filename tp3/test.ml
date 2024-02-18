module Tictactoe : (Tictactoe with type move = Tictactoe.move) =
struct
  type move = Empty | X | O
  type board = move list list

  let empty_board = [[Empty; Empty; Empty]; [Empty; Empty; Empty]; [Empty; Empty; Empty]]

  let possible_moves board move =
    (* Implement possible_moves function here *)
    []

  let play_move board move position =
    (* Implement play_move function here *)
    None

  let string_of_board board =
    let string_of_row row =
      let row_str = String.concat " | " (List.map (fun move -> string_of_move move) row) in
      "|" ^ row_str ^ "|\n" in

    let separator = "---------\n" in

    let board_str = String.concat separator (List.map string_of_row board) in
    "\n" ^ board_str

  let string_of_move = function
    | Empty -> "Empty"
    | X -> "X"
    | O -> "O"

  let winning_config board =
    (* Implement winning_config function here *)
    false

  let propose_move board move position =
    (* Implement propose_move function here *)
    None
end

module StringOfMove : (Tictactoe with type move = Tictactoe.move) =
struct
  type move = Tictactoe.move

  let string_of_move = function
    | Empty -> "Empty"
    | X -> "X"
    | O -> "O"
end

module StringOfBoard : (Tictactoe with type board = Tictactoe.board) =
struct
  type board = Tictactoe.board

  let string_of_board board =
    let string_of_row row =
      let row_str = String.concat " | " (List.map (fun move -> StringOfMove.string_of_move move) row) in
      "|" ^ row_str ^ "|\n" in

    let separator = "---------\n" in

    let board_str = String.concat separator (List.map string_of_row board) in
    "\n" ^ board_str
end

