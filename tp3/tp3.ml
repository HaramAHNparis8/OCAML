module type Tictactoe =
  sig
    type move = Empty | X | O
    type board

    val empty_board : board
    val possible_moves : board -> move -> (string * board) list
    val play_move : board -> move -> string -> (string * board) option
    val string_of_board : board -> string
    val string_of_move : move -> string
    val winning_config : board -> bool
    val propose_move : board -> move -> string -> string option
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
      let row_str = String.concat " | " (List.map Tictactoe.string_of_move row) in
      "|" ^ row_str ^ "|\n" in

    let separator = "---------\n" in

    let board_str = String.concat separator (List.map string_of_row board) in
    "\n" ^ board_str
end

