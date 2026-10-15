type board = Piece.t array array

(** [make_board ()] initializes a new game board for Onitama.
    @return a 2D array of [Piece.t] representing the initial state of the board. *)
val make_board : unit -> Piece.t array array

(** [board_init ()] initializes a new game board for Onitama.
    @return a 2D array of [Piece.t] representing the initial state of the board. *)
val board_init : unit -> Piece.t array array
