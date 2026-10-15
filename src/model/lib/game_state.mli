type t = {
  board : Board.board;
  mutable card : Card.t;
  mutable color : Color.t;
} 

(**
  * [state_init cards] initializes a game state with the given cards.
  * Requires: [cards] is a list of 5 cards.
*)
val state_init : Card.t list -> t

(**
  * [is_out_of_bounds state pos] checks if the given position is out of bounds.
*)
val is_out_of_bounds : t -> int * int -> bool

(**
  * [move_is_valid state start_pos end_pos] checks if the move from [start_pos] to
  * [end_pos] is legal.
*)
val move_is_valid : t -> int * int -> int * int -> bool

(**
  * [piece_possible_moves state pos card] returns a list of possible moves for the
  * piece at [pos] with the given [card].
*)
val piece_possible_moves : t -> int * int -> Card.t -> (int * int) list

(**
  * [has_moves_available state hand] checks if the current player has any moves available.
*)
val has_moves_available : t -> Player.hand -> bool

(**
  * [change_player state] changes the color of the state.
*)
val change_player : t -> unit

(**
  * [swap_cards player state card index] swaps the card at [index] in the player's hand
  * with the current card in the state.
*)
val swap_cards : Player.t -> t -> int -> unit

(**
  * [has_won state pos] checks if the current move has won the game.
*)
val has_won : t -> int * int -> bool

(**
  * [move_piece state start_pos end_pos] moves the piece at [start_pos] to [end_pos].
  * Returns true if the move has won, false otherwise.
*)
val move_piece : t -> int * int -> int * int -> bool
