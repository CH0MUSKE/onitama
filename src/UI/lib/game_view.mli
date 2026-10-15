open Model
val blue : string
val red : string
val white : string

(**
    This module contains functions that display the game state to the user and string conversion functions.
*)

val card_to_string : Card.t -> string
val string_to_card : string -> Card.t

val color_string : Color.t -> string

val piece_string : Piece.t -> string

val string_to_pos : string -> int * int
val pos_to_string : int * int -> string

val print_moves : (int * int) list -> unit

val print_state : Game_state.t -> Player.t -> unit

val print_card : Card.t -> unit