type t = Pawn of Color.t | King of Color.t | Empty

(**
  * [piece_is_color piece color] returns true if the given piece has the given color.
*)
val piece_is_color : t -> Color.t -> bool
