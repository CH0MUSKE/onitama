type hand = { mutable card1 : Card.t; mutable card2 : Card.t; }
type t = { name : string; hand : hand; color : Color.t; }

(**
  *[create_player name card1 card2 color] creates a player with the given name,
  * the given cards and the given color.
*)
val create_player : string -> Card.t -> Card.t -> Color.t -> t

(**
  * [change_card player number new_card] changes the number-th card of the player
  * to the new card and returns the old card.
*)
val change_card : t -> int -> Card.t -> Card.t
