type t =
    | Rooster
    | Crab
    | Boar
    | Dragon
    | Monkey
    | Elephant
    | Rabbit
    | Mantis
    | Horse
    | Eel
    | Tiger
    | Goose
    | Cobra
    | Frog
    | Crane
    | Ox
    | None

(** 
    possible_moves card color returns a list of possible moves for a piece of the given color using the given card, 
    without taking the position of the other pieces into account.
*)
val possible_moves : t -> Color.t -> (int * int) list

(**
    card_color card returns the color of the given card.
*)
val card_color : t -> Color.t

(**
    generate_cards () returns a list 5 different cards.
*) 
val generate_cards : unit -> t list
