open Model

(**
    * This module contains functions that prompt the user for input.
    *)
    
(**
    * [select_action ()] prompts the user to select an action.
    * Returns the index of the selected action.
*)
val select_action : unit -> int

(**
    * [select_card state player pos] gives the user the possible moves for the piece at position pos.
    * then prompts the user to select a card.
    * Returns the selected card and the position of the piece to move.
*)
val select_card : Game_state.t -> Player.t -> int * int -> Card.t * (int * int)

(**
    * [select_card_no_pos player] prompts the user to select a card.
    * Returns the selected card.
*)
val select_card_no_pos : Player.t -> Card.t

(**
    * [select_card_display color] prompts the user to select as many cards to display as they want.
    * The user can type anything to return to action selection.
    * Returns unit.
*)
val select_card_display : Color.t -> unit 

(**
    * [select_piece state card] prompts the user to select a piece.
    * prompts again if the piece does not have any possible moves given an already selected card.
    * Returns the selected piece and its position.
*)
val select_piece : Game_state.t -> Card.t -> Card.t * (int * int)

(**
    * [select_move state card pos] displays the possible moves for the piece at position pos with the given card. 
    * prompts the user to select a move.
    * Returns the selected move.
*)
val select_move : Game_state.t -> Card.t -> int * int -> Card.t * (int * int) * (int * int)