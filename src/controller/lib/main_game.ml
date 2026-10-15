open Model
open UI

(* initialize the players *)
let players_init name1 name2 (hand1:Player.hand) (hand2:Player.hand) = 
  let player1 = Player.create_player name1 hand1.card1 hand1.card2 Color.Blue in
  let player2 = Player.create_player name2 hand2.card1 hand2.card2 Color.Red in
  (player1,player2)
  
(* initialize the game_state and players *)
let game_init name1 name2 = 
  let cards = Card.generate_cards() in
  let state = Game_state.state_init cards in  
  let players = 
    let hand1:Player.hand = 
      let card1 = List.nth cards 1 in 
      let card2 = List.nth cards 2 in
      {card1;card2} in
    let hand2:Player.hand =  
      let card1 = List.nth cards 3 in 
      let card2 = List.nth cards 4 in
      {card1;card2} in
    players_init name1 name2 hand1 hand2 in
  let (player1,player2) = players in
  (state,player1,player2)

(* if no move is available, replace a card and pass turn *)
let make_no_move state (player: Player.t) = 
  print_endline "No move available, display the cards you want then select a card to replace";
  Prompt.select_card_display (player.color);
let selected_card = Prompt.select_card_no_pos player in 
  (let index = (if selected_card = player.hand.card1 then 0 else 1) in
  Game_state.swap_cards player state index;
  Game_state.change_player state;
  false)
  
(* ask the move to play or play the move if the move is already defined *)
let rec make_a_move state player selected_card selected_piece selected_move = 
  match (selected_card,selected_piece,selected_move) with
    (* starting case *)
    | (Card.None, (-1,0), (-1,0)) -> (match Prompt.select_action () with
        | 1 -> (make_a_move state player (Prompt.select_card_no_pos player) (-1,0) (-1,0))
        | 2 -> let (selected_card, selected_piece) = Prompt.select_piece state Card.None in (make_a_move state player selected_card selected_piece (-1,0))
        | 3 -> Prompt.select_card_display (player.color); Game_view.print_state state player; (make_a_move state player Card.None (-1,0) (-1,0))  
        | 4 -> true
        | _ -> print_endline "Incorrect make_a_move"; (make_a_move state player selected_card selected_piece selected_move))
    (* if you already gave a card, select the piece you move *)
    | (selected_card, (-1,0), (-1,0)) -> let (selected_card, selected_piece) = Prompt.select_piece state selected_card in (make_a_move state player selected_card selected_piece (-1,0))
    (* if you already gave a piece, select the card *)
    | (Card.None, selected_piece, (-1,0)) -> let (selected_card, selected_piece) = Prompt.select_card state player selected_piece in (make_a_move state player selected_card selected_piece (-1,0))
    (* if you already gave a card and a piece, select the move *)
    | (selected_card, selected_piece, (-1,0)) -> let (selected_card,selected_piece,selected_move) = Prompt.select_move state selected_card selected_piece in (make_a_move state player selected_card selected_piece selected_move)
    (* if you have all the information, make the move *)
    | (selected_card, selected_piece, selected_move) -> (let index = (if selected_card = player.hand.card1 then 0 else 1) in Game_state.swap_cards player state index;
      Game_state.change_player state;
      Game_state.move_piece state selected_piece selected_move) 


(* handle the turn of a player *)
let handle_turn state (player: Player.t) =
  Game_view.print_state state player;
  if (not (Game_state.has_moves_available state player.hand)) 
    then 
      make_no_move state player
    else 
      make_a_move state player Card.None (-1,0) (-1,0)


(* main function *)
let start () = 
  print_string "Enter blue player's name : ";
  let name1 = read_line () in
  print_string "Enter red player's name : ";
  let name2 = read_line () in
  let (state,player1,player2) = game_init name1 name2 in
  let rec game_loop end_game = 
    let cur_player = if player1.color = state.color then player1 else player2 in
    if end_game then print_endline (Game_view.color_string (Color.other_color state.color)^"Victory !") else
    game_loop (handle_turn state cur_player)
    in 
  game_loop false
  