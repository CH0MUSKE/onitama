open Model

let select_action () = 
  print_endline "Select an action: ";
  print_endline "1. Choose a card to play";
  print_endline "2. Choose a piece to move";
  print_endline "3. See a card";
  print_endline "4. Forfeit the game";
  let action = read_line () in 
  match action with 
  | "1" -> 1
  | "2" -> 2
  | "3" -> 3
  | "4" -> 4
  | _ -> 0

let select_card state (player: Player.t) pos = 
  let card1,card2 = player.hand.card1,player.hand.card2 in
  let moves1, moves2 = (Game_state.piece_possible_moves state pos card1, Game_state.piece_possible_moves state pos card2) in
  let card1,card2 = (match (moves1,moves2) with
    | ([],[])-> print_endline ("This piece does not have any move available !"); (Card.None,Card.None)
    | (_,[]) -> print_endline ("Here are the possible moves for this piece: "); print_string (Game_view.card_to_string card1^": "); Game_view.print_moves moves1; (card1,Card.None)
    | ([],_) -> print_endline ("Here are the possible moves for this piece: "); print_string (Game_view.card_to_string card2^": "); Game_view.print_moves moves2; (Card.None,card2)
    | (_,_) -> print_endline ("Here are the possible moves for this piece: "); print_string (Game_view.card_to_string card1^": "); Game_view.print_moves moves1; print_string (Game_view.card_to_string card2^": "); Game_view.print_moves moves2; (card1,card2)) in
  print_endline ("Select a card or type anything to return to action selection");
  let string_card = read_line () in 
  let imput = Game_view.string_to_card string_card in
  if imput = Card.None then (Card.None, (-1,0))
  else if imput = card1 then (card1, pos)
  else if imput = card2 then (card2, pos)
  else (print_endline "Incorrect card name !"; (Card.None, (-1,0)))
        
        
let select_card_no_pos player =
  let open Player in
  let card1,card2 = player.hand.card1,player.hand.card2 in
  print_endline ("Select a card: "^(Game_view.card_to_string card1)^" or "^(Game_view.card_to_string card2)^" ?");
  let string_card = read_line () in 
  let rec aux string_card =
    if Game_view.string_to_card string_card = Card.None then
      (print_endline "Incorrect card name !";
      print_endline ("Select a card: "^(Game_view.card_to_string card1)^" or "^(Game_view.card_to_string card2)^" ?");
      let string_card = read_line () in
      aux string_card)
    else Game_view.string_to_card string_card
  in aux string_card

let select_card_display (color : Color.t) = 
  let rec aux () : unit = 
    print_endline ((Game_view.color_string color)^"Select a card to display or type anything else to return to action selection: ");
    let string_card = read_line () in 
    let card = Game_view.string_to_card string_card in
    if card <> Card.None 
      then 
        (Game_view.print_card card;
        aux ())
      else 
        ()
  in aux ()

let select_piece state card = 
  print_endline "Select a piece or return to action selection (r): ";
  let string_piece = read_line () in 
  let rec aux string_piece = 
    let pos = Game_view.string_to_pos string_piece in
    if pos = (-1,-1) then( 
      (print_endline "Incorrect position !"); print_endline("Select a piece or return to action selection (r): ");
      let string_piece = read_line () in
      aux string_piece)
    else if pos = (-1,0) then (Card.None,(-1,0))
    else if card <> Card.None && Game_state.piece_possible_moves state (pos) card = [] then
        (print_endline "This piece does not have any move available !";print_endline("Select a piece or return to card selection (r): ");
          let string_piece = read_line () in
          aux string_piece)
        else (card, pos) 
      in aux string_piece

let select_move state card pos = 
  let moves = Game_state.piece_possible_moves state pos card in
  print_endline "Select a move (position: {A,B,C,D,E}X[[1,5]]) or return to piece selection (r): ";
  Game_view.print_moves moves;
  let string_move = read_line () in 
  let rec aux string_move = 
    if Game_view.string_to_pos string_move = (-1,-1) || not (List.exists (fun x -> x = Game_view.string_to_pos string_move) moves) then( 
      (print_endline "This move does not exist !"); print_endline("Select a move or return to piece selection (r):");
      Game_view.print_moves moves;
      let string_move = read_line () in
      aux string_move)
    else if Game_view.string_to_pos string_move = (-1,0) then (Card.None, (-1,0), (-1,0))
    else (card, pos, Game_view.string_to_pos string_move)
  in aux string_move
