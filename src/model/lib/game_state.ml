type t = {board:Board.board; mutable card: Card.t; mutable color: Color.t}

let state_init cards =   
  let board = Board.board_init() in 
  let card = List.nth cards 0 in 
  let color = Card.card_color card in
  {board;card;color}

  let is_out_of_bounds state pos = 
    let (i,j) = pos in 
    let length = Array.length state.board in
    i < 0 || i > length-1 || j < 0 || j > length-1

(*Checks wether a move is valid *)
let move_is_valid state source_pos target_pos = 
  Piece.piece_is_color state.board.(fst source_pos).(snd source_pos) state.color &&
  not (is_out_of_bounds state target_pos) &&
  not (Piece.piece_is_color state.board.(fst target_pos).(snd target_pos) state.color)

(*Returns a list that contains the possible moves for a piece with a given card*)
let piece_possible_moves state pos card = 
  List.filter (fun trgt -> move_is_valid state pos trgt) (List.map (fun (x,y) -> (x + fst pos, y + snd pos)) (Card.possible_moves card state.color))

let has_moves_available state (hand: Player.hand) = 
  let length = Array.length state.board in 
  let rec aux i j = 
    if i < 0 then false else if j < 0 then aux (i-1) (length -1) else
      if ((piece_possible_moves state (i,j) hand.card1) <> []) || ((piece_possible_moves state (i,j) hand.card2) <> []) then
        true else aux i (j-1) in
aux (length-1) (length-1)

let change_player state =
  state.color <- Color.other_color state.color

let swap_cards player state index = 
  state.card <- Player.change_card player index state.card

let has_won state target = 
  let (k,l) = target in
  (target = (0,2) && state.color = Color.Blue) ||
  (target = (4,2) && state.color = Color.Red)  ||
  (state.board.(k).(l) = Piece.King (state.color))
  
  
let move_piece state (piece: int*int) target = 
  if has_won state target then true else
  (state.board.(fst target).(snd target) <- state.board.(fst piece).(snd piece);
  state.board.(fst piece).(snd piece) <- Piece.Empty;
  false;)

