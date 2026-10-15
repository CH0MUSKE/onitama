open Model
open Game_state

let gen_state =
  QCheck.Gen.map3
    (fun board card color ->
      { board; card; color })
    Test_board.gen_board Test_card.gen_card Test_color.gen_color
let gen_state_empty = 
  QCheck.Gen.(map3
  (fun board card color ->
      { board; card; color })
    (array_size (return 5) (array_size (return 5) (return Piece.Empty))) Test_card.gen_card Test_color.gen_color
  )
let gen_state_full = 
  QCheck.Gen.(map3
  (fun board card color ->
      { board; card; color })
    (array_size (return 5) (array_size (return 5) (return (Piece.Pawn Color.Blue)))) Test_card.gen_card Test_color.gen_color
  )

let test_move_piece = 
  QCheck.(Test.make
  ~name: "move_piece"
  (triple (gen_state |> make) (pair (int_bound 4) (int_bound 4)) (pair (int_bound 4) (int_bound 4)))
  (fun (state,piece,target) ->
    if piece = target then true else
    let (i,j) = piece in 
    let (k,l) = target in 
    let piece_content = state.board.(i).(j) in
    let won = Game_state.move_piece state piece target in
    if won then won = Game_state.has_won state target else
    state.board.(k).(l) = piece_content &&
    state.board.(i).(j) = Piece.Empty
    )
  )
let test_has_won = 
  QCheck.(Test.make
  ~name: "has_won"
  (pair (gen_state_empty |> make) (Test_color.gen_color |> make)) 
  (fun (state,color) -> 
    let pos1 = (0,2) in 
    let pos2 = (4,2) in 
    let pos3 = (2,2) in 
    let pos4 = (3,3) in 
    let () = state.board.(2).(2) <- Piece.King color in
    let () = state.board.(3).(3) <- Piece.Empty in 
    Game_state.has_won state pos1 = (state.color = Color.Blue) &&
    Game_state.has_won state pos2 = (state.color = Color.Red) &&
    Game_state.has_won state pos3 = (state.color = color) &&
    not(Game_state.has_won state pos4))
  )
let test_is_out_of_bounds = 
  QCheck.(Test.make
  ~name: "is_out_of_bounds"
  (gen_state_empty |> make)
  (fun state -> 
    let pos1 = (9,9) in 
    let pos2 = (-1,-1) in 
    let pos3 = (0,0) in 
    Game_state.is_out_of_bounds state pos1 &&
    Game_state.is_out_of_bounds state pos2 &&
    not (Game_state.is_out_of_bounds state pos3))
  )

let test_move_is_valid = 
  QCheck.(Test.make
  ~name: "move_is_valid"
  (triple (make gen_state) (pair (int_bound 4) (int_bound 4)) (pair (int_bound 4) (int_bound 4)))
  (fun (state, source_pos, target_pos) ->
    let valid = move_is_valid state source_pos target_pos in
    let source_piece = state.board.(fst source_pos).(snd source_pos) in
    let target_piece = state.board.(fst target_pos).(snd target_pos) in
    let source_belongs_to_player = Piece.piece_is_color source_piece state.color in
    let target_is_not_ally = not (Piece.piece_is_color target_piece state.color) in
    let target_in_bounds = not (is_out_of_bounds state target_pos) in
    valid = (source_belongs_to_player && target_is_not_ally && target_in_bounds)
  )
)
let test_piece_possible_moves = 
  QCheck.(Test.make
  ~name:"piece_possible_moves"
  (triple (make gen_state) (pair (int_bound 4) (int_bound 4)) (make Test_card.gen_card))
  (fun (state, pos, card) ->
    let possible_moves = Game_state.piece_possible_moves state pos card in
    List.for_all (fun move -> move_is_valid state pos move) possible_moves
  )
)

let test_swap_cards = 
  QCheck.(Test.make
  ~name: "swap_card"
  (triple (make (Test_player.gen_player)) (make (gen_state)) (int_bound 1))
  (fun (player, state, card)->
    let prev_hand = [player.hand.card1; player.hand.card2] in 
    let prev_card = List.nth prev_hand card in 
    let other_card = List.nth prev_hand (1-card) in 
    let board_card = state.card in 
    let () = Game_state.swap_cards player state card in 
    let new_hand = [player.hand.card1; player.hand.card2] in
    List.nth new_hand card = board_card && 
    List.nth new_hand (1-card) = other_card &&
    state.card = prev_card
    )
  )

let test_change_player = 
  QCheck.(Test.make
  ~name: "change_player"
  (gen_state |> make)
  (fun state ->
    let prev_color = state.color in
    let () = Game_state.change_player state in
    prev_color = Color.other_color state.color
    )
  )

let test_has_move_available = 
  QCheck.(Test.make
  ~name: "test_has_move_available"
  (quad (gen_state_empty |> make) (gen_state_empty |> make) (gen_state_full |> make) (Test_player.gen_player |> make))
  (fun (state_empty, state_one_king,state_full,player) -> 
    state_one_king.board.(2).(2) <- Piece.King state_one_king.color;
    Game_state.has_moves_available state_one_king player.hand &&
    not(Game_state.has_moves_available state_empty player.hand) &&
    not (Game_state.has_moves_available state_full player.hand)
    )
  )





