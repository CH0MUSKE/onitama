open Model

let gen_name = 
  QCheck.Gen.string

let gen_player =
  QCheck.Gen.(triple gen_name 
    (pair (Test_card.gen_card) (Test_card.gen_card)) 
    (Test_color.gen_color)
  |> map (fun (name, (card1, card2), color) -> 
    Player.create_player name card1 card2 color
    )
  )

let test_create_player = 
  QCheck.(Test.make
    ~name:"create_player"
    (triple string (pair (make Test_card.gen_card) (make Test_card.gen_card) ) (make Test_color.gen_color))
    (fun (name, (card1,card2), color) ->
      let player = Player.create_player name card1 card2 color in
      player.name = name &&
      player.hand.card1 = card1 &&
      player.hand.card2 = card2 && 
      player.color = color
    )
  )
  
let test_change_card = 
  QCheck.(Test.make
    ~name:"change_card"
    (triple (make gen_player) (make Test_card.gen_card) (int_bound 1))
    (fun (player,card, index) ->
      let array_card = [player.hand.card1; player.hand.card2] in 
      let other_card = List.nth array_card (1-index) in
      let _ = Player.change_card player index card in
      if index = 0 then player.hand.card1 = card && player.hand.card2 = other_card else
        player.hand.card2 = card && player.hand.card1 = other_card
    )
  )
