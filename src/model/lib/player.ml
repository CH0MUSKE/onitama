  type hand = {mutable card1: Card.t; mutable card2: Card.t}
  type t = {name: string; hand: hand; color: Color.t}

  let create_player name card1 card2 color = let hand = {card1;card2} in {name;hand;color}
  let change_card player number new_card = 
    if number = 0 then 
      let return = player.hand.card1 in
      (player.hand.card1 <- new_card); 
      return 
    else
      let return = player.hand.card2 in
        (player.hand.card2 <- new_card);
        return 