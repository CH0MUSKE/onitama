type t = 
|Rooster
|Crab 
|Boar 
|Dragon 
|Monkey 
|Elephant 
|Rabbit 
|Mantis 
|Horse 
|Eel 
|Tiger 
|Goose 
|Cobra 
|Frog 
|Crane 
|Ox
|None (*Only used to indicate that the selected card doesn't exist*)

let value_of card = match card with
 (*returns a list that contains the possible moves relative to the current position in a counterclockwise order *)
  |Rooster -> [(0,1);(-1,1);(0,-1);(1,-1)]
  |Crab -> [(0,2);(-1,0);(0,-2)]
  |Boar -> [(0,1);(-1,0);(0,-1)]
  |Dragon -> [(-1,2);(-1,-2);(1,-1);(1,1)]
  |Monkey -> [(-1,1);(-1,-1);(1,-1);(1,1)]
  |Elephant -> [(0,1);(-1,1);(-1,-1);(0,-1)] 
  |Rabbit -> [(0,2);(-1,1);(1,-1)] 
  |Mantis -> [(-1,1);(-1,-1);(1,0)] 
  |Horse -> [(-1,0);(0,-1);(1,0)] 
  |Eel -> [(0,1);(-1,-1);(1,-1)] 
  |Tiger -> [(-2,0);(1,0)] 
  |Goose -> [(0,1);(-1,-1);(0,-1);(1,1)] 
  |Cobra -> [(-1,1);(0,-1);(1,1)] 
  |Frog -> [(-1,-1);(0,-2);(1,1)] 
  |Crane -> [(-1,0);(1,-1);(1,1)] 
  |Ox -> [(0,1);(-1,0);(1,0)]
  |None -> [] 
  

 (* returns a list that contains the possible moves relative to the current position correctly oriented *)
  let possible_moves card color =
  List.map (fun (x,y) -> if color = Color.Red then (x,y) else (-x,-y)) (value_of card)

let card_color card = match card with
  |Rooster -> Color.Red
  |Crab -> Color.Blue
  |Boar -> Color.Red
  |Dragon -> Color.Red
  |Monkey -> Color.Blue
  |Elephant -> Color.Red
  |Rabbit -> Color.Blue
  |Mantis -> Color.Red
  |Horse -> Color.Red
  |Eel -> Color.Blue
  |Tiger -> Color.Blue
  |Goose -> Color.Blue
  |Cobra -> Color.Red
  |Frog -> Color.Red
  |Crane -> Color.Blue
  |Ox -> Color.Blue
  |None -> Color.Blue
  

  let generate_random () = 
    Random.self_init();
    let all_cards = 
    [Rooster; Crab; Boar; Dragon; Monkey; Elephant; Rabbit; Mantis; 
    Horse; Eel; Tiger; Goose; Cobra; Frog; Crane; Ox] in
      let random = Random.int 16 in
        List.nth all_cards random
  
  let generate_cards () = 
    let rec aux selected_cards n = 
      if n = 0 then selected_cards 
      else
        let random_card = generate_random() in 
          if List.mem random_card selected_cards then
            aux selected_cards n
          else  
            aux (random_card::selected_cards) (n-1)
    in aux [] 5
        