open Model
let blue = "\027[34m"
let red = "\027[31m"
let white = "\027[37m"

let card_to_string card = match card with
  |Card.Rooster -> "Rooster"
  |Card.Crab -> "Crab"
  |Card.Boar -> "Boar"
  |Card.Dragon -> "Dragon"
  |Card.Monkey -> "Monkey"
  |Card.Elephant -> "Elephant"
  |Card.Rabbit -> "Rabbit"
  |Card.Mantis -> "Mantis"
  |Card.Horse -> "Horse"
  |Card.Eel -> "Eel"
  |Card.Tiger -> "Tiger"
  |Card.Goose -> "Goose"
  |Card.Cobra -> "Cobra"
  |Card.Frog -> "Frog"
  |Card.Crane -> "Crane"
  |Card.Ox -> "Ox"
  |Card.None -> ""

let string_to_card card = match String.lowercase_ascii card with
  |"rooster" -> Card.Rooster
  |"crab" -> Card.Crab 
  |"boar" -> Card.Boar
  |"dragon"->  Card.Dragon 
  |"monkey" -> Card.Monkey 
  |"elephant" -> Card.Elephant 
  |"rabbit" -> Card.Rabbit 
  |"mantis" -> Card.Mantis 
  |"horse" -> Card.Horse 
  |"eel" -> Card.Eel 
  |"tiger" -> Card.Tiger 
  |"goose" -> Card.Goose 
  |"cobra" -> Card.Cobra 
  |"frog" -> Card.Frog 
  |"crane" -> Card.Crane 
  |"ox" -> Card.Ox 
  |_ -> Card.None


let color_string color = match color with 
  |Color.Red -> red
  |Color.Blue -> blue

let piece_string piece = match piece with
  |Piece.Pawn Color.Blue -> blue^"pawn"
  |Piece.Pawn Color.Red -> red^"pawn"
  |Piece.King Color.Blue -> blue^"king"
  |Piece.King Color.Red -> red^"king"
  |Piece.Empty -> ""

let string_to_pos s = 
  match String.uppercase_ascii s with
  | "A1" -> (4, 0)
  | "A2" -> (3, 0)
  | "A3" -> (2, 0)
  | "A4" -> (1, 0)
  | "A5" -> (0, 0)
  | "B1" -> (4, 1)
  | "B2" -> (3, 1)
  | "B3" -> (2, 1)
  | "B4" -> (1, 1)
  | "B5" -> (0, 1)
  | "C1" -> (4, 2)
  | "C2" -> (3, 2)
  | "C3" -> (2, 2)
  | "C4" -> (1, 2)
  | "C5" -> (0, 2)
  | "D1" -> (4, 3)
  | "D2" -> (3, 3)
  | "D3" -> (2, 3)
  | "D4" -> (1, 3)
  | "D5" -> (0, 3)
  | "E1" -> (4, 4)
  | "E2" -> (3, 4)
  | "E3" -> (2, 4)
  | "E4" -> (1, 4)
  | "E5" -> (0, 4)
  | "R"  -> (-1,0)
  | _ -> (-1, -1) 

let pos_to_string pos = 
  match pos with
  | (4, 0) -> "A1"
  | (3, 0) -> "A2"
  | (2, 0) -> "A3"
  | (1, 0) -> "A4"
  | (0, 0) -> "A5"
  | (4, 1) -> "B1"
  | (3, 1) -> "B2"
  | (2, 1) -> "B3"
  | (1, 1) -> "B4"
  | (0, 1) -> "B5"
  | (4, 2) -> "C1"
  | (3, 2) -> "C2"
  | (2, 2) -> "C3"
  | (1, 2) -> "C4"
  | (0, 2) -> "C5"
  | (4, 3) -> "D1"
  | (3, 3) -> "D2"
  | (2, 3) -> "D3"
  | (1, 3) -> "D4"
  | (0, 3) -> "D5"
  | (4, 4) -> "E1"
  | (3, 4) -> "E2"
  | (2, 4) -> "E3"
  | (1, 4) -> "E4"
  | (0, 4) -> "E5"
  | _ -> "Invalid"

let print_moves moves = 
  let length = List.length moves in
  print_string "[";
  List.iteri (fun i x -> print_string(pos_to_string x); if i <> (length-1) then print_string ", ") moves;
  print_endline "]"
  
let print_state (state: Game_state.t) (player: Player.t) = 
  print_newline ();
  print_endline (white^" -------------------------");
  (Array.iteri 
  (fun i x ->Array.iteri 
    (fun n a -> if n = 0 then print_string (white^string_of_int (5-i)^"|"); 
      if piece_string a <> "" then 
      print_string ((piece_string a)^white^"|") else 
      print_string("    "^white^"|")) x;
      if i = 2 then print_string ("---"^(color_string (Card.card_color state.card))^(card_to_string state.card));
    print_newline();
    print_endline (white^" -------------------------"))
  state.board);
  print_endline "   A    B    C    D    E";
  print_newline ();
  print_endline (color_string (player.color) ^player.name^": " ^ card_to_string player.hand.card1 ^ " - "^ card_to_string player.hand.card2 )

  let print_card (card : Card.t) =
    let possible = List.map (fun (x,y) -> (x+2,y+2)) (Card.possible_moves card Color.Red) in 
    let color = color_string (Card.card_color card) in
    print_newline ();
    print_endline (white^" ------------------------");
    for i = 0 to 4 do
      print_string (white^"|");
      for j = 0 to 4 do
        if List.mem (i, j) possible then
          print_string (color^"XXXX"^white^"|")
        else if (i = 2 && j = 2) then
          print_string (white^"####"^white^"|")
        else
          print_string ("    "^white^"|")
      done;
      if i = 2 then print_string (color^"---"^(card_to_string card));
      print_newline ();
      print_endline (white^" ------------------------")
    done;

  