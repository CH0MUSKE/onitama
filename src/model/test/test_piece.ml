open Model

let gen_piece = 
  QCheck.Gen.oneofl [Piece.Pawn Color.Blue; Piece.Pawn Color.Red; Piece.King Color.Blue; Piece.King Color.Red; Empty ]

let test_piece_is_color = 
  QCheck.(Test.make
  ~name: "piece_is_color"
  (pair (make gen_piece) (make Test_color.gen_color) ))
  (fun (piece,color) -> 
    let res = Piece.piece_is_color piece color in 
    match piece with 
    |Piece.Empty -> not res
    |Piece.King c | Piece.Pawn c -> if c = color then res else not res

  )



