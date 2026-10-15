type board = Piece.t array array
let make_board () = Array.init 5 (fun _ -> Array.make 5 Piece.Empty)
let board_init () = 
  let open Piece in let open Color in
  Array.init 5 (fun i -> 
    Array.init 5 (fun j -> 
      match (i,j) with
      | (0,2) -> King Blue
      | (0,_) -> Pawn Blue
      | (4,2) -> King Red
      | (4,_) -> Pawn Red
      | _ -> Empty
    )
  )