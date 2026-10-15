open Model

let gen_board =
  QCheck.Gen.(
    array_size (return 5) (array_size (return 5) (return Piece.Empty)) >>= fun board ->
    
    let nking = int_bound 1 in     
    let nblue = int_bound 4 in 
    let nred = int_bound 4 in     

    let rec put_pieces kings blues reds =
      if kings = -1 && blues = 0 && reds = 0 then
        return board  
      else
        int_bound 4 >>= fun i ->
        int_bound 4 >>= fun j ->
        
          if board.(i).(j) <> Piece.Empty then 
            put_pieces kings blues reds
          else if kings = 1 then  
            (board.(i).(j) <- Piece.King Color.Red;
             put_pieces 0 blues reds)
          else if kings = 0 then  
            (board.(i).(j) <- Piece.King Color.Blue;
             put_pieces (-1) blues reds)
          else if blues > 0 then
            (board.(i).(j) <- Piece.Pawn Color.Blue;
             put_pieces kings (blues - 1) reds)
          else 
            (board.(i).(j) <- Piece.Pawn Color.Red;
             put_pieces kings blues (reds - 1))
          in
          
          
    nking >>= fun kings ->
    nblue >>= fun blues ->
    nred >>= fun reds ->
    put_pieces kings blues reds
  )
