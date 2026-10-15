type t = 
|Pawn of Color.t
|King of Color.t
|Empty

let piece_is_color piece color = match piece with
  |Pawn c  -> c = color
  |King c -> c = color
  |Empty -> false