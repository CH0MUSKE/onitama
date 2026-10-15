type t = 
|Red
|Blue

let other_color color = match color with
  |Red -> Blue
  |Blue -> Red
