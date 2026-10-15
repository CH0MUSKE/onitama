open Model

let gen_color = 
  QCheck.Gen.oneofl [Color.Red; Color.Blue]
let test_other_color = 
  QCheck.(Test.make
    ~name:"other_color"
    (gen_color |> make)
    (fun color -> 
      color = Color.other_color (Color.other_color color)
    ))

