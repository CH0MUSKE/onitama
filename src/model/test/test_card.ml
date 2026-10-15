open Model

let gen_card = 
  let cards = [
  Card.Rooster; Card.Crab; Card.Boar; Card.Dragon; Card.Monkey;
  Card.Elephant; Card.Rabbit; Card.Mantis; Card.Horse; Card.Eel;
  Card.Tiger; Card.Goose; Card.Cobra; Card.Frog; Card.Crane; Card.Ox
  ] in QCheck.Gen.oneofl cards



  