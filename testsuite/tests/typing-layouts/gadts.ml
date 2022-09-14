type _ foo =
  | Ifoo : int foo
  | Ufoo : unit foo

let generate_value (type a) (foo : a foo) =
  match foo with
  | Ifoo -> (), (42 : a)
  | Ufoo -> (), ()
