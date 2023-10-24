type parser_input =
  { text : string
  ; position : int
  }

type error = { msg : string }
type 'a parser_fn = parser_input -> (parser_input, 'a) result
type 'a parser = { fn : 'a parser_fn }

val parser_input_of_string : string -> parser_input
