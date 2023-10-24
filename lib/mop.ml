open Core

type parser_input =
  { text : string
  ; position : int
  }
[@@deriving sexp]

(* type char_parser = char -> bool *)
type error = { msg : string }
type 'a parser_fn = parser_input -> (parser_input, 'a)  result
type 'a parser = { fn : 'a parser_fn }

let parser_input_of_string text = { text; position = 0 }
let current_char (i : parser_input) = String.get i.text i.position

let%expect_test "current_char" =
  print_s ([%sexp_of: char] (current_char (parser_input_of_string "hello")));
  [%expect {| h |}]
;;

let mark_parsed c i =
  let ch = current_char i in
  if int_of_char c = int_of_char ch
  then Ok { text = i.text; position = i.position + 1 }
  else Error "parsing error different chars: c:"
;;

let%expect_test "mark_parsed" =
  print_s
    ([%sexp_of: parser_input]
       (match mark_parsed 'h' (parser_input_of_string "hello") with
        | Ok x -> x
        | Error _ -> raise (Invalid_argument "error")));
  [%expect {| ((text hello) (position 1)) |}]
;;

(* let rec mark_parsed_multi (c : char list) i  = *)
(*   let rec aux c i = match c with *) 
(*   | [] -> (c,i) *)
(*   | hd :: tl -> let res = match (mark_parsed hd i) with *)
(*    | Ok res -> *)
(*   let c' = fst res in *)
(*   let i' = snd res in *)
(*   aux c' i' *)
(*    | Error x -> raise (Invalid_argument "error"); *)
(*   end *)
   
(*   in *) 
(*   aux c i *)
(* ;; *)


(* let take_while (f : char_parser) : string parser = { *)
(*   { fn = fun parser_input -> *)

(* } *)
