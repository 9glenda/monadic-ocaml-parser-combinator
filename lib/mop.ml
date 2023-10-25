open Core
open Core.Result.Let_syntax

type parser_input =
  { text : string
  ; position : int
  }
[@@deriving sexp]

(* type char_parser = char -> bool *)
type error = { msg : string }
type 'a parser_fn = parser_input -> (parser_input, 'a) result
type 'a parser = { fn : 'a parser_fn }

let parser_input_of_string text = { text; position = 0 }
let current_char (i : parser_input) = Core.String.get i.text i.position
let get s i = Result.try_with (fun () -> String.get s i)

let unwrap_option (res : 'a option) : 'a =
  match res with
  | Some x -> x
  | None -> failwith "error unwrapping value"
;;

let unwrap_result (res : ('a, 'b) result) : 'a =
  match res with
  | Ok x -> x
  | Error _ -> failwith "error unwrapping option"
;;

let take_char c i =
  let ch = current_char i in
  if int_of_char c = int_of_char ch
  then Ok { text = i.text; position = i.position + 1 }
  else Error "parsing error different chars:"
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

(* Expect tests *)

let%expect_test "monads" =
  let int_sqrt = function
    | x when x < 0 -> Error "negative number"
    | x -> x |> float_of_int |> sqrt |> int_of_float |> Ok
  in
  let save_div x y =
    match y with
    | 0 -> Error "div by 0"
    | _ -> Ok (x / y)
  in
  print_s
    ([%sexp_of: int]
       (unwrap_result
          (let%bind sq = int_sqrt 4 in
           let%bind div = save_div 5 sq in
           Ok (sq * div))));
  [%expect {| 4 |}]
;;

let%expect_test "unwrap_option" =
  let example = function
    | 0 -> None
    | x -> Some x
  in
  print_s ([%sexp_of: int] (unwrap_option (example 1)));
  [%expect {| 1 |}]
;;

let%expect_test "get" =
  print_s ([%sexp_of: char] (unwrap_result (get "hello" 0)));
  [%expect {| h |}]
;;

let%expect_test "current_char" =
  print_s ([%sexp_of: char] (current_char (parser_input_of_string "hello")));
  [%expect {| h |}]
;;

let%expect_test "take_char" =
  print_s
    ([%sexp_of: parser_input]
       (match take_char 'h' (parser_input_of_string "hello") with
        | Ok x -> x
        | Error _ -> raise (Invalid_argument "error")));
  [%expect {| ((text hello) (position 1)) |}]
;;
