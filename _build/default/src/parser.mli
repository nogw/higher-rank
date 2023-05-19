
(* The type of tokens. *)

type token = 
  | SEMICOLON
  | RIGHT_PARENS
  | RIGHT_BRACKET
  | RIGHT_BRACES
  | PIPE
  | NUMBER of (int)
  | LOWER of (string)
  | LET
  | LEFT_PARENS
  | LEFT_BRACKET
  | LEFT_BRACES
  | LAMBDA
  | INT
  | FORALL
  | FATARROW
  | EQUAL
  | EOF
  | COMMA
  | COLON
  | ARROW

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val term: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Tree.Expr.t option)
