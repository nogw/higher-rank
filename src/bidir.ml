open! Tree
open! Lexer
open! Parser

let run () =
  "let a = lambda x => x; a(10)"
  |> Lexer.from_string Parser.term
  |> Option.get
  |> Typer.Infer.apply
  |> ignore