open! Tree
open! Lexer
open! Parser

let run () =
  "lambda x => x" |> Lexer.from_string Parser.term |> Option.get |> Typer.Infer.apply |> ignore
