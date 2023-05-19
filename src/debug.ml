(* TODO: unnecessary *)
type flags =
  [ `Debug
  | `Warning
  | `Error
  ]

let debug (flag : flags) fmt =
  match flag with
  | `Debug -> Format.printf ("[DEBUG] " ^^ fmt ^^ "\n")
  | `Warning -> Format.printf ("[WARNING] " ^^ fmt ^^ "\n")
  | `Error -> Format.printf ("[ERROR] " ^^ fmt ^^ "\n")