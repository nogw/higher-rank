(* TODO: unnecessary *)
type flags = [ `Debug | `Warning | `Error ]

val debug : flags -> ('a, Format.formatter, unit, unit, unit, unit) format6 -> 'a
