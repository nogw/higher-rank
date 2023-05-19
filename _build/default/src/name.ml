type t = string [@@deriving show]

let make name = name 
let repr name = name
let equal = String.equal
let compare = String.compare
