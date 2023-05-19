type t = int [@@deriving show]

let accu = Atomic.make 0
let next () = Atomic.fetch_and_add accu 1
let equal = Int.equal
let compare = Int.compare
