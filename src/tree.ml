module Type = struct
  type t = pretype

  and pretype =
    | LT_var of { value : Name.t }
    | LT_exist of { value : Name.t }
    | LT_const of { value : string }
    | LT_forall of { param : string; return : t }
    | LT_arrow of { param : t; return : t }
    | LT_apply of { base : t; arg : t }
  [@@deriving show]
end

module Expr = struct
  type t = expr
 
  and literal = 
    | LE_unit
    | LE_number of { value : int }
  [@@deriving show]

  and expr =
    | LE_literal of { value : literal }
    | LE_lower of { value : Name.t }
    | LE_lambda of { param : Name.t; body : t }
    | LE_let of { name : Name.t; bind : t; body : t }
    | LE_apply of { lambda : t; arg : t }
    | LE_annot of { value : t; annot : Type.t }
  [@@deriving show]
end