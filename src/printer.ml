open Tree
open Format

let rec pp_type fmt ty =
  match ty with
  | Type.LT_var { value }  -> 
      fprintf fmt "%s" (Name.repr value) 
  | Type.LT_exist { value }  -> 
      fprintf fmt "^%s" (Name.repr value) 
  | Type.LT_const { value } ->
      fprintf fmt "%s" value
  | Type.LT_forall { param; return } ->
      fprintf fmt "(forall %s => %a)" param pp_type return
  | Type.LT_arrow { param = Type.LT_arrow _ as param; return } ->
      fprintf fmt "(%a) -> %a" pp_type param pp_type return
  | Type.LT_arrow { param; return } ->
      fprintf fmt "%a -> %a" pp_type param pp_type return
  | Type.LT_apply { base; arg } ->
      fprintf fmt "%a %a" pp_type base pp_type arg

let rec pp_expr fmt expr =
  match expr with
  | Expr.LE_literal { value } ->
      fprintf fmt "%a" pp_literal value
  | Expr.LE_annot { value; annot } ->
      fprintf fmt "(%a : %a)" pp_expr value pp_type annot
  | Expr.LE_lower { value } ->
      fprintf fmt "%s" (Name.repr value)
  | Expr.LE_let { name; bind; body } ->
      fprintf fmt "let %s = %a; %a" (Name.repr name) pp_expr bind pp_expr body
  | Expr.LE_lambda { param; body } ->
      fprintf fmt "lambda %s => %a" (Name.repr param) pp_expr body
  | Expr.LE_apply { lambda; arg } ->
      fprintf fmt "%a(%a)" pp_expr lambda pp_expr arg

and pp_literal fmt literal =
  match literal with
  | Expr.LE_unit -> 
      fprintf fmt "()"
  | Expr.LE_number { value } -> 
      fprintf fmt "%d" value
