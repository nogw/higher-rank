%{
  open Tree

  let curry_type_apply base args = 
    List.fold_left (fun base arg -> 
      Type.LT_apply { base; arg }
    ) base args

  let curry_expr_apply lambda args = 
    List.fold_left (fun lambda arg -> 
      Expr.LE_apply { lambda; arg }
    ) lambda args
%} 

%token <string> LOWER
%token <int> NUMBER
%token INT
%token FORALL
%token LAMBDA
%token LET
%token FATARROW
%token ARROW
%token EQUAL
%token PIPE
%token COMMA
%token SEMICOLON
%token COLON
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token LEFT_PARENS
%token RIGHT_PARENS
%token LEFT_BRACES
%token RIGHT_BRACES
%token EOF

%start <Expr.t option> term
%%

let term :=
  | EOF; { None }
  | expr = expr; EOF; { Some expr }

let expr :=
  | expr_let
  | expr_lambda
  | expr_apply

let expr_let :=
  | LET; name = LOWER; EQUAL; bind = expr; SEMICOLON; body = expr; { Expr.LE_let { name = Name.make name; bind; body } }
let expr_lambda :=
  | LAMBDA; param = LOWER; FATARROW; body = expr; { Expr.LE_lambda { param = Name.make param; body } }

let expr_apply :=
  | expr_term
  | lambda = expr_apply; LEFT_PARENS; RIGHT_PARENS; { Expr.LE_apply { lambda; arg = Expr.LE_literal { value = Expr.LE_unit } } }
  | lambda = expr_apply; args = parens(non_empty_list(COMMA, expr_apply)); { curry_expr_apply lambda args }

let expr_term :=
  | expr_simple
  | expr_annot

let expr_simple :=
  | expr_unit
  | expr_number
  | expr_variable
  | expr_parens

let expr_unit :=
  | LEFT_PARENS; RIGHT_PARENS; { Expr.LE_literal { value = Expr.LE_unit } }
let expr_number :=
  | value = NUMBER; { Expr.LE_literal { value = Expr.LE_number { value } } }
let expr_variable :=
  | value = LOWER; { Expr.LE_lower { value = Name.make value } }
let expr_parens :=
  | LEFT_PARENS; expr = expr; RIGHT_PARENS; { expr }
let expr_annot :=
  | LEFT_PARENS; value = expr_term; COLON; annot = type_entry; RIGHT_PARENS; { Expr.LE_annot { value; annot } }

let type_entry :=
  | type_entry_rec

let type_entry_rec :=
  | type_forall
  | type_arrow
  | type_apply

let type_apply :=
  | base = type_apply; LEFT_PARENS; RIGHT_PARENS; { Type.LT_apply { base; arg = Type.LT_const { value = "unit" } } }
  | base = type_apply; args = parens(non_empty_list(COMMA, type_apply)); { curry_type_apply base args }
  | type_simple

let type_simple :=
  | type_variable
  | type_parens
  | type_int
  | type_unit

let type_variable ==
  | value = LOWER; { Type.LT_var { value = Name.make value } }
let type_arrow ==
  | param = type_simple; ARROW; return = type_entry; { Type.LT_arrow { param; return } }
let type_forall ==
  | FORALL; param = LOWER; FATARROW; return = type_entry; { Type.LT_forall { param; return } }
let type_parens ==
  | LEFT_PARENS; value = type_entry; RIGHT_PARENS; { value } 
let type_int ==
  | INT; { Type.LT_const { value = "int" } }
let type_unit :=
  | LEFT_PARENS; RIGHT_PARENS; { Type.LT_const { value = "unit" }  }

let non_empty_list(sep, expr) :=
  | init = expr; { [init] }
  | init = expr; sep; rest = separated_nonempty_list(sep, expr); { init :: rest }

let cases(content) :=
  | PIPE; cases = separated_nonempty_list(PIPE, content); { cases }
let cases_opt(content) :=
  | option(PIPE); cases = separated_nonempty_list(PIPE, content); { cases }

let parens(content) :=
  | LEFT_PARENS; expr = content; RIGHT_PARENS; { expr }
let braces(content) :=
  | LEFT_BRACES; expr = content; RIGHT_BRACES; { expr }
let brackets(content) :=
  | LEFT_BRACKET; expr = content; RIGHT_BRACKET; { expr }
