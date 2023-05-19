
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | SEMICOLON
    | RIGHT_PARENS
    | RIGHT_BRACKET
    | RIGHT_BRACES
    | PIPE
    | NUMBER of (
# 16 "src/parser.mly"
       (int)
# 20 "src/parser.ml"
  )
    | LOWER of (
# 15 "src/parser.mly"
       (string)
# 25 "src/parser.ml"
  )
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
  
end

include MenhirBasics

# 1 "src/parser.mly"
  
  open Tree

  let curry_type_apply base args = 
    List.fold_left (fun base arg -> 
      Type.LT_apply { base; arg }
    ) base args

  let curry_expr_apply lambda args = 
    List.fold_left (fun lambda arg -> 
      Expr.LE_apply { lambda; arg }
    ) lambda args

# 59 "src/parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_term) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: term. *)

  | MenhirState05 : (('s, _menhir_box_term) _menhir_cell1_LET _menhir_cell0_LOWER, _menhir_box_term) _menhir_state
    (** State 05.
        Stack shape : LET LOWER.
        Start symbol: term. *)

  | MenhirState06 : (('s, _menhir_box_term) _menhir_cell1_LEFT_PARENS, _menhir_box_term) _menhir_state
    (** State 06.
        Stack shape : LEFT_PARENS.
        Start symbol: term. *)

  | MenhirState10 : (('s, _menhir_box_term) _menhir_cell1_LAMBDA _menhir_cell0_LOWER, _menhir_box_term) _menhir_state
    (** State 10.
        Stack shape : LAMBDA LOWER.
        Start symbol: term. *)

  | MenhirState20 : (('s, _menhir_box_term) _menhir_cell1_expr_apply, _menhir_box_term) _menhir_state
    (** State 20.
        Stack shape : expr_apply.
        Start symbol: term. *)

  | MenhirState25 : ((('s, _menhir_box_term) _menhir_cell1_expr_apply, _menhir_box_term) _menhir_cell1_expr_apply, _menhir_box_term) _menhir_state
    (** State 25.
        Stack shape : expr_apply expr_apply.
        Start symbol: term. *)

  | MenhirState28 : ((('s, _menhir_box_term) _menhir_cell1_expr_apply, _menhir_box_term) _menhir_cell1_expr_apply, _menhir_box_term) _menhir_state
    (** State 28.
        Stack shape : expr_apply expr_apply.
        Start symbol: term. *)

  | MenhirState34 : ((('s, _menhir_box_term) _menhir_cell1_LEFT_PARENS, _menhir_box_term) _menhir_cell1_expr_term, _menhir_box_term) _menhir_state
    (** State 34.
        Stack shape : LEFT_PARENS expr_term.
        Start symbol: term. *)

  | MenhirState36 : (('s, _menhir_box_term) _menhir_cell1_LEFT_PARENS, _menhir_box_term) _menhir_state
    (** State 36.
        Stack shape : LEFT_PARENS.
        Start symbol: term. *)

  | MenhirState41 : (('s, _menhir_box_term) _menhir_cell1_FORALL _menhir_cell0_LOWER, _menhir_box_term) _menhir_state
    (** State 41.
        Stack shape : FORALL LOWER.
        Start symbol: term. *)

  | MenhirState44 : (('s, _menhir_box_term) _menhir_cell1_type_simple, _menhir_box_term) _menhir_state
    (** State 44.
        Stack shape : type_simple.
        Start symbol: term. *)

  | MenhirState48 : (('s, _menhir_box_term) _menhir_cell1_type_apply, _menhir_box_term) _menhir_state
    (** State 48.
        Stack shape : type_apply.
        Start symbol: term. *)

  | MenhirState52 : ((('s, _menhir_box_term) _menhir_cell1_type_apply, _menhir_box_term) _menhir_cell1_type_apply, _menhir_box_term) _menhir_state
    (** State 52.
        Stack shape : type_apply type_apply.
        Start symbol: term. *)

  | MenhirState54 : ((('s, _menhir_box_term) _menhir_cell1_type_apply, _menhir_box_term) _menhir_cell1_type_apply, _menhir_box_term) _menhir_state
    (** State 54.
        Stack shape : type_apply type_apply.
        Start symbol: term. *)

  | MenhirState68 : ((('s, _menhir_box_term) _menhir_cell1_LET _menhir_cell0_LOWER, _menhir_box_term) _menhir_cell1_expr, _menhir_box_term) _menhir_state
    (** State 68.
        Stack shape : LET LOWER expr.
        Start symbol: term. *)


and ('s, 'r) _menhir_cell1_expr = 
  | MenhirCell1_expr of 's * ('s, 'r) _menhir_state * (Tree.Expr.t)

and ('s, 'r) _menhir_cell1_expr_apply = 
  | MenhirCell1_expr_apply of 's * ('s, 'r) _menhir_state * (Tree.Expr.t)

and ('s, 'r) _menhir_cell1_expr_term = 
  | MenhirCell1_expr_term of 's * ('s, 'r) _menhir_state * (Tree.Expr.t)

and ('s, 'r) _menhir_cell1_type_apply = 
  | MenhirCell1_type_apply of 's * ('s, 'r) _menhir_state * (Tree.Type.t)

and ('s, 'r) _menhir_cell1_type_simple = 
  | MenhirCell1_type_simple of 's * ('s, 'r) _menhir_state * (Tree.Type.t)

and ('s, 'r) _menhir_cell1_FORALL = 
  | MenhirCell1_FORALL of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LAMBDA = 
  | MenhirCell1_LAMBDA of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LEFT_PARENS = 
  | MenhirCell1_LEFT_PARENS of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LET = 
  | MenhirCell1_LET of 's * ('s, 'r) _menhir_state

and 's _menhir_cell0_LOWER = 
  | MenhirCell0_LOWER of 's * (
# 15 "src/parser.mly"
       (string)
# 169 "src/parser.ml"
)

and _menhir_box_term = 
  | MenhirBox_term of (Tree.Expr.t option) [@@unboxed]

let _menhir_action_01 =
  fun _1 ->
    (_1 : (Tree.Expr.t))

let _menhir_action_02 =
  fun _1 ->
    (_1 : (Tree.Expr.t))

let _menhir_action_03 =
  fun _1 ->
    (_1 : (Tree.Expr.t))

let _menhir_action_04 =
  fun _1 _3 _5 annot value ->
    (
# 77 "src/parser.mly"
                                                                             ( Expr.LE_annot { value; annot } )
# 192 "src/parser.ml"
     : (Tree.Expr.t))

let _menhir_action_05 =
  fun _1 ->
    (_1 : (Tree.Expr.t))

let _menhir_action_06 =
  fun _2 _3 lambda ->
    (
# 55 "src/parser.mly"
                                                    ( Expr.LE_apply { lambda; arg = Expr.LE_literal { value = Expr.LE_unit } } )
# 204 "src/parser.ml"
     : (Tree.Expr.t))

let _menhir_action_07 =
  fun args lambda ->
    (
# 56 "src/parser.mly"
                                                                           ( curry_expr_apply lambda args )
# 212 "src/parser.ml"
     : (Tree.Expr.t))

let _menhir_action_08 =
  fun _1 _3 body param ->
    (
# 51 "src/parser.mly"
                                                  ( Expr.LE_lambda { param = Name.make param; body } )
# 220 "src/parser.ml"
     : (Tree.Expr.t))

let _menhir_action_09 =
  fun _1 _3 _5 bind body name ->
    (
# 49 "src/parser.mly"
                                                                   ( Expr.LE_let { name = Name.make name; bind; body } )
# 228 "src/parser.ml"
     : (Tree.Expr.t))

let _menhir_action_10 =
  fun value ->
    (
# 71 "src/parser.mly"
                    ( Expr.LE_literal { value = Expr.LE_number { value } } )
# 236 "src/parser.ml"
     : (Tree.Expr.t))

let _menhir_action_11 =
  fun _1 _3 expr ->
    (
# 75 "src/parser.mly"
                                            ( expr )
# 244 "src/parser.ml"
     : (Tree.Expr.t))

let _menhir_action_12 =
  fun _1 ->
    (_1 : (Tree.Expr.t))

let _menhir_action_13 =
  fun _1 ->
    (_1 : (Tree.Expr.t))

let _menhir_action_14 =
  fun _1 ->
    (_1 : (Tree.Expr.t))

let _menhir_action_15 =
  fun _1 ->
    (_1 : (Tree.Expr.t))

let _menhir_action_16 =
  fun _1 ->
    (_1 : (Tree.Expr.t))

let _menhir_action_17 =
  fun _1 ->
    (_1 : (Tree.Expr.t))

let _menhir_action_18 =
  fun _1 _2 ->
    (
# 69 "src/parser.mly"
                               ( Expr.LE_literal { value = Expr.LE_unit } )
# 276 "src/parser.ml"
     : (Tree.Expr.t))

let _menhir_action_19 =
  fun value ->
    (
# 73 "src/parser.mly"
                   ( Expr.LE_lower { value = Name.make value } )
# 284 "src/parser.ml"
     : (Tree.Expr.t))

let _menhir_action_20 =
  fun init ->
    (
# 112 "src/parser.mly"
                 ( [init] )
# 292 "src/parser.ml"
     : (Tree.Expr.t list))

let _menhir_action_21 =
  fun _2 init rest ->
    (
# 113 "src/parser.mly"
                                                                 ( init :: rest )
# 300 "src/parser.ml"
     : (Tree.Expr.t list))

let _menhir_action_22 =
  fun init ->
    (
# 112 "src/parser.mly"
                 ( [init] )
# 308 "src/parser.ml"
     : (Tree.Type.t list))

let _menhir_action_23 =
  fun _2 init rest ->
    (
# 113 "src/parser.mly"
                                                                 ( init :: rest )
# 316 "src/parser.ml"
     : (Tree.Type.t list))

let _menhir_action_24 =
  fun _1 _3 expr ->
    (
# 121 "src/parser.mly"
                                               ( expr )
# 324 "src/parser.ml"
     : (Tree.Expr.t list))

let _menhir_action_25 =
  fun _1 _3 expr ->
    (
# 121 "src/parser.mly"
                                               ( expr )
# 332 "src/parser.ml"
     : (Tree.Type.t list))

let _menhir_action_26 =
  fun x ->
    (
# 238 "<standard.mly>"
    ( [ x ] )
# 340 "src/parser.ml"
     : (Tree.Expr.t list))

let _menhir_action_27 =
  fun x xs ->
    (
# 240 "<standard.mly>"
    ( x :: xs )
# 348 "src/parser.ml"
     : (Tree.Expr.t list))

let _menhir_action_28 =
  fun x ->
    (
# 238 "<standard.mly>"
    ( [ x ] )
# 356 "src/parser.ml"
     : (Tree.Type.t list))

let _menhir_action_29 =
  fun x xs ->
    (
# 240 "<standard.mly>"
    ( x :: xs )
# 364 "src/parser.ml"
     : (Tree.Type.t list))

let _menhir_action_30 =
  fun _1 ->
    (
# 40 "src/parser.mly"
         ( None )
# 372 "src/parser.ml"
     : (Tree.Expr.t option))

let _menhir_action_31 =
  fun _2 expr ->
    (
# 41 "src/parser.mly"
                      ( Some expr )
# 380 "src/parser.ml"
     : (Tree.Expr.t option))

let _menhir_action_32 =
  fun _2 _3 base ->
    (
# 88 "src/parser.mly"
                                                  ( Type.LT_apply { base; arg = Type.LT_const { value = "unit" } } )
# 388 "src/parser.ml"
     : (Tree.Type.t))

let _menhir_action_33 =
  fun args base ->
    (
# 89 "src/parser.mly"
                                                                         ( curry_type_apply base args )
# 396 "src/parser.ml"
     : (Tree.Type.t))

let _menhir_action_34 =
  fun _1 ->
    (_1 : (Tree.Type.t))

let _menhir_action_35 =
  fun _1 ->
    (_1 : (Tree.Type.t))

let _menhir_action_36 =
  fun _1 _3 param return ->
    (
# 103 "src/parser.mly"
                                                          ( Type.LT_forall { param; return } )
# 412 "src/parser.ml"
     : (Tree.Type.t))

let _menhir_action_37 =
  fun _2 param return ->
    (
# 101 "src/parser.mly"
                                                     ( Type.LT_arrow { param; return } )
# 420 "src/parser.ml"
     : (Tree.Type.t))

let _menhir_action_38 =
  fun _1 ->
    (_1 : (Tree.Type.t))

let _menhir_action_39 =
  fun value ->
    (
# 99 "src/parser.mly"
                   ( Type.LT_var { value = Name.make value } )
# 432 "src/parser.ml"
     : (Tree.Type.t))

let _menhir_action_40 =
  fun _1 _3 value ->
    (
# 105 "src/parser.mly"
                                                   ( value )
# 440 "src/parser.ml"
     : (Tree.Type.t))

let _menhir_action_41 =
  fun _1 ->
    (
# 107 "src/parser.mly"
         ( Type.LT_const { value = "int" } )
# 448 "src/parser.ml"
     : (Tree.Type.t))

let _menhir_action_42 =
  fun _1 ->
    (_1 : (Tree.Type.t))

let _menhir_action_43 =
  fun _1 _2 ->
    (
# 109 "src/parser.mly"
                               ( Type.LT_const { value = "unit" }  )
# 460 "src/parser.ml"
     : (Tree.Type.t))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | ARROW ->
        "ARROW"
    | COLON ->
        "COLON"
    | COMMA ->
        "COMMA"
    | EOF ->
        "EOF"
    | EQUAL ->
        "EQUAL"
    | FATARROW ->
        "FATARROW"
    | FORALL ->
        "FORALL"
    | INT ->
        "INT"
    | LAMBDA ->
        "LAMBDA"
    | LEFT_BRACES ->
        "LEFT_BRACES"
    | LEFT_BRACKET ->
        "LEFT_BRACKET"
    | LEFT_PARENS ->
        "LEFT_PARENS"
    | LET ->
        "LET"
    | LOWER _ ->
        "LOWER"
    | NUMBER _ ->
        "NUMBER"
    | PIPE ->
        "PIPE"
    | RIGHT_BRACES ->
        "RIGHT_BRACES"
    | RIGHT_BRACKET ->
        "RIGHT_BRACKET"
    | RIGHT_PARENS ->
        "RIGHT_PARENS"
    | SEMICOLON ->
        "SEMICOLON"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37-39"]
  
  let rec _menhir_run_72 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _v _tok ->
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let (_2, expr) = ((), _v) in
          let _v = _menhir_action_31 _2 expr in
          MenhirBox_term _v
      | _ ->
          _eRR ()
  
  let rec _menhir_run_13_spec_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_05 _1 in
      _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState00 _tok
  
  and _menhir_run_19 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_term) _menhir_state -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LEFT_PARENS ->
          let _menhir_stack = MenhirCell1_expr_apply (_menhir_stack, _menhir_s, _v) in
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EOF | RIGHT_PARENS | SEMICOLON ->
          let _1 = _v in
          let _v = _menhir_action_03 _1 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_20 : type  ttv_stack. (ttv_stack, _menhir_box_term) _menhir_cell1_expr_apply -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RIGHT_PARENS ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_expr_apply (_menhir_stack, _menhir_s, lambda) = _menhir_stack in
          let (_2, _3) = ((), ()) in
          let _v = _menhir_action_06 _2 _3 lambda in
          _menhir_goto_expr_apply _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | NUMBER _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v =
            let value = _v in
            _menhir_action_10 value
          in
          let _1 = _v in
          let _v = _menhir_action_13 _1 in
          _menhir_run_14_spec_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | LOWER _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v =
            let value = _v in
            _menhir_action_19 value
          in
          let _1 = _v in
          let _v = _menhir_action_14 _1 in
          _menhir_run_14_spec_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | LEFT_PARENS ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState20
      | _ ->
          _eRR ()
  
  and _menhir_goto_expr_apply : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_term) _menhir_state -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState28 ->
          _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState25 ->
          _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState20 ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState00 ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState68 ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState05 ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState06 ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState10 ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_27 : type  ttv_stack. ((ttv_stack, _menhir_box_term) _menhir_cell1_expr_apply as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_term) _menhir_state -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LEFT_PARENS ->
          let _menhir_stack = MenhirCell1_expr_apply (_menhir_stack, _menhir_s, _v) in
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA ->
          let _menhir_stack = MenhirCell1_expr_apply (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | NUMBER _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_1 =
                let value = _v_0 in
                _menhir_action_10 value
              in
              let _1 = _v_1 in
              let _v = _menhir_action_13 _1 in
              _menhir_run_14_spec_28 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | LOWER _v_3 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_4 =
                let value = _v_3 in
                _menhir_action_19 value
              in
              let _1 = _v_4 in
              let _v = _menhir_action_14 _1 in
              _menhir_run_14_spec_28 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | LEFT_PARENS ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState28
          | _ ->
              _eRR ())
      | RIGHT_PARENS ->
          let x = _v in
          let _v = _menhir_action_26 x in
          _menhir_goto_separated_nonempty_list_COMMA_expr_apply_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_14_spec_28 : type  ttv_stack. ((ttv_stack, _menhir_box_term) _menhir_cell1_expr_apply, _menhir_box_term) _menhir_cell1_expr_apply -> _ -> _ -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_16 _1 in
      _menhir_run_13_spec_28 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_13_spec_28 : type  ttv_stack. ((ttv_stack, _menhir_box_term) _menhir_cell1_expr_apply, _menhir_box_term) _menhir_cell1_expr_apply -> _ -> _ -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_05 _1 in
      _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState28 _tok
  
  and _menhir_run_06 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_term) _menhir_state -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RIGHT_PARENS ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_1, _2) = ((), ()) in
          let _v = _menhir_action_18 _1 _2 in
          let _1 = _v in
          let _v = _menhir_action_12 _1 in
          _menhir_goto_expr_simple _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | NUMBER _v ->
          let _menhir_stack = MenhirCell1_LEFT_PARENS (_menhir_stack, _menhir_s) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v =
            let value = _v in
            _menhir_action_10 value
          in
          let _1 = _v in
          let _v = _menhir_action_13 _1 in
          _menhir_run_14_spec_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | LOWER _v ->
          let _menhir_stack = MenhirCell1_LEFT_PARENS (_menhir_stack, _menhir_s) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v =
            let value = _v in
            _menhir_action_19 value
          in
          let _1 = _v in
          let _v = _menhir_action_14 _1 in
          _menhir_run_14_spec_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | LET ->
          let _menhir_stack = MenhirCell1_LEFT_PARENS (_menhir_stack, _menhir_s) in
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState06
      | LEFT_PARENS ->
          let _menhir_stack = MenhirCell1_LEFT_PARENS (_menhir_stack, _menhir_s) in
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState06
      | LAMBDA ->
          let _menhir_stack = MenhirCell1_LEFT_PARENS (_menhir_stack, _menhir_s) in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState06
      | _ ->
          _eRR ()
  
  and _menhir_goto_expr_simple : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_term) _menhir_state -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_16 _1 in
      _menhir_goto_expr_term _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expr_term : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_term) _menhir_state -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState06 ->
          _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState00 ->
          _menhir_run_13_spec_00 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState68 ->
          _menhir_run_13_spec_68 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState05 ->
          _menhir_run_13_spec_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState28 ->
          _menhir_run_13_spec_28 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState25 ->
          _menhir_run_13_spec_25 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState20 ->
          _menhir_run_13_spec_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState10 ->
          _menhir_run_13_spec_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_33 : type  ttv_stack. ((ttv_stack, _menhir_box_term) _menhir_cell1_LEFT_PARENS as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_term) _menhir_state -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COLON ->
          let _menhir_stack = MenhirCell1_expr_term (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LOWER _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let value = _v_0 in
              let _v = _menhir_action_39 value in
              _menhir_run_43 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState34 _tok
          | LEFT_PARENS ->
              _menhir_run_36 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState34
          | INT ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = () in
              let _v = _menhir_action_41 _1 in
              _menhir_run_43 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState34 _tok
          | FORALL ->
              _menhir_run_39 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState34
          | _ ->
              _eRR ())
      | LEFT_PARENS | RIGHT_PARENS ->
          let _1 = _v in
          let _v = _menhir_action_05 _1 in
          _menhir_goto_expr_apply _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_43 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_term) _menhir_state -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | ARROW ->
          let _menhir_stack = MenhirCell1_type_simple (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LOWER _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let value = _v_0 in
              let _v = _menhir_action_39 value in
              _menhir_run_43 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState44 _tok
          | LEFT_PARENS ->
              _menhir_run_36 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState44
          | INT ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = () in
              let _v = _menhir_action_41 _1 in
              _menhir_run_43 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState44 _tok
          | FORALL ->
              _menhir_run_39 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState44
          | _ ->
              _eRR ())
      | LEFT_PARENS | RIGHT_PARENS ->
          let _1 = _v in
          let _v = _menhir_action_34 _1 in
          _menhir_goto_type_apply _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_36 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_term) _menhir_state -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RIGHT_PARENS ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_1, _2) = ((), ()) in
          let _v = _menhir_action_43 _1 _2 in
          let _1 = _v in
          let _v = _menhir_action_42 _1 in
          _menhir_goto_type_simple _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | LOWER _v ->
          let _menhir_stack = MenhirCell1_LEFT_PARENS (_menhir_stack, _menhir_s) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let value = _v in
          let _v = _menhir_action_39 value in
          _menhir_run_43 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState36 _tok
      | LEFT_PARENS ->
          let _menhir_stack = MenhirCell1_LEFT_PARENS (_menhir_stack, _menhir_s) in
          _menhir_run_36 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState36
      | INT ->
          let _menhir_stack = MenhirCell1_LEFT_PARENS (_menhir_stack, _menhir_s) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = () in
          let _v = _menhir_action_41 _1 in
          _menhir_run_43 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState36 _tok
      | FORALL ->
          let _menhir_stack = MenhirCell1_LEFT_PARENS (_menhir_stack, _menhir_s) in
          _menhir_run_39 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState36
      | _ ->
          _eRR ()
  
  and _menhir_goto_type_simple : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_term) _menhir_state -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState54 ->
          _menhir_run_50_spec_54 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState52 ->
          _menhir_run_50_spec_52 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState48 ->
          _menhir_run_50_spec_48 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState34 ->
          _menhir_run_43 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState36 ->
          _menhir_run_43 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState44 ->
          _menhir_run_43 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState41 ->
          _menhir_run_43 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_50_spec_54 : type  ttv_stack. ((ttv_stack, _menhir_box_term) _menhir_cell1_type_apply, _menhir_box_term) _menhir_cell1_type_apply -> _ -> _ -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_34 _1 in
      _menhir_run_53 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState54 _tok
  
  and _menhir_run_53 : type  ttv_stack. ((ttv_stack, _menhir_box_term) _menhir_cell1_type_apply as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_term) _menhir_state -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LEFT_PARENS ->
          let _menhir_stack = MenhirCell1_type_apply (_menhir_stack, _menhir_s, _v) in
          _menhir_run_48 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA ->
          let _menhir_stack = MenhirCell1_type_apply (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LOWER _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let value = _v_0 in
              let _v = _menhir_action_39 value in
              _menhir_run_50_spec_54 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | LEFT_PARENS ->
              _menhir_run_36 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState54
          | INT ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = () in
              let _v = _menhir_action_41 _1 in
              _menhir_run_50_spec_54 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | RIGHT_PARENS ->
          let x = _v in
          let _v = _menhir_action_28 x in
          _menhir_goto_separated_nonempty_list_COMMA_type_apply_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_48 : type  ttv_stack. (ttv_stack, _menhir_box_term) _menhir_cell1_type_apply -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RIGHT_PARENS ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_type_apply (_menhir_stack, _menhir_s, base) = _menhir_stack in
          let (_2, _3) = ((), ()) in
          let _v = _menhir_action_32 _2 _3 base in
          _menhir_goto_type_apply _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | LOWER _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let value = _v in
          let _v = _menhir_action_39 value in
          _menhir_run_50_spec_48 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | LEFT_PARENS ->
          _menhir_run_36 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState48
      | INT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = () in
          let _v = _menhir_action_41 _1 in
          _menhir_run_50_spec_48 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_type_apply : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_term) _menhir_state -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState54 ->
          _menhir_run_53 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState52 ->
          _menhir_run_53 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState48 ->
          _menhir_run_51 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState34 ->
          _menhir_run_47 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState36 ->
          _menhir_run_47 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState41 ->
          _menhir_run_47 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState44 ->
          _menhir_run_47 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_51 : type  ttv_stack. ((ttv_stack, _menhir_box_term) _menhir_cell1_type_apply as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_term) _menhir_state -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LEFT_PARENS ->
          let _menhir_stack = MenhirCell1_type_apply (_menhir_stack, _menhir_s, _v) in
          _menhir_run_48 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA ->
          let _menhir_stack = MenhirCell1_type_apply (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LOWER _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let value = _v_0 in
              let _v = _menhir_action_39 value in
              _menhir_run_50_spec_52 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | LEFT_PARENS ->
              _menhir_run_36 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState52
          | INT ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = () in
              let _v = _menhir_action_41 _1 in
              _menhir_run_50_spec_52 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | RIGHT_PARENS ->
          let init = _v in
          let _v = _menhir_action_22 init in
          _menhir_goto_non_empty_list_COMMA_type_apply_ _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_run_50_spec_52 : type  ttv_stack. ((ttv_stack, _menhir_box_term) _menhir_cell1_type_apply, _menhir_box_term) _menhir_cell1_type_apply -> _ -> _ -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_34 _1 in
      _menhir_run_53 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState52 _tok
  
  and _menhir_goto_non_empty_list_COMMA_type_apply_ : type  ttv_stack. (ttv_stack, _menhir_box_term) _menhir_cell1_type_apply -> _ -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_1, _3, expr) = ((), (), _v) in
      let _v = _menhir_action_25 _1 _3 expr in
      let MenhirCell1_type_apply (_menhir_stack, _menhir_s, base) = _menhir_stack in
      let args = _v in
      let _v = _menhir_action_33 args base in
      _menhir_goto_type_apply _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_47 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_term) _menhir_state -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LEFT_PARENS ->
          let _menhir_stack = MenhirCell1_type_apply (_menhir_stack, _menhir_s, _v) in
          _menhir_run_48 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RIGHT_PARENS ->
          let _1 = _v in
          let _v = _menhir_action_38 _1 in
          _menhir_goto_type_entry_rec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_type_entry_rec : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_term) _menhir_state -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _1 = _v in
      let _v = _menhir_action_35 _1 in
      _menhir_goto_type_entry _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_goto_type_entry : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_term) _menhir_state -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState34 ->
          _menhir_run_63 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState36 ->
          _menhir_run_61 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState41 ->
          _menhir_run_60 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState44 ->
          _menhir_run_46 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_63 : type  ttv_stack. ((ttv_stack, _menhir_box_term) _menhir_cell1_LEFT_PARENS, _menhir_box_term) _menhir_cell1_expr_term -> _ -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_expr_term (_menhir_stack, _, value) = _menhir_stack in
      let MenhirCell1_LEFT_PARENS (_menhir_stack, _menhir_s) = _menhir_stack in
      let (_1, _3, _5, annot) = ((), (), (), _v) in
      let _v = _menhir_action_04 _1 _3 _5 annot value in
      let _1 = _v in
      let _v = _menhir_action_17 _1 in
      _menhir_goto_expr_term _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_61 : type  ttv_stack. (ttv_stack, _menhir_box_term) _menhir_cell1_LEFT_PARENS -> _ -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_LEFT_PARENS (_menhir_stack, _menhir_s) = _menhir_stack in
      let (value, _1, _3) = (_v, (), ()) in
      let _v = _menhir_action_40 _1 _3 value in
      _menhir_goto_type_simple _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_60 : type  ttv_stack. (ttv_stack, _menhir_box_term) _menhir_cell1_FORALL _menhir_cell0_LOWER -> _ -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell0_LOWER (_menhir_stack, param) = _menhir_stack in
      let MenhirCell1_FORALL (_menhir_stack, _menhir_s) = _menhir_stack in
      let (_1, _3, return) = ((), (), _v) in
      let _v = _menhir_action_36 _1 _3 param return in
      _menhir_goto_type_entry_rec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_46 : type  ttv_stack. (ttv_stack, _menhir_box_term) _menhir_cell1_type_simple -> _ -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_type_simple (_menhir_stack, _menhir_s, param) = _menhir_stack in
      let (_2, return) = ((), _v) in
      let _v = _menhir_action_37 _2 param return in
      _menhir_goto_type_entry_rec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_50_spec_48 : type  ttv_stack. (ttv_stack, _menhir_box_term) _menhir_cell1_type_apply -> _ -> _ -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_34 _1 in
      _menhir_run_51 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState48 _tok
  
  and _menhir_goto_separated_nonempty_list_COMMA_type_apply_ : type  ttv_stack. ((ttv_stack, _menhir_box_term) _menhir_cell1_type_apply as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_term) _menhir_state -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState52 ->
          _menhir_run_57 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState54 ->
          _menhir_run_55 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_57 : type  ttv_stack. ((ttv_stack, _menhir_box_term) _menhir_cell1_type_apply, _menhir_box_term) _menhir_cell1_type_apply -> _ -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_type_apply (_menhir_stack, _, init) = _menhir_stack in
      let (_2, rest) = ((), _v) in
      let _v = _menhir_action_23 _2 init rest in
      _menhir_goto_non_empty_list_COMMA_type_apply_ _menhir_stack _menhir_lexbuf _menhir_lexer _v
  
  and _menhir_run_55 : type  ttv_stack. ((ttv_stack, _menhir_box_term) _menhir_cell1_type_apply, _menhir_box_term) _menhir_cell1_type_apply -> _ -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_type_apply (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_29 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_type_apply_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_39 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_term) _menhir_state -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_FORALL (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LOWER _v ->
          let _menhir_stack = MenhirCell0_LOWER (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | FATARROW ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | LOWER _v_0 ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let value = _v_0 in
                  let _v = _menhir_action_39 value in
                  _menhir_run_43 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState41 _tok
              | LEFT_PARENS ->
                  _menhir_run_36 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState41
              | INT ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _1 = () in
                  let _v = _menhir_action_41 _1 in
                  _menhir_run_43 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState41 _tok
              | FORALL ->
                  _menhir_run_39 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState41
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_13_spec_68 : type  ttv_stack. ((ttv_stack, _menhir_box_term) _menhir_cell1_LET _menhir_cell0_LOWER, _menhir_box_term) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_05 _1 in
      _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState68 _tok
  
  and _menhir_run_13_spec_05 : type  ttv_stack. (ttv_stack, _menhir_box_term) _menhir_cell1_LET _menhir_cell0_LOWER -> _ -> _ -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_05 _1 in
      _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState05 _tok
  
  and _menhir_run_13_spec_25 : type  ttv_stack. ((ttv_stack, _menhir_box_term) _menhir_cell1_expr_apply, _menhir_box_term) _menhir_cell1_expr_apply -> _ -> _ -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_05 _1 in
      _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState25 _tok
  
  and _menhir_run_13_spec_20 : type  ttv_stack. (ttv_stack, _menhir_box_term) _menhir_cell1_expr_apply -> _ -> _ -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_05 _1 in
      _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState20 _tok
  
  and _menhir_run_24 : type  ttv_stack. ((ttv_stack, _menhir_box_term) _menhir_cell1_expr_apply as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_term) _menhir_state -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LEFT_PARENS ->
          let _menhir_stack = MenhirCell1_expr_apply (_menhir_stack, _menhir_s, _v) in
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA ->
          let _menhir_stack = MenhirCell1_expr_apply (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | NUMBER _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_1 =
                let value = _v_0 in
                _menhir_action_10 value
              in
              let _1 = _v_1 in
              let _v = _menhir_action_13 _1 in
              _menhir_run_14_spec_25 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | LOWER _v_3 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_4 =
                let value = _v_3 in
                _menhir_action_19 value
              in
              let _1 = _v_4 in
              let _v = _menhir_action_14 _1 in
              _menhir_run_14_spec_25 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | LEFT_PARENS ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState25
          | _ ->
              _eRR ())
      | RIGHT_PARENS ->
          let init = _v in
          let _v = _menhir_action_20 init in
          _menhir_goto_non_empty_list_COMMA_expr_apply_ _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_run_14_spec_25 : type  ttv_stack. ((ttv_stack, _menhir_box_term) _menhir_cell1_expr_apply, _menhir_box_term) _menhir_cell1_expr_apply -> _ -> _ -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_16 _1 in
      _menhir_run_13_spec_25 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_non_empty_list_COMMA_expr_apply_ : type  ttv_stack. (ttv_stack, _menhir_box_term) _menhir_cell1_expr_apply -> _ -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_1, _3, expr) = ((), (), _v) in
      let _v = _menhir_action_24 _1 _3 expr in
      let MenhirCell1_expr_apply (_menhir_stack, _menhir_s, lambda) = _menhir_stack in
      let args = _v in
      let _v = _menhir_action_07 args lambda in
      _menhir_goto_expr_apply _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_13_spec_10 : type  ttv_stack. (ttv_stack, _menhir_box_term) _menhir_cell1_LAMBDA _menhir_cell0_LOWER -> _ -> _ -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_05 _1 in
      _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState10 _tok
  
  and _menhir_run_14_spec_06 : type  ttv_stack. (ttv_stack, _menhir_box_term) _menhir_cell1_LEFT_PARENS -> _ -> _ -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_16 _1 in
      _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState06 _tok
  
  and _menhir_run_03 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_term) _menhir_state -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LET (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LOWER _v ->
          let _menhir_stack = MenhirCell0_LOWER (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | EQUAL ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | NUMBER _v_0 ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _v_1 =
                    let value = _v_0 in
                    _menhir_action_10 value
                  in
                  let _1 = _v_1 in
                  let _v = _menhir_action_13 _1 in
                  _menhir_run_14_spec_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
              | LOWER _v_3 ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _v_4 =
                    let value = _v_3 in
                    _menhir_action_19 value
                  in
                  let _1 = _v_4 in
                  let _v = _menhir_action_14 _1 in
                  _menhir_run_14_spec_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
              | LET ->
                  _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState05
              | LEFT_PARENS ->
                  _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState05
              | LAMBDA ->
                  _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState05
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_14_spec_05 : type  ttv_stack. (ttv_stack, _menhir_box_term) _menhir_cell1_LET _menhir_cell0_LOWER -> _ -> _ -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_16 _1 in
      _menhir_run_13_spec_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_08 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_term) _menhir_state -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LAMBDA (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LOWER _v ->
          let _menhir_stack = MenhirCell0_LOWER (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | FATARROW ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | NUMBER _v_0 ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _v_1 =
                    let value = _v_0 in
                    _menhir_action_10 value
                  in
                  let _1 = _v_1 in
                  let _v = _menhir_action_13 _1 in
                  _menhir_run_14_spec_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
              | LOWER _v_3 ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _v_4 =
                    let value = _v_3 in
                    _menhir_action_19 value
                  in
                  let _1 = _v_4 in
                  let _v = _menhir_action_14 _1 in
                  _menhir_run_14_spec_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
              | LET ->
                  _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState10
              | LEFT_PARENS ->
                  _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState10
              | LAMBDA ->
                  _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState10
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_14_spec_10 : type  ttv_stack. (ttv_stack, _menhir_box_term) _menhir_cell1_LAMBDA _menhir_cell0_LOWER -> _ -> _ -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_16 _1 in
      _menhir_run_13_spec_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_separated_nonempty_list_COMMA_expr_apply_ : type  ttv_stack. ((ttv_stack, _menhir_box_term) _menhir_cell1_expr_apply as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_term) _menhir_state -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState28 ->
          _menhir_run_29 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState25 ->
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_29 : type  ttv_stack. ((ttv_stack, _menhir_box_term) _menhir_cell1_expr_apply, _menhir_box_term) _menhir_cell1_expr_apply -> _ -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_expr_apply (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_27 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_expr_apply_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_26 : type  ttv_stack. ((ttv_stack, _menhir_box_term) _menhir_cell1_expr_apply, _menhir_box_term) _menhir_cell1_expr_apply -> _ -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_expr_apply (_menhir_stack, _, init) = _menhir_stack in
      let (_2, rest) = ((), _v) in
      let _v = _menhir_action_21 _2 init rest in
      _menhir_goto_non_empty_list_COMMA_expr_apply_ _menhir_stack _menhir_lexbuf _menhir_lexer _v
  
  and _menhir_run_14_spec_20 : type  ttv_stack. (ttv_stack, _menhir_box_term) _menhir_cell1_expr_apply -> _ -> _ -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_16 _1 in
      _menhir_run_13_spec_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_term) _menhir_state -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState00 ->
          _menhir_run_72 _menhir_stack _v _tok
      | MenhirState68 ->
          _menhir_run_69 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState05 ->
          _menhir_run_67 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState06 ->
          _menhir_run_65 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState10 ->
          _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_69 : type  ttv_stack. ((ttv_stack, _menhir_box_term) _menhir_cell1_LET _menhir_cell0_LOWER, _menhir_box_term) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _, bind) = _menhir_stack in
      let MenhirCell0_LOWER (_menhir_stack, name) = _menhir_stack in
      let MenhirCell1_LET (_menhir_stack, _menhir_s) = _menhir_stack in
      let (_1, _3, body, _5) = ((), (), _v, ()) in
      let _v = _menhir_action_09 _1 _3 _5 bind body name in
      let _1 = _v in
      let _v = _menhir_action_01 _1 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_67 : type  ttv_stack. ((ttv_stack, _menhir_box_term) _menhir_cell1_LET _menhir_cell0_LOWER as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_term) _menhir_state -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEMICOLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | NUMBER _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_1 =
                let value = _v_0 in
                _menhir_action_10 value
              in
              let _1 = _v_1 in
              let _v = _menhir_action_13 _1 in
              _menhir_run_14_spec_68 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | LOWER _v_3 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_4 =
                let value = _v_3 in
                _menhir_action_19 value
              in
              let _1 = _v_4 in
              let _v = _menhir_action_14 _1 in
              _menhir_run_14_spec_68 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | LET ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState68
          | LEFT_PARENS ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState68
          | LAMBDA ->
              _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState68
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_14_spec_68 : type  ttv_stack. ((ttv_stack, _menhir_box_term) _menhir_cell1_LET _menhir_cell0_LOWER, _menhir_box_term) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_16 _1 in
      _menhir_run_13_spec_68 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_65 : type  ttv_stack. (ttv_stack, _menhir_box_term) _menhir_cell1_LEFT_PARENS -> _ -> _ -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RIGHT_PARENS ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LEFT_PARENS (_menhir_stack, _menhir_s) = _menhir_stack in
          let (_1, _3, expr) = ((), (), _v) in
          let _v = _menhir_action_11 _1 _3 expr in
          let _1 = _v in
          let _v = _menhir_action_15 _1 in
          _menhir_goto_expr_simple _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_32 : type  ttv_stack. (ttv_stack, _menhir_box_term) _menhir_cell1_LAMBDA _menhir_cell0_LOWER -> _ -> _ -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_LOWER (_menhir_stack, param) = _menhir_stack in
      let MenhirCell1_LAMBDA (_menhir_stack, _menhir_s) = _menhir_stack in
      let (_1, _3, body) = ((), (), _v) in
      let _v = _menhir_action_08 _1 _3 body param in
      let _1 = _v in
      let _v = _menhir_action_02 _1 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  let rec _menhir_run_14_spec_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_16 _1 in
      _menhir_run_13_spec_00 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  let rec _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_term =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | NUMBER _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v =
            let value = _v in
            _menhir_action_10 value
          in
          let _1 = _v in
          let _v = _menhir_action_13 _1 in
          _menhir_run_14_spec_00 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | LOWER _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v =
            let value = _v in
            _menhir_action_19 value
          in
          let _1 = _v in
          let _v = _menhir_action_14 _1 in
          _menhir_run_14_spec_00 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | LET ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | LEFT_PARENS ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | LAMBDA ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | EOF ->
          let _v =
            let _1 = () in
            _menhir_action_30 _1
          in
          MenhirBox_term _v
      | _ ->
          _eRR ()
  
end

let term =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_term v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
