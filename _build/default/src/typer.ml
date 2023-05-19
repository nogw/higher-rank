open Tree

type error =
  | Cannot_subtype of Type.t * Type.t
  | Cannot_instantiate of Name.t * Type.t
  | Cannot_synthesize of Expr.t
  | Cannot_synthesize_apply
  | Type_ill_formed of Type.t
  | Type_mono_expected
  | Could_not_drop_marker
  | Could_not_break_context
  | Unbounded_variable
[@@deriving show]

exception TyperErr of error

module Param : sig
  val next : unit -> Name.t
end = struct
  let counter = ref (-1)

  let next () =
    incr counter ;
    let diff = !counter mod 26 in
    let char = Char.chr (97 + diff) in
    let div = !counter / 26 in
    let str =
      match div with
      | 0 -> String.make 1 char
      | _ -> string_of_int div ^ String.make 1 char
    in
    Name.make str
end

module Context = struct
  open Tree

  type element =
    | CVar : Name.t -> element
    | CAnnot : Name.t * Type.t -> element
    | CMarker : Name.t -> element
    | CExists : Name.t -> element
    | CSolved : Name.t * Type.t -> element
  [@@deriving show]

  type t = element list [@@deriving show]

  let ( let* ) = Result.bind
  let empty = ([] : t)
  let insert element list = element :: list

  let update_value context old_value new_value =
    let rec loop acc = function
      | [] -> List.rev acc
      | hd :: tl when hd = old_value -> loop (new_value :: hd :: tl) tl
      | hd :: tl -> loop (hd :: acc) tl
    in
    loop [] context

  let drop_marker marker context =
    let rec loop acc = function
      | [] -> Error Could_not_drop_marker
      | hd :: tl when hd = marker -> Ok (List.rev_append acc tl)
      | hd :: tl -> loop (hd :: acc) tl
    in
    loop [] context

  let break_at expr context =
    let rec aux ys = function
      | [] -> Error Could_not_break_context
      | x :: xs when x = expr -> Ok (List.rev xs, ys)
      | x :: xs -> aux (x :: ys) xs
    in
    aux [] context

  let insert_at gamma m theta =
    let* gammaL, gammaR = break_at m gamma in
    Ok (List.append (List.append gammaL theta) gammaR)

  let collect predicate = List.fold_left predicate []

  let collect_variables context =
    let predicate xs expr =
      match expr with
      | CVar x -> x :: xs
      | _ -> xs
    in
    collect predicate context

  let collect_existentials context =
    let predicate xs expr =
      match expr with
      | CExists x -> x :: xs
      | CSolved (x, _) -> x :: xs
      | _ -> xs
    in
    collect predicate context

  let variable_mem key context = List.mem key (collect_variables context)
  let existential_mem key context = List.mem key (collect_existentials context)

  let find_variable name context =
    let predicate expr =
      match expr with
      | CAnnot (x, t) when x = name -> Some (t, context)
      | _ -> None
    in
    List.find_map predicate context

  let find_solved name context =
    let predicate expr =
      match expr with
      | CSolved (x, t) when x = name -> Some (t, context)
      | _ -> None
    in
    List.find_map predicate context

  let rec apply ty subst =
    match ty with
    | Type.LT_const _
    | Type.LT_var _ ->
        subst, ty
    | Type.LT_arrow { param; return } ->
        let subst, param = apply param subst in
        let subst, return = apply return subst in
        subst, Type.LT_arrow { param; return }
    | Type.LT_forall { param; return } ->
        let subst, return = apply return subst in
        subst, Type.LT_forall { param; return }
    | Type.LT_apply { base; arg } ->
        let subst, base = apply base subst in
        let subst, arg = apply arg subst in
        subst, Type.LT_apply { base; arg }
    | Type.LT_exist { value } -> (
        match find_solved value subst with
        | Some (solved, subst) -> apply solved subst
        | None -> subst, Type.LT_exist { value })
end

module WellFormed : sig
  val check : Context.t -> Type.t -> (unit, error) result
end = struct
  let ( let* ) = Result.bind

  let rec check context pretype =
    match pretype with
    | Type.LT_const _ -> Ok ()
    | Type.LT_var { value } when Context.variable_mem value context -> Ok ()
    | Type.LT_var { value } -> Error (Type_ill_formed (Type.LT_var { value }))
    | Type.LT_exist { value } when Context.existential_mem value context -> Ok ()
    | Type.LT_exist { value } -> Error (Type_ill_formed (Type.LT_exist { value }))
    | Type.LT_forall { param; return } ->
        let context = Context.insert (Context.CVar (Name.make param)) context in
        check context return
    | Type.LT_apply { base; arg } ->
        let* () = check context base in
        let* () = check context arg in
        Ok ()
    | Type.LT_arrow { param; return } ->
        let* () = check context param in
        let* () = check context return in
        Ok ()
end

module FreeVariables : sig
  module StringSet : Set.S with type elt = Name.t

  val apply : Type.t -> StringSet.t
  val includes : Name.t -> Type.pretype -> bool
end = struct
  module StringSet = Set.Make (struct
    type t = Name.t

    let compare = Name.compare
  end)

  let rec apply = function
    | Type.LT_const _ -> StringSet.empty
    | Type.LT_var { value } -> StringSet.singleton value
    | Type.LT_exist { value } -> StringSet.singleton value
    | Type.LT_apply { base; arg } ->
        let base = apply base in
        let arg = apply arg in
        StringSet.union base arg
    | Type.LT_arrow { param; return } ->
        let param = apply param in
        let return = apply return in
        StringSet.union param return
    | Type.LT_forall { param; return } ->
        let param = Name.make param in
        let return = apply return in
        StringSet.remove param return

  let includes var ty = StringSet.mem var (apply ty)
end

module Subst = struct
  open Type
  open Expr

  let rec apply sub var = function
    | LT_const { value } -> LT_const { value }
    | LT_var { value } when value = var -> sub
    | LT_var { value } -> LT_var { value }
    | LT_exist { value } when value = var -> sub
    | LT_exist { value } -> LT_exist { value }
    | LT_forall { param; return } when Name.make param = var -> LT_forall { param; return }
    | LT_forall { param; return } ->
        let return = apply sub var return in
        LT_forall { param; return }
    | LT_apply { base; arg } ->
        let base = apply sub var base in
        let arg = apply sub var arg in
        LT_apply { base; arg }
    | LT_arrow { param; return } ->
        let param = apply sub var param in
        let return = apply sub var return in
        LT_arrow { param; return }

  (* TODO: move expr_subst *)
  let rec expr_subst sub var = function
    | LE_literal { value } -> LE_literal { value }
    | LE_lower { value } when value = var -> sub
    | LE_lower { value } -> LE_lower { value }
    | LE_annot { value; annot } ->
        let value = expr_subst sub var value in
        LE_annot { value; annot }
    | LE_lambda { param; body } when param != var ->
        let body = expr_subst sub var body in
        LE_lambda { param; body }
    | LE_lambda { param; body } -> LE_lambda { param; body }
    | LE_apply { lambda; arg } ->
        let lambda = expr_subst sub var lambda in
        let arg = expr_subst sub var arg in
        LE_apply { lambda; arg }
    | LE_let { name; bind; body } ->
        let bind = expr_subst sub var bind in
        let body = expr_subst sub var body in
        LE_let { name; bind; body }
end

module Subtype = struct
  open Context
  open Type

  let ( let* ) = Result.bind

  (* TODO: move solve_context *)
  let solve_context gamma alpha tau =
    let* gammal, gammar = Context.break_at (CExists alpha) gamma in
    let* () = WellFormed.check gammal tau in
    Ok (List.append gammal (insert (CSolved (alpha, tau)) gammar))

  (* TODO: move monotype *)
  let rec monotype pretype =
    match pretype with
    | LT_const const -> Ok (LT_const const)
    | LT_var var -> Ok (LT_var var)
    | LT_exist ext -> Ok (LT_exist ext)
    | LT_arrow { param; return } ->
        let* param = monotype param in
        let* return = monotype return in
        Ok (LT_arrow { param; return })
    | LT_apply { base; arg } ->
        let* base = monotype base in
        let* arg = monotype arg in
        Ok (LT_apply { base; arg })
    | LT_forall _ -> Error Type_mono_expected

  let rec subtype context a b =
    let* () = WellFormed.check context a in
    let* () = WellFormed.check context b in
    match a, b with
    | LT_var x, LT_var y when x.value = y.value -> Ok context
    | LT_const x, LT_const y when x.value = y.value -> Ok context
    | LT_exist x, LT_exist y when x.value = y.value -> Ok context
    | LT_arrow x, LT_arrow y ->
        let* context = subtype context y.param x.param in
        let context, return_a = Context.apply x.return context in
        let context, return_b = Context.apply y.return context in
        subtype context return_a return_b
    | LT_forall { param; return }, sub ->
        let value = Param.next () in
        let gamma = context |> insert (CMarker value) |> insert (CExists value) in
        let subst = Subst.apply (Type.LT_exist { value }) (Name.make param) return in
        let* context = subtype gamma subst sub in
        drop_marker (CMarker value) context
    | sub, LT_forall { param; return } ->
        let param = Name.make param in
        let theta = insert (CVar param) context in
        let* delta = subtype theta sub return in
        drop_marker (CVar param) delta
    | LT_exist { value }, sub when not (FreeVariables.includes value sub) ->
        instantiate_left context value sub
    | sub, LT_exist { value } when not (FreeVariables.includes value sub) ->
        instantiate_right context sub value
    | _, _ -> Error (Cannot_subtype (a, b))

  and instantiate_left context alpha value =
    let* () = WellFormed.check context value in
    let* () = WellFormed.check context (Type.LT_exist { value = alpha }) in
    match Result.bind (monotype value) (solve_context context alpha) with
    | Ok gamma -> Ok gamma
    | Error _ -> (
        match value with
        | Type.LT_exist { value } -> solve_context context value (Type.LT_exist { value = alpha })
        | Type.LT_arrow { param; return } ->
            let alpha' = Param.next () in
            let beta' = Param.next () in
            let alpha_ext = LT_exist { value = alpha' } in
            let beta_ext = LT_exist { value = beta' } in
            let arrow_ext = LT_arrow { param = alpha_ext; return = beta_ext } in
            let arrow_ctx =
              empty
              |> insert (CExists beta')
              |> insert (CExists alpha')
              |> insert (CSolved (alpha, arrow_ext))
            in
            let* gamma = Context.insert_at context (CExists alpha) arrow_ctx in
            let* theta = instantiate_right gamma param alpha' in
            let theta, ty = Context.apply return theta in
            instantiate_left theta beta' ty
        | Type.LT_forall { param; return } ->
            let param = Name.make param in
            let gamma = context |> insert (CVar param) in
            let* theta = instantiate_left gamma alpha return in
            drop_marker (CVar param) theta
        | _ -> Error (Cannot_instantiate (alpha, value)))

  and instantiate_right context value alpha =
    let* () = WellFormed.check context value in
    let* () = WellFormed.check context (Type.LT_exist { value = alpha }) in
    match Result.bind (monotype value) (solve_context context alpha) with
    | Ok gamma -> Ok gamma
    | Error _ -> (
        match value with
        | Type.LT_exist { value } -> solve_context context value (Type.LT_exist { value = alpha })
        | Type.LT_arrow { param; return } ->
            let alpha1 = Param.next () in
            let alpha2 = Param.next () in
            let alpha_ext = LT_exist { value = alpha1 } in
            let beta_ext = LT_exist { value = alpha2 } in
            let arrow_ext = LT_arrow { param = alpha_ext; return = beta_ext } in
            let gamma =
              empty
              |> insert (CExists alpha2)
              |> insert (CExists alpha1)
              |> insert (CSolved (alpha, arrow_ext))
            in
            let* gamma = insert_at context (CExists alpha) gamma in
            let* theta = instantiate_left gamma alpha1 param in
            let theta, ty = Context.apply return theta in
            instantiate_right theta ty alpha2
        | Type.LT_forall { param; return } ->
            let beta = Param.next () in
            let param = Name.make param in
            let subst = Subst.apply (LT_exist { value = beta }) param return in
            let context = context |> insert (CMarker beta) |> insert (CExists beta) in
            let* theta = instantiate_left context beta subst in
            drop_marker (CMarker beta) theta
        | _ -> Error (Cannot_instantiate (alpha, value)))
end

module Infer = struct
  open Context
  open Expr
  open Type

  let ( let* ) = Result.bind

  let rec check context value poly =
    let* () = WellFormed.check context poly in
    match value, poly with
    | LE_literal { value = literal }, LT_const { value } -> check_literal context literal value
    | LE_lambda { param; body }, LT_arrow { param = param_type; return } ->
        check_lambda context param param_type body return
    | _, LT_forall { param; return } ->
        let alpha = CVar (Name.make param) in
        let gamma = insert alpha context in
        check gamma value return
    | _ ->
        let* theta, polytype = synth context value in
        let theta, polytype = Context.apply polytype theta in
        let theta, poly = Context.apply poly theta in
        Subtype.subtype theta polytype poly

  and check_literal context literal value =
    match literal, value with
    | LE_unit, "unit" -> Ok context
    | LE_number _, "int" -> Ok context
    | _ -> assert false

  and check_lambda context param param_type body return =
    let annot = CAnnot (param, param_type) in
    let gamma = insert annot context in
    let* theta = check gamma body return in
    drop_marker annot theta

  and synth gamma expr =
    match expr with
    | LE_literal { value } -> synth_literal gamma value
    | LE_lower { value } -> synth_lower gamma value
    | LE_lambda { param; body } -> synth_lambda gamma param body
    | LE_apply { lambda; arg } -> synth_apply gamma lambda arg
    | LE_annot { value; annot } -> synth_annot gamma value annot
    | LE_let { name; bind; body } -> synth_let gamma name bind body

  and synth_literal gamma value =
    match value with
    | LE_unit -> Ok (gamma, LT_const { value = "unit" })
    | LE_number _ -> Ok (gamma, LT_const { value = "int" })

  and synth_lower gamma value =
    match Context.find_variable value gamma with
    | Some (ty, gamma) -> Ok (gamma, ty)
    | None -> Error (Cannot_synthesize (LE_lower { value }))

  and synth_lambda gamma value body =
    let alpha = Param.next () in
    let beta = Param.next () in
    let alpha_exist = LT_exist { value = alpha } in
    let beta_exist = LT_exist { value = beta } in
    let theta =
      gamma
      |> insert (CExists alpha)
      |> insert (CExists beta)
      |> insert (CAnnot (value, alpha_exist))
    in
    let* delta = check theta body beta_exist in
    let* delta = drop_marker (CAnnot (value, alpha_exist)) delta in
    Ok (delta, LT_arrow { param = alpha_exist; return = beta_exist })

  and synth_annot_lambda gamma value annot body =
    let beta = Param.next () in
    let beta_exist = LT_exist { value = beta } in
    let theta = gamma |> insert (CExists beta) |> insert (CAnnot (value, annot)) in
    let* delta = check theta body beta_exist in
    let* delta = drop_marker (CAnnot (value, annot)) delta in
    Ok (delta, LT_arrow { param = annot; return = beta_exist })

  and synth_annot gamma value annot =
    let* theta = check gamma value annot in
    Ok (theta, annot)

  and synth_let gamma name bind body =
    let* theta, polytype = synth gamma bind in
    let annot = CAnnot (name, polytype) in
    let theta = insert annot theta in
    let* delta, body = synth theta body in
    let* context = insert_at delta annot Context.empty in
    Ok (context, body)

  and extend_context_with_arrow gamma marker a b =
    let alpha_ext = LT_exist { value = a } in
    let beta_ext = LT_exist { value = b } in
    gamma
    |> insert (CExists b)
    |> insert (CExists a)
    |> insert (CAnnot (marker, alpha_ext))
    |> insert (CSolved (marker, LT_arrow { param = alpha_ext; return = beta_ext }))

  and synth_apply gamma lambda arg =
    let* theta, polytype = synth gamma lambda in
    let theta, ty = Context.apply polytype theta in
    synth_app theta ty arg

  and synth_app gamma t expr =
    match t with
    | LT_forall { param; return } ->
        let alpha = Param.next () in
        let subst = Subst.apply (LT_exist { value = alpha }) (Name.make param) return in
        synth_app (gamma |> insert (CExists alpha)) subst expr
    | LT_exist { value } ->
        let alpha = Param.next () in
        let beta = Param.next () in
        let context = extend_context_with_arrow gamma value alpha beta in
        let* delta = check context expr (LT_exist { value = alpha }) in
        Ok (delta, LT_exist { value = beta })
    | LT_arrow { param; return } ->
        let* delta = check gamma expr param in
        Ok (delta, return)
    | _ -> Error Cannot_synthesize_apply

  and apply expr =
    match synth Context.empty expr with
    | Ok (context, ty) ->
        let _, ty = Context.apply ty context in
        Format.printf "(%a) : %a\n" Printer.pp_expr expr Printer.pp_type ty ;
        Ok ty
    | Error err -> raise (TyperErr err)
end
