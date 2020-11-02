module Type = Hflmc2_syntax.Type
module Hflz = Hflmc2_syntax.Hflz
module Id = Hflmc2_syntax.Id
module Arith = Hflmc2_syntax.Arith
open Hflz

type ty_env = (Type.simple_ty Type.arg Id.t) list

let eq_modulo_arg_ids : Type.simple_ty -> Type.simple_ty -> bool =
  let rec go = fun ty1 ty2 -> match ty1, ty2 with
  | Type.TyBool _, Type.TyBool _ -> true
  | Type.TyArrow ({ty=ty1;_}, body1), Type.TyArrow({ty=ty2;_}, body2) -> begin
    let tyf =
      match ty1, ty2 with
      | Type.TySigma ty1', Type.TySigma ty2' ->
        go ty1' ty2'
      | Type.TyInt, Type.TyInt -> true
      | _ -> false in
    tyf && go body1 body2
  end
  | _ -> false in
  go
  
let type_check_arith : ty_env -> Arith.t -> bool = fun env arith ->
  let show_arg_ty = fun fmt ty -> Format.pp_print_string fmt @@ Type.show_ty Fmt.nop ty in
  let show_arg = Type.show_arg show_arg_ty in
  let show_id = Id.show Fmt.nop in
  let rec go = fun arith -> match arith with
  | Arith.Int _ -> true
  | Arith.Var v -> begin
    match List.find_opt (fun k -> Id.eq k v) env with
    | Some {Id.ty=ty'; _} ->
      if ty' = Type.TyInt
      then true
      else failwith @@ "[Arith] var `" ^ show_id v ^ "`'s type should be Int, but actual: " ^ show_arg ty' ^ "."
    | None -> failwith @@ "[Arith] unbound var `" ^ show_id v ^ "`' "
  end
  | Arith.Op (_, args) ->
    List.length args = 2 &&
    List.for_all go args in
  go arith

let remove_id_from_type typ =
  let rec go typ = match typ with
    | Type.TyArrow (arg, exp) -> Type.TyArrow ({ arg with name = "x"; id = 1 }, go exp)
    | Type.TyBool _ -> typ in
  go typ
  
let get_hflz_type : ty_env -> Type.simple_ty Hflz.t -> Type.simple_ty = fun env hfl ->
  let show_arg_ty = fun fmt ty -> Format.pp_print_string fmt @@ Type.show_ty Fmt.nop ty in
  let show_arg = Type.show_arg show_arg_ty in
  let show_id = Id.show Fmt.nop in
  let show_formula f = 
    let buf = Buffer.create 100 in
    let fmt = Format.formatter_of_buffer buf in
    Print_syntax.hflz Print_syntax.simple_ty_ fmt f;
    Format.pp_print_flush fmt ();
    Buffer.contents buf 
  in
  let show_fm f =
    " (formula: " ^ (show_formula f) ^ ")"
  in
  let rec go : ty_env -> Type.simple_ty Hflz.t -> Type.simple_ty = fun env hfl -> match hfl with
  | Bool _ -> Type.TyBool ()
  | Var ({ty;_} as v) -> begin
    (* 環境に出現していることをチェック *)
    (* ここで出ているVarは、Int型ではない。Int型変数はArith内に出る *)
    match List.find_opt (fun k -> Id.eq k v) env with
    | Some {ty=ty';_} -> 
      if Type.TySigma ty = ty'
      then ty
      else failwith @@ "Var: `" ^ (show_id v) ^ "` type mismatch (expected: " ^ (show_arg @@ Type.TySigma ty) ^ " / actual: " ^ (show_arg ty')  ^ ")"
    | None -> failwith @@ "Var: unbound variable (" ^ (show_id v) ^ ")"
  end
  | Or (f1, f2) -> begin
    if go env f1 = Type.TyBool () && go env f2 = Type.TyBool ()
    then Type.TyBool ()
    else assert false
  end
  | And (f1, f2) -> begin
    if go env f1 = Type.TyBool () && go env f2 = Type.TyBool ()
    then Type.TyBool ()
    else assert false
  end
  | Abs (arg, body) -> begin
    Type.TyArrow (arg, go (arg::env) body)
  end
  | Forall (arg, body) -> go (arg::env) body
  | Exists (arg, body) -> go (arg::env) body
  | App (f1, f2) -> begin
    let ty1 = go env f1 in
    match ty1 with
    (* 唯一subformulaにIntが許される *)
    | TyArrow ({ty=TyInt; _}, tybody) -> begin
      match f2 with
        | Arith arith -> 
          if type_check_arith env arith
          then tybody
          else assert false
        | _ -> failwith @@ "App: f2 should be arithmetic expression" ^ (show_fm hfl)
    end
    | TyArrow ({ty=TySigma ty; _} as arg, tybody) -> begin
      let ty2 = go env f2 in
      if remove_id_from_type ty2 = remove_id_from_type ty
      then tybody
      else failwith @@ "App_TyArrow type mismatch" ^ (show_fm  hfl) ^ "ty1=" ^ (show_arg arg.ty) ^ "/ty2=" ^ (show_arg (TySigma ty2))
    end
    | TyBool _ -> failwith @@ "App: f1 should not be boolean."
  end
  | Pred (_, args) -> begin
    List.iter (fun arg -> if type_check_arith env arg then () else assert false) args;
    let arg_num = List.length args in
    if arg_num <> 2 then assert false;
    Type.TyBool ()
  end
  | Arith _ -> assert false in
  go env hfl
  
let type_check : Type.simple_ty hes -> unit = fun hes ->
  let show_ty = Type.show_ty Fmt.nop in
  let env = List.map (fun {var={ty;_} as var;_} -> {var with ty=Type.TySigma ty}) hes in
  List.iter (fun ({var={ty;_}; body; _}) -> 
    let ty' = get_hflz_type env body in
    if not @@ eq_modulo_arg_ids ty' ty then failwith @@ "rule type mismatch (Checked type: " ^ show_ty ty' ^ " / Env type: " ^ show_ty ty ^ ")"
  ) hes
