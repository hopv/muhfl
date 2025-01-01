open Hflmc2_syntax

val get_dual_hes :
  Hflmc2_syntax.Type.simple_ty Hflz.hes ->
  Hflmc2_syntax.Type.simple_ty Hflz.hes

val encode_body_forall_except_top :
  Hflmc2_syntax.Type.simple_ty Hflz.hes ->
  Hflmc2_syntax.Type.simple_ty Hflz.t *
  Hflmc2_syntax.Type.simple_ty Hflz.hes_rule list

val decompose_lambdas_hes :
  Hflmc2_syntax.Type.simple_ty Hflz.t *
  Hflmc2_syntax.Type.simple_ty Hflz.hes_rule list ->
  Hflmc2_syntax.Type.simple_ty Hflz.t *
  Hflmc2_syntax.Type.simple_ty Hflz.hes_rule list

val encode_body_exists :
  int ->
  int ->
  Hflmc2_syntax.Type.simple_ty Hflz.hes ->
  Hflz_util.variable_type Hflmc2_syntax.IdMap.t ->
  (unit Id.t * [ `Int ] Id.t) list ->
  bool ->
  Hflmc2_syntax.Type.simple_ty Hflz.t *
  Hflmc2_syntax.Type.simple_ty Hflz.hes_rule list

val elim_mu_with_rec :
  Hflmc2_syntax.Type.simple_ty Hflz.t *
  Hflmc2_syntax.Type.simple_ty Hflz.hes_rule list ->
  int ->
  int ->
  int ->
  Hflz_util.variable_type Hflmc2_syntax.IdMap.t ->
  bool ->
  (unit Id.t * [ `Int ] Id.t) list ->
  string ->
  Hflmc2_syntax.Type.simple_ty Hflz.t *
  Hflmc2_syntax.Type.simple_ty Hflz.hes_rule list

(* flag *)
val simplify_bound : bool ref

val get_outer_mu_funcs : 'a Hflz.hes_rule list -> ('a Id.t * 'a Id.t list) list
