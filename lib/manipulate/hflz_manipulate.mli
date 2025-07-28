open Hfl

val get_dual_hes :
  Type.simple_ty Hflz.hes ->
  Type.simple_ty Hflz.hes

val encode_body_forall_except_top :
  Type.simple_ty Hflz.hes ->
  Type.simple_ty Hflz.hes

val decompose_lambdas_hes :
  Type.simple_ty Hflz.hes ->
  Type.simple_ty Hflz.hes

val encode_body_exists :
  int ->
  int ->
  Type.simple_ty Hflz.hes ->
  Hflz_util.variable_type IdMap.t ->
  (unit Id.t * [`Int] Id.t) list ->
  bool ->
  Type.simple_ty Hflz.hes

val elim_mu_with_rec :
  Type.simple_ty Hflz.hes ->
  int ->
  int ->
  int ->
  Hflz_util.variable_type IdMap.t ->
  bool ->
  (unit Id.t * [`Int] Id.t) list ->
  string ->
  Type.simple_ty Hflz.hes

(* flag *)
val simplify_bound : bool ref

val get_outer_mu_funcs : 'a Hflz.hes_rule list -> ('a Id.t * 'a Id.t list) list
