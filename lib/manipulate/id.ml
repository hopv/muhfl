include Hfl.Id
let is_pred_name pvar_name =
  Stdlib.String.length pvar_name >= 0 &&
  Stdlib.String.sub pvar_name 0 1 <> "_" && (Stdlib.String.uppercase_ascii @@ Stdlib.String.sub pvar_name 0 1) = Stdlib.String.sub pvar_name 0 1

let remove_vars not_apply_vars =
  Base.List.filter ~f:(fun v -> not @@ Base.List.exists ~f:(fun v' -> eq v' (remove_ty v)) @@ not_apply_vars)

let to_string ?(without_id=false) id =
  if is_pred_name id.name || without_id
  then id.name
  else id.name ^ string_of_int id.id (* also show id if the id is for a variable *)
