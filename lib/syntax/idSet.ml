open Hflmc2_util
include Set.Make'(Id.Key)
let singleton : 'a. 'a Id.t -> t =
  fun v -> singleton (Id.remove_ty v)
let remove : 'a. t -> 'a Id.t -> t =
  fun set x -> Set.remove set (Id.remove_ty x)

 (* TODO (refactor): delete these *)
let mem set x = Set.mem set (Id.remove_ty x)
let add set x = Set.add set (Id.remove_ty x)
let union = Set.union
let exists = Set.exists
let filter = Set.filter
let find = Set.find
let fold = Set.fold
let is_empty = Set.is_empty
let to_list = Set.to_list
