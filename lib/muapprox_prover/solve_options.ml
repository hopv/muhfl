type solver_type = Iwayama | Katsura | Suzuki
type first_order_solver_type = FptProverRecLimit

type options = 
  {
    no_backend_inlining : bool;
    no_disprove: bool;
    timeout : float;
    print_for_debug : bool;
    separate_original_formula_in_exists : bool;
    solver : solver_type;
    solver_backend: string option;
    first_order_solver: first_order_solver_type option;
    coe : int * int;
    dry_run : bool;
    no_simplify: bool;
    stop_on_unknown: bool;
    pid: int;
    file: string;
    always_approximate: bool;
    assign_values_for_exists_at_first_iteration: bool;
    default_lexicographic_order: int;
    simplify_bound: bool;
    use_simple_encoding_when_lexico_is_one: bool;
    disable_lexicographic: bool;
    add_arguments: bool;
    coe_arguments: int * int;
    no_elim: bool;
    eliminate_unused_arguments: bool;
    partial_analysis: bool;
    use_related: bool;
    use_all_variables: bool;
  }

let get_solver solver_name = 
  match solver_name with
  | "iwayama" -> Iwayama
  | "katsura" -> Katsura
  | "suzuki"  -> Suzuki
  | s -> failwith @@ "unknown solver \"" ^ s ^ "\""

let get_first_order_solver use_fo_solver = 
  if use_fo_solver then Some FptProverRecLimit else None
  
let get_solver_backend s solver =
  match solver with
  | Katsura -> begin
    (* solver-backend option is only for katsura-solver *)
    let s = String.trim s in
    match s with
    | "" -> None
    | s -> Some s
  end
  | _ -> Some "hoice" (* set a random value to use only one solver when solving *)

(* let get_first_order_solver solver_name =
  match solver_name with
  | "fptprover-rec-limit" -> Some FptProverRecLimit
  | "" -> None
  | s -> failwith @@ "unknown solver \"" ^ s ^ "\"" *)

let get_coe coe_opt = 
  match String.trim coe_opt with
  | "" -> failwith "get_coe: should not be blank"
  | c -> begin
    match String.split_on_char ',' c with
    | [c1; c2] -> begin
      try
        (int_of_string c1, int_of_string c2)
      with Failure _ -> 
        failwith "get_coe: Format error. format of coefficients is like \" 2,10 \""
    end
    | _ -> failwith "get_coe: Format error. format of coefficients is like \" 2,10 \""
  end