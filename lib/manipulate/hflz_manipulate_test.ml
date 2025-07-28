
let%expect_test "beta" =
  let open Hflz_typecheck in
  let open Hflz in
  let open Type in
  let open Hfl in
  let open Print_syntax in
  ignore @@ Id.gen_id () + Id.gen_id () + Id.gen_id () + Id.gen_id () + Id.gen_id () + Id.gen_id ();
  (* print_endline @@ string_of_int @@ Id.gen_id (); *)
  let id_n n t = { Id.name = "x_" ^ string_of_int n; id = n; ty = t } in
  let tysbool = TySigma (TyBool ()) in
  let () =
    (* distinguish free variables *)
    (*  (\x. x /\ (\x. x)) y  *)
    let phi_original = And (Var (id_n 1 (TyBool ())), Abs (id_n 1 (TySigma (TyBool ())), Var (id_n 1 (TyBool ())))) in
    let phi_all_original = App (Abs (id_n 1 (TySigma (TyBool ())), phi_original), Bool false) in
    
    print_endline @@ "original: " ^ show_hflz phi_original;
    [%expect {| original: x_11 && (λx_11:bool.x_11) |}];
    
    let () =
      let phi = Trans.Subst.Hflz.hflz (IdMap.of_list [(id_n 1 (), Bool false)]) phi_original in
      print_endline @@ "after (Trans.Subst.Hflz.hflz): " ^ show_hflz phi;
      [%expect {| after (Trans.Subst.Hflz.hflz): false && (λx_11:bool.x_11) |}] in

    (* ***************** *)
    print_endline @@ "All Original: " ^ show_hflz phi_all_original;
    [%expect {| All Original: (λx_11:bool.x_11 && (λx_11:bool.x_11)) false |}];
    
    let () =
      let phi_all = Trans.Reduce.Hflz.beta phi_all_original in
      print_endline @@ "After (Trans.Reduce.Hflz.beta): " ^ show_hflz phi_all;
      [%expect {| After (Trans.Reduce.Hflz.beta): false && (λx_11:bool.x_11) |}] in
    
    let () =
      let _, phi_all = Hflz_util.beta IdMap.empty phi_all_original in 
      print_endline @@ "After (Hflz_util.beta): " ^ show_hflz phi_all;
      [%expect {| After (Hflz_util.beta): false && (λx_11:bool.x_11) |}] in
    
    ()
  in
  
  let () =
    (* substitution時に重複するbindingがあった場合、bindingの名前を変える *)
    (*  (\x. (\z. x /\ z)) (\z. z)  *)
    let phi_all_original =
      App (
        Abs (
          id_n 1 tysbool,
          Abs (
            id_n 2 tysbool,
            And (
              Var (id_n 1 (TyBool ())),
              Var (id_n 2 (TyBool ()))
            )
          )
        ),
        Abs (
          id_n 2 tysbool,
          Var (id_n 2 (TyBool ()))
        )
      ) in
      
      print_endline @@ "All Original: " ^ show_hflz phi_all_original;
      [%expect {| All Original: (λx_11:bool.λx_22:bool.x_11 && x_22) (λx_22:bool.x_22) |}];
      
      let () =
        let phi_all = Trans.Reduce.Hflz.beta phi_all_original in
        print_endline @@ "After (Trans.Reduce.Hflz.beta): " ^ show_hflz phi_all;
        [%expect {| After (Trans.Reduce.Hflz.beta): λx_22:bool.(λx_27:bool.x_27) && x_22 |}] in
      
      let () =
        let _, phi_all = Hflz_util.beta IdMap.empty phi_all_original in
        print_endline @@ "After (Hflz_util.beta): " ^ show_hflz phi_all;
        [%expect {| After (Hflz_util.beta): λx_22:bool.(λx_28:bool.x_28) && x_22 |}] in
      () 
  in
  print_endline "";
  let () =
    (* capture avoiding substitution *)
    (* TODO => していない！！！ *)
    (*  (\z. (\x. (\z. x)) z /\ z)  *)
    let phi_all_original =
      Abs (
        id_n 1 tysbool,
        And (
          App (
            Abs (
              id_n 2 tysbool,
              Abs (
                id_n 1 tysbool,
                Var (id_n 2 (TyBool ()))
              )
            ),
            Var (id_n 1 (TyBool ()))
          ),
          Var (id_n 1 (TyBool ()))
        )
      ) in
      
      print_endline @@ "All Original: " ^ show_hflz phi_all_original;
      [%expect {| All Original: λx_11:bool.(λx_22:bool.λx_11:bool.x_22) x_11 && x_11 |}];
      
      let () =
        try
          let phi_all = Trans.Reduce.Hflz.beta phi_all_original in
          print_endline @@ "After (Trans.Reduce.Hflz.beta): " ^ show_hflz phi_all
        with Failure s -> print_endline @@ "Failure: " ^ s in
      [%expect {| Failure: Variable capture in substituion (x_11) |}];
      
      let () =
        try
          let _, phi_all = Hflz_util.beta IdMap.empty phi_all_original in 
          print_endline @@ "After (Hflz_util.beta): " ^ show_hflz phi_all
        with Failure s -> print_endline @@ "Failure: " ^ s in
      [%expect {| Failure: Variable capture in substituion (x_11) |}];
      ()
    in
  ()

open Hflz_typecheck
open Hflz
open Hflz_manipulate
module Util = Hflmc2_util

let%expect_test "desugar_formula" =
  let open Type in
  let id_n n t = { Id.name = "x_" ^ string_of_int n; id = n; ty = t } in
  let sugar : simple_ty Hflz.Sugar.t =
    (* true && (not (true && ∀x2. 1 >= x2 || ∃x3. not (true && x4 5))) *)
    (* => *)
    (* true && (false || ∃x2. 1 < x2 && ∀x3. true && x4 5) *)
    And (Bool true, Not (And (Bool true, Forall (id_n 2 TyInt, Or (Pred (Ge, [Int 1; Var (id_n 2 `Int)]), Exists (id_n 3 TyInt, Not (And (Bool true, App (Var (id_n 4 (TyBool ())), Arith (Int 5)))))))))) in
  let desugar = Hflz.desugar_formula sugar in
  ignore [%expect.output];
  print_endline @@ Print_syntax.show_hflz desugar;
  [%expect {| true && (false || (∃x_22.1 < x_22 && (∀x_33.true && x_44 5))) |}]
