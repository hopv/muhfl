open Core

let main path1 path2 = 
  let phi1 = Muhfl.parse path1 in
  let phi2 = Muhfl.parse path2 in
  let (top1, rules1) = Muhfl.assign_serial_to_vars_hes phi1 in
  let phi1' = Hfl.Hflz.mk_hes top1 rules1 in
  let (top2, rules2) = Muhfl.assign_serial_to_vars_hes phi2 in
  let phi2' = Hfl.Hflz.mk_hes top2 rules2 in
  let phi1' = Muhfl.Manipulate.Hflz_util.merge_entry_rule phi1' in
  let phi2' = Muhfl.Manipulate.Hflz_util.merge_entry_rule phi2' in
  let res, error_path = Muhfl.check_equal_hes phi1' phi2' in
  (if res then
    print_endline "(func) Equal"
  else
    print_endline ("(func) Not equal: " ^ error_path))
  
let command =
  Command.basic
    ~summary:"Check whether two hes files have differences"
    Command.Let_syntax.(
      let%map_open
          path1 = anon ("path1" %: string)
      and path2 = anon ("path2" %: string)
      in
      (fun () -> main path1 path2)
    )

let () = Command_unix.run command
