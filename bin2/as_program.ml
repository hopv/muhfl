open Core

let map_file_path path converter =
  let dir, base, ext =
    Stdlib.Filename.dirname path,
    Stdlib.Filename.remove_extension (Stdlib.Filename.basename path),
    Stdlib.Filename.extension path in
  let dir, base, ext = converter (dir, base, ext) in
  Stdlib.Filename.concat dir (base ^ ext)

let main filepath =
  let hes = Muhfl.parse filepath in
  let hes = Muhfl.Manipulate.Hflz_manipulate.get_dual_hes hes in
  let path2 = map_file_path filepath (fun (a, b, _) -> (a, b ^ "_out", ".ml")) in
  ignore @@ Muhfl.Manipulate.Print_syntax.AsProgram.save_hes_to_file ~file:path2 hes;
  print_endline @@ "Program: " ^ path2
  
let command =
  Command.basic
    ~summary:"Return a \"program\" of the given HES formula"
    Command.Let_syntax.(
      let%map_open
          filepath = anon ("filepath" %: string)
      in
      (fun () -> main filepath)
    )

let () = Command_unix.run command
