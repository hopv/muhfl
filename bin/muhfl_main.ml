let () =
  let file =
    match Muhfl__Options.parse() with
    | Some (`File file) -> file
    | Some `Stdin ->
        let tmp_file = Filename.temp_file "stdin-" ".in" in
        let contents = Hflmc2_util.In_channel.(input_all stdin) in
        Hflmc2_util.Out_channel.with_file tmp_file ~f:begin fun ch ->
          Hflmc2_util.Out_channel.output_string ch contents
        end;
        tmp_file
    | None -> exit 1
  in
    Muhfl.main file (fun (s, debug_contexts) -> 
      match s with
      | r ->
          if Logs.Src.level Muhfl.log_src <> None then begin
            Fmt.pr "@[<v 2>[[MAIN]] Verification Result:@,%s@]@." @@ Muhfl.show_result r;
            Muhfl.report_times ();
            print_endline @@ Muhfl.show_debug_contexts debug_contexts
          end else 
            Fmt.pr "%s\n" (Muhfl.show_result r);
      | exception
          ( Muhfl.Util.Fn.Fatal e
          | Muhfl.Syntax.ParseError e
          | Muhfl.Syntax.LexingError e
          ) -> print_endline e; exit 1
    )
