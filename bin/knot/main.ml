open Knot

let mode = ref Repl.Typed
let max_steps = ref 100
let eval_expr = ref None
let input_file = ref None

let set_eval_string s =
  eval_expr := Some s

let set_input_file filename =
  match !input_file with
  | None -> input_file := Some filename
  | Some _ ->
      Printf.eprintf "Error: Multiple input files provided.\n";
      exit 1

let read_file filename =
  let ch = open_in filename in
  let len = in_channel_length ch in
  let content = really_input_string ch len in
  close_in ch;
  content

let run_non_interactive source =
  try
    if String.trim source <> "" then (
      let tokens = Parser.lex source in
      match Parser.parse tokens with
      | None -> ()
      | Some surface ->
          let desugared = Eval.desugar surface in

          if !mode = Repl.Typed then (
            Typecheck.current_level := 0;
            Typecheck.id_counter := 0;
            let t = Typecheck.infer (Typecheck.initial_env ()) desugared in
            let type_str = Ast.string_of_type t in
            Printf.printf "%s : %s\n" 
              (Repl.Color.fmt Repl.Color.gray "-") 
              (Repl.Color.fmt (Repl.Color.bold ^ Repl.Color.yellow) type_str)
          );

          let reduced_expr, stopped_early = Repl.eval_with_limit !max_steps desugared in
          if stopped_early then
            Printf.printf "%s Stopped after evaluating %d steps.\n"
              (Repl.Color.fmt Repl.Color.cyan "Info:") !max_steps;

          let resugared_surface = Eval.resugar [] reduced_expr in
          let expr_str = Ast.string_of_expr_surface resugared_surface in
          Printf.printf "%s = %s\n" 
            (Repl.Color.fmt Repl.Color.gray "-") 
            (Repl.Color.fmt (Repl.Color.bold ^ Repl.Color.green) expr_str)
    )
  with
  | Failure msg -> 
      Printf.eprintf "%s %s\n" 
        (Repl.Color.fmt (Repl.Color.bold ^ Repl.Color.red) "Error:") msg;
      exit 1
  | exn -> 
      Printf.eprintf "%s %s\n" 
        (Repl.Color.fmt (Repl.Color.bold ^ Repl.Color.red) "Error:") (Printexc.to_string exn);
      exit 1

let () =
  let speclist = ref [] in

  let print_usage () =
    Arg.usage !speclist "Usage: knot [options] [<file>]\n\nOptions:";
    exit 0
  in

  speclist := Arg.align [
    ("-e", Arg.String set_eval_string, " Evaluate expression directly");
    ("-t", Arg.Unit (fun () -> mode := Repl.Typed), " Enable type inference (default)");
    ("-u", Arg.Unit (fun () -> mode := Repl.Untyped), " Disable type inference (untyped mode)");
    ("-m", Arg.Set_int max_steps, " Set maximum evaluation steps (default: 100)");
    ("-h", Arg.Unit print_usage, " Display this list of options");
  ];

  (try
     Arg.parse_argv Sys.argv !speclist set_input_file "Usage: knot [options] [<file.knot>]\n\nOptions:"
   with
   | Arg.Bad msg ->
       Printf.eprintf "%s" msg;
       exit 1
   | Arg.Help _ ->
       print_usage ());

  match !eval_expr, !input_file with
  | Some source, _ ->
      run_non_interactive source
  | None, Some filename ->
      let source = read_file filename in
      run_non_interactive source
  | None, None ->
      Repl.run ()
