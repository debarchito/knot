open Ast

type mode = Typed | Untyped

module Color = struct
  let reset   = "\x1b[0m"
  let bold    = "\x1b[1m"
  let dim     = "\x1b[2m"
  
  let red     = "\x1b[31m"
  let green   = "\x1b[32m"
  let yellow  = "\x1b[33m"
  let blue    = "\x1b[34m"
  let magenta = "\x1b[35m"
  let cyan    = "\x1b[36m"
  let gray    = "\x1b[90m"

  let fmt style text = style ^ text ^ reset
end

type editor_state = {
  buffer : bytes;
  mutable len : int;
  mutable pos : int;
  mutable history : string list;
  mutable history_idx : int;
  mutable undo_stack : string list;
  mutable redo_stack : string list;
}

let create_state () = {
  buffer = Bytes.create 4096;
  len = 0;
  pos = 0;
  history = [];
  history_idx = -1;
  undo_stack = [];
  redo_stack = [];
}

let get_content state = Bytes.sub_string state.buffer 0 state.len

let save_undo state =
  let curr = get_content state in
  match state.undo_stack with
  | top :: _ when top = curr -> ()
  | _ ->
      state.undo_stack <- curr :: state.undo_stack;
      state.redo_stack <- []

let set_content state text =
  let l = String.length text in
  Bytes.blit_string text 0 state.buffer 0 l;
  state.len <- l;
  state.pos <- l

let insert_char state c =
  save_undo state;
  if state.len < Bytes.length state.buffer then (
    if state.pos < state.len then
      Bytes.blit state.buffer state.pos state.buffer (state.pos + 1) (state.len - state.pos);
    Bytes.set state.buffer state.pos c;
    state.len <- state.len + 1;
    state.pos <- state.pos + 1
  )

let delete_backspace state =
  if state.pos > 0 then (
    save_undo state;
    Bytes.blit state.buffer state.pos state.buffer (state.pos - 1) (state.len - state.pos);
    state.len <- state.len - 1;
    state.pos <- state.pos - 1
  )

let delete_forward state =
  if state.pos < state.len then (
    save_undo state;
    Bytes.blit state.buffer (state.pos + 1) state.buffer state.pos (state.len - state.pos - 1);
    state.len <- state.len - 1
  )

let perform_undo state =
  match state.undo_stack with
  | [] -> ()
  | top :: rest ->
      state.redo_stack <- get_content state :: state.redo_stack;
      state.undo_stack <- rest;
      set_content state top

let perform_redo state =
  match state.redo_stack with
  | [] -> ()
  | top :: rest ->
      state.undo_stack <- get_content state :: state.undo_stack;
      state.redo_stack <- rest;
      set_content state top

let refresh_line prompt_styled state =
  print_string ("\r\x1b[K" ^ prompt_styled ^ get_content state);
  let cursor_back = state.len - state.pos in
  if cursor_back > 0 then
    Printf.printf "\x1b[%dD" cursor_back;
  flush stdout

let read_key () =
  let buf = Bytes.create 3 in
  let n = Unix.read Unix.stdin buf 0 1 in
  if n = 0 then `Eof
  else match Bytes.get buf 0 with
  | '\x03' -> `CtrlC
  | '\x04' -> `Eof
  | '\x1a' -> `CtrlZ
  | '\x19' -> `CtrlY
  | '\r' | '\n' -> `Enter
  | '\x7f' | '\x08' -> `Backspace
  | '\x1b' ->
      let _ = Unix.read Unix.stdin buf 1 2 in
      (match Bytes.get buf 1, Bytes.get buf 2 with
       | '[', 'A' -> `Up
       | '[', 'B' -> `Down
       | '[', 'C' -> `Right
       | '[', 'D' -> `Left
       | '[', '3' -> `Delete
       | '[', 'H' -> `Home
       | '[', 'F' -> `End
       | _ -> `Unknown)
  | c -> `Char c

let set_raw_mode termios =
  let raw = { termios with
    Unix.c_icanon = false;
    Unix.c_echo = false;
    Unix.c_isig = false;
    Unix.c_vtime = 0;
    Unix.c_vmin = 1;
  } in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN raw

let is_complete input =
  try
    let tokens = Parser.lex input in
    let parens = ref 0 in
    let lets = ref 0 in
    let ins = ref 0 in
    let ended_with_in = ref false in
    
    let rec check = function
      | [] -> ()
      | Parser.Token_left_paren :: rest -> incr parens; check rest
      | Parser.Token_right_paren :: rest -> decr parens; check rest
      | Parser.Token_let :: rest -> incr lets; ended_with_in := false; check rest
      | Parser.Token_in :: rest -> 
          incr ins; 
          (match rest with [] -> ended_with_in := true | _ -> ended_with_in := false);
          check rest
      | _ :: rest -> check rest
    in
    check tokens;
    
    !parens <= 0 && !lets <= !ins && not !ended_with_in
  with _ ->
    false

let read_line_custom prompt state =
  state.len <- 0;
  state.pos <- 0;
  state.history_idx <- -1;
  state.undo_stack <- [];
  state.redo_stack <- [];
  refresh_line prompt state;
  
  let rec loop () =
    match read_key () with
    | `Eof -> None
    | `CtrlC -> print_newline (); None
    | `Enter ->
        print_newline ();
        Some (get_content state)
    | `Char c ->
        insert_char state c;
        refresh_line prompt state;
        loop ()
    | `Backspace ->
        delete_backspace state;
        refresh_line prompt state;
        loop ()
    | `Delete ->
        delete_forward state;
        refresh_line prompt state;
        loop ()
    | `Left ->
        if state.pos > 0 then state.pos <- state.pos - 1;
        refresh_line prompt state;
        loop ()
    | `Right ->
        if state.pos < state.len then state.pos <- state.pos + 1;
        refresh_line prompt state;
        loop ()
    | `Home ->
        state.pos <- 0;
        refresh_line prompt state;
        loop ()
    | `End ->
        state.pos <- state.len;
        refresh_line prompt state;
        loop ()
    | `CtrlZ ->
        perform_undo state;
        refresh_line prompt state;
        loop ()
    | `CtrlY ->
        perform_redo state;
        refresh_line prompt state;
        loop ()
    | `Up ->
        if state.history <> [] && state.history_idx < List.length state.history - 1 then (
          state.history_idx <- state.history_idx + 1;
          set_content state (List.nth state.history state.history_idx);
          refresh_line prompt state
        );
        loop ()
    | `Down ->
        if state.history_idx > 0 then (
          state.history_idx <- state.history_idx - 1;
          set_content state (List.nth state.history state.history_idx);
          refresh_line prompt state
        ) else if state.history_idx = 0 then (
          state.history_idx <- -1;
          set_content state "";
          refresh_line prompt state
        );
        loop ()
    | `Unknown -> loop ()
  in
  loop ()

let eval_with_limit max_steps expr =
  let reduced = Eval.eval max_steps expr in
  let can_step =
    try
      let double_check = Eval.eval 1 reduced in
      double_check <> reduced
    with _ -> false
  in
  (reduced, can_step)

let run () =
  let banner = Color.fmt (Color.bold ^ Color.cyan) "Knot" ^ " is a pure λ-calculus reduction engine with optional type inference. Type " ^ Color.fmt Color.yellow ":h" ^ " for help.\n" in
  print_endline banner;

  let orig_termios = Unix.tcgetattr Unix.stdin in
  set_raw_mode orig_termios;
  
  let mode = ref Typed in
  let max_steps = ref 100 in
  let editor = create_state () in

  let rec loop multiline_buffer =
    let prompt =
      if multiline_buffer <> "" then 
        Color.fmt Color.gray "...  "
      else if !mode = Typed then 
        Color.fmt (Color.bold ^ Color.cyan) "t> "
      else 
        Color.fmt (Color.bold ^ Color.magenta) "u> "
    in
    match read_line_custom prompt editor with
    | None ->
        Unix.tcsetattr Unix.stdin Unix.TCSADRAIN orig_termios
    | Some line ->
        let line_trimmed = String.trim line in
        if String.starts_with ~prefix:":m" line_trimmed && multiline_buffer = "" then (
          let parts = String.split_on_char ' ' line_trimmed |> List.filter (fun s -> s <> "") in
          (match parts with
           | [":m"; num_str] ->
               (match int_of_string_opt num_str with
                | Some n when n > 0 ->
                    max_steps := n;
                    Printf.printf "%s Maximum evaluation steps set to %d.\n\n" 
                      (Color.fmt Color.green "Updated:") n
                | _ ->
                    Printf.printf "%s Please provide a positive integer (e.g., :m 100).\n\n" 
                      (Color.fmt (Color.bold ^ Color.red) "Error:"))
           | [":m"] ->
               Printf.printf "%s Current maximum evaluation steps is set to %d.\n\n" 
                 (Color.fmt Color.cyan "Info:") !max_steps
           | _ ->
               Printf.printf "%s Usage: :m <positive_integer>\n\n" 
                 (Color.fmt (Color.bold ^ Color.red) "Error:"));
          loop ""
        ) else match line_trimmed with
        | ":h" when multiline_buffer = "" ->
            Printf.printf "%s = Exit REPL.\n" (Color.fmt Color.yellow ":e");
            Printf.printf "%s = Turn on type inference.\n" (Color.fmt Color.yellow ":t");
            Printf.printf "%s = Turn off type inference.\n" (Color.fmt Color.yellow ":u");
            Printf.printf "%s = Set maximum evaluation steps (e.g. :m 500).\n\n" (Color.fmt Color.yellow ":m <num>");
            loop ""
        | ":e" when multiline_buffer = "" ->
            Unix.tcsetattr Unix.stdin Unix.TCSADRAIN orig_termios
        | ":t" when multiline_buffer = "" ->
            mode := Typed;
            Printf.printf "%s Turned on type inference.\n\n"
              (Color.fmt Color.cyan "Info:");
            loop ""
        | ":u" when multiline_buffer = "" ->
            mode := Untyped;
            Printf.printf "%s Turned off type inference.\n\n"
              (Color.fmt Color.cyan "Info:");
            loop ""
        | _ ->
            if line_trimmed = "" && multiline_buffer = "" then loop ""
            else
              let full_input = if multiline_buffer = "" then line else multiline_buffer ^ "\n" ^ line in
              let force_run = String.ends_with ~suffix:";;" line_trimmed in
              let input_to_run =
                if force_run then
                  String.sub full_input 0 (String.length full_input - 2)
                else full_input
              in

              if force_run || is_complete input_to_run then (
                editor.history <- input_to_run :: editor.history;
                (try
                   if String.trim input_to_run <> "" then (
                     let tokens = Parser.lex input_to_run in
                     match Parser.parse tokens with
                     | None -> () 
                     | Some surface ->
                         let desugared = Eval.desugar surface in

                         if !mode = Typed then (
                           Typecheck.current_level := 0;
                           Typecheck.id_counter := 0;
                           let t = Typecheck.infer (Typecheck.initial_env ()) desugared in
                           let type_str = string_of_type t in
                           Printf.printf "%s : %s\n" 
                             (Color.fmt Color.gray "-") 
                             (Color.fmt (Color.bold ^ Color.yellow) type_str)
                         );

                         let reduced_expr, stopped_early = eval_with_limit !max_steps desugared in
                         if stopped_early then
                           Printf.printf "%s Stopped after evaluating %d steps.\n"
                             (Color.fmt Color.cyan "Info:") !max_steps;

                         let resugared_surface = Eval.resugar [] reduced_expr in
                         let expr_str = string_of_expr_surface resugared_surface in
                         Printf.printf "%s = %s\n\n" 
                           (Color.fmt Color.gray "-") 
                           (Color.fmt (Color.bold ^ Color.green) expr_str)
                   )
                 with
                 | Failure msg -> 
                     Printf.printf "%s %s\n\n" 
                       (Color.fmt (Color.bold ^ Color.red) "Error:") msg
                 | exn -> 
                     Printf.printf "%s %s\n\n" 
                       (Color.fmt (Color.bold ^ Color.red) "Error:") (Printexc.to_string exn));
                loop ""
              ) else (
                loop full_input
              )
  in
  loop ""
