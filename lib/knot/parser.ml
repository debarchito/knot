open Ast

type token =
  | Token_lambda
  | Token_dot
  | Token_fix
  | Token_let
  | Token_equal
  | Token_in
  | Token_left_paren
  | Token_right_paren
  | Token_variable of string
  | Token_end_of_file

let lex (str : string) : token list =
  let len = String.length str in
  let rec go i acc =
    if i >= len then List.rev (Token_end_of_file :: acc)
    else match str.[i] with
    | ' ' | '\t' | '\r' | '\n' -> go (i + 1) acc
    | '/' when i + 1 < len && str.[i + 1] = '/' ->
        let rec skip_comment j =
          if j >= len || str.[j] = '\n' then j
          else skip_comment (j + 1)
        in
        go (skip_comment (i + 2)) acc
    | '\\' -> go (i + 1) (Token_lambda :: acc)
    | '.' -> go (i + 1) (Token_dot :: acc)
    | '=' -> go (i + 1) (Token_equal :: acc)
    | '(' -> go (i + 1) (Token_left_paren :: acc)
    | ')' -> go (i + 1) (Token_right_paren :: acc)
    | c when (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || (c >= '0' && c <= '9') || c = '\'' ->
        let start = i in
        let rec find_end j =
          if j < len && (let cz = str.[j] in (cz >= 'a' && cz <= 'z') || (cz >= 'A' && cz <= 'Z') || cz = '_' || (cz >= '0' && cz <= '9') || cz = '\'')
          then find_end (j + 1)
          else j
        in
        let finish = find_end i in
        let word = String.sub str start (finish - start) in
        let token = match word with
          | "fn" -> Token_lambda
          | "fix" -> Token_fix
          | "let" -> Token_let
          | "in"  -> Token_in
          | _     -> Token_variable word
        in
        go finish (token :: acc)
    | invalid -> failwith (Printf.sprintf "Unexpected character: '%c'" invalid)
  in
  go 0 []

let parse (tokens : token list) : surface_expr option =
  let tokens = ref tokens in
  let peek () = List.hd !tokens in
  let consume () = tokens := List.tl !tokens in
  let expect token_expected =
    if peek () = token_expected then consume ()
    else failwith "Syntax error: Unexpected token"
  in

  let rec parse_expr () =
    match peek () = Token_lambda with
    | true ->
        consume ();
        let rec parse_params () =
          match peek () with
          | Token_variable param ->
              consume ();
              if peek () = Token_dot then (
                consume ();
                Surface_lambda (param, parse_expr ())
              ) else (
                Surface_lambda (param, parse_params ())
              )
          | _ -> failwith "Expected variable name after lambda"
        in
        parse_params ()
    | false ->
        (match peek () = Token_let with
         | true ->
             consume ();
             (match peek () with
              | Token_variable name ->
                  consume ();
                  expect Token_equal;
                  let e1 = parse_expr () in
                  expect Token_in;
                  let e2 = parse_expr () in
                  Surface_let (name, e1, e2)
              | _ -> failwith "Expected variable name after let")
         | false -> parse_app ())

  and parse_app () =
    let left = parse_atom () in
    let rec loop acc =
      match peek () with
      | Token_variable _ | Token_left_paren | Token_lambda | Token_fix ->
          let right = parse_atom () in
          loop (Surface_application (acc, right))
      | _ -> acc
    in
    loop left

  and parse_atom () =
    match peek () = Token_fix with
    | true ->
        consume ();
        Surface_fix
    | false ->
        (match peek () with
         | Token_variable name ->
             consume ();
             Surface_variable name
         | Token_left_paren ->
             consume ();
             let e = parse_expr () in
             expect Token_right_paren;
             e
         | _ -> failwith "Syntax error: Expected variable, fix, or '('")
  in

  if peek () = Token_end_of_file then None
  else
    let result = parse_expr () in
    if peek () <> Token_end_of_file then failwith "Syntax error: Trailing input";
    Some result
