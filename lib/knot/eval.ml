open Ast

let rec step = function
  | Application (Fix, f) ->
      (match step f with
       | Some f' -> Some (Application (Fix, f'))
       | None -> Some (Application (f, Application (Fix, f))))
  | Application (Lambda body, arg) ->
      Some (substitute 0 arg body)
  | Application (e1, e2) ->
      (match step e1 with
       | Some e1' -> Some (Application (e1', e2))
       | None ->
           (match step e2 with
            | Some e2' -> Some (Application (e1, e2'))
            | None -> None))
  | Lambda body ->
      (match step body with
       | Some body' -> Some (Lambda body')
       | None -> None)
  | Variable _ | Fix -> None

and shift distance cutoff expr =
  match expr with
  | Fix -> Fix
  | Variable i ->
      if i >= cutoff then Variable (i + distance) else Variable i
  | Lambda body ->
      Lambda (shift distance (cutoff + 1) body)
  | Application (e1, e2) ->
      Application (shift distance cutoff e1, shift distance cutoff e2)

and substitute_at depth target replacement expr =
  match expr with
  | Fix -> Fix
  | Variable i ->
      if i = target + depth then shift depth 0 replacement
      else if i > target + depth then Variable (i - 1)
      else Variable i
  | Lambda body ->
      Lambda (substitute_at (depth + 1) target replacement body)
  | Application (e1, e2) ->
      Application (
        substitute_at depth target replacement e1,
        substitute_at depth target replacement e2
      )

and substitute target replacement expr =
  substitute_at 0 target replacement expr

let rec eval max_steps e =
  if max_steps <= 0 then e
  else
    match step e with
    | Some e' -> eval (max_steps - 1) e'
    | None -> e

let desugar (surface : surface_expr) : expr =
  let rec go scope = function
    | Surface_variable name ->
        (match List.find_index ((=) name) scope with
         | Some idx -> Variable idx
         | None -> failwith ("Unbound variable: " ^ name))
    | Surface_lambda (param, body) ->
        Lambda (go (param :: scope) body)
    | Surface_fix -> Fix
    | Surface_application (e1, e2) ->
        Application (go scope e1, go scope e2)
    | Surface_let (name, e1, e2) ->
        Application (Lambda (go (name :: scope) e2), go scope e1)
  in
  go [] surface

let rec resugar scope = function
  | Fix -> Surface_fix
  | Variable i ->
      (match List.nth_opt scope i with
       | Some name -> Surface_variable name
       | None -> Surface_variable ("v" ^ string_of_int i))
  | Lambda body ->
      let fresh_name = "x" ^ string_of_int (List.length scope) in
      Surface_lambda (fresh_name, resugar (fresh_name :: scope) body)
  | Application (e1, e2) ->
      Surface_application (resugar scope e1, resugar scope e2)
