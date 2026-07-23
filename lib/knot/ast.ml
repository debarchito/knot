type surface_expr =
  | Surface_variable of string
  | Surface_lambda of string * surface_expr
  | Surface_fix
  | Surface_application of surface_expr * surface_expr
  | Surface_let of string * surface_expr * surface_expr

type expr =
  | Variable of int
  | Lambda of expr
  | Fix
  | Application of expr * expr

type type_ =
  | Type_variable of type_variable
  | Type_arrow of type_ * type_
and type_variable = type_node ref
and type_node =
  | Type_node_unbound of int * int
  | Type_node_link of type_

type scheme = Forall of int list * type_

let rec repr = function
  | Type_variable ({ contents = Type_node_link t } as r) ->
      let t' = repr t in
      r := Type_node_link t';
      t'
  | t -> t

let rec string_of_type t =
  let rec go vars t =
    match repr t with
    | Type_variable { contents = Type_node_unbound (id, _) } ->
        (match List.assoc_opt id !vars with
         | Some name -> name, !vars
         | None ->
             let name = "'" ^ String.make 1 (Char.chr (97 + (List.length !vars mod 26))) in
             let name = if List.length !vars >= 26 then name ^ string_of_int (List.length !vars / 26) else name in
             vars := (id, name) :: !vars;
             name, !vars)
    | Type_variable { contents = Type_node_link t } -> go vars t
    | Type_arrow (t1, t2) ->
        let s1, _ = go vars t1 in
        let s2, _ = go vars t2 in
        let s1 = match repr t1 with Type_arrow _ -> "(" ^ s1 ^ ")" | _ -> s1 in
        s1 ^ " -> " ^ s2, !vars
  in
  let vars = ref [] in
  fst (go vars t)

let rec string_of_expr_surface = function
  | Surface_variable name -> name
  | Surface_lambda (param, body) ->
      "fn " ^ param ^ ". " ^ string_of_expr_surface body
  | Surface_fix -> "fix"
  | Surface_application (e1, e2) ->
      let s1 = match e1 with
        | Surface_lambda _ -> "(" ^ string_of_expr_surface e1 ^ ")"
        | _ -> string_of_expr_surface e1
      in
      let s2 = match e2 with
        | Surface_application _ | Surface_lambda _ -> "(" ^ string_of_expr_surface e2 ^ ")"
        | _ -> string_of_expr_surface e2
      in
      s1 ^ " " ^ s2
  | Surface_let (name, e1, e2) ->
      "let " ^ name ^ " = " ^ string_of_expr_surface e1 ^ " in " ^ string_of_expr_surface e2
