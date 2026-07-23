open Ast

let current_level = ref 0
let id_counter = ref 0

let fresh_var () =
  incr id_counter;
  Type_variable (ref (Type_node_unbound (!id_counter, !current_level)))

let enter_level () = incr current_level
let leave_level () = decr current_level

let rec occurs_check id level = function
  | Type_arrow (t1, t2) ->
      occurs_check id level (repr t1);
      occurs_check id level (repr t2)
  | Type_variable ({ contents = Type_node_unbound (id2, level2) } as r2) ->
      if id = id2 then failwith "Type Error: Occurs check failure (infinite type)"
      else if level2 > level then
        r2 := Type_node_unbound (id2, level)
  | Type_variable { contents = Type_node_link t } ->
      occurs_check id level t

let rec unify t1 t2 =
  let t1' = repr t1 in
  let t2' = repr t2 in
  match (t1', t2') with
  | Type_variable r1, Type_variable r2 when r1 == r2 -> ()
  | Type_variable ({ contents = Type_node_unbound (id, level) } as r), t
  | t, Type_variable ({ contents = Type_node_unbound (id, level) } as r) ->
      occurs_check id level t;
      r := Type_node_link t
  | Type_arrow (a1, b1), Type_arrow (a2, b2) ->
      unify a1 a2;
      unify b1 b2
  | _, _ ->
      failwith "Type error: unification failed"

let generalize t =
  let rec find_vars acc t =
    match repr t with
    | Type_variable { contents = Type_node_unbound (id, level) } ->
        if level > !current_level && not (List.mem id acc) then id :: acc else acc
    | Type_arrow (t1, t2) -> find_vars (find_vars acc t1) t2
    | _ -> acc
  in
  let vars = find_vars [] t in
  Forall (vars, t)

let instantiate (Forall (quantified, t)) =
  let subst = List.map (fun id -> (id, fresh_var ())) quantified in
  let rec replace t =
    match repr t with
    | Type_variable { contents = Type_node_unbound (id, _) } ->
        (match List.assoc_opt id subst with
         | Some tv -> tv
         | None -> t)
    | Type_arrow (t1, t2) -> Type_arrow (replace t1, replace t2)
    | _ -> t
  in
  replace t

let initial_env () =
  let a = fresh_var () in
  let fix_type = Forall ([!id_counter], Type_arrow (Type_arrow (a, a), a)) in
  [fix_type]

let rec infer (env : scheme list) (e : expr) : type_ =
  match e with
  | Fix ->
      let a = fresh_var () in
      Type_arrow (Type_arrow (a, a), a)
  | Variable i ->
      (match List.nth_opt env i with
       | Some scheme -> instantiate scheme
       | None -> failwith "Unbound variable index in typechecker")
  | Lambda body ->
      let arg_t = fresh_var () in
      let body_t = infer (Forall ([], arg_t) :: env) body in
      Type_arrow (arg_t, body_t)
  | Application (Lambda body, e1) ->
      enter_level ();
      let t1 = infer env e1 in
      leave_level ();
      let scheme = generalize t1 in
      infer (scheme :: env) body
  | Application (e1, e2) ->
      let t1 = infer env e1 in
      let t2 = infer env e2 in
      let ret_t = fresh_var () in
      unify t1 (Type_arrow (t2, ret_t));
      ret_t
