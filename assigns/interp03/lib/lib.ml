
open Utils
include My_parser



let ty_subst t x =
  let rec go = function
  |TUnit -> TUnit
  |TInt -> TInt
  |TFloat -> TFloat
  |TBool -> TBool 
  |TVar y-> if x = y then t else TVar y 
  |TList t1 -> TList (go t1)
  |TOption t1 -> TOption (go t1)
  |TPair (t1, t2) -> TPair(go t1, go t2)
  |TFun (t1, t2) -> TFun(go t1, go t2)
in go

let ty_subst_c t x (t1, t2) = (ty_subst t x t1, ty_subst t x t2)
let ty_subst_cs t x = List.map (ty_subst_c t x)

let rec fvs = function
  | TUnit -> VarSet.empty
  | TInt -> VarSet.empty
  | TFloat -> VarSet.empty
  | TBool -> VarSet.empty
  | TVar x -> VarSet.of_list [x]
  | TList t -> fvs t 
  | TOption t -> fvs t
  | TPair (t1, t2) -> VarSet.union (fvs t1) (fvs t2)
  | TFun (t1, t2) -> VarSet.union (fvs t1) (fvs t2)
  


(*read constraints one by one and eliminate them till you run out or error*)
let unify (ty : Utils.ty) (cs : Utils.constr list) : Utils.ty_scheme option = 
  let rec go cs ty = 
    match cs with 
    |[] -> Some (Forall([], ty))
    |[TVar "_out", ty] -> Some (Forall([], ty))
    |(t1, t2) :: cs ->
      if t1 = t2 then go cs ty
        else match (t1, t2) with
        |TFun(s1, t1), TFun(s2, t2) ->
          go ((s1, s2) :: (t1, t2) :: cs) ty 

        |TVar x, t when not (VarSet.mem x (fvs t)) ->
          go (ty_subst_cs t x cs) ty 

        |t, TVar x ->
          go ((TVar x, t) :: cs) ty 

        |TList t1, TList t2 ->
          go ((t1, t2) :: cs) ty 

        |TOption t1, TOption t2 ->
          go ((t1, t2) :: cs) ty 

        |TPair(s1, t1), TPair(s2, t2) ->
          go ((s1, s2) :: (t1, t2) :: cs) ty 

        |_ -> None 
  in 
  match go cs ty with
  |Some(Forall(_, ty')) -> let vars = VarSet.to_list (fvs ty') in
    Some(Forall(vars, ty'))
  |_ -> None


(*Type of Helper functions*)





let rec instantiate scheme =
  match scheme with
  |Forall(vars, ty) ->
    (match vars with
    |[] -> ty
    |x :: xs ->  
      let fresh = TVar (gensym ()) in
      let new_scheme = Forall(xs, ty) in
      ty_subst fresh x (instantiate new_scheme))
    

  let int_bop_helper t1 c1 t2 c2 =
    (TInt, 
    (t1, TInt) :: (t2, TInt) :: c1 @ c2
    ) 
  
  let float_bop_helper t1 c1 t2 c2 =
    (TFloat, 
    (t1, TFloat) :: (t2, TFloat) :: c1 @ c2
    ) 

    
  let equalities_bop_helper t1 c1 t2 c2 =
    (TBool, 
    (t1, t2) :: c1 @ c2
    ) 

  let and_or_bop_helper t1 c1 t2 c2 =
    (TBool, 
    (t1, TBool) :: (t2, TBool) :: c1 @ c2
    ) 

let type_of' ctxt expr =
  let rec go ctxt = function 
    |Unit -> TUnit, [] (*Base cases*)
    |True -> TBool, []
    |False -> TBool, []
    |Nil -> 
      let fresh = TVar(gensym ()) in
      fresh, [] (*I thought it expected a type option but I guess it doesn't, may have to come back later to fix*)
    |ENone -> 
      let fresh = TVar(gensym ()) in
      TOption fresh, []
    |Int _ -> TInt, []
    |Float _ -> TFloat, []
    |Var x -> 
     (match Env.find_opt x ctxt with
      |Some t -> (instantiate t), []
      |None -> assert false)
    |Assert e -> 
      let t, c = go ctxt e in
      TUnit, (t, TBool) :: c
    |ESome e -> 
      let t1, c1 = go ctxt e in
      TOption t1, c1
    |Bop (b, e1, e2) -> 
      let t1, c1 = go ctxt e1 in
      let t2, c2 = go ctxt e2 in
      (match b with
      |Add -> int_bop_helper t1 c1 t2 c2
      |Sub -> int_bop_helper t1 c1 t2 c2
      |Mul -> int_bop_helper t1 c1 t2 c2
      |Div -> int_bop_helper t1 c1 t2 c2
      |AddF -> float_bop_helper t1 c1 t2 c2
      |SubF -> float_bop_helper t1 c1 t2 c2
      |MulF -> float_bop_helper t1 c1 t2 c2
      |DivF -> float_bop_helper t1 c1 t2 c2
      |PowF -> float_bop_helper t1 c1 t2 c2
      |Cons -> 
        (TList t1, 
        (t1, TList t1) :: (t2, TList t2) :: c1 @ c2
        )
      |Concat ->
        let fresh = TVar(gensym ()) in
        (TList fresh,
        (t1, TList fresh) :: (t2, TList fresh) :: c1 @ c2
        )
      |Lt -> equalities_bop_helper t1 c1 t2 c2
      |Lte -> equalities_bop_helper t1 c1 t2 c2
      |Gt -> equalities_bop_helper t1 c1 t2 c2
      |Gte -> equalities_bop_helper t1 c1 t2 c2
      |Eq -> equalities_bop_helper t1 c1 t2 c2
      |Neq -> equalities_bop_helper t1 c1 t2 c2
      |And -> and_or_bop_helper t1 c1 t2 c2
      |Or -> and_or_bop_helper t1 c1 t2 c2
      |Comma -> 
        let t1, c1 = go ctxt e1 in
        let t2, c2 = go ctxt e2 in
        (TPair (t1, t2),
        c1 @ c2
        ) 
      |_ -> assert false)
    |If (e1, e2, e3) ->
      let t1, c1 = go ctxt e1 in
      let t2, c2 = go ctxt e2 in
      let t3, c3 = go ctxt e3 in
      (t3, 
      (t1, TBool) :: (t2, t3) :: c1 @ c2 @ c3
      )
    |ListMatch {matched; hd_name; tl_name; cons_case; nil_case} ->
      let t, c = go ctxt matched in
      let fresh = TVar(gensym()) in
      let new_ctxt = Env.add tl_name (Forall([], TList fresh)) (Env.add hd_name (Forall([], fresh)) ctxt) in(*For got to put them in foralls beofre adding them to context :()*)
      let t1, c1 = go new_ctxt cons_case in
      let t2, c2 = go ctxt nil_case in
      (t2,
      (t, TList fresh) :: (t1, t2) :: c @ c1 @ c2
      )
    |OptMatch {matched; some_name; some_case; none_case} ->
      let t, c = go ctxt matched in
      let fresh = TVar(gensym()) in
      let new_ctxt = Env.add some_name (Forall([], fresh)) ctxt in
      let t1, c1 = go new_ctxt some_case in
      let t2, c2 = go ctxt none_case in
      (t2,
      (t, TOption fresh) :: (t1, t2) :: c @ c1 @ c2
      )
    |PairMatch {matched; fst_name; snd_name; case} ->
      let t, c = go ctxt matched in
      let a' = TVar(gensym()) in
      let b' = TVar(gensym()) in
      let new_ctxt = Env.add fst_name (Forall([], a')) (Env.add snd_name (Forall([], b'))ctxt) in
      let t', c' = go new_ctxt case in
      (t',
      (t, TPair (a',b')) :: c @ c' 
      )
    |Fun(id, tp, e) ->
      let arg = 
        (match tp with
        |None -> TVar(gensym())
        |Some t -> t)
      in 
      let new_ctxt = Env.add id (Forall([], arg)) ctxt in
      let t', c = go new_ctxt e in
      (TFun(arg, t'),
      c
      )
    |App(e1, e2) ->
      let t1, c1 = go ctxt e1 in
      let t2, c2 = go ctxt e2 in
      let a' = TVar(gensym()) in 
      (a', 
      (t1, TFun(t2, a')) :: c1 @ c2        
      )
    |Annot(e1, tp) -> 
      let t', c = go ctxt e1 in
      (tp,
      (tp, t') :: c
      )
    |Let {is_rec; name; value; body} ->
      if is_rec = true 
        then 
          let a' = TVar(gensym()) in
          let b' = TVar(gensym()) in
          let new_ctxt = Env.add name (Forall([], TFun(a', b'))) ctxt in
          let t1, c1 = go new_ctxt value in
          let new_ctxt' = Env.add name (Forall([], t1)) ctxt in
          let t2, c2 = go new_ctxt' body in
          (t2,
          (t1, TFun(a', b')) :: c1 @ c2
          )
        else
          let t1, c1 = go ctxt value in
          let new_ctxt = Env.add name (Forall([], t1)) ctxt in
          let t2, c2 = go new_ctxt body in
          (t2,
          c1 @ c2
          ) 
    in
    go ctxt expr



let type_of (stc_env : Utils.stc_env) (expr : Utils.expr) : ty_scheme option =
  let t, c = type_of' stc_env expr in
  unify t c


exception AssertFail
exception DivByZero
exception RecWithoutArg
exception CompareFunVals

let bop_int_helper bop i1 i2 =
  match bop with
  |Add -> VInt (i1 + i2)
  |Sub -> VInt (i1 - i2)
  |Mul -> VInt (i1 * i2)
  |Div -> if i2 <> 0 then VInt (i1 / i2) else raise DivByZero
  |Mod -> if i2 <> 0 then VInt (i1 mod i2) else raise DivByZero
  |Lt -> VBool (i1 < i2)
  |Lte -> VBool (i1 <= i2)
  |Gt -> VBool (i1 > i2)
  |Gte -> VBool (i1 >= i2)
  |Eq -> VBool (i1 = i2)
  |Neq -> VBool (i1 <> i2)
  |_ -> assert false
  
let bop_float_helper bop f1 f2 =
  match bop with
  |AddF -> VFloat (f1 +. f2)
  |SubF -> VFloat (f1 -. f2)
  |MulF -> VFloat (f1 *. f2)
  |DivF -> if f2 <> 0. then VFloat (f1 /. f2) else raise DivByZero
  |Lt -> VBool (f1 < f2)
  |Lte -> VBool (f1 <= f2)
  |Gt -> VBool (f1 > f2)
  |Gte -> VBool (f1 >= f2)
  |Eq -> VBool (f1 = f2)
  |Neq -> VBool (f1 <> f2)
  |_ -> assert false

let bop_and_or_helper bop b1 b2 =
  match bop with
  |And -> VBool (b1 && b2)
  |Or -> VBool (b1 || b2) 
  |_ -> assert false


let eval_expr (dyn_env : Utils.dyn_env) ( expr : Utils.expr) : value =
  let rec go dyn_env expr =
    match expr with
    |Unit -> VUnit
    |True -> VBool true
    |False -> VBool false
    |Nil -> VList []
    |ENone -> VNone
    |Int n -> VInt n
    |Float f -> VFloat f
    |Var s ->  
      (match Env.find_opt s dyn_env with (*Check the environment for the variable*)
      |Some s -> s
      |None -> assert false)
    |Assert e ->
      (match e with
      |True -> VUnit
      |_ -> assert false)
    |ESome e -> 
      let value = go dyn_env e in
      VSome value
    |Bop (b, e1, e2) ->
      let v1 = go dyn_env e1 in
      let v2 = go dyn_env e2 in
      (match v1, v2 with
      |VInt i1, VInt i2 -> bop_int_helper b i1 i2
      |VFloat f1, VFloat f2 -> bop_float_helper b f1 f2
      |VBool b1, VBool b2 -> bop_and_or_helper b b1 b2
      | v, VList l2 when b = Cons -> VList (v :: l2)
      | VList l1, VList l2 when b = Concat -> VList (l1 @ l2)
      | p1, p2 when b = Comma -> VPair(p1, p2)
      |_ -> assert false)
    |If (e1, e2, e3) ->
      (match go dyn_env e1 with
      |VBool true -> go dyn_env e2
      |VBool false -> go dyn_env e3
      |_ -> assert false ) 
    |ListMatch {matched; hd_name; tl_name; cons_case; nil_case} ->
     ( match go dyn_env matched with
      |VList (vh :: vt) -> 
        let new_env = Env.add hd_name vh dyn_env in
        let new_env' = Env.add tl_name (VList vt) new_env in
        go new_env' cons_case
      |VList [] -> 
        go dyn_env nil_case
      |_ -> failwith "No list fopr List match :( ")
    |OptMatch {matched; some_name; some_case; none_case} ->
      (match go dyn_env matched with
      |VSome v ->
        let new_env = Env.add some_name v dyn_env in
        go new_env some_case 
      |VNone -> 
        go dyn_env none_case
      | _ -> failwith "Invalid arg for Opt_match")
    |PairMatch {matched; fst_name; snd_name; case} ->
      (match go dyn_env matched with
      |VPair(v1, v2) ->
        let new_env = Env.add fst_name v1 dyn_env in
        let new_env' = Env.add snd_name v2 new_env in
        go new_env' case
      |_ -> failwith "Invalid args for pair")
    |Fun(x, _, e) -> VClos {name = None; arg = x; body = e; env = dyn_env }
    |App(e1, e2) -> 
      (match go dyn_env e1 with
      |VClos {name = Some n; arg; body; env = fun_env} ->
        let v = go dyn_env e2 in
        let new_env = Env.add n (VClos {name = Some n; arg; body; env = dyn_env}) fun_env in
        go (Env.add arg v new_env) body
      |VClos {name = None; arg; body; env = fun_env} ->
        let v = go dyn_env e1 in
        go (Env.add arg v fun_env) body
      |_ -> failwith "not a function for app")
    |Annot (e, t) ->
      (match type_of' Env.empty e with (*Find a way to pass the bound variables from dyn_env to typeof'*)
      |(tp, _) -> 
        if tp = t then go dyn_env e
        else failwith "wrong type in annot"
      )
    |Let {is_rec = true; name = x; value = e1; body = e2} ->
      (match go dyn_env e1 with
      |VClos {name = _; arg; body; env = closure_env} -> (*VClos allows the recursive call to carry its environment with it to be called on later*)
        let new_env = Env.add x (VClos{name = Some x; arg; body; env = closure_env}) dyn_env in (*Store VClos in the environment for the recursive call to be able to reference itself*)
        go new_env e2
      |_ -> assert false)
    |Let {is_rec = false; name = x; value = e1; body = e2} ->
      let v = go dyn_env e1 in (*This is way more straight forward than the recursive call, just evaluate e1 then*)
      go (Env.add x v dyn_env) e2 (*Add it to the environment with X before addressing e2*)
  in
  go dyn_env expr

let type_check =
  let rec go ctxt = function
  | [] -> Some (Forall ([], TUnit))
  | {is_rec;name;value} :: ls ->
    match type_of ctxt (Let {is_rec;name;value; body = Var name}) with
    | Some ty -> (
      match ls with
      | [] -> Some ty
      | _ ->
        let ctxt = Env.add name ty ctxt in
        go ctxt ls
    )
    | None -> None
  in go Env.empty

let eval p =
  let rec nest = function
    | [] -> Unit
    | [{is_rec;name;value}] -> Let {is_rec;name;value;body = Var name}
    | {is_rec;name;value} :: ls -> Let {is_rec;name;value;body = nest ls}
  in eval_expr Env.empty (nest p)

let interp input =
  match parse input with
  | Some prog -> (
    match type_check prog with
    | Some ty -> Ok (eval prog, ty)
    | None -> Error TypeError
  )
  | None -> Error ParseError
