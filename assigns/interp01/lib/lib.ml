
open Utils
open Stdlib320
let parse = My_parser.parse

  let expr_of_val v =
    match v with
      |VFun (x,e) -> Fun(x,e)
      |VUnit -> Unit
      |VNum i -> Num i 
      |VBool true -> True
      |VBool false -> False

  let rec replace_var x y e =
    match e with 
      |Var z -> if z = y then Var x else Var z
      |App (e1,e2) -> App(replace_var x y e1, replace_var x y e2)
      |Fun (z, e) -> Fun(z, replace_var x y e)
      |Num i -> Num i
      |Unit ->  Unit
      |True -> True
      |False -> False
      |Bop (op, e1, e2) -> Bop(op, replace_var x y e1, replace_var x y e2)
      |If (e1, e2, e3) -> If(replace_var x y e1, replace_var x y e2, replace_var x y e3)
      |Let(str, e1, e2) -> Let(str, replace_var x y e1, replace_var x y e2)

 
  let rec subst x e1 e2 = 
  match e2 with  
    |Var y -> if e1 = y then expr_of_val x else Var y
    |Num _ | Unit | True | False -> e2
    |Bop (op, e21, e22) -> Bop(op, subst x e1 e21, subst x e1 e22)
    |App (e21, e22) -> App( subst x e1 e21, subst x e1 e22)
    |If (e21, e22, e23) -> If(subst x e1 e21, subst x e1 e22, subst x e1 e23)
    |Fun (y, body) ->
      if e1 = y then Fun(y,body) 
      else 
        let z = gensym() in
        Fun (z, subst x e1 (replace_var z y body))
    |Let (y, e21, e22) -> 
      let e21' = subst x e1 e21 in 
      let e22' = if e1 = y then e22 else subst x e1 e22 in 
      Let (y, e21', e22')


let rec eval e = 
  match e with 
    | Num i -> Ok (VNum i)
    | True -> Ok (VBool true)
    | False -> Ok (VBool false)
    | Unit -> Ok(VUnit)
    | Var x -> Error(UnknownVar x)
    | If (e, e1, e2) ->
      let v = eval e in (
      let v1 = eval e1 in 
      let v2 = eval e2 in
      match v with 
        |Ok (VBool true) -> v1
        |Ok (VBool false) -> v2
        |_ -> Error (InvalidIfCond))
    |Bop (op, e1, e2) -> 
      let v1 = eval e1 in 
      let v2 = eval e2 in
        (match op, v1, v2 with 
          | Add, Ok(VNum v1), Ok(VNum v2) ->  Ok( VNum (v1 + v2))
          | Sub, Ok(VNum v1), Ok(VNum v2) ->  Ok( VNum (v1 - v2))
          | Mul, Ok(VNum v1), Ok(VNum v2) ->  Ok( VNum (v1 * v2))
          | Div, _, Ok(VNum 0) -> Error(DivByZero)
          | Div, Ok(VNum v1), Ok(VNum v2) ->  Ok( VNum (v1 / v2))
          | Mod, Ok(VNum v1), Ok(VNum v2) ->  Ok( VNum (v1 mod v2))
          | Lt, Ok(VNum v1), Ok(VNum v2) ->  Ok( VBool (v1 < v2))
          | Lte, Ok(VNum v1), Ok(VNum v2) ->  Ok( VBool (v1 <= v2))
          | Gt, Ok(VNum v1), Ok(VNum v2) ->  Ok( VBool (v1 > v2))
          | Gte, Ok(VNum v1), Ok(VNum v2) ->  Ok( VBool (v1 >= v2))
          | Eq, Ok(VFun (x,y)), Ok(VFun (m, n)) -> Ok( VBool((x = m ) &&(y = n )))
          | Eq, v1, v2 -> Ok(VBool(v1=v2))
          | Neq, Ok(VFun (x,y)), Ok(VFun (m, n)) -> Ok( VBool((x <> m ) &&(y <> n )))
          | Neq, v1, v2 ->  Ok( VBool (v1 <> v2))
          | And, Ok(VBool v1), Ok(VBool v2) ->  
              (match v1, v2 with 
                |false, _ -> Ok(VBool false)
                |true, v2 -> Ok(VBool v2)
              )
          | Or, Ok(VBool v1), Ok(VBool v2) ->  
            (match v1, v2 with 
              |true, _ -> Ok(VBool true)
              |false, v2 -> Ok(VBool v2)
            )
          | _ -> Error(InvalidArgs op))
      | Let(str, e1, e2) -> 
          (match eval e1 with 
            |Error(x) -> Error(x)
            |Ok(v) -> eval(subst v str e2) 
          )
      | Fun(str , e) -> Ok(VFun (str, e))
      | App (e1, e2) -> 
        match eval e1 with  
          |Ok(VFun (x, e)) ->
            (match (eval e2) with
              |Error(x) -> Error(x)
              |Ok(v) -> eval(subst v x e))
         |_ -> Error(InvalidApp)
      

let interp str =
  let v = parse str in
    match v with 
      |Some e -> 
        (match eval e with 
          |Ok(v) -> Ok(v)
          |Error(x) -> Error(x)
          )
      |_ -> Error(ParseFail)