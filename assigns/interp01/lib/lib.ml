
open Utils
open Stdlib320
let parse = My_parser.parse




  
  let rec subst x e1 e2 = 
  match e2 with 
    |Var y -> if x = y then x else Var y
    |Num _ | Unit | True | False -> e2
    |Bop (op, e21, e22) -> Bop(op, subst x e1 e21, subst x e1 e22)
    |App (e21, e22) -> App( subst x e1 e21, subst x e1 e22)
    |If (e21, e22, e23) -> If(subst x e1 e21, subst x e1 e22, subst x e1 e23)
    |Fun (y, body) ->
      if x = y then e2 else Fun (y, subst x e1 body)
    |Let (y, e21, e22) -> 
      let e21' = subst x e1 e21 in 
      let e22' = if x = y then e22 else subst x e1 e22 in 
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
          | Eq, v1, v2 -> Ok(VBool(v1=v2))
          | Neq, v1, v2 ->  Ok( VBool (v1 <> v2))
          | And, Ok(VBool v1), Ok(VBool v2) ->  Ok( VBool (v1 && v2))
          | Or, Ok(VBool v1), Ok(VBool v2) ->  Ok( VBool (v1 || v2))
          | _ -> Error(InvalidArgs op))
      | Let(str, e1, e2) -> assert false   
      | Fun(str , e) -> Ok(VFun (str, e))
      | App (e1, e2) -> (
        let v1 = eval e1 in 
        let v2 = eval e2 in 
        match v1 with 
         |Ok(VFun (x, body)) ->
            eval (subst x v2 body)
         |_ -> Error(InvalidApp)
      )


(*
    | Fun (x, body) -> Fun(x, body)
    | App (e1, e2) -> (
      let v1 = eval e1 in 
      let v2 = eval e2 in 
      match v1 with 
        |Fun (x, body) -> eval (subst x v2 body)
      )
*)


   (* | Let (x, e1, e2) -> 
        let v1 = eval e1 in 
        eval (subst x v1 e2)
    | Ifte (guard, bThen, bElse) -> 
        (match eval guard with 
          | Bool true -> eval bThen 
          | Bool false -> eval eElse
          | _ -> failwith ("type error"))
    | Fun (x, body) -> Fun(x, body)
    | App (e1, e2) -> (
      let v1 = eval e1 in 
      let v2 = eval e2 in 
      match v1 with 
        |Fun (x, body) -> eval (subst x v2 body)
        | _ -> failwith ("type error in function application")
      )
    | _ -> failwith "unimplemented"


let interp fname =
  let c = open_in fname in
  let m = Parser.parse c in
  let v = eval m in
  v
  
*)
