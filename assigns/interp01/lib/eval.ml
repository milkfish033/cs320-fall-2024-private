
open Syntax
include Parser

let rec subst x e1 e2 =
  match e2 with 
    | Int _ | Bool _ -> e2
    | UnaryOp (op, e2') -> UnaryOp (op, subst x e1 e2')
    | BinaryOp (op, e21, e22) -> BinaryOp(op, subst x e1 e21, subst x e1 e22)
    | Var y ->
      if x = y then e1 else Var y
    | App (e21, e22) -> App (subst x e1 e21, subst x e1 e22)
    | Fun (y, body) ->
      if x = y then e2 else Fun (y, subst x e1 body)
    | Let (y, e21, e22) -> 
      Let e21' = subst x e1 e21 in 
      Let e22' = if x = y then e22 else subst x e1 e22 in 
      Let (y, e21', e22')
    | Ifte (e21, e22, e23) -> 
      Ifte (subst x e1 e21, subst x e1 e22, subst x e1 e23)


(* Think of the return `v` of `eval m` as `m ⇓ v` *)
let rec eval e =
  match e with 
    | Int i -> Int i
    | Bool b -> Bool b 
    | UnaryOp (op, e') -> (
        match op, eval e' with 
          | Neg, Int i -> Int (-i)
          | Not, Bool b -> Bool (not b)
          | _ -> failwith "type error in unary op"
    )
    | BinaryOp (op, e1, e2) ->(
      let v1 = eval e1 in 
      let v2 = eval e2 in 
        match op, v1, v2 with 
          | Add, Int i1, Int i2 ->  Int(i1+i2)
          | Sub, Int i1, Int i2 ->  Int(i1-i2)
          | Mul, Int i1, Int i2 ->  Int(i1*i2)
          | Div, Int i1, Int i2 ->  Int(i1/i2)
          | Gt, Int i1, Int i2 ->  Int(i1+i2)
          | Lt, Int i1, Int i2 ->  Bool(i1 < i2)
          | Lte, Int i1, Int i2 ->  Bool(i1<=i2)
          | Gte, Int i1, Int i2 ->  Bool (i1>=i2)
          | And, Bool i1, Bool i2 ->  Bool (i1 && i2)
          | Neq, Bool i1, Bool i2 ->  Bool(i1 <>i2)
          | Or, Bool i1, Bool i2 ->  Bool(i1 || i2)
          | _ -> failwith "type error in binary op")
          | Let (x, e1, e2) -> 
             let v1 = eval e1 in 
              eval (subst x v1 e2)
          | Ifte (guard, bThen, bElse) -> 
              match eval guard iwth 
                | Bool true -> eval bThen 
                | Bool false -> eval eElse
                | _ -> failwith ("type error")

    | Var x -> failwith ("undefined variavle" ^ x)
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
