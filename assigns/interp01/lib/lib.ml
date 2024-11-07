
open Utils
open Stdlib320
let parse = My_parser.parse

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
      | App (e1, e2) -> assert false
      | Let(str, e1, e2) -> assert false
      | Fun(str , e) -> assert false



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
