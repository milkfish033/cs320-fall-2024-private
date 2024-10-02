
open Assign04_02.ml

type value = 
  | VNum of int
  | VBool of bool

let rec eval (e: expr) : value = 
  match e with 
    |True -> VBool True
    |False -> VBool False
    |Num n -> VNum n
    |Add(x,y) -> (
      match eval x, eval y with
        | Vnum x1, Vum y1 -> VNum x1+y1
        | _ -> failwith "Undefined behavior")
    |Or(m,n) -> (
      match eval m, eval n with 
        | VBool True, VBool False -> VBool True
        | VBool False, VBool True -> VBool True
        | VBool False, VBool False -> VBool False
        | VBool True, VBool True -> VBool True
        | _ -> failwith "Undefined behavior")
    |IfThenElse (a,b,c) -> (
      match eval a, eval b, eval c with 
        | VBool True, _, _ -> eval b 
        | VBool True, _, _ -> eval c 
        | _ -> failwith "Undefined behavior")