
open Assign04_02

type value = 
  | VNum of int
  | VBool of bool

let rec eval (e: expr) : value = 
  match e with 
    |True -> VBool true
    |False -> VBool false
    |Num n -> VNum n
    |Add(x,y) -> (
      match eval x, eval y with
        | VNum x1, VNum y1 -> VNum (x1+y1)
        | _ -> failwith "Undefined behavior")
    |Or(m,n) -> (
      match eval m, eval n with 
        | VBool true, VBool false -> VBool true
        | VBool false, VBool true -> VBool true
        | VBool false, VBool false -> VBool false
        | VBool true, VBool true -> VBool true
        | _ -> failwith "Undefined behavior")
    |IfThenElse (a,b,c) -> (
      match eval a, eval b, eval c with 
        | VBool true, _, _ -> eval b 
        | VBool false, _, _ -> eval c 
        | _ -> failwith "Undefined behavior")