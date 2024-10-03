
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
        | VBool true, _ -> VBool true
        | VBool _, VBool true -> VBool true
        | VBool false, VBool false -> VBool false
        | _ -> failwith "Undefined behavior")
    |IfThenElse (a,b,c) -> (
      match eval a with 
        | VBool true -> eval b 
        | VBool false -> eval c 
        | _ -> failwith "Undefined behavior")