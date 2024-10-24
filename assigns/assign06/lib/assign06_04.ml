
open Utils

type value = 
  | VNum of int
  | VBool of bool

let rec eval = function
  | Num n -> VNum n  (* Directly return the number wrapped in VNum *)
  | Add (e1, e2) -> 
      (match eval e1, eval e2 with
      | VNum n1, VNum n2 -> VNum (n1 + n2)  (* Sum the evaluated numbers *)
      | _ -> failwith "Type error in Add")  (* Should not happen if well-typed *)
  | Lt (e1, e2) -> 
      (match eval e1, eval e2 with
      | VNum n1, VNum n2 -> VBool (n1 < n2)  (* Compare the numbers *)
      | _ -> failwith "Type error in Lt")  (* Should not happen if well-typed *)
  | Ite (e1, e2, e3) -> 
      (match eval e1 with
      | VBool true -> eval e2  (* If condition is true, evaluate e2 *)
      | VBool false -> eval e3  (* If condition is false, evaluate e3 *)
      | _ -> failwith "Type error in Ite")  (* Should not happen if well-typed *)
