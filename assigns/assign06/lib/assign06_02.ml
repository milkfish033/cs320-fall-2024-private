
open Utils

type expr = 
  | Num of int
  | Add of expr * expr
  | Lt of expr * expr
  | Ite of expr * expr * expr

let parse toks =
  let rec aux stack = function
    | [] -> 
        (match stack with
        | [e] -> Some e  (* Exactly one final expression should remain *)
        | _ -> None)     (* Otherwise, the expression is malformed *)
    | TNum n :: rest -> aux (Num n :: stack) rest  (* Push `Num n` onto the stack *)
    | TAdd :: rest -> 
        (match stack with
        | e2 :: e1 :: st -> aux (Add (e1, e2) :: st) rest  (* Apply addition *)
        | _ -> None)
    | TLt :: rest -> 
        (match stack with
        | e2 :: e1 :: st -> aux (Lt (e1, e2) :: st) rest  (* Apply less-than *)
        | _ -> None)
    | TIte :: rest -> 
        (match stack with
        | e3 :: e2 :: e1 :: st -> aux (Ite (e1, e2, e3) :: st) rest  (* Apply if-then-else *)
        | _ -> None)
  in
  aux [] toks
