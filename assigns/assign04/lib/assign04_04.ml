
type ident = string

type expr' = 
  | True
  | False
  | Num of int
  | Var of ident
  | Let of ident * expr' * expr'
  | Add of expr' * expr'
  | Or of expr' * expr'
  | IfThenElse of expr' * expr' * expr'

type ty' = 
  | Int
  | Bool

type context = (ident * ty') list

let rec lookup (x: ident) (gamma: context) : ty' option =
  match gamma with
  | [] -> None
  | (y, ty)::rest -> if x = y then Some ty else lookup x rest


let rec type_of' (gamma: context) (e: expr') : ty' option =
  match e with
  | True -> Some Bool
  | False -> Some Bool
  | Num _ -> Some Int
  

  | Var x -> lookup x gamma
  

  | Let (x, e1, e2) -> 
      (match type_of' gamma e1 with
      | Some t1 -> 
 
          type_of' ((x, t1) :: gamma) e2
      | None -> None)
  

  | Add (e1, e2) ->
      (match type_of' gamma e1, type_of' gamma e2 with
      | Some Int, Some Int -> Some Int
      | _ -> None)
  

  | Or (e1, e2) ->
      (match type_of' gamma e1, type_of' gamma e2 with
      | Some Bool, Some Bool -> Some Bool
      | _ -> None)
  

  | IfThenElse (e1, e2, e3) ->
      (match type_of' gamma e1, type_of' gamma e2, type_of' gamma e3 with
      | Some Bool, Some t2, Some t3 when t2 = t3 -> Some t2
      | _ -> None)