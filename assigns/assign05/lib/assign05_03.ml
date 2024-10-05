
type ident = string

type ty = 
| Unit
| Arr of ty * ty

type expr = 
| Var of ident
| Fun of ident * ty * expr
| App of expr * expr

type ctxt = (ident * ty) list

let rec type_of (gamma: ctxt) (e: expr) : ty option =
  match e with
  | Var x -> 
      List.assoc_opt x gamma
    
  | Fun (x, t1, body) -> 
      (match type_of ((x, t1) :: gamma) body with
      | Some t2 -> Some (Arr (t1, t2))
      | None -> None)

  | App (e1, e2) -> 
      match type_of gamma e1, type_of gamma e2 with
      | Some (Arr (t1, t2)), Some t2' when t1 = t2' -> Some t2
      | _ -> None
  