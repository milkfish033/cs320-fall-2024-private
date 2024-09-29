
type expr = 
| True
| False
| Num of int
| Or of expr * expr
| Add of expr * expr
| IfThenElse of expr * expr * expr

type ty = 
  | Int
  | Bool

  let rec type_of (e: expr) : ty option =
    match e with
    (* Base cases *)
    | True -> Some Bool
    | False -> Some Bool
    | Num _ -> Some Int
    | Add(x,y) -> (
      match type_of x, type_of y with 
        |Some Int, Some Int -> Some Int
        |_ -> None)
    | Or(m,n) ->(
      match type_of m, type_of n with
        |Some Bool, Some Bool -> Some Bool
        | _ -> None
    )
    |IfThenElse (a,b,c) -> (
      match type_of a, type_of b, type_of c with
        |Some Bool, Some Int, Some Int -> Some Int
        |Some Bool, Some Bool, Some Bool -> Some Bool
        |_ -> None
    )