
open Utils
(include My_parser)

(*val desugar : prog -> expr
    
val type_of : expr -> (ty, error) result

exception AssertFail
exception DivByZero

val eval : expr -> value

val interp : string -> (value, error) result*)


let rec desugar prog =
    match prog with
        | [] -> Unit
        | l :: ls -> (
            match l with
            | TopLet (x, ty, body) ->
                Let (x, ty, body, desugar ls)
            | TopLetRec (f, x, ty_arg, ty_out, body) ->
                LetRec (f, x, ty_arg, ty_out, body, desugar ls)
            )


let rec type_of ctxt =
    let rec go = function
        | Unit -> Ok(UnitTy)
        | True -> Ok(BoolTy)
        | False -> Ok(BoolTy)
        | Var x -> Error(UnknownVar x)
        | Num _ -> Ok(IntTy)
        | Fun (x, ty, e) -> (
            match type_of ((x, ty) :: ctxt) e with
            | Some ty_out -> Some (FunTy (ty, ty_out))
            | _ -> None
            )
            | Add (e1, e2) -> go_op IntTy e1 e2
            | Sub (e1, e2) -> go_op IntTy e1 e2
            | Mul (e1, e2) -> go_op IntTy e1 e2
            | Eq (e1, e2) -> go_op BoolTy e1 e2
            | If (e1, e2, e3) -> (
                match go e1, go e2, go e3 with
                    | Some BoolTy, Some t2, Some t3 when t2 = t3 -> Some t3
                    | _ -> None
                    )
        | App (e1, e2) -> (
        match go e1, go e2 with
        | Some (FunTy (ty_arg, ty_out)), Some t2 when ty_arg = t2 -> Some ty_out
        | _ -> None
        )
        | Let (x, ty, e1, e2) -> (
        match go e1 with
        | Some t1 when ty = t1 -> type_of ((x, ty) :: ctxt) e2
        | _ -> None
        )
        | LetRec (f, x, ty_arg, ty_out, e1, e2) -> (
        match type_of ((x, ty_arg) :: (f, FunTy(ty_arg, ty_out)) :: ctxt) e1 with
        | Some ty when ty = ty_out -> type_of ((f, FunTy(ty_arg, ty_out)) :: ctxt) e2
        | _ -> None
        )
    and go_op ty_out e1 e2 =
        match go e1, go e2 with
        | Some IntTy, Some IntTy -> Some ty_out
        | _ -> None
    in go

let type_of = type_of []