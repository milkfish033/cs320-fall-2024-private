
open Utils

include My_parser
(*val desugar : prog -> expr
    
val type_of : expr -> (ty, error) result

exception AssertFail
exception DivByZero

val eval : expr -> value

val interp : string -> (value, error) result*) 


 let rec desugar_value s = 
    match s with 
        |SUnit -> Unit
        |STrue -> True
        |SFalse -> False
        |SNum x -> Num x
        |SVar x -> Var x
        |SIf (a, b, c) -> If (desugar_value a, desugar_value b, desugar_value c)
        |SBop (op, left, right) -> Bop (op, desugar_value left, desugar_value right)
        |SAssert expr -> Assert (desugar_value expr)
        | SFun { arg = (arg_name, arg_ty); args; body } ->
            List.fold_right
              (fun (arg_name, arg_ty) acc -> Fun (arg_name, arg_ty, acc))
              ((arg_name, arg_ty) :: args)
              (desugar_value body)
        | SApp (fn, arg) -> App (desugar_value fn, desugar_value arg)
        | SLet { is_rec; name; args; ty; value; body } ->
            let desugared_value =
              List.fold_right
                (fun (arg_name, arg_ty) acc -> Fun (arg_name, arg_ty, acc))
                args
                (desugar_value value)
            in
            Let
              { is_rec = is_rec
              ; name = name
              ; ty = ty
              ; value = desugared_value
              ; body = desugar_value body
              }


let rec desugar prog =
    match prog with
        | [] -> Unit
        | ({is_rec = b; name = x; args = a; ty= t; value = e} ):: ls -> 
            Let {is_rec = b; name= x; ty = t; value = desugar_value e ;body = desugar ls}
            


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