
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
            


(*expr -> (ty, error) result*)
let rec type_of ctxt =
    let rec go = function
        | Unit -> Ok(UnitTy)
        | True -> Ok(BoolTy)
        | False -> Ok(BoolTy)
        | Num _ -> Ok(IntTy)
        | Var x -> Error(UnknownVar x)
        | If (e1, e2, e3) -> (
            match go e1, go e2, go e3 with
            | Ok(BoolTy), Ok(t2), Ok(t3)  -> (
                if t2 = t3 then Ok(t2)
                else Error(IfTyErr(t2,t3))
                )
            | Ok(t), _,_ -> Error(IfCondTyErr(t))
            | _ -> Error(ParseErr)
            )
        |Assert e -> 
            (match type_of e with 
                | Ok(BoolTy) -> Ok(BoolTy)
                | Ok(t)-> Error(AssertTyErr(t))
                | _ -> Error(ParseErr) 
            )
        | Bop (op, x,y) -> (
            match op, x, y with 
                | Add, e1, e2 -> go_op Add IntTy e1 e2
                | Sub, e1, e2 -> go_op Sub IntTy e1 e2
                | Mul, e1, e2 -> go_op Mul IntTy e1 e2 
                | Div, e1, e2 -> go_op Div IntTy e1 e2
                | Mod, e1, e2 -> go_op Mod IntTy e1 e2
                | And, e1, e2 -> go_op And BoolTy e1 e2 
                | Or, e1, e2 -> go_op Or BoolTy e1 e2 
                | Lt, e1, e2 -> go_op_1 Lt BoolTy e1 e2 
                | Lte, e1, e2 -> go_op_1 Lte BoolTy e1 e2 
                | Gt, e1, e2 -> go_op_1 Gt BoolTy e1 e2 
                | Gte, e1, e2 -> go_op_1 Gte BoolTy e1 e2 
                | Eq, e1, e2 -> go_op_1 Eq BoolTy e1 e2 
                | Neq, e1, e2 -> go_op_1 Neq BoolTy e1 e2 
                )
        | Fun (_, ty, e) -> (
            match go e with 
                |Ok(t) -> if t = ty then Ok(FunTy(ty,t))
                            else Error(FunArgTyErr(ty, t))
                |_ -> Error(ParseErr)
            )
        | App (e1, e2) -> (
            match go e1, go e2 with
                | (Ok(FunTy (ty_arg, ty_out))), t2 when Ok(ty_arg) = t2 -> Ok(ty_out)
                | Ok(t),_ -> Error(FunAppTyErr(t))
                | _ -> Error(ParseErr)
            )
        | Let (x, ty, e1, e2) -> (
            match go e1 with
            | Ok(t1) when ty = t1 -> type_of ((x, ty)) e2
            | _ -> None
        )
    and go_op op ty_out e1 e2 =
        match go e1, go e2 with
        | Ok t1, Ok t2 when t1 = ty_out && t2 = ty_out -> Ok(ty_out)
        | Ok t1, Ok t2 when t2 = ty_out -> Error (OpTyErrL (op, ty_out, t1))
        | Ok t1, Ok t2 when t1 = ty_out -> Error (OpTyErrR (op, ty_out, t2))
        | _, _ -> Error(ParseErr)
    
    and go_op_1 op ty_out e1 e2 =
        match go e1, go e2 with
        | Ok t1, Ok t2 when t1 = IntTy && t2 = IntTy -> Ok(ty_out)
        | Ok t1, Ok t2 when t2 = IntTy -> Error (OpTyErrL (op, ty_out, t1))
        | Ok t1, Ok t2 when t1 = IntTy -> Error (OpTyErrR (op, ty_out, t2))
        | _, _ -> Error(ParseErr)

    in go 

    let type_of = type_of []


 