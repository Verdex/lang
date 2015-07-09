
type 'a option = Some of 'a
               | None

type var = Free | Bound

type t = TConst of string
       | TArrow of t * t

type expr = EVar of string
          | EAbs of string * t * expr
          | EApp of expr * expr

type ctx = (string * t * var) list 

let my_find (func : string -> bool) (ctx : ctx) : t option =
    let projT (s, t, v) = t in
    try Some (projT (List.find (fun (s, t, v) -> func s) ctx))
    with _ -> None

let rec typeof (ctx : ctx) (expr : expr) : t option =
    match expr with
    EVar name -> my_find (fun n -> n = name) ctx
    | EAbs (param, p_type, body) -> 
        let b_type = typeof ((param,p_type,Bound) :: ctx) body in
        (match b_type with
         None -> None
         | Some t -> Some (TArrow(p_type, t)))

(*
if e1 contains type variables and its bound then we can handle it the
same way as if it contained const types

alternatively if e1 contains type varaibles and its free then we need to
basically do a beta reduction on the types
*)



    | EApp (e1, e2) ->
        let ot1 = typeof ctx e1 in
        let ot2 = typeof ctx e2 in 
        (match (ot1, ot2) with
         (Some t1, Some t2) -> (match (t1, t2) with
                                (TArrow (a, b), _) when a = t2 -> Some b
                                | _ -> None)
         | _ -> None)
