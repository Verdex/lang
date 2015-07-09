
(* 

any arrow with type variables in it that gets applied to a variable needs
a type level beta reduction thingy to occur on it

type variables that come from bound  variables can remain like they are because we need
to match everything in the type sig up with the final type we arrive at (and the 
sig has those same type variables in it)

type variables that come from free variables need the alpha conversion treatment
done on them (every example i've seen does the debruijn index thing so they dont
have to worry about alpha conversion, but i really dont get why that works with 
free varaibles) ... anyway, to avoid the debruijn thing I can probably just
make a gensym and then apply that to any free variable's type variables

an internal let seems odd b/c if it doesn't capture any lexical values
it seems like it should be treated as free, but if it does capture
lexical values then those values should be treated as bound ...

I guess the same question comes up with a lambda ... when does the type variables
in a locally defined lambda be considered the same as the type varaibles
that show up in the function's sig and when should they be just considered independent?

this is an interesting question b/c let x (y : a) (z : a) = ... might be different
than let x (y : a) = \ z : a -> ... depending on how i answer

let x (blah : a -> b -> a) : ? = 
    blah id

    This instance the type of (blah id) has a mix of type varaibles where some of them
    need to be replaced and some of them do not

let x (blah : a -> b -> a -> b) : ? =
    blah id 5

    if you're going to feed some result of blah into a function that takes
    an int then you want to replace 'b' but if you have some other parameter
    'b' that you take into 'x' and maybe 'x' returns a 'b' then you don't 
    want to replace 'b'

*)


type 'a option = Some of 'a
               | None

type var = Free | Bound 

type t = TVar   of string 
       | TConst of string
       | TArrow of t * t

type expr = EVar of string
          | EAbs of string * t * expr
          | EApp of expr * expr

type ctx = (string * t * var) list 

let my_find (func : string -> bool) (ctx : ctx) : t option =
    let projT (s, t, v) = t in
    try Some (projT (List.find (fun (s, t, v) -> func s) ctx))
    with _ -> None

let rec t_beta (target : t) (varToReplace : string) (replaceWith : t) : t =
    match target with
        TVar n when n = varToReplace -> replaceWith
        | TVar n -> TVar n
        | TConst n -> TConst n
        | TArrow( t1,  t2 ) -> TArrow (t_beta t1 varToReplace replaceWith, t_beta t2 varToReplace replaceWith)

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
