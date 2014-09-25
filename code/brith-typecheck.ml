type _B_ =
  | Int of int
  | Pred of _B_
  | Succ of _B_
  | Plus of _B_ * _B_
  | Mult of _B_ * _B_
  (* new stuff *)
  | Var of string
  | Bool of bool
  | If of _B_ * _B_ * _B_
  | Div of (_B_ * _B_)

type typ =
  | TBool
  | TInt
  | TErr

type value =
  | VInt of int 
  | VBool of bool

type ans =
  | Val of value
  | Err of string

type val_env = (string * value) list

type typ_env = (string * typ) list

(* let rec tinf (e : _B_) : (typ_env * typ) = ... *)

let rec tval (e : _B_) (r : typ_env) : typ =
  match e with
    | Var x -> List.assoc x r
    | Int i -> TInt
    | Bool b -> TBool
    | Pred e ->
        (match tval e r with
           | TErr -> TErr
           | TBool -> TErr
           | TInt -> TInt)
    | Succ e ->
        (match tval e r with
           | TErr -> TErr
           | TBool -> TErr
           | TInt -> TInt)
    | Plus (e1, e2) ->
        (match tval e1 r with
           | TErr -> TErr
           | t1 ->
               (match tval e2 r with
                  | TErr -> TErr
                  | t2 ->
                      (match (t1, t2) with
                         | (TInt, TInt) -> TInt
                         | (TBool, _) -> TErr
                         | (_, TBool) -> TErr)))
    | Mult (e1, e2) ->
        (match tval e1 r with
           | TErr -> TErr
           | t1 ->
               (match tval e2 r with
                  | TErr -> TErr
                  | t2 ->
                      (match (t1, t2) with
                         | (TInt, TInt) -> TInt
                         | (TBool, _) -> TErr
                         | (_, TBool) -> TErr)))    
    | Div (e1, e2) ->
        (match tval e1 r with
           | TErr -> TErr
           | t1 ->
               (match tval e2 r with
                  | TErr -> TErr
                  | t2 ->
                      (match (t1, t2) with
                         | (TInt, TInt) -> TInt
                         | (TBool, _) -> TErr
                         | (_, TBool) -> TErr)))
    | If (e1, e2, e3) ->
        (match tval e1 r with
           | TErr -> TErr
           | TBool -> 
               let t2 = tval e2 r in
               let t3 = tval e3 r in
                 if t2=t3 then t2 else TErr
           | TInt -> TErr)

                  

let rec eval (e : _B_) (r : val_env) : ans =
  match e with
    | Var x -> Val (List.assoc x r)
    | Int i -> Val (VInt i)
    | Bool b -> Val (VBool b)

    | Pred e -> 
        (match eval e r with
          | Err _ as err -> err
          | Val (VBool b) -> Err "expected an int"
          | Val (VInt i) -> Val (VInt (i-1)))
    | Succ e -> 
        (match eval e r with
          | Err _ as err -> err
          | Val (VBool b) -> Err "expected an int"
          | Val (VInt i) -> Val (VInt (i+1)))
    | Plus (e1, e2) ->
        (match eval e1 r with
           | Err _ as err -> err
           | Val v1 ->
               (match eval e2 r with
                  | Err _ as err -> err
                  | Val v2 ->
                      (match (v1, v2) with
                         | (VInt i, VInt j) -> Val (VInt (i+j))
                         | (VBool _, _) -> Err "expected an int"
                         | (_, VBool _) -> Err "expected an int"))) 
    | Mult (e1, e2) ->
        (match eval e1 r with
           | Err _ as err -> err
           | Val v1 ->
               (match eval e2 r with
                  | Err _ as err -> err
                  | Val v2 ->
                      (match (v1, v2) with
                         | (VInt i, VInt j) -> Val (VInt (i*j))
                         | (VBool _, _) -> Err "expected an int"
                         | (_, VBool _) -> Err "expected an int"))) 
    | Div (e1, e2) ->
        (match eval e1 r with
           | Err _ as err -> err
           | Val v1 ->
               (match eval e2 r with
                  | Err _ as err -> err
                  | Val v2 ->
                      (match (v1, v2) with
                         | (VInt i, VInt 0) -> Err "divide by 0"
                         | (VInt i, VInt j) -> Val (VInt (i*j))
                         | (VBool _, _) -> Err "expected an int"
                         | (_, VBool _) -> Err "expected an int")))
    | If (e1, e2, e3) ->
        (match eval e1 r with
           | Err _ as err -> err
           | Val (VBool b) -> if b then eval e2 r else eval e3 r
           | Val (VInt i) -> Err "expected a bool")

        
(* A bogus approach to type checking. *)            
let rec tval' (e : _B_) (r : val_env) : typ =
  match eval e r with
    | Val (VBool b) -> TBool
    | Val (VInt i) -> TInt
    | Err "divide by 0" -> TInt
    | Err _ -> TErr
