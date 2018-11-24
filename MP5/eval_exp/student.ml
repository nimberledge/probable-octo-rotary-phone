open Common;;

let const_to_val c =
    match c with
      BoolConst b -> BoolVal b
    | IntConst n -> IntVal n
    | FloatConst d -> FloatVal d
    | StringConst s -> StringVal s
    | NilConst -> ListVal ([])
    | UnitConst -> UnitVal

let monOpApply op v =
    match op with
    | HdOp -> (match v with ListVal k -> (match k with
                                            | h :: t -> h
                                            | [] -> raise (Failure "hd")))
    | TlOp -> (match v with ListVal k -> (match k with
                                            | h :: t -> ListVal (t)
                                            | [] -> raise (Failure "tl")))
    | PrintOp -> (match v with
        StringVal s -> print_string s; UnitVal)
    | IntNegOp -> ( match v with
              IntVal n -> IntVal (- n))
    | FstOp -> ( match v with
          PairVal (a,b) -> a)
    | SndOp -> ( match v with
          PairVal (a,b) -> b )

let binOpApply binop (v1,v2) =
  match binop with
    IntPlusOp -> (match (v1, v2) with
      (IntVal a, IntVal b) -> IntVal (a + b))
  | IntMinusOp -> (match (v1, v2) with
    (IntVal a, IntVal b) -> IntVal (a - b))
  | IntTimesOp -> (match (v1, v2) with
    (IntVal a, IntVal b) -> IntVal (a * b))
  | IntDivOp -> (match (v1, v2) with
    (IntVal a, IntVal b) -> IntVal (a / b))
  | FloatPlusOp -> (match (v1, v2) with
    (FloatVal a, FloatVal b) -> FloatVal (a +. b))
  | FloatMinusOp -> (match (v1, v2) with
    (FloatVal a, FloatVal b) -> FloatVal (a -. b))
  | FloatTimesOp -> (match (v1, v2) with
    (FloatVal a, FloatVal b) -> FloatVal (a *. b))
  | FloatDivOp -> (match (v1, v2) with
    (FloatVal a, FloatVal b) -> FloatVal (a /. b))
  | ConcatOp -> (match (v1, v2) with
    (StringVal a, StringVal b) -> StringVal (a ^ b))
  | ConsOp -> (match v2 with
    ListVal b -> ListVal (v1 :: b) )
  | CommaOp -> PairVal (v1, v2)
  | EqOp -> BoolVal (v1 = v2)
  | GreaterOp -> BoolVal (v1 > v2)
  | ModOp -> (match (v1, v2) with
    (IntVal a, IntVal b) -> IntVal (a mod b))
  | ExpoOp -> (match (v1, v2) with
    (FloatVal a, FloatVal b) -> FloatVal (a ** b))

let rec eval_exp (exp, m) =
  match exp with
    ConstExp c -> const_to_val c
  | VarExp v -> let k = lookup_env m v in
          (match k with
            None -> UnitVal
          | Some c -> (match c with RecVarVal (g, y, e, m') -> Closure (y, e, (ins_env m' g c))
                                  | _ -> c ))
  | MonOpAppExp (mon, e) -> let v = eval_exp (e, m) in
          monOpApply mon v
  | BinOpAppExp (bin, e1, e2) -> let v1 = eval_exp (e1, m) in
                                let v2 = eval_exp (e2, m) in
                                binOpApply bin (v1, v2)
  | IfExp (e1, e2, e3) -> let b = eval_exp (e1, m) in
                      (match b with
                        BoolVal b1 -> if b1 then eval_exp(e2, m) else eval_exp (e3, m))
  | LetInExp (x, e1, e2) -> let v1 = eval_exp (e1, m) in
                    let new_m = ins_env m x v1 in
                    eval_exp (e2, new_m)
  | FunExp (x, e) -> Closure (x, e, m)
  | AppExp (e1, e2) -> let k1 = eval_exp (e1, m) in
                    let v' = eval_exp(e2, m) in
                    (match k1 with Closure (x, e', m') -> let new_m = ins_env m' x v' in
                            eval_exp (e', new_m))
  | LetRecInExp (f, x, e1, e2) -> let new_m = ins_env m f (RecVarVal (f, x, e1, m)) in
                                eval_exp (e2, new_m)
  | _ -> raise (Failure "Not implemented yet.")

let eval_dec (dec, m) =
  match dec with
    Anon e -> let v = eval_exp(e, m) in
        ((None, v), m)
  | Let (x, e) -> let v = eval_exp(e, m) in
      let new_m = ins_env m x v in ((Some(x), v), new_m)
  | LetRec (f, x, e) -> let rvv = RecVarVal (f, x, e, m) in
                        let new_m = ins_env m f rvv in
                        ((Some (f), rvv), new_m)
