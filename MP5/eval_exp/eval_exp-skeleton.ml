open Common;;

let const_to_val c = raise (Failure "Not implemented yet.")

let monOpApply op v = raise (Failure "Not implemented yet.")

let binOpApply binop (v1,v2) = raise (Failure "Not implemented yet.")

let rec eval_exp (exp, m) = raise (Failure "Not implemented yet.")

let eval_dec (dec, m) =
  match dec with
    Anon e -> let v = eval_exp(e, m) in
        ((None, v), m)
  | Let (x, e) ->
  | LetRec (x, f, e) ->
