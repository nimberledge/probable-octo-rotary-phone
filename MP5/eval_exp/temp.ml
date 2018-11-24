(match v with
    ListVal val_list -> (match val_list with
    | [] -> raise (Failure "tl")
    | h :: t -> t)
  | _ -> raise (Failure "tl"))
