open Common

let rec gather_exp_ty_substitution gamma exp tau =
    let judgment = ExpJudgment(gamma, exp, tau) in
    match exp

    with
    | ConstExp c ->
         let tau' = const_signature c in
         (match unify [(tau, freshInstance tau')]
          with None       -> None
             | Some sigma -> Some(Proof([],judgment), sigma))
    | VarExp v ->
          let t' = lookup_env gamma v in
              (match t' with
              | None -> None
              | Some tau' -> (match unify [(tau, freshInstance tau')] with
                              | None -> None
                              | Some sigma -> Some(Proof([], judgment), sigma)))
    | MonOpAppExp (m, e) ->
            let tau_1 = freshInstance (polyTy_of_monoTy (TyVar 0)) in
            let k = gather_exp_ty_substitution gamma e tau_1 in
                (match k with
                | None -> None
                | Some (proof_so_far, sigma) ->
                      let si = monoTy_lift_subst sigma (mk_fun_ty tau_1 tau) in
                      let tau' = monop_signature m in
                      (match unify [(si, freshInstance tau')] with
                          | None -> None
                          | Some sub -> Some (Proof([proof_so_far], judgment), (subst_compose sub sigma))))
    | BinOpAppExp (b, e1, e2) ->
              let tau_1 = freshInstance (polyTy_of_monoTy (TyVar 0)) in
              let k1 = gather_exp_ty_substitution gamma e1 tau_1 in
                  (match k1 with
                  | None -> None
                  | Some (proof_so_far, sigma1) ->
                        let tau_2 = freshInstance (polyTy_of_monoTy (TyVar 0)) in
                        let sig1_gamma = env_lift_subst sigma1 gamma in
                        let k2 = gather_exp_ty_substitution sig1_gamma e2 tau_2 in
                            (match k2 with
                            | None -> None
                            | Some (some_proof, sigma2) ->
                                let temp = mk_fun_ty tau_2 tau in
                                let temp2 = mk_fun_ty tau_1 temp in
                                let sub1 = monoTy_lift_subst sigma1 temp2 in
                                let sub2 = monoTy_lift_subst sigma2 sub1 in
                                let tau' = binop_signature b in
                                (match unify [(sub2, freshInstance tau')] with
                                      | None -> None
                                      | Some sub -> Some (Proof([proof_so_far;some_proof], judgment), (subst_compose sub (subst_compose sigma2 sigma1))))))
    | _ -> raise (Failure "Not implemented yet")
