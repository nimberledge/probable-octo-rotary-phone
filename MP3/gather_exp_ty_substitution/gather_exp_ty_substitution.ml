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
            let tau_1 = fresh () in
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
              let tau_1 = fresh () in
              let k1 = gather_exp_ty_substitution gamma e1 tau_1 in
                  (match k1 with
                  | None -> None
                  | Some (proof_so_far, sigma1) ->
                        let tau_2 = fresh () in
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
    | IfExp (e1, e2, e3) ->
            let k1 = gather_exp_ty_substitution gamma e1 bool_ty in
            (match k1 with
            | None -> None
            | Some (proof1, sigma1) ->
                let gamma_1 = env_lift_subst sigma1 gamma in
                let tau_1 = monoTy_lift_subst sigma1 tau in
                let k2 = gather_exp_ty_substitution gamma_1 e2 tau_1 in
                      (match k2 with
                      | None -> None
                      | Some (proof2, sigma2) ->
                      let gamma_2 = env_lift_subst sigma2 gamma_1 in
                      let tau_2 = monoTy_lift_subst sigma2 tau_1 in
                      let k3 = gather_exp_ty_substitution gamma_2 e3 tau_2 in
                      (match k3 with
                      | None -> None
                      | Some (proof3, sigma3) -> Some (Proof([proof1;proof2;proof3],judgment), subst_compose sigma3 (subst_compose sigma2 sigma1)))))

    | FunExp (x, e) ->
            let tau_1 = fresh () in
            let tau_2 = fresh () in
            let temp_gamma = ins_env gamma x (polyTy_of_monoTy tau_1) in
            let k1 = gather_exp_ty_substitution temp_gamma e tau_2 in
            (match k1 with
                | None -> None
                | Some (proof1, sigma) -> let t1 = monoTy_lift_subst sigma tau in
                let t2 = monoTy_lift_subst sigma (mk_fun_ty tau_1 tau_2) in
                (match unify[(t1, t2)] with
                      | None -> None
                      | Some sub -> Some (Proof([proof1], judgment), subst_compose sub sigma)))

    | AppExp (e1, e2) ->
            let tau_1 = fresh () in
            let temp_type = mk_fun_ty tau_1 tau in
            let k1 = gather_exp_ty_substitution gamma e1 temp_type in
            (match k1 with
                | None -> None
                | Some (proof1, sigma1) -> let gamma_1 = env_lift_subst sigma1 gamma in
                let tau_11 = monoTy_lift_subst sigma1 tau_1 in
                let k2 = gather_exp_ty_substitution gamma_1 e2 tau_11 in
                (match k2 with
                    | None -> None
                    | Some (proof2, sigma2) -> Some (Proof([proof1;proof2], judgment), subst_compose sigma2 sigma1)))

    | RaiseExp e ->
          let k = gather_exp_ty_substitution gamma e int_ty in
          (match k with
              | None -> None
              | Some (proof, sigma) -> Some (Proof([proof], judgment), sigma))

    | LetInExp (x, e1, e2) ->
          let tau_1 = fresh () in
          let k1 = gather_exp_ty_substitution gamma e1 tau_1 in
          (match k1 with
              | None -> None
              | Some (proof1, sigma1) -> let gamma_1 = env_lift_subst sigma1 gamma in
              let tau_11 = monoTy_lift_subst sigma1 tau_1 in
              let some_type = gen gamma_1 tau_11 in
              let new_gamma = ins_env gamma_1 x some_type in
              let new_tau = monoTy_lift_subst sigma1 tau in
              let k2 = gather_exp_ty_substitution new_gamma e2 new_tau in
              (match k2 with
                  | None -> None
                  | Some (proof2, sigma2) -> Some (Proof([proof1;proof2], judgment), subst_compose sigma2 sigma1)))

    | LetRecInExp (f, x, e1, e2) ->
          let tau_1 = fresh () in
          let tau_2 = fresh () in
          let funt1t2 = mk_fun_ty tau_1 tau_2 in
          let temp_gamma = ins_env gamma f (polyTy_of_monoTy funt1t2) in
          let gamma_1 = ins_env temp_gamma x (polyTy_of_monoTy tau_1) in
          let k1 = gather_exp_ty_substitution gamma_1 e1 tau_2 in
          (match k1 with
                | None -> None
                | Some (proof1, sigma1) -> let sig_gamma = env_lift_subst sigma1 gamma in
                let new_tau = monoTy_lift_subst sigma1 tau in
                let new_t1t2 = monoTy_lift_subst sigma1 (mk_fun_ty tau_1 tau_2) in
                let some_type = gen sig_gamma new_t1t2 in
                let new_gamma = ins_env sig_gamma f some_type in
                let k2 = gather_exp_ty_substitution new_gamma e2 new_tau in
                (match k2 with
                    | None -> None
                    | Some (proof2,sigma2) -> Some (Proof([proof1;proof2], judgment), subst_compose sigma2 sigma1)))


    | TryWithExp (e,intopt1,exp1,match_list) ->
            let k1 = gather_exp_ty_substitution gamma e tau in
            (match k1 with
                | None -> None
                | Some (proof1, sigma) ->
                let final_sigma = sigma in
                (match intopt1 with
                    | None -> Some (Proof([proof1], judgment), sigma)
                    | Some x -> let med_gamma = env_lift_subst final_sigma gamma in
                                let med_tau = monoTy_lift_subst final_sigma tau in
                                let k_firstop = gather_exp_ty_substitution med_gamma exp1 med_tau in
                                (match k_firstop with
                                | None -> None
                                | Some (proof_k, sigma_k) -> let final_sigma = subst_compose sigma_k final_sigma in
                                let rec check_all_matches list acc =
                                (match list with
                                | [] -> acc
                                | h :: t -> let (opt, exp) = h in
                                            let mediate_gamma = env_lift_subst final_sigma gamma in
                                            let mediate_tau = monoTy_lift_subst final_sigma tau in
                                            let k_loop = gather_exp_ty_substitution (mediate_gamma) (exp) (mediate_tau) in
                                            (match k_loop with
                                            | None -> None
                                            | Some (proof_l, sigma_l) -> match acc with
                                                                      | None -> None
                                                                      | Some prev_proof_list ->   let final_sigma = subst_compose sigma_l final_sigma in
                                                                                                  let temp1 = ([proof_l] @ prev_proof_list) in
                                                                                                  check_all_matches t (Some (temp1))))
                                in
                                let start_acc = Some ([]) in
                                let k2 = check_all_matches match_list start_acc in
                                (match k2 with
                                | None -> None
                                | Some (proof_list) -> Some (Proof(([proof1;proof_k] @ (proof_list)), judgment), final_sigma)))))
