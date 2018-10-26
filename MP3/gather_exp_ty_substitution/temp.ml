print_string(niceInfer_exp gather_exp_ty_substitution []
(LetRecInExp("length",
"list",
IfExp(BinOpAppExp(EqOp, VarExp "list", ConstExp NilConst),
ConstExp (IntConst 0),
BinOpAppExp(IntPlusOp, ConstExp (IntConst 1),
(AppExp(VarExp "length",
MonOpAppExp(TlOp,VarExp "list"))))),
AppExp(VarExp "length",
BinOpAppExp(ConsOp,
ConstExp (IntConst 2),
ConstExp NilConst)))));;

#load "common.cmo";;
#load "solution.cmo";;
#use "gather_exp_ty_substitution.ml";;


Inference:
SC: {'t -> int}
---------------------------------------------------------------------------
  {b : bool, y : int} |- y : 't

Status:
Failed side condition var_inst_sc: Polymorhic instance check: Domain of substitution {'t -> int}
 and binder of polymorphic type int do not match.
Points lost: 1


Inference:
---------------------------------------------------------------------------
  {x : ALL 't. 't, b : bool, y : int} |- x : int

Status:
Failed side condition var_inst_sc: Polymorhic instance check: Domain of substitution {}
 and binder of polymorphic type ALL 't. 't do not match.
Points lost: 1


Total: [18 / 38]

 244955e3-726d-4f31-8047-5897ed60fb4e 
