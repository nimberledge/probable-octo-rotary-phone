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
