(*Just stuff to paste into terminal ocaml interactive*)

#load "common.cmo";;
#use "student.ml";;

get_all_tokens "this is where if";;

get_all_tokens "let = in + , ; ( - )";;
get_all_tokens "42 0 7";;
get_all_tokens "0b110100100";; (*420*)
get_all_tokens "0x8844ffaa11";; (*585273158161*)
get_all_tokens "3.14 100.5 1.414";;
get_all_tokens "2.7e10";;
