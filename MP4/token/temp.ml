(*Just stuff to paste into terminal ocaml interactive*)

#load "common.cmo";;
#use "student.ml";;
get_all_tokens "\"Hello, \\  \\ world! \"";; (*exception*)



get_all_tokens "then hi \"by \\\n  \t  \\t well?\"" (*exception*)

get_all_tokens "then hi \"by \\\n  \t  \\t well?\""

get_all_tokens "\"by \\\n  \t  \\t well?\"";;
get_all_tokens "\"by \\\n  \t\"";;
get_all_tokens "\"by \\\n\"";;
get_all_tokens "\"by \\n\"";;
get_all_tokens "\"\\\n\"";;

get_all_tokens "\" she said, \\\"hello\\\"\"" (*exception*)
get_all_tokens "\" she said, \\\" hello \"\""



get_all_tokens "\"Hello, \\n \\ world! \""

get_all_tokens "\"a line \\n starts here; indent \\t starts here next string\" \"starts here\""
"a line \\n starts here; indent \\t starts here next string\" \"starts here"

get_all_tokens "this is where if";;
get_all_tokens "let = in + , ; ( - )";;
get_all_tokens "42 0 7";;
get_all_tokens "0b110100100";; (*420*)
get_all_tokens "0x8844ffaa11";; (*585273158161*)
get_all_tokens "3.14 100.5 1.414";;
get_all_tokens "2.7e10";;


[0 / 1] get_all_tokens "\"Hello, \\n \\ world! \"" (student solution returns an incorrect value)
