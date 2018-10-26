{
open Common;;

}

(* You can assign names to commonly-used regular expressions in this part
   of the code, to save the trouble of re-typing them each time they are used *)

let numeric = ['0' - '9']
let lowercase = ['a' - 'z']
let letter =['a' - 'z' 'A' - 'Z' '_']
let special= "\\$^.*+?[]"
let ident_spec = "_'"
let integer = numeric +
let binary = "0b" ['0' - '1'] +
let hex = "0x" (numeric | ['a'-'f'])*
let flt = numeric+ '.' numeric*
let flt_exp = flt 'e' integer
let ident = lowercase (lowercase | numeric | ident_spec)*
let line_comm = "//.*"'\n'

rule token = parse
  | [' ' '\t' '\n'] { token lexbuf }  (* skip over whitespace *)
  | eof             { EOF }
  | '~'             { NEG }
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '*'             { TIMES }
  | '/'             { DIV }
  | "+."            { DPLUS }
  | "-."            { DMINUS }
  | "*."            { DTIMES }
  | "/."            { DDIV }
  | '*'             { CARAT }
  | '<'             { LT }
  | '>'             { GT }
  | "<="            { LEQ }
  | ">="            { GEQ }
  | '='             { EQUALS }
  | "<>"            { NEQ }
  | '|'             { PIPE }
  | "->"            { ARROW }
  | ';'             { SEMI }
  | ";;"            { DSEMI }
  | "::"            { DCOLON }
  | '@'             { AT }
  | "[]"            { NIL }
  | "let"           { LET }
  | "rec"           { REC }
  | "and"           { AND }
  | "end"           { END }
  | "in"            { IN }
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }
  | "fun"           { FUN }
  | "mod"           { MOD }
  | "raise"         { RAISE }
  | "try"           { TRY }
  | "with"          { WITH }
  | "not"           { NOT }
  | "&&"            { LOGICALAND }
  | "||"            { LOGICALOR }
  | '['             { LBRAC }
  | ']'             { RBRAC }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | ','             { COMMA }
  | '_'             { UNDERSCORE }
  | "true"          { TRUE }
  | "false"         { FALSE }
  | "()"            { UNIT }
  | integer as k    { INT (int_of_string k)}
  | binary as k     { INT (int_of_string k)}
  | hex as k        { INT (int_of_string k)}
  | flt as k        { FLOAT (float_of_string k)}
  | flt_exp as k    { FLOAT (float_of_string k)}
  | ident as k      { IDENT k}
  | line_comm       { token lexbuf }



(* your rules go here *)


{(* do not modify this function: *)
 let lextest s = token (Lexing.from_string s)

 let get_all_tokens s =
     let b = Lexing.from_string (s^"\n") in
     let rec g () =
     match token b with EOF -> []
     | t -> t :: g () in
     g ()

let try_get_all_tokens s =
    try (Some (get_all_tokens s), true)
    with Failure "unmatched open comment" -> (None, true)
       | Failure "unmatched closed comment" -> (None, false)
 }
