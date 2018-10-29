{
open Common;;

}

(* You can assign names to commonly-used regular expressions in this part
   of the code, to save the trouble of re-typing them each time they are used *)

let numeric = ['0' - '9']
let lowercase = ['a' - 'z']
let uppercase = ['A' - 'Z']
let letter =['a' - 'z' 'A' - 'Z' '_']
let special= "\\$^.*+?[]"
let ident_spec = '_' | "'"
let integer = numeric +
let binary = "0b" ['0' - '1'] +
let hex = "0x" (numeric | ['a'-'f'])+
let flt = numeric+ '.' numeric*
let flt_exp = flt 'e' integer
let ident = lowercase (uppercase | lowercase | numeric | ident_spec)*
let not_newline = [ '\000' - '\255'] # '\n'
let any_char = ['\000' - '\255']
let not_escape = (any_char # ['n' 't' '\\' '\'' '\"' 'r' 'b' ' ' ])
let line_comm = "//" not_newline* '\n'
let open_comm = "(*"
let close_comm = "*)"
let printable = [ '\032' - '\127'] # ['\"' '\\']
let zero_to_five = [ '0' - '5' ]
let zero_to_four = [ '0' - '4' ]
let ddd = ((['0' - '1' ] numeric numeric) | ('2' ((zero_to_four numeric) | ('5' zero_to_five))))
let whitespace = [' ' '\t' '\n']
let split_lines = '\\''\n' (' ' | '\t')*

rule token = parse
  | whitespace      { token lexbuf }  (* skip over whitespace *)
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
  | '^'             { CARAT }
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
  | open_comm       { comment 1 lexbuf }
  | close_comm      { raise (Failure "unmatched closed comment") }
  | '"'             { string "" lexbuf }

and comment num= parse
  | open_comm       { comment (num+1) lexbuf }
  | close_comm      { if (num=1) then token lexbuf else comment (num-1) lexbuf }
  | eof             { raise (Failure "unmatched open comment") }
  | _               { comment (num) lexbuf }
and string curr_str = parse
  | '"'             { STRING (curr_str) }
  | printable* as k { string (curr_str ^ k) lexbuf }
  | "\\\\"          { string (curr_str ^ (String.make 1 '\\')) lexbuf }
  | "\\\'"          { string (curr_str ^ (String.make 1 '\'')) lexbuf }
  | "\\\""          { string (curr_str ^ (String.make 1 '\"')) lexbuf }
  | "\\t"           { string (curr_str ^ (String.make 1 '\t')) lexbuf }
  | "\\n"           { string (curr_str ^ (String.make 1 '\n')) lexbuf }
  | "\\r"           { string (curr_str ^ (String.make 1 '\r')) lexbuf }
  | "\\b"           { string (curr_str ^ (String.make 1 '\b')) lexbuf }
  | "\\ "           { string (curr_str ^ (String.make 1 ' ') ) lexbuf }
  | '\\' ddd as k   { string (curr_str ^ (String.make 1 (char_of_int (int_of_string (String.sub k 1 3))))) lexbuf }
  | split_lines     { string (curr_str) lexbuf }
  | "\\"_           { raise (Failure "illegal escape sequence")}




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
