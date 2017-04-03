{ open Printf }
(* Ocamllex scanner for MicroC *)

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { print_string "LPAREN "}
| ')'      { print_string "RPAREN "}
| '{'      { print_string "LBRACE "}
| '}'      { print_string "RBRACE "}
| ';'      { print_string "SEMI "}
| ','      { print_string "COMMA "}
| '+'      { print_string "PLUS "}
| '-'      { print_string "MINUS "}
| '*'      { print_string "TIMES "}
| '/'      { print_string "DIVIDE "}
| '='      { print_string "ASSIGN "}
| "=="     { print_string "EQ "}
| "!="     { print_string "NEQ "}
| '<'      { print_string "LT "}
| "<="     { print_string "LEQ "}
| ">"      { print_string "GT "}
| ">="     { print_string "GEQ "}
| "&&"     { print_string "AND "}
| "||"     { print_string "OR "}
| "!"      { print_string "NOT "}
| "if"     { print_string "IF "}
| "else"   { print_string "ELSE "}
| "for"    { print_string "FOR "}
| "while"  { print_string "WHILE "}
| "return" { print_string "RETURN "}
| "int"    { print_string "INT "}
| "bool"   { print_string "BOOL "}
| "string" { print_string "STRTYPE "}
| "void"   { print_string "VOID "}
| "true"   { print_string "TRUE "}
| "false"  { print_string "FALSE "}
| ['0'-'9']+ as lxm { print_string "LITERAL "; print_string(lxm); print_string " " }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { print_string"ID "; print_string (lxm); print_string " "  }

| '"' { read_string (Buffer.create 17) lexbuf }
(*| eof { print_string "EOF "}*)
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and read_string buf =
  parse
  | '"'       { print_string "STRING "; print_string (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (Failure ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (Failure ("String is not terminated")) }

{
  let main () =
    let lexbuf = Lexing.from_channel stdin in
    try
        while true do
            ignore (token lexbuf)
        done
    with _ -> print_string "invalid_token\n"
  let _ = Printexc.print main ()

}
