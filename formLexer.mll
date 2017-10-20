{
open Printf
open Lexing
open FormParser

exception SyntaxError of string
}

let space = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let name = ['a'-'z' 'A'-'Z' '0'-'9' '{' '}' '^' '_' '\\']+

rule read =
  parse
  | newline     { Lexing.new_line lexbuf; read lexbuf }
  | "bottom"    { FALSE }
  | "not "      { NEG }
  | " and "     { AND }
  | " or "      { OR }
  | " imply "   { IMPLY }
  | " equiv "   { EQUIV }
  | "exists "   { EXISTS }
  | "forall "   { FORALL }
  | name        { NAME (Lexing.lexeme lexbuf) }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | space       { SPACE }
  | eof         { EOF }
  | _           { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
