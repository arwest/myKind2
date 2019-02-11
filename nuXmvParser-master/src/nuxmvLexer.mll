(* Copyright (c) 2019 by the Board of Trustees of the University of Iowa

   Licensed under the Apache License, Version 2.0 (the "License"); you
   may not use this file except in compliance with the License.  You
   may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0 

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
   implied. See the License for the specific language governing
   permissions and limitations under the License. 

*)

(** @author Daniel Larraz *)

{
  module P = NuxmvParser

  exception Unexpected_Char of char
}

let whitespace = [' ' '\t']

let newline = '\r' | '\n' | "\r\n"

let digit = ['0'-'9']

let letter = ['_' 'a'-'z' 'A'-'Z']

let identifier = letter (digit | letter | ['$' '#' '-'])*

let constInt = (digit)+ | '-' (digit)+

rule token = parse
  | "MODULE"         { P.MODULE }
  | "VAR"            { P.VAR }
  | "DEFINE"         { P.DEFINE }
  | "ASSIGN"         { P.ASSIGN }
  | "self"           { P.SELF }
  | "init"           { P.INIT }
  | "next"           { P.NEXT }
  | "case"           { P.CASE }
  | "esac"           { P.ESAC }
  | "boolean"        { P.BOOL }
  | "integer"        { P.INT }
  | "TRUE"           { P.TRUE }
  | "FALSE"          { P.FALSE }
  | '.'              { P.PERIOD }
  | ';'              { P.SEMICOLON }
  | ':'              { P.COLON }
  | '{'              { P.LCURLBRACK }
  | '}'              { P.RCURLBRACK }
  | '['              { P.LSQBRACK }
  | ']'              { P.RSQBRACK }
  | ','              { P.COMMA }
  | '('              { P.LPAREN }
  | ')'              { P.RPAREN }
  | '&'              { P.AND }
  | '='              { P.EQUALS }
  | ":="             { P.ASSIGNMENT }
  | constInt as int  { P.CINT (int_of_string (int)) }
  | identifier as id { P.ID id }
  | whitespace       { token lexbuf }
  | newline          { Lexing.new_line lexbuf ; token lexbuf }
  | eof              { P.EOF }
  | _ as c           { raise (Unexpected_Char c) }

