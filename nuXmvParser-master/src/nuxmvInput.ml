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


type position = { fname : string; line: int; col: int }

type input = unit

type parse_error =
  | UnexpectedChar of position * char
  | SyntaxError of position


let pp_print_position fmt { fname; line; col } =
  let fname = if fname = "" then "<stdin>" else fname in
  Format.fprintf fmt "%s:%d:%d" fname line col


let get_position lexbuf =
  let pos = Lexing.lexeme_start_p lexbuf in
  {
    fname = pos.Lexing.pos_fname ;
    line = pos.Lexing.pos_lnum ; 
    col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 
  }


let parse_buffer lexbuf =
  try
    Ok (NuxmvParser.module_decl NuxmvLexer.token lexbuf)
  with 
  | NuxmvLexer.Unexpected_Char c ->
    let pos = get_position lexbuf in Error (UnexpectedChar (pos, c))
  | NuxmvParser.Error ->
    let pos = get_position lexbuf in Error (SyntaxError pos)


let from_channel in_ch =
  parse_buffer (Lexing.from_channel in_ch)


let from_file filename =
  let in_ch = open_in filename in
  let lexbuf = Lexing.from_channel in_ch in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with
                                Lexing.pos_fname = filename };
  parse_buffer lexbuf


