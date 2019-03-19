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

type output = (unit, Format.formatter, unit) format

type parse_error =
  | UnexpectedChar of Position.t * char
  | SyntaxError of Position.t
  | LtlUseError of Position.t
  | NextExprError of Position.t
  | DoubleNextExprError of Position.t
  | RangeLowerValueError of Position.t
  | ExpectedTypeError of Position.t (* *nuxmv_ast_type list * nuxmv_ast_type *)
  | NonMatchingTypeError of Position.t (* *nuxmv_ast_type * nuxmv_ast_type *)
  | MissingVariableError of Position.t (* *string *)
  | AssignTypeError of Position.t (* *nuxmv_ast_type * nuxmv_ast_type *)

let parse_buffer lexbuf : (output, parse_error) result =
  try
    let abstract_syntax = NuxmvParser.program NuxmvLexer.token lexbuf in
        match NuxmvChecker.semantic_eval abstract_syntax with
        | NuxmvChecker.CheckError (LtlUse pos )-> Error (LtlUseError pos)
        | NuxmvChecker.CheckError (NextExpr pos ) -> Error (NextExprError pos)
        | NuxmvChecker.CheckError (DoubleNextExpr pos ) -> Error (DoubleNextExprError pos)
        | NuxmvChecker.CheckError (RangeLowerValue pos ) -> Error (RangeLowerValueError pos)
        | NuxmvChecker.CheckOk -> (let type_res = NuxmvChecker.type_eval abstract_syntax in 
                                  match type_res with
                                  | NuxmvChecker.CheckError (Expected (pos, _, _ )) -> Error (ExpectedTypeError pos)
                                  | NuxmvChecker.CheckError (NonMatching (pos, _, _) ) -> Error (NonMatchingTypeError pos)
                                  | NuxmvChecker.CheckError (MissingVariable (pos, _) ) -> Error (MissingVariableError pos)
                                  | NuxmvChecker.CheckError (AssignType (pos, _, _) ) -> Error (AssignTypeError pos)
                                  | NuxmvChecker.CheckOk -> Ok ("Type Check Successful @.") )
        (* Ok (abstract_syntax) *)
  with 
  | NuxmvLexer.Unexpected_Char c ->
    let pos = Position.get_position lexbuf in Error (UnexpectedChar (pos, c))
  | NuxmvParser.Error ->
    let pos = Position.get_position lexbuf in Error (SyntaxError pos)


let from_channel in_ch =
  parse_buffer (Lexing.from_channel in_ch)


let from_file filename =
  let in_ch = open_in filename in
  let lexbuf = Lexing.from_channel in_ch in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with
                                Lexing.pos_fname = filename };
  parse_buffer lexbuf


