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
  | SymbolicExistenceError of Position.t (* *string *)

val from_channel: in_channel -> (output, parse_error) result

val from_file: string -> (output, parse_error) result

