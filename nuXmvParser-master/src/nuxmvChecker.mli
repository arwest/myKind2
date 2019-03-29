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

(** @author Andrew West *)

type semantic_error_type = 
    | LtlUse of Position.t
    | NextExpr of Position.t
    | DoubleNextExpr of Position.t
    | RangeLowerValue of Position.t

type nuxmv_ast_type = 
    | IntT
    | SymbolicT
    | FloatT
    | RangeT
    | EnumT of (string * nuxmv_ast_type) list
    | ArrayT of nuxmv_ast_type list
    | BoolT
    | SetT of nuxmv_ast_type list
    (* | FunT of nuxmv_ast_type list * nuxmv_ast_type *)
    (* | ModuleT of (string * nuxmv_ast_type) list *)

type type_error =
    | Expected of Position.t * nuxmv_ast_type list * nuxmv_ast_type
    | NonMatching of Position.t * nuxmv_ast_type * nuxmv_ast_type
    | MissingVariable of Position.t * string
    | AssignType of Position.t * nuxmv_ast_type * nuxmv_ast_type
    | EnumValueExist of Position.t * string
    | EnumNotContain of Position.t * string

type 'a check_result = 
    | CheckOk
    | CheckError of 'a

val semantic_eval: NuxmvAst.t ->  semantic_error_type check_result

val type_eval : NuxmvAst.t -> type_error check_result