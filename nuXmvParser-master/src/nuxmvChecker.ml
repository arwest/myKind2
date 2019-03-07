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

module A = NuxmvAst

type semantic_error_type =
    | LtlUse
    | NextExpr

type 'a check_result = 
    | Ok
    | Error of 'a

let empty_pos = Position.create_empty_position

let semantic_eval ml = 
    match ml with
    | [] -> Ok
    | A.CustomModule (_, _, m) :: [] -> Ok
    | A.CustomModule (_, _, m) :: t -> Ok

let s_eval_module_element me = true