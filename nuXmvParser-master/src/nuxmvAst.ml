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

(* @author Andrew West*)

type ident = string


(* Basic Expressions *)
type nuxmv_expr = 
    (* Values *)
    | True of Position.t
    | False of Position.t
    | CInt of Position.t * int
    | Ident of Position.t * ident
    | CRange of Position.t * nuxmv_expr * nuxmv_expr (* Make sure the expressions returned are ints *)

    (* Function Call *)
    | Call of Position.t * comp_ident * nuxmv_expr list

    (* Boolean operators *)
    | Not of Position.t * nuxmv_expr
    | And of Position.t * nuxmv_expr * nuxmv_expr
    | Or of Position.t * nuxmv_expr * nuxmv_expr
    | Xor of Position.t * nuxmv_expr * nuxmv_expr
    | Xnor of Position.t * nuxmv_expr * nuxmv_expr
    | Impl of Position.t * nuxmv_expr * nuxmv_expr
    | Equiv of Position.t * nuxmv_expr * nuxmv_expr

    (* Relations *)
    | Eq of Position.t * nuxmv_expr * nuxmv_expr
    | NotEq of Position.t * nuxmv_expr * nuxmv_expr
    | Lt of Position.t * nuxmv_expr * nuxmv_expr
    | Lte of Position.t * nuxmv_expr * nuxmv_expr
    | Gt of Position.t * nuxmv_expr * nuxmv_expr
    | Gte of Position.t * nuxmv_expr * nuxmv_expr

    (* Arithmetic operators *)
    | Plus of Position.t * nuxmv_expr * nuxmv_expr
    | Uminus of Position.t * nuxmv_expr
    | Minus of Position.t * nuxmv_expr * nuxmv_expr
    | Mod of Position.t * nuxmv_expr * nuxmv_expr
    
    (* Set Expression *)
    | SetExp of Position.t * nuxmv_expr list

    (* Array Expression*)
    | ArrayExp of Position.t * nuxmv_expr list

    (* Case Expression*)
    | CaseExp of Position.t * (nuxmv_expr * nuxmv_expr) list

    (* Next Expression*)
    | NextExp of Position.t * nuxmv_expr
    
    (* Ltl Operations *)
        (* FUTURE *)
    | NextState of Position.t * nuxmv_expr
    | Globally of Position.t * nuxmv_expr
    | Finally of Position.t * nuxmv_expr
    | Until of Position.t * nuxmv_expr * nuxmv_expr
    | Releases of Position.t * nuxmv_expr * nuxmv_expr
        (* PAST *)
    | PrevState of Position.t * nuxmv_expr
    | NotPrevStateNot of Position.t * nuxmv_expr
    | Historically of Position.t * nuxmv_expr
    | Once of Position.t * nuxmv_expr
    | Since of Position.t * nuxmv_expr * nuxmv_expr
    | Triggered of Position.t * nuxmv_expr * nuxmv_expr

(* Complex Identifiers *)
and comp_ident = 
    | CIdent of Position.t * ident
    | PerIdent of Position.t * comp_ident * ident
    | BrackIdent of Position.t * comp_ident * nuxmv_expr
    | Self of Position.t

type enum_type_value = 
    | ETId of Position.t * ident
    | ETCInt of Position.t * int

type simple_type_spec = 
    | Bool of Position.t
    | Int of Position.t
    | IntRange of Position.t * int * int
    | EnumType of Position.t * (enum_type_value) list (* Assert that it is either an indent or cint*)

type module_type_specifier = 
    | ModuleTypeSpecifier of Position.t * ident * (nuxmv_expr list) option

type state_var_decl =
    | SimpleType of Position.t * ident * simple_type_spec
    | ModuleType of Position.t * ident * module_type_specifier

type define_element = 
    | SimpleDef of Position.t * ident * nuxmv_expr (* Assert no next operation in expr *)
    | ArrayDef of Position.t * ident * nuxmv_expr (* Assert that it is an array expr and is allowed next *)

type assign_const = 
    | InitAssign of Position.t * comp_ident * nuxmv_expr (* Assert not next operation *)
    | NextAssign of Position.t * comp_ident * nuxmv_expr (* Assert not next operation *)
    | Assign of Position.t * comp_ident * nuxmv_expr (* Next operation allowed *)

type module_element = 
    | StateVarDecl of Position.t * state_var_decl list
    | DefineDecl of Position.t * define_element list
    | AssignConst of Position.t * assign_const list
    | TransConst of Position.t * nuxmv_expr (* Next operation is allowed *)
    | LtlSpec of Position.t * ltl_expr

type nuxmv_module = 
    | CustomModule of ident * (ident list) option * (module_element list) option

(* A nuxmv program *)
type t = nuxmv_module list


