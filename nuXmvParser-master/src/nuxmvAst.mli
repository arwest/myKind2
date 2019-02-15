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

type b_expr = 
    | True of Position.t
    | False of Position.t
    | CInt of Position.t * int
    | Ident of Position.t * ident
    | CRange of Position.t * b_expr * b_expr
    | Call of Position.t * comp_ident * b_expr list
    | Not of Position.t * b_expr
    | And of Position.t * b_expr * b_expr
    | Or of Position.t * b_expr * b_expr
    | Xor of Position.t * b_expr * b_expr
    | Xnor of Position.t * b_expr * b_expr
    | Impl of Position.t * b_expr * b_expr
    | Equiv of Position.t * b_expr * b_expr
    | Eq of Position.t * b_expr * b_expr
    | NotEq of Position.t * b_expr * b_expr
    | Lt of Position.t * b_expr * b_expr
    | Lte of Position.t * b_expr * b_expr
    | Gt of Position.t * b_expr * b_expr
    | Gte of Position.t * b_expr * b_expr
    | Plus of Position.t * b_expr * b_expr
    | Uminus of Position.t * b_expr
    | Minus of Position.t * b_expr * b_expr
    | Mod of Position.t * b_expr * b_expr
    | SetExp of Position.t * b_expr list
    | ArrayExp of Position.t * b_expr list
    | CaseExp of Position.t * (b_expr * b_expr) list
    | NextExp of Position.t * b_expr
and comp_ident = 
    | CIdent of Position.t * ident
    | PerIdent of Position.t * comp_ident * ident
    | BrackIdent of Position.t * comp_ident * b_expr
    | Self of Position.t

type enum_type_value = 
    | ETId of Position.t * ident
    | ETCInt of Position.t * int

type simple_type_spec = 
    | Bool of Position.t
    | Int of Position.t
    | IntRange of Position.t * b_expr * b_expr
    | EnumType of Position.t * (b_expr) list 

type module_type_param = 
    | MTParam of Position.t * (b_expr list) option

type state_var_decl = 
    | SimpleType of Position.t * ident * simple_type_spec
    | ModuleType of Position.t * ident * ident * (module_type_param list) option

type define_element = 
    | SimpleDef of Position.t * ident * b_expr 
    | ArrayDef of Position.t * ident * b_expr 

type assign_const = 
    | InitAssign of Position.t * comp_ident * b_expr 
    | NextAssign of Position.t * comp_ident * b_expr 
    | Assign of Position.t * comp_ident * b_expr

type ltl_expr = 
    | BoolExpr of Position.t * b_expr
    | LtlNot of Position.t * ltl_expr 
    | LtlAnd of Position.t * ltl_expr * ltl_expr
    | LtlOr of Position.t * ltl_expr * ltl_expr
    | LtlXor of Position.t * ltl_expr * ltl_expr
    | LtlXnor of Position.t * ltl_expr * ltl_expr
    | LtlImpl of Position.t * ltl_expr * ltl_expr
    | LtlEquiv of Position.t * ltl_expr * ltl_expr
    | NextState of Position.t * ltl_expr
    | Globally of Position.t * ltl_expr
    | Finally of Position.t * ltl_expr
    | Until of Position.t * ltl_expr * ltl_expr
    | Releases of Position.t * ltl_expr * ltl_expr
    | PrevState of Position.t * ltl_expr
    | NotPrevStateNot of Position.t * ltl_expr
    | Historically of Position.t * ltl_expr
    | Once of Position.t * ltl_expr
    | Since of Position.t * ltl_expr * ltl_expr
    | Triggered of Position.t * ltl_expr * ltl_expr

type module_element = 
    | StateVarDecl of Position.t * state_var_decl list
    | DefineDecl of Position.t * define_element list
    | AssignConst of Position.t * assign_const
    | TransConst of Position.t * b_expr list
    | LtlSpec of Position.t * ltl_expr

type t = (ident list) option * (module_element list) option