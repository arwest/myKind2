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

(* @author Daniel Larraz *)

%token MODULE VAR DEFINE ASSIGN
%token <string> ID
%token NEXT INIT CASE ESAC
%token EQUALS ASSIGNMENT
%token LPAREN RPAREN LCURLBRACK RCURLBRACK
%token COMMA SEMICOLON COLON
%token EOF

%start<unit> module_decl

%%

module_decl: MODULE ID option(module_params) option(module_body) EOF {} ; 

module_params: LPAREN separated_nonempty_list(COMMA, ID) RPAREN {} ;

module_body: nonempty_list(module_element) {};

module_element:
   var_declaration {}
 | define_declaration {}
 | assign_constraint {}
 ;

var_declaration: VAR {}; (* TODO *)

define_declaration: DEFINE {}; (* TODO *)

assign_constraint: ASSIGN {}; (* TODO *)

