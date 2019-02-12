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

(* @author Daniel Larraz (skeleton) and Andrew West*)

%token MODULE VAR DEFINE ASSIGN
%token <string> ID
%token <int> CINT
%token BOOL INT
%token TRUE FALSE
%token NEXT INIT CASE ESAC
%token SELF
%token EQUALS AND
%token ASSIGNMENT
%token LPAREN RPAREN LCURLBRACK RCURLBRACK LSQBRACK RSQBRACK
%token COMMA SEMICOLON COLON PERIOD
%token EOF

(* Priorities and associativity of operators, lowest first *)
%left AND
%left EQUALS

%start<unit> module_decl

%%

module_decl: MODULE ID option(module_params) option(module_body) EOF {} ; 

module_params: LPAREN mpl = separated_nonempty_list(COMMA, ID) RPAREN { mpl } ;

module_body: mel = nonempty_list(module_element) { mel };

module_element:
   svd = state_var_declaration { svd }
 | dd = define_declaration { dd }
 | ac = assign_constraint { ac }
 ;

(* State Var Declarations *)
state_var_declaration: VAR nonempty_list(state_var_element) {}; (* TODO *)

state_var_element:
    | ID COLON type_specifier SEMICOLON {}
    ;
type_specifier:
    | s = simple_type_specifier { s }
    | m = module_type_specifier { m }
    ;

module_type_specifier: (* TODO: Determine if module_type_specifier is necessary*)
    | ID option(module_type_spec_params) {} 
    ;

module_type_spec_params:
    | LPAREN separated_list(COMMA, ID) RPAREN {}
    ;

simple_type_specifier:
    | BOOL {}
    | INT {}
    | LCURLBRACK separated_nonempty_list(COMMA, ID) RCURLBRACK {}
    ;

(* Definition Declarations *)
define_declaration: DEFINE nonempty_list(define_element) {}; (* TODO *)

define_element:
    | ID ASSIGNMENT simple_expr SEMICOLON {}
    | ID ASSIGNMENT array_expr SEMICOLON {}
    ;

(* Assign Constraints *)
assign_constraint: ASSIGN nonempty_list(assign_element) {}; (* TODO *)

assign_element:
    | INIT LPAREN complex_indentifier RPAREN ASSIGNMENT simple_expr SEMICOLON {}
    | NEXT LPAREN complex_indentifier RPAREN ASSIGNMENT simple_expr SEMICOLON {}
    | complex_indentifier ASSIGNMENT next_expr {}
    ;

(* General Purpose Rules *)
basic_expr:
    | c = constant { c }
    | basic_expr EQUALS basic_expr {}
    | LPAREN basic_expr RPAREN {}
    | basic_expr AND basic_expr {}
    | LCURLBRACK separated_nonempty_list(COMMA, basic_expr) RCURLBRACK {}
    | CASE nonempty_list(case_element) ESAC {}
    ;

simple_expr: (* Note that simple expressions cannot contain next operators *)
    | b = basic_expr { b }
    ;

next_expr:
    | b = basic_expr { b }
    ;
    
(*
basic_expr_list:
    | separated_nonempty_list(COMMA, basic_expr) {}
*)

case_element:
    | basic_expr COLON basic_expr SEMICOLON {}
    ;

array_expr:
    | LSQBRACK separated_nonempty_list(COMMA, next_expr) RSQBRACK {}
    | LSQBRACK separated_nonempty_list(COMMA, array_expr) RSQBRACK {}
    ;

complex_indentifier:
    | ID {}
    | complex_indentifier PERIOD ID {}
    | complex_indentifier LSQBRACK basic_expr RSQBRACK {}
    | SELF {}
    ;

constant:
    | FALSE {}
    | TRUE {}
    | ID {}
    | CINT {}
    ;
