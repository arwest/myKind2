(* Copyright (c) 2019 by the Board of Trustees of the University of Iowa

   Licensed under the Apache License, Version 2.0 (the "License") you
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
%{

module A = NuxmvAst

let mk_pos = Position.create_position 

exception Ltl_Use_Error
  
%}


%token MODULE VAR DEFINE ASSIGN TRANS LTLSPEC
%token X G F U V Y Z H O S T
%token <string> ID
%token <int> CINT
%token BOOL INT
%token TRUE FALSE
%token EQ NEQ LT GT LTE GTE
%token RARROW DARROW
%token AND NOT OR XOR XNOR
%token PLUS MINUS MOD
%token NEXT INIT CASE ESAC
%token SELF
%token ASSIGNMENT
%token LPAREN RPAREN LCURLBRACK RCURLBRACK LSQBRACK RSQBRACK
%token COMMA SEMICOLON COLON PERIOD DPERIOD
%token EOF

(* Priorities and associativity of operators, lowest first *)
%right RARROW
%left  DARROW
%left  OR XOR XNOR
%left  AND
%left  EQ NEQ LT GT LTE GTE
%left  MOD
%left  PLUS MINUS
%left  NOT
%nonassoc X G F U V Y Z H O S T

%start<NuxmvAst.t> program
%type<NuxmvAst.nuxmv_expr> basic_expr next_expr simple_expr

%%
program: ml = nonempty_list(module_decl) EOF { ml }

module_decl: MODULE id = ID mp = option(module_params) mb = option(module_body) { A.CustomModule (id, mp, mb) }  

module_params: LPAREN mpl = separated_nonempty_list(COMMA, ID) RPAREN { mpl } 

module_body: mel = nonempty_list(module_element) { mel }

module_element:
   svd = state_var_declaration { svd }
 | dd = define_declaration { dd }
 | ac = assign_constraint { ac }
 | tc = trans_constraint { tc }
 | ltls = ltl_specification { ltls }
 

(* State Var Declarations *)
state_var_declaration: VAR sve = nonempty_list(state_var_element) { A.StateVarDecl (mk_pos $startpos, sve) } ;

state_var_element:
    | i = ID COLON sts = simple_type_specifier SEMICOLON { A.SimpleType (mk_pos $startpos, i, sts) }
    | i = ID COLON mts = module_type_spec SEMICOLON { A.ModuleType (mk_pos $startpos, i, mts) }
    ;

simple_type_specifier:
    | BOOL { A.Bool (mk_pos $startpos) }
    | INT { A.Int (mk_pos $startpos) }
    | i1 = CINT DPERIOD i2 = CINT { A.IntRange (mk_pos $startpos, i1, i2) }
    | LCURLBRACK etvl = separated_nonempty_list(COMMA, enum_type_value) RCURLBRACK { A.EnumType (mk_pos $startpos, etvl) }
    ;

enum_type_value:
    | i = ID { A.ETId (mk_pos $startpos, i) }
    | ci = CINT { A.ETCInt (mk_pos $startpos, ci) }
    ;

module_type_spec:
    | id = ID pl = option(module_type_spec_param_list) { A.ModuleTypeSpecifier (mk_pos $startpos, id, pl)}
    ;

module_type_spec_param_list:
    | LPAREN RPAREN { [] }
    | LPAREN il = basic_expr_list RPAREN { il }
    ;

(* Definition Declarations *)
define_declaration: DEFINE del = nonempty_list(define_element) { A.DefineDecl (mk_pos $startpos, del)}
    ;

define_element:
    | i = ID ASSIGNMENT e = simple_expr SEMICOLON { A.SimpleDef (mk_pos $startpos, i, e) }
    | i = ID ASSIGNMENT ae = array_expr SEMICOLON { A.ArrayDef (mk_pos $startpos, i, ae) }
    ;

(* Assign Constraints *)
assign_constraint: ASSIGN ael = nonempty_list(assign_element) { A.AssignConst (mk_pos $startpos, ael) }
    ;

assign_element:
    | INIT LPAREN ci = complex_indentifier RPAREN ASSIGNMENT e = simple_expr SEMICOLON { A.InitAssign (mk_pos $startpos, ci, e) }
    | NEXT LPAREN ci = complex_indentifier RPAREN ASSIGNMENT e = simple_expr SEMICOLON { A.NextAssign (mk_pos $startpos, ci, e) }
    | ci = complex_indentifier ASSIGNMENT e = next_expr { A.Assign (mk_pos $startpos, ci, e) }
    ;

complex_indentifier:
    | i = ID { A.CIdent (mk_pos $startpos, i) }
    | ci = complex_indentifier PERIOD i = ID { A.PerIdent (mk_pos $startpos, ci, i) }
    | ci = complex_indentifier LSQBRACK e = basic_expr RSQBRACK { BrackIdent (mk_pos $startpos, ci, e) }
    | SELF { A.Self (mk_pos $startpos) }
    ;

(* Trans Constraints *)
trans_constraint: TRANS e = next_expr option(SEMICOLON) { A.TransConst (mk_pos $startpos, e) } 
    ;

(* LTL Specifications *)
ltl_specification: LTLSPEC le = ltl_expr option(SEMICOLON) { A.LtlSpec (mk_pos $startpos, le) }
    ;

(* General Purpose Rules *)
(* dummy rule for parameter of pexpr to signal we allow quantifiers *)
%inline ltl:
  | { true }

(* dummy rule for parameter of pexpr to signal we do not allow quantifiers *)
%inline notltl:
  | { false }

ltl_expr:
    | e = expr(ltl) { e }
    ;

basic_expr:
    | e = expr(notltl) { e }
    ;

expr(L):
    | c = constant { c }
    | fc = function_call { fc }
    | NOT e = expr(L) { A.Not (mk_pos $startpos, e) }
    | e1 = expr(L) AND e2 = expr(L) { A.And (mk_pos $startpos, e1, e2) }
    | e1 = expr(L) OR e2 = expr(L) { A.Or (mk_pos $startpos, e1, e2) }
    | e1 = expr(L) XOR e2 = expr(L) { A.Xor (mk_pos $startpos, e1, e2) }
    | e1 = expr(L) XNOR e2 = expr(L) { A.Xnor (mk_pos $startpos, e1, e2) }
    | e1 = expr(L) RARROW e2 = expr(L) { A.Impl (mk_pos $startpos, e1, e2) }
    | e1 = expr(L) DARROW e2 = expr(L) { A.Equiv (mk_pos $startpos, e1, e2) }
    | e1 = expr(L) EQ e2 = expr(L) { A.Eq (mk_pos $startpos, e1, e2)}
    | e1 = expr(L) NEQ e2 = expr(L) { A.NotEq (mk_pos $startpos, e1, e2) }
    | e1 = expr(L) LT e2 = expr(L) { A.Lt (mk_pos $startpos, e1, e2) }
    | e1 = expr(L) GT e2 = expr(L) { A.Gt (mk_pos $startpos, e1, e2) }
    | e1 = expr(L) LTE e2 = expr(L) { A.Lte (mk_pos $startpos, e1, e2) }
    | e1 = expr(L) GTE e2 = expr(L) { A.Gte (mk_pos $startpos, e1, e2) }
    | MINUS e = expr(L) { A.Uminus (mk_pos $startpos, e)}
    | e1 = expr(L) PLUS e2 = expr(L) { A.Plus (mk_pos $startpos, e1, e2)}
    | e1 = expr(L) MINUS e2 = expr(L) { A.Minus (mk_pos $startpos, e1, e2) }
    | e1 = expr(L) MOD e2 = expr(L) { A.Mod (mk_pos $startpos, e1, e2) }
    | LPAREN e = expr(L) RPAREN { e }
    | LCURLBRACK el = separated_nonempty_list(COMMA, expr(L)) RCURLBRACK { A.SetExp (mk_pos $startpos, el) }
    | CASE cel = nonempty_list(case_element) ESAC { A.CaseExp (mk_pos $startpos, cel) }
    | NEXT LPAREN e = expr(L) RPAREN { A.NextExp (mk_pos $startpos, e)}

    (* Ltl Expressions*)
        (* FUTURE *)
    | X le = expr(L) { A.NextState (mk_pos $startpos, le) }
    | G le = expr(L) { A.Globally (mk_pos $startpos, le) }
    | F le = expr(L) { A.Finally (mk_pos $startpos, le) }
    | le1 = expr(L) U le2 = expr(L) { A.Until (mk_pos $startpos, le1, le2) }
    | le1 = expr(L) V le2 = expr(L) { A.Releases (mk_pos $startpos, le1, le2) }
        (* PAST *)
    | Y le = expr(L) { A.PrevState (mk_pos $startpos, le) }
    | Z le = expr(L) { A.NotPrevStateNot (mk_pos $startpos, le) }
    | H le = expr(L) { A.Historically (mk_pos $startpos, le) }
    | O le = expr(L) { A.Once (mk_pos $startpos, le) }
    | le1 = expr(L) S le2 = expr(L) { A.Since (mk_pos $startpos, le1, le2) }
    | le1 = expr(L) T le2 = expr(L) { A.Triggered (mk_pos $startpos, le1, le2) }
    ;

function_call: 
    | ci = complex_indentifier LPAREN el = function_call_params RPAREN { A.Call (mk_pos $startpos, ci, el) }
    ;

function_call_params:
    | e = basic_expr { [e] }
    | e = basic_expr function_call_params  {e :: []}
    ;

case_element:
    | e1 = basic_expr COLON e2 = basic_expr SEMICOLON { (e1, e2) }
    ;

simple_expr: (* Note that simple expressions cannot contain next operators *)
    | b = basic_expr { b }
    ;

next_expr:
    | b = basic_expr { b }
    ;

basic_expr_list:
    | el = separated_nonempty_list(COMMA, basic_expr) { el }
    ;

array_expr:
    | LSQBRACK el = separated_nonempty_list(COMMA, next_expr) RSQBRACK { A.ArrayExp (mk_pos $startpos, el) }
    | LSQBRACK al =  separated_nonempty_list(COMMA, array_expr) RSQBRACK { A.ArrayExp (mk_pos $startpos, al) }
    ;

constant:
    | FALSE { A.False (mk_pos $startpos) }
    | TRUE { A.True (mk_pos $startpos) }
    | s = ID { A.Ident (mk_pos $startpos, s) }
    | i = CINT { A.CInt (mk_pos $startpos, i) }
    | i1 = basic_expr DPERIOD i2 = basic_expr { A.CRange (mk_pos $startpos, i1, i2) }
    ;
