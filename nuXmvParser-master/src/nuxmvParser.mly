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

%start<NuxmvAst.t> program
%type<NuxmvAst.b_expr> basic_expr next_expr simple_expr

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

ltl_expr:
    | b = basic_bool_expr { A.LtlBool (mk_pos $startpos, b) }         (* Manual said simple_expr but specified boolean *)
    | LPAREN le = ltl_expr RPAREN { le }
    | NOT le = ltl_expr { A.LtlNot (mk_pos $startpos, le)}
    | le1 = ltl_expr AND le2 = ltl_expr { A.LtlAnd (mk_pos $startpos, le1, le2) }
    | le1 = ltl_expr OR le2 = ltl_expr { A.LtlOr (mk_pos $startpos, le1, le2) }
    | le1 = ltl_expr XOR le2 = ltl_expr { A.LtlXor (mk_pos $startpos, le1, le2) }
    | le1 = ltl_expr XNOR le2 = ltl_expr { A.LtlXnor (mk_pos $startpos, le1, le2) }
    | le1 = ltl_expr RARROW le2 = ltl_expr { A.LtlImpl (mk_pos $startpos, le1, le2) }
    | le1 = ltl_expr DARROW le2 = ltl_expr { A.LtlEquiv (mk_pos $startpos, le1, le2) }
    (* FUTURE *)
    | X le = ltl_expr { A.NextState (mk_pos $startpos, le) }
    | G le = ltl_expr { A.Globally (mk_pos $startpos, le) }
    | F le = ltl_expr { A.Finally (mk_pos $startpos, le) }
    | le1 = ltl_expr U le2 = ltl_expr { A.Until (mk_pos $startpos, le1, le2) }
    | le1 = ltl_expr V le2 = ltl_expr { A.Releases (mk_pos $startpos, le1, le2) }
    (* PAST *)
    | Y le = ltl_expr { A.PrevState (mk_pos $startpos, le) }
    | Z le = ltl_expr { A.NotPrevStateNot (mk_pos $startpos, le) }
    | H le = ltl_expr { A.Historically (mk_pos $startpos, le) }
    | O le = ltl_expr { A.Once (mk_pos $startpos, le) }
    | le1 = ltl_expr S le2 = ltl_expr { A.Since (mk_pos $startpos, le1, le2) }
    | le1 = ltl_expr T le2 = ltl_expr { A.Triggered (mk_pos $startpos, le1, le2) }
    ;

(* General Purpose Rules *)
basic_expr:
    | c = constant { c }
    | fc = function_call { fc }
    | b = basic_bool_expr { b }
    | MINUS e = basic_expr { A.Uminus (mk_pos $startpos, e)}
    | e1 = basic_expr PLUS e2 = basic_expr { A.Plus (mk_pos $startpos, e1, e2)}
    | e1 = basic_expr MINUS e2 = basic_expr { A.Minus (mk_pos $startpos, e1, e2) }
    | e1 = basic_expr MOD e2 = basic_expr { A.Mod (mk_pos $startpos, e1, e2) }
    | LPAREN e = basic_expr RPAREN { e }
    | LCURLBRACK el = separated_nonempty_list(COMMA, basic_expr) RCURLBRACK { A.SetExp (mk_pos $startpos, el) }
    | CASE cel = nonempty_list(case_element) ESAC { A.CaseExp (mk_pos $startpos, cel) }
    | NEXT LPAREN e = basic_expr RPAREN { A.NextExp (mk_pos $startpos, e)}
    ;

basic_bool_expr:
    | NOT e = basic_expr { A.Not (mk_pos $startpos, e) }
    | e1 = basic_expr AND e2 = basic_expr { A.And (mk_pos $startpos, e1, e2) }
    | e1 = basic_expr OR e2 = basic_expr { A.Or (mk_pos $startpos, e1, e2) }
    | e1 = basic_expr XOR e2 = basic_expr { A.Xor (mk_pos $startpos, e1, e2) }
    | e1 = basic_expr XNOR e2 = basic_expr { A.Xnor (mk_pos $startpos, e1, e2) }
    | e1 = basic_expr RARROW e2 = basic_expr { A.Impl (mk_pos $startpos, e1, e2) }
    | e1 = basic_expr DARROW e2 = basic_expr { A.Equiv (mk_pos $startpos, e1, e2) }
    | e1 = basic_expr EQ e2 = basic_expr { A.Eq (mk_pos $startpos, e1, e2)}
    | e1 = basic_expr NEQ e2 = basic_expr { A.NotEq (mk_pos $startpos, e1, e2) }
    | e1 = basic_expr LT e2 = basic_expr { A.Lt (mk_pos $startpos, e1, e2) }
    | e1 = basic_expr GT e2 = basic_expr { A.Gt (mk_pos $startpos, e1, e2) }
    | e1 = basic_expr LTE e2 = basic_expr { A.Lte (mk_pos $startpos, e1, e2) }
    | e1 = basic_expr GTE e2 = basic_expr { A.Gte (mk_pos $startpos, e1, e2) }
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
