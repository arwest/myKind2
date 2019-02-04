
(* Special characters *)
%token SEMICOLON
%token COMMA
%token EQUALS
%token ASSIGNMENT
%token LCURLBRACK
%token RCURLBRACK
%token LPAREN
%token RPAREN

(* Tokens for node declarations *)
%token VAR
%token DEFINE
%token ASSIGN
%token NAME

(* Tokens for assignment *)
%token INIT
%token NEXT

(* Tokens for Case*)
%token CASE
%token ESAC

 (* Boolean Operators *)
 %token FALSE
 %token TRUE
 %token AND
 %token OR
 %token XOR

 (* Types *)
%token BOOL
%token ENUM
%token INT
%token BINT
%token REAL
%token SWORD
%token UWORD

(* Token for end of file marker *)
%token EOF

(* Priorities and associativity of operators, lowest first *)
%left OR XOR
%left AND
%left EQUALS
%nonassoc NOT 

(* Start token *)
%start main

%%

(* A nuXmv program is split into 3 main parts VARIABLES, DEFINE, ASSIGNMENT *)
main: 
    VAR ; v = var_decl ; DEFINE ; d = def_decl ; ASSIGNSECTION ; a = assign_decl ; EOF 
    { true }

var_decl:
    | NAME COLON type SEMICOLON var_decl {true}
    | NAME COLON LCURLBRACK enum_values RCURLBRACK SEMICOLON var_decl {true}

def_decl:
    | NAME ASSIGNMENT LPAREN bool_exp RPAREN SEMICOLON def_decl {true}

assign_decl:
    | INIT LPAREN NAME RPAREN ASSIGNMENT assign_expr SEMICOLON assign_decl {true}
    | NEXT LPAREN NAME RPAREN ASSIGNMENT assign_expr SEMICOLON assign_decl {true}

type:
    | BOOL {true}
    | ENUM {true}
    | INT {true}
    | BINT {true}
    | REAL {true}
    | SWORD {true}
    | UWORD {true}

enum_values:
    | NAME {true}
    | NAME COMMA enum_values {true}

bool_exp:
    | value {true}
    | NOT value {true}
    | value EQUALS bool_exp {true}
    | value AND bool_exp {true}
    | value OR bool_exp {true}
    | value XOR bool_exp {true}

assign_expr:
    | CASE case_exprs ESAC {true}
    | value {true}

case_exprs:
    | value COLON case_value SEMICOLON {true}
    | value COLON case_value SEMICOLON case_exprs {true}

case_value:
    | LCURLBRACK NAME RCURLBRACK {true}
    | TRUE {true}
    | FALSE {true}

value:
    | NAME {true}
    | TRUE {true}
    | FALSE {true}