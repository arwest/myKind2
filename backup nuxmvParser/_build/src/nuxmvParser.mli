
(* The type of tokens. *)

type token = 
  | Z
  | Y
  | XOR
  | XNOR
  | X
  | VAR
  | V
  | U
  | TRUE
  | TRANS
  | T
  | SEMICOLON
  | SELF
  | S
  | RSQBRACK
  | RPAREN
  | REAL
  | RCURLBRACK
  | RARROW
  | PLUS
  | PERIOD
  | OR
  | O
  | NOT
  | NEXT
  | NEQ
  | MUL
  | MODULE
  | MOD
  | MINUS
  | LTLSPEC
  | LTE
  | LT
  | LSQBRACK
  | LPAREN
  | LCURLBRACK
  | INT
  | INIT
  | ID of (string)
  | H
  | GTE
  | GT
  | G
  | FRACTIONAL
  | FALSE
  | F
  | ESAC
  | EQ
  | EOF
  | DPERIOD
  | DIV
  | DEFINE
  | DARROW
  | CREAL of (float)
  | COMMA
  | COLON
  | CINT of (int)
  | CASE
  | BOOL
  | ASSIGNMENT
  | ASSIGN
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (NuxmvAst.t)
