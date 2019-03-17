
module MenhirBasics = struct
  
  exception Error
  
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
    | ID of (
# 31 "src/nuxmvParser.mly"
       (string)
# 49 "src/nuxmvParser.ml"
  )
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
    | CREAL of (
# 33 "src/nuxmvParser.mly"
       (float)
# 68 "src/nuxmvParser.ml"
  )
    | COMMA
    | COLON
    | CINT of (
# 32 "src/nuxmvParser.mly"
       (int)
# 75 "src/nuxmvParser.ml"
  )
    | CASE
    | BOOL
    | ASSIGNMENT
    | ASSIGN
    | AND
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState217
  | MenhirState207
  | MenhirState201
  | MenhirState200
  | MenhirState199
  | MenhirState195
  | MenhirState194
  | MenhirState191
  | MenhirState188
  | MenhirState187
  | MenhirState183
  | MenhirState181
  | MenhirState179
  | MenhirState174
  | MenhirState172
  | MenhirState169
  | MenhirState168
  | MenhirState163
  | MenhirState162
  | MenhirState160
  | MenhirState158
  | MenhirState157
  | MenhirState154
  | MenhirState153
  | MenhirState150
  | MenhirState136
  | MenhirState135
  | MenhirState128
  | MenhirState126
  | MenhirState125
  | MenhirState123
  | MenhirState121
  | MenhirState120
  | MenhirState113
  | MenhirState111
  | MenhirState110
  | MenhirState108
  | MenhirState107
  | MenhirState104
  | MenhirState103
  | MenhirState102
  | MenhirState101
  | MenhirState100
  | MenhirState99
  | MenhirState98
  | MenhirState96
  | MenhirState95
  | MenhirState94
  | MenhirState93
  | MenhirState92
  | MenhirState91
  | MenhirState90
  | MenhirState89
  | MenhirState88
  | MenhirState87
  | MenhirState86
  | MenhirState85
  | MenhirState84
  | MenhirState83
  | MenhirState82
  | MenhirState81
  | MenhirState80
  | MenhirState79
  | MenhirState78
  | MenhirState77
  | MenhirState76
  | MenhirState75
  | MenhirState74
  | MenhirState73
  | MenhirState71
  | MenhirState69
  | MenhirState67
  | MenhirState66
  | MenhirState65
  | MenhirState64
  | MenhirState63
  | MenhirState57
  | MenhirState56
  | MenhirState55
  | MenhirState54
  | MenhirState50
  | MenhirState45
  | MenhirState39
  | MenhirState38
  | MenhirState36
  | MenhirState35
  | MenhirState34
  | MenhirState33
  | MenhirState31
  | MenhirState30
  | MenhirState27
  | MenhirState26
  | MenhirState25
  | MenhirState24
  | MenhirState20
  | MenhirState14
  | MenhirState10
  | MenhirState9
  | MenhirState5
  | MenhirState3
  | MenhirState0

# 18 "src/nuxmvParser.mly"
  

module A = NuxmvAst

let mk_pos = Position.create_position 

exception Ltl_Use_Error
  

# 210 "src/nuxmvParser.ml"

let rec _menhir_goto_nonempty_list_module_element_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (A.module_element list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (mel : (A.module_element list)) = _v in
        let _v : (A.module_element list) = 
# 73 "src/nuxmvParser.mly"
                                                 ( mel )
# 222 "src/nuxmvParser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (A.module_element list)) = _v in
        let _v : (A.module_element list option) = 
# 116 "/usr/local/bin/_opam/lib/menhir/standard.mly"
    ( Some x )
# 230 "src/nuxmvParser.ml"
         in
        _menhir_goto_option_module_body_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState207 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (A.module_element list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (A.module_element))) = _menhir_stack in
        let _v : (A.module_element list) = 
# 223 "/usr/local/bin/_opam/lib/menhir/standard.mly"
    ( x :: xs )
# 241 "src/nuxmvParser.ml"
         in
        _menhir_goto_nonempty_list_module_element_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_array_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (A.expr_type list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RSQBRACK ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, (al : (A.expr_type list))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (A.expr_type) = let _startpos = _startpos__1_ in
            
# 225 "src/nuxmvParser.mly"
                                                                         ( A.ArrayExpr (mk_pos _startpos, al) )
# 267 "src/nuxmvParser.ml"
             in
            _menhir_goto_array_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState172 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (A.expr_type))), _, (xs : (A.expr_type list))) = _menhir_stack in
        let _2 = () in
        let _v : (A.expr_type list) = 
# 243 "/usr/local/bin/_opam/lib/menhir/standard.mly"
    ( x :: xs )
# 284 "src/nuxmvParser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_array_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_module_element : _menhir_env -> 'ttv_tail -> _menhir_state -> (A.module_element) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASSIGN ->
        _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState207 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DEFINE ->
        _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState207 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LTLSPEC ->
        _menhir_run157 _menhir_env (Obj.magic _menhir_stack) MenhirState207 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRANS ->
        _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState207 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | VAR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState207 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EOF | MODULE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (A.module_element))) = _menhir_stack in
        let _v : (A.module_element list) = 
# 221 "/usr/local/bin/_opam/lib/menhir/standard.mly"
    ( [ x ] )
# 313 "src/nuxmvParser.ml"
         in
        _menhir_goto_nonempty_list_module_element_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState207

and _menhir_goto_nonempty_list_assign_element_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (A.assign_const list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState181 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (ael : (A.assign_const list)) = _v in
        let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _1 = () in
        let _v : (A.module_element) = let _startpos = _startpos__1_ in
        
# 125 "src/nuxmvParser.mly"
                                                              ( A.AssignConst (mk_pos _startpos, ael) )
# 334 "src/nuxmvParser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (ac : (A.module_element)) = _v in
        let _v : (A.module_element) = 
# 78 "src/nuxmvParser.mly"
                          ( ac )
# 342 "src/nuxmvParser.ml"
         in
        _menhir_goto_module_element _menhir_env _menhir_stack _menhir_s _v
    | MenhirState201 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (A.assign_const list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (A.assign_const))) = _menhir_stack in
        let _v : (A.assign_const list) = 
# 223 "/usr/local/bin/_opam/lib/menhir/standard.mly"
    ( x :: xs )
# 353 "src/nuxmvParser.ml"
         in
        _menhir_goto_nonempty_list_assign_element_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_nonempty_list_define_element_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (A.define_element list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState160 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (del : (A.define_element list)) = _v in
        let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _1 = () in
        let _v : (A.module_element) = let _startpos = _startpos__1_ in
        
# 116 "src/nuxmvParser.mly"
                                                               ( A.DefineDecl (mk_pos _startpos, del))
# 372 "src/nuxmvParser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (dd : (A.module_element)) = _v in
        let _v : (A.module_element) = 
# 77 "src/nuxmvParser.mly"
                           ( dd )
# 380 "src/nuxmvParser.ml"
         in
        _menhir_goto_module_element _menhir_env _menhir_stack _menhir_s _v
    | MenhirState179 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (A.define_element list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (A.define_element))) = _menhir_stack in
        let _v : (A.define_element list) = 
# 223 "/usr/local/bin/_opam/lib/menhir/standard.mly"
    ( x :: xs )
# 391 "src/nuxmvParser.ml"
         in
        _menhir_goto_nonempty_list_define_element_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_array_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (A.expr_type) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState172 | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LSQBRACK ->
                _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState172)
        | RSQBRACK ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (A.expr_type))) = _menhir_stack in
            let _v : (A.expr_type list) = 
# 241 "/usr/local/bin/_opam/lib/menhir/standard.mly"
    ( [ x ] )
# 423 "src/nuxmvParser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_array_expr_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState162 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (i : (
# 31 "src/nuxmvParser.mly"
       (string)
# 444 "src/nuxmvParser.ml"
            )), _startpos_i_), _, (ae : (A.expr_type))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (A.define_element) = let _startpos = _startpos_i_ in
            
# 121 "src/nuxmvParser.mly"
                                                  ( A.ArrayDef (mk_pos _startpos, i, ae) )
# 452 "src/nuxmvParser.ml"
             in
            _menhir_goto_define_element _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_SEMICOLON_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState154 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_3 : (unit option)) = _v in
        let ((_menhir_stack, _menhir_s, _startpos__1_), _, (b : (A.nuxmv_expr)), _startpos_b_) = _menhir_stack in
        let _1 = () in
        let _v : (A.module_element) = let e =
          let _startpos = _startpos_b_ in
          
# 159 "src/nuxmvParser.mly"
               ( A.NextExpr(mk_pos _startpos, b) )
# 478 "src/nuxmvParser.ml"
          
        in
        let _startpos = _startpos__1_ in
        
# 142 "src/nuxmvParser.mly"
                                                        ( A.TransConst (mk_pos _startpos, e) )
# 485 "src/nuxmvParser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (tc : (A.module_element)) = _v in
        let _v : (A.module_element) = 
# 79 "src/nuxmvParser.mly"
                         ( tc )
# 493 "src/nuxmvParser.ml"
         in
        _menhir_goto_module_element _menhir_env _menhir_stack _menhir_s _v
    | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_3 : (unit option)) = _v in
        let ((_menhir_stack, _menhir_s, _startpos__1_), _, (e : (A.nuxmv_expr)), _startpos_e_) = _menhir_stack in
        let _1 = () in
        let _v : (A.module_element) = let le =
          let _startpos = _startpos_e_ in
          
# 151 "src/nuxmvParser.mly"
               ( A.LtlExpr(mk_pos _startpos, e) )
# 507 "src/nuxmvParser.ml"
          
        in
        let _startpos = _startpos__1_ in
        
# 146 "src/nuxmvParser.mly"
                                                           ( A.LtlSpec (mk_pos _startpos, le) )
# 514 "src/nuxmvParser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (ltls : (A.module_element)) = _v in
        let _v : (A.module_element) = 
# 80 "src/nuxmvParser.mly"
                            ( ltls )
# 522 "src/nuxmvParser.ml"
         in
        _menhir_goto_module_element _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_enum_type_value_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (A.enum_type_value list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RCURLBRACK ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _startpos__1_), _, (etvl : (A.enum_type_value list))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (A.simple_type_spec) = let _startpos = _startpos__1_ in
            
# 96 "src/nuxmvParser.mly"
                                                                                   ( A.EnumType (mk_pos _startpos, etvl) )
# 548 "src/nuxmvParser.ml"
             in
            _menhir_goto_simple_type_specifier _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (A.enum_type_value))), _, (xs : (A.enum_type_value list))) = _menhir_stack in
        let _2 = () in
        let _v : (A.enum_type_value list) = 
# 243 "/usr/local/bin/_opam/lib/menhir/standard.mly"
    ( x :: xs )
# 565 "src/nuxmvParser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_enum_type_value_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_nonempty_list_state_var_element_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (A.state_var_decl list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (A.state_var_decl list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (A.state_var_decl))) = _menhir_stack in
        let _v : (A.state_var_decl list) = 
# 223 "/usr/local/bin/_opam/lib/menhir/standard.mly"
    ( x :: xs )
# 582 "src/nuxmvParser.ml"
         in
        _menhir_goto_nonempty_list_state_var_element_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (sve : (A.state_var_decl list)) = _v in
        let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _1 = () in
        let _v : (A.module_element) = let _startpos = _startpos__1_ in
        
# 84 "src/nuxmvParser.mly"
                                                                  ( A.StateVarDecl (mk_pos _startpos, sve) )
# 595 "src/nuxmvParser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (svd : (A.module_element)) = _v in
        let _v : (A.module_element) = 
# 76 "src/nuxmvParser.mly"
                               ( svd )
# 603 "src/nuxmvParser.ml"
         in
        _menhir_goto_module_element _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_assign_element : _menhir_env -> 'ttv_tail -> _menhir_state -> (A.assign_const) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run184 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INIT ->
        _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run182 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ASSIGN | DEFINE | EOF | LTLSPEC | MODULE | TRANS | VAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (A.assign_const))) = _menhir_stack in
        let _v : (A.assign_const list) = 
# 221 "/usr/local/bin/_opam/lib/menhir/standard.mly"
    ( [ x ] )
# 630 "src/nuxmvParser.ml"
         in
        _menhir_goto_nonempty_list_assign_element_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState201

and _menhir_goto_define_element : _menhir_env -> 'ttv_tail -> _menhir_state -> (A.define_element) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ASSIGN | DEFINE | EOF | LTLSPEC | MODULE | TRANS | VAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (A.define_element))) = _menhir_stack in
        let _v : (A.define_element list) = 
# 221 "/usr/local/bin/_opam/lib/menhir/standard.mly"
    ( [ x ] )
# 653 "src/nuxmvParser.ml"
         in
        _menhir_goto_nonempty_list_define_element_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState179

and _menhir_goto_separated_nonempty_list_COMMA_next_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (A.expr_type list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RSQBRACK ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, (el : (A.expr_type list))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (A.expr_type) = let _startpos = _startpos__1_ in
            
# 224 "src/nuxmvParser.mly"
                                                                       ( A.ArrayExpr (mk_pos _startpos, el) )
# 681 "src/nuxmvParser.ml"
             in
            _menhir_goto_array_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState169 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (b : (A.nuxmv_expr)), _startpos_b_), _), _, (xs : (A.expr_type list))) = _menhir_stack in
        let _2 = () in
        let _v : (A.expr_type list) = let x =
          let _startpos = _startpos_b_ in
          
# 159 "src/nuxmvParser.mly"
               ( A.NextExpr(mk_pos _startpos, b) )
# 700 "src/nuxmvParser.ml"
          
        in
        
# 243 "/usr/local/bin/_opam/lib/menhir/standard.mly"
    ( x :: xs )
# 706 "src/nuxmvParser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_next_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce88 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit option) = 
# 114 "/usr/local/bin/_opam/lib/menhir/standard.mly"
    ( None )
# 717 "src/nuxmvParser.ml"
     in
    _menhir_goto_option_SEMICOLON_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run155 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = () in
    let _v : (unit option) = 
# 116 "/usr/local/bin/_opam/lib/menhir/standard.mly"
    ( Some x )
# 729 "src/nuxmvParser.ml"
     in
    _menhir_goto_option_SEMICOLON_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_separated_nonempty_list_COMMA_simple_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (A.expr_type list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (el : (A.expr_type list)) = _v in
        let _v : (A.expr_type list) = 
# 220 "src/nuxmvParser.mly"
                                                       ( el )
# 743 "src/nuxmvParser.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _startpos__1_), _, (il : (A.expr_type list))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (A.expr_type list) = 
# 112 "src/nuxmvParser.mly"
                                         ( il )
# 760 "src/nuxmvParser.ml"
             in
            _menhir_goto_module_type_spec_param_list _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (A.expr_type list)) = _v in
        let ((_menhir_stack, _menhir_s, (b : (A.nuxmv_expr)), _startpos_b_), _) = _menhir_stack in
        let _2 = () in
        let _v : (A.expr_type list) = let x =
          let _startpos = _startpos_b_ in
          
# 155 "src/nuxmvParser.mly"
               ( A.SimpleExpr(mk_pos _startpos, b) )
# 780 "src/nuxmvParser.ml"
          
        in
        
# 243 "/usr/local/bin/_opam/lib/menhir/standard.mly"
    ( x :: xs )
# 786 "src/nuxmvParser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_simple_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (A.nuxmv_expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RCURLBRACK ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, (el : (A.nuxmv_expr list))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos__1_ in
            
# 186 "src/nuxmvParser.mly"
                                                                      ( A.SetExp (mk_pos _startpos, el) )
# 813 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (x : (A.nuxmv_expr)), _startpos_x_), _), _, (xs : (A.nuxmv_expr list))) = _menhir_stack in
        let _2 = () in
        let _v : (A.nuxmv_expr list) = 
# 243 "/usr/local/bin/_opam/lib/menhir/standard.mly"
    ( x :: xs )
# 830 "src/nuxmvParser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_nonempty_list_case_element_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((A.nuxmv_expr * A.nuxmv_expr) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ESAC ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, (cel : ((A.nuxmv_expr * A.nuxmv_expr) list))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos__1_ in
            
# 187 "src/nuxmvParser.mly"
                                                  ( A.CaseExp (mk_pos _startpos, cel) )
# 857 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (A.nuxmv_expr * A.nuxmv_expr))), _, (xs : ((A.nuxmv_expr * A.nuxmv_expr) list))) = _menhir_stack in
        let _v : ((A.nuxmv_expr * A.nuxmv_expr) list) = 
# 223 "/usr/local/bin/_opam/lib/menhir/standard.mly"
    ( x :: xs )
# 873 "src/nuxmvParser.ml"
         in
        _menhir_goto_nonempty_list_case_element_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_function_call_params : _menhir_env -> 'ttv_tail -> _menhir_state -> (A.nuxmv_expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (ci : (A.comp_ident)), _startpos_ci_), _startpos__2_), _, (el : (A.nuxmv_expr list))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _startpos = _startpos_ci_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos_ci_ in
            
# 211 "src/nuxmvParser.mly"
                                                                       ( A.Call (mk_pos _startpos, ci, el) )
# 900 "src/nuxmvParser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (f : (A.nuxmv_expr)) = _v in
            let _startpos_f_ = _startpos in
            let _startpos = _startpos_f_ in
            let _v : (A.nuxmv_expr) = 
# 165 "src/nuxmvParser.mly"
                        ( f )
# 910 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (e : (A.nuxmv_expr)), _startpos_e_), _), _, (p : (A.nuxmv_expr list))) = _menhir_stack in
        let _2 = () in
        let _v : (A.nuxmv_expr list) = 
# 216 "src/nuxmvParser.mly"
                                               (e :: p)
# 927 "src/nuxmvParser.ml"
         in
        _menhir_goto_function_call_params _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run55 : _menhir_env -> 'ttv_tail * _menhir_state * (A.nuxmv_expr) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55

and _menhir_run65 : _menhir_env -> 'ttv_tail * _menhir_state * (A.nuxmv_expr) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65

and _menhir_run57 : _menhir_env -> 'ttv_tail * _menhir_state * (A.nuxmv_expr) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57

and _menhir_run67 : _menhir_env -> 'ttv_tail * _menhir_state * (A.nuxmv_expr) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_run69 : _menhir_env -> 'ttv_tail * _menhir_state * (A.nuxmv_expr) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69

and _menhir_run71 : _menhir_env -> 'ttv_tail * _menhir_state * (A.nuxmv_expr) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71

and _menhir_run98 : _menhir_env -> 'ttv_tail * _menhir_state * (A.nuxmv_expr) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98

and _menhir_run73 : _menhir_env -> 'ttv_tail * _menhir_state * (A.nuxmv_expr) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_run100 : _menhir_env -> 'ttv_tail * _menhir_state * (A.nuxmv_expr) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100

and _menhir_run81 : _menhir_env -> 'ttv_tail * _menhir_state * (A.nuxmv_expr) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81

and _menhir_run75 : _menhir_env -> 'ttv_tail * _menhir_state * (A.nuxmv_expr) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75

and _menhir_run77 : _menhir_env -> 'ttv_tail * _menhir_state * (A.nuxmv_expr) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_run83 : _menhir_env -> 'ttv_tail * _menhir_state * (A.nuxmv_expr) * Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83

and _menhir_run85 : _menhir_env -> 'ttv_tail * _menhir_state * (A.nuxmv_expr) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85

and _menhir_run87 : _menhir_env -> 'ttv_tail * _menhir_state * (A.nuxmv_expr) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87

and _menhir_run89 : _menhir_env -> 'ttv_tail * _menhir_state * (A.nuxmv_expr) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89

and _menhir_run91 : _menhir_env -> 'ttv_tail * _menhir_state * (A.nuxmv_expr) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91

and _menhir_run93 : _menhir_env -> 'ttv_tail * _menhir_state * (A.nuxmv_expr) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93

and _menhir_run79 : _menhir_env -> 'ttv_tail * _menhir_state * (A.nuxmv_expr) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79

and _menhir_run102 : _menhir_env -> 'ttv_tail * _menhir_state * (A.nuxmv_expr) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102

and _menhir_run95 : _menhir_env -> 'ttv_tail * _menhir_state * (A.nuxmv_expr) * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95

and _menhir_goto_enum_type_value : _menhir_env -> 'ttv_tail -> _menhir_state -> (A.enum_type_value) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CINT _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ID _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20)
    | RCURLBRACK ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (A.enum_type_value))) = _menhir_stack in
        let _v : (A.enum_type_value list) = 
# 241 "/usr/local/bin/_opam/lib/menhir/standard.mly"
    ( [ x ] )
# 2030 "src/nuxmvParser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_enum_type_value_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_state_var_element : _menhir_env -> 'ttv_tail -> _menhir_state -> (A.state_var_decl) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ASSIGN | DEFINE | EOF | LTLSPEC | MODULE | TRANS | VAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (A.state_var_decl))) = _menhir_stack in
        let _v : (A.state_var_decl list) = 
# 221 "/usr/local/bin/_opam/lib/menhir/standard.mly"
    ( [ x ] )
# 2055 "src/nuxmvParser.ml"
         in
        _menhir_goto_nonempty_list_state_var_element_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState150

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (A.nuxmv_expr) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState113 | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState54 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CASE ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CINT _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CREAL _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | F ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FALSE ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FRACTIONAL ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | G ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | H ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ID _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LCURLBRACK ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NEXT ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | O ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | SELF ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRUE ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | X ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Y ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Z ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110)
        | DARROW ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | EQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | GT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | GTE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | LT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | LTE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | MINUS ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | MUL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | NEQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | OR ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | RARROW ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | S ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | T ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | U ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | V ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | XNOR ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | XOR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54)
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | EQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | GT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | GTE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | LT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | LTE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | MINUS ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | MUL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | NEQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | S ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | T ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | U ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | V ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | ASSIGN | COLON | COMMA | DARROW | DEFINE | EOF | ID _ | INIT | LTLSPEC | MODULE | NEXT | OR | RARROW | RCURLBRACK | RPAREN | RSQBRACK | SELF | SEMICOLON | TRANS | VAR | XNOR | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (A.nuxmv_expr)), _startpos_e1_), _), _, (e2 : (A.nuxmv_expr)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos_e1_ in
            
# 169 "src/nuxmvParser.mly"
                              ( A.Xor (mk_pos _startpos, e1, e2) )
# 2215 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56)
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND | ASSIGN | COLON | COMMA | DARROW | DEFINE | DIV | EOF | EQ | GT | GTE | ID _ | INIT | LT | LTE | LTLSPEC | MINUS | MOD | MODULE | MUL | NEQ | NEXT | OR | PLUS | RARROW | RCURLBRACK | RPAREN | RSQBRACK | SELF | SEMICOLON | TRANS | VAR | XNOR | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (le1 : (A.nuxmv_expr)), _startpos_le1_), _), _, (le2 : (A.nuxmv_expr)), _startpos_le2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_le1_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos_le1_ in
            
# 196 "src/nuxmvParser.mly"
                              ( A.Releases (mk_pos _startpos, le1, le2) )
# 2236 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | DARROW ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | EQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | GT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | GTE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | LT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | LTE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | MINUS ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | MUL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | NEQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | OR ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | RARROW ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | RSQBRACK ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState64 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (ci : (A.comp_ident)), _startpos_ci_), _startpos__2_), _, (b : (A.nuxmv_expr)), _startpos_b_) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _startpos = _startpos_ci_ in
            let _v : (A.comp_ident) = let e =
              let _startpos = _startpos_b_ in
              
# 155 "src/nuxmvParser.mly"
               ( A.SimpleExpr(mk_pos _startpos, b) )
# 2294 "src/nuxmvParser.ml"
              
            in
            let _startpos = _startpos_ci_ in
            
# 137 "src/nuxmvParser.mly"
                                                                 ( BrackIdent (mk_pos _startpos, ci, e) )
# 2301 "src/nuxmvParser.ml"
             in
            _menhir_goto_complex_indentifier _menhir_env _menhir_stack _menhir_s _v _startpos
        | S ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | T ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | U ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | V ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | XNOR ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | XOR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64)
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | EQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | GT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | GTE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | LT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | LTE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | MINUS ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | MUL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | NEQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | S ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | T ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | U ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | V ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | ASSIGN | COLON | COMMA | DARROW | DEFINE | EOF | ID _ | INIT | LTLSPEC | MODULE | NEXT | OR | RARROW | RCURLBRACK | RPAREN | RSQBRACK | SELF | SEMICOLON | TRANS | VAR | XNOR | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (A.nuxmv_expr)), _startpos_e1_), _), _, (e2 : (A.nuxmv_expr)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos_e1_ in
            
# 170 "src/nuxmvParser.mly"
                               ( A.Xnor (mk_pos _startpos, e1, e2) )
# 2366 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND | ASSIGN | COLON | COMMA | DARROW | DEFINE | DIV | EOF | EQ | GT | GTE | ID _ | INIT | LT | LTE | LTLSPEC | MINUS | MOD | MODULE | MUL | NEQ | NEXT | OR | PLUS | RARROW | RCURLBRACK | RPAREN | RSQBRACK | SELF | SEMICOLON | TRANS | VAR | XNOR | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (le1 : (A.nuxmv_expr)), _startpos_le1_), _), _, (le2 : (A.nuxmv_expr)), _startpos_le2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_le1_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos_le1_ in
            
# 195 "src/nuxmvParser.mly"
                              ( A.Until (mk_pos _startpos, le1, le2) )
# 2387 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND | ASSIGN | COLON | COMMA | DARROW | DEFINE | DIV | EOF | EQ | GT | GTE | ID _ | INIT | LT | LTE | LTLSPEC | MINUS | MOD | MODULE | MUL | NEQ | NEXT | OR | PLUS | RARROW | RCURLBRACK | RPAREN | RSQBRACK | SELF | SEMICOLON | TRANS | VAR | XNOR | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (le1 : (A.nuxmv_expr)), _startpos_le1_), _), _, (le2 : (A.nuxmv_expr)), _startpos_le2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_le1_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos_le1_ in
            
# 203 "src/nuxmvParser.mly"
                              ( A.Triggered (mk_pos _startpos, le1, le2) )
# 2410 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND | ASSIGN | COLON | COMMA | DARROW | DEFINE | DIV | EOF | EQ | GT | GTE | ID _ | INIT | LT | LTE | LTLSPEC | MINUS | MOD | MODULE | MUL | NEQ | NEXT | OR | PLUS | RARROW | RCURLBRACK | RPAREN | RSQBRACK | SELF | SEMICOLON | TRANS | VAR | XNOR | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (le1 : (A.nuxmv_expr)), _startpos_le1_), _), _, (le2 : (A.nuxmv_expr)), _startpos_le2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_le1_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos_le1_ in
            
# 202 "src/nuxmvParser.mly"
                              ( A.Since (mk_pos _startpos, le1, le2) )
# 2433 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | MOD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | MUL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | S ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | T ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | U ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | V ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | AND | ASSIGN | COLON | COMMA | DARROW | DEFINE | EOF | EQ | GT | GTE | ID _ | INIT | LT | LTE | LTLSPEC | MINUS | MODULE | NEQ | NEXT | OR | PLUS | RARROW | RCURLBRACK | RPAREN | RSQBRACK | SELF | SEMICOLON | TRANS | VAR | XNOR | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (A.nuxmv_expr)), _startpos_e1_), _), _, (e2 : (A.nuxmv_expr)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos_e1_ in
            
# 180 "src/nuxmvParser.mly"
                               ( A.Plus (mk_pos _startpos, e1, e2))
# 2470 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74)
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | S ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | T ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | U ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | V ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | AND | ASSIGN | COLON | COMMA | DARROW | DEFINE | DIV | EOF | EQ | GT | GTE | ID _ | INIT | LT | LTE | LTLSPEC | MINUS | MOD | MODULE | MUL | NEQ | NEXT | OR | PLUS | RARROW | RCURLBRACK | RPAREN | RSQBRACK | SELF | SEMICOLON | TRANS | VAR | XNOR | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (A.nuxmv_expr)), _startpos_e1_), _), _, (e2 : (A.nuxmv_expr)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos_e1_ in
            
# 182 "src/nuxmvParser.mly"
                              ( A.Multiply (mk_pos _startpos, e1, e2))
# 2499 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76)
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | S ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | T ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | U ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | V ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | AND | ASSIGN | COLON | COMMA | DARROW | DEFINE | DIV | EOF | EQ | GT | GTE | ID _ | INIT | LT | LTE | LTLSPEC | MINUS | MOD | MODULE | MUL | NEQ | NEXT | OR | PLUS | RARROW | RCURLBRACK | RPAREN | RSQBRACK | SELF | SEMICOLON | TRANS | VAR | XNOR | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (A.nuxmv_expr)), _startpos_e1_), _), _, (e2 : (A.nuxmv_expr)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos_e1_ in
            
# 184 "src/nuxmvParser.mly"
                              ( A.Mod (mk_pos _startpos, e1, e2) )
# 2528 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78)
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | S ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | T ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | U ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | V ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | AND | ASSIGN | COLON | COMMA | DARROW | DEFINE | DIV | EOF | EQ | GT | GTE | ID _ | INIT | LT | LTE | LTLSPEC | MINUS | MOD | MODULE | MUL | NEQ | NEXT | OR | PLUS | RARROW | RCURLBRACK | RPAREN | RSQBRACK | SELF | SEMICOLON | TRANS | VAR | XNOR | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (A.nuxmv_expr)), _startpos_e1_), _), _, (e2 : (A.nuxmv_expr)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos_e1_ in
            
# 183 "src/nuxmvParser.mly"
                              ( A.Divide (mk_pos _startpos, e1, e2) )
# 2557 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | MINUS ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | MUL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | S ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | T ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | U ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | V ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | AND | ASSIGN | COLON | COMMA | DARROW | DEFINE | EOF | EQ | GT | GTE | ID _ | INIT | LT | LTE | LTLSPEC | MODULE | NEQ | NEXT | OR | RARROW | RCURLBRACK | RPAREN | RSQBRACK | SELF | SEMICOLON | TRANS | VAR | XNOR | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (A.nuxmv_expr)), _startpos_e1_), _), _, (e2 : (A.nuxmv_expr)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos_e1_ in
            
# 174 "src/nuxmvParser.mly"
                              ( A.NotEq (mk_pos _startpos, e1, e2) )
# 2596 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82)
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | MOD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | MUL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | S ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | T ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | U ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | V ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | AND | ASSIGN | COLON | COMMA | DARROW | DEFINE | EOF | EQ | GT | GTE | ID _ | INIT | LT | LTE | LTLSPEC | MINUS | MODULE | NEQ | NEXT | OR | PLUS | RARROW | RCURLBRACK | RPAREN | RSQBRACK | SELF | SEMICOLON | TRANS | VAR | XNOR | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (A.nuxmv_expr)), _startpos_e1_), _, _startpos__2_), _, (e2 : (A.nuxmv_expr)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos_e1_ in
            
# 181 "src/nuxmvParser.mly"
                                ( A.Minus (mk_pos _startpos, e1, e2) )
# 2631 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84)
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | MINUS ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | MUL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | S ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | T ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | U ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | V ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | AND | ASSIGN | COLON | COMMA | DARROW | DEFINE | EOF | EQ | GT | GTE | ID _ | INIT | LT | LTE | LTLSPEC | MODULE | NEQ | NEXT | OR | RARROW | RCURLBRACK | RPAREN | RSQBRACK | SELF | SEMICOLON | TRANS | VAR | XNOR | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (A.nuxmv_expr)), _startpos_e1_), _), _, (e2 : (A.nuxmv_expr)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos_e1_ in
            
# 177 "src/nuxmvParser.mly"
                              ( A.Lte (mk_pos _startpos, e1, e2) )
# 2670 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86)
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | MINUS ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | MUL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | S ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | T ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | U ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | V ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | AND | ASSIGN | COLON | COMMA | DARROW | DEFINE | EOF | EQ | GT | GTE | ID _ | INIT | LT | LTE | LTLSPEC | MODULE | NEQ | NEXT | OR | RARROW | RCURLBRACK | RPAREN | RSQBRACK | SELF | SEMICOLON | TRANS | VAR | XNOR | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (A.nuxmv_expr)), _startpos_e1_), _), _, (e2 : (A.nuxmv_expr)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos_e1_ in
            
# 175 "src/nuxmvParser.mly"
                             ( A.Lt (mk_pos _startpos, e1, e2) )
# 2709 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88)
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | MINUS ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | MUL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | S ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | T ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | U ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | V ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | AND | ASSIGN | COLON | COMMA | DARROW | DEFINE | EOF | EQ | GT | GTE | ID _ | INIT | LT | LTE | LTLSPEC | MODULE | NEQ | NEXT | OR | RARROW | RCURLBRACK | RPAREN | RSQBRACK | SELF | SEMICOLON | TRANS | VAR | XNOR | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (A.nuxmv_expr)), _startpos_e1_), _), _, (e2 : (A.nuxmv_expr)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos_e1_ in
            
# 178 "src/nuxmvParser.mly"
                              ( A.Gte (mk_pos _startpos, e1, e2) )
# 2748 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90)
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | MINUS ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | MUL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | S ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | T ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | U ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | V ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | AND | ASSIGN | COLON | COMMA | DARROW | DEFINE | EOF | EQ | GT | GTE | ID _ | INIT | LT | LTE | LTLSPEC | MODULE | NEQ | NEXT | OR | RARROW | RCURLBRACK | RPAREN | RSQBRACK | SELF | SEMICOLON | TRANS | VAR | XNOR | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (A.nuxmv_expr)), _startpos_e1_), _), _, (e2 : (A.nuxmv_expr)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos_e1_ in
            
# 176 "src/nuxmvParser.mly"
                             ( A.Gt (mk_pos _startpos, e1, e2) )
# 2787 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | MINUS ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | MUL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | S ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | T ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | U ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | V ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | AND | ASSIGN | COLON | COMMA | DARROW | DEFINE | EOF | EQ | GT | GTE | ID _ | INIT | LT | LTE | LTLSPEC | MODULE | NEQ | NEXT | OR | RARROW | RCURLBRACK | RPAREN | RSQBRACK | SELF | SEMICOLON | TRANS | VAR | XNOR | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (A.nuxmv_expr)), _startpos_e1_), _), _, (e2 : (A.nuxmv_expr)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos_e1_ in
            
# 173 "src/nuxmvParser.mly"
                             ( A.Eq (mk_pos _startpos, e1, e2))
# 2826 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | EQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | GT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | GTE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | LT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | LTE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | MINUS ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | MUL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | NEQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | S ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | T ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | U ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | V ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | AND | ASSIGN | COLON | COMMA | DARROW | DEFINE | EOF | ID _ | INIT | LTLSPEC | MODULE | NEXT | OR | RARROW | RCURLBRACK | RPAREN | RSQBRACK | SELF | SEMICOLON | TRANS | VAR | XNOR | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (A.nuxmv_expr)), _startpos_e1_), _), _, (e2 : (A.nuxmv_expr)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos_e1_ in
            
# 167 "src/nuxmvParser.mly"
                              ( A.And (mk_pos _startpos, e1, e2) )
# 2877 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96)
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | DARROW ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | EQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | GT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | GTE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | LT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | LTE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | MINUS ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | MUL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | NEQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | OR ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | RARROW ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | S ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | T ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | U ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | V ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | XNOR ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | XOR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | ASSIGN | COLON | COMMA | DEFINE | EOF | ID _ | INIT | LTLSPEC | MODULE | NEXT | RCURLBRACK | RPAREN | RSQBRACK | SELF | SEMICOLON | TRANS | VAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (A.nuxmv_expr)), _startpos_e1_), _), _, (e2 : (A.nuxmv_expr)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos_e1_ in
            
# 171 "src/nuxmvParser.mly"
                                 ( A.Impl (mk_pos _startpos, e1, e2) )
# 2940 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99)
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | EQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | GT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | GTE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | LT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | LTE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | MINUS ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | MUL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | NEQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | S ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | T ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | U ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | V ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | ASSIGN | COLON | COMMA | DARROW | DEFINE | EOF | ID _ | INIT | LTLSPEC | MODULE | NEXT | OR | RARROW | RCURLBRACK | RPAREN | RSQBRACK | SELF | SEMICOLON | TRANS | VAR | XNOR | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (A.nuxmv_expr)), _startpos_e1_), _), _, (e2 : (A.nuxmv_expr)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos_e1_ in
            
# 168 "src/nuxmvParser.mly"
                             ( A.Or (mk_pos _startpos, e1, e2) )
# 2993 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101)
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | EQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | GT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | GTE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | LT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | LTE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | MINUS ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | MUL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | NEQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | OR ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | S ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | T ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | U ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | V ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | XNOR ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | XOR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | ASSIGN | COLON | COMMA | DARROW | DEFINE | EOF | ID _ | INIT | LTLSPEC | MODULE | NEXT | RARROW | RCURLBRACK | RPAREN | RSQBRACK | SELF | SEMICOLON | TRANS | VAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (A.nuxmv_expr)), _startpos_e1_), _), _, (e2 : (A.nuxmv_expr)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos_e1_ in
            
# 172 "src/nuxmvParser.mly"
                                 ( A.Equiv (mk_pos _startpos, e1, e2) )
# 3052 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103)
    | MenhirState108 | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState107 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CASE ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CINT _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CREAL _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | F ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FALSE ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FRACTIONAL ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | G ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | H ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ID _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LCURLBRACK ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NEXT ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | O ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | SELF ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRUE ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | X ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Y ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Z ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
        | DARROW ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | EQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | GT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | GTE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | LT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | LTE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | MINUS ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | MUL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | NEQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | OR ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | RARROW ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | S ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | T ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | U ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | V ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | XNOR ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | XOR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (A.nuxmv_expr)), _startpos_e_) = _menhir_stack in
            let _v : (A.nuxmv_expr list) = 
# 215 "src/nuxmvParser.mly"
               ( [e] )
# 3163 "src/nuxmvParser.ml"
             in
            _menhir_goto_function_call_params _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107)
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | DARROW ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | EQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | GT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | GTE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | LT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | LTE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | MINUS ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | MUL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | NEQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | OR ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | RARROW ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | S ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState111 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (A.nuxmv_expr)), _startpos_e1_), _), _, (e2 : (A.nuxmv_expr)), _startpos_e2_) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (A.nuxmv_expr * A.nuxmv_expr) = 
# 207 "src/nuxmvParser.mly"
                                          ( (e1, e2) )
# 3218 "src/nuxmvParser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CASE ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CINT _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CREAL _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | F ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FALSE ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FRACTIONAL ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | G ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | H ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ID _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LCURLBRACK ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NEXT ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | O ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | SELF ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRUE ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | X ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Y ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Z ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ESAC ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (A.nuxmv_expr * A.nuxmv_expr))) = _menhir_stack in
                let _v : ((A.nuxmv_expr * A.nuxmv_expr) list) = 
# 221 "/usr/local/bin/_opam/lib/menhir/standard.mly"
    ( [ x ] )
# 3271 "src/nuxmvParser.ml"
                 in
                _menhir_goto_nonempty_list_case_element_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113)
        | T ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | U ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | V ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | XNOR ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | XOR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111)
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND | ASSIGN | COLON | COMMA | DARROW | DEFINE | DIV | EOF | EQ | GT | GTE | ID _ | INIT | LT | LTE | LTLSPEC | MINUS | MOD | MODULE | MUL | NEQ | NEXT | OR | PLUS | RARROW | RCURLBRACK | RPAREN | RSQBRACK | SELF | SEMICOLON | TRANS | VAR | XNOR | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, (le : (A.nuxmv_expr)), _startpos_le_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos__1_ in
            
# 194 "src/nuxmvParser.mly"
                  ( A.Finally (mk_pos _startpos, le) )
# 3306 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND | ASSIGN | COLON | COMMA | DARROW | DEFINE | DIV | EOF | EQ | GT | GTE | ID _ | INIT | LT | LTE | LTLSPEC | MINUS | MOD | MODULE | MUL | NEQ | NEXT | OR | PLUS | RARROW | RCURLBRACK | RPAREN | RSQBRACK | SELF | SEMICOLON | TRANS | VAR | XNOR | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, (le : (A.nuxmv_expr)), _startpos_le_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos__1_ in
            
# 193 "src/nuxmvParser.mly"
                  ( A.Globally (mk_pos _startpos, le) )
# 3329 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND | ASSIGN | COLON | COMMA | DARROW | DEFINE | DIV | EOF | EQ | GT | GTE | ID _ | INIT | LT | LTE | LTLSPEC | MINUS | MOD | MODULE | MUL | NEQ | NEXT | OR | PLUS | RARROW | RCURLBRACK | RPAREN | RSQBRACK | SELF | SEMICOLON | TRANS | VAR | XNOR | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, (le : (A.nuxmv_expr)), _startpos_le_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos__1_ in
            
# 200 "src/nuxmvParser.mly"
                  ( A.Historically (mk_pos _startpos, le) )
# 3352 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState121 | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState120 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CASE ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CINT _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CREAL _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | F ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FALSE ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FRACTIONAL ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | G ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | H ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ID _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LCURLBRACK ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NEXT ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | O ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | SELF ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRUE ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | X ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Y ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Z ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121)
        | DARROW ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | EQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | GT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | GTE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | LT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | LTE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | MINUS ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | MUL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | NEQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | OR ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | RARROW ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | S ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | T ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | U ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | V ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | XNOR ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | XOR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | RCURLBRACK ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (A.nuxmv_expr)), _startpos_x_) = _menhir_stack in
            let _v : (A.nuxmv_expr list) = 
# 241 "/usr/local/bin/_opam/lib/menhir/standard.mly"
    ( [ x ] )
# 3465 "src/nuxmvParser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120)
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | DARROW ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | EQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | GT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | GTE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | LT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | LTE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | MINUS ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | MUL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | NEQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | OR ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | RARROW ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState123 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, (e : (A.nuxmv_expr)), _startpos_e_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (A.nuxmv_expr) = 
# 185 "src/nuxmvParser.mly"
                             ( e )
# 3519 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | S ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | T ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | U ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | V ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | XNOR ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | XOR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | MOD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | MUL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | S ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | T ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | U ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | V ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | AND | ASSIGN | COLON | COMMA | DARROW | DEFINE | EOF | EQ | GT | GTE | ID _ | INIT | LT | LTE | LTLSPEC | MINUS | MODULE | NEQ | NEXT | OR | PLUS | RARROW | RCURLBRACK | RPAREN | RSQBRACK | SELF | SEMICOLON | TRANS | VAR | XNOR | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, (e : (A.nuxmv_expr)), _startpos_e_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos__1_ in
            
# 179 "src/nuxmvParser.mly"
                     ( A.Uminus (mk_pos _startpos, e))
# 3566 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125)
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | DARROW ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | EQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | GT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | GTE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | LT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | LTE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | MINUS ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | MUL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | NEQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | OR ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | RARROW ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState126 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, (e : (A.nuxmv_expr)), _startpos_e_) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos__1_ in
            
# 188 "src/nuxmvParser.mly"
                                  ( A.NextExp (mk_pos _startpos, e))
# 3622 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | S ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | T ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | U ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | V ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | XNOR ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | XOR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | S ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | T ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | U ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | V ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | AND | ASSIGN | COLON | COMMA | DARROW | DEFINE | DIV | EOF | EQ | GT | GTE | ID _ | INIT | LT | LTE | LTLSPEC | MINUS | MOD | MODULE | MUL | NEQ | NEXT | OR | PLUS | RARROW | RCURLBRACK | RPAREN | RSQBRACK | SELF | SEMICOLON | TRANS | VAR | XNOR | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, (e : (A.nuxmv_expr)), _startpos_e_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos__1_ in
            
# 166 "src/nuxmvParser.mly"
                   ( A.Not (mk_pos _startpos, e) )
# 3663 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128)
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND | ASSIGN | COLON | COMMA | DARROW | DEFINE | DIV | EOF | EQ | GT | GTE | ID _ | INIT | LT | LTE | LTLSPEC | MINUS | MOD | MODULE | MUL | NEQ | NEXT | OR | PLUS | RARROW | RCURLBRACK | RPAREN | RSQBRACK | SELF | SEMICOLON | TRANS | VAR | XNOR | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, (le : (A.nuxmv_expr)), _startpos_le_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos__1_ in
            
# 201 "src/nuxmvParser.mly"
                  ( A.Once (mk_pos _startpos, le) )
# 3684 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND | ASSIGN | COLON | COMMA | DARROW | DEFINE | DIV | EOF | EQ | GT | GTE | ID _ | INIT | LT | LTE | LTLSPEC | MINUS | MOD | MODULE | MUL | NEQ | NEXT | OR | PLUS | RARROW | RCURLBRACK | RPAREN | RSQBRACK | SELF | SEMICOLON | TRANS | VAR | XNOR | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, (le : (A.nuxmv_expr)), _startpos_le_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos__1_ in
            
# 192 "src/nuxmvParser.mly"
                  ( A.NextState (mk_pos _startpos, le) )
# 3707 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND | ASSIGN | COLON | COMMA | DARROW | DEFINE | DIV | EOF | EQ | GT | GTE | ID _ | INIT | LT | LTE | LTLSPEC | MINUS | MOD | MODULE | MUL | NEQ | NEXT | OR | PLUS | RARROW | RCURLBRACK | RPAREN | RSQBRACK | SELF | SEMICOLON | TRANS | VAR | XNOR | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, (le : (A.nuxmv_expr)), _startpos_le_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos__1_ in
            
# 198 "src/nuxmvParser.mly"
                  ( A.PrevState (mk_pos _startpos, le) )
# 3730 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND | ASSIGN | COLON | COMMA | DARROW | DEFINE | DIV | EOF | EQ | GT | GTE | ID _ | INIT | LT | LTE | LTLSPEC | MINUS | MOD | MODULE | MUL | NEQ | NEXT | OR | PLUS | RARROW | RCURLBRACK | RPAREN | RSQBRACK | SELF | SEMICOLON | TRANS | VAR | XNOR | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, (le : (A.nuxmv_expr)), _startpos_le_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos__1_ in
            
# 199 "src/nuxmvParser.mly"
                  ( A.NotPrevStateNot (mk_pos _startpos, le) )
# 3753 "src/nuxmvParser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState136 | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState135 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CASE ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CINT _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CREAL _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | F ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FALSE ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FRACTIONAL ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | G ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | H ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ID _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LCURLBRACK ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NEXT ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | O ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | SELF ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRUE ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | X ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Y ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Z ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136)
        | DARROW ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | EQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | GT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | GTE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | LT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | LTE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | MINUS ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | MUL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | NEQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | OR ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | RARROW ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | S ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | T ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | U ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | V ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | XNOR ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | XOR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (b : (A.nuxmv_expr)), _startpos_b_) = _menhir_stack in
            let _v : (A.expr_type list) = let x =
              let _startpos = _startpos_b_ in
              
# 155 "src/nuxmvParser.mly"
               ( A.SimpleExpr(mk_pos _startpos, b) )
# 3868 "src/nuxmvParser.ml"
              
            in
            
# 241 "/usr/local/bin/_opam/lib/menhir/standard.mly"
    ( [ x ] )
# 3874 "src/nuxmvParser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_simple_expr_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135)
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | DARROW ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | EQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | GT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | GTE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | LT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | LTE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | MINUS ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | MUL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | NEQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | OR ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | RARROW ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | S ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | SEMICOLON ->
            _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | T ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | U ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | V ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | XNOR ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | XOR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | ASSIGN | DEFINE | EOF | LTLSPEC | MODULE | TRANS | VAR ->
            _menhir_reduce88 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState154)
    | MenhirState157 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | DARROW ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | EQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | GT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | GTE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | LT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | LTE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | MINUS ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | MUL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | NEQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | OR ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | RARROW ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | S ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | SEMICOLON ->
            _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | T ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | U ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | V ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | XNOR ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | XOR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | ASSIGN | DEFINE | EOF | LTLSPEC | MODULE | TRANS | VAR ->
            _menhir_reduce88 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState158)
    | MenhirState169 | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState168 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CASE ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CINT _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CREAL _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | F ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FALSE ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FRACTIONAL ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | G ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | H ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ID _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LCURLBRACK ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NEXT ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | O ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | SELF ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRUE ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | X ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Y ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Z ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState169)
        | DARROW ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | EQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | GT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | GTE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | LT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | LTE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | MINUS ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | MUL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | NEQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | OR ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | RARROW ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | S ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | T ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | U ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | V ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | XNOR ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | XOR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | RSQBRACK ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (b : (A.nuxmv_expr)), _startpos_b_) = _menhir_stack in
            let _v : (A.expr_type list) = let x =
              let _startpos = _startpos_b_ in
              
# 159 "src/nuxmvParser.mly"
               ( A.NextExpr(mk_pos _startpos, b) )
# 4097 "src/nuxmvParser.ml"
              
            in
            
# 241 "/usr/local/bin/_opam/lib/menhir/standard.mly"
    ( [ x ] )
# 4103 "src/nuxmvParser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_next_expr_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState168)
    | MenhirState162 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | DARROW ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | EQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | GT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | GTE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | LT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | LTE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | MINUS ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | MUL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | NEQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | OR ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | RARROW ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | S ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState174 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (i : (
# 31 "src/nuxmvParser.mly"
       (string)
# 4155 "src/nuxmvParser.ml"
            )), _startpos_i_), _, (b : (A.nuxmv_expr)), _startpos_b_) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (A.define_element) = let e =
              let _startpos = _startpos_b_ in
              
# 155 "src/nuxmvParser.mly"
               ( A.SimpleExpr(mk_pos _startpos, b) )
# 4164 "src/nuxmvParser.ml"
              
            in
            let _startpos = _startpos_i_ in
            
# 120 "src/nuxmvParser.mly"
                                                  ( A.SimpleDef (mk_pos _startpos, i, e) )
# 4171 "src/nuxmvParser.ml"
             in
            _menhir_goto_define_element _menhir_env _menhir_stack _menhir_s _v
        | T ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | U ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | V ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | XNOR ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | XOR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState174)
    | MenhirState187 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState188
        | DARROW ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState188
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState188
        | EQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState188
        | GT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState188
        | GTE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState188
        | LT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState188
        | LTE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState188
        | MINUS ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState188
        | MUL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState188
        | NEQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState188
        | OR ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState188
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState188
        | RARROW ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState188
        | S ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState188
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState188 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, (ci : (A.comp_ident)), _startpos_ci_), _, (b : (A.nuxmv_expr)), _startpos_b_) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (A.assign_const) = let e =
              let _startpos = _startpos_b_ in
              
# 155 "src/nuxmvParser.mly"
               ( A.SimpleExpr(mk_pos _startpos, b) )
# 4241 "src/nuxmvParser.ml"
              
            in
            let _startpos = _startpos__1_ in
            
# 130 "src/nuxmvParser.mly"
                                                                                       ( A.NextAssign (mk_pos _startpos, ci, e) )
# 4248 "src/nuxmvParser.ml"
             in
            _menhir_goto_assign_element _menhir_env _menhir_stack _menhir_s _v
        | T ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState188
        | U ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState188
        | V ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState188
        | XNOR ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState188
        | XOR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState188
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState188)
    | MenhirState194 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | DARROW ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | EQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | GT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | GTE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | LT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | LTE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | MINUS ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | MUL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | NEQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | OR ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | RARROW ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | S ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState195 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, (ci : (A.comp_ident)), _startpos_ci_), _, (b : (A.nuxmv_expr)), _startpos_b_) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (A.assign_const) = let e =
              let _startpos = _startpos_b_ in
              
# 155 "src/nuxmvParser.mly"
               ( A.SimpleExpr(mk_pos _startpos, b) )
# 4318 "src/nuxmvParser.ml"
              
            in
            let _startpos = _startpos__1_ in
            
# 129 "src/nuxmvParser.mly"
                                                                                       ( A.InitAssign (mk_pos _startpos, ci, e) )
# 4325 "src/nuxmvParser.ml"
             in
            _menhir_goto_assign_element _menhir_env _menhir_stack _menhir_s _v
        | T ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | U ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | V ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | XNOR ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | XOR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState195)
    | MenhirState199 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | DARROW ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | EQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | GT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | GTE ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | LT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | LTE ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | MINUS ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState200 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | MUL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | NEQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | OR ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | RARROW ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | S ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | T ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | U ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | V ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | XNOR ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | XOR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | ASSIGN | DEFINE | EOF | ID _ | INIT | LTLSPEC | MODULE | NEXT | SELF | TRANS | VAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (ci : (A.comp_ident)), _startpos_ci_), _, (b : (A.nuxmv_expr)), _startpos_b_) = _menhir_stack in
            let _2 = () in
            let _v : (A.assign_const) = let e =
              let _startpos = _startpos_b_ in
              
# 159 "src/nuxmvParser.mly"
               ( A.NextExpr(mk_pos _startpos, b) )
# 4398 "src/nuxmvParser.ml"
              
            in
            let _startpos = _startpos_ci_ in
            
# 131 "src/nuxmvParser.mly"
                                                        ( A.Assign (mk_pos _startpos, ci, e) )
# 4405 "src/nuxmvParser.ml"
             in
            _menhir_goto_assign_element _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState200)
    | _ ->
        _menhir_fail ()

and _menhir_run61 : _menhir_env -> 'ttv_tail * _menhir_state * (A.comp_ident) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (i : (
# 31 "src/nuxmvParser.mly"
       (string)
# 4428 "src/nuxmvParser.ml"
        )) = _v in
        let _startpos_i_ = _startpos in
        let (_menhir_stack, _menhir_s, (ci : (A.comp_ident)), _startpos_ci_) = _menhir_stack in
        let _2 = () in
        let _startpos = _startpos_ci_ in
        let _v : (A.comp_ident) = let _startpos = _startpos_ci_ in
        
# 136 "src/nuxmvParser.mly"
                                             ( A.PerIdent (mk_pos _startpos, ci, i) )
# 4438 "src/nuxmvParser.ml"
         in
        _menhir_goto_complex_indentifier _menhir_env _menhir_stack _menhir_s _v _startpos
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run63 : _menhir_env -> 'ttv_tail * _menhir_state * (A.comp_ident) * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 31 "src/nuxmvParser.mly"
       (string)
# 4502 "src/nuxmvParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (
# 31 "src/nuxmvParser.mly"
       (string)
# 4510 "src/nuxmvParser.ml"
    )) = _v in
    let _startpos_i_ = _startpos in
    let _v : (A.enum_type_value) = let _startpos = _startpos_i_ in
    
# 100 "src/nuxmvParser.mly"
             ( A.ETId (mk_pos _startpos, i) )
# 4517 "src/nuxmvParser.ml"
     in
    _menhir_goto_enum_type_value _menhir_env _menhir_stack _menhir_s _v

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 32 "src/nuxmvParser.mly"
       (int)
# 4524 "src/nuxmvParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (ci : (
# 32 "src/nuxmvParser.mly"
       (int)
# 4532 "src/nuxmvParser.ml"
    )) = _v in
    let _startpos_ci_ = _startpos in
    let _v : (A.enum_type_value) = let _startpos = _startpos_ci_ in
    
# 101 "src/nuxmvParser.mly"
                ( A.ETCInt (mk_pos _startpos, ci) )
# 4539 "src/nuxmvParser.ml"
     in
    _menhir_goto_enum_type_value _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_option_module_type_spec_param_list_ : _menhir_env -> 'ttv_tail -> (A.expr_type list option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (pl : (A.expr_type list option)) = _v in
    let (_menhir_stack, (id : (
# 31 "src/nuxmvParser.mly"
       (string)
# 4551 "src/nuxmvParser.ml"
    )), _startpos_id_) = _menhir_stack in
    let _v : (A.module_type_specifier) = let _startpos = _startpos_id_ in
    
# 105 "src/nuxmvParser.mly"
                                                       ( match pl with 
                                                         | None -> A.ModuleTypeSpecifier (mk_pos _startpos, id, [])
                                                         | Some p -> A.ModuleTypeSpecifier (mk_pos _startpos, id, p) )
# 4559 "src/nuxmvParser.ml"
     in
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (i : (
# 31 "src/nuxmvParser.mly"
       (string)
# 4573 "src/nuxmvParser.ml"
        )), _startpos_i_), (mts : (A.module_type_specifier))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _v : (A.state_var_decl) = let _startpos = _startpos_i_ in
        
# 88 "src/nuxmvParser.mly"
                                                    ( A.ModuleType (mk_pos _startpos, i, mts) )
# 4581 "src/nuxmvParser.ml"
         in
        _menhir_goto_state_var_element _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_module_type_spec_param_list : _menhir_env -> 'ttv_tail -> (A.expr_type list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (x : (A.expr_type list)) = _v in
    let _v : (A.expr_type list option) = 
# 116 "/usr/local/bin/_opam/lib/menhir/standard.mly"
    ( Some x )
# 4599 "src/nuxmvParser.ml"
     in
    _menhir_goto_option_module_type_spec_param_list_ _menhir_env _menhir_stack _v

and _menhir_goto_simple_type_specifier : _menhir_env -> 'ttv_tail -> (A.simple_type_spec) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (i : (
# 31 "src/nuxmvParser.mly"
       (string)
# 4617 "src/nuxmvParser.ml"
        )), _startpos_i_), (sts : (A.simple_type_spec))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _v : (A.state_var_decl) = let _startpos = _startpos_i_ in
        
# 87 "src/nuxmvParser.mly"
                                                         ( A.SimpleType (mk_pos _startpos, i, sts) )
# 4625 "src/nuxmvParser.ml"
         in
        _menhir_goto_state_var_element _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_constant : _menhir_env -> 'ttv_tail -> _menhir_state -> (A.nuxmv_expr) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (c : (A.nuxmv_expr)) = _v in
    let _startpos_c_ = _startpos in
    let _startpos = _startpos_c_ in
    let _v : (A.nuxmv_expr) = 
# 164 "src/nuxmvParser.mly"
                   ( c )
# 4645 "src/nuxmvParser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run163 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LSQBRACK ->
        _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState163

and _menhir_goto_complex_indentifier : _menhir_env -> 'ttv_tail -> _menhir_state -> (A.comp_ident) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState199 | MenhirState194 | MenhirState187 | MenhirState162 | MenhirState163 | MenhirState169 | MenhirState157 | MenhirState153 | MenhirState24 | MenhirState136 | MenhirState25 | MenhirState26 | MenhirState27 | MenhirState30 | MenhirState31 | MenhirState33 | MenhirState34 | MenhirState35 | MenhirState36 | MenhirState121 | MenhirState38 | MenhirState39 | MenhirState45 | MenhirState113 | MenhirState50 | MenhirState110 | MenhirState55 | MenhirState104 | MenhirState108 | MenhirState63 | MenhirState98 | MenhirState102 | MenhirState100 | MenhirState65 | MenhirState95 | MenhirState93 | MenhirState91 | MenhirState89 | MenhirState87 | MenhirState85 | MenhirState81 | MenhirState83 | MenhirState73 | MenhirState79 | MenhirState77 | MenhirState75 | MenhirState71 | MenhirState69 | MenhirState67 | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CASE ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CINT _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CREAL _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | F ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FALSE ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FRACTIONAL ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | G ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | H ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ID _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LCURLBRACK ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NEXT ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | O ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | SELF ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRUE ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | X ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Y ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Z ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104)
        | LSQBRACK ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PERIOD ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState183 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LSQBRACK ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PERIOD ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ASSIGNMENT ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CASE ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | CINT _v ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | CREAL _v ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | F ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | FALSE ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | FRACTIONAL ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | G ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | H ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | ID _v ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LCURLBRACK ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LPAREN ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | MINUS ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NEXT ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NOT ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | O ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | SELF ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TRUE ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | X ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Y ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Z ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState187)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState191 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LSQBRACK ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PERIOD ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ASSIGNMENT ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CASE ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | CINT _v ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | CREAL _v ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | F ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | FALSE ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | FRACTIONAL ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | G ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | H ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | ID _v ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LCURLBRACK ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LPAREN ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | MINUS ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NEXT ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NOT ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | O ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | SELF ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TRUE ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | X ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Y ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Z ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState194)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState201 | MenhirState181 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASSIGNMENT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CASE ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CINT _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | CREAL _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | F ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FALSE ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FRACTIONAL ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | G ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | H ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ID _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LCURLBRACK ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NEXT ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NOT ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | O ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | SELF ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRUE ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | X ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Y ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | Z ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState199)
        | LSQBRACK ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PERIOD ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce9 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 31 "src/nuxmvParser.mly"
       (string)
# 4992 "src/nuxmvParser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (i : (
# 31 "src/nuxmvParser.mly"
       (string)
# 4998 "src/nuxmvParser.ml"
    )), _startpos_i_) = _menhir_stack in
    let _startpos = _startpos_i_ in
    let _v : (A.comp_ident) = let _startpos = _startpos_i_ in
    
# 135 "src/nuxmvParser.mly"
             ( A.CIdent (mk_pos _startpos, i) )
# 5005 "src/nuxmvParser.ml"
     in
    _menhir_goto_complex_indentifier _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_goto_nonempty_list_module_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (NuxmvAst.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (ml : (NuxmvAst.t))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 59 "src/nuxmvParser.mly"
      (NuxmvAst.t)
# 5026 "src/nuxmvParser.ml"
            ) = 
# 63 "src/nuxmvParser.mly"
                                             ( ml )
# 5030 "src/nuxmvParser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 59 "src/nuxmvParser.mly"
      (NuxmvAst.t)
# 5037 "src/nuxmvParser.ml"
            )) = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState217 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (NuxmvAst.nuxmv_module))), _, (xs : (NuxmvAst.t))) = _menhir_stack in
        let _v : (NuxmvAst.t) = 
# 223 "/usr/local/bin/_opam/lib/menhir/standard.mly"
    ( x :: xs )
# 5053 "src/nuxmvParser.ml"
         in
        _menhir_goto_nonempty_list_module_decl_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 31 "src/nuxmvParser.mly"
       (string)
# 5062 "src/nuxmvParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos__1_ = _startpos in
            let _1 = () in
            let _v : (A.simple_type_spec) = let _startpos = _startpos__1_ in
            
# 92 "src/nuxmvParser.mly"
           ( A.Bool (mk_pos _startpos) )
# 5085 "src/nuxmvParser.ml"
             in
            _menhir_goto_simple_type_specifier _menhir_env _menhir_stack _v
        | CINT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DPERIOD ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CINT _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (i2 : (
# 32 "src/nuxmvParser.mly"
       (int)
# 5108 "src/nuxmvParser.ml"
                    )) = _v in
                    let _startpos_i2_ = _startpos in
                    let (_menhir_stack, (i1 : (
# 32 "src/nuxmvParser.mly"
       (int)
# 5114 "src/nuxmvParser.ml"
                    )), _startpos_i1_) = _menhir_stack in
                    let _2 = () in
                    let _v : (A.simple_type_spec) = let _startpos = _startpos_i1_ in
                    
# 95 "src/nuxmvParser.mly"
                                  ( A.IntRange (mk_pos _startpos, i1, i2) )
# 5121 "src/nuxmvParser.ml"
                     in
                    _menhir_goto_simple_type_specifier _menhir_env _menhir_stack _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    raise _eRR)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                let _menhir_stack = (_menhir_stack, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CASE ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | CINT _v ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | CREAL _v ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | F ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | FALSE ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | FRACTIONAL ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | G ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | H ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | ID _v ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LCURLBRACK ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LPAREN ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | MINUS ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NEXT ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NOT ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | O ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | RPAREN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState24 in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _startpos__1_) = _menhir_stack in
                    let _2 = () in
                    let _1 = () in
                    let _v : (A.expr_type list) = 
# 111 "src/nuxmvParser.mly"
                    ( [] )
# 5189 "src/nuxmvParser.ml"
                     in
                    _menhir_goto_module_type_spec_param_list _menhir_env _menhir_stack _v
                | SELF ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TRUE ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | X ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Y ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | Z ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24)
            | SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _v : (A.expr_type list option) = 
# 114 "/usr/local/bin/_opam/lib/menhir/standard.mly"
    ( None )
# 5211 "src/nuxmvParser.ml"
                 in
                _menhir_goto_option_module_type_spec_param_list_ _menhir_env _menhir_stack _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | INT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos__1_ = _startpos in
            let _1 = () in
            let _v : (A.simple_type_spec) = let _startpos = _startpos__1_ in
            
# 93 "src/nuxmvParser.mly"
          ( A.Int (mk_pos _startpos) )
# 5230 "src/nuxmvParser.ml"
             in
            _menhir_goto_simple_type_specifier _menhir_env _menhir_stack _v
        | LCURLBRACK ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CINT _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ID _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14)
        | REAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos__1_ = _startpos in
            let _1 = () in
            let _v : (A.simple_type_spec) = let _startpos = _startpos__1_ in
            
# 94 "src/nuxmvParser.mly"
           ( A.Real (mk_pos _startpos) )
# 5259 "src/nuxmvParser.ml"
             in
            _menhir_goto_simple_type_specifier _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (A.nuxmv_expr) = let _startpos = _startpos__1_ in
    
# 230 "src/nuxmvParser.mly"
           ( A.True (mk_pos _startpos) )
# 5439 "src/nuxmvParser.ml"
     in
    _menhir_goto_constant _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CASE ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | CINT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | CREAL _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | F ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FALSE ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FRACTIONAL ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | G ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | H ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ID _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LCURLBRACK ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEXT ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | O ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | SELF ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TRUE ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | X ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Y ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Z ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_run36 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 31 "src/nuxmvParser.mly"
       (string)
# 5765 "src/nuxmvParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AND | ASSIGN | COLON | COMMA | DARROW | DEFINE | DIV | EOF | EQ | GT | GTE | ID _ | INIT | LT | LTE | LTLSPEC | MINUS | MOD | MODULE | MUL | NEQ | NEXT | OR | PLUS | RARROW | RCURLBRACK | RPAREN | RSQBRACK | S | SELF | SEMICOLON | T | TRANS | U | V | VAR | XNOR | XOR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (s : (
# 31 "src/nuxmvParser.mly"
       (string)
# 5777 "src/nuxmvParser.ml"
        )), _startpos_s_) = _menhir_stack in
        let _startpos = _startpos_s_ in
        let _v : (A.nuxmv_expr) = let _startpos = _startpos_s_ in
        
# 231 "src/nuxmvParser.mly"
             ( A.Ident (mk_pos _startpos, s) )
# 5784 "src/nuxmvParser.ml"
         in
        _menhir_goto_constant _menhir_env _menhir_stack _menhir_s _v _startpos
    | LPAREN | LSQBRACK | PERIOD ->
        _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run38 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CINT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CINT _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (c2 : (
# 32 "src/nuxmvParser.mly"
       (int)
# 5924 "src/nuxmvParser.ml"
                )) = _v in
                let _startpos_c2_ = _startpos in
                let ((_menhir_stack, _menhir_s, _startpos__1_), (c1 : (
# 32 "src/nuxmvParser.mly"
       (int)
# 5930 "src/nuxmvParser.ml"
                )), _startpos_c1_) = _menhir_stack in
                let _3 = () in
                let _1 = () in
                let _startpos = _startpos__1_ in
                let _v : (A.nuxmv_expr) = let _startpos = _startpos__1_ in
                
# 234 "src/nuxmvParser.mly"
                                         ( let f1 = float_of_int (c1) in
                                           let f2 = float_of_int (c2) in
                                                A.CFloat (mk_pos _startpos, (f1 /. f2)) )
# 5941 "src/nuxmvParser.ml"
                 in
                _menhir_goto_constant _menhir_env _menhir_stack _menhir_s _v _startpos
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (A.nuxmv_expr) = let _startpos = _startpos__1_ in
    
# 229 "src/nuxmvParser.mly"
            ( A.False (mk_pos _startpos) )
# 5974 "src/nuxmvParser.ml"
     in
    _menhir_goto_constant _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run45 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 33 "src/nuxmvParser.mly"
       (float)
# 6032 "src/nuxmvParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (r : (
# 33 "src/nuxmvParser.mly"
       (float)
# 6040 "src/nuxmvParser.ml"
    )) = _v in
    let _startpos_r_ = _startpos in
    let _startpos = _startpos_r_ in
    let _v : (A.nuxmv_expr) = let _startpos = _startpos_r_ in
    
# 233 "src/nuxmvParser.mly"
                ( A.CFloat (mk_pos _startpos, r) )
# 6048 "src/nuxmvParser.ml"
     in
    _menhir_goto_constant _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 32 "src/nuxmvParser.mly"
       (int)
# 6055 "src/nuxmvParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DPERIOD ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CINT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (i2 : (
# 32 "src/nuxmvParser.mly"
       (int)
# 6075 "src/nuxmvParser.ml"
            )) = _v in
            let _startpos_i2_ = _startpos in
            let (_menhir_stack, _menhir_s, (i1 : (
# 32 "src/nuxmvParser.mly"
       (int)
# 6081 "src/nuxmvParser.ml"
            )), _startpos_i1_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_i1_ in
            let _v : (A.nuxmv_expr) = let _startpos = _startpos_i1_ in
            
# 237 "src/nuxmvParser.mly"
                                  ( A.CRange (mk_pos _startpos, i1, i2) )
# 6089 "src/nuxmvParser.ml"
             in
            _menhir_goto_constant _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | AND | ASSIGN | COLON | COMMA | DARROW | DEFINE | DIV | EOF | EQ | GT | GTE | ID _ | INIT | LT | LTE | LTLSPEC | MINUS | MOD | MODULE | MUL | NEQ | NEXT | OR | PLUS | RARROW | RCURLBRACK | RPAREN | RSQBRACK | S | SELF | SEMICOLON | T | TRANS | U | V | VAR | XNOR | XOR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (i : (
# 32 "src/nuxmvParser.mly"
       (int)
# 6103 "src/nuxmvParser.ml"
        )), _startpos_i_) = _menhir_stack in
        let _startpos = _startpos_i_ in
        let _v : (A.nuxmv_expr) = let _startpos = _startpos_i_ in
        
# 232 "src/nuxmvParser.mly"
               ( A.CInt (mk_pos _startpos, i) )
# 6110 "src/nuxmvParser.ml"
         in
        _menhir_goto_constant _menhir_env _menhir_stack _menhir_s _v _startpos
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50

and _menhir_run161 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 31 "src/nuxmvParser.mly"
       (string)
# 6174 "src/nuxmvParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASSIGNMENT ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CASE ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | CINT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | CREAL _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | F ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FALSE ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FRACTIONAL ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | G ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | H ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ID _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LCURLBRACK ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LSQBRACK ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEXT ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NOT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | O ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | SELF ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TRUE ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | X ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Y ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | Z ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState162)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (A.comp_ident) = let _startpos = _startpos__1_ in
    
# 138 "src/nuxmvParser.mly"
           ( A.Self (mk_pos _startpos) )
# 6250 "src/nuxmvParser.ml"
     in
    _menhir_goto_complex_indentifier _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run182 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run184 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | SELF ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState183)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run190 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run184 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | SELF ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState191)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run184 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 31 "src/nuxmvParser.mly"
       (string)
# 6313 "src/nuxmvParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack)

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_option_module_body_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (A.module_element list option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (mb : (A.module_element list option)) = _v in
    let (((_menhir_stack, _menhir_s), (id : (
# 31 "src/nuxmvParser.mly"
       (string)
# 6333 "src/nuxmvParser.ml"
    )), _startpos_id_), (mp : (string list option))) = _menhir_stack in
    let _1 = () in
    let _v : (NuxmvAst.nuxmv_module) = 
# 65 "src/nuxmvParser.mly"
                                                                                ( match (mp,mb) with 
                                                                                  | (None, None)     -> A.CustomModule (id, [], [])
                                                                                  | (Some p, None)   -> A.CustomModule (id, p, [])
                                                                                  | (None, Some b)   -> A.CustomModule (id, [], b)
                                                                                  | (Some p, Some b) -> A.CustomModule (id, p, b) )
# 6343 "src/nuxmvParser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | MODULE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState217
    | EOF ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (NuxmvAst.nuxmv_module))) = _menhir_stack in
        let _v : (NuxmvAst.t) = 
# 221 "/usr/local/bin/_opam/lib/menhir/standard.mly"
    ( [ x ] )
# 6358 "src/nuxmvParser.ml"
         in
        _menhir_goto_nonempty_list_module_decl_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState217

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10

and _menhir_run153 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState153

and _menhir_run157 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CASE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CINT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CREAL _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | F ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FRACTIONAL ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | G ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | H ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ID _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LCURLBRACK ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NOT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | O ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | X ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Y ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | Z ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState157

and _menhir_run160 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState160

and _menhir_run181 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run184 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INIT ->
        _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEXT ->
        _menhir_run182 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState181

and _menhir_goto_separated_nonempty_list_COMMA_ID_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (
# 31 "src/nuxmvParser.mly"
       (string)
# 6523 "src/nuxmvParser.ml"
        )), _startpos_x_), _, (xs : (string list))) = _menhir_stack in
        let _2 = () in
        let _v : (string list) = 
# 243 "/usr/local/bin/_opam/lib/menhir/standard.mly"
    ( x :: xs )
# 6529 "src/nuxmvParser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_ID_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _startpos__1_), _, (mpl : (string list))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (string list) = 
# 71 "src/nuxmvParser.mly"
                                                                      ( mpl )
# 6547 "src/nuxmvParser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (x : (string list)) = _v in
            let _v : (string list option) = 
# 116 "/usr/local/bin/_opam/lib/menhir/standard.mly"
    ( Some x )
# 6555 "src/nuxmvParser.ml"
             in
            _menhir_goto_option_module_params_ _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_module_params_ : _menhir_env -> 'ttv_tail -> (string list option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASSIGN ->
        _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DEFINE ->
        _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LTLSPEC ->
        _menhir_run157 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRANS ->
        _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | VAR ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EOF | MODULE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState9 in
        let _v : (A.module_element list option) = 
# 114 "/usr/local/bin/_opam/lib/menhir/standard.mly"
    ( None )
# 6590 "src/nuxmvParser.ml"
         in
        _menhir_goto_option_module_body_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 31 "src/nuxmvParser.mly"
       (string)
# 6601 "src/nuxmvParser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5)
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (
# 31 "src/nuxmvParser.mly"
       (string)
# 6624 "src/nuxmvParser.ml"
        )), _startpos_x_) = _menhir_stack in
        let _v : (string list) = 
# 241 "/usr/local/bin/_opam/lib/menhir/standard.mly"
    ( [ x ] )
# 6629 "src/nuxmvParser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_ID_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState217 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState207 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState201 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState200 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState199 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState195 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState194 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState191 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState188 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState187 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState183 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState181 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState179 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState174 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState172 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState169 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState168 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState162 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState160 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState157 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState154 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3)
        | ASSIGN | DEFINE | EOF | LTLSPEC | MODULE | TRANS | VAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _v : (string list option) = 
# 114 "/usr/local/bin/_opam/lib/menhir/standard.mly"
    ( None )
# 7074 "src/nuxmvParser.ml"
             in
            _menhir_goto_option_module_params_ _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 59 "src/nuxmvParser.mly"
      (NuxmvAst.t)
# 7105 "src/nuxmvParser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | MODULE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "/usr/local/bin/_opam/lib/menhir/standard.mly"
  

# 7129 "src/nuxmvParser.ml"
