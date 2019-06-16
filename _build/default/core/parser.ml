
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WHILE of (
# 10 "core/parser.mly"
       (Support.Error.info)
# 11 "core/parser.ml"
  )
    | VOID
    | TRUE of (
# 11 "core/parser.mly"
       (Support.Error.info)
# 17 "core/parser.ml"
  )
    | TIMES of (
# 20 "core/parser.mly"
       (Support.Error.info)
# 22 "core/parser.ml"
  )
    | SEMICOLON of (
# 14 "core/parser.mly"
       (Support.Error.info)
# 27 "core/parser.ml"
  )
    | RPAREN of (
# 15 "core/parser.mly"
       (Support.Error.info)
# 32 "core/parser.ml"
  )
    | RETURN of (
# 13 "core/parser.mly"
       (Support.Error.info)
# 37 "core/parser.ml"
  )
    | RBRACK of (
# 17 "core/parser.mly"
       (Support.Error.info)
# 42 "core/parser.ml"
  )
    | RBRACE of (
# 16 "core/parser.mly"
       (Support.Error.info)
# 47 "core/parser.ml"
  )
    | PLUS of (
# 20 "core/parser.mly"
       (Support.Error.info)
# 52 "core/parser.ml"
  )
    | OR of (
# 21 "core/parser.mly"
       (Support.Error.info)
# 57 "core/parser.ml"
  )
    | NUM of (
# 8 "core/parser.mly"
       (int Support.Error.withinfo)
# 62 "core/parser.ml"
  )
    | NULL of (
# 26 "core/parser.mly"
       (Support.Error.info)
# 67 "core/parser.ml"
  )
    | NOT of (
# 22 "core/parser.mly"
       (Support.Error.info)
# 72 "core/parser.ml"
  )
    | NEW of (
# 25 "core/parser.mly"
       (Support.Error.info)
# 77 "core/parser.ml"
  )
    | NEQ of (
# 23 "core/parser.mly"
       (Support.Error.info)
# 82 "core/parser.ml"
  )
    | MINUS of (
# 20 "core/parser.mly"
       (Support.Error.info)
# 87 "core/parser.ml"
  )
    | LT of (
# 23 "core/parser.mly"
       (Support.Error.info)
# 92 "core/parser.ml"
  )
    | LPAREN of (
# 15 "core/parser.mly"
       (Support.Error.info)
# 97 "core/parser.ml"
  )
    | LEQ of (
# 23 "core/parser.mly"
       (Support.Error.info)
# 102 "core/parser.ml"
  )
    | LBRACK of (
# 17 "core/parser.mly"
       (Support.Error.info)
# 107 "core/parser.ml"
  )
    | LBRACE of (
# 16 "core/parser.mly"
       (Support.Error.info)
# 112 "core/parser.ml"
  )
    | INT
    | IF of (
# 10 "core/parser.mly"
       (Support.Error.info)
# 118 "core/parser.ml"
  )
    | ID of (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 123 "core/parser.ml"
  )
    | GT of (
# 23 "core/parser.mly"
       (Support.Error.info)
# 128 "core/parser.ml"
  )
    | GEQ of (
# 23 "core/parser.mly"
       (Support.Error.info)
# 133 "core/parser.ml"
  )
    | FOR of (
# 10 "core/parser.mly"
       (Support.Error.info)
# 138 "core/parser.ml"
  )
    | FALSE of (
# 11 "core/parser.mly"
       (Support.Error.info)
# 143 "core/parser.ml"
  )
    | EQ of (
# 23 "core/parser.mly"
       (Support.Error.info)
# 148 "core/parser.ml"
  )
    | EOF
    | ELSE of (
# 10 "core/parser.mly"
       (Support.Error.info)
# 154 "core/parser.ml"
  )
    | DOT of (
# 18 "core/parser.mly"
       (Support.Error.info)
# 159 "core/parser.ml"
  )
    | DIV of (
# 20 "core/parser.mly"
       (Support.Error.info)
# 164 "core/parser.ml"
  )
    | COMMA of (
# 14 "core/parser.mly"
       (Support.Error.info)
# 169 "core/parser.ml"
  )
    | CLASS
    | BOOL
    | ASSIGN of (
# 23 "core/parser.mly"
       (Support.Error.info)
# 176 "core/parser.ml"
  )
    | AND of (
# 21 "core/parser.mly"
       (Support.Error.info)
# 181 "core/parser.ml"
  )
  
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
  | MenhirState173
  | MenhirState169
  | MenhirState163
  | MenhirState161
  | MenhirState155
  | MenhirState152
  | MenhirState150
  | MenhirState148
  | MenhirState147
  | MenhirState141
  | MenhirState136
  | MenhirState134
  | MenhirState132
  | MenhirState128
  | MenhirState124
  | MenhirState122
  | MenhirState120
  | MenhirState117
  | MenhirState116
  | MenhirState96
  | MenhirState94
  | MenhirState92
  | MenhirState90
  | MenhirState88
  | MenhirState86
  | MenhirState84
  | MenhirState82
  | MenhirState80
  | MenhirState77
  | MenhirState75
  | MenhirState72
  | MenhirState69
  | MenhirState67
  | MenhirState63
  | MenhirState58
  | MenhirState56
  | MenhirState55
  | MenhirState53
  | MenhirState47
  | MenhirState46
  | MenhirState42
  | MenhirState40
  | MenhirState38
  | MenhirState34
  | MenhirState30
  | MenhirState22
  | MenhirState15
  | MenhirState8
  | MenhirState0

# 1 "core/parser.mly"
  
  module A = Ast
  open Support.Error
  module S = Symbol


# 256 "core/parser.ml"

let rec _menhir_goto_elseopt : _menhir_env -> 'ttv_tail -> 'tv_elseopt -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (((('freshtv685 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 264 "core/parser.ml"
    )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 268 "core/parser.ml"
    )) * _menhir_state * 'tv_exp) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 272 "core/parser.ml"
    )) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
    let (_v : 'tv_elseopt) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (((('freshtv683 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 279 "core/parser.ml"
    )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 283 "core/parser.ml"
    )) * _menhir_state * 'tv_exp) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 287 "core/parser.ml"
    )) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
    let ((sop : 'tv_elseopt) : 'tv_elseopt) = _v in
    ((let (((((_menhir_stack, _menhir_s, (i : (
# 10 "core/parser.mly"
       (Support.Error.info)
# 293 "core/parser.ml"
    ))), (_2 : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 297 "core/parser.ml"
    ))), _, (e : 'tv_exp)), (_4 : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 301 "core/parser.ml"
    ))), _, (s : 'tv_stmt)) = _menhir_stack in
    let _v : 'tv_control = 
# 268 "core/parser.mly"
                                                  ( A.If(e,s,sop,i) )
# 306 "core/parser.ml"
     in
    _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v) : 'freshtv684)) : 'freshtv686)

and _menhir_goto_control : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_control -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv681) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_control) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv679) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((s : 'tv_control) : 'tv_control) = _v in
    ((let _v : 'tv_stmt = 
# 139 "core/parser.mly"
                                                  ( s )
# 323 "core/parser.ml"
     in
    _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv680)) : 'freshtv682)

and _menhir_goto_stmt_list : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_stmt_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv669 * _menhir_state * (
# 16 "core/parser.mly"
       (Support.Error.info)
# 336 "core/parser.ml"
        )) * _menhir_state * 'tv_stmt_list) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv665 * _menhir_state * (
# 16 "core/parser.mly"
       (Support.Error.info)
# 346 "core/parser.ml"
            )) * _menhir_state * 'tv_stmt_list) = Obj.magic _menhir_stack in
            let (_v : (
# 16 "core/parser.mly"
       (Support.Error.info)
# 351 "core/parser.ml"
            )) = _v in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv663 * _menhir_state * (
# 16 "core/parser.mly"
       (Support.Error.info)
# 358 "core/parser.ml"
            )) * _menhir_state * 'tv_stmt_list) = Obj.magic _menhir_stack in
            let ((_3 : (
# 16 "core/parser.mly"
       (Support.Error.info)
# 363 "core/parser.ml"
            )) : (
# 16 "core/parser.mly"
       (Support.Error.info)
# 367 "core/parser.ml"
            )) = _v in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 16 "core/parser.mly"
       (Support.Error.info)
# 372 "core/parser.ml"
            ))), _, (s : 'tv_stmt_list)) = _menhir_stack in
            let _v : 'tv_block = 
# 115 "core/parser.mly"
                                                ( s )
# 377 "core/parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv661) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_block) = _v in
            ((match _menhir_s with
            | MenhirState40 | MenhirState161 | MenhirState163 | MenhirState116 | MenhirState122 | MenhirState155 | MenhirState152 | MenhirState136 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv649) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_block) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv647) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((s : 'tv_block) : 'tv_block) = _v in
                ((let _v : 'tv_stmt = 
# 140 "core/parser.mly"
                                                  ( s )
# 396 "core/parser.ml"
                 in
                _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv648)) : 'freshtv650)
            | MenhirState38 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv659 * _menhir_state * 'tv_ty) * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 404 "core/parser.ml"
                )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 408 "core/parser.ml"
                )) * _menhir_state * 'tv_param_list) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 412 "core/parser.ml"
                )) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_block) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv657 * _menhir_state * 'tv_ty) * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 420 "core/parser.ml"
                )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 424 "core/parser.ml"
                )) * _menhir_state * 'tv_param_list) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 428 "core/parser.ml"
                )) = Obj.magic _menhir_stack in
                let (_ : _menhir_state) = _menhir_s in
                let ((defs : 'tv_block) : 'tv_block) = _v in
                ((let (((((_menhir_stack, _menhir_s, (t : 'tv_ty)), (id : (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 435 "core/parser.ml"
                ))), (_3 : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 439 "core/parser.ml"
                ))), _, (pl : 'tv_param_list)), (_5 : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 443 "core/parser.ml"
                ))) = _menhir_stack in
                let _v : 'tv_fundefn = 
# 69 "core/parser.mly"
    ( let {v;i}=id in 
      let (il,tl) = List.split pl in 
      let t' = A.Arrow(tl,t) in 
      fun s -> 
      A.Fundefn(v,il,t',defs,s,i)
    )
# 453 "core/parser.ml"
                 in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv655) = _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_fundefn) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv653) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_fundefn) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv651) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((f : 'tv_fundefn) : 'tv_fundefn) = _v in
                ((let _v : 'tv_gdecl = 
# 48 "core/parser.mly"
                                                  ( f )
# 470 "core/parser.ml"
                 in
                _menhir_goto_gdecl _menhir_env _menhir_stack _menhir_s _v) : 'freshtv652)) : 'freshtv654)) : 'freshtv656)) : 'freshtv658)) : 'freshtv660)
            | _ ->
                _menhir_fail ()) : 'freshtv662)) : 'freshtv664)) : 'freshtv666)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv667 * _menhir_state * (
# 16 "core/parser.mly"
       (Support.Error.info)
# 482 "core/parser.ml"
            )) * _menhir_state * 'tv_stmt_list) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv668)) : 'freshtv670)
    | MenhirState161 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv673 * _menhir_state * 'tv_stmt) * _menhir_state * 'tv_stmt_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv671 * _menhir_state * 'tv_stmt) * _menhir_state * 'tv_stmt_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (s : 'tv_stmt)), _, (sl : 'tv_stmt_list)) = _menhir_stack in
        let _v : 'tv_stmt_list = 
# 121 "core/parser.mly"
    ( match sl with 
      | A.Seq(sl',_) -> A.Seq(s::sl', A.Util.extract_info_stmt s) 
      | _ as s' -> A.Seq([s;s'], A.Util.extract_info_stmt s) )
# 497 "core/parser.ml"
         in
        _menhir_goto_stmt_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv672)) : 'freshtv674)
    | MenhirState163 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv677 * _menhir_state * 'tv_decl) * _menhir_state * 'tv_stmt_list) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv675 * _menhir_state * 'tv_decl) * _menhir_state * 'tv_stmt_list) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (d : 'tv_decl)), _, (sl : 'tv_stmt_list)) = _menhir_stack in
        let _v : 'tv_stmt_list = 
# 119 "core/parser.mly"
                                                  ( d sl )
# 509 "core/parser.ml"
         in
        _menhir_goto_stmt_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv676)) : 'freshtv678)
    | _ ->
        _menhir_fail ()

and _menhir_goto_simpopt : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_simpopt -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState124 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv633 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 524 "core/parser.ml"
        )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 528 "core/parser.ml"
        )) * _menhir_state * 'tv_simpopt) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv629 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 538 "core/parser.ml"
            )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 542 "core/parser.ml"
            )) * _menhir_state * 'tv_simpopt) = Obj.magic _menhir_stack in
            let (_v : (
# 14 "core/parser.mly"
       (Support.Error.info)
# 547 "core/parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FALSE _v ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
            | ID _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
            | LPAREN _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
            | MINUS _v ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
            | NEW _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
            | NOT _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
            | NULL _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
            | NUM _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
            | TRUE _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState132) : 'freshtv630)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv631 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 582 "core/parser.ml"
            )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 586 "core/parser.ml"
            )) * _menhir_state * 'tv_simpopt) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv632)) : 'freshtv634)
    | MenhirState134 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv639 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 595 "core/parser.ml"
        )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 599 "core/parser.ml"
        )) * _menhir_state * 'tv_simpopt) * (
# 14 "core/parser.mly"
       (Support.Error.info)
# 603 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) * (
# 14 "core/parser.mly"
       (Support.Error.info)
# 607 "core/parser.ml"
        )) * _menhir_state * 'tv_simpopt) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv635 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 617 "core/parser.ml"
            )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 621 "core/parser.ml"
            )) * _menhir_state * 'tv_simpopt) * (
# 14 "core/parser.mly"
       (Support.Error.info)
# 625 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) * (
# 14 "core/parser.mly"
       (Support.Error.info)
# 629 "core/parser.ml"
            )) * _menhir_state * 'tv_simpopt) = Obj.magic _menhir_stack in
            let (_v : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 634 "core/parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FALSE _v ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
            | FOR _v ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
            | ID _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
            | IF _v ->
                _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
            | LBRACE _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
            | LPAREN _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
            | MINUS _v ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
            | NEW _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
            | NOT _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
            | NULL _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
            | NUM _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
            | RETURN _v ->
                _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
            | TRUE _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
            | WHILE _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136) : 'freshtv636)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv637 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 679 "core/parser.ml"
            )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 683 "core/parser.ml"
            )) * _menhir_state * 'tv_simpopt) * (
# 14 "core/parser.mly"
       (Support.Error.info)
# 687 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) * (
# 14 "core/parser.mly"
       (Support.Error.info)
# 691 "core/parser.ml"
            )) * _menhir_state * 'tv_simpopt) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv638)) : 'freshtv640)
    | MenhirState150 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv645 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 700 "core/parser.ml"
        )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 704 "core/parser.ml"
        )) * _menhir_state * 'tv_decl) * _menhir_state * 'tv_exp) * (
# 14 "core/parser.mly"
       (Support.Error.info)
# 708 "core/parser.ml"
        )) * _menhir_state * 'tv_simpopt) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv641 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 718 "core/parser.ml"
            )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 722 "core/parser.ml"
            )) * _menhir_state * 'tv_decl) * _menhir_state * 'tv_exp) * (
# 14 "core/parser.mly"
       (Support.Error.info)
# 726 "core/parser.ml"
            )) * _menhir_state * 'tv_simpopt) = Obj.magic _menhir_stack in
            let (_v : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 731 "core/parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FALSE _v ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
            | FOR _v ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
            | ID _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
            | IF _v ->
                _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
            | LBRACE _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
            | LPAREN _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
            | MINUS _v ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
            | NEW _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
            | NOT _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
            | NULL _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
            | NUM _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
            | RETURN _v ->
                _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
            | TRUE _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
            | WHILE _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState152) : 'freshtv642)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv643 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 776 "core/parser.ml"
            )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 780 "core/parser.ml"
            )) * _menhir_state * 'tv_decl) * _menhir_state * 'tv_exp) * (
# 14 "core/parser.mly"
       (Support.Error.info)
# 784 "core/parser.ml"
            )) * _menhir_state * 'tv_simpopt) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv644)) : 'freshtv646)
    | _ ->
        _menhir_fail ()

and _menhir_goto_stmt : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_stmt -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState136 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv605 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 800 "core/parser.ml"
        )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 804 "core/parser.ml"
        )) * _menhir_state * 'tv_simpopt) * (
# 14 "core/parser.mly"
       (Support.Error.info)
# 808 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) * (
# 14 "core/parser.mly"
       (Support.Error.info)
# 812 "core/parser.ml"
        )) * _menhir_state * 'tv_simpopt) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 816 "core/parser.ml"
        )) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv603 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 822 "core/parser.ml"
        )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 826 "core/parser.ml"
        )) * _menhir_state * 'tv_simpopt) * (
# 14 "core/parser.mly"
       (Support.Error.info)
# 830 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) * (
# 14 "core/parser.mly"
       (Support.Error.info)
# 834 "core/parser.ml"
        )) * _menhir_state * 'tv_simpopt) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 838 "core/parser.ml"
        )) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let (((((((((_menhir_stack, _menhir_s, (i : (
# 10 "core/parser.mly"
       (Support.Error.info)
# 843 "core/parser.ml"
        ))), (_2 : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 847 "core/parser.ml"
        ))), _, (sop1 : 'tv_simpopt)), (_4 : (
# 14 "core/parser.mly"
       (Support.Error.info)
# 851 "core/parser.ml"
        ))), _, (e : 'tv_exp)), (_6 : (
# 14 "core/parser.mly"
       (Support.Error.info)
# 855 "core/parser.ml"
        ))), _, (sop2 : 'tv_simpopt)), (_8 : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 859 "core/parser.ml"
        ))), _, (s : 'tv_stmt)) = _menhir_stack in
        let _v : 'tv_control = 
# 272 "core/parser.mly"
    ( A.Seq([sop1;A.While(e, A.Seq([s; sop2], A.Util.extract_info_stmt s), i)], A.Util.extract_info_stmt sop1) )
# 864 "core/parser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v) : 'freshtv604)) : 'freshtv606)
    | MenhirState152 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv609 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 872 "core/parser.ml"
        )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 876 "core/parser.ml"
        )) * _menhir_state * 'tv_decl) * _menhir_state * 'tv_exp) * (
# 14 "core/parser.mly"
       (Support.Error.info)
# 880 "core/parser.ml"
        )) * _menhir_state * 'tv_simpopt) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 884 "core/parser.ml"
        )) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv607 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 890 "core/parser.ml"
        )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 894 "core/parser.ml"
        )) * _menhir_state * 'tv_decl) * _menhir_state * 'tv_exp) * (
# 14 "core/parser.mly"
       (Support.Error.info)
# 898 "core/parser.ml"
        )) * _menhir_state * 'tv_simpopt) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 902 "core/parser.ml"
        )) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let ((((((((_menhir_stack, _menhir_s, (i : (
# 10 "core/parser.mly"
       (Support.Error.info)
# 907 "core/parser.ml"
        ))), (_2 : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 911 "core/parser.ml"
        ))), _, (d : 'tv_decl)), _, (e : 'tv_exp)), (_5 : (
# 14 "core/parser.mly"
       (Support.Error.info)
# 915 "core/parser.ml"
        ))), _, (sop2 : 'tv_simpopt)), (_7 : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 919 "core/parser.ml"
        ))), _, (s : 'tv_stmt)) = _menhir_stack in
        let _v : 'tv_control = 
# 275 "core/parser.mly"
    ( d (A.While(e, A.Seq([s; sop2], A.Util.extract_info_stmt s), i)) )
# 924 "core/parser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v) : 'freshtv608)) : 'freshtv610)
    | MenhirState122 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv617 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 932 "core/parser.ml"
        )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 936 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 940 "core/parser.ml"
        )) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ELSE _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv611) = Obj.magic _menhir_stack in
            let (_v : (
# 10 "core/parser.mly"
       (Support.Error.info)
# 951 "core/parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FALSE _v ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
            | FOR _v ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
            | ID _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
            | IF _v ->
                _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
            | LBRACE _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
            | LPAREN _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
            | MINUS _v ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
            | NEW _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
            | NOT _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
            | NULL _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
            | NUM _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
            | RETURN _v ->
                _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
            | TRUE _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
            | WHILE _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState155) : 'freshtv612)
        | BOOL | FALSE _ | FOR _ | ID _ | IF _ | INT | LBRACE _ | LPAREN _ | MINUS _ | NEW _ | NOT _ | NULL _ | NUM _ | RBRACE _ | RETURN _ | TRUE _ | VOID | WHILE _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv613) = Obj.magic _menhir_stack in
            ((let _v : 'tv_elseopt = 
# 283 "core/parser.mly"
                                                  ( None )
# 995 "core/parser.ml"
             in
            _menhir_goto_elseopt _menhir_env _menhir_stack _v) : 'freshtv614)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv615 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 1005 "core/parser.ml"
            )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 1009 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 1013 "core/parser.ml"
            )) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv616)) : 'freshtv618)
    | MenhirState155 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv621 * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 1022 "core/parser.ml"
        )) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv619 * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 1028 "core/parser.ml"
        )) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, (_1 : (
# 10 "core/parser.mly"
       (Support.Error.info)
# 1033 "core/parser.ml"
        ))), _, (s : 'tv_stmt)) = _menhir_stack in
        let _v : 'tv_elseopt = 
# 282 "core/parser.mly"
                                                  ( Some(s) )
# 1038 "core/parser.ml"
         in
        _menhir_goto_elseopt _menhir_env _menhir_stack _v) : 'freshtv620)) : 'freshtv622)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv625 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 1046 "core/parser.ml"
        )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 1050 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 1054 "core/parser.ml"
        )) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv623 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 1060 "core/parser.ml"
        )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 1064 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 1068 "core/parser.ml"
        )) * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let (((((_menhir_stack, _menhir_s, (i : (
# 10 "core/parser.mly"
       (Support.Error.info)
# 1073 "core/parser.ml"
        ))), (_2 : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 1077 "core/parser.ml"
        ))), _, (e : 'tv_exp)), (_4 : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 1081 "core/parser.ml"
        ))), _, (s : 'tv_stmt)) = _menhir_stack in
        let _v : 'tv_control = 
# 277 "core/parser.mly"
    ( A.While(e, s, i))
# 1086 "core/parser.ml"
         in
        _menhir_goto_control _menhir_env _menhir_stack _menhir_s _v) : 'freshtv624)) : 'freshtv626)
    | MenhirState163 | MenhirState161 | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv627 * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | FALSE _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | FOR _v ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | ID _v ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | IF _v ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | INT ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | LBRACE _v ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | LPAREN _v ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | MINUS _v ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | NEW _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | NOT _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | NULL _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | NUM _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | RETURN _v ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | TRUE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | VOID ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | WHILE _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | RBRACE _ ->
            _menhir_reduce84 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState161) : 'freshtv628)
    | _ ->
        _menhir_fail ()

and _menhir_reduce74 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_lvalue -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (l : 'tv_lvalue)) = _menhir_stack in
    let _v : 'tv_simpexp = 
# 213 "core/parser.mly"
                                                  ( A.Var(l) )
# 1144 "core/parser.ml"
     in
    _menhir_goto_simpexp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run65 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_lvalue -> (
# 18 "core/parser.mly"
       (Support.Error.info)
# 1151 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv599 * _menhir_state * 'tv_lvalue) * (
# 18 "core/parser.mly"
       (Support.Error.info)
# 1163 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        let (_v : (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 1168 "core/parser.ml"
        )) = _v in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv597 * _menhir_state * 'tv_lvalue) * (
# 18 "core/parser.mly"
       (Support.Error.info)
# 1175 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        let ((id : (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 1180 "core/parser.ml"
        )) : (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 1184 "core/parser.ml"
        )) = _v in
        ((let ((_menhir_stack, _menhir_s, (l : 'tv_lvalue)), (_2 : (
# 18 "core/parser.mly"
       (Support.Error.info)
# 1189 "core/parser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_complvalue = 
# 204 "core/parser.mly"
                                                  ( let {v;i}=id in A.FieldVar(l,v,i) )
# 1194 "core/parser.ml"
         in
        _menhir_goto_complvalue _menhir_env _menhir_stack _menhir_s _v) : 'freshtv598)) : 'freshtv600)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv601 * _menhir_state * 'tv_lvalue) * (
# 18 "core/parser.mly"
       (Support.Error.info)
# 1204 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv602)

and _menhir_reduce84 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_stmt_list = 
# 118 "core/parser.mly"
                                                  ( A.Nop )
# 1214 "core/parser.ml"
     in
    _menhir_goto_stmt_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run125 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 1221 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACK _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _v
    | LPAREN _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _v
    | ID _ ->
        _menhir_reduce91 _menhir_env (Obj.magic _menhir_stack)
    | AND _ | ASSIGN _ | DIV _ | DOT _ | EQ _ | GEQ _ | GT _ | LEQ _ | LT _ | MINUS _ | NEQ _ | OR _ | PLUS _ | SEMICOLON _ | TIMES _ ->
        _menhir_reduce76 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv595 * _menhir_state * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 1243 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv596)

and _menhir_reduce77 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_simpopt = 
# 144 "core/parser.mly"
                                                  ( A.Nop )
# 1253 "core/parser.ml"
     in
    _menhir_goto_simpopt _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_simp : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_simp -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState40 | MenhirState163 | MenhirState161 | MenhirState116 | MenhirState122 | MenhirState155 | MenhirState152 | MenhirState136 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv589 * _menhir_state * 'tv_simp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv585 * _menhir_state * 'tv_simp) = Obj.magic _menhir_stack in
            let (_v : (
# 14 "core/parser.mly"
       (Support.Error.info)
# 1273 "core/parser.ml"
            )) = _v in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv583 * _menhir_state * 'tv_simp) = Obj.magic _menhir_stack in
            let ((_2 : (
# 14 "core/parser.mly"
       (Support.Error.info)
# 1281 "core/parser.ml"
            )) : (
# 14 "core/parser.mly"
       (Support.Error.info)
# 1285 "core/parser.ml"
            )) = _v in
            ((let (_menhir_stack, _menhir_s, (s : 'tv_simp)) = _menhir_stack in
            let _v : 'tv_stmt = 
# 138 "core/parser.mly"
                                                  ( s )
# 1291 "core/parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv584)) : 'freshtv586)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv587 * _menhir_state * 'tv_simp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv588)) : 'freshtv590)
    | MenhirState150 | MenhirState124 | MenhirState134 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv593 * _menhir_state * 'tv_simp) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv591 * _menhir_state * 'tv_simp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (s : 'tv_simp)) = _menhir_stack in
        let _v : 'tv_simpopt = 
# 145 "core/parser.mly"
                                                  ( s )
# 1310 "core/parser.ml"
         in
        _menhir_goto_simpopt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv592)) : 'freshtv594)
    | _ ->
        _menhir_fail ()

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "core/parser.mly"
       (Support.Error.info)
# 1319 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv579 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 1331 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        let (_v : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 1336 "core/parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FALSE _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | ID _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | LPAREN _v ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | MINUS _v ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | NEW _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | NOT _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | NULL _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | NUM _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | TRUE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42) : 'freshtv580)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv581 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 1371 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv582)

and _menhir_run117 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 13 "core/parser.mly"
       (Support.Error.info)
# 1379 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
    | ID _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
    | LPAREN _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
    | MINUS _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
    | NEW _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
    | NOT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
    | NULL _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
    | NUM _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
    | TRUE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
    | RPAREN _ | SEMICOLON _ ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv577 * _menhir_state * (
# 13 "core/parser.mly"
       (Support.Error.info)
# 1409 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (i : (
# 13 "core/parser.mly"
       (Support.Error.info)
# 1414 "core/parser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_simp = 
# 152 "core/parser.mly"
                                                  ( A.Return(A.Void_exp,i) )
# 1419 "core/parser.ml"
         in
        _menhir_goto_simp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv578)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117

and _menhir_run119 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "core/parser.mly"
       (Support.Error.info)
# 1430 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv573 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 1442 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        let (_v : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 1447 "core/parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FALSE _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
        | ID _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
        | LPAREN _v ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
        | MINUS _v ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
        | NEW _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
        | NOT _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
        | NULL _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
        | NUM _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
        | TRUE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120) : 'freshtv574)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv575 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 1482 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv576)

and _menhir_run123 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "core/parser.mly"
       (Support.Error.info)
# 1490 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv569 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 1502 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        let (_v : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 1507 "core/parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | FALSE _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
        | ID _v ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
        | INT ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | LPAREN _v ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
        | MINUS _v ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
        | NEW _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
        | NOT _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
        | NULL _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
        | NUM _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
        | RETURN _v ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
        | TRUE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
        | VOID ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | SEMICOLON _ ->
            _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124) : 'freshtv570)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv571 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 1552 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv572)

and _menhir_goto_rev_arg_list : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_rev_arg_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv567 * _menhir_state * 'tv_rev_arg_list) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv561 * _menhir_state * 'tv_rev_arg_list) = Obj.magic _menhir_stack in
        let (_v : (
# 14 "core/parser.mly"
       (Support.Error.info)
# 1571 "core/parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FALSE _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | ID _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | LPAREN _v ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | MINUS _v ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | NEW _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | NOT _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | NULL _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | NUM _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | TRUE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63) : 'freshtv562)
    | RPAREN _ ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv563 * _menhir_state * 'tv_rev_arg_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (l : 'tv_rev_arg_list)) = _menhir_stack in
        let _v : 'tv_arg_list = 
# 252 "core/parser.mly"
                                                  ( List.rev l )
# 1606 "core/parser.ml"
         in
        _menhir_goto_arg_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv564)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv565 * _menhir_state * 'tv_rev_arg_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv566)) : 'freshtv568)

and _menhir_run69 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_exp -> (
# 20 "core/parser.mly"
       (Support.Error.info)
# 1620 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | ID _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | LPAREN _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | MINUS _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | NEW _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | NOT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | NULL _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | NUM _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | TRUE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69

and _menhir_run75 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_exp -> (
# 20 "core/parser.mly"
       (Support.Error.info)
# 1653 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | ID _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | LPAREN _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | MINUS _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | NEW _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | NOT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | NULL _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | NUM _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | TRUE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75

and _menhir_run80 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_exp -> (
# 21 "core/parser.mly"
       (Support.Error.info)
# 1686 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | ID _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | LPAREN _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | MINUS _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | NEW _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | NOT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | NULL _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | NUM _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | TRUE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80

and _menhir_run82 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_exp -> (
# 23 "core/parser.mly"
       (Support.Error.info)
# 1719 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | ID _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | LPAREN _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | MINUS _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | NEW _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | NOT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | NULL _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | NUM _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | TRUE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82

and _menhir_run84 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_exp -> (
# 20 "core/parser.mly"
       (Support.Error.info)
# 1752 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | ID _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | LPAREN _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | MINUS _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | NEW _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | NOT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | NULL _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | NUM _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | TRUE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84

and _menhir_run86 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_exp -> (
# 23 "core/parser.mly"
       (Support.Error.info)
# 1785 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | ID _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | LPAREN _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | MINUS _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | NEW _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | NOT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | NULL _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | NUM _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | TRUE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86

and _menhir_run88 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_exp -> (
# 23 "core/parser.mly"
       (Support.Error.info)
# 1818 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | ID _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | LPAREN _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | MINUS _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | NEW _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | NOT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | NULL _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | NUM _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | TRUE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88

and _menhir_run90 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_exp -> (
# 23 "core/parser.mly"
       (Support.Error.info)
# 1851 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | ID _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | LPAREN _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | MINUS _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | NEW _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | NOT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | NULL _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | NUM _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | TRUE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90

and _menhir_run92 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_exp -> (
# 23 "core/parser.mly"
       (Support.Error.info)
# 1884 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | ID _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | LPAREN _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | MINUS _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | NEW _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | NOT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | NULL _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | NUM _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | TRUE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92

and _menhir_run94 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_exp -> (
# 23 "core/parser.mly"
       (Support.Error.info)
# 1917 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | ID _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | LPAREN _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | MINUS _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | NEW _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | NOT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | NULL _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | NUM _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | TRUE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94

and _menhir_run77 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_exp -> (
# 20 "core/parser.mly"
       (Support.Error.info)
# 1950 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | ID _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | LPAREN _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | MINUS _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | NEW _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | NOT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | NULL _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | NUM _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | TRUE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_run96 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_exp -> (
# 21 "core/parser.mly"
       (Support.Error.info)
# 1983 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | ID _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | LPAREN _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | MINUS _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | NEW _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | NOT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | NULL _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | NUM _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | TRUE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96

and _menhir_reduce56 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_complvalue -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (l : 'tv_complvalue)) = _menhir_stack in
    let _v : 'tv_lvalue = 
# 193 "core/parser.mly"
                                                    ( l )
# 2019 "core/parser.ml"
     in
    _menhir_goto_lvalue _menhir_env _menhir_stack _menhir_s _v

and _menhir_run72 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_complvalue -> (
# 17 "core/parser.mly"
       (Support.Error.info)
# 2026 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | ID _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | LPAREN _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | MINUS _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | NEW _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | NOT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | NULL _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | NUM _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | TRUE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72

and _menhir_goto_lvalue : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_lvalue -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState148 | MenhirState147 | MenhirState141 | MenhirState132 | MenhirState128 | MenhirState120 | MenhirState117 | MenhirState42 | MenhirState46 | MenhirState53 | MenhirState55 | MenhirState56 | MenhirState58 | MenhirState96 | MenhirState94 | MenhirState92 | MenhirState90 | MenhirState88 | MenhirState86 | MenhirState84 | MenhirState82 | MenhirState80 | MenhirState77 | MenhirState75 | MenhirState72 | MenhirState69 | MenhirState67 | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv553 * _menhir_state * 'tv_lvalue) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) _v
        | AND _ | COMMA _ | DIV _ | EQ _ | GEQ _ | GT _ | LEQ _ | LT _ | MINUS _ | NEQ _ | OR _ | PLUS _ | RBRACK _ | RPAREN _ | SEMICOLON _ | TIMES _ ->
            _menhir_reduce74 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv551 * _menhir_state * 'tv_lvalue) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv552)) : 'freshtv554)
    | MenhirState40 | MenhirState163 | MenhirState161 | MenhirState116 | MenhirState122 | MenhirState155 | MenhirState150 | MenhirState152 | MenhirState124 | MenhirState134 | MenhirState136 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv559 * _menhir_state * 'tv_lvalue) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSIGN _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv555 * _menhir_state * 'tv_lvalue) = Obj.magic _menhir_stack in
            let (_v : (
# 23 "core/parser.mly"
       (Support.Error.info)
# 2089 "core/parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FALSE _v ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
            | ID _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
            | LPAREN _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
            | MINUS _v ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
            | NEW _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
            | NOT _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
            | NULL _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
            | NUM _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
            | TRUE _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141) : 'freshtv556)
        | DOT _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) _v
        | AND _ | DIV _ | EQ _ | GEQ _ | GT _ | LEQ _ | LT _ | MINUS _ | NEQ _ | OR _ | PLUS _ | RPAREN _ | SEMICOLON _ | TIMES _ ->
            _menhir_reduce74 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv557 * _menhir_state * 'tv_lvalue) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv558)) : 'freshtv560)
    | _ ->
        _menhir_fail ()

and _menhir_goto_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_decl -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState124 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv543 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 2140 "core/parser.ml"
        )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 2144 "core/parser.ml"
        )) * _menhir_state * 'tv_decl) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FALSE _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
        | ID _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
        | LPAREN _v ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
        | MINUS _v ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
        | NEW _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
        | NOT _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
        | NULL _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
        | NUM _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
        | TRUE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState148) : 'freshtv544)
    | MenhirState40 | MenhirState163 | MenhirState161 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv545 * _menhir_state * 'tv_decl) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | FALSE _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | FOR _v ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | ID _v ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | IF _v ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | INT ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | LBRACE _v ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | LPAREN _v ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | MINUS _v ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | NEW _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | NOT _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | NULL _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | NUM _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | RETURN _v ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | TRUE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | VOID ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | WHILE _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | RBRACE _ ->
            _menhir_reduce84 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState163) : 'freshtv546)
    | MenhirState0 | MenhirState169 | MenhirState173 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv549 * _menhir_state * 'tv_decl) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv547 * _menhir_state * 'tv_decl) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (d : 'tv_decl)) = _menhir_stack in
        let _v : 'tv_gdecl = 
# 46 "core/parser.mly"
                                                  ( d )
# 2226 "core/parser.ml"
         in
        _menhir_goto_gdecl _menhir_env _menhir_stack _menhir_s _v) : 'freshtv548)) : 'freshtv550)
    | _ ->
        _menhir_fail ()

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 16 "core/parser.mly"
       (Support.Error.info)
# 2235 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | FALSE _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | FOR _v ->
        _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | ID _v ->
        _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | IF _v ->
        _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | INT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | LBRACE _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | LPAREN _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | MINUS _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | NEW _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | NOT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | NULL _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | NUM _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | RETURN _v ->
        _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | TRUE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | WHILE _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | RBRACE _ ->
        _menhir_reduce84 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_goto_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState147 | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv377 * _menhir_state * 'tv_id_with_lbrack) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | DIV _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) _v
        | EQ _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | GEQ _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | GT _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | LEQ _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | LT _v ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _v
        | MINUS _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | NEQ _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) _v
        | OR _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _v
        | PLUS _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) _v
        | RBRACK _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv373 * _menhir_state * 'tv_id_with_lbrack) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            let (_v : (
# 17 "core/parser.mly"
       (Support.Error.info)
# 2321 "core/parser.ml"
            )) = _v in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv371 * _menhir_state * 'tv_id_with_lbrack) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            let ((_3 : (
# 17 "core/parser.mly"
       (Support.Error.info)
# 2329 "core/parser.ml"
            )) : (
# 17 "core/parser.mly"
       (Support.Error.info)
# 2333 "core/parser.ml"
            )) = _v in
            ((let ((_menhir_stack, _menhir_s, (vi : 'tv_id_with_lbrack)), _, (e : 'tv_exp)) = _menhir_stack in
            let _v : 'tv_complvalue = 
# 203 "core/parser.mly"
                                                  ( let {v;i}=vi in A.SubscriptVar(A.SimpVar(v,i),e,i) )
# 2339 "core/parser.ml"
             in
            _menhir_goto_complvalue _menhir_env _menhir_stack _menhir_s _v) : 'freshtv372)) : 'freshtv374)
        | TIMES _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv375 * _menhir_state * 'tv_id_with_lbrack) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv376)) : 'freshtv378)
    | MenhirState69 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv381 * _menhir_state * 'tv_exp) * (
# 20 "core/parser.mly"
       (Support.Error.info)
# 2356 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv379 * _menhir_state * 'tv_exp) * (
# 20 "core/parser.mly"
       (Support.Error.info)
# 2362 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_exp)), (i : (
# 20 "core/parser.mly"
       (Support.Error.info)
# 2367 "core/parser.ml"
        ))), _, (e2 : 'tv_exp)) = _menhir_stack in
        let _v : 'tv_compexp = let op = 
# 257 "core/parser.mly"
                                                  ( A.Times,i )
# 2372 "core/parser.ml"
         in
        
# 225 "core/parser.mly"
                                                  ( A.Bin(e1,fst op,e2,snd op) )
# 2377 "core/parser.ml"
         in
        _menhir_goto_compexp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv380)) : 'freshtv382)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv389 * _menhir_state * 'tv_complvalue) * (
# 17 "core/parser.mly"
       (Support.Error.info)
# 2385 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | DIV _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) _v
        | EQ _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | GEQ _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | GT _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | LEQ _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | LT _v ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _v
        | MINUS _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | NEQ _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) _v
        | OR _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _v
        | PLUS _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) _v
        | RBRACK _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv385 * _menhir_state * 'tv_complvalue) * (
# 17 "core/parser.mly"
       (Support.Error.info)
# 2417 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            let (_v : (
# 17 "core/parser.mly"
       (Support.Error.info)
# 2422 "core/parser.ml"
            )) = _v in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv383 * _menhir_state * 'tv_complvalue) * (
# 17 "core/parser.mly"
       (Support.Error.info)
# 2429 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            let ((_4 : (
# 17 "core/parser.mly"
       (Support.Error.info)
# 2434 "core/parser.ml"
            )) : (
# 17 "core/parser.mly"
       (Support.Error.info)
# 2438 "core/parser.ml"
            )) = _v in
            ((let (((_menhir_stack, _menhir_s, (l : 'tv_complvalue)), (_2 : (
# 17 "core/parser.mly"
       (Support.Error.info)
# 2443 "core/parser.ml"
            ))), _, (e : 'tv_exp)) = _menhir_stack in
            let _v : 'tv_complvalue = 
# 202 "core/parser.mly"
                                                  ( A.SubscriptVar(l,e,A.Util.extract_info_var l) )
# 2448 "core/parser.ml"
             in
            _menhir_goto_complvalue _menhir_env _menhir_stack _menhir_s _v) : 'freshtv384)) : 'freshtv386)
        | TIMES _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv387 * _menhir_state * 'tv_complvalue) * (
# 17 "core/parser.mly"
       (Support.Error.info)
# 2460 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv388)) : 'freshtv390)
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv395 * _menhir_state * 'tv_exp) * (
# 20 "core/parser.mly"
       (Support.Error.info)
# 2469 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) _v
        | TIMES _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _v
        | AND _ | COMMA _ | EQ _ | GEQ _ | GT _ | LEQ _ | LT _ | MINUS _ | NEQ _ | OR _ | PLUS _ | RBRACK _ | RPAREN _ | SEMICOLON _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv391 * _menhir_state * 'tv_exp) * (
# 20 "core/parser.mly"
       (Support.Error.info)
# 2483 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_exp)), (i : (
# 20 "core/parser.mly"
       (Support.Error.info)
# 2488 "core/parser.ml"
            ))), _, (e2 : 'tv_exp)) = _menhir_stack in
            let _v : 'tv_compexp = let op = 
# 255 "core/parser.mly"
                                                  ( A.Plus,i )
# 2493 "core/parser.ml"
             in
            
# 225 "core/parser.mly"
                                                  ( A.Bin(e1,fst op,e2,snd op) )
# 2498 "core/parser.ml"
             in
            _menhir_goto_compexp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv392)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv393 * _menhir_state * 'tv_exp) * (
# 20 "core/parser.mly"
       (Support.Error.info)
# 2508 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv394)) : 'freshtv396)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv399 * _menhir_state * 'tv_exp) * (
# 20 "core/parser.mly"
       (Support.Error.info)
# 2517 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv397 * _menhir_state * 'tv_exp) * (
# 20 "core/parser.mly"
       (Support.Error.info)
# 2523 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_exp)), (i : (
# 20 "core/parser.mly"
       (Support.Error.info)
# 2528 "core/parser.ml"
        ))), _, (e2 : 'tv_exp)) = _menhir_stack in
        let _v : 'tv_compexp = let op = 
# 258 "core/parser.mly"
                                                  ( A.Div,i )
# 2533 "core/parser.ml"
         in
        
# 225 "core/parser.mly"
                                                  ( A.Bin(e1,fst op,e2,snd op) )
# 2538 "core/parser.ml"
         in
        _menhir_goto_compexp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv398)) : 'freshtv400)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv405 * _menhir_state * 'tv_exp) * (
# 21 "core/parser.mly"
       (Support.Error.info)
# 2546 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | DIV _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) _v
        | EQ _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | GEQ _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | GT _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | LEQ _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | LT _v ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _v
        | MINUS _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | NEQ _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) _v
        | PLUS _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) _v
        | TIMES _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _v
        | COMMA _ | OR _ | RBRACK _ | RPAREN _ | SEMICOLON _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv401 * _menhir_state * 'tv_exp) * (
# 21 "core/parser.mly"
       (Support.Error.info)
# 2578 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_exp)), (i : (
# 21 "core/parser.mly"
       (Support.Error.info)
# 2583 "core/parser.ml"
            ))), _, (e2 : 'tv_exp)) = _menhir_stack in
            let _v : 'tv_compexp = let op = 
# 263 "core/parser.mly"
                                                  ( A.Or,i )
# 2588 "core/parser.ml"
             in
            
# 225 "core/parser.mly"
                                                  ( A.Bin(e1,fst op,e2,snd op) )
# 2593 "core/parser.ml"
             in
            _menhir_goto_compexp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv402)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv403 * _menhir_state * 'tv_exp) * (
# 21 "core/parser.mly"
       (Support.Error.info)
# 2603 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv404)) : 'freshtv406)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv411 * _menhir_state * 'tv_exp) * (
# 23 "core/parser.mly"
       (Support.Error.info)
# 2612 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) _v
        | GEQ _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | GT _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | LEQ _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | LT _v ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _v
        | MINUS _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | PLUS _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) _v
        | TIMES _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _v
        | AND _ | COMMA _ | EQ _ | NEQ _ | OR _ | RBRACK _ | RPAREN _ | SEMICOLON _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv407 * _menhir_state * 'tv_exp) * (
# 23 "core/parser.mly"
       (Support.Error.info)
# 2638 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_exp)), (i : (
# 23 "core/parser.mly"
       (Support.Error.info)
# 2643 "core/parser.ml"
            ))), _, (e2 : 'tv_exp)) = _menhir_stack in
            let _v : 'tv_compexp = 
# 228 "core/parser.mly"
                                                  ( A.Un(A.Not, A.Bin(e1,A.Eq,e2,i), i) )
# 2648 "core/parser.ml"
             in
            _menhir_goto_compexp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv408)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv409 * _menhir_state * 'tv_exp) * (
# 23 "core/parser.mly"
       (Support.Error.info)
# 2658 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv410)) : 'freshtv412)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv417 * _menhir_state * 'tv_exp) * (
# 20 "core/parser.mly"
       (Support.Error.info)
# 2667 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) _v
        | TIMES _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _v
        | AND _ | COMMA _ | EQ _ | GEQ _ | GT _ | LEQ _ | LT _ | MINUS _ | NEQ _ | OR _ | PLUS _ | RBRACK _ | RPAREN _ | SEMICOLON _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv413 * _menhir_state * 'tv_exp) * (
# 20 "core/parser.mly"
       (Support.Error.info)
# 2681 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_exp)), (i : (
# 20 "core/parser.mly"
       (Support.Error.info)
# 2686 "core/parser.ml"
            ))), _, (e2 : 'tv_exp)) = _menhir_stack in
            let _v : 'tv_compexp = let op = 
# 256 "core/parser.mly"
                                                  ( A.Minus,i )
# 2691 "core/parser.ml"
             in
            
# 225 "core/parser.mly"
                                                  ( A.Bin(e1,fst op,e2,snd op) )
# 2696 "core/parser.ml"
             in
            _menhir_goto_compexp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv414)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv415 * _menhir_state * 'tv_exp) * (
# 20 "core/parser.mly"
       (Support.Error.info)
# 2706 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv416)) : 'freshtv418)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv423 * _menhir_state * 'tv_exp) * (
# 23 "core/parser.mly"
       (Support.Error.info)
# 2715 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) _v
        | MINUS _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | PLUS _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) _v
        | TIMES _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _v
        | AND _ | COMMA _ | EQ _ | GEQ _ | GT _ | LEQ _ | LT _ | NEQ _ | OR _ | RBRACK _ | RPAREN _ | SEMICOLON _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv419 * _menhir_state * 'tv_exp) * (
# 23 "core/parser.mly"
       (Support.Error.info)
# 2733 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_exp)), (i : (
# 23 "core/parser.mly"
       (Support.Error.info)
# 2738 "core/parser.ml"
            ))), _, (e2 : 'tv_exp)) = _menhir_stack in
            let _v : 'tv_compexp = let op = 
# 259 "core/parser.mly"
                                                  ( A.Lt,i )
# 2743 "core/parser.ml"
             in
            
# 225 "core/parser.mly"
                                                  ( A.Bin(e1,fst op,e2,snd op) )
# 2748 "core/parser.ml"
             in
            _menhir_goto_compexp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv420)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv421 * _menhir_state * 'tv_exp) * (
# 23 "core/parser.mly"
       (Support.Error.info)
# 2758 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv422)) : 'freshtv424)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv429 * _menhir_state * 'tv_exp) * (
# 23 "core/parser.mly"
       (Support.Error.info)
# 2767 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) _v
        | MINUS _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | PLUS _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) _v
        | TIMES _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _v
        | AND _ | COMMA _ | EQ _ | GEQ _ | GT _ | LEQ _ | LT _ | NEQ _ | OR _ | RBRACK _ | RPAREN _ | SEMICOLON _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv425 * _menhir_state * 'tv_exp) * (
# 23 "core/parser.mly"
       (Support.Error.info)
# 2785 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_exp)), (i : (
# 23 "core/parser.mly"
       (Support.Error.info)
# 2790 "core/parser.ml"
            ))), _, (e2 : 'tv_exp)) = _menhir_stack in
            let _v : 'tv_compexp = 
# 226 "core/parser.mly"
                                                  ( A.Un(A.Not, A.Bin(e1,A.Gt,e2,i), i) )
# 2795 "core/parser.ml"
             in
            _menhir_goto_compexp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv426)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv427 * _menhir_state * 'tv_exp) * (
# 23 "core/parser.mly"
       (Support.Error.info)
# 2805 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv428)) : 'freshtv430)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv435 * _menhir_state * 'tv_exp) * (
# 23 "core/parser.mly"
       (Support.Error.info)
# 2814 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) _v
        | MINUS _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | PLUS _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) _v
        | TIMES _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _v
        | AND _ | COMMA _ | EQ _ | GEQ _ | GT _ | LEQ _ | LT _ | NEQ _ | OR _ | RBRACK _ | RPAREN _ | SEMICOLON _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv431 * _menhir_state * 'tv_exp) * (
# 23 "core/parser.mly"
       (Support.Error.info)
# 2832 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_exp)), (i : (
# 23 "core/parser.mly"
       (Support.Error.info)
# 2837 "core/parser.ml"
            ))), _, (e2 : 'tv_exp)) = _menhir_stack in
            let _v : 'tv_compexp = let op = 
# 260 "core/parser.mly"
                                                  ( A.Gt,i )
# 2842 "core/parser.ml"
             in
            
# 225 "core/parser.mly"
                                                  ( A.Bin(e1,fst op,e2,snd op) )
# 2847 "core/parser.ml"
             in
            _menhir_goto_compexp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv432)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv433 * _menhir_state * 'tv_exp) * (
# 23 "core/parser.mly"
       (Support.Error.info)
# 2857 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv434)) : 'freshtv436)
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv441 * _menhir_state * 'tv_exp) * (
# 23 "core/parser.mly"
       (Support.Error.info)
# 2866 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) _v
        | MINUS _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | PLUS _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) _v
        | TIMES _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _v
        | AND _ | COMMA _ | EQ _ | GEQ _ | GT _ | LEQ _ | LT _ | NEQ _ | OR _ | RBRACK _ | RPAREN _ | SEMICOLON _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv437 * _menhir_state * 'tv_exp) * (
# 23 "core/parser.mly"
       (Support.Error.info)
# 2884 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_exp)), (i : (
# 23 "core/parser.mly"
       (Support.Error.info)
# 2889 "core/parser.ml"
            ))), _, (e2 : 'tv_exp)) = _menhir_stack in
            let _v : 'tv_compexp = 
# 227 "core/parser.mly"
                                                  ( A.Un(A.Not, A.Bin(e1,A.Lt,e2,i), i) )
# 2894 "core/parser.ml"
             in
            _menhir_goto_compexp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv438)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv439 * _menhir_state * 'tv_exp) * (
# 23 "core/parser.mly"
       (Support.Error.info)
# 2904 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv440)) : 'freshtv442)
    | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv447 * _menhir_state * 'tv_exp) * (
# 23 "core/parser.mly"
       (Support.Error.info)
# 2913 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) _v
        | GEQ _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | GT _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | LEQ _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | LT _v ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _v
        | MINUS _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | PLUS _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) _v
        | TIMES _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _v
        | AND _ | COMMA _ | EQ _ | NEQ _ | OR _ | RBRACK _ | RPAREN _ | SEMICOLON _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv443 * _menhir_state * 'tv_exp) * (
# 23 "core/parser.mly"
       (Support.Error.info)
# 2939 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_exp)), (i : (
# 23 "core/parser.mly"
       (Support.Error.info)
# 2944 "core/parser.ml"
            ))), _, (e2 : 'tv_exp)) = _menhir_stack in
            let _v : 'tv_compexp = let op = 
# 261 "core/parser.mly"
                                                  ( A.Eq,i )
# 2949 "core/parser.ml"
             in
            
# 225 "core/parser.mly"
                                                  ( A.Bin(e1,fst op,e2,snd op) )
# 2954 "core/parser.ml"
             in
            _menhir_goto_compexp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv444)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv445 * _menhir_state * 'tv_exp) * (
# 23 "core/parser.mly"
       (Support.Error.info)
# 2964 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv446)) : 'freshtv448)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv453 * _menhir_state * 'tv_exp) * (
# 21 "core/parser.mly"
       (Support.Error.info)
# 2973 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) _v
        | EQ _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | GEQ _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | GT _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | LEQ _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | LT _v ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _v
        | MINUS _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | NEQ _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) _v
        | PLUS _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) _v
        | TIMES _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _v
        | AND _ | COMMA _ | OR _ | RBRACK _ | RPAREN _ | SEMICOLON _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv449 * _menhir_state * 'tv_exp) * (
# 21 "core/parser.mly"
       (Support.Error.info)
# 3003 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_exp)), (i : (
# 21 "core/parser.mly"
       (Support.Error.info)
# 3008 "core/parser.ml"
            ))), _, (e2 : 'tv_exp)) = _menhir_stack in
            let _v : 'tv_compexp = let op = 
# 262 "core/parser.mly"
                                                  ( A.And,i )
# 3013 "core/parser.ml"
             in
            
# 225 "core/parser.mly"
                                                  ( A.Bin(e1,fst op,e2,snd op) )
# 3018 "core/parser.ml"
             in
            _menhir_goto_compexp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv450)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv451 * _menhir_state * 'tv_exp) * (
# 21 "core/parser.mly"
       (Support.Error.info)
# 3028 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv452)) : 'freshtv454)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv459 * _menhir_state * 'tv_rev_arg_list) * (
# 14 "core/parser.mly"
       (Support.Error.info)
# 3037 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | DIV _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) _v
        | EQ _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | GEQ _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | GT _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | LEQ _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | LT _v ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _v
        | MINUS _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | NEQ _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) _v
        | OR _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _v
        | PLUS _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) _v
        | TIMES _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _v
        | COMMA _ | RPAREN _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv455 * _menhir_state * 'tv_rev_arg_list) * (
# 14 "core/parser.mly"
       (Support.Error.info)
# 3071 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (rl : 'tv_rev_arg_list)), (_2 : (
# 14 "core/parser.mly"
       (Support.Error.info)
# 3076 "core/parser.ml"
            ))), _, (e : 'tv_exp)) = _menhir_stack in
            let _v : 'tv_rev_arg_list = 
# 247 "core/parser.mly"
                                                  ( e::rl )
# 3081 "core/parser.ml"
             in
            _menhir_goto_rev_arg_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv456)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv457 * _menhir_state * 'tv_rev_arg_list) * (
# 14 "core/parser.mly"
       (Support.Error.info)
# 3091 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv458)) : 'freshtv460)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv465 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | DIV _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) _v
        | EQ _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | GEQ _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | GT _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | LEQ _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | LT _v ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _v
        | MINUS _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | NEQ _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) _v
        | OR _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _v
        | PLUS _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) _v
        | TIMES _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _v
        | COMMA _ | RPAREN _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv461 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (e : 'tv_exp)) = _menhir_stack in
            let _v : 'tv_rev_arg_list = 
# 246 "core/parser.mly"
                                                  ( [e] )
# 3132 "core/parser.ml"
             in
            _menhir_goto_rev_arg_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv462)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv463 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv464)) : 'freshtv466)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv469 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | DIV _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) _v
        | EQ _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | GEQ _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | GT _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | LEQ _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | LT _v ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _v
        | MINUS _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | NEQ _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) _v
        | OR _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _v
        | PLUS _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) _v
        | TIMES _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv467 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv468)) : 'freshtv470)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv473 * _menhir_state * (
# 20 "core/parser.mly"
       (Support.Error.info)
# 3184 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv471 * _menhir_state * (
# 20 "core/parser.mly"
       (Support.Error.info)
# 3190 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (i : (
# 20 "core/parser.mly"
       (Support.Error.info)
# 3195 "core/parser.ml"
        ))), _, (e : 'tv_exp)) = _menhir_stack in
        let _v : 'tv_compexp = 
# 224 "core/parser.mly"
                                                  ( A.Bin(A.Intconst(0,dummyinfo),A.Minus,e,i) )
# 3200 "core/parser.ml"
         in
        _menhir_goto_compexp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv472)) : 'freshtv474)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv487 * _menhir_state * 'tv_array_alloc_prefix) * (
# 17 "core/parser.mly"
       (Support.Error.info)
# 3208 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | DIV _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) _v
        | EQ _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | GEQ _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | GT _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | LEQ _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | LT _v ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _v
        | MINUS _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | NEQ _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) _v
        | OR _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _v
        | PLUS _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) _v
        | RBRACK _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv483 * _menhir_state * 'tv_array_alloc_prefix) * (
# 17 "core/parser.mly"
       (Support.Error.info)
# 3240 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            let (_v : (
# 17 "core/parser.mly"
       (Support.Error.info)
# 3245 "core/parser.ml"
            )) = _v in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv481 * _menhir_state * 'tv_array_alloc_prefix) * (
# 17 "core/parser.mly"
       (Support.Error.info)
# 3252 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            let ((_4 : (
# 17 "core/parser.mly"
       (Support.Error.info)
# 3257 "core/parser.ml"
            )) : (
# 17 "core/parser.mly"
       (Support.Error.info)
# 3261 "core/parser.ml"
            )) = _v in
            ((let (((_menhir_stack, _menhir_s, (t : 'tv_array_alloc_prefix)), (_2 : (
# 17 "core/parser.mly"
       (Support.Error.info)
# 3266 "core/parser.ml"
            ))), _, (e : 'tv_exp)) = _menhir_stack in
            let _v : 'tv_array_alloc = 
# 236 "core/parser.mly"
                                                  ( A.ArrayAlloc(t, e, A.Util.extract_info_exp e) )
# 3271 "core/parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv479) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_array_alloc) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv477 * _menhir_state * (
# 25 "core/parser.mly"
       (Support.Error.info)
# 3281 "core/parser.ml"
            )) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_array_alloc) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv475 * _menhir_state * (
# 25 "core/parser.mly"
       (Support.Error.info)
# 3289 "core/parser.ml"
            )) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            let ((e : 'tv_array_alloc) : 'tv_array_alloc) = _v in
            ((let (_menhir_stack, _menhir_s, (_1 : (
# 25 "core/parser.mly"
       (Support.Error.info)
# 3296 "core/parser.ml"
            ))) = _menhir_stack in
            let _v : 'tv_compexp = 
# 230 "core/parser.mly"
                                                  ( e )
# 3301 "core/parser.ml"
             in
            _menhir_goto_compexp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv476)) : 'freshtv478)) : 'freshtv480)) : 'freshtv482)) : 'freshtv484)
        | TIMES _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv485 * _menhir_state * 'tv_array_alloc_prefix) * (
# 17 "core/parser.mly"
       (Support.Error.info)
# 3313 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv486)) : 'freshtv488)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv491 * _menhir_state * (
# 22 "core/parser.mly"
       (Support.Error.info)
# 3322 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv489 * _menhir_state * (
# 22 "core/parser.mly"
       (Support.Error.info)
# 3328 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (i : (
# 22 "core/parser.mly"
       (Support.Error.info)
# 3333 "core/parser.ml"
        ))), _, (e : 'tv_exp)) = _menhir_stack in
        let _v : 'tv_compexp = 
# 223 "core/parser.mly"
                                                  ( A.Un(A.Not,e,i) )
# 3338 "core/parser.ml"
         in
        _menhir_goto_compexp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv490)) : 'freshtv492)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv497 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 3346 "core/parser.ml"
        )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 3350 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | DIV _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) _v
        | EQ _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | GEQ _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | GT _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | LEQ _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | LT _v ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _v
        | MINUS _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | NEQ _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) _v
        | OR _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _v
        | PLUS _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) _v
        | RPAREN _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv493 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 3382 "core/parser.ml"
            )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 3386 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            let (_v : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 3391 "core/parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FALSE _v ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | FOR _v ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | ID _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | IF _v ->
                _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | LBRACE _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | LPAREN _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | MINUS _v ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | NEW _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | NOT _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | NULL _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | NUM _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | RETURN _v ->
                _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | TRUE _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | WHILE _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116) : 'freshtv494)
        | TIMES _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv495 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 3438 "core/parser.ml"
            )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 3442 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv496)) : 'freshtv498)
    | MenhirState117 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv503 * _menhir_state * (
# 13 "core/parser.mly"
       (Support.Error.info)
# 3451 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | DIV _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) _v
        | EQ _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | GEQ _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | GT _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | LEQ _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | LT _v ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _v
        | MINUS _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | NEQ _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) _v
        | OR _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _v
        | PLUS _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) _v
        | TIMES _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _v
        | RPAREN _ | SEMICOLON _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv499 * _menhir_state * (
# 13 "core/parser.mly"
       (Support.Error.info)
# 3485 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (i : (
# 13 "core/parser.mly"
       (Support.Error.info)
# 3490 "core/parser.ml"
            ))), _, (e : 'tv_exp)) = _menhir_stack in
            let _v : 'tv_simp = 
# 151 "core/parser.mly"
                                                  ( A.Return(e,i) )
# 3495 "core/parser.ml"
             in
            _menhir_goto_simp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv500)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv501 * _menhir_state * (
# 13 "core/parser.mly"
       (Support.Error.info)
# 3505 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv502)) : 'freshtv504)
    | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv509 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 3514 "core/parser.ml"
        )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 3518 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | DIV _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) _v
        | EQ _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | GEQ _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | GT _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | LEQ _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | LT _v ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _v
        | MINUS _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | NEQ _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) _v
        | OR _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _v
        | PLUS _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) _v
        | RPAREN _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv505 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 3550 "core/parser.ml"
            )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 3554 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            let (_v : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 3559 "core/parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FALSE _v ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
            | FOR _v ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
            | ID _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
            | IF _v ->
                _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
            | LBRACE _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
            | LPAREN _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
            | MINUS _v ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
            | NEW _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
            | NOT _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
            | NULL _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
            | NUM _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
            | RETURN _v ->
                _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
            | TRUE _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
            | WHILE _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122) : 'freshtv506)
        | TIMES _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv507 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 3606 "core/parser.ml"
            )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 3610 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv508)) : 'freshtv510)
    | MenhirState128 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv517 * _menhir_state * 'tv_ty) * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 3619 "core/parser.ml"
        )) * (
# 23 "core/parser.mly"
       (Support.Error.info)
# 3623 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | DIV _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) _v
        | EQ _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | GEQ _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | GT _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | LEQ _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | LT _v ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _v
        | MINUS _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | NEQ _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) _v
        | OR _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _v
        | PLUS _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) _v
        | SEMICOLON _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv513 * _menhir_state * 'tv_ty) * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 3655 "core/parser.ml"
            )) * (
# 23 "core/parser.mly"
       (Support.Error.info)
# 3659 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            let (_v : (
# 14 "core/parser.mly"
       (Support.Error.info)
# 3664 "core/parser.ml"
            )) = _v in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv511 * _menhir_state * 'tv_ty) * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 3671 "core/parser.ml"
            )) * (
# 23 "core/parser.mly"
       (Support.Error.info)
# 3675 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            let ((_5 : (
# 14 "core/parser.mly"
       (Support.Error.info)
# 3680 "core/parser.ml"
            )) : (
# 14 "core/parser.mly"
       (Support.Error.info)
# 3684 "core/parser.ml"
            )) = _v in
            ((let ((((_menhir_stack, _menhir_s, (t : 'tv_ty)), (id : (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 3689 "core/parser.ml"
            ))), (ii : (
# 23 "core/parser.mly"
       (Support.Error.info)
# 3693 "core/parser.ml"
            ))), _, (e : 'tv_exp)) = _menhir_stack in
            let _v : 'tv_decl = 
# 132 "core/parser.mly"
    ( let {v;i}=id in 
      fun s ->
      A.Vardecl(v,t,A.Seq([A.Assign(A.SimpVar(v,i),e,ii);s],i),i) )
# 3700 "core/parser.ml"
             in
            _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v) : 'freshtv512)) : 'freshtv514)
        | TIMES _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv515 * _menhir_state * 'tv_ty) * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 3712 "core/parser.ml"
            )) * (
# 23 "core/parser.mly"
       (Support.Error.info)
# 3716 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv516)) : 'freshtv518)
    | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv523 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 3725 "core/parser.ml"
        )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 3729 "core/parser.ml"
        )) * _menhir_state * 'tv_simpopt) * (
# 14 "core/parser.mly"
       (Support.Error.info)
# 3733 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | DIV _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) _v
        | EQ _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | GEQ _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | GT _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | LEQ _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | LT _v ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _v
        | MINUS _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | NEQ _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) _v
        | OR _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _v
        | PLUS _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) _v
        | SEMICOLON _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv519 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 3765 "core/parser.ml"
            )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 3769 "core/parser.ml"
            )) * _menhir_state * 'tv_simpopt) * (
# 14 "core/parser.mly"
       (Support.Error.info)
# 3773 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            let (_v : (
# 14 "core/parser.mly"
       (Support.Error.info)
# 3778 "core/parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FALSE _v ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
            | ID _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
            | LPAREN _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
            | MINUS _v ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
            | NEW _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
            | NOT _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
            | NULL _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
            | NUM _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
            | RETURN _v ->
                _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
            | TRUE _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
            | RPAREN _ ->
                _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState134
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134) : 'freshtv520)
        | TIMES _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv521 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 3819 "core/parser.ml"
            )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 3823 "core/parser.ml"
            )) * _menhir_state * 'tv_simpopt) * (
# 14 "core/parser.mly"
       (Support.Error.info)
# 3827 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv522)) : 'freshtv524)
    | MenhirState141 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv529 * _menhir_state * 'tv_lvalue) * (
# 23 "core/parser.mly"
       (Support.Error.info)
# 3836 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | DIV _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) _v
        | EQ _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | GEQ _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | GT _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | LEQ _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | LT _v ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _v
        | MINUS _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | NEQ _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) _v
        | OR _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _v
        | PLUS _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) _v
        | TIMES _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _v
        | RPAREN _ | SEMICOLON _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv525 * _menhir_state * 'tv_lvalue) * (
# 23 "core/parser.mly"
       (Support.Error.info)
# 3870 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (l : 'tv_lvalue)), (ii : (
# 23 "core/parser.mly"
       (Support.Error.info)
# 3875 "core/parser.ml"
            ))), _, (e : 'tv_exp)) = _menhir_stack in
            let _v : 'tv_simp = 
# 150 "core/parser.mly"
                                                  ( A.Assign(l,e,ii) )
# 3880 "core/parser.ml"
             in
            _menhir_goto_simp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv526)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv527 * _menhir_state * 'tv_lvalue) * (
# 23 "core/parser.mly"
       (Support.Error.info)
# 3890 "core/parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv528)) : 'freshtv530)
    | MenhirState40 | MenhirState163 | MenhirState161 | MenhirState116 | MenhirState122 | MenhirState155 | MenhirState150 | MenhirState152 | MenhirState124 | MenhirState134 | MenhirState136 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv535 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | DIV _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) _v
        | EQ _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | GEQ _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | GT _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | LEQ _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | LT _v ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _v
        | MINUS _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | NEQ _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) _v
        | OR _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _v
        | PLUS _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) _v
        | TIMES _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _v
        | RPAREN _ | SEMICOLON _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv531 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (e : 'tv_exp)) = _menhir_stack in
            let _v : 'tv_simp = 
# 149 "core/parser.mly"
                                                  ( A.Exp(e, A.Util.extract_info_exp e) )
# 3931 "core/parser.ml"
             in
            _menhir_goto_simp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv532)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv533 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv534)) : 'freshtv536)
    | MenhirState148 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv541 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 3946 "core/parser.ml"
        )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 3950 "core/parser.ml"
        )) * _menhir_state * 'tv_decl) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | DIV _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) _v
        | EQ _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | GEQ _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | GT _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | LEQ _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | LT _v ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _v
        | MINUS _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | NEQ _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) _v
        | OR _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _v
        | PLUS _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) _v
        | SEMICOLON _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv537 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 3982 "core/parser.ml"
            )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 3986 "core/parser.ml"
            )) * _menhir_state * 'tv_decl) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            let (_v : (
# 14 "core/parser.mly"
       (Support.Error.info)
# 3991 "core/parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FALSE _v ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
            | ID _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
            | LPAREN _v ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
            | MINUS _v ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
            | NEW _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
            | NOT _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
            | NULL _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
            | NUM _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
            | RETURN _v ->
                _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
            | TRUE _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
            | RPAREN _ ->
                _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState150) : 'freshtv538)
        | TIMES _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv539 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 4032 "core/parser.ml"
            )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4036 "core/parser.ml"
            )) * _menhir_state * 'tv_decl) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv540)) : 'freshtv542)
    | _ ->
        _menhir_fail ()

and _menhir_goto_complvalue : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_complvalue -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState40 | MenhirState161 | MenhirState163 | MenhirState42 | MenhirState116 | MenhirState120 | MenhirState122 | MenhirState155 | MenhirState124 | MenhirState148 | MenhirState150 | MenhirState152 | MenhirState147 | MenhirState132 | MenhirState134 | MenhirState136 | MenhirState141 | MenhirState128 | MenhirState117 | MenhirState46 | MenhirState53 | MenhirState55 | MenhirState58 | MenhirState63 | MenhirState67 | MenhirState72 | MenhirState80 | MenhirState96 | MenhirState94 | MenhirState82 | MenhirState92 | MenhirState90 | MenhirState88 | MenhirState86 | MenhirState84 | MenhirState75 | MenhirState77 | MenhirState69 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv361 * _menhir_state * 'tv_complvalue) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRACK _v ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) _v
        | AND _ | ASSIGN _ | COMMA _ | DIV _ | DOT _ | EQ _ | GEQ _ | GT _ | LEQ _ | LT _ | MINUS _ | NEQ _ | OR _ | PLUS _ | RBRACK _ | RPAREN _ | SEMICOLON _ | TIMES _ ->
            _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv359 * _menhir_state * 'tv_complvalue) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv360)) : 'freshtv362)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv369 * _menhir_state * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4069 "core/parser.ml"
        )) * _menhir_state * 'tv_complvalue) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRACK _v ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) _v
        | RPAREN _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv365 * _menhir_state * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4081 "core/parser.ml"
            )) * _menhir_state * 'tv_complvalue) = Obj.magic _menhir_stack in
            let (_v : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4086 "core/parser.ml"
            )) = _v in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv363 * _menhir_state * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4093 "core/parser.ml"
            )) * _menhir_state * 'tv_complvalue) = Obj.magic _menhir_stack in
            let ((_3 : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4098 "core/parser.ml"
            )) : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4102 "core/parser.ml"
            )) = _v in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4107 "core/parser.ml"
            ))), _, (l : 'tv_complvalue)) = _menhir_stack in
            let _v : 'tv_complvalue = 
# 200 "core/parser.mly"
                                                  ( l )
# 4112 "core/parser.ml"
             in
            _menhir_goto_complvalue _menhir_env _menhir_stack _menhir_s _v) : 'freshtv364)) : 'freshtv366)
        | AND _ | DIV _ | DOT _ | EQ _ | GEQ _ | GT _ | LEQ _ | LT _ | MINUS _ | NEQ _ | OR _ | PLUS _ | TIMES _ ->
            _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv367 * _menhir_state * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4124 "core/parser.ml"
            )) * _menhir_state * 'tv_complvalue) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv368)) : 'freshtv370)
    | _ ->
        _menhir_fail ()

and _menhir_reduce55 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_simplvalue -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (l : 'tv_simplvalue)) = _menhir_stack in
    let _v : 'tv_lvalue = 
# 192 "core/parser.mly"
                                                    ( l )
# 4137 "core/parser.ml"
     in
    _menhir_goto_lvalue _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_arg_list : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_arg_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv357 * _menhir_state * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 4148 "core/parser.ml"
    )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4152 "core/parser.ml"
    )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv353 * _menhir_state * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 4162 "core/parser.ml"
        )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4166 "core/parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        let (_v : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4171 "core/parser.ml"
        )) = _v in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv351 * _menhir_state * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 4178 "core/parser.ml"
        )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4182 "core/parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        let ((_4 : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4187 "core/parser.ml"
        )) : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4191 "core/parser.ml"
        )) = _v in
        ((let (((_menhir_stack, _menhir_s, (id : (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 4196 "core/parser.ml"
        ))), (_2 : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4200 "core/parser.ml"
        ))), _, (al : 'tv_arg_list)) = _menhir_stack in
        let _v : 'tv_compexp = 
# 229 "core/parser.mly"
                                                  ( let {v;i}=id in A.App(v,al,i) )
# 4205 "core/parser.ml"
         in
        _menhir_goto_compexp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv352)) : 'freshtv354)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv355 * _menhir_state * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 4215 "core/parser.ml"
        )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4219 "core/parser.ml"
        )) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv356)) : 'freshtv358)

and _menhir_goto_array_alloc_prefix : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_array_alloc_prefix -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv349 * _menhir_state * 'tv_array_alloc_prefix) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACK _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv345 * _menhir_state * 'tv_array_alloc_prefix) = Obj.magic _menhir_stack in
        let (_v : (
# 17 "core/parser.mly"
       (Support.Error.info)
# 4238 "core/parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FALSE _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | ID _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | LPAREN _v ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | MINUS _v ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | NEW _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | NOT _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | NULL _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | NUM _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | RBRACK _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv343 * _menhir_state * 'tv_array_alloc_prefix) * (
# 17 "core/parser.mly"
       (Support.Error.info)
# 4265 "core/parser.ml"
            )) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState53 in
            let (_v : (
# 17 "core/parser.mly"
       (Support.Error.info)
# 4271 "core/parser.ml"
            )) = _v in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv341 * _menhir_state * 'tv_array_alloc_prefix) * (
# 17 "core/parser.mly"
       (Support.Error.info)
# 4278 "core/parser.ml"
            )) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            let ((_3 : (
# 17 "core/parser.mly"
       (Support.Error.info)
# 4284 "core/parser.ml"
            )) : (
# 17 "core/parser.mly"
       (Support.Error.info)
# 4288 "core/parser.ml"
            )) = _v in
            ((let ((_menhir_stack, _menhir_s, (t : 'tv_array_alloc_prefix)), (_2 : (
# 17 "core/parser.mly"
       (Support.Error.info)
# 4293 "core/parser.ml"
            ))) = _menhir_stack in
            let _v : 'tv_array_alloc_prefix = 
# 241 "core/parser.mly"
                                                  ( A.ArrayTy(t) )
# 4298 "core/parser.ml"
             in
            _menhir_goto_array_alloc_prefix _menhir_env _menhir_stack _menhir_s _v) : 'freshtv342)) : 'freshtv344)
        | TRUE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53) : 'freshtv346)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv347 * _menhir_state * 'tv_array_alloc_prefix) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv348)) : 'freshtv350)

and _menhir_goto_rev_param_list : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_rev_param_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv339 * _menhir_state * 'tv_rev_param_list) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv333 * _menhir_state * 'tv_rev_param_list) = Obj.magic _menhir_stack in
        let (_v : (
# 14 "core/parser.mly"
       (Support.Error.info)
# 4329 "core/parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | ID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | INT ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | VOID ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34) : 'freshtv334)
    | RPAREN _ ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv335 * _menhir_state * 'tv_rev_param_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (l : 'tv_rev_param_list)) = _menhir_stack in
        let _v : 'tv_param_list = 
# 88 "core/parser.mly"
                                                  ( List.rev l )
# 4354 "core/parser.ml"
         in
        _menhir_goto_param_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv336)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv337 * _menhir_state * 'tv_rev_param_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv338)) : 'freshtv340)

and _menhir_run29 : _menhir_env -> ('ttv_tail * _menhir_state * 'tv_ty) * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 4368 "core/parser.ml"
) -> (
# 14 "core/parser.mly"
       (Support.Error.info)
# 4372 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv331 * _menhir_state * 'tv_ty) * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 4380 "core/parser.ml"
    )) = Obj.magic _menhir_stack in
    let ((_3 : (
# 14 "core/parser.mly"
       (Support.Error.info)
# 4385 "core/parser.ml"
    )) : (
# 14 "core/parser.mly"
       (Support.Error.info)
# 4389 "core/parser.ml"
    )) = _v in
    ((let ((_menhir_stack, _menhir_s, (t : 'tv_ty)), (id : (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 4394 "core/parser.ml"
    ))) = _menhir_stack in
    let _v : 'tv_decl = 
# 128 "core/parser.mly"
    ( let {v;i}=id in 
      fun s ->
      A.Vardecl(v,t,s,i)  )
# 4401 "core/parser.ml"
     in
    _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v) : 'freshtv332)

and _menhir_goto_param_list : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_param_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv329 * _menhir_state * 'tv_ty) * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 4412 "core/parser.ml"
    )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4416 "core/parser.ml"
    )) * _menhir_state * 'tv_param_list) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv325 * _menhir_state * 'tv_ty) * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 4426 "core/parser.ml"
        )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4430 "core/parser.ml"
        )) * _menhir_state * 'tv_param_list) = Obj.magic _menhir_stack in
        let (_v : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4435 "core/parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRACE _v ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
        | SEMICOLON _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv323 * _menhir_state * 'tv_ty) * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 4448 "core/parser.ml"
            )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4452 "core/parser.ml"
            )) * _menhir_state * 'tv_param_list) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4456 "core/parser.ml"
            )) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState38 in
            let (_v : (
# 14 "core/parser.mly"
       (Support.Error.info)
# 4462 "core/parser.ml"
            )) = _v in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv321 * _menhir_state * 'tv_ty) * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 4469 "core/parser.ml"
            )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4473 "core/parser.ml"
            )) * _menhir_state * 'tv_param_list) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4477 "core/parser.ml"
            )) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            let ((_6 : (
# 14 "core/parser.mly"
       (Support.Error.info)
# 4483 "core/parser.ml"
            )) : (
# 14 "core/parser.mly"
       (Support.Error.info)
# 4487 "core/parser.ml"
            )) = _v in
            ((let (((((_menhir_stack, _menhir_s, (t : 'tv_ty)), (id : (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 4492 "core/parser.ml"
            ))), (_3 : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4496 "core/parser.ml"
            ))), _, (pl : 'tv_param_list)), (_5 : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4500 "core/parser.ml"
            ))) = _menhir_stack in
            let _v : 'tv_fundecl = 
# 107 "core/parser.mly"
    ( let {v;i}=id in  
      let (_,tl) = List.split pl in 
      let t' = A.Arrow(tl,t) in 
      fun s -> 
      A.Fundecl(v,t',s,i) )
# 4509 "core/parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv319) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_fundecl) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv317) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_fundecl) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv315) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((f : 'tv_fundecl) : 'tv_fundecl) = _v in
            ((let _v : 'tv_gdecl = 
# 47 "core/parser.mly"
                                                  ( f )
# 4526 "core/parser.ml"
             in
            _menhir_goto_gdecl _menhir_env _menhir_stack _menhir_s _v) : 'freshtv316)) : 'freshtv318)) : 'freshtv320)) : 'freshtv322)) : 'freshtv324)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38) : 'freshtv326)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv327 * _menhir_state * 'tv_ty) * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 4540 "core/parser.ml"
        )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4544 "core/parser.ml"
        )) * _menhir_state * 'tv_param_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv328)) : 'freshtv330)

and _menhir_run128 : _menhir_env -> ('ttv_tail * _menhir_state * 'tv_ty) * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 4552 "core/parser.ml"
) -> (
# 23 "core/parser.mly"
       (Support.Error.info)
# 4556 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | ID _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | LPAREN _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | MINUS _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | NEW _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | NOT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | NULL _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | NUM _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | TRUE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128

and _menhir_goto_rev_field_list : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_rev_field_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv313 * _menhir_state * 'tv_rev_field_list) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | INT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | RBRACE _ ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv311 * _menhir_state * 'tv_rev_field_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (l : 'tv_rev_field_list)) = _menhir_stack in
        let _v : 'tv_field_list = 
# 101 "core/parser.mly"
                                                   ( List.rev l)
# 4609 "core/parser.ml"
         in
        _menhir_goto_field_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv312)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15) : 'freshtv314)

and _menhir_reduce44 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_compexp -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (e : 'tv_compexp)) = _menhir_stack in
    let _v : 'tv_exp = 
# 209 "core/parser.mly"
                                                  ( e )
# 4623 "core/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_simpexp : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_simpexp -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv309) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_simpexp) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv307) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((e : 'tv_simpexp) : 'tv_simpexp) = _v in
    ((let _v : 'tv_exp = 
# 208 "core/parser.mly"
                                                  ( e )
# 4640 "core/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv308)) : 'freshtv310)

and _menhir_reduce76 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 4647 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (vi : (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 4653 "core/parser.ml"
    ))) = _menhir_stack in
    let _v : 'tv_simplvalue = 
# 197 "core/parser.mly"
                                                  ( let {v;i}=vi in A.SimpVar(v,i) )
# 4658 "core/parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv305) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_simplvalue) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState40 | MenhirState163 | MenhirState161 | MenhirState116 | MenhirState122 | MenhirState155 | MenhirState150 | MenhirState152 | MenhirState148 | MenhirState147 | MenhirState124 | MenhirState134 | MenhirState141 | MenhirState136 | MenhirState132 | MenhirState128 | MenhirState120 | MenhirState117 | MenhirState42 | MenhirState46 | MenhirState53 | MenhirState55 | MenhirState96 | MenhirState94 | MenhirState92 | MenhirState90 | MenhirState88 | MenhirState86 | MenhirState84 | MenhirState82 | MenhirState80 | MenhirState77 | MenhirState75 | MenhirState72 | MenhirState69 | MenhirState67 | MenhirState63 | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv295 * _menhir_state * 'tv_simplvalue) = Obj.magic _menhir_stack in
        (_menhir_reduce55 _menhir_env (Obj.magic _menhir_stack) : 'freshtv296)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv303 * _menhir_state * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4675 "core/parser.ml"
        )) * _menhir_state * 'tv_simplvalue) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv299 * _menhir_state * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4685 "core/parser.ml"
            )) * _menhir_state * 'tv_simplvalue) = Obj.magic _menhir_stack in
            let (_v : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4690 "core/parser.ml"
            )) = _v in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv297 * _menhir_state * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4697 "core/parser.ml"
            )) * _menhir_state * 'tv_simplvalue) = Obj.magic _menhir_stack in
            let ((_3 : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4702 "core/parser.ml"
            )) : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4706 "core/parser.ml"
            )) = _v in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4711 "core/parser.ml"
            ))), _, (l : 'tv_simplvalue)) = _menhir_stack in
            let _v : 'tv_complvalue = 
# 201 "core/parser.mly"
                                                  ( l )
# 4716 "core/parser.ml"
             in
            _menhir_goto_complvalue _menhir_env _menhir_stack _menhir_s _v) : 'freshtv298)) : 'freshtv300)
        | AND _ | DIV _ | DOT _ | EQ _ | GEQ _ | GT _ | LEQ _ | LT _ | MINUS _ | NEQ _ | OR _ | PLUS _ | TIMES _ ->
            _menhir_reduce55 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv301 * _menhir_state * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4728 "core/parser.ml"
            )) * _menhir_state * 'tv_simplvalue) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv302)) : 'freshtv304)
    | _ ->
        _menhir_fail ()) : 'freshtv306)

and _menhir_run58 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 4738 "core/parser.ml"
) -> (
# 15 "core/parser.mly"
       (Support.Error.info)
# 4742 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | ID _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | LPAREN _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | MINUS _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | NEW _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | NOT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | NULL _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | NUM _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | TRUE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | RPAREN _ ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv293) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState58 in
        ((let _v : 'tv_arg_list = 
# 251 "core/parser.mly"
                                                  ( [] )
# 4774 "core/parser.ml"
         in
        _menhir_goto_arg_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv294)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_goto_simpty : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_simpty -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState173 | MenhirState169 | MenhirState0 | MenhirState40 | MenhirState163 | MenhirState161 | MenhirState124 | MenhirState34 | MenhirState30 | MenhirState15 | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv287) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_simpty) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv285) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((t : 'tv_simpty) : 'tv_simpty) = _v in
        ((let _v : 'tv_ty = 
# 161 "core/parser.mly"
                                                   ( t )
# 4797 "core/parser.ml"
         in
        _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv286)) : 'freshtv288)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv291) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_simpty) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv289) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((t : 'tv_simpty) : 'tv_simpty) = _v in
        ((let _v : 'tv_array_alloc_prefix = 
# 240 "core/parser.mly"
                                                  ( t )
# 4812 "core/parser.ml"
         in
        _menhir_goto_array_alloc_prefix _menhir_env _menhir_stack _menhir_s _v) : 'freshtv290)) : 'freshtv292)
    | _ ->
        _menhir_fail ()

and _menhir_goto_ty : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_ty -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv235 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv231 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            let (_v : (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 4834 "core/parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | SEMICOLON _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv227 * _menhir_state * 'tv_ty) * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 4845 "core/parser.ml"
                )) = Obj.magic _menhir_stack in
                let (_v : (
# 14 "core/parser.mly"
       (Support.Error.info)
# 4850 "core/parser.ml"
                )) = _v in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv225 * _menhir_state * 'tv_ty) * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 4857 "core/parser.ml"
                )) = Obj.magic _menhir_stack in
                let ((_3 : (
# 14 "core/parser.mly"
       (Support.Error.info)
# 4862 "core/parser.ml"
                )) : (
# 14 "core/parser.mly"
       (Support.Error.info)
# 4866 "core/parser.ml"
                )) = _v in
                ((let ((_menhir_stack, _menhir_s, (t : 'tv_ty)), (id : (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 4871 "core/parser.ml"
                ))) = _menhir_stack in
                let _v : 'tv_rev_field_list = 
# 93 "core/parser.mly"
    ( let {v;_}=id in [v,t] )
# 4876 "core/parser.ml"
                 in
                _menhir_goto_rev_field_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv226)) : 'freshtv228)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv229 * _menhir_state * 'tv_ty) * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 4886 "core/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv230)) : 'freshtv232)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv233 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv234)) : 'freshtv236)
    | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv247 * _menhir_state * 'tv_rev_field_list) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv243 * _menhir_state * 'tv_rev_field_list) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            let (_v : (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 4909 "core/parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | SEMICOLON _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv239 * _menhir_state * 'tv_rev_field_list) * _menhir_state * 'tv_ty) * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 4920 "core/parser.ml"
                )) = Obj.magic _menhir_stack in
                let (_v : (
# 14 "core/parser.mly"
       (Support.Error.info)
# 4925 "core/parser.ml"
                )) = _v in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv237 * _menhir_state * 'tv_rev_field_list) * _menhir_state * 'tv_ty) * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 4932 "core/parser.ml"
                )) = Obj.magic _menhir_stack in
                let ((_4 : (
# 14 "core/parser.mly"
       (Support.Error.info)
# 4937 "core/parser.ml"
                )) : (
# 14 "core/parser.mly"
       (Support.Error.info)
# 4941 "core/parser.ml"
                )) = _v in
                ((let (((_menhir_stack, _menhir_s, (l : 'tv_rev_field_list)), _, (t : 'tv_ty)), (id : (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 4946 "core/parser.ml"
                ))) = _menhir_stack in
                let _v : 'tv_rev_field_list = 
# 95 "core/parser.mly"
    ( let {v;_}=id in 
      (v,t)::l )
# 4952 "core/parser.ml"
                 in
                _menhir_goto_rev_field_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv238)) : 'freshtv240)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv241 * _menhir_state * 'tv_rev_field_list) * _menhir_state * 'tv_ty) * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 4962 "core/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv242)) : 'freshtv244)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv245 * _menhir_state * 'tv_rev_field_list) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv246)) : 'freshtv248)
    | MenhirState173 | MenhirState169 | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv259 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv255 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            let (_v : (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 4985 "core/parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSIGN _v ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) _v
            | LPAREN _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv251 * _menhir_state * 'tv_ty) * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 4998 "core/parser.ml"
                )) = Obj.magic _menhir_stack in
                let (_v : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 5003 "core/parser.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | BOOL ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState30
                | ID _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
                | INT ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState30
                | VOID ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState30
                | RPAREN _ ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv249) = Obj.magic _menhir_stack in
                    let (_menhir_s : _menhir_state) = MenhirState30 in
                    ((let _v : 'tv_param_list = 
# 87 "core/parser.mly"
                                                  ( [] )
# 5024 "core/parser.ml"
                     in
                    _menhir_goto_param_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv250)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30) : 'freshtv252)
            | SEMICOLON _v ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv253 * _menhir_state * 'tv_ty) * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 5040 "core/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv254)) : 'freshtv256)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv257 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv258)) : 'freshtv260)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv267 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv263 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            let (_v : (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 5063 "core/parser.ml"
            )) = _v in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv261 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            let ((id : (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 5071 "core/parser.ml"
            )) : (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 5075 "core/parser.ml"
            )) = _v in
            ((let (_menhir_stack, _menhir_s, (t : 'tv_ty)) = _menhir_stack in
            let _v : 'tv_rev_param_list = 
# 79 "core/parser.mly"
    ( let {v;_}=id in [v,t] )
# 5081 "core/parser.ml"
             in
            _menhir_goto_rev_param_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv262)) : 'freshtv264)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv265 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv266)) : 'freshtv268)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv275 * _menhir_state * 'tv_rev_param_list) * (
# 14 "core/parser.mly"
       (Support.Error.info)
# 5096 "core/parser.ml"
        )) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv271 * _menhir_state * 'tv_rev_param_list) * (
# 14 "core/parser.mly"
       (Support.Error.info)
# 5106 "core/parser.ml"
            )) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            let (_v : (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 5111 "core/parser.ml"
            )) = _v in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv269 * _menhir_state * 'tv_rev_param_list) * (
# 14 "core/parser.mly"
       (Support.Error.info)
# 5118 "core/parser.ml"
            )) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            let ((id : (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 5123 "core/parser.ml"
            )) : (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 5127 "core/parser.ml"
            )) = _v in
            ((let (((_menhir_stack, _menhir_s, (l : 'tv_rev_param_list)), (_2 : (
# 14 "core/parser.mly"
       (Support.Error.info)
# 5132 "core/parser.ml"
            ))), _, (t : 'tv_ty)) = _menhir_stack in
            let _v : 'tv_rev_param_list = 
# 82 "core/parser.mly"
    ( let {v;_}=id in 
      (v,t)::l )
# 5138 "core/parser.ml"
             in
            _menhir_goto_rev_param_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv270)) : 'freshtv272)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv273 * _menhir_state * 'tv_rev_param_list) * (
# 14 "core/parser.mly"
       (Support.Error.info)
# 5148 "core/parser.ml"
            )) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv274)) : 'freshtv276)
    | MenhirState163 | MenhirState161 | MenhirState40 | MenhirState124 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv283 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ID _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv279 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            let (_v : (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 5164 "core/parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSIGN _v ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) _v
            | SEMICOLON _v ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv277 * _menhir_state * 'tv_ty) * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 5181 "core/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv278)) : 'freshtv280)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv281 * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv282)) : 'freshtv284)
    | _ ->
        _menhir_fail ()

and _menhir_goto_compexp : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_compexp -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState40 | MenhirState161 | MenhirState163 | MenhirState42 | MenhirState116 | MenhirState120 | MenhirState122 | MenhirState155 | MenhirState124 | MenhirState148 | MenhirState150 | MenhirState152 | MenhirState147 | MenhirState132 | MenhirState134 | MenhirState136 | MenhirState141 | MenhirState128 | MenhirState117 | MenhirState46 | MenhirState53 | MenhirState55 | MenhirState58 | MenhirState63 | MenhirState67 | MenhirState69 | MenhirState72 | MenhirState80 | MenhirState96 | MenhirState94 | MenhirState82 | MenhirState92 | MenhirState90 | MenhirState88 | MenhirState86 | MenhirState84 | MenhirState75 | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv215 * _menhir_state * 'tv_compexp) = Obj.magic _menhir_stack in
        (_menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) : 'freshtv216)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv223 * _menhir_state * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 5208 "core/parser.ml"
        )) * _menhir_state * 'tv_compexp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv219 * _menhir_state * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 5218 "core/parser.ml"
            )) * _menhir_state * 'tv_compexp) = Obj.magic _menhir_stack in
            let (_v : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 5223 "core/parser.ml"
            )) = _v in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv217 * _menhir_state * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 5230 "core/parser.ml"
            )) * _menhir_state * 'tv_compexp) = Obj.magic _menhir_stack in
            let ((_3 : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 5235 "core/parser.ml"
            )) : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 5239 "core/parser.ml"
            )) = _v in
            ((let ((_menhir_stack, _menhir_s, (_1 : (
# 15 "core/parser.mly"
       (Support.Error.info)
# 5244 "core/parser.ml"
            ))), _, (e : 'tv_compexp)) = _menhir_stack in
            let _v : 'tv_compexp = 
# 219 "core/parser.mly"
                                                  ( e )
# 5249 "core/parser.ml"
             in
            _menhir_goto_compexp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv218)) : 'freshtv220)
        | AND _ | DIV _ | EQ _ | GEQ _ | GT _ | LEQ _ | LT _ | MINUS _ | NEQ _ | OR _ | PLUS _ | TIMES _ ->
            _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv221 * _menhir_state * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 5261 "core/parser.ml"
            )) * _menhir_state * 'tv_compexp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv222)) : 'freshtv224)
    | _ ->
        _menhir_fail ()

and _menhir_reduce79 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_varty -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (t : 'tv_varty)) = _menhir_stack in
    let _v : 'tv_simpty = 
# 165 "core/parser.mly"
                                                  ( t )
# 5274 "core/parser.ml"
     in
    _menhir_goto_simpty _menhir_env _menhir_stack _menhir_s _v

and _menhir_run43 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "core/parser.mly"
       (Support.Error.info)
# 5281 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv213) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 11 "core/parser.mly"
       (Support.Error.info)
# 5291 "core/parser.ml"
    )) : (
# 11 "core/parser.mly"
       (Support.Error.info)
# 5295 "core/parser.ml"
    )) = _v in
    ((let _v : 'tv_compexp = 
# 221 "core/parser.mly"
                                                  ( A.True(i) )
# 5300 "core/parser.ml"
     in
    _menhir_goto_compexp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv214)

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "core/parser.mly"
       (int Support.Error.withinfo)
# 5307 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv211) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((vi : (
# 8 "core/parser.mly"
       (int Support.Error.withinfo)
# 5317 "core/parser.ml"
    )) : (
# 8 "core/parser.mly"
       (int Support.Error.withinfo)
# 5321 "core/parser.ml"
    )) = _v in
    ((let _v : 'tv_compexp = 
# 220 "core/parser.mly"
                                                  ( let {v;i}=vi in A.Intconst(v,i) )
# 5326 "core/parser.ml"
     in
    _menhir_goto_compexp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv212)

and _menhir_run45 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 26 "core/parser.mly"
       (Support.Error.info)
# 5333 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv209) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 26 "core/parser.mly"
       (Support.Error.info)
# 5343 "core/parser.ml"
    )) : (
# 26 "core/parser.mly"
       (Support.Error.info)
# 5347 "core/parser.ml"
    )) = _v in
    ((let _v : 'tv_simpexp = 
# 214 "core/parser.mly"
                                                  ( A.Nil(i) )
# 5352 "core/parser.ml"
     in
    _menhir_goto_simpexp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv210)

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 22 "core/parser.mly"
       (Support.Error.info)
# 5359 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | ID _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | LPAREN _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | MINUS _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | NEW _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | NOT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | NULL _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | NUM _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | TRUE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 25 "core/parser.mly"
       (Support.Error.info)
# 5392 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv207) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState47 in
        let (_v : (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 5408 "core/parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce91 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv208)
    | INT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 20 "core/parser.mly"
       (Support.Error.info)
# 5425 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | ID _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | LPAREN _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | MINUS _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | NEW _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | NOT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | NULL _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | NUM _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | TRUE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55

and _menhir_run56 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 15 "core/parser.mly"
       (Support.Error.info)
# 5458 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE _v ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | ID _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | LPAREN _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | MINUS _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | NEW _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | NOT _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | NULL _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | NUM _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | TRUE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run57 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 5491 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACK _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _v
    | LPAREN _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _v
    | AND _ | ASSIGN _ | COMMA _ | DIV _ | DOT _ | EQ _ | GEQ _ | GT _ | LEQ _ | LT _ | MINUS _ | NEQ _ | OR _ | PLUS _ | RBRACK _ | RPAREN _ | SEMICOLON _ | TIMES _ ->
        _menhir_reduce76 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv205 * _menhir_state * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 5511 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv206)

and _menhir_run59 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "core/parser.mly"
       (Support.Error.info)
# 5519 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv203) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 11 "core/parser.mly"
       (Support.Error.info)
# 5529 "core/parser.ml"
    )) : (
# 11 "core/parser.mly"
       (Support.Error.info)
# 5533 "core/parser.ml"
    )) = _v in
    ((let _v : 'tv_compexp = 
# 222 "core/parser.mly"
                                                  ( A.False(i) )
# 5538 "core/parser.ml"
     in
    _menhir_goto_compexp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv204)

and _menhir_run23 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_id_with_lbrack -> _menhir_state -> (
# 17 "core/parser.mly"
       (Support.Error.info)
# 5545 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv201 * _menhir_state * 'tv_id_with_lbrack) = Obj.magic _menhir_stack in
    let (_ : _menhir_state) = _menhir_s in
    let ((_2 : (
# 17 "core/parser.mly"
       (Support.Error.info)
# 5555 "core/parser.ml"
    )) : (
# 17 "core/parser.mly"
       (Support.Error.info)
# 5559 "core/parser.ml"
    )) = _v in
    ((let (_menhir_stack, _menhir_s, (vi : 'tv_id_with_lbrack)) = _menhir_stack in
    let _v : 'tv_compty = 
# 180 "core/parser.mly"
                                                  ( let {v;_}=vi in A.ArrayTy(A.NameTy(v)) )
# 5565 "core/parser.ml"
     in
    _menhir_goto_compty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv202)

and _menhir_reduce63 : _menhir_env -> ('ttv_tail * _menhir_state * 'tv_gdecl) * _menhir_state * (
# 37 "core/parser.mly"
       (Ast.stmt)
# 5572 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, (d : 'tv_gdecl)), _, (l : (
# 37 "core/parser.mly"
       (Ast.stmt)
# 5578 "core/parser.ml"
    ))) = _menhir_stack in
    let _3 = () in
    let _v : (
# 37 "core/parser.mly"
       (Ast.stmt)
# 5584 "core/parser.ml"
    ) = 
# 42 "core/parser.mly"
                                                  ( d l )
# 5588 "core/parser.ml"
     in
    _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v

and _menhir_run170 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce62 _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce80 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_primitivety -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (t : 'tv_primitivety)) = _menhir_stack in
    let _v : 'tv_simpty = 
# 166 "core/parser.mly"
                                                  ( t )
# 5608 "core/parser.ml"
     in
    _menhir_goto_simpty _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_compty : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_compty -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv199) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_compty) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv197) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((t : 'tv_compty) : 'tv_compty) = _v in
    ((let _v : 'tv_ty = 
# 162 "core/parser.mly"
                                                ( t )
# 5625 "core/parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv198)) : 'freshtv200)

and _menhir_reduce91 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 5632 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (id : (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 5638 "core/parser.ml"
    ))) = _menhir_stack in
    let _v : 'tv_varty = 
# 169 "core/parser.mly"
                                                  ( let {v;_}=id in A.NameTy(v) )
# 5643 "core/parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv195) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_varty) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState173 | MenhirState169 | MenhirState163 | MenhirState161 | MenhirState40 | MenhirState124 | MenhirState34 | MenhirState30 | MenhirState0 | MenhirState15 | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv187 * _menhir_state * 'tv_varty) = Obj.magic _menhir_stack in
        (_menhir_reduce79 _menhir_env (Obj.magic _menhir_stack) : 'freshtv188)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv193 * _menhir_state * (
# 25 "core/parser.mly"
       (Support.Error.info)
# 5660 "core/parser.ml"
        )) * _menhir_state * 'tv_varty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRACK _ ->
            _menhir_reduce79 _menhir_env (Obj.magic _menhir_stack)
        | AND _ | COMMA _ | DIV _ | EQ _ | GEQ _ | GT _ | LEQ _ | LT _ | MINUS _ | NEQ _ | OR _ | PLUS _ | RBRACK _ | RPAREN _ | SEMICOLON _ | TIMES _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv189 * _menhir_state * (
# 25 "core/parser.mly"
       (Support.Error.info)
# 5672 "core/parser.ml"
            )) * _menhir_state * 'tv_varty) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (i : (
# 25 "core/parser.mly"
       (Support.Error.info)
# 5677 "core/parser.ml"
            ))), _, (t : 'tv_varty)) = _menhir_stack in
            let _v : 'tv_compexp = 
# 231 "core/parser.mly"
                                                  ( A.Alloc(t,i))
# 5682 "core/parser.ml"
             in
            _menhir_goto_compexp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv190)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv191 * _menhir_state * (
# 25 "core/parser.mly"
       (Support.Error.info)
# 5692 "core/parser.ml"
            )) * _menhir_state * 'tv_varty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv192)) : 'freshtv194)
    | _ ->
        _menhir_fail ()) : 'freshtv196)

and _menhir_run4 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 5702 "core/parser.ml"
) -> (
# 17 "core/parser.mly"
       (Support.Error.info)
# 5706 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv185 * _menhir_state * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 5714 "core/parser.ml"
    )) = Obj.magic _menhir_stack in
    let ((_2 : (
# 17 "core/parser.mly"
       (Support.Error.info)
# 5719 "core/parser.ml"
    )) : (
# 17 "core/parser.mly"
       (Support.Error.info)
# 5723 "core/parser.ml"
    )) = _v in
    ((let (_menhir_stack, _menhir_s, (vi : (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 5728 "core/parser.ml"
    ))) = _menhir_stack in
    let _v : 'tv_id_with_lbrack = 
# 156 "core/parser.mly"
                                                  ( vi )
# 5733 "core/parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv183) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_id_with_lbrack) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState173 | MenhirState169 | MenhirState0 | MenhirState30 | MenhirState34 | MenhirState8 | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv177 * _menhir_state * 'tv_id_with_lbrack) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACK _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22) : 'freshtv178)
    | MenhirState116 | MenhirState122 | MenhirState155 | MenhirState150 | MenhirState152 | MenhirState148 | MenhirState147 | MenhirState134 | MenhirState136 | MenhirState141 | MenhirState132 | MenhirState128 | MenhirState120 | MenhirState117 | MenhirState42 | MenhirState46 | MenhirState53 | MenhirState55 | MenhirState56 | MenhirState58 | MenhirState96 | MenhirState94 | MenhirState92 | MenhirState90 | MenhirState88 | MenhirState86 | MenhirState84 | MenhirState82 | MenhirState80 | MenhirState77 | MenhirState75 | MenhirState72 | MenhirState69 | MenhirState67 | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv179 * _menhir_state * 'tv_id_with_lbrack) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FALSE _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | ID _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | LPAREN _v ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | MINUS _v ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | NEW _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | NOT _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | NULL _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | NUM _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | TRUE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67) : 'freshtv180)
    | MenhirState40 | MenhirState163 | MenhirState161 | MenhirState124 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv181 * _menhir_state * 'tv_id_with_lbrack) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FALSE _v ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
        | ID _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
        | LPAREN _v ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
        | MINUS _v ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
        | NEW _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
        | NOT _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
        | NULL _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
        | NUM _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
        | RBRACK _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
        | TRUE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147) : 'freshtv182)
    | _ ->
        _menhir_fail ()) : 'freshtv184)) : 'freshtv186)

and _menhir_goto_prog : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 37 "core/parser.mly"
       (Ast.stmt)
# 5817 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv163 * _menhir_state * (
# 37 "core/parser.mly"
       (Ast.stmt)
# 5827 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv161 * _menhir_state * (
# 37 "core/parser.mly"
       (Ast.stmt)
# 5833 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : (
# 37 "core/parser.mly"
       (Ast.stmt)
# 5838 "core/parser.ml"
        ))) = _menhir_stack in
        Obj.magic _1) : 'freshtv162)) : 'freshtv164)
    | MenhirState169 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv169 * _menhir_state * 'tv_gdecl) * _menhir_state * (
# 37 "core/parser.mly"
       (Ast.stmt)
# 5846 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv165 * _menhir_state * 'tv_gdecl) * _menhir_state * (
# 37 "core/parser.mly"
       (Ast.stmt)
# 5856 "core/parser.ml"
            )) = Obj.magic _menhir_stack in
            (_menhir_reduce63 _menhir_env (Obj.magic _menhir_stack) : 'freshtv166)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv167 * _menhir_state * 'tv_gdecl) * _menhir_state * (
# 37 "core/parser.mly"
       (Ast.stmt)
# 5866 "core/parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv168)) : 'freshtv170)
    | MenhirState173 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv175 * _menhir_state * 'tv_gdecl) * _menhir_state * (
# 37 "core/parser.mly"
       (Ast.stmt)
# 5875 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv171 * _menhir_state * 'tv_gdecl) * _menhir_state * (
# 37 "core/parser.mly"
       (Ast.stmt)
# 5885 "core/parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce63 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv172)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv173 * _menhir_state * 'tv_gdecl) * _menhir_state * (
# 37 "core/parser.mly"
       (Ast.stmt)
# 5896 "core/parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv174)) : 'freshtv176)
    | _ ->
        _menhir_fail ()

and _menhir_goto_gdecl : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_gdecl -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv157 * _menhir_state * 'tv_gdecl) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState169
        | CLASS ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState169
        | EOF ->
            _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState169
        | ID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _v
        | INT ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState169
        | VOID ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState169
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState169) : 'freshtv158)
    | MenhirState173 | MenhirState169 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv159 * _menhir_state * 'tv_gdecl) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState173
        | CLASS ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState173
        | EOF ->
            _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState173
        | ID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _v
        | INT ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState173
        | VOID ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState173
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState173) : 'freshtv160)
    | _ ->
        _menhir_fail ()

and _menhir_goto_field_list : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_field_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv155 * _menhir_state) * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 5961 "core/parser.ml"
    )) * (
# 16 "core/parser.mly"
       (Support.Error.info)
# 5965 "core/parser.ml"
    )) * _menhir_state * 'tv_field_list) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RBRACE _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv151 * _menhir_state) * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 5975 "core/parser.ml"
        )) * (
# 16 "core/parser.mly"
       (Support.Error.info)
# 5979 "core/parser.ml"
        )) * _menhir_state * 'tv_field_list) = Obj.magic _menhir_stack in
        let (_v : (
# 16 "core/parser.mly"
       (Support.Error.info)
# 5984 "core/parser.ml"
        )) = _v in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv149 * _menhir_state) * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 5991 "core/parser.ml"
        )) * (
# 16 "core/parser.mly"
       (Support.Error.info)
# 5995 "core/parser.ml"
        )) * _menhir_state * 'tv_field_list) = Obj.magic _menhir_stack in
        let ((_5 : (
# 16 "core/parser.mly"
       (Support.Error.info)
# 6000 "core/parser.ml"
        )) : (
# 16 "core/parser.mly"
       (Support.Error.info)
# 6004 "core/parser.ml"
        )) = _v in
        ((let ((((_menhir_stack, _menhir_s), (vi : (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 6009 "core/parser.ml"
        ))), (_3 : (
# 16 "core/parser.mly"
       (Support.Error.info)
# 6013 "core/parser.ml"
        ))), _, (pl : 'tv_field_list)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_strcutdefn = 
# 62 "core/parser.mly"
    ( let {v;i}=vi in 
      fun s -> A.Structdefn(v,pl,s,i) )
# 6020 "core/parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv147) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_strcutdefn) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv145) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_strcutdefn) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv143) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((s : 'tv_strcutdefn) : 'tv_strcutdefn) = _v in
        ((let _v : 'tv_gdecl = 
# 50 "core/parser.mly"
                                                  ( s )
# 6037 "core/parser.ml"
         in
        _menhir_goto_gdecl _menhir_env _menhir_stack _menhir_s _v) : 'freshtv144)) : 'freshtv146)) : 'freshtv148)) : 'freshtv150)) : 'freshtv152)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv153 * _menhir_state) * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 6047 "core/parser.ml"
        )) * (
# 16 "core/parser.mly"
       (Support.Error.info)
# 6051 "core/parser.ml"
        )) * _menhir_state * 'tv_field_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv154)) : 'freshtv156)

and _menhir_goto_primitivety : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_primitivety -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState173 | MenhirState169 | MenhirState0 | MenhirState40 | MenhirState163 | MenhirState161 | MenhirState124 | MenhirState30 | MenhirState34 | MenhirState8 | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv139 * _menhir_state * 'tv_primitivety) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRACK _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv135 * _menhir_state * 'tv_primitivety) = Obj.magic _menhir_stack in
            let (_v : (
# 17 "core/parser.mly"
       (Support.Error.info)
# 6072 "core/parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RBRACK _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv131 * _menhir_state * 'tv_primitivety) * (
# 17 "core/parser.mly"
       (Support.Error.info)
# 6083 "core/parser.ml"
                )) = Obj.magic _menhir_stack in
                let (_v : (
# 17 "core/parser.mly"
       (Support.Error.info)
# 6088 "core/parser.ml"
                )) = _v in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv129 * _menhir_state * 'tv_primitivety) * (
# 17 "core/parser.mly"
       (Support.Error.info)
# 6095 "core/parser.ml"
                )) = Obj.magic _menhir_stack in
                let ((_3 : (
# 17 "core/parser.mly"
       (Support.Error.info)
# 6100 "core/parser.ml"
                )) : (
# 17 "core/parser.mly"
       (Support.Error.info)
# 6104 "core/parser.ml"
                )) = _v in
                ((let ((_menhir_stack, _menhir_s, (t : 'tv_primitivety)), (_2 : (
# 17 "core/parser.mly"
       (Support.Error.info)
# 6109 "core/parser.ml"
                ))) = _menhir_stack in
                let _v : 'tv_compty = 
# 179 "core/parser.mly"
                                                  ( A.ArrayTy(t) )
# 6114 "core/parser.ml"
                 in
                _menhir_goto_compty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv130)) : 'freshtv132)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv133 * _menhir_state * 'tv_primitivety) * (
# 17 "core/parser.mly"
       (Support.Error.info)
# 6124 "core/parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)) : 'freshtv136)
        | ID _ ->
            _menhir_reduce80 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv137 * _menhir_state * 'tv_primitivety) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)) : 'freshtv140)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv141 * _menhir_state * 'tv_primitivety) = Obj.magic _menhir_stack in
        (_menhir_reduce80 _menhir_env (Obj.magic _menhir_stack) : 'freshtv142)
    | _ ->
        _menhir_fail ()

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState173 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv31 * _menhir_state * 'tv_gdecl) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState169 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv33 * _menhir_state * 'tv_gdecl) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState163 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv35 * _menhir_state * 'tv_decl) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState161 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv37 * _menhir_state * 'tv_stmt) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState155 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv39 * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 6172 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv40)
    | MenhirState152 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv41 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 6180 "core/parser.ml"
        )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 6184 "core/parser.ml"
        )) * _menhir_state * 'tv_decl) * _menhir_state * 'tv_exp) * (
# 14 "core/parser.mly"
       (Support.Error.info)
# 6188 "core/parser.ml"
        )) * _menhir_state * 'tv_simpopt) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 6192 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState150 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv43 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 6201 "core/parser.ml"
        )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 6205 "core/parser.ml"
        )) * _menhir_state * 'tv_decl) * _menhir_state * 'tv_exp) * (
# 14 "core/parser.mly"
       (Support.Error.info)
# 6209 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState148 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv45 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 6218 "core/parser.ml"
        )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 6222 "core/parser.ml"
        )) * _menhir_state * 'tv_decl) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState147 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv47 * _menhir_state * 'tv_id_with_lbrack) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState141 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv49 * _menhir_state * 'tv_lvalue) * (
# 23 "core/parser.mly"
       (Support.Error.info)
# 6236 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState136 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv51 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 6245 "core/parser.ml"
        )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 6249 "core/parser.ml"
        )) * _menhir_state * 'tv_simpopt) * (
# 14 "core/parser.mly"
       (Support.Error.info)
# 6253 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) * (
# 14 "core/parser.mly"
       (Support.Error.info)
# 6257 "core/parser.ml"
        )) * _menhir_state * 'tv_simpopt) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 6261 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState134 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv53 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 6270 "core/parser.ml"
        )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 6274 "core/parser.ml"
        )) * _menhir_state * 'tv_simpopt) * (
# 14 "core/parser.mly"
       (Support.Error.info)
# 6278 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) * (
# 14 "core/parser.mly"
       (Support.Error.info)
# 6282 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv55 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 6291 "core/parser.ml"
        )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 6295 "core/parser.ml"
        )) * _menhir_state * 'tv_simpopt) * (
# 14 "core/parser.mly"
       (Support.Error.info)
# 6299 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState128 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv57 * _menhir_state * 'tv_ty) * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 6308 "core/parser.ml"
        )) * (
# 23 "core/parser.mly"
       (Support.Error.info)
# 6312 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState124 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv59 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 6321 "core/parser.ml"
        )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 6325 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState122 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv61 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 6334 "core/parser.ml"
        )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 6338 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 6342 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv63 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 6351 "core/parser.ml"
        )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 6355 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState117 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv65 * _menhir_state * (
# 13 "core/parser.mly"
       (Support.Error.info)
# 6364 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv67 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 6373 "core/parser.ml"
        )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 6377 "core/parser.ml"
        )) * _menhir_state * 'tv_exp) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 6381 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv69 * _menhir_state * 'tv_exp) * (
# 21 "core/parser.mly"
       (Support.Error.info)
# 6390 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv71 * _menhir_state * 'tv_exp) * (
# 23 "core/parser.mly"
       (Support.Error.info)
# 6399 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv73 * _menhir_state * 'tv_exp) * (
# 23 "core/parser.mly"
       (Support.Error.info)
# 6408 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv75 * _menhir_state * 'tv_exp) * (
# 23 "core/parser.mly"
       (Support.Error.info)
# 6417 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv77 * _menhir_state * 'tv_exp) * (
# 23 "core/parser.mly"
       (Support.Error.info)
# 6426 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv79 * _menhir_state * 'tv_exp) * (
# 23 "core/parser.mly"
       (Support.Error.info)
# 6435 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv81 * _menhir_state * 'tv_exp) * (
# 20 "core/parser.mly"
       (Support.Error.info)
# 6444 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv83 * _menhir_state * 'tv_exp) * (
# 23 "core/parser.mly"
       (Support.Error.info)
# 6453 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv85 * _menhir_state * 'tv_exp) * (
# 21 "core/parser.mly"
       (Support.Error.info)
# 6462 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv87 * _menhir_state * 'tv_exp) * (
# 20 "core/parser.mly"
       (Support.Error.info)
# 6471 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv89 * _menhir_state * 'tv_exp) * (
# 20 "core/parser.mly"
       (Support.Error.info)
# 6480 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv91 * _menhir_state * 'tv_complvalue) * (
# 17 "core/parser.mly"
       (Support.Error.info)
# 6489 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState69 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv93 * _menhir_state * 'tv_exp) * (
# 20 "core/parser.mly"
       (Support.Error.info)
# 6498 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv95 * _menhir_state * 'tv_id_with_lbrack) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv97 * _menhir_state * 'tv_rev_arg_list) * (
# 14 "core/parser.mly"
       (Support.Error.info)
# 6512 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv99 * _menhir_state * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 6521 "core/parser.ml"
        )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 6525 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv101 * _menhir_state * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 6534 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv103 * _menhir_state * (
# 20 "core/parser.mly"
       (Support.Error.info)
# 6543 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv105 * _menhir_state * 'tv_array_alloc_prefix) * (
# 17 "core/parser.mly"
       (Support.Error.info)
# 6552 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv107 * _menhir_state * (
# 25 "core/parser.mly"
       (Support.Error.info)
# 6561 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv109 * _menhir_state * (
# 22 "core/parser.mly"
       (Support.Error.info)
# 6570 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv111 * _menhir_state * (
# 10 "core/parser.mly"
       (Support.Error.info)
# 6579 "core/parser.ml"
        )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 6583 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv113 * _menhir_state * (
# 16 "core/parser.mly"
       (Support.Error.info)
# 6592 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv115 * _menhir_state * 'tv_ty) * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 6601 "core/parser.ml"
        )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 6605 "core/parser.ml"
        )) * _menhir_state * 'tv_param_list) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 6609 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv117 * _menhir_state * 'tv_rev_param_list) * (
# 14 "core/parser.mly"
       (Support.Error.info)
# 6618 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv119 * _menhir_state * 'tv_ty) * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 6627 "core/parser.ml"
        )) * (
# 15 "core/parser.mly"
       (Support.Error.info)
# 6631 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv121 * _menhir_state * 'tv_id_with_lbrack) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)
    | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv123 * _menhir_state * 'tv_rev_field_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv125 * _menhir_state) * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 6650 "core/parser.ml"
        )) * (
# 16 "core/parser.mly"
       (Support.Error.info)
# 6654 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv127) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv128)

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv29) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_primitivety = 
# 174 "core/parser.mly"
                                                ( A.Void )
# 6673 "core/parser.ml"
     in
    _menhir_goto_primitivety _menhir_env _menhir_stack _menhir_s _v) : 'freshtv30)

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv27) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_primitivety = 
# 173 "core/parser.mly"
                                                ( A.Int )
# 6687 "core/parser.ml"
     in
    _menhir_goto_primitivety _menhir_env _menhir_stack _menhir_s _v) : 'freshtv28)

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 6694 "core/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACK _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _v
    | ID _ ->
        _menhir_reduce91 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25 * _menhir_state * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 6712 "core/parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)

and _menhir_reduce62 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _1 = () in
    let _v : (
# 37 "core/parser.mly"
       (Ast.stmt)
# 6723 "core/parser.ml"
    ) = 
# 41 "core/parser.mly"
                                                  ( A.Nop )
# 6727 "core/parser.ml"
     in
    _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 6743 "core/parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRACE _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv9 * _menhir_state) * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 6754 "core/parser.ml"
            )) = Obj.magic _menhir_stack in
            let (_v : (
# 16 "core/parser.mly"
       (Support.Error.info)
# 6759 "core/parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
            | INT ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState8
            | VOID ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState8
            | RBRACE _ ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv7) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState8 in
                ((let _v : 'tv_field_list = 
# 100 "core/parser.mly"
                                                   ( [] )
# 6780 "core/parser.ml"
                 in
                _menhir_goto_field_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv8)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8) : 'freshtv10)
        | BOOL | CLASS | EOF | ID _ | INT | VOID ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv17 * _menhir_state) * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 6792 "core/parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), (vi : (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 6797 "core/parser.ml"
            ))) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_structdecl = 
# 55 "core/parser.mly"
    ( let {v;i}=vi in 
      fun s -> A.Structdecl(v,s,i) )
# 6804 "core/parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv15) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_structdecl) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv13) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_structdecl) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv11) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((s : 'tv_structdecl) : 'tv_structdecl) = _v in
            ((let _v : 'tv_gdecl = 
# 49 "core/parser.mly"
                                                  ( s )
# 6821 "core/parser.ml"
             in
            _menhir_goto_gdecl _menhir_env _menhir_stack _menhir_s _v) : 'freshtv12)) : 'freshtv14)) : 'freshtv16)) : 'freshtv18)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv19 * _menhir_state) * (
# 9 "core/parser.mly"
       (Symbol.t Support.Error.withinfo)
# 6831 "core/parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)) : 'freshtv22)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv5) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_primitivety = 
# 175 "core/parser.mly"
                                                ( A.Bool )
# 6853 "core/parser.ml"
     in
    _menhir_goto_primitivety _menhir_env _menhir_stack _menhir_s _v) : 'freshtv6)

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

and prog : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 37 "core/parser.mly"
       (Ast.stmt)
# 6872 "core/parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv3) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | CLASS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState0 in
        (_menhir_reduce62 _menhir_env (Obj.magic _menhir_stack) _menhir_s : 'freshtv2)
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | INT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv4))

# 269 "/Users/Komma/.opam/ocaml-variants.4.07.1+flambda/lib/menhir/standard.mly"
  

# 6914 "core/parser.ml"
