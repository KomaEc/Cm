%{
  module A = Ast
  open Support.Error
  module S = Symbol

%}

%token <int Support.Error.withinfo> NUM
%token <Symbol.t Support.Error.withinfo> ID 
%token <Support.Error.info> FOR IF ELSE WHILE
%token <Support.Error.info> TRUE FALSE
%token INT VOID BOOL
%token <Support.Error.info> RETURN
%token <Support.Error.info> SEMICOLON COMMA
%token <Support.Error.info> LPAREN RPAREN
%token <Support.Error.info> LBRACE RBRACE
%token <Support.Error.info> LBRACK RBRACK
%token <Support.Error.info> DOT
%token EOF
%token <Support.Error.info> PLUS MINUS TIMES DIV
%token <Support.Error.info> AND OR
%token <Support.Error.info> NOT
%token <Support.Error.info> ASSIGN NEQ LT GT LEQ GEQ EQ
%token CLASS
%token <Support.Error.info> NEW
%token <Support.Error.info> NULL

%left OR
%left AND
%left EQ NEQ
%left LT LEQ GT GEQ
%left PLUS MINUS 
%left TIMES DIV 
%nonassoc UMINUS
%right NOT

%start <Ast.stmt> prog 
%%

prog :
  | EOF                                           { A.Nop }
  | d=gdecl; l=prog; EOF                          { d l }
  ;

gdecl : 
  | d=decl                                        { d }     
  | f=fundecl                                     { f }
  | f=fundefn                                     { f }
  | s=structdecl                                  { s }
  | s=strcutdefn                                  { s }
  ;

structdecl : 
  | CLASS; vi=ID                     
    { let {v;i}=vi in 
      fun s -> A.Structdecl(v,s,i) }
  ;

strcutdefn : 
  | CLASS; vi=ID; LBRACE; 
    pl=field_list; RBRACE
    { let {v;i}=vi in 
      fun s -> A.Structdefn(v,pl,s,i) }
  ;

fundefn : 
  | t=ty; id=ID; LPAREN; pl=param_list;
    RPAREN; defs=block                               
    { let {v;i}=id in 
      let (il,tl) = List.split pl in 
      let t' = A.Arrow(tl,t) in 
      fun s -> 
      A.Fundefn(v,il,t',defs,s,i)
    }
  ;

rev_param_list : 
  | t=ty; id=ID
    { let {v;_}=id in [v,t] }
  | l=rev_param_list; COMMA;
    t=ty; id=ID
    { let {v;_}=id in 
      (v,t)::l }
  ;

param_list : 
  |                                               { [] }
  | l=rev_param_list                              { List.rev l }
  ;

rev_field_list : 
  | t=ty; id=ID; SEMICOLON
    { let {v;_}=id in [v,t] }
  | l=rev_field_list; t=ty; id=ID; SEMICOLON 
    { let {v;_}=id in 
      (v,t)::l }
  ;

field_list : 
  |                                                { [] }
  | l=rev_field_list                               { List.rev l}
  ;
  
fundecl : 
  | t=ty; id=ID; LPAREN; pl=param_list;
    RPAREN; SEMICOLON       
    { let {v;i}=id in  
      let (_,tl) = List.split pl in 
      let t' = A.Arrow(tl,t) in 
      fun s -> 
      A.Fundecl(v,t',s,i) }
  ;

block : 
  LBRACE; s=stmt_list; RBRACE                   { s }
  ;
stmt_list :
  | (* nothing *)                                 { A.Nop }
  | d=decl; sl=stmt_list                          { d sl }
  | s=stmt; sl=stmt_list                          
    { match sl with 
      | A.Seq(sl',_) -> A.Seq(s::sl', A.Util.extract_info_stmt s) 
      | _ as s' -> A.Seq([s;s'], A.Util.extract_info_stmt s) }
  ;

decl : 
  | t=ty; id=ID; SEMICOLON;
    { let {v;i}=id in 
      fun s ->
      A.Vardecl(v,t,s,i)  }
  | t=ty; id=ID; ii=ASSIGN; e=exp; SEMICOLON;
    { let {v;i}=id in 
      fun s ->
      A.Vardecl(v,t,A.Seq([A.Assign(A.SimpVar(v,i),e,ii);s],i),i) }
  ;

stmt : 
  | s=simp; SEMICOLON                             { s }
  | s=control                                     { s }
  | s=block                                       { s }
  ;

simpopt : 
  |  (* Nothing *)                                { A.Nop }
  |  s=simp                                       { s }
  ;

simp : 
  | e=exp                                         { A.Exp(e, A.Util.extract_info_exp e) }
  | l=lvalue; ii=ASSIGN; e=exp                    { A.Assign(l,e,ii) }
  | i=RETURN; e=exp                               { A.Return(e,i) }
  | i=RETURN;                                     { A.Return(A.Void_exp,i) }
  ;

id_with_lbrack:
  | vi=ID; LBRACK                                 { vi }
  ;


ty : 
  | t=simpty                                       { t }
  | t=compty                                    { t }
  ;
simpty:
  | t=varty                                       { t }
  | t=primitivety                                 { t }
  ;
varty:
  | id=ID                                         { let {v;_}=id in A.NameTy(v) }
  ;

primitivety:
  | INT                                         { A.Int }
  | VOID                                        { A.Void }
  | BOOL                                        { A.Bool }
  ;

compty : 
  | t=primitivety; LBRACK; RBRACK                 { A.ArrayTy(t) }
  | vi=id_with_lbrack; RBRACK                     { let {v;_}=vi in A.ArrayTy(A.NameTy(v)) }
  ;
(*
lvalue : 
  | vi=ID                                         { let {v;i}=vi in A.SimpVar(v,i) }
  | LPAREN; l=lvalue; RPAREN                      { l }
  | l=lvalue; LBRACK; e=exp; RBRACK               { A.SubscriptVar(l,e,A.extract_info_var l) }
  | l=lvalue; DOT; id=ID                          { let {v;i}=id in A.FieldVar(l,v,i) }
  ;
  *)

lvalue : 
  | l=simplvalue                                    { l }
  | l=complvalue                                    { l }
  ;

simplvalue:
  | vi=ID                                         { let {v;i}=vi in A.SimpVar(v,i) }
  ;
complvalue:
  | LPAREN; l=complvalue; RPAREN                  { l }
  | LPAREN; l=simplvalue; RPAREN                  { l }
  | l=complvalue; LBRACK; e=exp; RBRACK           { A.SubscriptVar(l,e,A.Util.extract_info_var l) }
  | vi=id_with_lbrack; e=exp; RBRACK              { let {v;i}=vi in A.SubscriptVar(A.SimpVar(v,i),e,i) }
  | l=lvalue; DOT; id=ID                          { let {v;i}=id in A.FieldVar(l,v,i) }
  ;

exp : 
  | e=simpexp                                     { e }
  | e=compexp                                     { e }
  ;

simpexp : 
  | l=lvalue                                      { A.Var(l) }
  | i=NULL                                        { A.Nil(i) }
  ;


compexp : 
  | LPAREN; e=compexp; RPAREN                     { e }
  | vi=NUM                                        { let {v;i}=vi in A.Intconst(v,i) }
  | i=TRUE                                        { A.True(i) }
  | i=FALSE                                       { A.False(i) }
  | i=NOT; e=exp                                  { A.Un(A.Not,e,i) }
  | i=MINUS; e=exp %prec UMINUS                   { A.Bin(A.Intconst(0,dummyinfo),A.Minus,e,i) }
  | e1=exp; op=bop; e2=exp                        { A.Bin(e1,fst op,e2,snd op) } 
  | e1=exp; i=LEQ; e2=exp                         { A.Un(A.Not, A.Bin(e1,A.Gt,e2,i), i) }
  | e1=exp; i=GEQ; e2=exp                         { A.Un(A.Not, A.Bin(e1,A.Lt,e2,i), i) }
  | e1=exp; i=NEQ; e2=exp                         { A.Un(A.Not, A.Bin(e1,A.Eq,e2,i), i) }
  | id=ID; LPAREN; al=arg_list; RPAREN            { let {v;i}=id in A.App(v,al,i) }
  | NEW; e=array_alloc                            { e }
  | i=NEW; t=varty                                { A.Alloc(t,i)}
  ;

  (* TODO: only one-dimensional new is allowed *)
array_alloc : 
  | t=array_alloc_prefix; LBRACK; e=exp; RBRACK   { A.ArrayAlloc(t, e, A.Util.extract_info_exp e) }
  ;

array_alloc_prefix : 
  | t=simpty                                      { t } 
  | t=array_alloc_prefix; LBRACK; RBRACK          { A.ArrayTy(t) } 
  ;
  

rev_arg_list : 
  | e=exp                                         { [e] }
  | rl=rev_arg_list; COMMA; e=exp                 { e::rl }
  ;

arg_list : 
  |                                               { [] }
  | l=rev_arg_list                                { List.rev l }

%inline bop :
  | i=PLUS                                        { A.Plus,i }
  | i=MINUS                                       { A.Minus,i }
  | i=TIMES                                       { A.Times,i }
  | i=DIV                                         { A.Div,i }
  | i=LT                                          { A.Lt,i }
  | i=GT                                          { A.Gt,i }
  | i=EQ                                          { A.Eq,i }
  | i=AND                                         { A.And,i }
  | i=OR                                          { A.Or,i }
  ;

control : 
  | i=IF; LPAREN; e=exp; RPAREN; s=stmt; 
    sop=elseopt                                   { A.If(e,s,sop,i) }
  | i=FOR; LPAREN; sop1=simpopt;
    SEMICOLON; e=exp; SEMICOLON; sop2=simpopt;
    RPAREN; s=stmt                                
    { A.Seq([sop1;A.While(e, A.Seq([s; sop2], A.Util.extract_info_stmt s), i)], A.Util.extract_info_stmt sop1) }
  | i=FOR; LPAREN; d=decl; e=exp; SEMICOLON; sop2=simpopt;
    RPAREN; s=stmt                                
    { d (A.While(e, A.Seq([s; sop2], A.Util.extract_info_stmt s), i)) }
  | i=WHILE; LPAREN; e=exp; RPAREN; s=stmt 
    { A.While(e, s, i)}

  ;

elseopt : 
  | ELSE; s=stmt                                  { Some(s) }
  | (* Nothing *)                                 { None }
  ;
