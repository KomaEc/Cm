
(* The type of tokens. *)

type token = 
  | WHILE of (Support.Error.info)
  | VOID
  | TRUE of (Support.Error.info)
  | TIMES of (Support.Error.info)
  | SEMICOLON of (Support.Error.info)
  | RPAREN of (Support.Error.info)
  | RETURN of (Support.Error.info)
  | RBRACK of (Support.Error.info)
  | RBRACE of (Support.Error.info)
  | PLUS of (Support.Error.info)
  | OR of (Support.Error.info)
  | NUM of (int Support.Error.withinfo)
  | NULL of (Support.Error.info)
  | NOT of (Support.Error.info)
  | NEW of (Support.Error.info)
  | NEQ of (Support.Error.info)
  | MINUS of (Support.Error.info)
  | LT of (Support.Error.info)
  | LPAREN of (Support.Error.info)
  | LEQ of (Support.Error.info)
  | LBRACK of (Support.Error.info)
  | LBRACE of (Support.Error.info)
  | INT
  | IF of (Support.Error.info)
  | ID of (Symbol.t Support.Error.withinfo)
  | GT of (Support.Error.info)
  | GEQ of (Support.Error.info)
  | FOR of (Support.Error.info)
  | FALSE of (Support.Error.info)
  | EQ of (Support.Error.info)
  | EOF
  | ELSE of (Support.Error.info)
  | DOT of (Support.Error.info)
  | DIV of (Support.Error.info)
  | COMMA of (Support.Error.info)
  | CLASS
  | BOOL
  | ASSIGN of (Support.Error.info)
  | AND of (Support.Error.info)

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.stmt)
