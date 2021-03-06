%{
  open Ast
  open Ast_maker
%}


%token <string> CONST
%token IDENT Y

%token PLUS MINUS DIV TIMES 

%token POW LOG LN SQRT

%token SIN COS TAN ARCSIN ARCCOS ARCTAN

%token PI NATEXP PHI

%token EQUALS

%token EOF


%token LPAREN RPAREN COMMA CARAT

%left PLUS MINUS TIMES DIV CARAT


%start <Ast.expr> parse_expression


%%

parse_expression:
  | e = expr; EOF
        { e }


expr:
  | c = CONST 
        { make_const c }
  | LPAREN; e = expr; RPAREN 
        { e }
  | IDENT
        { make_identity () }
  | e1 = expr; PLUS; e2 = expr
        { make_add e1 e2 }
  | MINUS; e1 = expr 
        { make_minus (make_const "0") e1 } 
  | e1 = expr; MINUS; e2 = expr
        { make_minus e1 e2 }
  | e1 = expr; TIMES; e2 = expr
        { make_mult e1 e2 }
  | c = CONST; IDENT
        { make_mult (make_const c) (make_identity ()) }
  | e1 = expr; DIV; e2 = expr
        { make_div e1 e2 }
  | POW; LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN
        { make_pow e1 e2 }
  | SQRT; LPAREN; e1 = expr; RPAREN
        { make_pow e1 (make_const "0.5")}
  | e1 = expr; CARAT; e2 = expr
        { make_pow e1 e2 }
  | SIN; LPAREN; e = expr; RPAREN
        { make_sin e }
  | COS; LPAREN; e = expr; RPAREN
        { make_cos e }
  | TAN; LPAREN; e = expr; RPAREN
        { make_tan e }
  | ARCSIN; LPAREN; e = expr; RPAREN
        { make_arcsin e }
  | ARCCOS; LPAREN; e = expr; RPAREN
        { make_arccos e }
  | ARCTAN; LPAREN; e = expr; RPAREN
        { make_arctan e }
  | LN; LPAREN; e = expr; RPAREN
        { make_ln e }
  | LOG; LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN
        { make_div (make_ln e2) (make_ln e1) }
  | LPAREN; e = expr; LPAREN
        { e }
  | PI
        { make_const "3.14159265" }
  | NATEXP 
        { make_const (string_of_float (exp 1.)) }
  | PHI
        { make_const "1.61803399" }
  | Y; EQUALS; e = expr
        { e }
  | EOF
        { raise End_of_file }
  ;