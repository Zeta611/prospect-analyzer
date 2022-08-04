%start program

%token <int> LANG NUM
%token <string> VAR
%token HASH HOLE LPAR RPAR COMMA FST SND PLUS MINUS CASE IF LET EOF

%right LET
%nonassoc CASE IF
%left PLUS MINUS
%right FST SND
%nonassoc LANG NUM HOLE VAR LPAR RPAR COMMA EOF

%type <L.prog> program

%%

program:
    version samples expr EOF { ($1, $2, $3) }
  ;
version:
    HASH LANG { $2 }
  ;
samples:
    { [] }
  |  HASH vexpr vexpr samples { ($2, $3) :: $4 }
  ;
vexpr:
    NUM { `Num $1 }
  | LPAR vexpr COMMA vexpr RPAR { `Pair ($2, $4) }
expr:
    NUM { L.Num $1 }
  | VAR { L.Var $1 }
  | HOLE { L.Hole }
  | LPAR expr COMMA expr RPAR { L.Pair ($2, $4) }
  | LPAR expr RPAR { $2 }
  | expr FST { L.Fst $1 }
  | expr SND { L.Snd $1 }
  | expr PLUS expr { L.Add ($1, $3) }
  | MINUS expr { L.Neg $2 }
  | CASE expr LPAR VAR COMMA VAR RPAR expr expr { L.Case ($2, $4, $6, $8, $9) }
  | IF expr expr expr { L.If ($2, $3, $4) }
  | LET VAR expr expr { L.Let ($2, $3, $4) }
  ;
%%
