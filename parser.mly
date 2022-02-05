%start program

%token <int> NUM
%token <string> VAR
%token LPAR RPAR COMMA FST SND PLUS MINUS CASE IF LET EOF

%left PLUS MINUS COMMA
%right FST SND LET
%nonassoc IF

%type <L.expr> program

%%

program:
    expr EOF { $1 }
  ;
expr:
    NUM { L.Num $1 }
  | VAR { L.Var $1 }
  | LPAR expr COMMA expr RPAR { L.Pair ($2, $4) }
  | LPAR expr RPAR { $2 }
  | expr FST { L.Fst $1 }
  | expr SND { L.Snd $1 }
  | expr PLUS expr { L.Add ($1, $3) }
  | MINUS expr { L.Neg $2 }
  | CASE expr expr expr expr { L.Case ($2, $3, $4, $5) }
  | IF expr expr expr { L.If ($2, $3, $4) }
  | LET VAR expr expr { L.Let ($2, $3, $4) }
  ;
%%
