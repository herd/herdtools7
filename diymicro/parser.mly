%token <string> ID
%token LBRACKET RBRACKET
%token ARROW
%token COLON

%token RF
%token FR
%token WS
%token PO
%token DP
%token IICO

%token R W
%token INT EXT
%token SAME DIFFERENT
%token ADDR DATA CTR

%token EOF

%start main
%type <Edge.edge> main

%%
main:
    edge EOF { $1 }
;
edge:
    | RF ie { Edge.Rf $2 }
    | FR ie { Edge.Fr $2 }
    | WS ie { Edge.Ws $2 }
    | PO sd dir dir    { Edge.Po ($2, $3, $4) }
    | DP dp sd dir dir { Edge.Dp ($2, $3, $4, $5) }
    | IICO LBRACKET ID COLON ID ARROW ID RBRACKET
                    { Edge.Iico ($3, ($5, $7)) }
;
ie:
    | INT { Edge.Internal }
    | EXT { Edge.External }
;
sd:
    | SAME { Edge.Same }
    | DIFFERENT { Edge.Different }
;
dir:
    | R { Edge.Rm }
    | W { Edge.Wm }
;
dp:
    | ADDR { Edge.Addr }
    | DATA { Edge.Data }
    | CTR  { Edge.Ctr  }
;