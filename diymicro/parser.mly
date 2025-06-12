%token <string> ID

%token RF
%token FR
%token WS
%token PO
%token DP
%token BASIC_DEP
%token IICO

%token RM WM
%token RR WR
%token INT EXT
%token SAME DIFFERENT
%token ADDR DATA CTRL REG

%token COLON
%token A L X

%token EOF

%start main
%type <Edge.t * Edge.annot> main

%%
main:
    | edge EOF { ($1, Edge.AnnotNone) }
    | edge COLON annot EOF { ($1, $3) }
;
annot:
    | A { Edge.A }
    | L { Edge.L }
    | X { Edge.X }
;
edge:
    | RF ie { Edge.Rf $2 }
    | FR ie { Edge.Fr $2 }
    | WS ie { Edge.Ws $2 }
    | PO sd dir dir     { Edge.Po ($2, $3, $4) }
    | DP dp sd dir      { Edge.Dp ($2, $3, Edge.Rm false, $4) }
    | DP dp sd dir dir  { Edge.Dp ($2, $3, $4, $5) }
    | BASIC_DEP dir dir { Edge.BasicDep ($2, $3) }
    | IICO ID           { Edge.Iico (Edge.get_iico ($2)) }
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
    | RM { Edge.Rm false }
    | WM { Edge.Wm false }
    | RR { Edge.Rr }
    | WR { Edge.Wr }
;
dp:
    | ADDR { Edge.Addr }
    | DATA { Edge.Data }
    | CTRL { Edge.Ctrl }
    | REG  { Edge.Reg }
;