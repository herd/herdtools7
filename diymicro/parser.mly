%{
    let get_iico_edge (s, src, dst) = Edge.get_iico_edge s src dst

    let get_multi_iico (s, src, dst) =
        let iico = Edge.get_iico s in
        ( iico,
        (if src = "*" then iico.Edge.inputs else [src]),
        if dst = "*" then iico.Edge.outputs else [dst] )
%}

%token <string * string * string> IICO_ARGS

%token RF
%token FR
%token WS
%token PO
%token DP
%token BASIC_DEP
%token IICO

%token RM WM R
%token INT EXT
%token SAME DIFFERENT
%token ADDR DATA CTRL

%token COLON
%token A L X

%token EOF

%start main
%type <Edge.t * Edge.annot> main

%start parse_iico
%type <Edge.iico * string list * string list> parse_iico

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
    | IICO IICO_ARGS    { get_iico_edge $2 }
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
    | R { Edge.RegEvent }
;
dp:
    | ADDR { Edge.Addr }
    | DATA { Edge.Data }
    | CTRL { Edge.Ctrl }
;

parse_iico:
    | IICO IICO_ARGS { get_multi_iico $2 }