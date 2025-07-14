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
%token DMB LD ST
%token RF_REG
%token IICO

%token RM WM R
%token INT EXT
%token SAME DIFFERENT
%token ADDR DATA CTRL

%token DOT
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
    | DP dp sd read_dir dir  { Edge.Dp ($2, $3, $4, $5) }

    | DMB DOT barrier_type sd dir dir { Edge.Dmb ($3, $4, $5, $6) }
    | DMB sd dir dir { Edge.Dmb (AArch64_compile.FULL, $2, $3, $4) }

    | RF_REG            { Edge.RfReg }
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
read_dir:
    | RM { Edge.Rm false }
    | R  { Edge.RegEvent }
;
dir:
    | RM { Edge.Rm false }
    | WM { Edge.Wm false }
    | R  { Edge.RegEvent }
;
dp:
    | ADDR { Edge.Addr }
    | DATA { Edge.Data }
    | CTRL { Edge.Ctrl }
;
barrier_type:
    | LD   { AArch64_compile.LD }
    | ST   { AArch64_compile.ST }
;

parse_iico:
    | IICO IICO_ARGS { get_multi_iico $2 }