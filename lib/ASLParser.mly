%{
open ASLBase

let eminus e = EBinop (ELiteral 0, Op.Sub, e)

let make_pgm sl = ([main_asl_proc], [asl_top_level] :: List.map (fun x -> [x]) sl)

%}

%token AND UNKNOWN ARRAY ASSUMES CALL CLASS DO END ENDEVENT ENDIF ENDPROPERTY ENDTRY EXCEPTION FEATURE GIVES IMPORT INVARIANT MAP NEWMAP PARALLEL PRIVATE PUBLIC REQUIRES SET STRING THROW TYPEOF VAR WITH DIV NOT UNSTABLE AS BIT CASE CONFIG DOWNTO ENDCASE ENDFOR ENDMODULE ENDRULE ENDWHILE EXPORT FOR IF INTEGER IS MODULE OF PASS PROFILE REAL RETHROW SETTER SUBTYPES TO UNION WHEN ZTYPE EOR IN OR SAMPLE ANY ASSERT ASSUME BITS BOOLEAN CAST CATCH CONSTANT DICT ELSE ELSIF ENDCATCH ENDCLASS ENDFUNC ENDGETTER ENDNAMESPACE ENDPACKAGE ENDSETTER ENDTEMPLATE ENUMERATION EVENT EXTENDS EXTERN FUNC GETTER IFF IMPLIES INTERSECT INTRINSIC LET LIST NAMESPACE NEWEVENT OTHERWISE PACKAGE PORT PRAGMA PROPERTY PROTECTED RECORD REPEAT RETURN RULE SHARED SIGNAL TEMPLATE THEN TRY TYPE UNTIL USING WHERE WHILE
%token EOF NEG COMMA LT SHR BAND IMPL SHL RBRACKET RPAR SLICING EQ LBRACE NEQ MINUS BEQ LBRACKET LPAR DOT LEQ POW MUL RDIV EQ_OP BOR PLUS COLON ARROW RBRACE CONCAT COLON_COLON GT PLUS_COLON SEMI_COLON GEQ MOD
%token <string> IDENTIFIER
%token <string> INT_LIT REAL_LIT BITVECTOR_LIT
%token <bool> BOOL_LIT

%type <value> value
%type <expr> expr
%type <stmt> stmt
%type <MiscParser.proc list * pseudo list list> pgm

%start pgm

%%

plist(X):
| xs = delimited(LPAR, separated_list(COMMA, X), RPAR)
    { xs }

value:
| INT_LIT   { int_of_string $1}

%inline unop:
| NEG    { Op.Inv }
| NOT    { Op.Not }

%inline binop:
| BAND   { Op.And }
| BOR   { Op.Or }
| EQ_OP   { Op.Eq }
| NEQ   { Op.Ne }
| GT   { Op.Gt }
| GEQ   { Op.Ge }
| LT   { Op.Lt }
| LEQ   { Op.Le }
| PLUS   { Op.Add }
| MINUS   { Op.Sub }
| OR   { Op.Or }
| AND   { Op.And }
| EOR   { Op.Xor }
| MUL   { Op.Mul }
| RDIV   { Op.Div }
| DIV   { Op.Div }
| SHL   { Op.ShiftLeft }
| SHR   { Op.ShiftRight }

expr:
| value                         { ELiteral $1           }
| IDENTIFIER                    { EVar $1               }
| expr binop expr               { EBinop($1, $2, $3)    }
| MINUS expr                    { eminus ($2)           }
| unop expr                     { EUnop ($1, $2)        }
| expr LBRACKET expr RBRACKET   { EGet ($1, $3)         }
| LPAR expr RPAR                { $2                    }

lexpr:
| IDENTIFIER                    { LEVar $1          }
| lexpr LBRACKET expr RBRACKET  { LESet ($1, $3)    }

stmt:
| PASS                              { SPass                 }
| stmt SEMI_COLON stmt              { SThen ($1, $3)        }
| lexpr EQ expr                     { SAssign ($1, $3)      }
| IF expr THEN stmt ELSE stmt END   { SCond ($2, $4, $6)    }
| x=IDENTIFIER args=plist(expr)     { SCall (x, args)       }
| RETURN                            { SReturn               }

decl:
| FUNC x=IDENTIFIER args=plist(IDENTIFIER) body=stmt SEMI_COLON? ENDFUNC
    { make_func x args body }

pgm:
| decl* EOF  { make_pgm $1 }
