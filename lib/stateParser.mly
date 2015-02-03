/*********************************************************************/
/*                        Memevents                                  */
/*                                                                   */
/* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     */
/* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         */
/*                                                                   */
/*  Copyright 2010 Institut National de Recherche en Informatique et */
/*  en Automatique and the authors. All rights reserved.             */
/*  This file is distributed  under the terms of the Lesser GNU      */
/*  General Public License.                                          */
/*********************************************************************/

%{
open Constant
open MiscParser
open ConstrGen
%}

%token EOF
%token <int> PROC
%token <string> SYMB_REG
%token <string> NAME
%token <string> DOLLARNAME
%token <int> NUM

%token TRUE FALSE
%token EQUAL PLUS_DISJ
%token FINAL FORALL EXISTS OBSERVED TOKAND NOT AND OR IMPLIES CASES WITH
%token LOCATIONS STAR
%token LBRK RBRK LPAR RPAR SEMI COLON AMPER

%token PTX_REG_DEC 
%token <string> PTX_REG_TYPE


%left PLUS_DISJ OR
%left AND
%right IMPLIES
%nonassoc NOT

%type <MiscParser.state> init
%start init
%type <MiscParser.location> location
%start location
%type <(MiscParser.location * MiscParser.run_type) list * MiscParser.constr * (string * MiscParser.quantifier) list> constraints
%start constraints
%type  <MiscParser.constr> constr
%start constr
%type  <MiscParser.constr> skip_loc_constr
%start skip_loc_constr
%type  <(MiscParser.location * MiscParser.run_type) list * MiscParser.constr> loc_constr
%start loc_constr
%type <MiscParser.location list> locs
%start locs
%%

/* For initial state */
init:
| init_semi_list EOF { $1 }


reg:
| NAME       {  $1 }
| DOLLARNAME {  $1 }
maybev:
| NUM  { Concrete $1 }
| NAME { Symbolic $1 }

location_reg:
| PROC COLON reg  {Location_reg ($1,$3)}
| NUM COLON reg   {Location_reg ($1,$3)}
| SYMB_REG        {Location_sreg $1 }
/* PTX registers */
| NUM COLON PTX_REG_DEC PTX_REG_TYPE reg 
                  {Location_reg($1,$5)}
| PROC COLON PTX_REG_DEC PTX_REG_TYPE reg 
                  {Location_reg($1,$5)}

location_deref:
| location_reg { $1 }
| STAR location_reg { $2 }
| STAR NAME { Location_global (Symbolic $2) }

location:
| location_reg { $1 }
| LBRK maybev RBRK {Location_global $2}
/* Hum, for backward compatibility, and compatibility with printer */
| maybev { Location_global $1 }

atom:
| location {($1,Concrete 0)}
| location EQUAL maybev {($1,$3)}

atom_init:
| atom { let x,v = $1 in x,(TyDef,v) }
| NAME location { $2,(Ty $1,Concrete 0)}
| NAME location EQUAL maybev { ($2,(Ty $1,$4))}
| NAME STAR location { ($3,(Pointer $1,Concrete 0))}
| NAME STAR location EQUAL amperopt maybev { ($3,(Pointer $1,$6))}
| STAR location { ($2,(TyDefPointer,Concrete 0))}
| STAR location EQUAL amperopt maybev { ($2,(TyDefPointer,$5))}
| NAME NAME LBRK NUM RBRK { (Location_global (Symbolic $2),(TyArray ($1,$4),Concrete 0)) }
amperopt:
| AMPER { () }
| { () }
init_semi_list:
| {[]}
| SEMI {[]}
| atom_init {$1::[]}
| atom_init SEMI init_semi_list  {$1::$3}


/* For final state constraints */

loc_typ:
| location { ($1, TyDef) }
| location STAR { ($1, TyDefPointer) }
| location NAME { ($1, Ty $2) }
| location NAME STAR { ($1, Pointer $2) }

loc_semi_list:
| {[]}
| SEMI {[]}
| loc_typ {$1::[]}
| loc_typ SEMI loc_semi_list  {$1::$3}

locations:
|  LOCATIONS LBRK loc_semi_list RBRK { $3 }
| { [] }

locs:
| { [] }
| location locs { $1 :: $2 }

constraints :
| locations old_constraints
  { let x = $1 in
    let y,z = $2 in
    x,y,z }

old_constraints :
| final EOF { $1,[] }
| final WITH kinds EOF { $1,$3 }


kinds :
| kind         { [$1] }
| kind SEMI    { [$1] }
| kind SEMI kinds   { $1 :: $3 }

kind:
| NAME COLON FORALL { ($1,Require) }
| NAME COLON EXISTS { ($1,Allow) }
| NAME COLON NOT EXISTS { ($1,Forbid) }


final:
| constr { $1 }
| constr SEMI { $1 }

constr:
|  { ForallStates (And []) }
| FORALL prop
    {ForallStates $2}
| EXISTS prop
    {ExistsState $2}
| NOT EXISTS prop
	{ NotExistsState $3 }
| FINAL prop
        { ExistsState $2 }
| LPAR prop RPAR
    {ExistsState $2}
| OBSERVED obs
    { ExistsState (Or $2) }

obs:
|  obsone {  [ And $1 ] }
|  obsone TOKAND obs { And $1 :: $3 }

obsone:
|                       { [] }
| atom_prop SEMI obsone { Atom $1 :: $3 }

loc_constr:
| locations constr { $1,$2 }

skip_loc_constr:
| locations constr { $2 }


atom_prop:
| atom { let l,v = $1 in LV (l,v) }
| location EQUAL location_deref {LL ($1,$3)}


prop:
|
    {And []}
| TRUE
    {And []}
| FALSE
    {Or []}
| atom_prop
    { Atom $1 }
| NOT prop
    {Not $2}
| prop AND prop
    {And [$1;$3]}
| prop OR  prop
    {Or [$1;$3]}
| prop IMPLIES prop
    { Implies ($1,$3) }
| LPAR prop RPAR
    { $2 }
