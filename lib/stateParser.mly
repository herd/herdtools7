%{
(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Constant
open MiscParser
open ConstrGen
let mk_sym_tag s t =
  Symbolic {default_symbolic_data with name=s;tag=Some t;}
let mk_sym_morello p s t =
  let p_int = (Misc.string_as_int p) in
  if p_int land 0x7 <> 0 || p_int >= 1 lsl 36
    then Printf.eprintf "Warning: incorrect address encoding: %#x\n" p_int ;
  let truncated_perms = p_int lsr 3 in
  let tag = if Misc.string_as_int t <> 0 then 1 else 0 in
  Symbolic {default_symbolic_data with name=s;cap=truncated_perms lor (tag lsl 33)}
let mk_sym_with_index s i = Symbolic
  {default_symbolic_data with name=s; offset=Misc.string_as_int i}
let mk_lab p s = Label (p,s)
%}

%token EOF
%token <int> PROC
%token <string> SYMB_REG
%token <string> NAME
%token <string> DOLLARNAME
%token <string> NUM

%token TRUE FALSE
%token EQUAL NOTEQUAL EQUALEQUAL
%token FINAL FORALL EXISTS OBSERVED TOKAND NOT AND OR IMPLIES WITH FILTER
%token LOCATIONS FAULT STAR
%token LBRK RBRK LPAR RPAR LCURLY RCURLY SEMI COLON AMPER COMMA
%token ATOMIC
%token ATOMICINIT

%token PTX_REG_DEC
%token <string> PTX_REG_TYPE

%left OR
%left AND
%right IMPLIES
%nonassoc NOT

%type <MiscParser.state> init
%start init
%type <MiscParser.location> main_location
%start main_location
%type <(MiscParser.location ConstrGen.rloc * MiscParser.run_type) list * MiscParser.prop option * MiscParser.constr * (string * MiscParser.quantifier) list> constraints
%start constraints
%type  <MiscParser.constr> main_constr
%start main_constr
%type  <MiscParser.constr> skip_loc_constr
%start skip_loc_constr
%type  <(MiscParser.location ConstrGen.rloc * MiscParser.run_type) list * MiscParser.constr> main_loc_constr
%start main_loc_constr
%type <MiscParser.location list> main_locs
%start main_locs
%type <MiscParser.prop option> main_filter
%start main_filter
%%

/* For initial state */
init:
| st=init_semi_list EOF { st }


reg:
| NAME       {  $1 }
| DOLLARNAME {  $1 }

location_global:
| NAME { mk_sym $1  }
| NAME COLON NAME { mk_sym_tag $1 $3 }
(* TODO: have MTE and Morello tags be usable at the same time? *)
| NUM COLON NAME COLON NUM {mk_sym_morello $1 $3 $5}
| NAME COLON NUM { mk_sym_morello "0" $1 $3 }

maybev_notag:
| location_global { $1 }
| NUM  { Concrete $1 }
/* conflicts with location_reg:
| NUM COLON NAME { mk_sym_morello $1 $3 "0" }
*/
(* TODO: restrict to something like "NUM COLON BOOL"? *)
| NUM COLON NUM { Concrete ($1 ^ ":" ^ $3) }
| NAME LBRK NUM RBRK { mk_sym_with_index $1 $3 }

maybev:
| maybev_notag { $1 }
| COLON NAME  { Tag $2 }

maybev_list:
| maybev_notag COMMA maybev_list { $1::$3 }
| maybev_notag { [$1] }

maybev_label:
| maybev { $1 }
| PROC COLON NAME { mk_lab $1 $3 }

location_reg:
| PROC COLON reg  {Location_reg ($1,$3)}
| NUM COLON reg   {Location_reg (Misc.string_as_int $1,$3)}
| PROC COLON SYMB_REG  {Location_reg ($1,$3)}
| NUM COLON SYMB_REG   {Location_reg (Misc.string_as_int $1,$3)}
| SYMB_REG        {Location_sreg $1 }
/* PTX registers */
| NUM COLON PTX_REG_DEC PTX_REG_TYPE reg
                  {Location_reg(Misc.string_as_int $1,$5)}
| PROC COLON PTX_REG_DEC PTX_REG_TYPE reg
                  {Location_reg($1,$5)}
/* memory tags
| PROC COLON reg PATAG {Location_reg ($1,$3)}
| NUM COLON reg PATAG {Location_reg (Misc.string_as_int $1,$3)}
*/
location_deref:
| location_reg { $1 }
| STAR location_reg { $2 }
| STAR NAME { Location_global (Constant.mk_sym $2) }

main_location:
| loc=location EOF { loc }

location:
| location_reg { $1 }
| location_global { Location_global $1 }

atom:
| location {($1,ParsedConstant.zero)}
| location EQUAL maybev_label {($1,$3)}

atom_init:
| atom { let x,v = $1 in x,(TyDef,v) }
| NAME location
/* We either have uninitalized arrays or scalars here: "typ v[i]" or "typ v" */
   { match $2 with
     | Location_global (Symbolic ({offset=sz;_} as s)) when sz > 0 ->
       let xs = Misc.replicate sz ParsedConstant.zero in
       let arr = TyArray ($1,sz),Constant.mk_vec sz xs in
       (Location_global (Symbolic ({s with offset=0})), arr)
     | _ -> ($2, (Ty $1,ParsedConstant.zero)) }
| ATOMIC NAME location { $3,(Atomic $2,ParsedConstant.zero)}
| NAME location EQUAL maybev { ($2,(Ty $1,$4))}
| NAME locindex EQUAL LCURLY maybev_list RCURLY
   { match $2 with
     | Deref (Location_global (Symbolic ({offset=0;_})) as s,sz) when sz = List.length $5 ->
       let arr = (TyArray ($1,sz),Constant.mk_vec sz $5) in
       (s, arr)
     | _ -> assert false }
/* prohibit "v[i] = scalar" form in init allow only "v[i]={scalar_list}" */
| locindex EQUAL maybev { raise Parsing.Parse_error }
| NAME locindex EQUAL maybev { raise Parsing.Parse_error }
| NAME location EQUAL ATOMICINIT LPAR maybev RPAR { ($2,(Ty $1,$6))}
| NAME STAR location { ($3,(Pointer $1,ParsedConstant.zero))}
| NAME STAR location EQUAL amperopt maybev { ($3,(Pointer $1,$6))}
| STAR location { ($2,(TyDefPointer,ParsedConstant.zero))}
| STAR location EQUAL amperopt maybev { ($2,(TyDefPointer,$5))}

amperopt:
| AMPER { () }
| { () }


init_semi_list:
| {[]}
| SEMI {[]}
| atom_init {$1::[]}
| atom_init SEMI init_semi_list  {$1::$3}


/* For final state constraints */

rloc_typ:
| rloc { ($1, TyDef) }
| rloc STAR { ($1, TyDefPointer) }
| rloc NAME { ($1, Ty $2) }
| rloc NAME STAR { ($1, Pointer $2) }

main_locs:
| ls = list(location)  EOF { ls }

rloc_semi_list:
| {[]}
| SEMI {[]}
| rloc_typ {$1::[]}
| rloc_typ SEMI rloc_semi_list  {$1::$3}

locations:
|  LOCATIONS LBRK rloc_semi_list RBRK { $3 }
| { [] }

filter:
| { None }
| FILTER prop { Some $2 }

main_filter:
| f=filter EOF { f }

constraints:
| locations filter old_constraints EOF
  { let x = $1 in
    let f = $2 in
    let y,z = $3 in
    x,f,y,z }

old_constraints :
| final { $1,[] }
| final WITH kinds { $1,$3 }


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

main_constr:
| c = constr EOF { c }

constr:
|  { ConstrGen.constr_true }
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
| atom_prop SEMI obsone { $1 :: $3 }

main_loc_constr:
| lc = loc_constr EOF { lc }

loc_constr:
| locations constr { $1,$2 }

skip_loc_constr:
| locations constr EOF { $2 }

lbl:
| PROC { ($1,None) }
| PROC COLON NAME { ($1,Some $3) }

rloc:
| location { Loc $1 }
| locindex { $1 }

locindex:
| location LBRK NUM RBRK { Deref ($1,Misc.string_as_int $3) }


atom_prop:
| location EQUAL maybev {Atom (LV (Loc $1,$3))}
| location EQUALEQUAL maybev {Atom (LV (Loc $1,$3))}
| locindex EQUAL maybev {Atom (LV ($1,$3))}
| locindex EQUALEQUAL maybev {Atom (LV ($1,$3))}
| location NOTEQUAL maybev {Not (Atom (LV (Loc $1,$3)))}
| locindex NOTEQUAL maybev {Not (Atom (LV ($1,$3)))}
| location EQUAL location_deref {Atom (LL ($1,$3))}
| location EQUALEQUAL location_deref {Atom (LL ($1,$3))}
| FAULT LPAR lbl COMMA NAME RPAR { Atom (FF ($3,mk_sym $5)) }

prop:
| TRUE
    {And []}
| FALSE
    {Or []}
| atom_prop
    { $1 }
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
