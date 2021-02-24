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
open TestType
open LocationsItem
open MiscParser
open ConstrGen

let mk_sym_tag s t =
  Symbolic (Virtual {default_symbolic_data with name=s;tag=Some t;})

let mk_sym_morello p s t =
  let p_int = Misc.string_as_int64 p in
  if
    not (Int64.equal (Int64.logand p_int 0x7L) 0L)
    || Int64.compare p_int (Int64.shift_left 1L 36) >= 0
    || Int64.compare p_int 0L < 0
    then Printf.eprintf "Warning: incorrect address encoding: %#Lx\n" p_int ;
  let truncated_perms = Int64.shift_right_logical p_int 3 in
  let tag = if Misc.string_as_int t <> 0 then 1L else 0L in
  Symbolic
    (Virtual
       {default_symbolic_data
       with
       name=s;
       cap=Int64.logor truncated_perms (Int64.shift_left tag 33); })

let mk_sym_with_index s i =
  Symbolic
    (Virtual
       {default_symbolic_data
       with name=s; offset=Misc.string_as_int i})

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
%token ATTRS

%token PTX_REG_DEC
%token <string> PTX_REG_TYPE

%left OR
%left AND
%right IMPLIES
%nonassoc NOT

%type <PTEVal.t> pteval
%start pteval
%type <MiscParser.state> init
%start init
%type <MiscParser.location> main_location
%start main_location
%type < (MiscParser.location,MiscParser.maybev) LocationsItem.t list * MiscParser.prop option * MiscParser.constr * (string * MiscParser.quantifier) list> constraints
%start constraints
%type  <MiscParser.constr> main_constr
%start main_constr
%type  <MiscParser.constr> skip_loc_constr
%start skip_loc_constr
%type  <(MiscParser.location,MiscParser.maybev) LocationsItem.t list * MiscParser.constr> main_loc_constr
%start main_loc_constr
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

name_or_num:
| NAME { $1 }
| NUM { $1 }

maybev_prop:
| separated_pair(NAME, COLON, name_or_num) { PTEVal.KV $1 }
| ATTRS COLON LPAR separated_nonempty_list(COMMA, NAME) RPAR { PTEVal.Attrs $4 }

pteval:
| LPAR separated_nonempty_list(COMMA, maybev_prop) RPAR
    { PTEVal.of_list0 $2 }

maybev_notag:
| NUM  { Concrete $1 }
| location_global { $1 }
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
| NAME location  { ($2, (Ty $1,ParsedConstant.zero)) }
| ATOMIC NAME location { $3,(Atomic $2,ParsedConstant.zero)}
| NAME location EQUAL maybev { ($2,(Ty $1,$4))}
| NAME arrayspec
   { let (t,sz) = $2 in
     let v0 = Constant.mk_replicate sz ParsedConstant.zero in
     (t,(TyArray ($1,sz),v0)) }
| NAME arrayspec EQUAL LCURLY maybev_list RCURLY
   { let (t,sz) = $2 and vs = $5 in
     if sz = List.length vs then
       let arr = (TyArray ($1,sz),Constant.mk_vec sz vs) in
       (t, arr)
     else
       Warn.user_error
         "Declared size of array %s does not match initial vakue size"
	 (dump_location t) }
/* prohibit "v[i] = scalar" form in init allow only "v[i]={scalar_list}" */
| locindex EQUAL maybev { raise Parsing.Parse_error }
| NAME location EQUAL ATOMICINIT LPAR maybev RPAR { ($2,(Ty $1,$6))}
| NAME STAR location { ($3,(Pointer $1,ParsedConstant.zero))}
| NAME STAR location EQUAL amperopt maybev { ($3,(Pointer $1,$6))}
| STAR location { ($2,(TyDefPointer,ParsedConstant.zero))}
| STAR location EQUAL amperopt maybev { ($2,(TyDefPointer,$5))}
| location EQUAL LPAR separated_nonempty_list(COMMA, maybev_prop) RPAR 
  { ($1,(Ty "pteval_t", mk_pte_val $1 $4)) }

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

fault: FAULT LPAR lbl COMMA NAME RPAR { ($3,mk_sym $5) }

loc_item:
| rloc_typ { let a,t = $1 in LocationsItem.Loc (a,t) }
| fault {  Fault $1 }

loc_items:
| loc_item {$1::[]}
| loc_item SEMI {$1::[]}
| loc_item SEMI loc_items  {$1::$3}

locations:
| LOCATIONS LBRK loc_items RBRK { $3 }
| LOCATIONS LBRK RBRK { [] }
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
| location { ConstrGen.Loc $1 }
| locindex { $1 }

locindex:
| arrayspec
    { let (loc,sz) = $1 in Deref (loc,sz) }

arrayspec:
| NAME LBRK NUM RBRK
    { (Location_global (Constant.mk_sym $1),Misc.string_as_int $3) }


atom_prop:
| location EQUAL maybev {Atom (LV (Loc $1,$3))}
| location EQUAL LCURLY maybev_list RCURLY
    { let sz = List.length $4 in
      let vec = Constant.mk_vec sz $4 in
      Atom (LV (Loc $1,vec)) }
| location EQUALEQUAL maybev {Atom (LV (Loc $1,$3))}
| locindex EQUAL maybev {Atom (LV ($1,$3))}
| locindex EQUALEQUAL maybev {Atom (LV ($1,$3))}
| location NOTEQUAL maybev {Not (Atom (LV (Loc $1,$3)))}
| locindex NOTEQUAL maybev {Not (Atom (LV ($1,$3)))}
| location NOTEQUAL LCURLY maybev_list RCURLY
    { let sz = List.length $4 in
      let vec = Constant.mk_vec sz $4 in
      Not (Atom (LV (Loc $1,vec))) }
| location EQUAL location_deref {Atom (LL ($1,$3))}
| location EQUALEQUAL location_deref {Atom (LL ($1,$3))}
| fault { Atom (FF $1) }
| location EQUAL LPAR separated_nonempty_list(COMMA, maybev_prop) RPAR
  { Atom (LV (Loc $1, mk_pte_val $1 $4)) }

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
