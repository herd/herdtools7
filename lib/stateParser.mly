%{
(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
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

let do_mk_sym_tagloc s o =
  if
    o < 0 ||
    o mod MachSize.granule_nbytes <> 0
  then raise Parsing.Parse_error ;
  Symbolic (TagAddr (VIR,s,o))

let mk_sym_tagloc s o =
  let o =
    try int_of_string o
    with Invalid_argument _ -> raise Parsing.Parse_error in
  do_mk_sym_tagloc s o

let mk_sym_tagloc_zero s = do_mk_sym_tagloc s 0

let mk_lab (p,s) = Symbolic (Virtual ({default_symbolic_data with name=Symbol.Label (p,s)}))
%}

%token EOF
%token <int> PROC
%token <string> SYMB_REG
%token <string> NAME
%token <string> DOLLARNAME
%token <string> NUM
%token <string> VALUE

%token TRUE FALSE
%token EQUAL NOTEQUAL EQUALEQUAL
%token FINAL FORALL EXISTS OBSERVED TOKAND NOT AND OR IMPLIES WITH FILTER
%token LOCATIONS FAULT STAR PLUS
%token LBRK RBRK LPAR RPAR LCURLY RCURLY SEMI COLON AMPER COMMA
%token ATOMIC
%token ATOMICINIT
%token ATTRS TOK_OA
%token TOK_PTE TOK_PA
%token TOK_TAG
%token TOK_NOP
%token <string> INSTR
%token <int * string> LABEL
%token PTX_REG_DEC
%token <string> PTX_REG_TYPE
%token TOK_PAR

%left OR
%left AND
%right IMPLIES
%nonassoc NOT

%type <ParsedPteVal.t> pteval
%start pteval
%type <ParsedAddrReg.t> addrregval
%start addrregval
%type <MiscParser.state> init
%start init
%type <MiscParser.location> main_location
%start main_location
%type < (MiscParser.location,MiscParser.maybev,MiscParser.fault_type) LocationsItem.t list * MiscParser.prop option * MiscParser.constr * (string * MiscParser.quantifier) list> constraints
%start constraints
%type  <MiscParser.constr> main_constr
%start main_constr
%type  <MiscParser.constr> skip_loc_constr
%start skip_loc_constr
%type  <(MiscParser.location,MiscParser.maybev,MiscParser.fault_type) LocationsItem.t list * MiscParser.constr> main_loc_constr
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
| NAME { Constant.mk_sym $1 }
| TOK_PTE LPAR NAME RPAR { Constant.mk_sym_pte  $3 }
| TOK_PTE LPAR TOK_PTE LPAR NAME RPAR RPAR { Constant.mk_sym_pte2 $5 }
| TOK_PA LPAR NAME RPAR { Constant.mk_sym_pa $3 }
| NAME COLON NAME { Constant.mk_sym_tag $1 $3 }
| TOK_TAG LPAR id=NAME RPAR { mk_sym_tagloc_zero id }
| TOK_TAG LPAR id=NAME PLUS o=NUM RPAR { mk_sym_tagloc id o }
(* TODO: have MTE and Morello tags be usable at the same time? *)
| NUM COLON NAME COLON NUM { Constant.mk_sym_morello $1 $3 $5}
| NAME COLON NUM { Constant.mk_sym_morello "0" $1 $3 }

name_or_num:
| NAME { $1 }
| NUM { $1 }

output_address:
| name=NAME { OutputAddress.parse name }
| TOK_PA LPAR name=NAME RPAR { OutputAddress.PHY name }
| TOK_PTE LPAR name=NAME RPAR { OutputAddress.PTE name }

prop_tail:
| { ParsedPteVal.empty }
| COMMA pteval=prop_head { pteval }

prop_head:
| TOK_OA COLON oa=output_address tail=prop_tail
    { ParsedPteVal.add_oa oa tail }
| key=NAME COLON v=name_or_num tail=prop_tail
    { ParsedPteVal.add_kv key v tail }
| a=NAME tail=prop_tail
    { ParsedPteVal.add_attr a tail }
| ATTRS COLON LPAR attrs=separated_nonempty_list(COMMA, NAME) RPAR
  tail=prop_tail
    { ParsedPteVal.add_attrs attrs tail }

pteval:
| LPAR pteval=prop_head RPAR { pteval }

addrregval_update_tail:
| { ParsedAddrReg.empty }
| COMMA addrregval=addrregval_prop_head { addrregval }

addrregval_prop_head:
| TOK_OA COLON oa=output_address tail=addrregval_update_tail
    { ParsedAddrReg.add_oa oa tail }
| key=NAME COLON v=NUM tail=addrregval_update_tail
    { ParsedAddrReg.add_kv key v tail }

addrregval:
| TOK_PAR COLON LPAR addrregval=addrregval_prop_head RPAR { addrregval }

maybev_notag:
| NUM  { Concrete $1 }
| VALUE { Concrete $1 }
| location_global { $1 }
/* conflicts with location_reg:
| NUM COLON NAME { mk_sym_morello $1 $3 "0" }
*/
(* TODO: restrict to something like "NUM COLON BOOL"? *)
| NUM COLON NUM { Concrete ($1 ^ ":" ^ $3) }
| NAME LBRK NUM RBRK { Constant.mk_sym_with_index $1 (Misc.string_as_int $3) }

maybev_amper:
| AMPER NAME { Constant.mk_sym $2 }
| AMPER NAME LBRK NUM RBRK { Constant.mk_sym_with_index $2 (Misc.string_as_int $4)}

maybev:
| maybev_notag { $1 }
| COLON NAME  { Tag $2 }

%inline maybev_or_amper:
| maybev { $1 }
| maybev_amper { $1 }

maybev_list:
| maybev_notag COMMA maybev_list { $1::$3 }
| maybev_notag { [$1] }

maybev_label:
| maybev { $1 }
| PROC COLON NAME { mk_lab ($1, $3) }
| NUM COLON NAME { mk_lab (Misc.string_as_int $1, $3) }
| l=LABEL { mk_lab l }

%inline std_reg:
| PROC COLON reg  {Location_reg ($1,$3)}
| NUM COLON reg   {Location_reg (Misc.string_as_int $1,$3)}

%inline location_reg:
| std_reg { $1 }
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
| loc_brk { $1 }

main_location:
| loc=location EOF { loc }

%inline location:
| location_reg { $1 }
| location_global { Location_global $1 }

%inline left_loc:
| loc=location { loc }
| LBRK loc=location_global RBRK { Location_global loc }

atom:
| location {($1,ParsedConstant.zero)}
| left_loc EQUAL maybev_label {($1,$3)}

instr:
| TOK_NOP { None }
| i=INSTR { Some i }

atom_init:
| atom { let x,v = $1 in x,(TyDef,v) }
| std_reg EQUAL maybev_amper { $1,(TyDefPointer,$3) }
| typ=NAME loc=left_loc  { (loc, (Ty typ,ParsedConstant.zero)) }
| ATOMIC typ=NAME loc=left_loc { loc,(Atomic typ,ParsedConstant.zero)}
| loc=left_loc EQUAL i=instr  { (loc,(Ty "ins_t", mk_instr_val i)) }
| NAME loc=left_loc EQUAL i=instr  { (loc,(Ty "ins_t", mk_instr_val i)) }
| ATOMIC typ=NAME loc=left_loc EQUAL v=maybev { loc,(Atomic typ,v)}
| typ=NAME loc=left_loc EQUAL v=maybev { (loc,(Ty typ,v))}
| typ=NAME loc=left_loc EQUAL ATOMICINIT LPAR v=maybev RPAR
   { (loc,(Ty typ,v))}
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
         "Declared size of array %s does not match initial value size"
	 (dump_location t) }
/* prohibit "v[i] = scalar" form in init allow only "v[i]={scalar_list}" */
| locindex EQUAL maybev { raise Parsing.Parse_error }
| typ=NAME STAR loc=left_loc { (loc,(Pointer typ,ParsedConstant.zero))}
| typ=NAME STAR loc=left_loc EQUAL v=maybev_or_amper { (loc,(Pointer typ,v))}
| STAR loc=left_loc { (loc,(TyDefPointer,ParsedConstant.zero))}
| STAR loc=left_loc EQUAL v=maybev_or_amper { (loc,(TyDefPointer,v))}
| typ=NAME loc=left_loc EQUAL v=pteval
  { (loc,(Ty typ, MiscParser.add_oa_if_none loc v)) }
| loc=left_loc EQUAL v=pteval
  { (loc,(Ty "pteval_t", MiscParser.add_oa_if_none loc v)) }

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

fault_pte_loc:
| TOK_PTE LPAR name=NAME RPAR { (Constant.mk_sym_pte name) }

fault_loc:
| name=NAME { Constant.mk_sym name }
| fl=fault_pte_loc { fl }

composite_faulttype:
| fst=NAME COLON rem=separated_nonempty_list(COLON,NAME)
     { String.concat ":" (fst::rem) }
fault:
| FAULT LPAR lab=lbl RPAR { (lab,None,None) }
| FAULT LPAR lab=lbl COMMA name=NAME RPAR
   { if FaultType.is name then (lab,None,Some name)
     else (lab,Some (Constant.mk_sym name),None) }
| FAULT LPAR lab=lbl COMMA fl=fault_pte_loc RPAR { (lab,Some fl,None) }
| FAULT LPAR lab=lbl COMMA ft=composite_faulttype RPAR { (lab,None,Some ft) }
| FAULT LPAR lab=lbl COMMA fl=fault_loc COMMA
  ft=separated_nonempty_list(COLON, NAME) RPAR
    { (lab,Some fl,Some (String.concat ":" ft)) }


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

%inline equal:
| EQUAL { () }
| EQUALEQUAL { () }

%inline loc_brk:
| LBRK loc=location_global RBRK { Location_global loc }

atom_prop:
| loc=location EQUAL i=instr
  {Atom (LV (Loc loc,(mk_instr_val i)))}
| loc=location EQUAL l=LABEL
    {Atom (LV (Loc loc,(mk_lab l)))}
| location equal maybev {Atom (LV (Loc $1,$3))}
| loc=loc_brk equal v=maybev
   {Atom (LV (Loc loc,v))}
| location NOTEQUAL maybev {Not (Atom (LV (Loc $1,$3)))}
| loc=loc_brk NOTEQUAL v=maybev
   {Not (Atom (LV (Loc loc,v)))}
| loc=location equal v=pteval
  { Atom (LV (Loc loc, MiscParser.add_oa_if_none loc v )) }
| loc=loc_brk equal v=pteval
  { Atom (LV (Loc loc, MiscParser.add_oa_if_none loc v)) }
| loc=location equal v=addrregval
  { Atom (LV (Loc loc, AddrReg v)) }
/* Array, array cell, equality of content no [x] = .. notation */
| location equal LCURLY maybev_list RCURLY
    { let sz = List.length $4 in
      let vec = Constant.mk_vec sz $4 in
      Atom (LV (Loc $1,vec)) }
| location NOTEQUAL LCURLY maybev_list RCURLY
    { let sz = List.length $4 in
      let vec = Constant.mk_vec sz $4 in
      Not (Atom (LV (Loc $1,vec))) }
| locindex equal maybev {Atom (LV ($1,$3))}
| locindex NOTEQUAL maybev {Not (Atom (LV ($1,$3)))}
| location equal location_deref {Atom (LL ($1,$3))}
| loc1=loc_brk  equal loc2=loc_brk
    { Atom (LL (loc1,loc2)) }
| loc1=loc_brk  equal loc2=location_reg
    { Atom (LL (loc1,loc2)) }
/* The following rule add conflicts, e.g shifting it vs.
   reducing this production first rule.
   Delitng it is not a real problem by symetry of equal */
/* | loc1=location_reg  equal loc2=loc_brk
    { Atom (LL (loc1,loc2)) } */
| fault { Atom (FF $1) }

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
