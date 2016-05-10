(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
module type Parser = sig
  include GenParser.S
  type parsedPseudo
  val instr_from_string : string -> parsedPseudo list
end

module type Dumper = sig
    type pseudo
    val dump_info : 
      out_channel -> Name.t ->
      (MiscParser.state, (int * pseudo list) list,
       MiscParser.prop, MiscParser.location)
        MiscParser.result
      -> unit
end

module type Common = sig
    
  include ArchBase.S

  exception Error of string
      
  type substitution =
    | Reg of string * reg
    | Cst of string * int
    | Lab of string * string
    | Addr of string * string
    | Code of string * pseudo list

  val add_subs : substitution list -> substitution list ->
    substitution list
  val sr_name : reg -> string
  val cv_name : MetaConst.k -> string
  val dump_pseudos : pseudo list -> string
  val conv_reg : substitution list -> reg list ->
                 reg -> reg
  val find_lab : substitution list -> reg list ->
                 string -> string
  val find_code : substitution list -> reg list ->
                  string -> pseudo list
  val find_cst : substitution list -> reg list ->
                 string -> MetaConst.k
    
end

module MakeCommon : functor(A:ArchBase.S)
    -> Common with type reg = A.reg
	      and type instruction = A.instruction
	      and type parsedInstruction = A.parsedInstruction
	      and type pins = A.pins
	      and type 'k kpseudo = 'k A.kpseudo
	      and type parsedPseudo = A.parsedPseudo

module type S = sig
    include Common

    val match_instruction : substitution list -> 
			    parsedPseudo -> pseudo ->
			    substitution list option

    val instanciate_with : substitution list -> reg list ->
			   parsedPseudo list ->
			   pseudo list
   					 
    module Parser : Parser with type parsedPseudo = parsedPseudo
			   and type pseudo = pseudo
				  
    module Dumper : Dumper with type pseudo = pseudo
  end

module MakeArch : functor(I:sig

  include Common 
  val match_instr : substitution list ->
                    parsedInstruction ->
                    instruction ->
                    substitution list option
  val expl_instr : substitution list -> reg list ->
                   parsedInstruction ->
                   parsedInstruction
end)
    -> sig

      include Common 
	
      val match_instruction : substitution list -> 
                              parsedPseudo -> pseudo ->
                              substitution list option
	  
      val instanciate_with : substitution list -> reg list ->
                             parsedPseudo list ->
                             pseudo list

    end with type reg = I.reg
	and type instruction = I.instruction
	and type parsedInstruction = I.parsedInstruction
	and type pins = I.pins
	and type 'k kpseudo = 'k I.kpseudo
	and type parsedPseudo = I.parsedPseudo
  
module MakeParser : 
functor(A:ArchBase.S) ->
    functor
	 (P:sig
	      include GenParser.LexParse 
		      with type instruction = A.parsedPseudo
	      val instr_parser : 
	     (Lexing.lexbuf -> token) -> Lexing.lexbuf ->
	     A.parsedPseudo list
	    end)
      -> Parser with type pseudo = A.pseudo
		and type parsedPseudo =A.parsedPseudo
	    
module DefaultDumper : functor(A:ArchBase.S)
    -> Dumper with type pseudo = A.pseudo
