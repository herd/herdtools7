[@@@warning "-40-42"]
(* Create a set of relaxations for diy using a cat file *)

open Printf

(*
Bugs
- FreIP output as a single relaxation when generating for -let Instr-obs. probably due to something in pp_relaxations when calculating relaxations for after's
- output of -let Haz-ob to be verified
- bob is not generating properly with L and A. Potentially because of something with po's being eaten and atom info not being transfered
- Po being eaten by a surrounding edge is not correctly transferring all its information (i.e. direction, annotations etc)

things to add
- support for pick-lob
- just R in a sequence (i.e. R and not [Exp&R]) should be a super set of an explicit read and also implicit instruction read. (for cse-ob primarily)

code improvements
- marked within functions with TODO: at beginning of comment
- Move sub-functions in pp_relaxations relating to afters out of the function to improve legibility

further work
- add functionality for output to be a configuration file
- make documentation explaining how tool works and when to use
- don't output composite relaxations with DC/IC when no instruction fetch is included
- implement support for faults (reliant on diy updates)
- implement more comprehensive support for pte and mte
*)

let prog =
  if Array.length Sys.argv > 0 then Filename.basename Sys.argv.(0)
  else "cat2config7"

module Make (O : sig
  val verbose : int
  val lets_to_print : string list
  val conds : string list
  val unroll : int
  val print_tree : bool
end) =
struct
  module ML = MyLib.Make (struct
    let includes = []
    let env = Some "HERDLIB"
    let libdir = Filename.concat Version.libdir "herd"
    let debug = O.verbose > 0
  end)

  module ParserConfig = struct
    let debug = O.verbose > 2
    let libfind = ML.find
  end

  module A = AArch64Arch_gen.Make (struct
    include AArch64Arch_gen.Config

    let moreedges = !Config.moreedges
  end)

  module E = Edge.Make (Edge.Config) ((A : Fence.S))

  (*
      ---------------------------------------------------------------
      Custom types
      ---------------------------------------------------------------
  *)
  open Code

  exception NotImplemented of string
  exception Skip of string

  type extr = Code.extr
  type atom = A.atom
  type edge = E.edge
  type edge_node = { node_val : edge; branch_list : edge_node list }

  module Parser = ParseModel.Make (ParserConfig)

  type 'a union = Union of 'a list
  type 'a sequence = Sequence of 'a list
  type 'a intersection = Intersection of 'a list | Event of 'a list
  type var = string
  (*
       ir is used to represent the let statements
       each let statement consists of a union of sequences. Each sequence is a list of intersections
       where single variables are represented as intersections of length 1
      *)

  type ir = var intersection sequence union
  (* let_statements is the list of all let statements in an AST*)

  type let_statements = (string * ir) list
  type pn = Prev | Next

  type edge_ir =
  (*TODO: Some of these do not have to be their own type but can probably be
  merged into each other to simplify*)
    | Tedge of E.tedge * string
    | Empty of string
    | Imp of string
    | PTE of string
    | D_atom of extr * atom option * string
    | Exp of string
    | Mix of edge_ir * edge_ir * string
    | Loc of sd * string
    | Ie of ie * string
    | Fr_single of string
    | Rf_single of string
    | Ws_single of string
    | After of edge_ir * string
    | DC of pn option * string
    | IC of pn option * string
    | Ignore
    | First of string

  (*
    ------------------------------------------
    helper functions
    ------------------------------------------
    *)
  let can_merge_po = function

  | Tedge ((E.Fenced _ | E.Dp _ | E.Rmw _ | E.Insert _), _) -> true
  | _ -> false
  let lxsx = [ Tedge (Rmw A.LrSc, "lrsc") ]

  let amo =
    [
      Tedge (Rmw A.Swp, "Amo.Swp");
      Tedge (Rmw A.Cas, "Amo.Cas");
      Tedge (Rmw (LdOp A_ADD), "Amo.LdAdd");
      Tedge (Rmw (LdOp A_EOR), "Amo.LdEor");
      Tedge (Rmw (LdOp A_SET), "Amo.LdSet");
      Tedge (Rmw (LdOp A_CLR), "Amo.LdClr");
      Tedge (Rmw (StOp A_ADD), "Amo.StAdd");
      Tedge (Rmw (StOp A_EOR), "Amo.StEor");
      Tedge (Rmw (StOp A_SET), "Amo.StSet");
      Tedge (Rmw (StOp A_CLR), "Amo.StClr");
    ]

  let match_var v : edge_ir union =
    let open A in
    let open E in
    let matching =
      match v with
      | "R" -> [ D_atom ( (Dir R), None,  v) ]
      | "W" -> [ D_atom ( (Dir W), None,  v) ]
      | "M" -> [ D_atom ( Irr, None, v) ]
      | "L" -> [ D_atom (NoDir, Some (A.Rel None, None), v) ]
      | "P" -> [ D_atom (NoDir, Some (A.plain, None), v) ]
      | "A" -> [ D_atom (NoDir, Some (A.Acq None, None), v) ]
      | "Q" -> [ D_atom (NoDir, Some (A.AcqPc None, None), v) ]
      | "T" | "Tag" -> [ D_atom (NoDir, Some (A.Tag, None), v) ]
      | "Instr" ->  [D_atom (NoDir, Some (A.Instr, None), v)]
      | "Exp" -> [Exp v]
      | "Imp" | "NExp" -> [ Imp v ]
      | "po" -> [ Tedge (Po (Diff, Irr, Irr), v) ]
      | "po-loc" -> [ Tedge (Po (Same, Irr, Irr), v) ]
      | "lrs" -> [ Tedge (Rf Int, v) ]
      | "addr" -> [ Tedge (Dp ((A.D.ADDR, A.NoCsel), Same, Irr), v) ]
      | "ctrl" -> [ Tedge (Dp ((A.D.CTRL, A.NoCsel), Same, Irr), v) ]
      | "data" -> [ Tedge (Dp ((A.D.DATA, A.NoCsel), Same, Irr), v) ]
      | "DMB.ISH" ->
          [ Tedge (Fenced (Barrier (DMB (ISH, FULL)), Same, Irr, Irr), v) ]
      | "DMB.ISHLD" ->
          [ Tedge (Fenced (Barrier (DMB (ISH, LD)), Same, Irr, Irr), v) ]
      | "DMB.ISHST" ->
          [ Tedge (Fenced (Barrier (DMB (ISH, ST)), Same, Irr, Irr), v) ]
      | "DMB.OSH" ->
          [ Tedge (Fenced (Barrier (DMB (OSH, FULL)), Same, Irr, Irr), v) ]
      | "DMB.OSHLD" ->
          [ Tedge (Fenced (Barrier (DMB (OSH, LD)), Same, Irr, Irr), v) ]
      | "DMB.OSHST" ->
          [ Tedge (Fenced (Barrier (DMB (OSH, ST)), Same, Irr, Irr), v) ]
      | "dmb.fullsy"
      | "DMB.SY" ->
          [ Tedge (Fenced (Barrier (DMB (SY, FULL)), Same, Irr, Irr), v) ]
      | "DMB.LD" ->
          [ Tedge (Fenced (Barrier (DMB (SY, LD)), Same, Irr, Irr), v) ]
      | "DMB.ST" ->
          [ Tedge (Fenced (Barrier (DMB (SY, ST)), Same, Irr, Irr), v) ]
      | "DSB.ISH" ->
          [ Tedge (Fenced (Barrier (DSB (ISH, FULL)), Same, Irr, Irr), v) ]
      | "DSB.ISHLD" ->
          [ Tedge (Fenced (Barrier (DSB (ISH, LD)), Same, Irr, Irr), v) ]
      | "DSB.ISHST" ->
          [ Tedge (Fenced (Barrier (DSB (ISH, ST)), Same, Irr, Irr), v) ]
      | "DSB.OSH" ->
          [ Tedge (Fenced (Barrier (DSB (OSH, FULL)), Same, Irr, Irr), v) ]
      | "DSB.OSHLD" ->
          [ Tedge (Fenced (Barrier (DSB (OSH, LD)), Same, Irr, Irr), v) ]
      | "DSB.OSHST" ->
          [ Tedge (Fenced (Barrier (DSB (OSH, ST)), Same, Irr, Irr), v) ]
      | "DSB.SY" ->
          [ Tedge (Fenced (Barrier (DSB (SY, FULL)), Same, Irr, Irr), v) ]
      | "DSB.LD" ->
          [ Tedge (Fenced (Barrier (DSB (SY, LD)), Same, Irr, Irr), v) ]
      | "DSB.ST" ->
          [ Tedge (Fenced (Barrier (DSB (SY, ST)), Same, Irr, Irr), v) ]
      | "ISB" -> [ Tedge (Insert (Barrier ISB), v) ]
      (* to reduce tests which don't produce meaningful differences, the following 4 sets are limited to these options *)
      | "dsb.full" -> [ Tedge (Fenced (Barrier (DSB (ISH, FULL)), Same, Irr, Irr), v) ]
      | "dsb.ld" -> [ Tedge (Fenced (Barrier (DSB (SY, LD)), Same, Irr, Irr), v) ]
      | "dsb.st" -> [ Tedge (Fenced (Barrier (DSB (SY, ST)), Same, Irr, Irr), v) ]
      | "ca" -> [Fr_single v]
      | "rfi" -> [ Tedge (Rf Int, v) ]
      | "lxsx" -> lxsx
      | "co" -> [Ws_single v]
      | "fr" -> [Fr_single v]
      | "rf" -> [Rf_single v]
      | "fri" -> [ Tedge (Fr Int, v) ]
      | "fre" -> [ Tedge (Fr Ext, v) ]
      | "rfe" -> [ Tedge (Rf Ext, v) ]
      | "coe" -> [ Tedge (Ws Ext, v) ]
      | "coi" -> [ Tedge (Ws Int, v) ]
      | "amo" -> amo
      | "rmw" -> lxsx @ amo
      | "sm" | "si" | "ignore" -> [ Ignore ]
      | "pick-ctrl-dep" -> [ Tedge (Dp ((A.D.CTRL, A.OkCsel), Same, Irr), v) ]
      | "pick-addr-dep" -> [ Tedge (Dp ((A.D.ADDR, A.OkCsel), Same, Irr), v) ]
      | "pick-data-dep" -> [ Tedge (Dp ((A.D.DATA, A.OkCsel), Same, Irr), v) ]
      | "DC.CVAU" -> [DC (Some Prev, v)]
      | "IC.IVAU" -> [IC (Some Prev, v)]
      | "ext" -> [Ie (Ext, v)]
      | "DC-after" -> [After ((DC (Some Prev, v)), v)]
      | "IC-after" -> [After ((IC (Some Prev, v)), v)]
      | "iico_data" | "iico_order" | "iico_ctrl" | "pick-basic-dep" | "TagCheck"
      | "Fault" | "MMU" | "TLBI-after" | "TLBI" | "PTE" | "TTD" | "same-instance"
      | "rf-reg" | "BCC" | "Rreg" | "loc" | "same-low-order-bits" 
      | "IC.IALLUIS" | "IC.IALLU" | "tc-ib" | "sca-class"
      | "EXC-ENTRY" | "EXC-RET-CSE" | "PTEV"
      | "EXC-ENTRY-CSE" -> raise (Skip (Printf.sprintf "%s not implemented" v))
      | _ -> [ Empty v ]
    in
    Union matching

  let check_dirfroma letl a =
    match letl with
    | Some (A.Acq None, None) | Some (A.AcqPc None, None) -> Dir R
    | Some (A.Rel None, None) -> Dir W
    | _ -> a

  let pp_op2 a =
    let open AST in
    match a with
    | Union -> "Union"
    | Inter -> "Inter"
    | Diff -> "Diff"
    | Seq -> "Seq"
    | Cartesian -> "Cartesian"
    | Add -> "Add"
    | Tuple -> "Tuple"

  let pp_op1 a =
    let open AST in
    match a with
    | Plus -> "Plus"
    | Star -> "Star"
    | Opt -> "Opt"
    | Comp -> "Comp"
    | Inv -> "Inv"
    | ToId -> "ToId"

  let pp_exp a =
    let open AST in
    match a with
    | Konst _ -> "Konst"
    | Tag _ -> "Tag"
    | Var (_,v) -> sprintf "Var: %s" v
    | Op1 (_,op1,_) -> sprintf "Op1: %s" (pp_op1 op1)
    | Op (_,op2,_) -> sprintf "Op: %s" (pp_op2 op2)
    | App _ -> "App"
    | Bind _ -> "Bind"
    | BindRec _ -> "BindRec"
    | Fun _ -> "Fun"
    | ExplicitSet _ -> "ExplicitSet"
    | Match _ -> "Match"
    | MatchSet _ -> "MatchSet"
    | Try _ -> "Try"
    | If _ -> "If"
  let pp_edge_ir a =
    match a with
    | Tedge (_,v) -> (sprintf "tedge: %s" v)
    | Empty _ -> "Empty"
    | Imp _ -> "Imp"
    | PTE _ -> "PTE"
    | After (DC _, _) -> (sprintf "DC_a")
    | After (IC _, _) -> (sprintf "IC_a")
    | D_atom (_,_, v) -> (sprintf "D_atom: %s" v)
    | Exp _ -> "exp"
    | Rf_single _ -> "rf"
    | Fr_single _ -> "fr"
    | Ws_single _ -> "ws"
    | DC _ -> (sprintf "DC")
    | IC _ -> (sprintf "IC")
    | Ignore | Mix _ | Loc _ | Ie _ | _ -> ""
  let get_inter = function
    | Intersection a
    | Event a -> a
  let pp_intersection exp =
    let exp = get_inter exp in
    match exp with
    | h :: [] -> if h = "ignore" then "" else h
    | h :: t -> sprintf "[%s]" (String.concat "&" (h :: t))
    | [] -> raise (Misc.Fatal "Intersection cannot have an empty list")

  let pp_sequence (Sequence expl) =
    String.concat ";" (List.filter (fun a -> match a with "" -> false | _ -> true) (List.map pp_intersection expl))

  let pp_tree (tree : let_statements) : unit =
    (* This function takes a list of let statements, in the form
        of string * expression, and prints the expanded tree of the cat file.
        It is important to note that the output gets quite large, as the fully expanded statements are very long
        Inputs:
          - tree: A list of let statements, as ast type.
        Outputs:
          - Unit
    *)
    let pp (name, Union ins) =
      printf "\n\n(%s)\n" name;
      printf "   %s" (String.concat "\n  |" (List.filter (fun a -> match a with "" -> false | _ -> true) (List.map pp_sequence ins)))
    in
    List.iter pp tree

  let empty_expr = Empty ""
  let first_expr = First ""

  let get_option_list l =
    let f a = match a with Some x -> [ x ] | None -> [] in
    Misc.concat_map f l

  let get_option a = List.hd
   (match a with
   | Some x -> [ x ]
   | None -> [AST.Konst ({loc_start=Lexing.dummy_pos;loc_end=Lexing.dummy_pos;loc_ghost=true},Empty (SET))])

  (* ir, sequence and inter helper functions*)
  let make_sequence expl = Sequence expl
  let intersection_singleton exp = Intersection [ exp ]
  let concatenate_sequences (Sequence s1) (Sequence s2) = Sequence (s1 @ s2)
  let seq_to_inter (Sequence s1) = Intersection (List.fold_left (fun a b -> a@(get_inter b)) [] s1)
  let ir_single_inter : var intersection -> ir =
   fun inter -> Union [ Sequence [ inter ] ]
  
  let ir_single_var : var -> ir =
   fun var -> ir_single_inter (Intersection [ var ])

  let ir_multiple_inter : var intersection list -> ir = fun inter_list -> Union (List.map (fun a -> make_sequence [a]) inter_list)
  let make_id is_id (Union exp) = 
    (* TODO: code improvement *)
    let f = 
      Union (List.map 
      (fun (Sequence s) -> 
        Sequence (List.map 
        (fun i -> 
          Event (get_inter i))
        s)) 
      exp) in
    if is_id then f else (Union exp)
  let union_concat_map (f : 'a -> 'b union) (Union xs : 'a union) : 'b union =
    let f' x =
      let (Union ys) = f x in
      ys
    in
    Union (Misc.concat_map f' xs)

  let union_map f (Union xs) : 'b union =
    let f' x =
      let ys = f x in
      ys
    in
    Union (List.map f' xs)

  let rec union_fold_cross l f =
    match l with
    | [] -> [ [] ]
    | hd :: tl ->
        let (Union expanded_hd) = f hd in
        let expanded_tl = union_fold_cross tl f in
        Misc.concat_map
          (fun hd_item ->
            List.map (fun tl_item -> hd_item :: tl_item) expanded_tl)
          expanded_hd

  let rec fold_cross l f =
    match l with
    | [] -> [ [] ]
    | hd :: tl ->
        let expanded_hd = f hd in
        let expanded_tl = fold_cross tl f in
        Misc.concat_map
          (fun hd_item ->
            List.map (fun tl_item -> hd_item :: tl_item) expanded_tl)
          expanded_hd

  let match_inter expl =
    (* TODO: This function currently works by comparing the intersections' variables string and edge type to a list of strings
    this should be improved for better code legibility and to make it more general*)
    let has_edge s a = match s, a with
    | "R", D_atom (Dir R,_, _) | "W", D_atom (Dir W,_, _)| "M", D_atom (Irr,_, _)
    | "Imp", Imp _
    | "T", D_atom (_,Some (A.Tag, None), _) 
    | "PTE", PTE _ | "Exp", Exp _ 
    | "rf", Rf_single _ | "co", Ws_single _ | "fr", Fr_single _
    | "ext", Ie _
    | "Instr", D_atom (_, Some (A.Instr, None), _)
    -> true
    | _ -> false in
    let has_edges l = List.for_all (fun a -> List.exists (has_edge a) expl) l in
    match expl with
    | [_; _] when has_edges ["R";"T"] -> D_atom ( (Dir R), Some (A.Tag, None), "tag read")
    | [_; _] when has_edges ["M";"Exp"] -> D_atom ( Irr, None, "memory event")
    | [_; _] when has_edges ["R";"Exp"] -> D_atom ( (Dir R), None,  "read")
    | [_; _] when has_edges ["W";"Exp"] -> D_atom ( (Dir W), None,  "write")
    
    | [_;_ ] when has_edges ["rf";"ext"] -> Tedge ( Rf Ext, "rfe")
    | [_;_ ] when has_edges ["fr";"ext"] -> Tedge ( Fr Ext, "fre")
    | [_;_ ] when has_edges ["co";"ext"] -> Tedge ( Ws Ext, "coe")
    | [ h; h2 ] -> raise (NotImplemented
            (sprintf "inter not implemented in matcher: %s"
            (sprintf "[%s] " (String.concat "," (List.map pp_edge_ir [h; h2])))))
    | [_; _;_] when has_edges ["R";"T";"Imp"] -> D_atom ( (Dir R), Some (A.Tag, None), "ImpTag Read")
    | [_; _;_] when has_edges ["R";"Instr";"Imp"] -> D_atom ( (Dir R), Some (A.Instr, None), "Imp instr Read")
    | [ h; h2; h3 ] -> raise (NotImplemented 
        (sprintf "inter not implemented in matcher: %s" 
        (sprintf "[%s] " (String.concat "," (List.map pp_edge_ir [h; h2;h3])))))
    | _ -> raise (NotImplemented
        (sprintf "inter not implemented in matcher: %s"
        (sprintf "[%s] " (String.concat "," (List.map pp_edge_ir expl)))))

  let unroll_inter expr : AST.exp list =
    let open AST in
    let rec f l =
      match l with
      | Op (_, Inter, expl) -> Misc.concat_map f expl
      | _ -> [ l ]
    in
    match expr with
    | Op (_, Inter, exp) -> (Misc.concat_map f exp)
    | _ -> raise (Misc.Fatal "non intersection passed to unroll inter function")
  (*
    ------------------------------------------
      Parse AST and make into custom type
    ------------------------------------------   
    *)

  let get_ins ins =
    (* Returns the name and expression of the given AST instruction
       Inputs:
         - ins: the AST instruction to extract the name and expression from
       Outputs:
         - varname: the name of the instruction, as AST.pat
         - expression: the expression of the instruction, as AST.exp
    *)
    let open AST in
    match ins with
    | Rec (_, (_, Pvar (Some varname), expression) :: _, _)
    | Let (_, (_, Pvar (Some varname), expression) :: _) ->
        (match expression with
        | Op (_, Diff, expl) -> (match (List.hd expl) with
          | Var (_,a) -> if a = varname then raise (Skip "skipping recursive definition")
          | _ -> ())
        | _ -> ());
        if List.exists (fun v -> varname = v)
          ["po-loc";"addr";"ctrl";"data"; "lrs"; "pick-addr-dep";"pick-ctrl-dep"; "pick-data-dep"; "pick-basic-dep"; "DC-after"; "IC-after"; "dsb.full"; "dsb.st"; "dsb.ld"; "ca"]
          then raise (Skip (sprintf "Custom %s not supported" varname)) else
        (varname, expression)
    | _ -> raise (Skip "instruction not supported")

  let expand_var tree (a, var) =
    (* compare a single AST variable to all let statements. If a match is found, return that expression*)
    let open AST in
    let is_var (name, _) = String.equal name var in
    match List.find_opt is_var tree with
    | Some (_, exp) -> exp
    | None -> Var (a, var)

  let rec inline_vars varname tree expression =
    (*Expand AST function logic and inline variables that are defined in other let statements*)
    let open AST in
    match expression with
    | Op (a, op, expl) ->
        Some
          (Op (a, op, get_option_list (List.map (inline_vars varname tree) expl)))
    | Op1 (a, op, exp) ->
        Some (Op1 (a, op, get_option (inline_vars varname tree exp)))
    | Var (a, var) -> (
        (* checking a variable against the name of its let statement allows
           for the removal of recursive definitions*)
        match List.find_opt (fun a -> String.equal var a) varname with
        | Some _ -> None
        | None -> (
            let br = expand_var tree (a, var) in
            match br with
            | Var (_, "emptyset") -> None
            | Var (_, _) -> Some br
            | Op (_, _, _) | Op1 (_,_,_)-> inline_vars (var :: varname) tree br
            | _ -> Some expression))
    | App (_, exp1, exp2) -> (
        match exp1 with
        | Var (_, ("range"|"domain")) -> inline_vars varname tree exp2
        | Var (_, ("intervening-write"))
        | Var (_, ("same-oa"))
        | Var (_, "oa-changes")
        | Var (_, ("intervening"))
        | Var (_, "at-least-one-writable") -> None
        (* Skip will skip entire let *)
        | Var (_, ("fencerel")) -> raise (Skip (sprintf "%s cannot generate relaxations because fencerel is not supported" (List.hd varname)))
        | Var (_,s) ->
            raise
              (Misc.Fatal (sprintf "%s cannot generate relaxations because %s is not supported" (List.hd varname) s))
        | _ -> raise (Misc.Fatal (sprintf "%s cannot generate because an unknown function is not supported" (List.hd varname))))
    | Try _ -> raise (Skip (sprintf "%s cannot generate because try is not supported" (List.hd varname)))
    |  If (_,VariantCond a,exp,exp2) -> begin
      let find_var v = begin
        match List.find_opt (fun s -> s=v) O.conds with
        | Some _ ->
          true
        | None -> false
      end in
      let rec eval_variant_cond = function
      | Variant v -> find_var v
      | OpNot v -> not (eval_variant_cond v)
      | OpAnd (v1,v2) -> (eval_variant_cond v1) && (eval_variant_cond v2)
      | OpOr (v1,v2) -> (eval_variant_cond v1) || (eval_variant_cond v2) in
      if eval_variant_cond a then (inline_vars varname tree exp)
      else (inline_vars varname tree exp2)
      end
    | Fun _ -> None
    | Konst _ -> None
    | _ ->
        raise
          (Misc.Fatal
             (sprintf "%s cannot generate because the following expression is not supported: %s" (List.hd varname) (pp_exp expression)))

  let get_dir = function
    | "R" -> Some (Dir R) 
    | "W" -> Some (Dir W)
    | "A" -> Some (Dir R) 
    | "L" -> Some (Dir W)
    | "Q" -> Some (Dir W)
    | _ -> None
  let get_annotation = function
    | "P" -> Some A.plain
    | "A" -> Some (A.Acq None)
    | "Q" -> Some (A.AcqPc None)
    | "L" -> Some (A.Rel None)
    | "T" -> Some A.Tag
    | "I" -> Some A.Instr
    | _ -> None
  let get_explicitness = function
    | "Exp" -> Some (Exp "e")
    | "NExp" | "Imp" -> Some (Imp "i")
    | _ -> None
  let compare_id a b =
    let open A in
    let apply_get_state state1 state2 =
      match state1, state2 with
      | None, None -> None
      | Some a, None -> Some a
      | None, Some a -> Some a
      | Some a, Some b -> if a = b then Some a else raise (NotImplemented "states do not match") in
    let compare_states a b =
      match a, b with
      | None, Some _ -> 1
      | Some _, None -> -1
      | None, None -> 0
      | Some a, Some b -> if a = b then 0 else raise (NotImplemented "states do not match") in
    

    let get_value ele = 
      let dir = get_dir ele in
      let ann = get_annotation ele in
      let exp = get_explicitness ele in
      if Option.is_none dir && Option.is_none ann && Option.is_none exp && not (ele = "M") && not (ele = "id") then raise (NotImplemented (Printf.sprintf "the following is not parsed as a value: %s\n" ele));
      dir,ann,exp in
    let get_state l = List.fold_left (
      fun (a_dir, a_ann, a_exp) a -> 
        let b_dir, b_ann, b_exp = get_value a in
        apply_get_state a_dir b_dir, apply_get_state a_ann b_ann, apply_get_state a_exp b_exp
    ) (None,None,None) l in
    try 
      let (a_dir, a_ann, a_exp) = get_state a in
      let (b_dir, b_ann, b_exp) = get_state b in
      ignore (apply_get_state a_dir b_dir);
      ignore (apply_get_state a_ann b_ann);
      ignore (apply_get_state a_exp b_exp);
      (* TODO: the following code can be improved.*)
      match compare_states a_ann b_ann with
      | 1 -> b
      | -1 -> a
      | 0 -> begin
        match compare_states a_dir b_dir with
        | 1 -> b
        | -1 -> a
        | 0 -> begin
          match compare_states a_exp b_exp with
          | 1 -> b
          | -1 -> a
          | 0 -> raise (NotImplemented "elements do not match")
          | _ -> raise (Misc.Fatal "impossible outcome")
        end
        | _ -> raise (Misc.Fatal "impossible outcome")
      end
      | _ -> raise (Misc.Fatal "impossible outcome")
    with NotImplemented s -> raise (Skip s)
  let solve_id tree = 
    (* Iterate over all sequences where there are two ToId next to each other, then solve for and remove 1 *)
    let rec iter_sequence seq = match seq with
      | [] -> []
      | h::h2::t -> begin

        match h, h2 with
        | Event a, Event b -> 
          if List.equal String.equal a b then Intersection a::(iter_sequence t)
          else
            (
            try
              let new_inter = compare_id a b in
              Intersection new_inter::(iter_sequence t)
            with Skip s -> 
              raise (Skip s)
            )
        | _,_ -> h::iter_sequence (h2::t) end 
      | h::t -> h::(iter_sequence t) in
    
    List.map (fun (n,Union t) -> 
      n, Union (List.map 
      (fun (Sequence s) -> try 
        Sequence (iter_sequence s)
    with 
    | Skip _-> Sequence []
    | NotImplemented s -> raise (NotImplemented s)
    ) t)) 
      tree
    
  let rec apply_match_var a : edge_ir union =
    let b = get_inter a in
    let a = List.filter_map (fun x -> if (match_var x) = Union [Ignore] then None else Some x) b in
    match a with
    | h :: [] ->
        (* single variables *)
        let v = match_var h in begin
        match v with
        | Union [ (Empty _) ] -> raise (Misc.Fatal (sprintf "Variable not in matcher: %s" h))
        | _ -> v end
    | h :: t ->
        (* Intersections *)
        let inter_list = List.map intersection_singleton (h :: t) in
        let new_inters = union_fold_cross inter_list apply_match_var in
        Union (List.map match_inter new_inters)
    | _ -> if 
              List.for_all (fun x -> if (match_var x) = Union [Ignore] then true else false) b 
            then raise (Skip "has ignore") 
            else raise (Misc.Fatal "cannot have empty intersection")

  let var_to_edge tree =
    (* translate variables in let statements to edge_ir type
       Inputs:
         - tree: list of let statemenets
       Outputs: list of let statements with var replaced by edge_ir in the ir type
    *)
    let edges_of_sequence expl =
      (fun (Sequence seq) ->
        try
          Union (List.map make_sequence (union_fold_cross seq apply_match_var))
        with Skip msg ->
          if O.verbose > 1 then
            eprintf "the following sequence %s didn't generate relaxations because: %s\n" (pp_sequence (Sequence seq)) msg;
          Union [])
        expl
    in
    let edges_of_union let_name =
      let name, p =
        List.find (fun (name, _) -> String.equal name let_name) tree
      in
      (name, union_concat_map edges_of_sequence p)
    in 
    (* when compiling edges of tree, also include local-hw-reqs for the use in DC calculations later *)
    try List.map edges_of_union (O.lets_to_print@["local-hw-reqs"])
    with Not_found ->
      raise
        (Misc.Fatal
           (sprintf "let statements that were asked for are not in cat file"))

  (*
       ---------------------------------------------------------------
       Expand sequences and replace non-primitive variables
       --------------------------------------------------------------- 
*)
  let repeat l t =
    let rec f n = match n with
    | 0 ->  []
    | _ -> l::(f (n-1)) in
    let rec ff m = match m with
    | 0 -> []
    | _ -> (AST.Op (t,AST.Seq,f m))::(ff (m-1)) in
    ff O.unroll
  let rec apply_expand tree : let_statements =
    (* Expand operations in AST and translate AST to internal representation
       Input:
       - tree : (var * AST.exp) list
       Output:
       - let_statements : (var * ir) list
    *)
    let rec f (name, instr) = try
      match instr with
      | AST.Op (_, AST.Seq, _) | AST.Op (_, AST.Inter, _) ->(name, expand false instr)
      | AST.Op1 (t,AST.Plus,exp) -> f (name, AST.Op (t,AST.Union,repeat exp t))
      | AST.Op1 (_,AST.Inv, _) -> raise (Skip "Let statement defined as an inverse relation is not supported")
      | AST.Op1 (_,AST.Comp, _) -> raise (Skip "Let statement defined as a complement is not supported")
      | AST.Op (_,AST.Diff, expl) -> (name, expand false (List.hd expl))
      | AST.Konst (_,AST.Empty _) -> raise (Skip "Expression is empty")
      | AST.Op (_, AST.Union, expl) ->
          (name, union_concat_map (expand false) (Union expl))
      | AST.Var (_,_) -> (name, expand false instr)
      | _ ->
          raise
            (Misc.Fatal (sprintf "Expression not supported: %s" (pp_exp instr)))
      with
      | Skip s -> if O.verbose > 1 then Printf.eprintf "\nlet statement (%s) not generated because of: %s\n" name s;(name, Union [])
      | NotImplemented s -> if O.verbose > 0 then Printf.eprintf "\nlet statement (%s) not generated because of: %s\n" name s;(name, Union [])
    in
    List.map f tree

  and expand_expression is_id input_item : ir =
    (* Expand a chosen expression, unrolling intersections, expanding unions and extracting variables from operations
       Inputs:
       - input_item: expression to expand (AST.exp)
       Output:
       - list of var intersection lists. each var intersection list represents a sequence
    *)
    let open AST in
    let make_id = make_id is_id in
    match input_item with
    | Op (_, AST.Union, expl) ->
        union_concat_map (expand_expression is_id) (Union expl) (* union of variables *)
    | Op1 (_, AST.ToId, exp) -> expand_expression true exp 
    | Op1 (t, AST.Plus, exp) -> (expand_expression is_id) (Op (t,AST.Union, repeat exp t))
    | Op1 (t, AST.Star, exp) -> (expand_expression is_id) (Op (t,AST.Union, Var (t,"ignore")::repeat exp t))
    | Op1 (t,AST.Opt, exp) -> union_concat_map (expand_expression is_id) (Union [exp; Var (t,"ignore")])
    | Op1 (_,AST.Inv, exp) ->
         let Union a = (expand_expression is_id) exp in
         Union (List.map (fun (Sequence b) -> Sequence (List.rev b)) a)
    | Op1 (_,AST.Comp, exp) ->
      (match exp with
      | Op (_,AST.Inter, _) ->
        let unrolled = unroll_inter exp in
        (match unrolled with
        | [Var (_,"NExp"); Var (_,"M")] -> make_id (ir_multiple_inter [Intersection ["Exp"; "M"]; Intersection ["ISB"]; Intersection["DC.CVAU"]; Intersection ["IC.IVAU"]])
        | [Var (_,"NExp"); Var (_,"Instr"); Var (_,"R")] -> make_id (ir_single_inter (Intersection ["Exp"; "R"]))
        | _ -> raise (Skip (sprintf "Complement not supported: ~(%s)" (String.concat ";" (List.map pp_exp unrolled)))))
      | _ -> raise (Skip (sprintf "Complement of non-intersection not supported: %s" (pp_exp exp)))
      )
    | Op (_,AST.Cartesian, _) -> raise (Skip "Cartesian not implemented yet")
    | Op (_, AST.Inter, _) ->
      let unrolled = unroll_inter input_item in
      let unrolled = (match unrolled with
        | [Var (t,"po"); Var (_,"loc")] -> [Var (t,"po-loc")]
        | _ -> unrolled)
      in
      let unrolled = expand_list false unrolled in (* Making is_id false here so make_id isnt applied twice*)
      let unrolled = make_id (union_concat_map (fun a -> ir_single_inter (seq_to_inter a)) unrolled) in
      unrolled
    | Op (_, AST.Seq, expl) -> expand_list false expl (*making is_id false as we can't have an entire sequence be id*)
    | Op (_, AST.Diff, expl) -> (expand_expression is_id) (List.hd expl)
    (* here, I am assuming that the edge matched in match_var is not included in the diff*)
    | Konst (_,AST.Empty _) -> raise (Skip "Empty exp")
    | App (_, _, exp) -> (expand_expression is_id) exp
    | Var (_, var) -> make_id (ir_single_var var)
    | _ ->
        raise
          (Misc.Fatal
             (sprintf "Expression not supported: %s" (pp_exp input_item)))

  and expand_list is_id input_item : ir =
    (* apply a fold_cross to a list of AST expressions, preserving their order and calling expand_expression on each expression
       Inputs:
       - input_item, list of AST expressions. represents a sequence or intersection of variables
       Output:
       - A list of var intersection lists, each var intersection list represents either a sequence of intersections or a single intersection
    *)
    match input_item with
    | [] -> Union [ Sequence [] ]
    | hd :: tl ->
        let expanded_hd = expand_expression is_id hd in
        let expanded_tl = expand_list is_id tl in
        union_concat_map
          (fun hd_item ->
            union_map
              (fun tl_item -> concatenate_sequences hd_item tl_item)
              expanded_tl)
          expanded_hd

  and expand is_id exp : ir =
    (* construct a list of sequences using the expanded output from expand_list
       Inputs:
       - exp AST expression to be expanded
       Output:
       - list of sequences
    *)
    try
      match exp with
      | Op (_, AST.Union, expl) -> union_concat_map (expand is_id) (Union expl)
      | Op (_, AST.Inter, expl) -> expand_list is_id expl
      | Op (_, AST.Seq, expl) -> expand_list is_id expl
      | Op1 (_, ToId, expl) -> expand true expl
      | Op (_, AST.Diff, expl) -> expand is_id (List.hd expl)
      | Op (_, AST.Cartesian, _) -> raise (Skip "Cartesian not yet implemented")
      | Konst (_,AST.Empty _) -> raise (Skip "Empty exp")
      | Var (_, var) -> (make_id is_id) (ir_single_var var)
      | _ ->
          raise
            (Misc.Fatal (sprintf "Expression not supported: %s" (pp_exp exp)))
    with
    | Skip s ->
      if O.verbose > 1 then eprintf "%s\n" s;
      Union []
    | NotImplemented s | Misc.Fatal s ->
        raise (Misc.Fatal ("Fail in expand:" ^ s))

  (*
    ------------------------------------------
      match cumulative edges
    ------------------------------------------   
    *)
  let is_po e = match e with
  | Tedge (Po (_), _ ) -> true
  | _ -> false
  let merge_pos (Sequence expl : edge_ir sequence) =
    let rec f e = 
      match e with
      | [] -> []
      | h :: [] -> [h]
      | h :: a :: t when ((can_merge_po h) && (is_po a)) ->  f (h::t)
      | a :: h :: t when ((can_merge_po h) && (is_po a)) ->  f (h::t)
      | h :: t -> h :: f t
    in
    Sequence (f expl)
  
  let merge_cmo (Sequence expl : edge_ir sequence) =
    let rec f e = 
      match e with
      | [] -> []
      | h :: [] -> [h]
      | DC _ :: After (DC (a,b),v) :: t
      | After (DC (a,b),v) :: DC _ :: t -> f (After (DC (a,b),v)::t)
      | IC _ :: After (IC (a,b),v) :: t
      | After (IC (a,b),v) :: IC _ :: t-> f (After (IC (a,b),v)::t)
      | h :: t -> h :: f t
    in
    Sequence (f expl)
  let expand_fencedp el =
    (* expand cumulative macros
       Inputs:
       - el: list of edges to expand
       Outputs:
       - expanded list of edges
    *)
    (* TODO: update this function to better merge po's and maintain information for subsequent edge*)
    let open E in
    let rec f e l =
      match e with
      | [] -> l
      | h :: [] -> h :: l
      | h :: h2 :: t -> (
          match [ h.E.edge; h2.E.edge ] with
          | [ Dp (dp, sd, _); _ ] -> (
              match h,h2 with
              |
               { E.edge = Dp ((A.D.CTRLISYNC, _), _, _); E.a1; E.a2 = _ },
               { E.edge = Po (_, _, e2); E.a1 = _; E.a2 }
              ->
                  { E.edge = Dp (dp, sd, e2); E.a1; E.a2 } :: f t l
              |
               { E.edge = Dp ((A.D.CTRL, cs), _, e2); a1; a2 = _ },
               { E.edge = Insert (A.Barrier A.ISB); a1 = _; a2 }->
                  f
                    ({ E.edge = Dp ((A.D.CTRLISYNC, cs), sd, e2); a1; a2 }
                    :: t)
                    l
              | _ -> h :: f (h2 :: t) l)
          | [Insert (Barrier ISB); Insert (Barrier ISB) ] -> f (h::t) l (*Remove repeated ISBs*)
          | [Fenced ((Barrier ISB), sd, extr1, _);Fenced ((Barrier ISB), _, _, extr2)] -> 
            f ({E.edge = Fenced ((Barrier ISB), sd, extr1, extr2); a1 = h.E.a1; a2 = h2.E.a2} :: t) l
          | [ Fenced (fence, sd, extr1, _); _ ] -> (
              match [ h; h2 ] with
              | [
               { E.edge = Fenced _; a1; a2 = _ };
               { E.edge = Po (_, _, e2); a1 = _; a2 };
              ] ->
                  { E.edge = Fenced (fence, sd, extr1, e2); a1; a2 } :: f t l
              | _ -> h :: f (h2 :: t) l)
          | [ Po _; Fenced (fence, sd, _, extr2) ] -> (
              match h,h2,t with
              |
               { E.edge = Po (_, e1, _); a1; a2 = _ },
               { E.edge = Fenced _; a1 = _; a2 = _ },
               { E.edge = Po (_, _, e2); a1 = _; a2 }::_
               ->
                  { E.edge = Fenced (fence, sd, e1, e2); a1; a2 }
                  :: f (List.tl t) l
              |
               { E.edge = Po (_, e1, _); a1; a2 = _ },
               { E.edge = Fenced _; a1 = _; a2 },_ ->
                  { E.edge = Fenced (fence, sd, e1, extr2); a1; a2 } :: f t l
              | _ -> h :: f (h2 :: t) l)
          | [ Po _; Rmw _ ] -> (
              match [ h; h2 ] with
              | [
               { E.edge = Po (sd, _, _); a1; a2 = poa2 };
               { E.edge = Rmw a; a1 = _; a2 };
              ] ->
                  if A.applies_atom_rmw a poa2 a2 then
                    { E.edge = Po (sd, Irr, Irr); a1; a2 = None }
                    :: { E.edge = Rmw a; a1 = poa2; a2 }
                    :: f t l
                  else raise (Skip "atoms not applied correctly for rmw")
              | _ -> h :: f (h2 :: t) l)
          | _ -> h :: f (h2 :: t) l)
    in
    f el []

  (*
    ------------------------------------------
      logic for matching
      - parse letlines into edge type
      - match sequences of edge's into single or composite relaxations
    ------------------------------------------   
    *)
  let listofnode nodel =
    (*translate edge_node list into edge list list for printing
      *)
    let rec f node =
      match node.branch_list with
      | h :: t ->
          Misc.concat_map
            (fun a -> List.map (fun b -> node.node_val :: b) (f a))
            (h :: t)
      | [] -> [ [ node.node_val ] ]
    in
    Misc.concat_map f nodel

  let check_prev a =
    match a with
    | Empty _ -> None
    | D_atom (_, an, _) -> an
    | _ -> None

  let check_if_dir a =
    match a with
    | D_atom (Dir r, _, _) -> Dir r
    | _ -> Irr

  let extract_diratom prev next =
    let extr1 = check_if_dir prev in
    let extr2 = check_if_dir next in

    let a1 = check_prev prev in
    let a2 = check_prev next in
  
    let extr1 = check_dirfroma a1 extr1 in
    let extr2 = check_dirfroma a2 extr2 in
    (a1, a2, extr1, extr2)
  let insert_of_edgeir e = match e with
    | Tedge (Insert a, _) ->  a
    | DC (Some Next,_) ->  (A.CMO (DC_CVAU,Next))
    | DC (Some Prev,_) ->  (A.CMO (DC_CVAU,Prev))
    | IC (Some Next,_) ->  (A.CMO (IC_IVAU,Next))
    | IC (Some Prev,_) ->  (A.CMO (IC_IVAU,Prev))
    | _ -> raise (Misc.Fatal "insert of edgeir called without insert")
  let matcher (Sequence expl : edge_ir sequence) =
    (* translate expressions to E.edge type. Expand edges to include Same and Diff
        Inputs:
        - expl: sequence of expressions to create edge / edge list for
        Outputs:
        - tree of edge nodes, representing all possible edge sequences*)
    let open AST in
    let open E in
    let match_edge h prev h2 branch_list =
      let a1, a2, extr1, extr2 = extract_diratom prev h2 in
      let e =
        match h with
        | Tedge (Po (Diff, _, _), _) ->
            [ E.Po (Same, extr1, extr2); E.Po (Diff, extr1, extr2) ]
        | Tedge (Po (Same, _, _), _) ->
          
          [ E.Po (Same, extr1, extr2) ]
        | Tedge (Dp (dp, _, _), _) ->
            [ E.Dp (dp, Same, extr2); E.Dp (dp, Diff, extr2) ]
        | Tedge (Fenced (fence, _, _, _), _) -> begin
          (* D_atom and exp are here because we want fences to be a relaxation only when precedded by a memory access, otherwise it needs to be a pseudoedge *)
          match prev with
          | D_atom _ | Exp _ | First _->
            [
              E.Fenced (fence, Same, extr1, extr2);
              E.Fenced (fence, Diff, extr1, extr2);
            ]
          | _ -> [E.Insert (fence)] end
        | Tedge (Rf ie, _) -> [ E.Rf ie ]
        | Tedge (Fr ie, _) -> [ E.Fr ie ]
        | Tedge (Ws ie, _) -> [ E.Ws ie ]
        
        | Ws_single _ -> [E.Ws Ext] 
        | Fr_single _ -> [E.Fr Ext] 
        | Rf_single _ -> [E.Rf Ext] 
        | Tedge (Insert _,_)
        | DC _ 
        | IC _ -> begin 
          let fence = insert_of_edgeir h in
          match prev with
          | First _->
            [
              E.Fenced (fence, Same, extr1, extr2);
              E.Fenced (fence, Diff, extr1, extr2);
            ]
          | _ -> [E.Insert (fence)] end
        | Tedge (Rmw rmw, _) ->
            if A.applies_atom_rmw rmw a1 a2 then [ Rmw rmw ] else raise (Skip "cannot generate relaxation with mismatching annotations")
        | Ignore -> []
        | _ -> []
      in
      match e with
      | [] -> branch_list None
      | _ ->
          List.map
            (fun a ->
              let a1, a2 = match a with | E.Insert _ -> None,None | _ -> a1, a2 in
              E.
                {
                  node_val = { edge = a; a1; a2 };
                  branch_list = branch_list a2;
                })
            e
    in
    let rec f (l : edge_ir list) prev =
      match l with
      | Empty v :: _ ->
          raise
            (NotImplemented
               (sprintf "variable not implemented in matcher: %s" v))
      | Ignore :: t -> f t prev
      | h :: h2 :: t ->
          let branch_list a2 = if ((a2 = None) || (a2 = Some (A.Instr, None))) then f (h2 :: t) h else f t h in
          match_edge h prev h2 branch_list
      | h :: [] -> match_edge h prev empty_expr (fun _ -> [])
      | [] -> []
    in
    f expl first_expr
  
  let solve_instr el = 
    List.map ( fun seq ->
      let rec f l =
        match l with
        | [] -> None
        | h::[] -> 
          if E.is_non_pseudo h.E.edge then
            h.E.a1
          else None
        | h::t -> 
          if E.is_non_pseudo h.E.edge then
            h.E.a1
          else f t in
      let rec start l = match l with
        | [] -> []
        | h::[] -> [h]
        | h::t ->
          if E.is_non_pseudo h.E.edge then
            let n = f t in
            {h with E.a2 = n} :: start t

          else h :: start t in
      start seq
    ) el
  let remove_isb_doubles el =
    (* TODO: clean function *)
    List.map (fun seq ->
      let rec f l = match l with
        | [] -> []
        | h::[] -> [h]
        | h::t -> 
          if not (E.is_non_pseudo h.E.edge) then
            let rec ff ll = match ll with
            | [] -> false
            | hh::[] -> if hh = h then true else false
            | hh::tt -> if hh.E.edge = h.E.edge && (match hh.E.edge with E.Insert (Barrier ISB) -> true | _ ->false) then true else ff tt in
            if ff t then f t else h :: f t
          else h :: f t in
      f seq
          
      ) el
  (*
    ------------------------------------------
    entry point for constructing tree
    ------------------------------------------   
    *)
  let ast_to_ir ast : let_statements =
    let map_ast f l =
      List.fold_left
        (fun acc a ->
          try f a :: acc
          with
          | NotImplemented s ->
            if O.verbose > 0 then eprintf "%s\n" s;
            acc
          | Skip s ->
            if O.verbose > 1 then eprintf "%s\n" s;
            acc)
        [] l
    in
    let tree_base = map_ast get_ins ast in
    let map_vars =
      map_ast
        (if O.print_tree then fun (varname, expression) ->
           (varname, get_option (inline_vars [ varname ] tree_base expression))
         else fun (varname, expression) ->
          ( varname,
                 get_option (inline_vars [ varname ] tree_base expression) )
          (* NOTE: this code is in case we want to restrict the compiled let statements. Currently this is not desired as the whole tree is necessary to construct composite relaxations for DC/IC after *)
           (* match
             List.find_opt
               (fun name -> String.equal name varname)
               O.lets_to_print
           with
           | Some _ ->
               ( varname,
                 get_option (inline_vars [ varname ] tree_base expression) )
           | None ->
               raise (Skip "") *)
        )
        tree_base
    in
    apply_expand map_vars

  (*
      ---------------------------------------------------------------
      Printing functions
      ---------------------------------------------------------------
      *)
  let is_insert e = 
    match e with
    | DC _ | IC _ | Tedge (E.Insert _,_) -> true
    | _ -> false
  let pp_relaxations (tree : let_statements) =
    (* Pretty prints the given list of let statements as DIY relaxations
       Inputs:
         - tree: the list of let statements to pretty print, of type ast
       Outputs: None
       Side effects: Prints the DIY relaxation to the console
    *)
    let tree = var_to_edge tree in
    let f expl =
      try
        let expl = merge_pos expl in
        let expl = merge_cmo expl in
        let Sequence a = expl in
        let afters = if 
          List.length (List.find_all (fun a -> match a with After _ -> true | _ -> false) a) = 2 
        then begin
          (* NOTE: currently, this block of code calculates the composite relaxations for when there are two 'After' in a cat sequence
          this code should be adjustable to work with different configurations too *)
          let left = List.hd a in

          let body = 
            let rec f aa = match aa with
            | [] -> []
            | _::[] -> []
            | h::t -> if h = List.hd a then f t else h::(f t) in
            f a in
          let right = List.hd (List.rev a) in
          let lets_afters p = List.find_all 
          (* Note: this function identifies the sequences we want to use to expand the after edges *)
            (fun (Sequence b) -> 
              List.for_all (fun x -> match x with
              | After _ -> false
              | _ -> true) b &&
              List.exists
              (* TODO: currently this list checks all sequences for edges/events etc that are relevant for Ifetch. For other features, i.e. TLBI, this list needs to be expanded*)
                (fun x -> match x with
                | D_atom (_, Some (Instr,None),_)
                | Tedge (Insert (Barrier ISB), _)
                | DC _
                | IC _ -> true
                | _ -> false) b) 
            p in
          let lets_afters = 
            lets_afters (List.concat_map 
              (fun b ->
                let _, p = b in
                let (Union p) = p in
                p
              ) tree) in
          let po_end_nonpo_tedges =
            (* TODO: improve function and make more understandable *)
            (* go over sequences and identify ones that end in po and their first tedge is not po or fenced *)
            List.filter (fun (Sequence s) ->
              try
              match List.hd (List.rev s), List.find (fun a -> match a with Tedge _ -> true | _ -> false) s with 
              | Tedge (E.Po _,_),a when (match a with Tedge (E.Po _,_) | Tedge (E.Fenced _,_) -> false | _ -> true)  -> true
              | _ -> false
              with Not_found -> false
            ) lets_afters in
          let check_dir a b =
            match a, b with
            | Irr, Irr
            | (Dir R), (Dir R)
            | (Dir W), (Dir W) -> 0
            | (Dir R), Irr
            | (Dir W), Irr -> -1
            | Irr, (Dir R)
            | Irr, (Dir W) -> 1
            | _,_ -> 2 in
          let create_afters lefts rights =
            let possible_afters start contains rev =
              (* NOTE: this function filters the sequences that might be interesting to extend the afters and identifies all that are possible *)
              List.filter_map 
                (fun (Sequence s) ->
                  let contains_check = List.exists (fun a -> match contains, a with After (DC _,_), DC _ | After (IC _,_), IC _ -> true | _ -> false) s in
                  let s = if rev then List.rev s else s in
                  let check_CMO = match start, List.hd s with
                  | DC _, DC _ | IC _, IC _ -> true
                  | _ -> false in
                  if check_CMO &&  contains_check then Some s
                  else
                    match start, List.hd s with
                    | D_atom (da,atoma,_), D_atom (db,atomb,_ )-> begin
                      match check_dir da db with
                      | 1 | 0 when contains_check && atoma == atomb-> Some s 
                      | -1 when contains_check && atoma == atomb -> Some (start :: (List.tl s))
                      | _ -> None 
                    end
                    | _,Tedge (Po _,_) when contains_check -> Some (start :: s)
                    | _ -> None
                  )
                lets_afters in
            let pass_one_right l contains rev =
              let (Sequence l) = l in
              let l = if rev then List.rev l else l in
              let l_start = List.rev (List.tl (List.rev l)) in
              let l_end = List.hd (List.rev l) in
              let p_rights = possible_afters l_end contains rev in
              let concat_lists h = 
                if rev then 
                  let h = List.fold_left (fun acc e -> match e with DC (_,v) -> DC (Some Next,v)::acc| IC (_,v) -> IC (Some Next,v)::acc| _ -> e::acc) [] h in
                  List.rev (l_start @ (List.rev h))
                else 
                  let h = List.fold_left (fun acc e -> match e with DC (_,v) -> DC (Some Prev,v)::acc| IC (_,v) -> IC (Some Prev,v)::acc| _ -> e::acc) [] h in
                  l_start @ (List.rev h) in
              let rec f a = match a with
              | [] -> []
              | h::[] -> [Sequence (concat_lists h)]
              | h::t -> Sequence (concat_lists h) :: f t in
              f p_rights in
            let iter_afters afters sequences rev =
              let rec ff rs ll =
                let rec f l r = match l with
                | [] -> []
                | h::[] -> pass_one_right h r rev
                | h::t -> (pass_one_right h r rev) @ f t r in
                match rs with
                | [] -> ll
                | h::[] -> f ll h
                | h::t -> ff t (f ll h) in
              ff afters sequences in
            let right_solved = iter_afters rights [(Sequence body)] false in
            let full_solved = iter_afters lefts right_solved true in
            let append_starts seq=
              let Sequence seq = seq in
              let new_l = if is_insert (List.hd seq) then 
                List.fold_left (fun acc (Sequence s) -> (Sequence (s@seq))::acc) [] ((Sequence [])::po_end_nonpo_tedges)
              else [Sequence seq] in
              new_l in
            let full_solved = Misc.concat_map append_starts full_solved in
            full_solved
            in
          let afters = (create_afters [] [left; right]) @ (create_afters [left] [right]) @ (create_afters [left; right] []) in
          afters
        end else [] in

        (* Note: add the sequences compiled for afters to the rest *)
        let afters = List.map merge_pos afters in
        let after_nodes = Misc.concat_map matcher afters in

        (* Note: format sequences and match to edge type to take advantage of pp_edge *)
        let edge = matcher expl in
        let edge = listofnode (edge @ after_nodes) in
        let edge = solve_instr edge in
        let edge = remove_isb_doubles edge in
        let edge = List.map expand_fencedp edge in
        List.map
          (fun e ->
            match e with
            | _ :: [] -> sprintf "%s" (String.concat "," (List.map E.pp_edge e))
            | _ :: _ ->
                sprintf "[%s]" (String.concat "," (List.map E.pp_edge e))
            | [] -> raise (Misc.Fatal "Cannot have empty edge"))
          edge
      with Skip msg -> if O.verbose > 1 then eprintf "%s\n" msg; []
    in
    try
      List.iter
        (fun b ->
          let _, p = List.find (fun (name, _) -> String.equal name b) tree in
          let (Union p) = p in
          let s_relax = List.concat_map f p in
          let cons_uniq xs x = if List.mem x xs then xs else x :: xs in (* remove duplicate relaxations*)
          let remove_duplicates xs = List.rev (List.fold_left cons_uniq [] xs) in
          List.iter (Printf.printf "%s ") (remove_duplicates s_relax)
          )
        O.lets_to_print
    with Not_found ->
      raise
        (Misc.Fatal "let statements that were asked for are not in cat file \n")


  (*
----------------------------------------------
entry point
----------------------------------------------   

*)
  let rec get_includes ins =
    let open AST in
    match ins with
    | Include (_,fname) ->
      let _, (_,_,ast) = Parser.find_parse fname in
      let ast = (List.concat_map get_includes ast)@ast in
      ast
    | _ -> []

  let get_imports ast = List.concat_map get_includes ast
  let zyva name =
    try
      let _, (_,_,ast) = Parser.find_parse name in
      let tree = ast@(get_imports ast) in
      let tree = ast_to_ir tree in
      let tree = solve_id tree in
      if O.print_tree then (
        pp_tree tree;
        printf "\n\n\n");
      pp_relaxations tree
    with
    | NotImplemented msg -> printf "\n\nNot Implemented Error: %s\n%!" msg
    | Misc.Fatal msg -> printf "\n\nFatal Error: %s\n%!" msg
    | Misc.Exit -> ()
end

let verbose = ref 0
let lets_to_print = ref []
let conds = ref []
let unroll = ref 1
let arg = ref []
let setarg name = arg := !arg @ [ name ]
let print_tree = ref false

let opts =
  [
    ("-v", Arg.Unit (fun () -> incr verbose), " be verbose");
    ( "-let",
      Arg.String (fun s -> lets_to_print := !lets_to_print @ [ s ]),
      "<statement> print out selected let statements" );
    ("-conds", Arg.String (fun s -> conds := !conds @ [ s ]),
      "<cond> choose what variant conditions to set");
    ("-unroll", Arg.Int (fun i -> unroll := i),
    "<unroll> choose how many times transitive and reflexive and transitive operators unroll. Default = 1");
    ("-tree", Arg.Bool (fun s -> print_tree := s),
    "<true|false> print out expanded cat file")
  ]

let () = Arg.parse opts setarg (sprintf "Usage: %s [options]* cats*" prog)

module Z = Make (struct
  let verbose = !verbose
  let lets_to_print = !lets_to_print
  let conds = !conds
  let unroll = !unroll
  let print_tree = !print_tree
end)

let () =
  List.iter Z.zyva !arg;
  exit 0
