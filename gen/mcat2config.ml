[@@@warning "-40-42"]
(* Create a set of relaxations for diy using a cat file *)

open Printf

let prog =
  if Array.length Sys.argv > 0 then
    Filename.basename Sys.argv.(0)
  else "cat2config7"

module Make
    (O:sig
      val verbose : int
      val lets_to_print : string list
      val print_tree : bool
    end) =
  struct
    module ML =
      MyLib.Make
        (struct
          let includes = []
          let env =  Some "HERDLIB"
          let libdir = Filename.concat Version.libdir "herd"
          let debug = O.verbose > 0
        end)

    module ParserConfig = struct
      let debug = O.verbose > 1
      let libfind = ML.find
    end
    module A =
      AArch64Arch_gen.Make
        (struct
          include AArch64Arch_gen.Config
          let moreedges = !Config.moreedges
        end)
    module E = Edge.Make(Edge.Config)(A:Fence.S)
    (* 
       ---------------------------------------------------------------
       Custom types
       ---------------------------------------------------------------
       
    *)
    open Code
    
    exception NotImplemented of string

    type extr = Code.extr 
    
    type atom = A.atom
  
    type edge = E.edge
    
    type edge_node = {node_val: edge; branch_list: edge_node list}
    
    module Parser = ParseModel.Make(ParserConfig)

    type 'a union = Union of 'a list
    type 'a sequence = Sequence of 'a list
    type 'a intersection =  Intersection of 'a list
    type var = string
      (*
       ir is used to represent the let statements
       each let statement consists of a union of sequences. Each sequence is a list of intersections
       where single variables are represented as intersections of length 1
      *)
    type ir = var intersection sequence union
      (* let_statements is the list of all let statements in an AST*)
    type let_statements = (string * ir) list

    type edge_ir =
    | Tedge of E.tedge * string
    | D of dir * string
    | Atom of atom * string
    | Empty of string
    | Imp of string
    | PTE of string
    | D_atom of extr*(atom option) * string
    | Ext of extr * string
    | Lrs of string
    let empty_expr = Empty ""

    let make_sequence expl =
      Sequence expl
    let make_intersection exp =
      Intersection [exp]

       (* 
       ---------------------------------------------------------------
       Printing functions
       --------------------------------------------------------------- 
       *)
    let rec pp_relaxations (tree : let_statements) =
      (* Pretty prints the given list of let statements as DIY relaxations
        Inputs:
          - tree: the list of let statements to pretty print, of type ast
        Outputs: None
        Side effects: Prints the DIY relaxation to the console
      *)
      let tree = var_to_edge tree in
      let f expl =
        try
          let edge = matcher expl in
          let edge = listofnode edge in
          let edge = List.map expand_fencedp edge in
          List.iter (fun e ->
            match e with
            | _::[] -> printf "%s " (String.concat "," (List.map E.pp_edge e))
            | _::_ -> printf "[%s] " (String.concat "," (List.map E.pp_edge e))
            | [] -> raise (Misc.Fatal "Cannot have empty edge")
            ) edge;
        with
          | NotImplemented msg -> if O.verbose > 0 then eprintf "%s\n" msg;
      in
      try
      List.iter (fun b -> 
        let (_,p) = (List.find (fun (name,_) -> String.equal name b) tree) in
        let Union p = p in
        List.iter f p
        ) O.lets_to_print
      with
      | Not_found -> raise (Misc.Fatal "let statements that were asked for are not in cat file");

    and pp_tree (tree: let_statements) : unit =
      (* This function takes a list of let statements, in the form
      of string * expression, and prints the expanded tree of the cat file.
      Inputs:
        - tree: A list of let statements, as ast type.
      Outputs: 
        - None
      Side effects: The function prints the expanded tree of the cat file.
    *)
      let pp_intersection (Intersection exp) =
        match exp with
        | h:: [] -> sprintf "%s" h;
        | h::t -> sprintf "(%s)" (String.concat "&" (h::t));
        | [] -> raise (Misc.Fatal "Intersection cannot have an empty list") in
      let pp_sequence (Sequence expl) =
        sprintf "%s" (String.concat ";" (List.map (fun a -> pp_intersection a) expl)); in
      let pp (name, Union ins) =
        printf "\n\n(%s)\n  |" name;
        printf "%s" (String.concat "\n  |" (List.map (fun a -> pp_sequence a) ins)); in
      List.iter pp tree
      (*
    ------------------------------------------
      Parse AST and make into custom type
    ------------------------------------------   
    *)
    and get_ins ins =
      (* Returns the name and expression of the given AST instruction
      Inputs:
        - ins: the AST instruction to extract the name and expression from
      Outputs:
        - varname: the name of the instruction, as AST.pat
        - expression: the expression of the instruction, as AST.exp
      Side effects: None
      *)
      let open AST in
      match ins with
      | Rec (_,(_,Pvar (Some varname),expression) :: _,_)
      | Let (_, (_,Pvar (Some varname),expression) :: _) -> varname,expression
      | _ -> raise (NotImplemented "instruction not supported" )

    and inline_vars varname tree expression =
      (*identify any vars that are defined as let statements and implement them into AST*)
      let open AST in
      match expression with
      | Op (a, op, expl) -> begin
        (Op (a, op, List.map (inline_vars varname tree) expl))
      end
      | Op1 (a, op, exp) -> Op1 (a, op, inline_vars varname tree exp)
      | Var (a, var) -> begin
        (* checking a variable against the name of its let statement allows
           for the removal of recursive definitions*)
        if (String.equal var varname) then expression
        else
        let br = expand_var tree (a,var) in
        match br with
          | Var (_,_) -> br
          | Op (_, _, _) -> begin
            let a = inline_vars varname tree br in
            a
          end
          | _ -> expression
      end
      | App (_,_,exp2) -> inline_vars varname tree exp2 (*at the moment, only dealing with exp1 = range, where we can ignore it*)
      | _ -> raise (Misc.Fatal (sprintf "%s: expression not supported" (pp_exp expression)) )

    and expand_var tree (a,var) =
    (* compare a single AST variable and compare to all let statements. If a match is found, return that expression*)
      let open AST in
      let is_var (name, _) = String.equal name var in
      match List.find_opt is_var tree with
      | Some (_,exp) -> exp
      | None -> Var (a,var)

    and var_to_edge tree =
      (* translate variables in let statements to edge_ir type
          Inputs:
            - tree: list of let statemenets
          Outputs: list of let statements with var replaced by edge_ir in the ir type
        *)
        let edges_of_sequence expl =
          List.concat_map
          (fun (Sequence seq) ->
            try
              List.map make_sequence (fold_cross seq apply_match_var)
            with
              | NotImplemented msg -> if O.verbose > 0 then eprintf "%s" msg; []
          )
          expl in
        let edges_of_union let_name =
          let (name,p) = (List.find (fun (name,_) -> String.equal name let_name) tree) in
          let Union p = p in
          name,Union (edges_of_sequence p) in
        try
          List.map edges_of_union O.lets_to_print
        with
        | Not_found -> raise (Misc.Fatal (sprintf "let statements that were asked for are not in cat file"))
  
    and apply_match_var a : edge_ir list =
      match a with
      | Intersection (h::[]) -> begin (* single variables *)
        match_var h
      end
      | Intersection (h::t) -> begin (* Intersections *)
        let inter_list = List.map make_intersection (h::t) in
        let new_inters = fold_cross inter_list apply_match_var in
        List.map match_inter new_inters
      end
      | _ -> raise (Misc.Fatal "cannot have empty intersection")
      (*
       ---------------------------------------------------------------
       Expand sequences and replace non-primitive variables
       --------------------------------------------------------------- 
*)


    and apply_expand tree : let_statements =
      (* Expand operations in AST and translate AST to internal representation
      Input:
      - tree : (var * AST.exp) list
      Output:
      - let_statements : (var * ir) list
       *)
      let f (name, instr) =
        match instr with
        | AST.Op (_,AST.Seq,_) | AST.Op (_,AST.Inter,_) ->
          name, Union (expand instr)
        | AST.Op (_,AST.Union,expl) ->
          name, Union (List.concat_map expand expl)
        | _ -> raise (Misc.Fatal (sprintf "Expression not supported: %s" (pp_exp instr)))
        in
      List.map f tree
    and expand_expression input_item : var intersection list list=
      (* Expand a chosen expression, unrolling intersections, expanding unions and extracting variables from operations
        Inputs:
        - input_item: expression to expand (AST.exp)
        Output: 
        - list of var intersection lists. each var intersection list represents a sequence
        *)
      let open AST in
      match input_item with
      | Op (_,AST.Union, expl) -> List.concat_map expand_expression expl  (* union of variables *)
      | Op1 (_,AST.ToId, expl) -> expand_expression expl
      | Op (_,AST.Inter, _) -> [[unroll_inter input_item]]
      | Op (_,AST.Seq, expl) -> expand_list expl
      | Op (_,AST.Diff, expl) -> expand_expression (List.hd expl)
      (* here, I am assuming that the edge matched in match_var is not included in the diff*)
      | App (_,_,exp) -> expand_expression exp
      (* applied functions are currently not supported, here I am assuming that they can be interpreted as the variable the function is being applied to *)
      | Var (_,var) -> [[Intersection [var]]]
      | _ -> raise (Misc.Fatal (sprintf "Expression not supported: %s" (pp_exp input_item)))

    and expand_list input_item : var intersection list list=
      (* apply a fold_cross to a list of AST expressions, preserving their order and calling expand_expression on each expression
      Inputs: 
      - input_item, list of AST expressions. represents a sequence or intersection of variables
      Output: 
      - A list of var intersection lists, each var intersection list represents either a sequence of intersections or a single intersection
      *)
      match input_item with
      | [] -> [[]]
      | hd::tl ->
          let expanded_hd = expand_expression hd in
          let expanded_tl = expand_list tl in
          List.concat_map (fun hd_item ->
            List.map (fun tl_item ->
              hd_item @ tl_item
            ) expanded_tl
          ) expanded_hd

    and expand exp : 'a sequence list=
      (* construct a list of sequences using the expanded output from expand_list
      Inputs: 
      - exp AST expression to be expanded
      Output: 
      - list of sequences
      *)
      let open AST in
      try
      match exp with
      | Op (_,AST.Union, expl) -> List.concat_map expand expl
      | Op (_,AST.Inter, expl) ->
        let expl = expand_list expl in
        List.map make_sequence expl
      | Op (_,AST.Seq, expl) -> List.map make_sequence (expand_list expl)
      | Op1 (_,ToId, expl) -> expand expl
      | Var (_,var) -> [Sequence [Intersection [var]]]
      | Op1 (_,a, _) -> raise (Misc.Fatal (sprintf "Expression not supported: %s " (pp_op1 a)))
      | Op (_,a,_) -> raise (Misc.Fatal (sprintf "Expression not supported: %s" (pp_op2 a)))
      | _ -> raise (Misc.Fatal (sprintf "Expression not supported: %s" (pp_exp exp)))
      with
      | NotImplemented s -> if O.verbose > 0 then eprintf "%s\n" s; []
      | Misc.Fatal s -> raise (Misc.Fatal ("Fail in expand:"^s))

    and fold_cross l f =
      match l with
        | [] -> [[]]
        | hd::tl ->
          let expanded_hd = f hd in
          let expanded_tl = fold_cross tl f in
          List.concat_map (fun hd_item ->
            List.map (fun tl_item ->
              hd_item::tl_item
            ) (expanded_tl)
          ) (expanded_hd)

    (*
    ------------------------------------------
      match cumulative edges
    ------------------------------------------   
    *)

    and expand_fencedp el =
    (* expand cumulative macros
       Inputs:
       - el: list of edges to expand
       Outputs:
       - expanded list of edges
       *)
      let open E in
      let rec f e l = 
        match e with
        | [] -> l
        | h::[] -> h::l
        | h::h2::t -> begin 
          match [h.E.edge;h2.E.edge] with
          |  [Dp (dp, sd, _);_] -> begin
            match [h;h2] with
            | [{E.edge=Dp ((A.D.CTRLISYNC,A.NoCsel),_,_); E.a1=a1; E.a2=_;};
               {E.edge=Po (_,_,e2); E.a1=_; E.a2=a2;}] ->
              {E.edge=Dp (dp,sd,e2); E.a1=a1; E.a2=a2}::(f t l)
            | [{E.edge=Dp ((A.D.CTRL,_),_,_); a1=a1; a2=_;};
               {E.edge=Fenced (A.Barrier A.ISB,_,_,e2); a1=_; a2=a2;}] ->
              f ({E.edge=Dp ((A.D.CTRLISYNC,A.NoCsel),sd,e2); a1=a1; a2=a2}::t) l
            | _ -> h::(f (h2::t) l)
            end
          | [Fenced (fence, sd, extr1, _);_ ]->begin
            match [h;h2] with
            | [{E.edge=Fenced _; a1=a1; a2=_;};
                {E.edge=Po (_,_,e2); a1=_; a2=a2;}] ->
              {E.edge=Fenced (fence,sd,extr1,e2); a1=a1; a2=a2}::(f t l)
            | _ -> h::(f (h2::t) l)
            end
          | [(Po _);(Fenced (fence, sd, _, extr2))] ->begin
            match [h;h2]@t with
            | [{E.edge=Po (_,e1,_); a1=a1; a2=_;};
               {E.edge=Fenced _; a1=_; a2=_;};
               {E.edge=Po (_,_,e2); a1=_; a2=a2;}] ->
              {E.edge=Fenced (fence,sd,e1,e2); a1=a1; a2=a2}::(f (List.tl t) l)
            | [{E.edge=Po (_,e1,_); a1=a1; a2=_;};
               {E.edge=Fenced _; a1=_; a2=a2;}] ->
              {E.edge=Fenced (fence,sd,e1,extr2); a1=a1; a2=a2}::(f t l)
            | _ -> h::(f (h2::t) l)
          end
          | [(Po _);(Rmw _)] ->begin
            match [h;h2] with
            | [{E.edge=Po (sd,_,_); a1=a1; a2=poa2;};
              {E.edge=Rmw a; a1=_; a2=a2;}] ->
                if (A.applies_atom_rmw a poa2 a2) then
                  {E.edge=Po (sd,Irr,Irr); a1=a1; a2=None;}::{E.edge=Rmw a; a1=poa2; a2=a2;}::(f t l)
                else raise (NotImplemented "atoms not applied correctly for rmw")
            | _ -> h::(f (h2::t) l)
              end
          | _ -> h::(f (h2::t) l)
        end
      in f el []
    
      (*
    ------------------------------------------
      logic for matching
      - parse letlines into edge type
      - match sequences of edge's into single or composite relaxations
    ------------------------------------------   
    *)

    and matcher (Sequence expl:edge_ir sequence) =
    (* translate expressions to E.edge type. Expand edges to include Same and Diff
        Inputs:
        - expl: sequence of expressions to create edge / edge list for
        Outputs:
        - tree of edge nodes, representing all possible edge sequences*)
      let open AST in
      let open E in
      let match_edge h prev h2 branch_list =
        let a1, a2, extr1, extr2 = extract_diratom prev h2 in
        let e = match h with
        | Tedge (Po (_,_,_),_) -> [E.Po(Same,extr1,extr2);E.Po(Diff,extr1,extr2)]
        | Tedge (Dp (dp, _, _),_) -> [E.Dp (dp,Same,extr2);E.Dp (dp,Diff,extr2)]
        | Tedge (Fenced (fence,_,_,_),_) -> [E.Fenced (fence,Same,extr1,extr2);E.Fenced (fence,Diff,extr1,extr2)]
        | Tedge ((Rf ie), _) -> [E.Rf ie]
        | Tedge ((Fr ie), _) -> [E.Fr ie]
        | Tedge ((Ws ie), _) -> [E.Ws ie]
        | Tedge (Rmw rmw,_) ->
          if (A.applies_atom_rmw rmw a1 a2) then [Rmw rmw]
          else []
        | _ -> [] in
        List.map (fun a -> E.{ node_val = { edge = a ; a1 ; a2 }; branch_list = branch_list a2 }) e in
      let rec f (l:edge_ir list) prev =
        match l with
        | Empty v :: _ -> raise (NotImplemented (sprintf "variable not implemented in matcher: %s" v))
        | (Lrs _ as h) :: h2 :: t ->
          let a1, a2, _, _ = extract_diratom prev h2 in
            [
              {
                node_val = E.{ edge = Po (Same, Dir W, Dir R); a1; a2 };
                branch_list = f (h2 :: t) h;
              };
            ]
        | h::h2::t ->
          let branch_list a2 =
            if a2 = None then f (h2 :: t) h
            else f t h
          in
          match_edge h prev h2 branch_list
        | h::[] -> match_edge h prev empty_expr (fun _ -> [])
        | [] -> []
      in f expl empty_expr
    
    and listofnode nodel = 
      (*translate edge_node list into edge list list for printing
        *)
      let rec f node =
        match node.branch_list with
        | h::t ->
          List.concat_map (fun a ->
                       List.map (fun b -> node.node_val::b)
                       (f a)
                       ) (h::t)
        | [] -> [[node.node_val]] in
      List.concat_map f nodel
    
    and check_prev a = 
      match a with
      | Empty _ -> None
      | Atom (b,_) -> Some b
      | D_atom (_,an,_) -> an
      | _ -> None
    and check_if_dir a =
      match a with
      | D (a,_) ->  Dir a
      | Ext (NoDir,_) -> NoDir
      | D_atom (r,_,_) -> r
      | _ -> Irr
    
    and match_inter expl =
      match expl with
      | h::h2::[] -> begin
        match h,h2 with
        | D (R,_), Atom ((A.Tag, None),_) -> D_atom (Dir R, Some (A.Tag, None), "tag read")
        | _ -> raise (NotImplemented "inter not implemented in matcher")
      end
      | h::h2::h3::[] -> begin
        match h,h2,h3 with
        | D (R,_), PTE _, Imp _ -> D_atom (Dir R, Some (A.Pte A.Read, None), "ImpPTE Read")
        | D (R,_), Atom ((A.Tag, None),_), Imp _->D_atom (Dir R, Some (A.Tag, None), "ImpTag Read")
        | _ -> raise (NotImplemented "inter not implemented in matcher")
      end
      | _ -> raise (NotImplemented "inter not implemented in matcher")

      and unroll_inter expr : var intersection =
        let open AST in
          let rec f l =
            match l with
            | Op (_,Inter, expl) -> List.concat_map f expl
            | Var (_,var) -> [var]
            | _ -> raise (Misc.Fatal "operation in intersection not supported") in
          match expr with
          | Op (_,Inter, exp) -> Intersection (List.concat_map f exp)
          | _ -> raise (Misc.Fatal "non intersection passed to unroll inter function")

    and extract_diratom prev next =
      let extr1 = check_if_dir prev in
      let extr2 = check_if_dir next in
      
      let a1 = check_prev prev in
      let a2 = check_prev next in
      let extr1 = check_dirfroma a1 extr1 in
      let extr2 = check_dirfroma a2 extr2 in
      a1,a2,extr1,extr2
        
    (*
    ------------------------------------------
    entry point for constructing tree
    ------------------------------------------   
    *)
    and ast_to_ir ast : let_statements =
      let map_ast f l =
        List.fold_left
        (fun acc a ->
          try
            f a::acc
          with
            | NotImplemented s -> if O.verbose > 0 then (eprintf "%s\n" s);
              acc)
        [] l in
      let tree_base = map_ast get_ins ast in
      let map_vars = (map_ast (
        if O.print_tree then
          fun (varname, expression) -> varname, inline_vars varname tree_base expression
        else
          fun (varname, expression) ->
          match List.find_opt (fun name -> String.equal name varname) O.lets_to_print with
          | Some _ -> varname, inline_vars varname tree_base expression
          | None -> raise (NotImplemented (sprintf "not generating for instruction: %s\n" varname))
        ) tree_base) in
      let tree = apply_expand map_vars in
      tree
    
    (*
    ------------------------------------------
    helper functions
    ------------------------------------------   
    *)
  and lrsc = Tedge (Rmw A.LrSc, "lrsc")

  and match_var v = 
    (* Exp, Imp and PTE have been included as placeholders. *)
    let open A in
    let open E in
      match v with
      | "R"-> [D (R, v)]
      | "W" -> [D (W, v)]
      | "M" -> [Ext (Irr, v)]
      | "L" -> [Atom ((A.Rel None,None), v)]
      | "P" -> [Atom ((A.plain,None), v)]
      | "A" -> [Atom ((A.Acq None,None), v)]
      | "Q" -> [Atom ((A.AcqPc None,None), v)]
      | "T" -> [Atom ((A.Tag, None), v)]
      | "Exp" -> [Atom ((A.Pte Read, None), v)]
      | "Imp" -> [Imp v]
      | "PTE" -> [PTE v]
      | "po" -> [Tedge ((Po (Same,Irr,Irr)), v)]
      | "lrs" -> [Lrs v ]
      | "addr" -> [Tedge ((Dp ((A.D.ADDR,A.NoCsel), Same, Irr)), v)]
      | "ctrl" -> [Tedge ((Dp ((A.D.CTRL,A.NoCsel), Same, Irr)), v)]
      | "data" -> [Tedge ((Dp ((A.D.DATA,A.NoCsel), Same, Irr)), v)]
      | "DMB.ISH" -> [Tedge ((Fenced (Barrier (DMB (ISH,FULL)) ,Same, Irr, Irr)), v)]
      | "DMB.OSH" -> [Tedge ((Fenced (Barrier (DMB (OSH,FULL)),Same, Irr, Irr)), v) ]
      | "DMB.SY" -> [Tedge ((Fenced (Barrier (DMB (SY,FULL)),Same, Irr, Irr)), v) ]
      | "DSB.ISH" -> [Tedge ((Fenced (Barrier (DSB (ISH,FULL)),Same, Irr, Irr)), v)]
      | "DSB.OSH" -> [Tedge ((Fenced (Barrier (DSB (OSH,FULL)),Same, Irr, Irr)), v) ]
      | "DSB.SY" -> [Tedge ((Fenced (Barrier (DSB (SY,FULL)),Same, Irr, Irr)), v)  ]
      | "ISB" -> [Tedge ((Fenced (Barrier (ISB),Same, Irr, Irr)), v)  ]
      | "rfi" -> [Tedge ((Rf Int), v)]
      | "lxsx" -> [lrsc]
      | "co" -> [
        Tedge ((Ws Int), v);
        Tedge ((Ws Ext), v)
      ]
      | "fr" -> [
        Tedge ((Fr Int), v);
        Tedge ((Fr Ext), v)
      ]
      | "fri" -> [Tedge ((Fr Int), v)]
      | "fre" -> [Tedge ((Fr Ext), v)]
      | "rfe" -> [Tedge ((Rf Ext), v)]
      | "coe" -> [Tedge ((Ws Ext), v)]
      | "coi" -> [Tedge ((Ws Int), v)]
      | "amo" -> [
        Tedge (Rmw A.Swp, "Amo.Swp");
        Tedge (Rmw A.Cas, "Amo.Cas");

        Tedge (Rmw (LdOp A_ADD), "Amo.LdAdd");
        Tedge (Rmw (LdOp A_EOR), "Amo.LdEor");
        Tedge (Rmw (LdOp A_SET), "Amo.LdSet");
        Tedge (Rmw (LdOp A_CLR), "Amo.LdClr");

        Tedge (Rmw (StOp A_ADD), "Amo.StAdd");
        Tedge (Rmw (StOp A_EOR), "Amo.StEor");
        Tedge (Rmw (StOp A_SET), "Amo.StSet");
        Tedge (Rmw (StOp A_CLR), "Amo.StClr")
      ]
      | _ -> [Empty v]

    and check_dirfroma letl a = 
      match letl with
      | Some (A.Acq None,None) | Some (A.AcqPc None,None) -> Dir R
      | Some (A.Rel None,None) -> Dir W
      | _ -> a

    and pp_exp a = 
      let open AST in 
      match a with 
      | Konst _ -> "Konst"
      | Tag _ -> "Tag"
      | Var _ -> "Var"
      | Op1 _ -> "Op1"
      | Op _ -> "Op"
      | App _ -> "App"
      | Bind _ -> "Bind"
      | BindRec _  -> "BindRec"
      | Fun _ -> "Fun"
      | ExplicitSet _ -> "ExplicitSet"
      | Match _ -> "Match"
      | MatchSet _ -> "MatchSet"
      | Try _ -> "Try"
      | If _ -> "If"
    
    and pp_op2 a = 
      let open AST in
      match a with
      | Union -> "Union"
      | Inter -> "Inter"
      | Diff -> "Diff"
      | Seq -> "Seq"
      | Cartesian -> "Cartesian"
      | Add -> "Add"
      | Tuple -> "Tuple"

    and pp_op1 a = 
      let open AST in 
      match a with
      | Plus -> "Plus"
      | Star -> "Star"
      | Opt -> "Opt"
      | Comp -> "Comp"
      | Inv -> "Inv"
      | ToId -> "ToId"

    and pp_edge_ir a =
      match a with
      | Tedge _ -> "tedge"
      | D _ -> "D"
      | Atom _ -> "Atom"
      | Empty _ -> "Empty"
      | Imp _ -> "Imp"
      | PTE _ -> "PTE"
      | D_atom _ -> "D_atom"
      | Ext _ -> "ext"
      | Lrs _ -> "lrs"
(*
----------------------------------------------
entry point
----------------------------------------------   

*)
    let zyva name =
      try
        let _,(_,_,ast)  = Parser.find_parse name in
        let tree = ast_to_ir ast in
        if O.print_tree then begin
          pp_tree tree;
          printf "\n\n\n"
        end;
        printf "\n\n\n";
        pp_relaxations tree;
        printf "\n";
        ()
      with
      | Misc.Fatal msg -> printf "ERROR %s\n%!" msg
      | Misc.Exit -> ()
  end

let verbose = ref 0
let lets_to_print = ref []
let arg = ref []
let print_tree = ref false
let setarg name = arg := !arg @ [name]

let opts =
  [
   "-v",Arg.Unit (fun () -> incr verbose), " be verbose";
   "-let",Arg.String (fun s -> lets_to_print := !lets_to_print @ [s]),
   "<statement> print out selected let statements";
   "-tree",Arg.Unit (fun () -> print_tree := true), "print expanded cat file";
  ]

let () =
  Arg.parse opts setarg
    (sprintf "Usage: %s [options]* cats*" prog)


module Z =
  Make
    (struct
      let verbose = !verbose
      let lets_to_print = !lets_to_print
      let print_tree = !print_tree
    end)

let () = List.iter Z.zyva !arg ; exit 0
