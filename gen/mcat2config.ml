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
    
    exception Failed of string
    exception Including of AST.ins list

    type extr = Code.extr 
    
    type atom = A.atom
  
    type edge = E.edge
    
    type edge_node = {node_val: edge; branch_list: edge_node list}
    
    module Parser = ParseModel.Make(ParserConfig)
      
    type expr = 
      | Op2 of AST.op2 * (expr list)
      | Op1 of AST.op1 * expr
      | App of expr * expr
      | Empty of string
      | Var of string
      | T of E.tedge * string
      | D of dir * string
      | I of ie * string
      | At of A.rmw * string
      | An of atom * string
      | Ext of extr * string
      | Imp of string
      | PTE of string
      | Dd of extr*(atom option) * string
      | Lrs of string

    type tree = (string * expr) list
    let empty_expr = Empty ""


       (* 
       ---------------------------------------------------------------
       Printing functions
       --------------------------------------------------------------- 
       *)

    let rec pp_relaxations tree =
      (* Pretty prints the given list of let statements as DIY relaxations
        Inputs:
          - tree: the list of let statements to pretty print, of type ast
        Outputs: None
        Side effects: Prints the DIY relaxation to the console
      *)
      let open AST in
      let rec f a = 
        match a with
        | Op2 (Seq, expl)-> begin
          try 
            let edge = matcher expl in
            let edge = listofnode edge in
            let edge = List.map expand_fencedp edge in
            List.iter (fun e -> 
              match e with
              | _::[] -> printf "%s " (String.concat "," (List.map E.pp_edge e))
              | _::_ -> printf "[%s] " (String.concat "," (List.map E.pp_edge e))
              | _ -> if O.verbose > 0 then eprintf "matching logic output invalid"
              ) edge;
          with 
          | Failed a-> begin 
            if O.verbose > 0 then eprintf "An unsupported item was passed to matcher function: %s\n" a;
          end
          | _ -> ();
        end
        | Var _ | T (_, _) -> begin
          try 
            let edge = matcher [a] in
            let edge = listofnode edge in
            let edge = List.map expand_fencedp edge in
            List.iter (fun e -> printf "%s " (String.concat "," (List.map E.pp_edge e))) edge;
          with 
          | Failed a-> begin 
            if O.verbose > 0 then eprintf "An unsupported item was passed to matcher function: %s\n" a;
          end
          | _ -> ()
        end
        | Op2 (Union, expl) -> List.iter f expl
        | _ -> ()
      in
      try
      List.iter (fun b -> 
        let (_,p) = (List.find (fun (name,_) -> String.equal name b) tree) in
        f p
        ) O.lets_to_print
      with
      | Not_found -> eprintf "let statements that were asked for are not in cat file\n";
   
    and pp_tree tree = 
      (* This function takes a list of let statements, in the form
      of string* expression, and prints the expanded tree of the cat file.
      Inputs:
        - tree: A list of let statements, as ast type.
      Outputs: 
        - None
      Side effects: The function prints the expanded tree of the cat file.
    *)
      let open AST in
      let pp (name,exp) =
        let rec f a = 
          match a with
          | Op2 (Seq, expl) -> begin 
            sprintf "[%s]" (String.concat ";" (List.map (fun a -> f a) expl));
          end
          | Op2 (Union, expl) -> begin
            sprintf "(%s)" (String.concat "|" (List.map (fun a -> f a) expl));
          end
          | Op2 (Inter, expl) -> begin
            sprintf "(%s)" (String.concat "&" (List.map (fun a -> f a) expl));
          end
          | Op1 (ToId, expl) -> begin 
            sprintf "[%s]" (f expl);
          end
          | Op2 (Diff, expl) -> begin
            sprintf "(%s)" (String.concat "/" (List.map (fun a -> f a) expl));
          end
          | App (exp1,exp2) -> sprintf "%s[%s]" (f exp1) (f exp2);
          | Var s -> begin
            sprintf "%s" s;
          end
          | T (_,s) | An (_, s) | Imp s| PTE s | D (_,s )| Ext (_,s)| Empty s | Lrs s-> sprintf "%s" s;
          | _ -> begin 
            raise (Failed (sprintf "Operation not implemented in tree printer"))
          end in
        printf "\n\n\n(%s)\n" name ;
        let rec ff a =
          match a with
          | h::t -> begin try printf "\n|  %s" (f h); ff t
          with
          | Failed s -> if O.verbose > 0 then eprintf "%s\n" s;
        end
          | [] -> () in
      match exp with
      | Op2 (_, expl) -> ff expl
      | Op1 (_, expl) -> ff [expl]
      | Var _ -> ff [exp]
      | _ -> if O.verbose > 0 then eprintf "Error: let statement not valid\n"; in
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
      | _ -> raise (Failed "instruction not supported\n" )

    and get_vars varname tree expression = 
      (* Retrieves an expr of the given AST.exp expression, with all variables 
      replaced as either their E.edge type or replaced with their expressions if they 
      are defined in another let statement in the cat file.
      Inputs:
      - varname: the name of the variable to get an expression for
      - tree: the list of let statements representing all variables in the cat file
      - expression: the AST.exp of the expression to get an expr for
      Outputs:
      - The expr representation of the expression
      *)
      let open AST in
      match expression with
      | Op (_, op, expl) -> begin
        (Op2 (op, List.map (get_vars varname tree) expl))
      end
      | Op1 (_, op, exp) -> Op1 (op, get_vars varname tree exp)
      | Var (a, var) -> begin
        (* check if the variable is the same as its parent variable name (to take out recursion)
           expand variable from other let statements*)
        if (String.equal var varname) then Empty var
        else
        let br = expand_var tree (a,var) in
        match br with
          | Var (_,var) -> begin
            match_var var
          end
          | Op (_, _, _) -> begin
            let a = get_vars varname tree br in
            a
          end
          | _ -> Empty ""
      end
      | App (_,_,exp2) -> get_vars varname tree exp2 (*at the moment, only dealing with exp1 = range, where we can ignore it*)
      | _ -> Empty ""
(* 
       ---------------------------------------------------------------
       Expand sequences and replace non-primitive variables
       --------------------------------------------------------------- 
*)
    and apply_expand tree =
      (* Expands the given tree of let statements to produce a new tree with all operations 
      expanded as necessary, in the expand function.
      An expansion entails expanding unions within sequences into a list of sequences.
      i.e. 
      [a; (b | c); d ]-> [a;b;d],[a;c;d]

      expansion also unrolls intersections.
      i.e. inter[a; inter(b;inter(c))] -> inter[a;b;c]
      the output will be a list of sequences and individual variables

      Inputs:
      - tree: the list of let statements to be expanded
      Outputs:
      - A new tree of let statements with all operations expanded.
      *)
      let f (name, expr) = 
        (* expand a single let statement, as ast, with expand*)
        match expr with
        | Op2 (AST.Seq,_) | Op2 (AST.Inter,_) ->
          name,Op2 (AST.Union, expand (unroll_inter expr))
        | Op2 (AST.Union,expl) ->
          name,Op2 (AST.Union, List.concat (List.map expand expl))
        | _ -> raise (Failed (sprintf "Statement type not implemented for: %s" name))
        in
      List.map f tree
    
    and expand_item input_item =
      (* Expand a chosen expression
        Inputs:
        - input_item, expression to expand
        Output: 
        - A list of lists of expressions representing a list of possible sequences
        *)
      match input_item with
      | Op2 (AST.Union, expl) -> List.concat (List.map expand_item expl)  (* union of variables *)
      | Op1 (AST.ToId, expl) -> expand_item expl
      | Op2 (AST.Inter, _) -> begin
        let a = unroll_inter input_item in
        match a with
        | Op2 (AST.Inter, expl) ->
        let expl = fold_cross expl in
        (* fold_cross has to be used to preserve sequence types, and not have them expanded as a list of expressions*)
        List.map (fun b -> [Op2 (AST.Inter,b)]) (expl)
        | _ -> raise (Failed "")
      end
      | Op2 (AST.Seq, expl) -> expand_list expl
      | _ -> [[input_item]]

    and expand_list input_item =
      (* Expands a sequence of expressions into all its possible expansions, returning a list of sequences
      Inputs: 
      - input_item, expression list representing a single sequence. Single variables are represented as a sequence of length 1
      Output: 
      - A list of lists of expressions representing a list of possible sequences
      *)
      match input_item with
      | [] -> [[]]
      | hd::tl ->
          let expanded_hd = expand_item hd in
          let expanded_tl = expand_list tl in
          List.concat (List.map (fun hd_item ->
            List.map (fun tl_item ->
              hd_item @ tl_item
            ) expanded_tl
          ) expanded_hd)

    and expand exp =
      (* Matches an input expression against three possible cases, 
      then calls expand_list and expand_item to generate all possible expansions
      Inputs: 
      - exp (expression to expand)
      Output: 
      - A list of expressions: the list of expressions here is just a list of expressions, not a sequence. 
      *)
      match exp with
      | Op2 (AST.Union, expl) -> List.concat (List.map expand expl)
      | Op2 (AST.Inter, expl) ->
        let expl = fold_cross expl in
        List.map (fun b -> Op2 (AST.Inter,b)) (expl)
      | Op2 (a, expl) -> List.map (fun b -> Op2 (a,b)) (expand_list expl)
      | Op1 (ToId, expl) -> List.concat (expand_item expl)
      | Op1 (a, _) -> raise (Failed (sprintf "%s not supported\n " (pp_op1 a)))
      | _ -> [exp]

    and expand_var tree (a,var) =
      let open AST in
      let is_var (name, _) = String.equal name var in
      match List.find_opt is_var tree with
      | Some (_,exp) -> exp
      | None -> Var (a,var)

    and fold_cross l =
      match l with
        | [] -> [[]]
        | hd::tl ->
          List.concat (List.map (fun hd_item ->
            List.map (fun tl_item ->
              hd_item::tl_item
            ) (fold_cross tl)
          ) (expand hd))
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
                else raise (Failed "atoms not applied correctly for rmw")
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
    
    and matcher expl = 
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
        | T (Po (_,_,_),_) -> [E.Po(Same,extr1,extr2);E.Po(Diff,extr1,extr2)]
        | T (Dp (dp, _, _),_) -> [E.Dp (dp,Same,extr2);E.Dp (dp,Diff,extr2)]
        | T (Fenced (fence,_,_,_),_) -> [E.Fenced (fence,Same,extr1,extr2);E.Fenced (fence,Diff,extr1,extr2)]
        | T ((Rf ie), _) -> [E.Rf ie]
        | T ((Fr ie), _) -> [E.Fr ie]
        | T ((Ws ie), _) -> [E.Ws ie]
        | T (Rmw rmw,_) -> if (A.applies_atom_rmw rmw a1 a2) then [Rmw rmw] else raise (Failed "atoms not applied correctly for rmw")
        | _ -> raise (Failed "unknown edge") in
        List.map (fun a -> E.{ node_val = { edge = a ; a1 ; a2 }; branch_list = branch_list a2 }) e in
      let rec f l prev = 
        match l with
        | Op2 (Inter, expl) :: t -> f t (match_inter expl)
        | (Var name | Empty name):: _ ->
            raise (Failed (sprintf "Unknown Variable: %s\n" name))
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
        | [] -> raise (Failed "cannot provide empty list")
      in f expl empty_expr
    
    and listofnode nodel = 
      (*translate edge_node list into edge list list for printing
        *)
      let rec f node =
        match node.branch_list with
        | h::t ->
          List.concat (List.map (fun a -> 
                       List.map (fun b -> node.node_val::b)
                       (f a)
                       ) (h::t))
        | [] -> [[node.node_val]] in
      List.concat (List.map f nodel)
    
    and check_prev a = 
      match a with
      | Empty _ -> None
      | An (b,_) -> Some b
      | Dd (_,an,_) -> an
      | _ -> None
    and check_if_dir a =
      match a with
      | D (a,_) ->  Dir a
      | Ext (NoDir,_) -> NoDir
      | Dd (r,_,_) -> r
      | _ -> Irr
    
    and match_inter expl =
      match expl with
      | h::h2::[] -> begin
        match (check_if_dir h),h2 with
        | Dir R, An ((A.Tag, None),_) -> Dd (Dir R, Some (A.Tag, None), "tag read")
        | _ -> raise (Failed "wrong inter")
      end
      | h::h2::h3::[] -> begin
        match (check_if_dir h),h2,h3 with
        | Dir R, PTE _, Imp _ -> Dd (Dir R, Some (A.Pte A.Read, None), "ImpPTE Read")
        | Dir R, An ((A.Tag, None),_), Imp _->Dd (Dir R, Some (A.Tag, None), "ImpTag Read")
        | _ -> raise (Failed "wrong inter")
      end
      | _ -> raise (Failed "wrong inter")
   
    and unroll_inter expr =
    let open AST in
      let f l =
        match l with 
        | Op2 (Inter, expl) -> expl
        | _ -> [l] in
      match expr with
      | Op2 (Inter, exp) -> Op2 (Inter,(List.concat (List.map f exp)))
      | _ -> expr
    
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
    and ast_to_tree ast : tree =
      let map_ast f l =
        List.fold_left
        (fun acc a ->
          try
            f a::acc
          with
            | Failed s -> if O.verbose > 0 then (eprintf "%s" s);
              acc)
        [] l in
      let tree_base = map_ast get_ins ast in
      let map_vars = (map_ast (
        (*if we want to print tree, then expand the entire AST, otherwise, only expand the desired let statement*)
        if O.print_tree then
          fun (varname, expression) -> varname, get_vars varname tree_base expression
        else
          fun (varname, expression) ->
          match List.find_opt (fun name -> String.equal name varname) O.lets_to_print with
          | Some _ -> varname, get_vars varname tree_base expression
          | None -> raise (Failed (sprintf "not generating for instruction: %s\n" varname))
        ) tree_base) in
      let tree = apply_expand map_vars in
      tree
    
    (*
    ------------------------------------------
    helper functions
    ------------------------------------------   
    *)

  and lrsc = T (Rmw A.LrSc, "lrsc")
  and swa = T (Rmw A.Swp, "Amo.Swp")
  and cas = T (Rmw A.Cas, "Amo.Cas")
  and ldop = Op2 (AST.Union,[
    T (Rmw (LdOp A_ADD), "Amo.LdAdd");
    T (Rmw (LdOp A_EOR), "Amo.LdEor");
    T (Rmw (LdOp A_SET), "Amo.LdSet");
    T (Rmw (LdOp A_CLR), "Amo.LdClr")])

  and storeop = Op2 (AST.Union,[
    T (Rmw (StOp A_ADD), "Amo.StAdd");
    T (Rmw (StOp A_EOR), "Amo.StEor");
    T (Rmw (StOp A_SET), "Amo.StSet");
    T (Rmw (StOp A_CLR), "Amo.StClr")])

  and amo = "amo",Op2 (AST.Union,[
    swa;cas;ldop;storeop])

  and match_var v = 
    (* Exp, Imp and PTE have been included as placeholders. rmw has not yet been included*)
    let open A in
    let open E in
      match v with
      | "R"-> D (R, v)
      | "W" -> D (W, v)
      | "M" -> Ext (Irr, v)
      | "L" -> An ((A.Rel None,None), v)
      | "P" -> An ((A.plain,None), v)
      | "A" -> An ((A.Acq None,None), v)
      | "Q" -> An ((A.AcqPc None,None), v)
      | "T" -> An ((A.Tag, None), v)
      | "Exp" -> An ((A.Pte Read, None), v)
      | "Imp" -> Imp v
      | "PTE" -> PTE v
      | "po" -> T ((Po (Same,Irr,Irr)), v)
      | "lrs" -> Lrs v
      | "addr" -> T ((Dp ((A.D.ADDR,A.NoCsel), Same, Irr)), v)
      | "ctrl" -> T ((Dp ((A.D.CTRL,A.NoCsel), Same, Irr)), v)
      | "data" -> T ((Dp ((A.D.DATA,A.NoCsel), Same, Irr)), v)
      | "DMB.ISH" -> T ((Fenced (Barrier (DMB (ISH,FULL)) ,Same, Irr, Irr)), v)
      | "DMB.OSH" -> T ((Fenced (Barrier (DMB (OSH,FULL)),Same, Irr, Irr)), v) 
      | "DMB.SY" -> T ((Fenced (Barrier (DMB (SY,FULL)),Same, Irr, Irr)), v) 
      | "DSB.ISH" -> T ((Fenced (Barrier (DSB (ISH,FULL)),Same, Irr, Irr)), v)
      | "DSB.OSH" -> T ((Fenced (Barrier (DSB (OSH,FULL)),Same, Irr, Irr)), v) 
      | "DSB.SY" -> T ((Fenced (Barrier (DSB (SY,FULL)),Same, Irr, Irr)), v)  
      | "ISB" -> T ((Fenced (Barrier (ISB),Same, Irr, Irr)), v)  
      | "rfi" -> T ((Rf Int), v)
      | "lxsx" -> lrsc
      | "co" -> Op2 (AST.Union, [
        T ((Ws Int), v);
        T ((Ws Ext), v)
      ])
      | "fr" -> Op2 (AST.Union, [
        T ((Fr Int), v);
        T ((Fr Ext), v)
      ])
      | "fri" -> T ((Fr Int), v)
      | "fre" -> T ((Fr Ext), v)
      | "rfe" -> T ((Rf Ext), v)
      | "coe" -> T ((Ws Ext), v)
      | "coi" -> T ((Ws Int), v)
      | "amo" -> let _,amo_instr = amo in amo_instr
      | _ -> Empty v

    

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

(*
----------------------------------------------
entry 
----------------------------------------------   

*)
    let zyva name =
      try
        let _,(_,_,ast)  = Parser.find_parse name in
        let tree = ast_to_tree ast in
        if O.print_tree then begin
          pp_tree tree;
          printf "\n\n\n"
        end;
        pp_relaxations tree;
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
