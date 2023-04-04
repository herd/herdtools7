(** Create a set of relaxations for diy using a cat file *)



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
      
    type vars = 
      | Op2 of AST.op2 
      | Op1 of AST.op1 
      | Empty 
      | Var
      | Annotation 
      | T of E.tedge
      | D of dir
      | I of ie
      | At of A.rmw
      | An of atom
      | Ext of extr
      | Imp
      | PTE
      | Dd of extr*(atom option)

    type node = {
      varname : AST.var;
      vartype : vars;
      exp_list : node list;
    }
    let empty_node = {varname="";vartype=Empty;exp_list=[]}

       (* 
       ---------------------------------------------------------------
       Entry point
       --------------------------------------------------------------- *)
    let rec get_ast fname =
        let fname0,(_,_,ast)  = Parser.find_parse fname in
        let tree = get_tree ast in
        if O.print_tree then begin
          pp_tree tree;
          printf "\n\n\n";
        end;
        
        pp_relaxations tree;
        ()
    
       (* 
       ---------------------------------------------------------------
       Printing functions
       --------------------------------------------------------------- 
       *)

    and pp_relaxations tree = 
(* pretty printing relaxations *)
      let open AST in
      let rec f a = 
        match a.vartype with
        | Op2 Seq -> begin
          let expl = a.exp_list in
          try 
            let edge = matcher expl in
            let edge = listofnode edge in
            let edge = List.map expand_fencedp edge in
            List.iter (fun e -> 
              match e with
              | _::[] -> printf "%s " (String.concat "," (List.map E.pp_edge e))
              | _::_ -> printf "[%s] " (String.concat "," (List.map E.pp_edge e))
              | _ -> if O.verbose > 0 then printf "matching logic output invalid"
              ) edge;
          with 
          | Failed a-> begin 
            if O.verbose > 0 then printf "An unsupported item was passed to matcher function: %s\n" a;
          end
          | _ -> ();
        end
        | Var | T _ -> begin
          let expl = [a] in
          try 
            let edge = matcher expl in
            let edge = listofnode edge in
            let edge = List.map expand_fencedp edge in
            List.iter (fun e -> printf "%s " (String.concat "," (List.map E.pp_edge e))) edge;
          with 
          | Failed a-> begin 
            if O.verbose > 0 then printf "An unsupported item was passed to matcher function: %s\n" a;
          end
          | _ -> ()
        end
        | Op2 Union -> List.iter f a.exp_list
        | _ -> ()
      in
      try
      List.iter (fun b -> 
        let p = (List.find (fun a -> String.equal a.varname b) tree) in
        f p
        ) O.lets_to_print
      with
      | Not_found -> printf "let statements that were asked for are not in cat file\n";
    
    and pp_tree tree = 
      let open AST in
      let pp tr =
        let rec f a = 
          match a.vartype with
          | Op2 Seq -> begin 
            try
            printf "\n|  ";
            List.iter (printf "%s") (pp_seq a.exp_list)  
            with 
            | Failed s -> if O.verbose > 0 then printf "%s\n" s;
            | _ -> if O.verbose > 0 then printf "Unknown fail";
          end
          | Op2 Union -> begin
            List.iter f a.exp_list
          end
          | Op2 Inter -> begin
            printf "[%s]" (String.concat "&" (List.map (fun a -> a.varname) a.exp_list));
          end
          | Op1 ToId -> begin 
            printf "%s["  a.varname;
            List.iter f a.exp_list;
            printf "]"
          end
          | Var | T _-> begin
            printf "%s " a.varname;
          end
          | _ -> begin 
            raise (Failed (sprintf "NOT IMPLEMENTED: %s\n" a.varname))
          end in
        printf "\n\n\n(%s)\n" tr.varname ;
        let rec ff a =
          match a with
          | h::t -> begin try f h; ff t
          with
          | Failed s -> if O.verbose > 0 then printf "%s\n" s;
        end
          | [] -> () in

        ff tr.exp_list in
      List.iter pp tree
    
    and pp_seq exp_l = 
      let rec f s a =
        match a with
        | h::t -> begin
          match h.vartype with 
          | Var -> f (append_string s h.varname) t
          | Op2 AST.Inter -> begin
            f (append_string s (sprintf "[%s]" (String.concat "&"  (List.map (fun a -> List.hd (f [""] [a])) h.exp_list)))) t;
          end
          | Op2 _ -> f s (h.exp_list@t)
          | Op1 _ -> f (f s h.exp_list) t
          | Empty -> raise (Failed "Empty var")
          | _ -> f (append_string s h.varname) t
        end
        | [] -> s  in 
      f [""] exp_l

    and append_string l ex = 
      let rec f ll =
        match ll with
        | h::t -> (h^";"^ex)::(f t)
        | [] -> [] in 
      f l
      (*
    ------------------------------------------
      Parse AST and make into custom type
    ------------------------------------------   
    *)
    and get_ins ins =
      let open AST in
      match ins with
      | Rec (_,bindl,_)
      | Let (_, bindl) -> begin
        try get_tags (List.hd bindl) with
        | Failed s -> raise (Failed s)
        | _ -> raise (Failed "unknown fail")
      end
      (* | Include (_,fname) -> 
        let _,(_,_,ast)  = Parser.find_parse fname in
        raise (Including ast) *)
      | _ -> raise (Failed "instruction not supported\n" )
    
    and get_tags (_,varname, expression) = 
      let open AST in
      let varname = match varname with | Pvar str_var -> str_var | _ -> Some "" in
      let varname = Option.get varname in
      let ins, vartype = match expression with
      | Op (_, op, expl) -> begin
        match op with
        | Union -> (List.map (get_vars varname) expl),Op2 AST.Union
        | Seq -> (List.map (get_vars varname) expl),Op2 AST.Seq
        | Inter -> (List.map (get_vars varname) expl),Op2 AST.Inter
        | _ -> raise (Failed (sprintf "%s not supported\n " (pp_op2 op)))
      end
      | Var (_,_)-> [get_vars varname expression], Var
      | _ -> raise (Failed (sprintf "operation not supported\n "))
      in {varname=varname; exp_list=ins; vartype=vartype}

    and get_vars varname expression = 
      let open AST in
      let rec f expr = 
        match expr with
        | Op (_, op, expl) -> begin
          {varname=(pp_op2 op)^":";exp_list=(List.map f expl); vartype=Op2 op}
        end
        | Op1 (_, op, expl) -> {varname=(pp_op1 op)^":";exp_list=[f expl]; vartype=Op1 op}
        | Var (_, var) ->
          if (String.equal var varname) then {varname="recursive";exp_list=[];vartype=Empty}
          else {varname=var;exp_list=[]; vartype=Var}
        | App (_,exp1,exp2) -> {varname="App:";exp_list=(List.map f (exp1::[exp2]));vartype=Empty}
        | _ -> {varname=("empty or: "^(pp_exp expr));exp_list=[];vartype=Empty} in
      f expression
(* 
       ---------------------------------------------------------------
       Expand sequences and replace non-primitive variables
       --------------------------------------------------------------- 
*)
    and apply_expand ?(second) tree =
      let f a = 
        let tree = match second with
        | None -> tree
        | Some t -> t in
        match a.vartype with
        | Op2 AST.Seq | Op2 AST.Inter ->
          {varname=a.varname;
          vartype=Op2 AST.Union;
          exp_list= expand tree {varname=a.varname;vartype=a.vartype;exp_list=unroll_inter a.exp_list}
          }
        | Op2 AST.Union | _ ->
          {varname=a.varname;
          vartype=Op2 AST.Union;
          exp_list=List.concat (List.map (expand tree) a.exp_list)
          } in
      List.map f tree

    and expand tree exp =
    (* expand each element in a let statement, expanding unions and emplacing let statements*)
      let exp_l = exp.exp_list in
      let rec f a l  =
        match a with
        | h::t -> begin
          match h.vartype with
          | Op2 AST.Union -> 
            List.concat (List.map (fun aa -> f (aa::t) l) h.exp_list)
          | Op1 AST.ToId ->
            List.concat (List.map (fun aa -> f  (aa::t) l) h.exp_list)
          | Op2 AST.Inter ->
            let hexp = unroll_inter h.exp_list in
            let hexp = f hexp [{varname="Inter:";vartype=Op2 AST.Inter;exp_list=[]}] in
            List.concat (List.map (fun a -> f t (extend_node l a)) hexp)
          | Op2 AST.Seq -> 
            f (h.exp_list@t) l
          | Var ->
            let br = check_lets tree h in
            begin match br.vartype with 
            | Var -> begin
              let vtype = 
                try match_var br with
                | Failed _ -> Var
                | _ -> Var in
              f t (extend_node l {varname=br.varname;vartype=vtype;exp_list=[]})
            end
            | _ -> 
              let bre = List.concat (List.map (expand tree) br.exp_list) in
              f ({varname=br.varname;vartype=br.vartype;exp_list=bre}::t) l
            end
          | _ -> f t (extend_node l h)

        end
        | [] -> l in
      match exp.vartype with
      | Op2 AST.Seq -> f exp_l [{varname="Seq:";vartype=Op2 AST.Seq;exp_list=[]}]
      | Op1 AST.ToId -> f exp_l [{varname="ToId:";vartype=Op1 AST.ToId;exp_list=[]}]
      | Op2 AST.Inter -> f exp_l [{varname="Inter:";vartype=Op2 AST.Inter;exp_list=[]}]
      | Var -> let br = check_lets tree exp in begin
        match br.exp_list with
        | [] -> 
          let vtype = 
            try match_var br with
            | Failed _ -> Var
            | _ -> Var in
          [{varname=br.varname;vartype=vtype;exp_list=[]}]
        | _::_ -> List.concat (List.map (expand tree) br.exp_list)
      end
      | _ -> [exp]
      
    and extend_node l ex =
      let extend_expl ll = {varname=ll.varname;vartype=ll.vartype;exp_list=(ll.exp_list@[ex])} in 
      List.map extend_expl l
    
    and check_lets tree var = 
      let rec f tr =
        match tr with
        | h::t -> begin
          if (String.equal h.varname var.varname) then begin
            h 
          end else f t
        end
        | [] -> var in
      f tree

    (*
    ------------------------------------------
      match cumulative edges
    ------------------------------------------   
    *)

    and expand_fencedp el =
    (* expand cumulative macros*)
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
          | _ -> f (h2::t) l
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
    (* translate letline sequences to E.edge type. 
       using a recursive tree to represent the different ways each edge can be represented*)
      let open AST in
      let open E in
      let rec f l prev = 
        match l with 
        | h::h2::t -> 
        begin
          let a1, a2, extr1, extr2 = extract_diratom prev h2 in
          try 
            let e a = match h.vartype with
              | T (Po (_,_,_)) -> E.Po (a,extr1,extr2)
              | T (Dp (dp, _, _)) -> E.Dp (dp,a,extr2)
              | T (Fenced (fence,_,_,_)) -> E.Fenced (fence,a,extr1,extr2)
              | _ -> raise (Failed "") in
            List.map (fun a -> 
                      {node_val={E.edge=e a;E.a1=a1;E.a2=a2};
                      branch_list=f (h2::t) h}
                      ) [Same; Diff]
          with
          | Failed _ -> match h.vartype with
            | Op2 Inter -> 
              begin try 
                let h = match_inter h in
                  f (h2::t) h
              with
                | Failed s -> raise (Failed s)
                | _ -> raise (Failed "unknown failure in inter\n")
              end
            | Var -> raise (Failed (sprintf "Unknown Variable: %s\n" h.varname))
            | _ -> f (h2::t) h
        end
        | h::[] -> 
        begin 
          let a1, a2, extr1, extr2 = extract_diratom prev empty_node in 
          try
            let e a = match h.vartype with
            | T (Po (_,_,_)) -> E.Po (a,extr1,extr2)
            | T (Dp (dp, _, _)) -> E.Dp (dp,a,extr2)
            | T (Fenced (fence,_,_,_)) ->E.Fenced (fence,a,extr1,extr2)
            | _ -> raise (Failed "") in
            List.map (fun a -> 
                      {node_val={E.edge= e a; E.a1=a1;E.a2=a2};
                      branch_list=[]}
                      ) [Same; Diff]
          with
            | Failed _ -> []
            | _ -> raise (Failed "Unknown fail")
        end
        | [] -> raise (Failed "cannot provide empty list")
      in f expl empty_node
    
    and listofnode nodel = 
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
      | Empty -> Some (A.Plain None,None)
      | An b -> Some b
      | Dd (_,an) -> an
      | _ -> None
    and check_if_dir a =
      match a with
      | D R ->  Dir R
      | D W ->  Dir W
      | Ext NoDir -> NoDir
      | Dd (r,_) -> r
      | _ -> Irr
    
    and match_inter a =
      match a.exp_list with
      | h::h2::h3::[] -> begin
        match (check_if_dir h.vartype),h2.vartype,h3.vartype with
        | Dir R, PTE, Imp -> {vartype= Dd (Dir R, Some (A.Pte A.Read, None));exp_list=[];varname="inter"}
        | Dir R, An (A.Tag, None), Imp ->{vartype= Dd (Dir R, Some (A.Tag, None));exp_list=[];varname="inter"}
        | _ -> raise (Failed "wrong inter")
      end
      | h::h2::[] -> raise (Failed "wrong inter")
      | _ -> raise (Failed "wrong inter")
    
    and unroll_inter a = 
    let open AST in
      let f l =
        match l.vartype with 
        | Op2 Inter -> l.exp_list
        | _ -> [l] in
      List.concat (List.map f a)
    
    and extract_diratom prev next =
      let extr1 = check_if_dir prev.vartype in
      let extr2 = check_if_dir next.vartype in
      
      let a1 = check_prev prev.vartype in
      let a2 = check_prev next.vartype in
      let extr1 = check_dirfroma a1 extr1 in
      let extr2 = check_dirfroma a2 extr2 in
      a1,a2,extr1,extr2
        
    (*
    ------------------------------------------
    entry point for constructing tree
    ------------------------------------------   
    *)
    and get_tree ast = 
      let rec tree l = 
        match l with
        | h::t -> begin try
          (get_ins h)::(tree t)
        with
          | Failed s -> begin 
            if O.verbose > 0 then 
              (printf "%s" s)
          end;
            (tree t)
          | Including a -> tree (a@t)
          | _ -> (tree t)
        end
        | [] -> [] in

      let tree = apply_expand (tree ast) in
      tree  
    
    (*
    ------------------------------------------
    helper functions
    ------------------------------------------   
    *)
    and match_var v = 
    (* Exp, Imp and PTE have been included as placeholders. rmw has not yet been included*)
    let open A in
    let open E in
      match v.vartype with
      | Var -> begin 
        match v.varname with
        | "R"-> D R
        | "W" -> D W
        | "M" -> Ext Irr
        | "L" -> An (A.Rel None,None)
        | "P" -> An (A.plain,None)
        | "A" -> An (A.Acq None,None)
        | "Q" -> An (A.AcqPc None,None)
        | "T" -> An (A.Tag, None)
        | "Exp" -> An (A.Pte Read, None)
        | "Imp" -> Imp
        | "PTE" -> PTE
        | "po" -> T (Po (Same,Irr,Irr))
        | "addr" -> T (Dp ((A.D.ADDR,A.NoCsel), Same, Irr))
        | "ctrl" -> T (Dp ((A.D.CTRL,A.NoCsel), Same, Irr))
        | "data" -> T (Dp ((A.D.DATA,A.NoCsel), Same, Irr))
        | "DMB.ISH" -> T (Fenced (Barrier (DMB (ISH,FULL)) ,Same, Irr, Irr))
        | "DMB.OSH" -> T (Fenced (Barrier (DMB (OSH,FULL)),Same, Irr, Irr)) 
        | "DMB.SY" -> T (Fenced (Barrier (DMB (SY,FULL)),Same, Irr, Irr)) 
        | "DSB.ISH" -> T (Fenced (Barrier (DSB (ISH,FULL)),Same, Irr, Irr)) 
        | "DSB.OSH" -> T (Fenced (Barrier (DSB (OSH,FULL)),Same, Irr, Irr)) 
        | "DSB.SY" -> T (Fenced (Barrier (DSB (SY,FULL)),Same, Irr, Irr))  
        | "ISB" -> T (Fenced (Barrier (ISB),Same, Irr, Irr))  
        | "rfi" -> T (Rf Int)
        | "co" | "fr" -> T (Fr Int)
        | _ -> raise (Failed (sprintf "variable not supported: %s\n" v.varname))
      end
      | _ -> v.vartype


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
      try get_ast name
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
