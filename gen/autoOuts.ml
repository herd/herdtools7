open Printf

module type S = sig
  module A : AutoArch.S
  type relax = A.R.relax
  type set = A.R.Set.t

  type outs
  val empty : outs
  val is_empty : outs -> bool
  val equal : outs -> outs -> bool

  val pp : out_channel -> string -> outs -> unit

  val make : set -> A.L.outcome list -> outs

  val intests : outs -> set

  val diff : outs -> outs -> outs
  val union : outs -> outs -> outs

  val expand_cumul : outs -> outs

(* Get relaxed from safe set *)
  val get_relaxed_assuming : set -> outs -> set

  val get_relax_witnesses : set -> relax -> outs -> outs


(*******************)
(* Safe heuristics *)
(*******************)

(* first argument is relaxed set, second is tested relaxations *)
  val simplify_for_safes : set -> set -> outs -> outs

(* heuristics proper *)
  val safe_by_inter : outs -> set
(* first arg is tests seen ok, second is tests seen safe *)
  val safe_by_cardinal : outs -> outs -> set

(* Compute (some of) false safes *)
  val unexplained : set -> outs -> outs
  val false_safes : set -> set -> outs -> set

end

module Make(C:AutoConf.S) : S with module A = C.A
    = struct
      open C
      module A = A
      module E = A.E
      module R = A.R
      module L = A.L


      type relax = R.relax

      type set = R.Set.t


(* Outcome proper *)
      module O =  struct 
        type t =
            { name : string ;
              cycle : string ;
              interpret : I.t
            }

        let pp chan o =
          fprintf chan "%s: '%s' %a\n" o.name o.cycle I.pp o.interpret

      end

      module M = Map.Make(String)
      include M

(* outs is really a set of keys *)
      let equal os1 os2 = M.equal (fun _ _ -> true) os1 os2

      type outs = O.t t

      let pp chan title os =
        if is_empty os then ()
        else begin
          fprintf chan "++ %s ++\n" title ;
          iter (fun _ o -> O.pp chan o) os ;
          fprintf chan "++++++++\n%!"
        end

      let make all os =
        List.fold_left
          (fun k o ->
            add
              o.L.cycle
              {O.name = o.L.name ;
               cycle = o.L.cycle ;
               interpret = I.interpret all o }
              k)
          empty os

      let intests os =
        let xs =
          fold (fun _ o k -> I.intest o.O.interpret::k)              
            os [] in
        R.Set.unions xs

(*******************************)
(* Convenient additions to Map *)
(*******************************)

      let union = fold add          

      let diff o1 o2 =
        fold
          (fun k v r ->
            if mem k o2 then r
            else add k v r)
          o1 empty

(************)
(* Specific *)
(************)

      let expand_cumul =
        map
          (fun o -> { o with O.interpret = I.expand_cumul o.O.interpret })

      let simplify_for_safes relaxed testing os =
        fold
          (fun k o r ->
            match
              I.simplify_for_safes relaxed testing
                o.O.interpret
            with
            | None -> r
            | Some i ->
                add k { o with O.interpret = i } r)
          os empty

      let get_relaxed_assuming safe os =
        let rs =
          fold
            (fun _ o -> I.get_relaxed_assuming safe o.O.interpret)
            os [] in
        R.Set.of_list rs        

  let get_relax_witnesses safe r os =
    fold
      (fun cy o k->
        if I.shows_relax safe r o.O.interpret then add cy o k
        else k)
      os empty

(* Safe heuristics *)
      let safe_by_inter os =
        R.Set.unions
          (fold
             (fun _ o k -> I.safe_by_inter o.O.interpret::k)
             os [])

      let group =
        let rec g_rec c k = function
          | [] -> [k]
          | (x,d)::xs ->
              if d = c then g_rec c (x::k) xs
              else
                let xs = g_rec d [x] xs in
                k::xs in
        function
          | [] -> []
          | (x,c)::xs -> g_rec c [x] xs

(* select relaxations that occur most *)
      let factor = 4

      let count xs =
        List.fold_left
          (fun m rs ->
            A.R.Set.fold
              (fun r m ->
                  let v =
                    try A.R.Map.find r m
                    with Not_found -> 0 in
                  A.R.Map.add r (v+1) m)
              rs m) A.R.Map.empty xs
      let pp_count m =
        A.R.Map.iter
          (fun r o -> eprintf " %s->%i" (A.R.pp_relax r) o)
          m ;
        eprintf "\n%!"

      let take_high ok xs =
        let ms  = count xs
        and mr = count ok in
        if C.verbose > 0 then begin
          eprintf "Occurrence counts for safes:" ;
          pp_count ms ;
          eprintf "Occurrence counts for relaxs:" ;
          pp_count mr ;
          ()
        end ;
        let xs =
          A.R.Map.fold
            (fun r o k ->
              let o_r =
                try A.R.Map.find r mr
                with Not_found -> 0 in
              if factor*o > o_r then r::k else k)
            ms [] in
        let r = A.R.Set.of_list xs in
        if C.verbose > 0 then begin
          eprintf "Above %i/%i: %a\n%!" (factor-1) factor A.R.pp_set r
        end;
        r

      let extract os =
        let xs = 
          fold
            (fun _ o ->I.safe_by_cardinal o.O.interpret)
            os [] in
        let xs =
          List.sort (fun (_,c1) (_,c2) -> Misc.int_compare c1 c2)
            xs in
        match group xs with
        | [] -> []
        | xs::_ -> xs

      let safe_by_cardinal ok os =
        match extract os with 
        | [] -> R.Set.empty
        | xs ->
            if !Misc.switch then
              take_high (extract ok) xs
            else
              R.Set.unions xs


(* False safe heuristics *)
      let count safe os =
        fold
          (fun _ o -> I.count o.O.name safe o.O.interpret)
          os R.Map.empty

      let find_max avoid rm =
        let _,r =
          R.Map.fold 
            (fun r n ((max,k) as acc) -> 
              if R.Set.mem r avoid  then acc
              else
                if n < max then acc
                else if n > max then (n,R.Set.singleton r)
                else (n,R.Set.add r k))
            rm (0,R.Set.empty) in 
        r


      let unexplained safe ok =
        fold
          (fun cy o k -> match I.unexplained safe o.O.interpret with
          | None -> k
          | Some i ->
              add cy { o with O.interpret = i } k)
          ok empty

      let false_safes avoid safe ok =
        let rm = count safe ok in
        if verbose > 1 && not (R.Map.is_empty rm) then begin
          eprintf "++++++++++\n" ;
          R.Map.iter
            (fun r n ->
              eprintf "%s -> %i\n" (R.pp_relax r) n)
            rm ;
          eprintf "++++++++++\n%!"
        end ;
        find_max avoid rm

    end
