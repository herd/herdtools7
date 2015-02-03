open Printf
open Gen_ast

(*****************************)
(* General purpose utilities *)
(*****************************)

let default xo y = match xo with
  | None -> y
  | Some x -> x

let (!!) opt_ref = match !opt_ref with None -> assert false | Some x -> x

(************************)
(* Regions              *)
(************************)

let pp_region = function
  | Global -> "global"
  | Local -> "local"
  | Both -> assert false

let pp_region_flag = function
  | Global -> "CLK_GLOBAL_MEM_FENCE"
  | Local -> "CLK_LOCAL_MEM_FENCE"
  | Both -> "CLK_GLOBAL_MEM_FENCE|CLK_LOCAL_MEM_FENCE"

(************************)
(* Atomicity            *)
(************************)

let pp_atomicity = function
  | true -> "atomic_"
  | false -> ""

(************************)
(* Access modes         *)
(************************)

let pp_mode = function
  | Rlx -> "memory_order_relaxed"
  | Acq -> "memory_order_acquire"
  | Rel -> "memory_order_release"
  | AR -> "memory_order_acq_rel"
  | SC -> "memory_order_seq_cst"
  | NA | Bar -> assert false

let is_valid_access_mode mode atomic = 
   atomic || (mode = NA)

(************************)
(* Scope annotations    *)
(************************)

let pp_scope = function
  | S_wi -> assert false
  | S_wg -> "memory_scope_work_group"
  | S_dev -> "memory_scope_device"
  | S_all -> "memory_scope_all_svm_devices"

let is_valid_scope oscope = function
  | None | Some Global | Some Both -> true
  | Some Local -> 
    begin match oscope with
      | None | Some S_wg -> true
      | _ -> false
    end

let default_scope = function
  | Local -> S_wg
  | Global | Both -> S_all

(***************)
(* Scope trees *)
(***************)

let pp_scopetree scopetree = 
  sprintf "(device %s)"
    (String.concat " " (List.map (fun wg -> 
         sprintf "(work_group %s)" 
           (String.concat " " (List.map (fun wi -> 
                sprintf "P%i" wi) wg))) scopetree))
    

(************************************************)
(* Sending tests to herd, writing them to files *)
(************************************************)

let runherd = ref false
let herdargs = ref []

let write_file filename file_contents =
  if Sys.file_exists filename then begin
    eprintf "Warning: Overwriting %s.\n" filename
    (* ; exit 0 *)
  end;
  let oc = open_out filename in
  fprintf oc "%s" file_contents;
  close_out oc

let handle_test family test_name file_contents =
  let extn = "litmus" in
  let filename = sprintf "%s/%s.%s" family test_name extn in
  write_file filename file_contents;
  if !runherd then
    let herdargs = String.concat " " (List.rev (!herdargs)) in
    let full_filename = "testsuite/OpenCLTests/" ^ filename in
    let herd_cmd = 
      sprintf "cd ../.. && ./herd %s '%s'" herdargs full_filename in
    ignore (Sys.command herd_cmd)  
  else
    printf "%s\n" file_contents

(************************)
(* Instructions         *)
(************************)

let pp_ld id mode oscope = match mode with
  | NA -> 
    if oscope != None then raise (Arg.Bad "Spurious load scope given.");
    sprintf "*%s" id
  | SC when oscope = None -> 
    sprintf "atomic_load(%s)" id
  | Rlx | Acq | Rel | AR | SC -> 
    begin match oscope with 
      | None -> 
        sprintf "atomic_load_explicit(%s,%s)" 
          id (pp_mode mode)
      | Some scope ->
        sprintf "atomic_load_explicit(%s,%s,%s)" 
          id (pp_mode mode) (pp_scope scope)
    end
  | Bar -> assert false

let pp_st id exp mode oscope = match mode with
  | NA -> 
    if oscope != None then raise (Arg.Bad "Spurious store scope given.");
    sprintf "*%s = %s" id exp
  | SC when oscope = None -> 
    sprintf "atomic_store(%s,%s)" id exp
  | Rlx | Acq | Rel | AR | SC ->
    begin match oscope with
      | None ->
        sprintf "atomic_store_explicit(%s,%s,%s)" 
          id exp (pp_mode mode)
      | Some scope ->
        sprintf "atomic_store_explicit(%s,%s,%s,%s)" 
          id exp (pp_mode mode) (pp_scope scope) 
    end
  | Bar -> assert false

let pp_fence = function
  | None -> ""
  | Some (region,mode,oscope) ->
    match mode with
    | Bar ->
      let scope = default oscope S_wg in
      sprintf "work_group_barrier(%s,%s);\n  " 
        (pp_region_flag region) (pp_scope scope)
    | _ ->
      let scope = default oscope (default_scope region) in
      sprintf "atomic_work_item_fence(%s,%s,%s);\n  " 
        (pp_region_flag region) (pp_mode mode) (pp_scope scope)

(********************************)
(* Checking mandatory arguments *)
(********************************)

type reference = 
  | Region_ref of region option ref
  | Atomicity_ref of bool option ref
  | Mode_ref of mode option ref
  | Scope_ref of scope option ref
  | Fence_ref of fence option ref
  | Scopetree_ref of scopetree option ref

let is_empty = function
  | Region_ref r ->    !r = None
  | Atomicity_ref r -> !r = None
  | Mode_ref r ->      !r = None
  | Scope_ref r ->     !r = None
  | Fence_ref r ->     !r = None
  | Scopetree_ref r -> !r = None

let ref_of = function
  | "region_x" -> Region_ref region_x
  | "region_y" -> Region_ref region_y
  | "region_z" -> Region_ref region_z
  | "atomic_x" -> Atomicity_ref atomic_x
  | "atomic_y" -> Atomicity_ref atomic_y
  | "atomic_z" -> Atomicity_ref atomic_z
  | "ld1_x_mode" -> Mode_ref ld1_x_mode
  | "ld2_x_mode" -> Mode_ref ld2_x_mode
  | "ld1_y_mode" -> Mode_ref ld1_y_mode
  | "ld2_y_mode" -> Mode_ref ld2_y_mode
  | "ld1_z_mode" -> Mode_ref ld1_z_mode
  | "st1_x_mode" -> Mode_ref st1_x_mode
  | "st2_x_mode" -> Mode_ref st2_x_mode
  | "st1_y_mode" -> Mode_ref st1_y_mode
  | "st2_y_mode" -> Mode_ref st2_y_mode
  | "st1_z_mode" -> Mode_ref st1_z_mode
  | "st2_z_mode" -> Mode_ref st2_z_mode
  | "ld1_x_scope" -> Scope_ref ld1_x_scope
  | "ld2_x_scope" -> Scope_ref ld2_x_scope
  | "ld1_y_scope" -> Scope_ref ld1_y_scope
  | "ld2_y_scope" -> Scope_ref ld2_y_scope
  | "ld1_z_scope" -> Scope_ref ld1_z_scope
  | "st1_x_scope" -> Scope_ref st1_x_scope
  | "st2_x_scope" -> Scope_ref st2_x_scope
  | "st1_y_scope" -> Scope_ref st1_y_scope
  | "st2_y_scope" -> Scope_ref st2_y_scope
  | "st1_z_scope" -> Scope_ref st1_z_scope
  | "st2_z_scope" -> Scope_ref st2_z_scope
  | "fence1" -> Fence_ref fence1
  | "fence2" -> Fence_ref fence2
  | "fence3" -> Fence_ref fence3
  | "scopetree" -> Scopetree_ref scopetree
  | _ -> assert false

let check s = if is_empty (ref_of s) then failwith (Printf.sprintf "%s not set.\n" s)

(************************************)
(* Generating tests for each family *)
(************************************) 

let gen_mp_kernel rawspec =
  List.iter check [
    "region_x"; "atomic_x"; 
    "region_y"; "atomic_y";
    "ld1_x_mode"; "st1_x_mode";
    "ld1_y_mode"; "st1_y_mode";
    "scopetree"
  ];
  let local_size, global_size =
    match !!scopetree with
    | [[0;1]] -> 2,2
    | [[0];[1]] -> 1,2
    | _ -> eprintf "Bad scopetree for MP"; exit 0
  in
  sprintf
"//--local_size=%i --global_size=%i

__kernel void foo(%s %sint *x, %s %sint *y) { 
  int tid = get_global_id(0);
  
  if (tid == 0) {
    %s;
    %s%s;
  }
  if (tid == 1) {
    int r0 = %s;
    %sint r1 = %s;
  }
}"
  local_size global_size
  (pp_region !!region_x)
  (pp_atomicity !!atomic_x)
  (pp_region !!region_y)
  (pp_atomicity !!atomic_y)
  (pp_st "x" "1" !!st1_x_mode !st1_x_scope)
  (pp_fence !fence1)
  (pp_st "y" "1" !!st1_y_mode !st1_y_scope)
  (pp_ld "y" !!ld1_y_mode !ld1_y_scope)
  (pp_fence !fence2)
  (pp_ld "x" !!ld1_x_mode !ld1_x_scope)

let gen_mp rawspec = 
  List.iter check [
    "region_x"; "atomic_x"; 
    "region_y"; "atomic_y";
    "ld1_x_mode"; "st1_x_mode";
    "ld1_y_mode"; "st1_y_mode";
    "scopetree"
  ];
sprintf 
"OpenCL MP_%s
                        
{ 
  [x]=0;
  [y]=0;
}

P0 (%s %sint* x, %s %sint* y) {
  %s;
  %s%s;
}

P1 (%s %sint* x, %s %sint* y) {
  int r0 = %s;
  %sint r1 = %s;
}

scopeTree
%s
exists (1:r0=1 /\\ 1:r1=0)"
  rawspec
  (pp_region !!region_x)
  (pp_atomicity !!atomic_x)
  (pp_region !!region_y)
  (pp_atomicity !!atomic_y)
  (pp_st "x" "1" !!st1_x_mode !st1_x_scope)
  (pp_fence !fence1)
  (pp_st "y" "1" !!st1_y_mode !st1_y_scope)
  (pp_region !!region_x)
  (pp_atomicity !!atomic_x)
  (pp_region !!region_y)
  (pp_atomicity !!atomic_y)
  (pp_ld "y" !!ld1_y_mode !ld1_y_scope)
  (pp_fence !fence2)
  (pp_ld "x" !!ld1_x_mode !ld1_x_scope)
  (pp_scopetree !!scopetree)

let gen_3lb rawspec = 
  List.iter check [
    "region_x"; "atomic_x"; 
    "region_y"; "atomic_y";
    "region_z"; "atomic_z";
    "ld1_x_mode"; "st1_x_mode";
    "ld1_y_mode"; "st1_y_mode";
    "ld1_z_mode"; "st1_z_mode";
    "scopetree"
  ];
sprintf 
  "OpenCL 3LB_%s
                        
{ 
  [x]=0;
  [y]=0;
  [z]=0;
}

P0 (%s %sint* x, %s %sint* y, %s %sint* z) {
  int r0 = %s;
  %s%s;
}

P1 (%s %sint* x, %s %sint* y, %s %sint* z) {
  int r1 = %s;
  %s%s;
}

P2 (%s %sint* x, %s %sint* y, %s %sint* z) {
  int r2 = %s;
  %s%s;
}

scopeTree
%s
exists (0:r0=1 /\\ 1:r1=1 /\\ 2:r2=1)"
  rawspec
  (pp_region !!region_x) (pp_atomicity !!atomic_x)
  (pp_region !!region_y) (pp_atomicity !!atomic_y)
  (pp_region !!region_z) (pp_atomicity !!atomic_z)
  (pp_ld "x" !!ld1_x_mode !ld1_x_scope)
  (pp_fence !fence1)
  (pp_st "y" "1" !!st1_y_mode !st1_y_scope)
  
  (pp_region !!region_x) (pp_atomicity !!atomic_x)
  (pp_region !!region_y) (pp_atomicity !!atomic_y)
  (pp_region !!region_z) (pp_atomicity !!atomic_z)
  (pp_ld "y" !!ld1_y_mode !ld1_y_scope)
  (pp_fence !fence2)
  (pp_st "z" "1" !!st1_z_mode !st1_z_scope)
  
  (pp_region !!region_x) (pp_atomicity !!atomic_x)
  (pp_region !!region_y) (pp_atomicity !!atomic_y)
  (pp_region !!region_z) (pp_atomicity !!atomic_z)
  (pp_ld "z" !!ld1_z_mode !ld1_z_scope)
  (pp_fence !fence3)
  (pp_st "x" "1" !!st1_x_mode !st1_x_scope)
  
  (pp_scopetree !!scopetree)

let gen_iriw rawspec = 
  List.iter check [
    "region_x"; "atomic_x"; 
    "region_y"; "atomic_y";
    "ld1_x_mode"; "ld2_x_mode"; "st1_x_mode";
    "ld1_y_mode"; "ld2_y_mode"; "st1_y_mode";
    "scopetree"
  ];
sprintf 
"OpenCL IRIW_%s
                        
{ 
  [x]=0;
  [y]=0;
}

P0 (%s %sint* x, %s %sint* y) {
  %s;
}

P1 (%s %sint* x, %s %sint* y) {
  %s;
}

P2 (%s %sint* x, %s %sint* y) {
  int r0 = %s;
  %sint r1 = %s;
}

P3 (%s %sint* x, %s %sint* y) {
  int r2 = %s;
  %sint r3 = %s;
}

scopeTree
%s
exists (2:r0=1 /\\ 2:r1=0 /\\ 3:r2=1 /\\ 3:r3=0)"
  rawspec
  (pp_region !!region_x) (pp_atomicity !!atomic_x)
  (pp_region !!region_y) (pp_atomicity !!atomic_y)
  (pp_st "x" "1" !!st1_x_mode !st1_x_scope)

  (pp_region !!region_x) (pp_atomicity !!atomic_x)
  (pp_region !!region_y) (pp_atomicity !!atomic_y)
  (pp_st "y" "1" !!st1_y_mode !st1_y_scope)

  (pp_region !!region_x) (pp_atomicity !!atomic_x)
  (pp_region !!region_y) (pp_atomicity !!atomic_y)
  (pp_ld "x" !!ld1_x_mode !ld1_x_scope)
  (pp_fence !fence1)
  (pp_ld "y" !!ld1_y_mode !ld1_y_scope)
  
  (pp_region !!region_x) (pp_atomicity !!atomic_x)
  (pp_region !!region_y) (pp_atomicity !!atomic_y)
  (pp_ld "y" !!ld2_y_mode !ld2_y_scope)
  (pp_fence !fence2)
  (pp_ld "x" !!ld2_x_mode !ld2_x_scope)
  
  (pp_scopetree !!scopetree)

let gen_isa2 rawspec = 
  List.iter check [
    "region_x"; "atomic_x"; 
    "region_y"; "atomic_y";
    "region_z"; "atomic_z";
    "ld1_x_mode"; "st1_x_mode";
    "ld1_y_mode"; "st1_y_mode";
    "ld1_z_mode"; "st1_z_mode";
    "scopetree"
  ];
sprintf 
"OpenCL ISA2_%s
                        
{ 
  [x]=0;
  [y]=0;
  [z]=0;
}

P0 (%s %sint* x, %s %sint* y, %s %sint* z) {
  %s;
  %s%s;
}

P1 (%s %sint* x, %s %sint* y, %s %sint* z) {
  int r0 = %s;
  %s%s;
}

P2 (%s %sint* x, %s %sint* y, %s %sint* z) {
  int r1 = %s;
  %sint r2 = %s;
}

scopeTree
%s
exists (1:r0=1 /\\ 2:r1=1 /\\ 2:r2=0)"
  rawspec
  (pp_region !!region_x) (pp_atomicity !!atomic_x)
  (pp_region !!region_y) (pp_atomicity !!atomic_y)
  (pp_region !!region_z) (pp_atomicity !!atomic_z)
  (pp_st "x" "1" !!st1_x_mode !st1_x_scope)
  (pp_fence !fence1)
  (pp_st "y" "1" !!st1_y_mode !st1_y_scope)

  (pp_region !!region_x) (pp_atomicity !!atomic_x)
  (pp_region !!region_y) (pp_atomicity !!atomic_y)
  (pp_region !!region_z) (pp_atomicity !!atomic_z)
  (pp_ld "y" !!ld1_y_mode !ld1_y_scope)
  (pp_fence !fence2)
  (pp_st "z" "1" !!st1_z_mode !st1_z_scope)

  (pp_region !!region_x) (pp_atomicity !!atomic_x)
  (pp_region !!region_y) (pp_atomicity !!atomic_y)
  (pp_region !!region_z) (pp_atomicity !!atomic_z)
  (pp_ld "z" !!ld1_z_mode !ld1_z_scope)
  (pp_fence !fence3)
  (pp_ld "x" !!ld1_x_mode !ld1_x_scope)
  
  (pp_scopetree !!scopetree)
  
let gen_2_2w rawspec = 
  List.iter check [
    "region_x"; "atomic_x"; 
    "region_y"; "atomic_y";
    "st1_x_mode"; "st2_x_mode";
    "st1_y_mode"; "st2_y_mode";
    "scopetree"
  ];
sprintf 
"OpenCL 2+2W_%s
                        
{ 
  [x]=0;
  [y]=0;
}

P0 (%s %sint* x, %s %sint* y) {
  %s;
  %s%s;
}

P1 (%s %sint* x, %s %sint* y) {
  %s;
  %s%s;
}

scopeTree
%s
exists ([x]=1 /\\ [y]=0)"
  rawspec
  (pp_region !!region_x)
  (pp_atomicity !!atomic_x)
  (pp_region !!region_y)
  (pp_atomicity !!atomic_y)
  (pp_st "x" "2" !!st1_x_mode !st1_x_scope)
  (pp_fence !fence1)
  (pp_st "y" "1" !!st1_y_mode !st1_y_scope)
  (pp_region !!region_x)
  (pp_atomicity !!atomic_x)
  (pp_region !!region_y)
  (pp_atomicity !!atomic_y)
  (pp_st "y" "2" !!st2_x_mode !st2_x_scope)
  (pp_fence !fence2)
  (pp_st "x" "1" !!st2_y_mode !st2_y_scope)
  (pp_scopetree !!scopetree)

let gen_r rawspec = 
  List.iter check [
    "region_x"; "atomic_x"; 
    "region_y"; "atomic_y";
    "st1_x_mode"; "ld1_x_mode";
    "st1_y_mode"; "st2_y_mode";
    "scopetree"
  ];
sprintf 
"OpenCL R_%s
                        
{ 
  [x]=0;
  [y]=0;
}

P0 (%s %sint* x, %s %sint* y) {
  %s;
  %s%s;
}

P1 (%s %sint* x, %s %sint* y) {
  %s;
  %sint r0 = %s;
}

scopeTree
%s
exists (1:r0=0)"
  rawspec
  (pp_region !!region_x)
  (pp_atomicity !!atomic_x)
  (pp_region !!region_y)
  (pp_atomicity !!atomic_y)
  (pp_st "x" "1" !!st1_x_mode !st1_x_scope)
  (pp_fence !fence1)
  (pp_st "y" "1" !!st1_y_mode !st1_y_scope)

  (pp_region !!region_x)
  (pp_atomicity !!atomic_x)
  (pp_region !!region_y)
  (pp_atomicity !!atomic_y)
  (pp_st "y" "2" !!st2_y_mode !st2_y_scope)
  (pp_fence !fence2)
  (pp_ld "x" !!ld1_x_mode !ld1_x_scope)
  (pp_scopetree !!scopetree)

let gen_s rawspec = 
  List.iter check [
    "region_x"; "atomic_x"; 
    "region_y"; "atomic_y";
    "st1_x_mode"; "ld1_y_mode";
    "st1_y_mode"; "st2_x_mode";
    "scopetree"
  ];
sprintf 
"OpenCL S_%s
                        
{ 
  [x]=0;
  [y]=0;
}

P0 (%s %sint* x, %s %sint* y) {
  %s;
  %s%s;
}

P1 (%s %sint* x, %s %sint* y) {
  int r0 = %s;
  %s%s;
}

scopeTree
%s
exists (1:r0=1)"
  rawspec
  (pp_region !!region_x)
  (pp_atomicity !!atomic_x)
  (pp_region !!region_y)
  (pp_atomicity !!atomic_y)
  (pp_st "x" "2" !!st1_x_mode !st1_x_scope)
  (pp_fence !fence1)
  (pp_st "y" "1" !!st1_y_mode !st1_y_scope)

  (pp_region !!region_x)
  (pp_atomicity !!atomic_x)
  (pp_region !!region_y)
  (pp_atomicity !!atomic_y)
  (pp_ld "y" !!ld1_y_mode !ld1_y_scope)
  (pp_fence !fence2)
  (pp_st "x" "1" !!st2_x_mode !st2_x_scope)
  (pp_scopetree !!scopetree)

let gen_lb rawspec = 
  List.iter check [
    "region_x"; "atomic_x"; 
    "region_y"; "atomic_y";
    "ld1_x_mode"; "ld1_y_mode";
    "st1_x_mode"; "st1_y_mode";
    "scopetree"
  ];
sprintf 
"OpenCL LB_%s
                        
{ 
  [x]=0;
  [y]=0;
}

P0 (%s %sint* x, %s %sint* y) {
  int r0 = %s;
  %s%s;
}

P1 (%s %sint* x, %s %sint* y) {
  int r1 = %s;
  %s%s;
}

scopeTree
%s
exists (0:r0=1 /\\ 1:r1=1)"
  rawspec
  (pp_region !!region_x)
  (pp_atomicity !!atomic_x)
  (pp_region !!region_y)
  (pp_atomicity !!atomic_y)
  (pp_ld "x" !!ld1_x_mode !ld1_x_scope)
  (pp_fence !fence1)
  (pp_st "y" "1" !!st1_y_mode !st1_y_scope)

  (pp_region !!region_x)
  (pp_atomicity !!atomic_x)
  (pp_region !!region_y)
  (pp_atomicity !!atomic_y)
  (pp_ld "y" !!ld1_y_mode !ld1_y_scope)
  (pp_fence !fence2)
  (pp_st "x" "1" !!st1_x_mode !st1_x_scope)
  (pp_scopetree !!scopetree)

let gen_sb rawspec = 
  List.iter check [
    "region_x"; "atomic_x"; 
    "region_y"; "atomic_y";
    "st1_x_mode"; "st1_y_mode";
    "ld1_x_mode"; "ld1_y_mode";
    "scopetree"
  ];
sprintf 
"OpenCL SB_%s
                        
{ 
  [x]=0;
  [y]=0;
}

P0 (%s %sint* x, %s %sint* y) {
  %s;
  %sint r0 = %s;
}

P1 (%s %sint* x, %s %sint* y) {
  %s;
  %sint r1 = %s;
}

scopeTree
%s
exists (0:r0=0 /\\ 1:r1=0)"
  rawspec
  (pp_region !!region_x)
  (pp_atomicity !!atomic_x)
  (pp_region !!region_y)
  (pp_atomicity !!atomic_y)
  (pp_st "x" "1" !!st1_x_mode !st1_x_scope)
  (pp_fence !fence1)
  (pp_ld "y" !!ld1_y_mode !ld1_y_scope)

  (pp_region !!region_x)
  (pp_atomicity !!atomic_x)
  (pp_region !!region_y)
  (pp_atomicity !!atomic_y)
  (pp_st "y" "1" !!st1_y_mode !st1_y_scope)
  (pp_fence !fence2)
  (pp_ld "x" !!ld1_x_mode !ld1_x_scope)
  (pp_scopetree !!scopetree)


let gen_wrc rawspec = 
  List.iter check [
    "region_x"; "atomic_x"; 
    "region_y"; "atomic_y";
    "ld1_x_mode"; "ld2_x_mode"; "ld1_y_mode";
    "st1_x_mode"; "st1_y_mode";
    "scopetree"
  ];
sprintf 
"OpenCL WRC_%s
                        
{ 
  [x]=0;
  [y]=0;
}

P0 (%s %sint* x, %s %sint* y) {
  %s;
  %s
}

P1 (%s %sint* x, %s %sint* y) {
  int r0 = %s;
  %s%s;
}

P2 (%s %sint* x, %s %sint* y) {
  int r1 = %s;
  %sint r2 = %s;
}

scopeTree
%s
exists (1:r0=1 /\\ 2:r1=1 /\\ 2:r2=0)"
  rawspec
  (pp_region !!region_x)
  (pp_atomicity !!atomic_x)
  (pp_region !!region_y)
  (pp_atomicity !!atomic_y)
  (pp_st "x" "1" !!st1_x_mode !st1_x_scope)
  (pp_fence !fence1)

  (pp_region !!region_x)
  (pp_atomicity !!atomic_x)
  (pp_region !!region_y)
  (pp_atomicity !!atomic_y)
  (pp_ld "x" !!ld1_x_mode !ld1_x_scope)
  (pp_fence !fence2)
  (pp_st "y" "1" !!st1_y_mode !st1_y_scope)

  (pp_region !!region_x)
  (pp_atomicity !!atomic_x)
  (pp_region !!region_y)
  (pp_atomicity !!atomic_y)
  (pp_ld "y" !!ld1_y_mode !ld1_y_scope)
  (pp_fence !fence3)
  (pp_ld "x" !!ld2_x_mode !ld2_x_scope)
  (pp_scopetree !!scopetree)

let gen_rwc rawspec = 
  List.iter check [
    "region_x"; "atomic_x"; 
    "region_y"; "atomic_y";
    "ld1_x_mode"; "ld2_x_mode"; "ld1_y_mode";
    "st1_x_mode"; "st1_y_mode";
    "scopetree"
  ];
sprintf 
"OpenCL RWC_%s
                        
{ 
  [x]=0;
  [y]=0;
}

P0 (%s %sint* x, %s %sint* y) {
  %s;
  %s
}

P1 (%s %sint* x, %s %sint* y) {
  int r0 = %s;
  %sint r1 = %s;
}

P2 (%s %sint* x, %s %sint* y) {
  %s;
  %sint r2 = %s;
}

scopeTree
%s
exists (1:r0=1 /\\ 1:r1=0 /\\ 2:r2=0)"
  rawspec
  (pp_region !!region_x)
  (pp_atomicity !!atomic_x)
  (pp_region !!region_y)
  (pp_atomicity !!atomic_y)
  (pp_st "x" "1" !!st1_x_mode !st1_x_scope)
  (pp_fence !fence1)

  (pp_region !!region_x)
  (pp_atomicity !!atomic_x)
  (pp_region !!region_y)
  (pp_atomicity !!atomic_y)
  (pp_ld "x" !!ld1_x_mode !ld1_x_scope)
  (pp_fence !fence2)
  (pp_ld "y" !!ld1_y_mode !ld1_y_scope)

  (pp_region !!region_x)
  (pp_atomicity !!atomic_x)
  (pp_region !!region_y)
  (pp_atomicity !!atomic_y)
  (pp_st "y" "1" !!st1_y_mode !st1_y_scope)
  (pp_fence !fence3)
  (pp_ld "x" !!ld2_x_mode !ld2_x_scope)
  (pp_scopetree !!scopetree)

let gen_3_2w rawspec = 
  List.iter check [
    "region_x"; "atomic_x"; 
    "region_y"; "atomic_y";
    "region_z"; "atomic_z";
    "st1_x_mode"; "st2_x_mode";
    "st1_y_mode"; "st2_y_mode";
    "st1_z_mode"; "st2_z_mode";
    "scopetree"
  ];
sprintf 
  "OpenCL 3.2W_%s
                        
{ 
  [x]=0;
  [y]=0;
  [z]=0;
}

P0 (%s %sint* x, %s %sint* y, %s %sint* z) {
  %s;
  %s%s;
}

P1 (%s %sint* x, %s %sint* y, %s %sint* z) {
  %s;
  %s%s;
}

P2 (%s %sint* x, %s %sint* y, %s %sint* z) {
  %s;
  %s%s;
}

scopeTree
%s
exists ([x]=2 /\\ [y]=2 /\\ [z]=2)"
  rawspec
  (pp_region !!region_x) (pp_atomicity !!atomic_x)
  (pp_region !!region_y) (pp_atomicity !!atomic_y)
  (pp_region !!region_z) (pp_atomicity !!atomic_z)
  (pp_st "x" "2" !!st1_x_mode !st1_x_scope)
  (pp_fence !fence1)
  (pp_st "y" "1" !!st1_y_mode !st1_y_scope)
  
  (pp_region !!region_x) (pp_atomicity !!atomic_x)
  (pp_region !!region_y) (pp_atomicity !!atomic_y)
  (pp_region !!region_z) (pp_atomicity !!atomic_z)
  (pp_st "y" "2" !!st2_y_mode !st2_y_scope)
  (pp_fence !fence2)
  (pp_st "z" "1" !!st1_z_mode !st1_z_scope)
  
  (pp_region !!region_x) (pp_atomicity !!atomic_x)
  (pp_region !!region_y) (pp_atomicity !!atomic_y)
  (pp_region !!region_z) (pp_atomicity !!atomic_z)
  (pp_st "z" "2" !!st2_z_mode !st2_z_scope)
  (pp_fence !fence3)
  (pp_st "x" "1" !!st2_x_mode !st2_x_scope)
  
  (pp_scopetree !!scopetree)


let () =
  let prog = Sys.argv.(0) in
  let usage_msg = 
    sprintf "Usage: %s <%s> [options]" 
      prog (String.concat "|" [
          "MP";"3LB";"IRIW";"ISA2";
          "2+2W"; "R"; "S"; "LB"; "SB";
          "WRC"; "RWC"; "3.2W";
        ])
  in
  let family, do_parsing, handler = try 
      match Sys.argv.(1) with
      | "MP"   | "mp"   -> "MP",   Gen_parser.spec_mp,   gen_mp
      | "3LB"  | "3lb"  -> "3LB",  Gen_parser.spec_3lb,  gen_3lb
      | "IRIW" | "iriw" -> "IRIW", Gen_parser.spec_iriw, gen_iriw
      | "ISA2" | "isa2" -> "ISA2", Gen_parser.spec_isa2, gen_isa2
      | "2+2W" | "2+2w" -> "2+2W", Gen_parser.spec_2_2w, gen_2_2w
      | "R"    | "r"    -> "R",    Gen_parser.spec_r,    gen_r
      | "S"    | "s"    -> "S",    Gen_parser.spec_s,    gen_s
      | "LB"   | "lb"   -> "LB",   Gen_parser.spec_lb,   gen_lb
      | "SB"   | "sb"   -> "SB",   Gen_parser.spec_sb,   gen_sb
      | "WRC"  | "wrc"  -> "WRC",  Gen_parser.spec_wrc,  gen_wrc
      | "RWC"  | "rwc"  -> "RWC",  Gen_parser.spec_rwc,  gen_rwc
      | "3.2W" | "3.2w" -> "3.2W", Gen_parser.spec_3_2w, gen_3_2w
      | x -> raise (Arg.Bad (sprintf "Error: bad family %s." x))
    with 
    | Invalid_argument _ ->
      eprintf "%s\n%s\n" "Error: missing family." usage_msg; 
      exit 0
    | Arg.Bad msg ->
      eprintf "%s\n%s\n" msg usage_msg;
      exit 0
  in
  Arg.current := 1; (* Skip first argument *)
  let rawspec = ref "" in
  let set_rawspec v = rawspec := v in
  let process_herd_arg arg = 
    runherd := true; herdargs := arg :: (!herdargs) in
  let process_unlabelled_value x = 
    raise (Arg.Bad (sprintf "Unlabelled value: %s." x)) in 
  Arg.parse [
    "-herd", Arg.Rest process_herd_arg, "[options] Run herd with the given options";
    "-spec", Arg.String set_rawspec, sprintf "[string] Options for the %s test" family;
  ]
    process_unlabelled_value
    usage_msg;
  if !rawspec = "" then begin
    eprintf "Error: missing -spec.\n"; exit 0
  end;
  do_parsing Gen_lexer.token (Lexing.from_string !rawspec);
  let test_name = sprintf "%s_%s" family !rawspec in
  let file_contents = handler !rawspec in
  handle_test family test_name file_contents
  
