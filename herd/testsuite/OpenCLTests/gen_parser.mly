%{
open Gen_ast

let set_var (ref1,ref2) (val1,val2) =
  ref1 := Some val1;
  ref2 := Some val2

let set_op (ref1,ref2) (val1,oval2) =
  ref1 := Some val1;
  ref2 := oval2

let set_op_fence (ref1,ref2,ref3) (val1,oval2,oval3) =
  set_op (ref1,ref2) (val1,oval2);
  ref3 := oval3

let set_op_fence_op (ref1,ref2,ref3,ref4,ref5) (val1,oval2,oval3,val4,oval5) =
  set_op (ref1,ref2) (val1,oval2);
  ref3 := oval3;
  set_op (ref4,ref5) (val4,oval5)

%}

%token <int> PROC
%token <bool> ATOMICITY
%token <Gen_ast.region> REGION
%token <Gen_ast.mode> MODE
%token <Gen_ast.scope> SCOPE
%token F
%token B
%token X Y Z
%token SEP SEPSEP USCORE DASH LBRK RBRK
%token EOF

%right SEPSEP
%right SEP

%type <unit> 
  spec_mp spec_3lb spec_iriw spec_isa2 spec_2_2w
  spec_r spec_s spec_lb spec_sb spec_wrc spec_rwc spec_3_2w

%start 
  spec_mp spec_3lb spec_iriw spec_isa2 spec_2_2w
  spec_r spec_s spec_lb spec_sb spec_wrc spec_rwc spec_3_2w

%%

opt_scope:
| { None }
| LBRK SCOPE RBRK
  { Some $2 }

opt_fence:
| { None }
| REGION F MODE opt_scope 
  { Some ($1, $3, $4) }
| REGION B opt_scope
  { Some ($1, Bar, $3) }

scopetree_inner:
| PROC 
  { [[$1]] }
| scopetree_inner SEP scopetree_inner
  { match $1,$3 with [x],[y] -> [x @ y] | _ -> assert false }
| scopetree_inner SEPSEP scopetree_inner
  { $1 @ $3 }

scopetree: scopetree_inner 
{ scopetree := Some $1 }

var_spec: ATOMICITY REGION 
{ ($1, $2) }

op: 
    MODE opt_scope
{ ($1,$2) }

op_fence: 
    MODE opt_scope DASH opt_fence
{ ($1,$2,$4) }

op_fence_op: 
    MODE opt_scope DASH opt_fence DASH MODE opt_scope
{ ($1,$2,$4,$6,$7) }

two_vars: X var_spec USCORE Y var_spec
{
  set_var (atomic_x, region_x) $2;
  set_var (atomic_y, region_y) $5;
}

three_vars: X var_spec USCORE Y var_spec USCORE Z var_spec
{
  set_var (atomic_x, region_x) $2;
  set_var (atomic_y, region_y) $5;
  set_var (atomic_z, region_z) $8;
}

ops_mp: op_fence_op USCORE op_fence_op
{
  set_op_fence_op (st1_x_mode, st1_x_scope, fence1, st1_y_mode, st1_y_scope) $1;
  set_op_fence_op (ld1_y_mode, ld1_y_scope, fence2, ld1_x_mode, ld1_x_scope) $3;
}

spec_mp: two_vars USCORE ops_mp USCORE scopetree EOF {} 

ops_r: op_fence_op USCORE op_fence_op
{
  set_op_fence_op (st1_x_mode, st1_x_scope, fence1, st1_y_mode, st1_y_scope) $1;
  set_op_fence_op (st2_y_mode, st2_y_scope, fence2, ld1_x_mode, ld1_x_scope) $3;  
}

spec_r: two_vars USCORE ops_r USCORE scopetree EOF {}

ops_s: op_fence_op USCORE op_fence_op
{
  set_op_fence_op (st1_x_mode, st1_x_scope, fence1, st1_y_mode, st1_y_scope) $1;
  set_op_fence_op (ld1_y_mode, ld1_y_scope, fence2, st2_x_mode, st2_x_scope) $3;  
}

spec_s: two_vars USCORE ops_s USCORE scopetree EOF {}

ops_lb: op_fence_op USCORE op_fence_op
{
  set_op_fence_op (ld1_x_mode, ld1_x_scope, fence1, st1_y_mode, st1_y_scope) $1;
  set_op_fence_op (ld1_y_mode, ld1_y_scope, fence2, st1_x_mode, st1_x_scope) $3;
}

spec_lb: two_vars USCORE ops_lb USCORE scopetree EOF {}

ops_sb: op_fence_op USCORE op_fence_op
{
  set_op_fence_op (st1_x_mode, st1_x_scope, fence1, ld1_y_mode, ld1_y_scope) $1;
  set_op_fence_op (st1_y_mode, st1_y_scope, fence2, ld1_x_mode, ld1_x_scope) $3;
}

spec_sb: two_vars USCORE ops_sb USCORE scopetree EOF {}

ops_2_2w: op_fence_op USCORE op_fence_op
{
  set_op_fence_op (st1_x_mode, st1_x_scope, fence1, st1_y_mode, st1_y_scope) $1;
  set_op_fence_op (st2_y_mode, st2_y_scope, fence2, st2_x_mode, st2_x_scope) $3;  
}

spec_2_2w: two_vars USCORE ops_2_2w USCORE scopetree EOF {}

ops_3lb: op_fence_op USCORE op_fence_op USCORE op_fence_op
{
  set_op_fence_op (ld1_x_mode, ld1_x_scope, fence1, st1_y_mode, st1_y_scope) $1;
  set_op_fence_op (ld1_y_mode, ld1_y_scope, fence2, st1_z_mode, st1_z_scope) $3;
  set_op_fence_op (ld1_z_mode, ld1_z_scope, fence3, st1_x_mode, st1_x_scope) $5
}

spec_3lb: three_vars USCORE ops_3lb USCORE scopetree EOF {}

ops_wrc: op_fence USCORE op_fence_op USCORE op_fence_op
{
  set_op_fence (st1_x_mode, st1_x_scope, fence1) $1;
  set_op_fence_op (ld1_x_mode, ld1_x_scope, fence2, st1_y_mode, st1_y_scope) $3;
  set_op_fence_op (ld1_y_mode, ld1_y_scope, fence3, ld2_x_mode, ld2_x_scope) $5
}

spec_wrc: two_vars USCORE ops_wrc USCORE scopetree EOF {}

ops_rwc: op_fence USCORE op_fence_op USCORE op_fence_op
{
  set_op_fence (st1_x_mode, st1_x_scope, fence1) $1;
  set_op_fence_op (ld1_x_mode, ld1_x_scope, fence2, ld1_y_mode, ld1_y_scope) $3;
  set_op_fence_op (st1_y_mode, st1_y_scope, fence3, ld2_x_mode, ld2_x_scope) $5
}

spec_rwc: two_vars USCORE ops_rwc USCORE scopetree EOF {}

ops_iriw: op USCORE op USCORE op_fence_op USCORE op_fence_op
{
  set_op (st1_x_mode, st1_x_scope) $1;
  set_op (st1_y_mode, st1_y_scope) $3;
  set_op_fence_op (ld1_x_mode, ld1_x_scope, fence1, ld1_y_mode, ld1_y_scope) $5;
  set_op_fence_op (ld2_y_mode, ld2_y_scope, fence2, ld2_x_mode, ld2_x_scope) $7
}

spec_iriw: two_vars USCORE ops_iriw USCORE scopetree EOF {}

ops_isa2: op_fence_op USCORE op_fence_op USCORE op_fence_op
{
  set_op_fence_op (st1_x_mode, st1_x_scope, fence1, st1_y_mode, st1_y_scope) $1;
  set_op_fence_op (ld1_y_mode, ld1_y_scope, fence2, st1_z_mode, st1_z_scope) $3;
  set_op_fence_op (ld1_z_mode, ld1_z_scope, fence3, ld1_x_mode, ld1_x_scope) $5
}

spec_isa2: three_vars USCORE ops_isa2 USCORE scopetree EOF {}

ops_3_2w: op_fence_op USCORE op_fence_op USCORE op_fence_op
{
  set_op_fence_op (st1_x_mode, st1_x_scope, fence1, st1_y_mode, st1_y_scope) $1;
  set_op_fence_op (st2_y_mode, st2_y_scope, fence2, st1_z_mode, st1_z_scope) $3;
  set_op_fence_op (st2_z_mode, st2_z_scope, fence3, st2_x_mode, st2_x_scope) $5
}

spec_3_2w: three_vars USCORE ops_3_2w USCORE scopetree EOF {}
