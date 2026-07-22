  $ cat > single_ldr.litmus <<'EOF'
  > AArch64 SingleLDR
  > {
  > x=0;
  > 0:X0=x;
  > }
  >  P0          ;
  >  LDR W1,[X0] ;
  > exists ([x]=0)
  > EOF
  $ litmus2desc -set-libdir ./libdir --describe-dep-path single_ldr.litmus
  This test asks whether the following execution is architecturally Allowed:
  
  * The Load instruction on P0, i.e. LDR W1,[X0], generates an Explicit Memory Read Effect of Location x;
  * The value of x is 0 in the end, because x retains its initial value.
