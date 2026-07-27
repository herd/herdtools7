Reject tests with post-condition that produce no candidate executions.

  $ cat > no_candidate.litmus <<'EOF'
  > AArch64 NoCandidate
  > {
  > x=0;
  > 0:X0=x;
  > }
  >  P0          ;
  >  LDR W1,[X0] ;
  > exists ([x]=1)
  > EOF
  $ litmus2desc -set-libdir ./libdir --describe-dep-path no_candidate.litmus
  litmus2desc: no candidate executions satisfy the post-condition.
  [1]
