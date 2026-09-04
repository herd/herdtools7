Extract test output from litmus log
  $ mlogselect7 -oknames SB-PPC ../../../doc/SB-PPC.log
  Test SB-PPC Allowed
  Histogram (3 states)
  1784  *>0:r3=0; 1:r3=0;
  498564:>0:r3=1; 1:r3=0;
  499652:>0:r3=0; 1:r3=1;
  Ok
  Witnesses
  Positive: 1784, Negative: 998216
  Condition exists (0:r3=0 /\ 1:r3=0) is validated
  Hash=4edecf6abc507611612efaecc1c4a9bc
  Observation SB-PPC Sometimes 1784 998216
  Time SB-PPC 0.55
  
Extract test output from msum log
  $ msum7 ../../../doc/SB-PPC*.log 2>/dev/null | mlogselect7 -select ../../../doc/SB-PPC.litmus
  Test SB-PPC Allow
  Histogram (3 states)
  3549    :> 0:r3=0; 1:r3=0;
  999146  :> 0:r3=0; 1:r3=1;
  997305  :> 0:r3=1; 1:r3=0;
  Ok
  Witnesses
  Positive: 3549 Negative: 1996451
  Condition exists (0:r3=0 /\ 1:r3=0) is validated
  Hash=4edecf6abc507611612efaecc1c4a9bc
  Time SB-PPC 1.12
  
Extract test output from herd log
  $ for i in $(seq 1 2 9); do echo "A00$i"; done > NAMES
  $ herd7 -set-libdir ../../../herd/libdir ../../../herd/tests/instructions/X86_64/*.litmus | mlogselect7 -names NAMES -nonames A003,A004 -oknames A012
  Test A001 Required
  States 1
  0:rip=0;
  Ok
  Witnesses
  Positive: 1 Negative: 0
  Condition forall (0:rip=0)
  Observation A001 Always 1 0
  Time A001 0.00
  Hash=5586d6c213112d3683c915b4d7bb700a
  
  Test A005 Required
  States 1
  0:rax=0;
  Ok
  Witnesses
  Positive: 1 Negative: 0
  Condition forall (0:rax=0)
  Observation A005 Always 1 0
  Time A005 0.00
  Hash=33e970980643a03ccf92e4989259cd01
  
  Test A007 Required
  States 1
  0:rax=4;
  Ok
  Witnesses
  Positive: 1 Negative: 0
  Condition forall (0:rax=4)
  Observation A007 Always 1 0
  Time A007 0.00
  Hash=a335aafc9060d9b8c4320424ba909740
  
  Test A009 Required
  States 1
  0:rcx=-1;
  Ok
  Witnesses
  Positive: 1 Negative: 0
  Condition forall (0:rcx=-1)
  Observation A009 Always 1 0
  Time A009 0.00
  Hash=7ca3c35015d75a877ccf509d75062e79
  
  Test A012 Required
  States 1
  [x]=1;
  Ok
  Witnesses
  Positive: 1 Negative: 0
  Condition forall ([x]=1)
  Observation A012 Always 1 0
  Time A012 0.00
  Hash=3b021e40517dcff1f1cb8894d645b677
  
