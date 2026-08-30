Check GCSPOPM dot output under shadowstack.

  $ herd7 -set-libdir ../libdir -variant shadowstack GCSPOPM.litmus -show all -showevents all -showinitwrites false -o - | sed '/^Time/d'
  
  DOTBEGIN GCSPOPM
  DOTCOM dot
  digraph G {
  
  
  /* legend */
  label="Test GCSPOPM-dot, Generic[withcatdep](Unknown)";
  
  
  /* the unlocked events */
  subgraph cluster_proc0 { rank=sink; label = "Thread 0"; color=magenta; shape=box;
  eiid0 [label="a: R[x]GCSq=4\lproc:P0 poi:0\lGCSPOPM X1", shape="box", color="blue"];
  eiid2 [label="c: R0:GCSPR_EL1q=x (addr)\lproc:P0 poi:0\lGCSPOPM X1", shape="box", color="blue"];
  eiid3 [label="d: Branching(pred)(PCAligned)\lproc:P0 poi:0\lGCSPOPM X1", shape="box", color="blue"];
  eiid4 [label="e: W0:X1q=4\lproc:P0 poi:0\lGCSPOPM X1", shape="box", color="blue"];
  eiid5 [label="f: W0:GCSPR_EL1q=x+8\lproc:P0 poi:0\lGCSPOPM X1", shape="box", color="blue"];
  }
  
  /* the intra_causality_data edges */
  
  eiid0 -> eiid3 [label="iico_data", color="black", fontcolor="black"];
  eiid0 -> eiid4 [label="iico_data", color="black", fontcolor="black"];
  eiid2 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid2 -> eiid5 [label="iico_data", color="black", fontcolor="black"];
  
  /* the intra_causality_control edges */
  eiid3 -> eiid4 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid3 -> eiid5 [label="iico_ctrl", color="grey", fontcolor="grey"];
  
  /* the poi edges */
  /* the rfmap edges */
  
  
  /* The viewed-before edges */
  }
  
  DOTEND GCSPOPM
  Test GCSPOPM-dot Required
  States 1
  
  Ok
  Witnesses
  Positive: 1 Negative: 0
  Flag Guarded-Control-Stack-is-work-in-progress
  Condition forall (true)
  Observation GCSPOPM-dot Always 1 0
  Hash=b42c2d302c23e4a44044dd980148c33a
  

Check GCSPUSHM dot output under shadowstack.

  $ herd7 -set-libdir ../libdir -variant shadowstack GCSPUSHM.litmus -show all -showevents all -showinitwrites false -o - | sed '/^Time/d'
  
  DOTBEGIN GCSPUSHM
  DOTCOM dot
  digraph G {
  
  
  /* legend */
  label="Test GCSPUSHM, Generic[withcatdep](Unknown)";
  
  
  /* the unlocked events */
  subgraph cluster_proc0 { rank=sink; label = "Thread 0"; color=magenta; shape=box;
  eiid0 [label="a: W[x]GCSq=4\lproc:P0 poi:0\lGCSPUSHM X0", shape="box", color="blue"];
  eiid2 [label="c: R0:GCSPR_EL1q=x+8 (addr)\lproc:P0 poi:0\lGCSPUSHM X0", shape="box", color="blue"];
  eiid3 [label="d: W0:GCSPR_EL1q=x\lproc:P0 poi:0\lGCSPUSHM X0", shape="box", color="blue"];
  eiid4 [label="e: R0:X0q=4 (data)\lproc:P0 poi:0\lGCSPUSHM X0", shape="box", color="blue"];
  }
  
  /* the intra_causality_data edges */
  
  eiid2 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid2 -> eiid3 [label="iico_data", color="black", fontcolor="black"];
  eiid4 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  
  /* the intra_causality_control edges */
  
  /* the poi edges */
  /* the rfmap edges */
  
  
  /* The viewed-before edges */
  }
  
  DOTEND GCSPUSHM
  Test GCSPUSHM Required
  States 1
  
  Ok
  Witnesses
  Positive: 1 Negative: 0
  Flag Guarded-Control-Stack-is-work-in-progress
  Condition forall (true)
  Observation GCSPUSHM Always 1 0
  Hash=f647485f9c4c227df7e6e064a001a623
  

Check GCSPOPM dot output under shadowstack,vmsa.

  $ herd7 -set-libdir ../libdir -variant shadowstack,vmsa GCSPOPM.litmus -show all -showevents all -showinitwrites false -o - | sed '/^Time/d'
  
  DOTBEGIN GCSPOPM
  DOTCOM dot
  digraph G {
  
  
  /* legend */
  label="Test GCSPOPM-dot, Generic[withcatdep](Unknown)";
  
  
  /* the unlocked events */
  subgraph cluster_proc0 { rank=sink; label = "Thread 0"; color=magenta; shape=box;
  eiid0 [label="a: R[PTE(x)]NExpq=(oa:PA(x))\lproc:P0 poi:0\lGCSPOPM X1", shape="box", color="blue"];
  eiid1 [label="b: R[PA(x)]GCSq=4\lproc:P0 poi:0\lGCSPOPM X1", shape="box", color="blue"];
  eiid4 [label="e: R0:GCSPR_EL1q=x (addr)\lproc:P0 poi:0\lGCSPOPM X1", shape="box", color="blue"];
  eiid5 [label="f: Branching(pred)(valid:1 && af:1)\lproc:P0 poi:0\lGCSPOPM X1", shape="box", color="blue"];
  eiid6 [label="g: Branching(pred)(PCAligned)\lproc:P0 poi:0\lGCSPOPM X1", shape="box", color="blue"];
  eiid7 [label="h: W0:X1q=4\lproc:P0 poi:0\lGCSPOPM X1", shape="box", color="blue"];
  eiid8 [label="i: W0:GCSPR_EL1q=x+8\lproc:P0 poi:0\lGCSPOPM X1", shape="box", color="blue"];
  }
  
  /* the intra_causality_data edges */
  
  eiid0 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  eiid0 -> eiid5 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid6 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid7 [label="iico_data", color="black", fontcolor="black"];
  eiid4 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid4 -> eiid8 [label="iico_data", color="black", fontcolor="black"];
  
  /* the intra_causality_control edges */
  eiid5 -> eiid1 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid6 -> eiid7 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid6 -> eiid8 [label="iico_ctrl", color="grey", fontcolor="grey"];
  
  /* the poi edges */
  /* the rfmap edges */
  
  
  /* The viewed-before edges */
  }
  
  DOTEND GCSPOPM
  Test GCSPOPM-dot Required
  States 1
  
  Ok
  Witnesses
  Positive: 1 Negative: 0
  Flag Guarded-Control-Stack-is-work-in-progress
  Condition forall (true)
  Observation GCSPOPM-dot Always 1 0
  Hash=b42c2d302c23e4a44044dd980148c33a
  

Check GCSPUSHM dot output under shadowstack,vmsa.

  $ herd7 -set-libdir ../libdir -variant shadowstack,vmsa GCSPUSHM.litmus -show all -showevents all -showinitwrites false -o - | sed '/^Time/d'
  
  DOTBEGIN GCSPUSHM
  DOTCOM dot
  digraph G {
  
  
  /* legend */
  label="Test GCSPUSHM, Generic[withcatdep](Unknown)";
  
  
  /* the unlocked events */
  subgraph cluster_proc0 { rank=sink; label = "Thread 0"; color=magenta; shape=box;
  eiid0 [label="a: R[PTE(x)]NExpq=(oa:PA(x))\lproc:P0 poi:0\lGCSPUSHM X0", shape="box", color="blue"];
  eiid1 [label="b: W[PA(x)]GCSq=4\lproc:P0 poi:0\lGCSPUSHM X0", shape="box", color="blue"];
  eiid4 [label="e: R0:GCSPR_EL1q=x+8 (addr)\lproc:P0 poi:0\lGCSPUSHM X0", shape="box", color="blue"];
  eiid5 [label="f: Branching(pred)(valid:1 && af:1 && db:1)\lproc:P0 poi:0\lGCSPUSHM X0", shape="box", color="blue"];
  eiid6 [label="g: W0:GCSPR_EL1q=x\lproc:P0 poi:0\lGCSPUSHM X0", shape="box", color="blue"];
  eiid7 [label="h: R0:X0q=4 (data)\lproc:P0 poi:0\lGCSPUSHM X0", shape="box", color="blue"];
  }
  
  /* the intra_causality_data edges */
  
  eiid0 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  eiid0 -> eiid5 [label="iico_data", color="black", fontcolor="black"];
  eiid4 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid4 -> eiid6 [label="iico_data", color="black", fontcolor="black"];
  eiid7 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  
  /* the intra_causality_control edges */
  eiid5 -> eiid1 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid5 -> eiid6 [label="iico_ctrl", color="grey", fontcolor="grey"];
  
  /* the poi edges */
  /* the rfmap edges */
  
  
  /* The viewed-before edges */
  }
  
  DOTEND GCSPUSHM
  Test GCSPUSHM Required
  States 1
  
  Ok
  Witnesses
  Positive: 1 Negative: 0
  Flag Guarded-Control-Stack-is-work-in-progress
  Condition forall (true)
  Observation GCSPUSHM Always 1 0
  Hash=f647485f9c4c227df7e6e064a001a623
  
