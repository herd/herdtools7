Check Post-idx STR dot output.

  $ herd7 -set-libdir ../libdir STNP.litmus -show all -showevents all -showinitwrites false -o - | sed '/^Time/d'
  
  DOTBEGIN STNP
  DOTCOM dot
  digraph G {
  
  
  /* legend */
  label="Test STNP, Generic[withcatdep](Unknown)";
  
  
  /* the unlocked events */
  subgraph cluster_proc0 { rank=sink; label = "Thread 0"; color=magenta; shape=box;
  eiid0 [label="a: W[x]NT=1\lproc:P0 poi:0\lSTNP S0,S2,[X0]", shape="box", color="blue"];
  eiid1 [label="b: W[x+4]NT=0\lproc:P0 poi:0\lSTNP S0,S2,[X0]", shape="box", color="blue"];
  eiid4 [label="e: R0:X0q=x (addr)\lproc:P0 poi:0\lSTNP S0,S2,[X0]", shape="box", color="blue"];
  eiid5 [label="f: R0:V0s=1 (data)\lproc:P0 poi:0\lSTNP S0,S2,[X0]", shape="box", color="blue"];
  eiid6 [label="g: R0:V2s=0 (data)\lproc:P0 poi:0\lSTNP S0,S2,[X0]", shape="box", color="blue"];
  }
  
  /* the intra_causality_data edges */
  
  eiid4 -> eiid5 [label="iico_data", color="black", fontcolor="black"];
  eiid4 -> eiid6 [label="iico_data", color="black", fontcolor="black"];
  eiid5 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid6 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  
  /* the intra_causality_control edges */
  
  /* the poi edges */
  /* the rfmap edges */
  
  
  /* The viewed-before edges */
  }
  
  DOTEND STNP
  Test STNP Required
  States 1
  x={1,0};
  No
  Witnesses
  Positive: 0 Negative: 1
  Condition forall (x={1,2})
  Observation STNP Never 0 1
  Hash=42d44458fa8af14f45691658b9b94c8b
  
