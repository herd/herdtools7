Check Post-index STR dot output.

  $ herd7 -set-libdir ../libdir STR-postindex.litmus -show all -showevents all -showinitwrites false -o - | sed '/^Time/d'
  
  DOTBEGIN STR-postindex
  DOTCOM dot
  digraph G {
  
  
  /* legend */
  label="Test STR-postindex, Generic[withcatdep](Unknown)";
  
  
  /* the unlocked events */
  subgraph cluster_proc0 { rank=sink; label = "Thread 0"; color=magenta; shape=box;
  eiid0 [label="a: W[x]=1\lproc:P0 poi:0\lSTR W1,[X0],#4", shape="box", color="blue"];
  eiid3 [label="d: R0:X0q=x (addr)\lproc:P0 poi:0\lSTR W1,[X0],#4", shape="box", color="blue"];
  eiid4 [label="e: W0:X0q=x+4\lproc:P0 poi:0\lSTR W1,[X0],#4", shape="box", color="blue"];
  eiid5 [label="f: R0:X1q=1 (data)\lproc:P0 poi:0\lSTR W1,[X0],#4", shape="box", color="blue"];
  }
  
  /* the intra_causality_data edges */
  
  eiid3 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid3 -> eiid4 [label="iico_data", color="black", fontcolor="black"];
  eiid5 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  
  /* the intra_causality_control edges */
  
  /* the poi edges */
  /* the rfmap edges */
  
  
  /* The viewed-before edges */
  }
  
  DOTEND STR-postindex
  Test STR-postindex Required
  States 1
  0:X0=x+4; x={1,0};
  Ok
  Witnesses
  Positive: 1 Negative: 0
  Condition forall (true)
  Observation STR-postindex Always 1 0
  Hash=abf1f5d5aa6ce3d247a5c3d6fd20a00c
  
