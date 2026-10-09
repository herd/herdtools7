Check intrinsic dependencies for a simple LDR/STR with sync MTE
  $ herd7 -set-libdir ../libdir -variant mte,sync LDR-ok.litmus -show all -showevents all -showinitwrites false -o - | sed '/^Time/d'
  
  DOTBEGIN LDR-ok
  DOTCOM dot
  digraph G {
  
  
  /* legend */
  label="Test LDR-ok, Generic[withcatdep](Unknown)";
  
  
  /* the unlocked events */
  subgraph cluster_proc0 { rank=sink; label = "Thread 0"; color=magenta; shape=box;
  eiid0 [label="a: R[tag(x)]NExpq=:green\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid1 [label="b: R[x]=1\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid5 [label="f: R0:X1q=x:green (addr)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid6 [label="g: Branching(pred)(color)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid7 [label="h: W0:X0q=1\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  }
  
  /* the intra_causality_data edges */
  
  eiid0 -> eiid6 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid7 [label="iico_data", color="black", fontcolor="black"];
  eiid5 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid5 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  eiid5 -> eiid6 [label="iico_data", color="black", fontcolor="black"];
  
  /* the intra_causality_control edges */
  eiid6 -> eiid1 [label="iico_ctrl", color="grey", fontcolor="grey"];
  
  /* the poi edges */
  /* the rfmap edges */
  
  
  /* The viewed-before edges */
  }
  
  DOTEND LDR-ok
  Test LDR-ok Required
  States 1
  
  Ok
  Witnesses
  Positive: 1 Negative: 0
  Condition forall (true)
  Observation LDR-ok Always 1 0
  Hash=5594a3e46976179cd468fe456c95531b
  
  $ herd7 -set-libdir ../libdir -variant mte,sync LDR-TagCheckFault.litmus -show all -showevents all -showinitwrites false -o - | sed '/^Time/d'
  
  DOTBEGIN LDR-TagCheckFault
  DOTCOM dot
  digraph G {
  
  
  /* legend */
  label="Test LDR-TagCheckFault, Generic[withcatdep](Unknown)";
  
  
  /* the unlocked events */
  subgraph cluster_proc0 { rank=sink; label = "Thread 0"; color=magenta; shape=box;
  eiid0 [label="a: R[tag(x)]NExpq=:green\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid4 [label="e: R0:X1q=x:red (addr)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid5 [label="f: Branching(pred)(color)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid6 [label="g: ExcEntry(R,loc:x:red,TagCheck)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid7 [label="h: W0:ELR_EL1q=label:\"P0:L0\"\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  }
  
  /* the intra_causality_data edges */
  
  eiid0 -> eiid5 [label="iico_data", color="black", fontcolor="black"];
  eiid4 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid4 -> eiid5 [label="iico_data", color="black", fontcolor="black"];
  
  /* the intra_causality_control edges */
  eiid5 -> eiid6 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid5 -> eiid7 [label="iico_ctrl", color="grey", fontcolor="grey"];
  
  /* the poi edges */
  /* the rfmap edges */
  
  
  /* The viewed-before edges */
  }
  
  DOTEND LDR-TagCheckFault
  Test LDR-TagCheckFault Required
  States 1
  
  Ok
  Witnesses
  Positive: 1 Negative: 0
  Condition forall (true)
  Observation LDR-TagCheckFault Always 1 0
  Hash=4cb7f30a949f2145296982f821e145c5
  
  $ herd7 -set-libdir ../libdir -variant mte,sync STR-ok.litmus -show all -showevents all -showinitwrites false -o - | sed '/^Time/d'
  
  DOTBEGIN STR-ok
  DOTCOM dot
  digraph G {
  
  
  /* legend */
  label="Test STR-ok, Generic[withcatdep](Unknown)";
  
  
  /* the unlocked events */
  subgraph cluster_proc0 { rank=sink; label = "Thread 0"; color=magenta; shape=box;
  eiid5 [label="f: W0:X0q=1\lproc:P0 poi:0\lMOV W0,#1", shape="box", color="blue"];
  eiid0 [label="a: R[tag(x)]NExpq=:green\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid1 [label="b: W[x]=1\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid6 [label="g: R0:X1q=x:green (addr)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid7 [label="h: Branching(pred)(color)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid8 [label="i: R0:X0q=1 (data)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  }
  
  /* the intra_causality_data edges */
  
  eiid0 -> eiid7 [label="iico_data", color="black", fontcolor="black"];
  eiid6 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid6 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  eiid6 -> eiid7 [label="iico_data", color="black", fontcolor="black"];
  eiid8 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  
  /* the intra_causality_control edges */
  eiid7 -> eiid1 [label="iico_ctrl", color="grey", fontcolor="grey"];
  
  /* the poi edges */
  eiid5 -> eiid6 [label="po", color="black", fontcolor="black"];
  /* the rfmap edges */
  
  
  /* The viewed-before edges */
  eiid5 -> eiid8 [label="rf-reg", color="brown", fontcolor="brown"];
  }
  
  DOTEND STR-ok
  Test STR-ok Required
  States 1
  
  Ok
  Witnesses
  Positive: 1 Negative: 0
  Condition forall (true)
  Observation STR-ok Always 1 0
  Hash=c4b42ca87249e5438a8b67ee947357c7
  
  $ herd7 -set-libdir ../libdir -variant mte,sync STR-TagCheckFault.litmus -show all -showevents all -showinitwrites false -o - | sed '/^Time/d'
  
  DOTBEGIN STR-TagCheckFault
  DOTCOM dot
  digraph G {
  
  
  /* legend */
  label="Test STR-TagCheckFault, Generic[withcatdep](Unknown)";
  
  
  /* the unlocked events */
  subgraph cluster_proc0 { rank=sink; label = "Thread 0"; color=magenta; shape=box;
  eiid4 [label="e: W0:X0q=1\lproc:P0 poi:0\lMOV W0,#1", shape="box", color="blue"];
  eiid0 [label="a: R[tag(x)]NExpq=:green\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid5 [label="f: R0:X1q=x:red (addr)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid6 [label="g: Branching(pred)(color)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid7 [label="h: ExcEntry(W,loc:x:red,TagCheck)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid8 [label="i: W0:ELR_EL1q=label:\"P0:L0\"\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  }
  
  /* the intra_causality_data edges */
  
  eiid0 -> eiid6 [label="iico_data", color="black", fontcolor="black"];
  eiid5 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid5 -> eiid6 [label="iico_data", color="black", fontcolor="black"];
  
  /* the intra_causality_control edges */
  eiid6 -> eiid7 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid6 -> eiid8 [label="iico_ctrl", color="grey", fontcolor="grey"];
  
  /* the poi edges */
  eiid4 -> eiid5 [label="po", color="black", fontcolor="black"];
  /* the rfmap edges */
  
  
  /* The viewed-before edges */
  }
  
  DOTEND STR-TagCheckFault
  Test STR-TagCheckFault Required
  States 1
  
  Ok
  Witnesses
  Positive: 1 Negative: 0
  Condition forall (true)
  Observation STR-TagCheckFault Always 1 0
  Hash=29c2cc919dabe4d68e470f354c95e024
  

Check intrinsic dependencies for a simple LDR/STR with async MTE
  $ herd7 -set-libdir ../libdir -variant mte,async LDR-ok.litmus -show all -showevents all -showinitwrites false -o - | sed '/^Time/d'
  
  DOTBEGIN LDR-ok
  DOTCOM dot
  digraph G {
  
  
  /* legend */
  label="Test LDR-ok, Generic[withcatdep](Unknown)";
  
  
  /* the unlocked events */
  subgraph cluster_proc0 { rank=sink; label = "Thread 0"; color=magenta; shape=box;
  eiid0 [label="a: R[tag(x)]NExpq=:green\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid1 [label="b: R[x]=1\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid5 [label="f: R0:X1q=x:green (addr)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid6 [label="g: Branching(pred)(color)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid7 [label="h: Empty\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid8 [label="i: W0:X0q=1\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  }
  
  /* the intra_causality_data edges */
  
  eiid0 -> eiid6 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid8 [label="iico_data", color="black", fontcolor="black"];
  eiid5 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid5 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  eiid5 -> eiid6 [label="iico_data", color="black", fontcolor="black"];
  
  /* the intra_causality_control edges */
  eiid6 -> eiid7 [label="iico_ctrl", color="grey", fontcolor="grey"];
  
  /* the poi edges */
  /* the rfmap edges */
  
  
  /* The viewed-before edges */
  }
  
  DOTEND LDR-ok
  Test LDR-ok Required
  States 1
  
  Ok
  Witnesses
  Positive: 1 Negative: 0
  Condition forall (true)
  Observation LDR-ok Always 1 0
  Hash=5594a3e46976179cd468fe456c95531b
  
  $ herd7 -set-libdir ../libdir -variant mte,async LDR-TagCheckFault.litmus -show all -showevents all -showinitwrites false -o - | sed '/^Time/d'
  
  DOTBEGIN LDR-TagCheckFault
  DOTCOM dot
  digraph G {
  
  
  /* legend */
  label="Test LDR-TagCheckFault, Generic[withcatdep](Unknown)";
  
  
  /* the unlocked events */
  subgraph cluster_proc0 { rank=sink; label = "Thread 0"; color=magenta; shape=box;
  eiid0 [label="a: R[tag(x)]NExpq=:green\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid1 [label="b: R[x]=0\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid5 [label="f: R0:X1q=x:red (addr)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid6 [label="g: Branching(pred)(color)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid7 [label="h: W0:X0q=0\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid8 [label="i: W0:TFSR_ELxq=1\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid9 [label="j: Fault(R,TagCheck)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  }
  
  /* the intra_causality_data edges */
  
  eiid0 -> eiid6 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid7 [label="iico_data", color="black", fontcolor="black"];
  eiid5 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid5 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  eiid5 -> eiid6 [label="iico_data", color="black", fontcolor="black"];
  
  /* the intra_causality_control edges */
  eiid6 -> eiid8 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid6 -> eiid9 [label="iico_ctrl", color="grey", fontcolor="grey"];
  
  /* the poi edges */
  /* the rfmap edges */
  
  
  /* The viewed-before edges */
  }
  
  DOTEND LDR-TagCheckFault
  Test LDR-TagCheckFault Required
  States 1
  
  Ok
  Witnesses
  Positive: 1 Negative: 0
  Condition forall (true)
  Observation LDR-TagCheckFault Always 1 0
  Hash=4cb7f30a949f2145296982f821e145c5
  
  $ herd7 -set-libdir ../libdir -variant mte,async STR-ok.litmus -show all -showevents all -showinitwrites false -o - | sed '/^Time/d'
  
  DOTBEGIN STR-ok
  DOTCOM dot
  digraph G {
  
  
  /* legend */
  label="Test STR-ok, Generic[withcatdep](Unknown)";
  
  
  /* the unlocked events */
  subgraph cluster_proc0 { rank=sink; label = "Thread 0"; color=magenta; shape=box;
  eiid5 [label="f: W0:X0q=1\lproc:P0 poi:0\lMOV W0,#1", shape="box", color="blue"];
  eiid0 [label="a: R[tag(x)]NExpq=:green\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid1 [label="b: W[x]=1\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid6 [label="g: R0:X1q=x:green (addr)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid7 [label="h: Branching(pred)(color)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid8 [label="i: Empty\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid9 [label="j: R0:X0q=1 (data)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  }
  
  /* the intra_causality_data edges */
  
  eiid0 -> eiid7 [label="iico_data", color="black", fontcolor="black"];
  eiid6 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid6 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  eiid6 -> eiid7 [label="iico_data", color="black", fontcolor="black"];
  eiid9 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  
  /* the intra_causality_control edges */
  eiid7 -> eiid8 [label="iico_ctrl", color="grey", fontcolor="grey"];
  
  /* the poi edges */
  eiid5 -> eiid6 [label="po", color="black", fontcolor="black"];
  /* the rfmap edges */
  
  
  /* The viewed-before edges */
  eiid5 -> eiid9 [label="rf-reg", color="brown", fontcolor="brown"];
  }
  
  DOTEND STR-ok
  Test STR-ok Required
  States 1
  
  Ok
  Witnesses
  Positive: 1 Negative: 0
  Condition forall (true)
  Observation STR-ok Always 1 0
  Hash=c4b42ca87249e5438a8b67ee947357c7
  
  $ herd7 -set-libdir ../libdir -variant mte,async STR-TagCheckFault.litmus -show all -showevents all -showinitwrites false -o - | sed '/^Time/d'
  
  DOTBEGIN STR-TagCheckFault
  DOTCOM dot
  digraph G {
  
  
  /* legend */
  label="Test STR-TagCheckFault, Generic[withcatdep](Unknown)";
  
  
  /* the unlocked events */
  subgraph cluster_proc0 { rank=sink; label = "Thread 0"; color=magenta; shape=box;
  eiid5 [label="f: W0:X0q=1\lproc:P0 poi:0\lMOV W0,#1", shape="box", color="blue"];
  eiid0 [label="a: R[tag(x)]NExpq=:green\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid1 [label="b: W[x]=1\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid6 [label="g: R0:X1q=x:red (addr)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid7 [label="h: Branching(pred)(color)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid8 [label="i: R0:X0q=1 (data)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid9 [label="j: W0:TFSR_ELxq=1\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid10 [label="k: Fault(W,TagCheck)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  }
  
  /* the intra_causality_data edges */
  
  eiid0 -> eiid7 [label="iico_data", color="black", fontcolor="black"];
  eiid6 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid6 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  eiid6 -> eiid7 [label="iico_data", color="black", fontcolor="black"];
  eiid8 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  
  /* the intra_causality_control edges */
  eiid7 -> eiid9 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid7 -> eiid10 [label="iico_ctrl", color="grey", fontcolor="grey"];
  
  /* the poi edges */
  eiid5 -> eiid6 [label="po", color="black", fontcolor="black"];
  /* the rfmap edges */
  
  
  /* The viewed-before edges */
  eiid5 -> eiid8 [label="rf-reg", color="brown", fontcolor="brown"];
  }
  
  DOTEND STR-TagCheckFault
  Test STR-TagCheckFault Required
  States 1
  
  Ok
  Witnesses
  Positive: 1 Negative: 0
  Condition forall (true)
  Observation STR-TagCheckFault Always 1 0
  Hash=29c2cc919dabe4d68e470f354c95e024
  

Check intrinsic dependencies for a simple LDR/STR with sync MTE with VMSA semantics
  $ herd7 -set-libdir ../libdir -variant vmsa,mte,sync LDR-ok.litmus -show all -showevents all -showinitwrites false -o - | sed '/^Time/d'
  
  DOTBEGIN LDR-ok
  DOTCOM dot
  digraph G {
  
  
  /* legend */
  label="Test LDR-ok, Generic[withcatdep](Unknown)";
  
  
  /* the unlocked events */
  subgraph cluster_proc0 { rank=sink; label = "Thread 0"; color=magenta; shape=box;
  eiid0 [label="a: R[PTE(x)]NExpq=(oa:PA(x), attrs:(TaggedNormal))\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid1 [label="b: R[PTE(x)]NExpq=(oa:PA(x), attrs:(TaggedNormal))\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid2 [label="c: R[tag(PA(x))]NExpq=:green\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid3 [label="d: R[PA(x)]=1\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid7 [label="h: R0:X1q=x:green (addr)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid8 [label="i: Branching(pred)(Data, valid:1 && af:1)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid9 [label="j: R0:X1q=x:green (addr)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid10 [label="k: Branching(pred)(Tag, valid:1 && af:1)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid11 [label="l: Branching(pred)(color)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid12 [label="m: W0:X0q=1\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  }
  
  /* the intra_causality_data edges */
  
  eiid0 -> eiid3 [label="iico_data", color="black", fontcolor="black"];
  eiid0 -> eiid8 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid2 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid10 [label="iico_data", color="black", fontcolor="black"];
  eiid2 -> eiid11 [label="iico_data", color="black", fontcolor="black"];
  eiid3 -> eiid12 [label="iico_data", color="black", fontcolor="black"];
  eiid7 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid9 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  eiid9 -> eiid11 [label="iico_data", color="black", fontcolor="black"];
  
  /* the intra_causality_control edges */
  eiid8 -> eiid3 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid10 -> eiid2 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid11 -> eiid3 [label="iico_ctrl", color="grey", fontcolor="grey"];
  
  /* the poi edges */
  /* the rfmap edges */
  
  
  /* The viewed-before edges */
  }
  
  DOTEND LDR-ok
  Test LDR-ok Required
  States 1
  
  Ok
  Witnesses
  Positive: 1 Negative: 0
  Flag combining-vmsa-and-memtag-is-not-supported
  Condition forall (true)
  Observation LDR-ok Always 1 0
  Hash=5594a3e46976179cd468fe456c95531b
  
  $ herd7 -set-libdir ../libdir -variant vmsa,mte,sync LDR-TagCheckFault.litmus -show all -showevents all -showinitwrites false -o - | sed '/^Time/d'
  
  DOTBEGIN LDR-TagCheckFault
  DOTCOM dot
  digraph G {
  
  
  /* legend */
  label="Test LDR-TagCheckFault, Generic[withcatdep](Unknown)";
  
  
  /* the unlocked events */
  subgraph cluster_proc0 { rank=sink; label = "Thread 0"; color=magenta; shape=box;
  eiid0 [label="a: R[PTE(x)]NExpq=(oa:PA(x), attrs:(TaggedNormal))\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid1 [label="b: R[PTE(x)]NExpq=(oa:PA(x), attrs:(TaggedNormal))\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid2 [label="c: R[tag(PA(x))]NExpq=:green\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid6 [label="g: R0:X1q=x:red (addr)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid7 [label="h: Branching(pred)(Data, valid:1 && af:1)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid8 [label="i: R0:X1q=x:red (addr)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid9 [label="j: Branching(pred)(Tag, valid:1 && af:1)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid10 [label="k: Branching(pred)(color)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid11 [label="l: ExcEntry(R,loc:x:red,TagCheck)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid12 [label="m: W0:ELR_EL1q=label:\"P0:L0\"\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  }
  
  /* the intra_causality_data edges */
  
  eiid0 -> eiid7 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid2 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid9 [label="iico_data", color="black", fontcolor="black"];
  eiid2 -> eiid10 [label="iico_data", color="black", fontcolor="black"];
  eiid6 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid8 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  eiid8 -> eiid10 [label="iico_data", color="black", fontcolor="black"];
  
  /* the intra_causality_control edges */
  eiid7 -> eiid11 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid7 -> eiid12 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid9 -> eiid2 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid10 -> eiid11 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid10 -> eiid12 [label="iico_ctrl", color="grey", fontcolor="grey"];
  
  /* the poi edges */
  /* the rfmap edges */
  
  
  /* The viewed-before edges */
  }
  
  DOTEND LDR-TagCheckFault
  Test LDR-TagCheckFault Required
  States 1
  
  Ok
  Witnesses
  Positive: 1 Negative: 0
  Flag combining-vmsa-and-memtag-is-not-supported
  Condition forall (true)
  Observation LDR-TagCheckFault Always 1 0
  Hash=4cb7f30a949f2145296982f821e145c5
  
  $ herd7 -set-libdir ../libdir -variant vmsa,mte,sync STR-ok.litmus -show all -showevents all -showinitwrites false -o - | sed '/^Time/d'
  
  DOTBEGIN STR-ok
  DOTCOM dot
  digraph G {
  
  
  /* legend */
  label="Test STR-ok, Generic[withcatdep](Unknown)";
  
  
  /* the unlocked events */
  subgraph cluster_proc0 { rank=sink; label = "Thread 0"; color=magenta; shape=box;
  eiid7 [label="h: W0:X0q=1\lproc:P0 poi:0\lMOV W0,#1", shape="box", color="blue"];
  eiid0 [label="a: R[PTE(x)]NExpq=(oa:PA(x), attrs:(TaggedNormal))\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid1 [label="b: R[PTE(x)]NExpq=(oa:PA(x), attrs:(TaggedNormal))\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid2 [label="c: R[tag(PA(x))]NExpq=:green\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid3 [label="d: W[PA(x)]=1\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid8 [label="i: R0:X1q=x:green (addr)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid9 [label="j: Branching(pred)(Data, valid:1 && af:1 && db:1)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid10 [label="k: R0:X1q=x:green (addr)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid11 [label="l: Branching(pred)(Tag, valid:1 && af:1)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid12 [label="m: Branching(pred)(color)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid13 [label="n: R0:X0q=1 (data)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  }
  
  /* the intra_causality_data edges */
  
  eiid0 -> eiid3 [label="iico_data", color="black", fontcolor="black"];
  eiid0 -> eiid9 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid2 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid11 [label="iico_data", color="black", fontcolor="black"];
  eiid2 -> eiid12 [label="iico_data", color="black", fontcolor="black"];
  eiid8 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid10 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  eiid10 -> eiid12 [label="iico_data", color="black", fontcolor="black"];
  eiid13 -> eiid3 [label="iico_data", color="black", fontcolor="black"];
  
  /* the intra_causality_control edges */
  eiid9 -> eiid3 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid11 -> eiid2 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid12 -> eiid3 [label="iico_ctrl", color="grey", fontcolor="grey"];
  
  /* the poi edges */
  eiid7 -> eiid8 [label="po", color="black", fontcolor="black"];
  eiid7 -> eiid10 [label="po", color="black", fontcolor="black"];
  /* the rfmap edges */
  
  
  /* The viewed-before edges */
  eiid7 -> eiid13 [label="rf-reg", color="brown", fontcolor="brown"];
  }
  
  DOTEND STR-ok
  Test STR-ok Required
  States 1
  
  Ok
  Witnesses
  Positive: 1 Negative: 0
  Flag combining-vmsa-and-memtag-is-not-supported
  Condition forall (true)
  Observation STR-ok Always 1 0
  Hash=c4b42ca87249e5438a8b67ee947357c7
  
  $ herd7 -set-libdir ../libdir -variant vmsa,mte,sync STR-TagCheckFault.litmus -show all -showevents all -showinitwrites false -o - | sed '/^Time/d'
  
  DOTBEGIN STR-TagCheckFault
  DOTCOM dot
  digraph G {
  
  
  /* legend */
  label="Test STR-TagCheckFault, Generic[withcatdep](Unknown)";
  
  
  /* the unlocked events */
  subgraph cluster_proc0 { rank=sink; label = "Thread 0"; color=magenta; shape=box;
  eiid6 [label="g: W0:X0q=1\lproc:P0 poi:0\lMOV W0,#1", shape="box", color="blue"];
  eiid0 [label="a: R[PTE(x)]NExpq=(oa:PA(x), attrs:(TaggedNormal))\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid1 [label="b: R[PTE(x)]NExpq=(oa:PA(x), attrs:(TaggedNormal))\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid2 [label="c: R[tag(PA(x))]NExpq=:green\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid7 [label="h: R0:X1q=x:red (addr)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid8 [label="i: Branching(pred)(Data, valid:1 && af:1 && db:1)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid9 [label="j: R0:X1q=x:red (addr)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid10 [label="k: Branching(pred)(Tag, valid:1 && af:1)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid11 [label="l: Branching(pred)(color)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid12 [label="m: ExcEntry(W,loc:x:red,TagCheck)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid13 [label="n: W0:ELR_EL1q=label:\"P0:L0\"\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  }
  
  /* the intra_causality_data edges */
  
  eiid0 -> eiid8 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid2 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid10 [label="iico_data", color="black", fontcolor="black"];
  eiid2 -> eiid11 [label="iico_data", color="black", fontcolor="black"];
  eiid7 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid9 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  eiid9 -> eiid11 [label="iico_data", color="black", fontcolor="black"];
  
  /* the intra_causality_control edges */
  eiid8 -> eiid12 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid8 -> eiid13 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid10 -> eiid2 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid11 -> eiid12 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid11 -> eiid13 [label="iico_ctrl", color="grey", fontcolor="grey"];
  
  /* the poi edges */
  eiid6 -> eiid7 [label="po", color="black", fontcolor="black"];
  eiid6 -> eiid9 [label="po", color="black", fontcolor="black"];
  /* the rfmap edges */
  
  
  /* The viewed-before edges */
  }
  
  DOTEND STR-TagCheckFault
  Test STR-TagCheckFault Required
  States 1
  
  Ok
  Witnesses
  Positive: 1 Negative: 0
  Flag combining-vmsa-and-memtag-is-not-supported
  Condition forall (true)
  Observation STR-TagCheckFault Always 1 0
  Hash=29c2cc919dabe4d68e470f354c95e024
  
  $ herd7 -set-libdir ../libdir -variant vmsa,mte,sync LDR-TranslationFault.litmus -show all -showevents all -showinitwrites false -o - | sed '/^Time/d'
  
  DOTBEGIN LDR-TranslationFault
  DOTCOM dot
  digraph G {
  
  
  /* legend */
  label="Test LDR-TranslationFault, Generic[withcatdep](Unknown)";
  
  
  /* the unlocked events */
  subgraph cluster_proc0 { rank=sink; label = "Thread 0"; color=magenta; shape=box;
  eiid0 [label="a: R[PTE(x)]NExpq=(oa:PA(x), valid:0, attrs:(TaggedNormal))\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid4 [label="e: R0:X1q=x:red (addr)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid5 [label="f: Branching(pred)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid6 [label="g: W0:ELR_EL1q=label:\"P0:L0\"\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid7 [label="h: ExcEntry(R,loc:x:red,D-MMU:Translation)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  }
  
  /* the intra_causality_data edges */
  
  eiid0 -> eiid5 [label="iico_data", color="black", fontcolor="black"];
  eiid4 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  
  /* the intra_causality_control edges */
  eiid5 -> eiid6 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid5 -> eiid7 [label="iico_ctrl", color="grey", fontcolor="grey"];
  
  /* the poi edges */
  /* the rfmap edges */
  
  
  /* The viewed-before edges */
  }
  
  DOTEND LDR-TranslationFault
  Test LDR-TranslationFault Required
  States 1
  
  Ok
  Witnesses
  Positive: 1 Negative: 0
  Flag combining-vmsa-and-memtag-is-not-supported
  Condition forall (true)
  Observation LDR-TranslationFault Always 1 0
  Hash=1be524ca9d5d0520243d8555aaf58429
  
  $ herd7 -set-libdir ../libdir -variant vmsa,mte,sync STR-PermissionFault.litmus -show all -showevents all -showinitwrites false -o - | sed '/^Time/d'
  
  DOTBEGIN STR-PermissionFault
  DOTCOM dot
  digraph G {
  
  
  /* legend */
  label="Test STR-PermissonFault, Generic[withcatdep](Unknown)";
  
  
  /* the unlocked events */
  subgraph cluster_proc0 { rank=sink; label = "Thread 0"; color=magenta; shape=box;
  eiid4 [label="e: W0:X0q=1\lproc:P0 poi:0\lMOV W0,#1", shape="box", color="blue"];
  eiid0 [label="a: R[PTE(x)]NExpq=(oa:PA(x), db:0, attrs:(TaggedNormal))\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid5 [label="f: R0:X1q=x:red (addr)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid6 [label="g: Branching(pred)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid7 [label="h: W0:ELR_EL1q=label:\"P0:L0\"\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid8 [label="i: ExcEntry(W,loc:x:red,D-MMU:Permission)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  }
  
  /* the intra_causality_data edges */
  
  eiid0 -> eiid6 [label="iico_data", color="black", fontcolor="black"];
  eiid5 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  
  /* the intra_causality_control edges */
  eiid6 -> eiid7 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid6 -> eiid8 [label="iico_ctrl", color="grey", fontcolor="grey"];
  
  /* the poi edges */
  eiid4 -> eiid5 [label="po", color="black", fontcolor="black"];
  /* the rfmap edges */
  
  
  /* The viewed-before edges */
  }
  
  DOTEND STR-PermissionFault
  Test STR-PermissonFault Required
  States 1
  
  Ok
  Witnesses
  Positive: 1 Negative: 0
  Flag combining-vmsa-and-memtag-is-not-supported
  Condition forall (true)
  Observation STR-PermissonFault Always 1 0
  Hash=4f1e9775050dbd48535fa531e20bc527
  

Check intrinsic dependencies for a simple LDR/STR with async MTE with VMSA semantics
  $ herd7 -set-libdir ../libdir -variant vmsa,mte,async LDR-ok.litmus -show all -showevents all -showinitwrites false -o - | sed '/^Time/d'
  
  DOTBEGIN LDR-ok
  DOTCOM dot
  digraph G {
  
  
  /* legend */
  label="Test LDR-ok, Generic[withcatdep](Unknown)";
  
  
  /* the unlocked events */
  subgraph cluster_proc0 { rank=sink; label = "Thread 0"; color=magenta; shape=box;
  eiid0 [label="a: R[PTE(x)]NExpq=(oa:PA(x), attrs:(TaggedNormal))\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid1 [label="b: R[PTE(x)]NExpq=(oa:PA(x), attrs:(TaggedNormal))\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid2 [label="c: R[tag(PA(x))]NExpq=:green\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid3 [label="d: R[PA(x)]=1\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid7 [label="h: R0:X1q=x:green (addr)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid8 [label="i: Branching(pred)(Data, valid:1 && af:1)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid9 [label="j: R0:X1q=x:green (addr)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid10 [label="k: Branching(pred)(Tag, valid:1 && af:1)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid11 [label="l: Branching(pred)(color)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid12 [label="m: Empty\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid13 [label="n: W0:X0q=1\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  }
  
  /* the intra_causality_data edges */
  
  eiid0 -> eiid3 [label="iico_data", color="black", fontcolor="black"];
  eiid0 -> eiid8 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid2 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid10 [label="iico_data", color="black", fontcolor="black"];
  eiid2 -> eiid11 [label="iico_data", color="black", fontcolor="black"];
  eiid3 -> eiid13 [label="iico_data", color="black", fontcolor="black"];
  eiid7 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid9 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  eiid9 -> eiid11 [label="iico_data", color="black", fontcolor="black"];
  
  /* the intra_causality_control edges */
  eiid8 -> eiid3 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid10 -> eiid2 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid11 -> eiid12 [label="iico_ctrl", color="grey", fontcolor="grey"];
  
  /* the poi edges */
  /* the rfmap edges */
  
  
  /* The viewed-before edges */
  }
  
  DOTEND LDR-ok
  Test LDR-ok Required
  States 1
  
  Ok
  Witnesses
  Positive: 1 Negative: 0
  Flag combining-vmsa-and-memtag-is-not-supported
  Condition forall (true)
  Observation LDR-ok Always 1 0
  Hash=5594a3e46976179cd468fe456c95531b
  
  $ herd7 -set-libdir ../libdir -variant vmsa,mte,async LDR-TagCheckFault.litmus -show all -showevents all -showinitwrites false -o - | sed '/^Time/d'
  
  DOTBEGIN LDR-TagCheckFault
  DOTCOM dot
  digraph G {
  
  
  /* legend */
  label="Test LDR-TagCheckFault, Generic[withcatdep](Unknown)";
  
  
  /* the unlocked events */
  subgraph cluster_proc0 { rank=sink; label = "Thread 0"; color=magenta; shape=box;
  eiid0 [label="a: R[PTE(x)]NExpq=(oa:PA(x), attrs:(TaggedNormal))\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid1 [label="b: R[PTE(x)]NExpq=(oa:PA(x), attrs:(TaggedNormal))\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid2 [label="c: R[tag(PA(x))]NExpq=:green\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid3 [label="d: R[PA(x)]=0\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid7 [label="h: R0:X1q=x:red (addr)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid8 [label="i: Branching(pred)(Data, valid:1 && af:1)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid9 [label="j: R0:X1q=x:red (addr)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid10 [label="k: Branching(pred)(Tag, valid:1 && af:1)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid11 [label="l: Branching(pred)(color)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid12 [label="m: W0:X0q=0\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid13 [label="n: W0:TFSR_ELxq=1\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid14 [label="o: Fault(R,TagCheck)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  }
  
  /* the intra_causality_data edges */
  
  eiid0 -> eiid3 [label="iico_data", color="black", fontcolor="black"];
  eiid0 -> eiid8 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid2 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid10 [label="iico_data", color="black", fontcolor="black"];
  eiid2 -> eiid11 [label="iico_data", color="black", fontcolor="black"];
  eiid3 -> eiid12 [label="iico_data", color="black", fontcolor="black"];
  eiid7 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid9 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  eiid9 -> eiid11 [label="iico_data", color="black", fontcolor="black"];
  
  /* the intra_causality_control edges */
  eiid8 -> eiid3 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid10 -> eiid2 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid11 -> eiid13 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid11 -> eiid14 [label="iico_ctrl", color="grey", fontcolor="grey"];
  
  /* the poi edges */
  /* the rfmap edges */
  
  
  /* The viewed-before edges */
  }
  
  DOTEND LDR-TagCheckFault
  Test LDR-TagCheckFault Required
  States 1
  
  Ok
  Witnesses
  Positive: 1 Negative: 0
  Flag combining-vmsa-and-memtag-is-not-supported
  Condition forall (true)
  Observation LDR-TagCheckFault Always 1 0
  Hash=4cb7f30a949f2145296982f821e145c5
  
  $ herd7 -set-libdir ../libdir -variant vmsa,mte,async STR-ok.litmus -show all -showevents all -showinitwrites false -o - | sed '/^Time/d'
  
  DOTBEGIN STR-ok
  DOTCOM dot
  digraph G {
  
  
  /* legend */
  label="Test STR-ok, Generic[withcatdep](Unknown)";
  
  
  /* the unlocked events */
  subgraph cluster_proc0 { rank=sink; label = "Thread 0"; color=magenta; shape=box;
  eiid7 [label="h: W0:X0q=1\lproc:P0 poi:0\lMOV W0,#1", shape="box", color="blue"];
  eiid0 [label="a: R[PTE(x)]NExpq=(oa:PA(x), attrs:(TaggedNormal))\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid1 [label="b: R[PTE(x)]NExpq=(oa:PA(x), attrs:(TaggedNormal))\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid2 [label="c: R[tag(PA(x))]NExpq=:green\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid3 [label="d: W[PA(x)]=1\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid8 [label="i: R0:X1q=x:green (addr)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid9 [label="j: Branching(pred)(Data, valid:1 && af:1 && db:1)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid10 [label="k: R0:X1q=x:green (addr)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid11 [label="l: Branching(pred)(Tag, valid:1 && af:1)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid12 [label="m: Branching(pred)(color)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid13 [label="n: Empty\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid14 [label="o: R0:X0q=1 (data)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  }
  
  /* the intra_causality_data edges */
  
  eiid0 -> eiid3 [label="iico_data", color="black", fontcolor="black"];
  eiid0 -> eiid9 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid2 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid11 [label="iico_data", color="black", fontcolor="black"];
  eiid2 -> eiid12 [label="iico_data", color="black", fontcolor="black"];
  eiid8 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid10 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  eiid10 -> eiid12 [label="iico_data", color="black", fontcolor="black"];
  eiid14 -> eiid3 [label="iico_data", color="black", fontcolor="black"];
  
  /* the intra_causality_control edges */
  eiid9 -> eiid3 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid11 -> eiid2 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid12 -> eiid13 [label="iico_ctrl", color="grey", fontcolor="grey"];
  
  /* the poi edges */
  eiid7 -> eiid8 [label="po", color="black", fontcolor="black"];
  eiid7 -> eiid10 [label="po", color="black", fontcolor="black"];
  /* the rfmap edges */
  
  
  /* The viewed-before edges */
  eiid7 -> eiid14 [label="rf-reg", color="brown", fontcolor="brown"];
  }
  
  DOTEND STR-ok
  Test STR-ok Required
  States 1
  
  Ok
  Witnesses
  Positive: 1 Negative: 0
  Flag combining-vmsa-and-memtag-is-not-supported
  Condition forall (true)
  Observation STR-ok Always 1 0
  Hash=c4b42ca87249e5438a8b67ee947357c7
  
  $ herd7 -set-libdir ../libdir -variant vmsa,mte,async STR-TagCheckFault.litmus -show all -showevents all -showinitwrites false -o - | sed '/^Time/d'
  
  DOTBEGIN STR-TagCheckFault
  DOTCOM dot
  digraph G {
  
  
  /* legend */
  label="Test STR-TagCheckFault, Generic[withcatdep](Unknown)";
  
  
  /* the unlocked events */
  subgraph cluster_proc0 { rank=sink; label = "Thread 0"; color=magenta; shape=box;
  eiid7 [label="h: W0:X0q=1\lproc:P0 poi:0\lMOV W0,#1", shape="box", color="blue"];
  eiid0 [label="a: R[PTE(x)]NExpq=(oa:PA(x), attrs:(TaggedNormal))\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid1 [label="b: R[PTE(x)]NExpq=(oa:PA(x), attrs:(TaggedNormal))\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid2 [label="c: R[tag(PA(x))]NExpq=:green\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid3 [label="d: W[PA(x)]=1\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid8 [label="i: R0:X1q=x:red (addr)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid9 [label="j: Branching(pred)(Data, valid:1 && af:1 && db:1)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid10 [label="k: R0:X1q=x:red (addr)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid11 [label="l: Branching(pred)(Tag, valid:1 && af:1)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid12 [label="m: Branching(pred)(color)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid13 [label="n: R0:X0q=1 (data)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid14 [label="o: W0:TFSR_ELxq=1\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid15 [label="p: Fault(W,TagCheck)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  }
  
  /* the intra_causality_data edges */
  
  eiid0 -> eiid3 [label="iico_data", color="black", fontcolor="black"];
  eiid0 -> eiid9 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid2 [label="iico_data", color="black", fontcolor="black"];
  eiid1 -> eiid11 [label="iico_data", color="black", fontcolor="black"];
  eiid2 -> eiid12 [label="iico_data", color="black", fontcolor="black"];
  eiid8 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  eiid10 -> eiid1 [label="iico_data", color="black", fontcolor="black"];
  eiid10 -> eiid12 [label="iico_data", color="black", fontcolor="black"];
  eiid13 -> eiid3 [label="iico_data", color="black", fontcolor="black"];
  
  /* the intra_causality_control edges */
  eiid9 -> eiid3 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid11 -> eiid2 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid12 -> eiid14 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid12 -> eiid15 [label="iico_ctrl", color="grey", fontcolor="grey"];
  
  /* the poi edges */
  eiid7 -> eiid8 [label="po", color="black", fontcolor="black"];
  eiid7 -> eiid10 [label="po", color="black", fontcolor="black"];
  /* the rfmap edges */
  
  
  /* The viewed-before edges */
  eiid7 -> eiid13 [label="rf-reg", color="brown", fontcolor="brown"];
  }
  
  DOTEND STR-TagCheckFault
  Test STR-TagCheckFault Required
  States 1
  
  Ok
  Witnesses
  Positive: 1 Negative: 0
  Flag combining-vmsa-and-memtag-is-not-supported
  Condition forall (true)
  Observation STR-TagCheckFault Always 1 0
  Hash=29c2cc919dabe4d68e470f354c95e024
  
  $ herd7 -set-libdir ../libdir -variant vmsa,mte,async LDR-TranslationFault.litmus -show all -showevents all -showinitwrites false -o - | sed '/^Time/d'
  
  DOTBEGIN LDR-TranslationFault
  DOTCOM dot
  digraph G {
  
  
  /* legend */
  label="Test LDR-TranslationFault, Generic[withcatdep](Unknown)";
  
  
  /* the unlocked events */
  subgraph cluster_proc0 { rank=sink; label = "Thread 0"; color=magenta; shape=box;
  eiid0 [label="a: R[PTE(x)]NExpq=(oa:PA(x), valid:0, attrs:(TaggedNormal))\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid4 [label="e: R0:X1q=x:red (addr)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid5 [label="f: Branching(pred)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid6 [label="g: W0:ELR_EL1q=label:\"P0:L0\"\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid7 [label="h: ExcEntry(R,loc:x:red,D-MMU:Translation)\lproc:P0 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  }
  
  /* the intra_causality_data edges */
  
  eiid0 -> eiid5 [label="iico_data", color="black", fontcolor="black"];
  eiid4 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  
  /* the intra_causality_control edges */
  eiid5 -> eiid6 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid5 -> eiid7 [label="iico_ctrl", color="grey", fontcolor="grey"];
  
  /* the poi edges */
  /* the rfmap edges */
  
  
  /* The viewed-before edges */
  }
  
  DOTEND LDR-TranslationFault
  Test LDR-TranslationFault Required
  States 1
  
  Ok
  Witnesses
  Positive: 1 Negative: 0
  Flag combining-vmsa-and-memtag-is-not-supported
  Condition forall (true)
  Observation LDR-TranslationFault Always 1 0
  Hash=1be524ca9d5d0520243d8555aaf58429
  
  $ herd7 -set-libdir ../libdir -variant vmsa,mte,async STR-PermissionFault.litmus -show all -showevents all -showinitwrites false -o - | sed '/^Time/d'
  
  DOTBEGIN STR-PermissionFault
  DOTCOM dot
  digraph G {
  
  
  /* legend */
  label="Test STR-PermissonFault, Generic[withcatdep](Unknown)";
  
  
  /* the unlocked events */
  subgraph cluster_proc0 { rank=sink; label = "Thread 0"; color=magenta; shape=box;
  eiid4 [label="e: W0:X0q=1\lproc:P0 poi:0\lMOV W0,#1", shape="box", color="blue"];
  eiid0 [label="a: R[PTE(x)]NExpq=(oa:PA(x), db:0, attrs:(TaggedNormal))\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid5 [label="f: R0:X1q=x:red (addr)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid6 [label="g: Branching(pred)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid7 [label="h: W0:ELR_EL1q=label:\"P0:L0\"\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid8 [label="i: ExcEntry(W,loc:x:red,D-MMU:Permission)\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  }
  
  /* the intra_causality_data edges */
  
  eiid0 -> eiid6 [label="iico_data", color="black", fontcolor="black"];
  eiid5 -> eiid0 [label="iico_data", color="black", fontcolor="black"];
  
  /* the intra_causality_control edges */
  eiid6 -> eiid7 [label="iico_ctrl", color="grey", fontcolor="grey"];
  eiid6 -> eiid8 [label="iico_ctrl", color="grey", fontcolor="grey"];
  
  /* the poi edges */
  eiid4 -> eiid5 [label="po", color="black", fontcolor="black"];
  /* the rfmap edges */
  
  
  /* The viewed-before edges */
  }
  
  DOTEND STR-PermissionFault
  Test STR-PermissonFault Required
  States 1
  
  Ok
  Witnesses
  Positive: 1 Negative: 0
  Flag combining-vmsa-and-memtag-is-not-supported
  Condition forall (true)
  Observation STR-PermissonFault Always 1 0
  Hash=4f1e9775050dbd48535fa531e20bc527
  
