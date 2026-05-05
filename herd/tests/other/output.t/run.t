Check that duplicate tests with the same hash are only reported once.

  $ herd7 -set-libdir ../libdir MP.litmus MP.litmus | grep '^Test '
  Test MP Allowed

Check correct handling of outputdir, stdout, and suffix.

  $ rm -rf out-graph
  $ mkdir out-graph
  $ herd7 -set-libdir ../libdir MP.litmus -show prop -o out-graph -suffix .graph | grep '^DOT' || echo 'No dot on stdout'
  No dot on stdout
  $ ls out-graph
  MP.graph.dot
  $ grep '^label=' out-graph/MP.graph.dot
  label="Test MP, Generic[withcatdep](Unknown)";

Check dot label output.

  $ herd7 -set-libdir ../libdir MP.litmus -show prop -o - -shortlegend true | grep '^label='
  label="MP";
  $ herd7 -set-libdir ../libdir MP.litmus -show prop -o - -showkind true | grep '^label='
  label="Test MP: Allowed (Generic[withcatdep](Unknown))";

Check dot printing when no executions is shown.

  $ herd7 -set-libdir ../libdir MP.litmus -show none -o - | grep -E '^(DOTBEGIN|DOTEND|digraph G|Test |Hash=)'
  DOTBEGIN MP
  DOTEND MP
  Test MP Allowed
  Hash=211d5b298572012a0869d4ded6a40b7f

Check candidates output.

  $ herd7 -set-libdir ../libdir MP.litmus -candidates true | sed -n '/^Candidates/p'
  Candidates MP 4

Exercise condition-kind-dependent show logic in the result iterator.

  $ herd7 -set-libdir ../libdir MP-forall.litmus -show cond -o - | grep -c '^digraph G'
  1
  $ herd7 -set-libdir ../libdir MP-forall.litmus -show wit -o - | grep -c '^digraph G'
  3
  $ herd7 -set-libdir ../libdir MP-notexists.litmus -show cond -o - | grep -c '^digraph G'
  3
  $ herd7 -set-libdir ../libdir MP-notexists.litmus -show wit -o - | grep -c '^digraph G'
  1

Check -show prop output of MP test.

  $ herd7 -set-libdir ../libdir MP.litmus -show prop -o - -debug pretty 2>&1 | sed '/^Time/d'
  
  DOTBEGIN MP
  DOTCOM dot
  digraph G {
  
  
  /* legend */
  label="Test MP, Generic[withcatdep](Unknown)";
  
  
  /* init events */
  eiid4 [label="e: W[y]=0\lInit", shape="box", color="blue"];
  eiid5 [label="f: W[x]=0\lInit", shape="box", color="blue"];
  
  /* the unlocked events */
  subgraph cluster_proc0 { rank=sink; label = "Thread 0"; color=magenta; shape=box;
  eiid0 [label="a: W[x]=1\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid1 [label="b: W[y]=1\lproc:P0 poi:3\lSTR W2,[X3]", shape="box", color="blue"];
  }
  subgraph cluster_proc1 { rank=sink; label = "Thread 1"; color=magenta; shape=box;
  eiid2 [label="c: R[y]=1\lproc:P1 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid3 [label="d: R[x]=0\lproc:P1 poi:1\lLDR W2,[X3]", shape="box", color="blue"];
  }
  
  /* the intra_causality_data edges */
  
  
  /* the intra_causality_control edges */
  
  /* the poi edges */
  eiid0 -> eiid1 [label="po", color="black", fontcolor="black"];
  eiid2 -> eiid3 [label="po", color="black", fontcolor="black"];
  /* the rfmap edges */
  
  
  /* The viewed-before edges */
  eiid1 -> eiid2 [label="rf", color="red", fontcolor="red"];
  eiid5 -> eiid3 [label="rf", color="red", fontcolor="red"];
  eiid3 -> eiid0 [label="ca", color="blue", fontcolor="blue"];
  eiid4 -> eiid1 [label="ca", color="blue", fontcolor="blue"];
  eiid5 -> eiid0 [label="ca", color="blue", fontcolor="blue"];
  }
  
  DOTEND MP
  Test MP Allowed
  States 4
  1:X0=0; 1:X2=0;
  1:X0=0; 1:X2=1;
  1:X0=1; 1:X2=0;
  1:X0=1; 1:X2=1;
  Ok
  Witnesses
  Positive: 1 Negative: 3
  Condition exists (1:X0=1 /\ 1:X2=0)
  Observation MP Sometimes 1 3
  Hash=211d5b298572012a0869d4ded6a40b7f
  
  show MP file

  $ herd7 -set-libdir ../libdir MP.litmus -show neg -o - -debug pretty 2>&1 | sed '/^Time/d'
  
  DOTBEGIN MP
  DOTCOM dot
  digraph G {
  
  
  /* legend */
  label="Test MP, Generic[withcatdep](Unknown)";
  
  
  /* init events */
  eiid4 [label="e: W[y]=0\lInit", shape="box", color="blue"];
  eiid5 [label="f: W[x]=0\lInit", shape="box", color="blue"];
  
  /* the unlocked events */
  subgraph cluster_proc0 { rank=sink; label = "Thread 0"; color=magenta; shape=box;
  eiid0 [label="a: W[x]=1\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid1 [label="b: W[y]=1\lproc:P0 poi:3\lSTR W2,[X3]", shape="box", color="blue"];
  }
  subgraph cluster_proc1 { rank=sink; label = "Thread 1"; color=magenta; shape=box;
  eiid2 [label="c: R[y]=0\lproc:P1 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid3 [label="d: R[x]=0\lproc:P1 poi:1\lLDR W2,[X3]", shape="box", color="blue"];
  }
  
  /* the intra_causality_data edges */
  
  
  /* the intra_causality_control edges */
  
  /* the poi edges */
  eiid0 -> eiid1 [label="po", color="black", fontcolor="black"];
  eiid2 -> eiid3 [label="po", color="black", fontcolor="black"];
  /* the rfmap edges */
  
  
  /* The viewed-before edges */
  eiid4 -> eiid2 [label="rf", color="red", fontcolor="red"];
  eiid5 -> eiid3 [label="rf", color="red", fontcolor="red"];
  eiid2 -> eiid1 [label="ca", color="blue", fontcolor="blue"];
  eiid3 -> eiid0 [label="ca", color="blue", fontcolor="blue"];
  eiid4 -> eiid1 [label="ca", color="blue", fontcolor="blue"];
  eiid5 -> eiid0 [label="ca", color="blue", fontcolor="blue"];
  }
  digraph G {
  
  
  /* legend */
  label="Test MP, Generic[withcatdep](Unknown)";
  
  
  /* init events */
  eiid4 [label="e: W[y]=0\lInit", shape="box", color="blue"];
  eiid5 [label="f: W[x]=0\lInit", shape="box", color="blue"];
  
  /* the unlocked events */
  subgraph cluster_proc0 { rank=sink; label = "Thread 0"; color=magenta; shape=box;
  eiid0 [label="a: W[x]=1\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid1 [label="b: W[y]=1\lproc:P0 poi:3\lSTR W2,[X3]", shape="box", color="blue"];
  }
  subgraph cluster_proc1 { rank=sink; label = "Thread 1"; color=magenta; shape=box;
  eiid2 [label="c: R[y]=0\lproc:P1 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid3 [label="d: R[x]=1\lproc:P1 poi:1\lLDR W2,[X3]", shape="box", color="blue"];
  }
  
  /* the intra_causality_data edges */
  
  
  /* the intra_causality_control edges */
  
  /* the poi edges */
  eiid0 -> eiid1 [label="po", color="black", fontcolor="black"];
  eiid2 -> eiid3 [label="po", color="black", fontcolor="black"];
  /* the rfmap edges */
  
  
  /* The viewed-before edges */
  eiid0 -> eiid3 [label="rf", color="red", fontcolor="red"];
  eiid4 -> eiid2 [label="rf", color="red", fontcolor="red"];
  eiid2 -> eiid1 [label="ca", color="blue", fontcolor="blue"];
  eiid4 -> eiid1 [label="ca", color="blue", fontcolor="blue"];
  eiid5 -> eiid0 [label="ca", color="blue", fontcolor="blue"];
  }
  digraph G {
  
  
  /* legend */
  label="Test MP, Generic[withcatdep](Unknown)";
  
  
  /* init events */
  eiid4 [label="e: W[y]=0\lInit", shape="box", color="blue"];
  eiid5 [label="f: W[x]=0\lInit", shape="box", color="blue"];
  
  /* the unlocked events */
  subgraph cluster_proc0 { rank=sink; label = "Thread 0"; color=magenta; shape=box;
  eiid0 [label="a: W[x]=1\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid1 [label="b: W[y]=1\lproc:P0 poi:3\lSTR W2,[X3]", shape="box", color="blue"];
  }
  subgraph cluster_proc1 { rank=sink; label = "Thread 1"; color=magenta; shape=box;
  eiid2 [label="c: R[y]=1\lproc:P1 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid3 [label="d: R[x]=1\lproc:P1 poi:1\lLDR W2,[X3]", shape="box", color="blue"];
  }
  
  /* the intra_causality_data edges */
  
  
  /* the intra_causality_control edges */
  
  /* the poi edges */
  eiid0 -> eiid1 [label="po", color="black", fontcolor="black"];
  eiid2 -> eiid3 [label="po", color="black", fontcolor="black"];
  /* the rfmap edges */
  
  
  /* The viewed-before edges */
  eiid0 -> eiid3 [label="rf", color="red", fontcolor="red"];
  eiid1 -> eiid2 [label="rf", color="red", fontcolor="red"];
  eiid4 -> eiid1 [label="ca", color="blue", fontcolor="blue"];
  eiid5 -> eiid0 [label="ca", color="blue", fontcolor="blue"];
  }
  
  DOTEND MP
  Test MP Allowed
  States 4
  1:X0=0; 1:X2=0;
  1:X0=0; 1:X2=1;
  1:X0=1; 1:X2=0;
  1:X0=1; 1:X2=1;
  Ok
  Witnesses
  Positive: 1 Negative: 3
  Condition exists (1:X0=1 /\ 1:X2=0)
  Observation MP Sometimes 1 3
  Hash=211d5b298572012a0869d4ded6a40b7f
  
  show MP file
  $ herd7 -set-libdir ../libdir MP.litmus -dumpes true -show all -o - | sed '/^Time/d'
  
  DOTBEGIN MP
  DOTCOM dot
  digraph G {
  
  
  
  /* init events */
  eiid4 [label="e: W[y]=0\lInit", shape="box", color="blue"];
  eiid5 [label="f: W[x]=0\lInit", shape="box", color="blue"];
  
  /* the unlocked events */
  subgraph cluster_proc0 { rank=sink; label = "Thread 0"; color=magenta; shape=box;
  eiid0 [label="a: W[x]=1\lproc:P0 poi:1\lSTR W0,[X1]", shape="box", color="blue"];
  eiid1 [label="b: W[y]=1\lproc:P0 poi:3\lSTR W2,[X3]", shape="box", color="blue"];
  }
  subgraph cluster_proc1 { rank=sink; label = "Thread 1"; color=magenta; shape=box;
  eiid2 [label="c: R[y]=S1\lproc:P1 poi:0\lLDR W0,[X1]", shape="box", color="blue"];
  eiid3 [label="d: R[x]=S3\lproc:P1 poi:1\lLDR W2,[X3]", shape="box", color="blue"];
  }
  
  /* the intra_causality_data edges */
  
  
  /* the intra_causality_control edges */
  
  /* the poi edges */
  eiid0 -> eiid1 [label="po", color="black", fontcolor="black"];
  eiid2 -> eiid3 [label="po", color="black", fontcolor="black"];
  /* the rfmap edges */
  
  
  /* The viewed-before edges */
  }
  
  DOTEND MP
  $ herd7 -set-libdir ../libdir MP+CAS-rfi-ctrl+acq.litmus -outcomereads false -debug pretty | sed '/^Time/d'
  Test MP+CAS-rfi-ctrl+acq Allowed
  States 4
  1:X0=0; 1:X2=0; [x]=1;
  1:X0=0; 1:X2=1; [x]=1;
  1:X0=1; 1:X2=0; [x]=1;
  1:X0=1; 1:X2=1; [x]=1;
  Ok
  Witnesses
  Positive: 2 Negative: 6
  Condition exists ([x]=1 /\ 1:X0=1 /\ 1:X2=0)
  Observation MP+CAS-rfi-ctrl+acq Sometimes 2 6
  Hash=62c8603fff543d64f663988ed4b06cd9
  
  $ herd7 -set-libdir ../libdir MP+CAS-rfi-ctrl+acq.litmus -outcomereads true -debug pretty | sed '/^Time/d'
  Test MP+CAS-rfi-ctrl+acq Allowed
  States 4
  0:X1=0; 1:X0=0; 1:X2=0; [x]=1;
  0:X1=0; 1:X0=0; 1:X2=1; [x]=1;
  0:X1=0; 1:X0=1; 1:X2=0; [x]=1;
  0:X1=0; 1:X0=1; 1:X2=1; [x]=1;
  Ok
  Witnesses
  Positive: 2 Negative: 6
  Condition exists ([x]=1 /\ 1:X0=1 /\ 1:X2=0)
  Observation MP+CAS-rfi-ctrl+acq Sometimes 2 6
  Hash=62c8603fff543d64f663988ed4b06cd9
  
