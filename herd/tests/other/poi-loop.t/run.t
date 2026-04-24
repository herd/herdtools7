Check that po indices/edges are assigned correctly in the presence of loops (only checks the last (longest) execution graph)
  $ herd7 -set-libdir ../libdir -dumpes true -o - ./wait-flag.litmus | awk '/digraph G \{/ {block=""} {block = block $0 "\n"} END {printf "%s", block}' | grep -e 'poi' -e 'label="po"'
  eiid0 [label="a: W[x]=1\lproc:P0 poi:1\lSTR W1,[X0]", shape="box", color="blue"];
  eiid1 [label="b: R[x]=S1\lproc:P1 poi:0\lLDR W1,[X0]", shape="box", color="blue"];
  eiid11 [label="l: Branching(bcc)\lproc:P1 poi:1\lCBZ W1,.-4", shape="box", color="blue"];
  eiid2 [label="c: R[x]=S5\lproc:P1 poi:2\lLDR W1,[X0]", shape="box", color="blue"];
  eiid15 [label="p: Branching(bcc)\lproc:P1 poi:3\lCBZ W1,.-4", shape="box", color="blue"];
  eiid3 [label="d: R[x]=S9\lproc:P1 poi:4\lLDR W1,[X0]", shape="box", color="blue"];
  eiid19 [label="t: Branching(bcc)\lproc:P1 poi:5\lCBZ W1,.-4", shape="box", color="blue"];
  eiid20 [label="u: CutOff:L0\lproc:P1 poi:6\lLDR W1,[X0]", shape="box", color="blue"];
  /* the poi edges */
  eiid1 -> eiid11 [label="po", color="black", fontcolor="black"];
  eiid2 -> eiid15 [label="po", color="black", fontcolor="black"];
  eiid3 -> eiid19 [label="po", color="black", fontcolor="black"];
  eiid11 -> eiid2 [label="po", color="black", fontcolor="black"];
  eiid15 -> eiid3 [label="po", color="black", fontcolor="black"];
  eiid19 -> eiid20 [label="po", color="black", fontcolor="black"];
