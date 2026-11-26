Compare every caption with the reported states, joining wrapped caption lines
and ignoring spacing. Keep duplicate captions to check the execution count too.

  $ check_states () {
  >   cat > herd.out
  >   sed -n 's/^label=".*Final state:\\n\(.*\)";$/\1/p' herd.out |
  >     sed 's/\\n/ /g; s/^(empty)$//; s/  */ /g; s/^ //; s/ $//' > captions
  >   awk '/^States / { n=$2; next } n>0 { print; n-- }' herd.out |
  >     sed 's/  */ /g; s/^ //; s/ $//' > states
  >   test -s captions || return 1
  >   while IFS= read -r state; do
  >     grep -Fxq -- "$state" states || return 1
  >   done < captions
  >   printf '%s captions match %s reported states\n' "$(grep -c '^' captions)" "$(grep -c '^' states)"
  > }

Captions are optional and independent of the title legend.

  $ herd7 -set-libdir ../libdir MP.litmus -show prop -o - -shortlegend true | grep '^label='
  label="MP";
  $ herd7 -set-libdir ../libdir MP.litmus -show prop -o - -conf state.cfg | grep '^label='
  label="MP\n\nFinal state:\n1:X0=1; 1:X2=0;";
  $ herd7 -set-libdir ../libdir MP.litmus -show prop -o - -showfinalstate true -showlegend false | grep '^label='
  label="Final state:\n1:X0=1; 1:X2=0;";

Caption order follows execution order with both projection modes.

  $ herd7 -set-libdir ../libdir MP.litmus -show all -o - -conf state.cfg | grep '^label='
  label="MP\n\nFinal state:\n1:X0=0; 1:X2=0;";
  label="MP\n\nFinal state:\n1:X0=0; 1:X2=1;";
  label="MP\n\nFinal state:\n1:X0=1; 1:X2=0;";
  label="MP\n\nFinal state:\n1:X0=1; 1:X2=1;";
  $ herd7 -set-libdir ../libdir MP.litmus -show all -o - -conf state.cfg -outcomereads true | grep '^label='
  label="MP\n\nFinal state:\n1:X0=0; 1:X2=0;";
  label="MP\n\nFinal state:\n1:X0=0; 1:X2=1;";
  label="MP\n\nFinal state:\n1:X0=1; 1:X2=0;";
  label="MP\n\nFinal state:\n1:X0=1; 1:X2=1;";

With outcomereads, include registers loaded in hidden executions too, but
exclude unrelated initial registers (X1 and X4).

  $ herd7 -set-libdir ../libdir Branches.litmus -show prop -o - -conf state.cfg -outcomereads true | grep '^label='
  label="Branches\n\nFinal state:\n0:X0=0; 0:X2=0; 0:X3=0;";
  label="Branches\n\nFinal state:\n0:X0=0; 0:X2=1; 0:X3=0;";
  $ herd7 -set-libdir ../libdir Branches.litmus -show prop -o - -conf state.cfg -outcomereads true | check_states
  2 captions match 3 reported states
  $ herd7 -set-libdir ../libdir Branches.litmus -show all -o - -conf state.cfg -outcomereads false | check_states
  3 captions match 2 reported states
  $ herd7 -set-libdir ../libdir Branches.litmus -show all -o - -conf state.cfg -outcomereads true | check_states
  3 captions match 3 reported states

Early stopping still delivers the buffered executions and projects using only
the executions counted before stopping.

  $ herd7 -set-libdir ../libdir Branches.litmus -show all -o - -conf state.cfg -outcomereads true -nshow 1 | check_states
  1 captions match 1 reported states
  $ herd7 -set-libdir ../libdir Branches.litmus -show all -o - -conf state.cfg -outcomereads true -speedcheck fast | check_states
  1 captions match 1 reported states

Long states wrap between bindings; TeX captions escape symbolic underscores.

  $ herd7 -set-libdir ../libdir Wide.litmus -show prop -o - -conf state.cfg | grep '^label='
  label="Wide\n\nFinal state:\n0:X0=1; 0:X1=long_symbolic_address; 0:X2=2; 0:X3=3; 0:X4=4;\n0:X5=5; 0:X6=6; 0:X7=7; 0:X8=8; 0:X9=9; 0:X10=0;";
  $ herd7 -set-libdir ../libdir Wide.litmus -show prop -o - -conf state.cfg | check_states
  1 captions match 1 reported states
  $ herd7 -set-libdir ../libdir Wide.litmus -show prop -o - -conf state.cfg -texmacros true | grep '^label='
  label="Wide\n\nFinal state:\n\\texttt{0:X0=1; 0:X1=long\\_symbolic\\_address; 0:X2=2; 0:X3=3; 0:X4=4;}\n\\texttt{0:X5=5; 0:X6=6; 0:X7=7; 0:X8=8; 0:X9=9; 0:X10=0;}";

Empty projected states are explicit.

  $ herd7 -set-libdir ../libdir Empty.litmus -show all -o - -conf state.cfg | check_states
  4 captions match 1 reported states
  $ herd7 -set-libdir ../libdir Empty.litmus -show all -o - -conf state.cfg -nshow 1 | grep '^label='
  label="MP-true\n\nFinal state:\n(empty)";

Captions match the reported states when faults are present and when hexadecimal output is enabled.

  $ herd7 -set-libdir ../libdir Fault.litmus -variant shadowstack -show all -o - -conf state.cfg -outcomereads true | check_states
  1 captions match 1 reported states
  $ herd7 -set-libdir ../libdir Wide.litmus -show prop -o - -conf state.cfg -hexa true | check_states
  1 captions match 1 reported states
