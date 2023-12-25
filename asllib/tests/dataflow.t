
Some fake examples for demonstration purposes: 

  $ cat >add_v0.asl <<EOF
  > var t = if Rt == 31 then SP else Rt;
  > var op1 = X[t, 64];
  > var op2 = X[s, 64];
  > X[d, 64] = op1 + op2;

  $ asldataflow --opn -0 add_v0.asl
  Interesting stuff:
    Output at "X": {d}
    Input at "X": {s}
    Input at "X": {Rt, SP}

  $ asldataflow --opn -0 add_v0.asl --raw
  Interesting stuff:
    Output at "X": {d}
    Input at "X": {s}
    Input at "X": {t}

