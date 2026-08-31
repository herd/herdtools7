Check that mcompare7 and mcmp7 yield the same result
File NEW.00 is an authentic litmus log, OLD.00 is a hand made alteration of it,
standing for what a very old version of litmus could have produced.
  $ diff OLD.00 NEW.00
  77,78c77,79
  < 398281:>[x]=1; fault(P0,x); ~fault(P0:L0);
  < 12595916:>[x]=0; fault(P0:L0,x); 
  ---
  > 5864063:>[x]=0; fault(P0:L0,x,D-MMU:Permission); 
  > 398281:>[x]=1; fault(P0,x,D-MMU:Permission); ~fault(P0:L0);
  > 6731853:>[x]=0; fault(P0:L0,x,D-MMU:Translation); 
  [1]

  $ mcompare7 OLD.00 NEW.00 -pos P -neg N >/dev/null 2>/dev/null
  $ cat P N | wc -l
  0
  $ mcmp7 OLD.00 NEW.00 2>/dev/null
  $ msum7 OLD.00 NEW.00 > ALL.00 2>/dev/null
  $ cat ALL.00
  Test Load+Fault Require
  Histogram (2 states)
  39722272:> 0:X0=0; fault(P0:L0,x,D-MMU:Translation);
  277728  :> 0:X0=1; ~fault(P0);
  Ok
  Witnesses
  Positive: 40000000 Negative: 0
  Condition forall (0:X0=1 \/ fault(P0)) is validated
  Hash=4cf70958bd2d7e2535dd1fc869f21e67
  Time Load+Fault 49.40
  
  Test Stores+Faults Require
  Histogram (4 states)
  18459979:> [x]=0; fault(P0:L0,x,D-MMU:Permission);
  6731853 :> [x]=0; fault(P0:L0,x,D-MMU:Translation);
  796562  :> [x]=1; fault(P0,x,D-MMU:Permission); ~fault(P0:L0);
  14011606:> [x]=2; ~fault(P0); ~fault(P0:L0);
  Ok
  Witnesses
  Positive: 40000000 Negative: 0
  Condition forall ((x=1 => not (fault(P0:L0))) /\ (x=2 => not (fault(P0)))) is validated
  Hash=d5abb72c93a2349c5e576da02287bb5e
  Time Stores+Faults 49.78
  
  $ herd7 -set-libdir ../libdir src/@all > Herd
  $ mcmp7 -pos P1 -neg N1 Herd ALL.00
  Stores+Faults
  [1]
  $ mcompare7 -pos P2 -neg N2 Herd ALL.00 >/dev/null
  $ diff P1 P2
  $ diff N1 N2
