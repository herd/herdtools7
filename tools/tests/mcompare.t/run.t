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
Check that the files P and N are empty.
  $ test ! -s P  && test ! -s N
  $ mcmp7 OLD.00 NEW.00 2>/dev/null
  $ msum7 OLD.00 NEW.00 > ALL.00 2>/dev/null
  $ herd7 -set-libdir ../libdir src/@all > Herd
  $ mcmp7 -pos P1 -neg N1 Herd ALL.00
  Stores+Faults
  [1]
  $ mcompare7 -pos P2 -neg N2 Herd ALL.00 >/dev/null
  $ diff P1 P2
  $ diff N1 N2
  $ mcmp7 -mmu-faults-as-data false -pos P1 -neg N1 old.log new.log
  $ mcmp7 -mmu-faults-as-data true -pos P2 -neg N2 old.log new.log
  $ diff P1 P2  
  $ diff N1 N2
Negative faults do not intervene in state comparisons
  $ mcompare7 -show s xxx.log yyy.log
  *Outcomes*
   | xxx.log               yyy.log              
  -----------------------------------------------
  -----------------------------------------------
  T| [[x]=1; ~fault(P0,x)] [[x]=1; ~fault(P0,y)]
  
  $ mcompare7 -show d xxx.log yyy.log
  *Diffs*
   |Kind | xxx.log               yyy.log
  --------------------------------------
  --------------------------------------
  T|Allow| [[x]=1; ~fault(P0,x)] ==     
   |Ok   |                              
  
  $ mcmp7 xxx.log yyy.log
