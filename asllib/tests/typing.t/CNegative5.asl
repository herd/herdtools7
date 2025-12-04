// width of bv must be equal to i
func printLengths{N}(i: integer {N}, bv: bits(N))
begin
  print i;
  print " == ";
  print N;
  print " == ";
  print Len(bv);
  print "\n";
end;
func negative5() => integer
begin
  printLengths{12}(3, Zeros{12}); // illegal
end;
