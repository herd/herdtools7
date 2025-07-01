// width of bv must be equal to i
func printLengths{N}(i: integer {N}, bv: bits(N))
begin
  print DecStr(i);
  print " == ";
  print DecStr(N);
  print " == ";
  print DecStr(Len(bv));
  print "\n";
end;
func negative5() => integer
begin
  printLengths{12}(3, Zeros{12}); // illegal
end;
