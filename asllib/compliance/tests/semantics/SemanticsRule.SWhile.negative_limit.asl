func main () => integer
begin
  // A negative loop limit prevents loops from executing.
  while TRUE looplimit -1 do
    println "This should not be printed";
  end;
  return 0;
end;
