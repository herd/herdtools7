func recurse (n: integer) => integer
recurselimit 5
begin
  print (n);
  if n >= 10 then return 1;
  else return 1 + recurse (n+1); end;
end;

func main () => integer
begin
  print("Number of calls: ", recurse (0));
  print("Number of calls: ", recurse (0));

  return 0;
end;

