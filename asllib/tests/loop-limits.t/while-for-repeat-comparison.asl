func run_while(n: integer)
begin
  var i : integer = 1;
  while i <= n looplimit 1 do
    println i;
    i = i + 1;
  end;
end;

func run_repeat(n: integer)
begin
  var i : integer = 1;
  repeat
    println i;
    i = i + 1;
  until !(i <= n) looplimit 1;
end;

func run_for(n: integer)
begin
  for i = 1 to n looplimit 1 do
    println i;
  end;
end;

func main() => integer
begin
  println "while loop:";
  run_while(1);

  println "repeat loop:";
  run_repeat(1);

  println "for loop:";
  run_for(1);

  return 0;
end;
