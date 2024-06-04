func main () => integer
begin
  var a: integer {0..2} = 0;
  var b: integer {10..12} = 10;

  for i = a to b do
    print (i, a, b);
  end

  return 0;
end


