func main () => integer
begin
  var i: integer = 0;
  var j: integer = 0;

  while (i < 10) looplimit 20 do
    i = i + 1;
    j = 0;
    while (j < 10) looplimit 20 do
      j = j + 1;
    end;
  end;

  return 0;
end;
