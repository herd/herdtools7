func main () => integer
begin
  var d:integer;
  var n:integer;

  if d IN {13,15} || n IN {13,15} then
      UNPREDICTABLE();
  end
end
