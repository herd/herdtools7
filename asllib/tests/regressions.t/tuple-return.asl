func mytuple (x : integer, y : integer) => (integer, integer)
begin
	if (x == 0) then
	   var pair = (x, y);
	   return pair;
	end;
	
	return (x, y);
end;

func main () => integer
begin
	var a : integer;
	var b : integer;
	(a, b) = mytuple(0, 1);
  (b, a) = mytuple(1, 1);
	return 0;
end;
