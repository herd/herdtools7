func inv(i::integer) => integer
begin
  case i of 
    when 0: return 1;
    when 1: return 0;
  end
end

func main()
begin
  assert 1 == inv(0);
  assert 0 == inv(1);
end

