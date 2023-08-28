func main() => integer
begin
  let a =  '11110000';
  let b = '101';
  let c = [a,b];
  assert c == '11110000101';
  return 0;
end
