func main() => integer
begin
  let o13 = Ones(13);
  let o26 = [o13,o13];
  assert o26 =='11111111111111111111111111';
  let o5 = Ones(5);
  let o11 = Ones(11);
  let o16 = [o5,o11];
  let p16 = [o11,o5];
  assert o16 == p16;
  let o15 = Ones(15);
  let o20 = [o5,o15];
  let p20 = [o15,o5];
  assert o20 == p20;
  let o8 = Ones(8);
  let o6 = Ones(6);
  let o14 = [o8,o6];
  let p14 = [o6,o8];
  assert o14 == p14;
return 0;
end