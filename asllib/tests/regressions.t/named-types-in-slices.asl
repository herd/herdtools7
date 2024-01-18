type pagros of integer{1,2,4,8,16};

var ones = Ones(64);

func f(sz:pagros) => bits(sz)
begin
  return ones[sz-1:0];
end

func main() => integer
begin
  let x = f(8);
  DEBUG(x);
  return 0;
end
