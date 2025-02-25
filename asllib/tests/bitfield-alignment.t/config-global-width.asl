config sub_k: integer{5, 64} = 5;

type my_type of bits(sub_k) {
  [0] flag
};

func main() => integer
begin
  return 0;
end;
