let sub_k = ARBITRARY: integer{5, 64};

type my_type of bits(sub_k) {
  [0] flag
};

func main() => integer
begin
  return 0;
end;
