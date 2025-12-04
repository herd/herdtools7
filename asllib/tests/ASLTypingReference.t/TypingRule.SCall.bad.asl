func zero() => integer
begin
  return 0;
end;

func main() => integer
begin
    - = zero(); // Legal: returned value is consumed.
    // Illegal: 'zero' is a function, not a procedure,
    // and its returned value must be consumed.
    zero();
    return 0;
end;
