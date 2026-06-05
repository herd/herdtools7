func nop()
begin
  pass;
end;

func main() => integer
begin
    nop();
    - = nop(); // Illegal: nop is a procedure, therefore no value to consume.
    return 0;
end;
