func main() => integer
begin
    // Illeal, since 5.0 is neither a bitvector type nor a integer type.
    let x = 5.0[2:0];
    return 0;
end;
