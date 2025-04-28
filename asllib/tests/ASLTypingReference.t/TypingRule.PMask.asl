func main () => integer
begin
    assert '101010' IN {'xx1010'};
    assert '101010' IN {'(10)1010'};
    assert '101010' IN {'(10)10xx'};
    return 0;
end;
