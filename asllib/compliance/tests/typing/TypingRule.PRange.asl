func main () => integer
begin
    assert 42 IN { 3..42 };
    assert 42.4 IN { -1.8..142.4 };
    assert 42.4 IN { -1.8..142.0 };
    return 0;
end;
