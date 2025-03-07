type Color of enumeration {RED, GREEN, BLUE};

func main () => integer
begin
    assert TRUE IN {TRUE};
    assert 42 IN { 42 };
    assert 42.4 IN { 42.4 };
    assert "hello" IN { "hello" };
    assert RED IN { RED };
    assert '101' IN { '101' };
    return 0;
end;
