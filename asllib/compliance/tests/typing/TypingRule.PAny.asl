type Color of enumeration {RED, GREEN, BLUE};

func main () => integer
begin
    assert TRUE IN {FALSE, TRUE};
    assert 42 IN { 40, 41, 42 };
    assert 42.4 IN { 42.4 };
    assert "hello" IN { "hello", "world" };
    assert RED IN { RED, GREEN };
    assert '101' IN { '101', '110' };
    return 0;
end;
