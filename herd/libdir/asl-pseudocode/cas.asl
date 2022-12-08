func main(s, t, n)
    address = read_register(n);
    compare_value = read_register(s);
    new_value = read_register(t);
    old_value = read_memory(address);
    if compare_value == old_value then
        write_memory(address, new_value)
    else
        pass
    end;
    write_register(s, old_value)
endfunc