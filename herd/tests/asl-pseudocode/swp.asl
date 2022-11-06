func main(s, t, n)
    store_value = read_register(s);
    address = read_register(n);
    old_value = read_memory(address);
    write_memory(address, store_value);
    write_register(t, old_value)
endfunc