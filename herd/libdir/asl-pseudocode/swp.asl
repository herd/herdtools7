func main()
begin
    store_value = read_register(s, datasize);
    address = read_register(n, 64);
    old_value = read_memory(address, datasize);
    write_memory(address, datasize, store_value);
    write_register(t, datasize, old_value);
end
