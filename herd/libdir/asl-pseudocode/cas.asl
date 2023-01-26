func main()
begin
    address = read_register(n, 64);
    compare_value = read_register(s, datasize);
    new_value = read_register(t, datasize);
    old_value = read_memory(address, datasize);

    if compare_value == old_value then
        write_memory(address, datasize, new_value);
    else
        pass;
    end

    write_register(s, datasize, old_value);
end
