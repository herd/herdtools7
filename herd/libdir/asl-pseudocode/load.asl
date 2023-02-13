func main()
begin
  address = read_register(n, 64);
  data = read_memory(address, datasize);
  write_register(t, datasize, data);
end
