func main(t::integer, n::integer, datasize::integer)
  address = read_register(n, 64);
  data = read_register(t, datasize);
  write_memory(address, datasize, data)
end
