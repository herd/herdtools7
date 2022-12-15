func main(t::integer, n::integer)
  address = read_register(n);
  data = read_register(t);
  write_memory(address, data)
end
