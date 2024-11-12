
func main () => integer
begin
  assert ('111' IN '1xx') == TRUE;
  assert ('111' IN '0xx') == FALSE;

  return 0;
end
