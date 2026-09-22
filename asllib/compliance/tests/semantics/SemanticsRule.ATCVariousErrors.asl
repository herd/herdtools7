func main() => integer
begin
  var a: integer{1, 2, 3} = 2 as integer{1, 2, 3}; // Legal
  if FALSE then
      // A dynamic error
      var e: integer{4, 5, 6} = 2 as integer{4, 5, 6};
  end;
  var c: integer{4, 5, 6} = 2 as integer{4, 5, 6}; // A dynamic error
  return 0;
end;
