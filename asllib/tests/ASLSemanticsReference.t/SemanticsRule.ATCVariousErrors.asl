func ErrorExample()
begin
  var a: integer{1, 2, 3} = 2 as integer{1, 2, 3}; // Legal
  var b: integer{4, 5, 6} = 2;                     // A type error
  var c: integer{4, 5, 6} = 2 as integer{4, 5, 6}; // A dynamic error
  if FALSE then
      var d: integer{4, 5, 6} = 2; // A type error
      // A dynamic error
      var e: integer{4, 5, 6} = 2 as integer{4, 5, 6};
  end;
end;
