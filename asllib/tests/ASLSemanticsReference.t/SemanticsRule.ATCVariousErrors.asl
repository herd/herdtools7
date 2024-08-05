func ErrorExample()
begin
  var a: integer{1, 2, 3} = 2 as integer{1, 2, 3}; // legal
  var b: integer{4, 5, 6} = 2;                     // static error
  var c: integer{4, 5, 6} = 2 as integer{4, 5, 6}; // dynamic error
  if FALSE then
      var d: integer{4, 5, 6} = 2;                     // static error
      var e: integer{4, 5, 6} = 2 as integer{4, 5, 6}; // not a dynamic error as will never be evaluated
  end
end
