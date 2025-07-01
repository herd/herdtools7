func loop_forever() => integer
begin
    // Even though the following loop will never terminate,
    // the typechecker conservatively determines that there
    // may be terminating paths that do not terminate
    // by either returning a value, throwing an exception, or
    // executing unreachable.
    while (TRUE) do
        pass;
    end;
    // The following commented statement is needed to appease
    // the typechecker.
    // unreachable;
end;
