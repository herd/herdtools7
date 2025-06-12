type BAD_OPCODE of exception;

func decode_instruction(op: bits(32))
begin
    if op[1] == '1' then
        throw BAD_OPCODE{-};
    end;
end;

func top(opcode: bits(32))
begin
    try
        decode_instruction(opcode);
    catch
        when e: BAD_OPCODE =>
            handle_exception();
    end;
end;

type IsExceptionTaken of exception;

func handle_exception()
begin
    try
        if HandleExceptionTransitions(commitState) then
            SteppingDebug();
            VectorCatchDebug();
        end;
        if LockedUp && NextInstrAddr() != 0xEFFFFFFE[31:0] then
            LockedUp = FALSE;
        end;
        if !LockedUp then
            InstructionAdvance(commitState);
        end;
    catch
        when exn: IsExceptionTaken =>
            pass; // ignore exception
    end;
end;

func NextInstrAddr() => bits(32)
begin
    return ARBITRARY: bits(32);
end;

func SteppingDebug() begin pass; end;
func VectorCatchDebug() begin pass; end;

var commitState: integer;
var LockedUp: boolean;

func InstructionAdvance(state: integer)
begin pass; end;

func HandleExceptionTransitions(state: integer) => boolean
begin
    return TRUE;
end;
