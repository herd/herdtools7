//R_WVQT: The repeat loop terminates once the condition is TRUE.

@looplimit(20)
repeat
  emptySlot = !_InstInfo[i].Valid;
  if emptySlot && (isBeatInst || i == 0) then
      _InstInfo[i].Valid  = TRUE;
      _InstInfo[i].Length = len;
      _InstInfo[i].Opcode = opcode;
end
i = i + 1;
until emptySlot || (!isBeatInst && i > 0) || (i >= MAX_OVERLAPPING_INSTRS);

