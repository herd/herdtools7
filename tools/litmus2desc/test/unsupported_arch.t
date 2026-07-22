Reject non-AArch64 litmus tests with a clear error.

  $ cat > unsupported_x86.litmus <<'EOF'
  > X86_64 UnsupportedArch
  > {}
  > P0;
  >   NOP;
  > forall 0:rax = 0
  > EOF
  $ litmus2desc unsupported_x86.litmus
  litmus2desc: unsupported: litmus test with architecture `X86_64`.
  [1]
