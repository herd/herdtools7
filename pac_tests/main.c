#import <stdio.h>
#import <stdint.h>

uint64_t compute_pac(uint64_t data, uint64_t modifier) {
  asm volatile (
    "pacda %[reg], %[mod]"
    : [reg] "+r" (data)
    : [mod] "r" (modifier)
  );

  return (uint64_t) data;
}

uint64_t compute_aut(uint64_t data, uint64_t modifier) {
  asm volatile (
    "autda %[reg], %[mod]"
    : [reg] "+r" (data)
    : [mod] "r" (modifier)
  );

  return (uint64_t) data;
}

uint64_t compute_xpacd(uint64_t data) {
  asm volatile (
    "xpacd %[reg]"
    : [reg] "+r" (data)
  );

  return (uint64_t) data;
}

int main() {


  uint64_t x = 0x12345678;
  printf("let x = 0x%llx\n", x);

  uint64_t mod = 1024;
  uint64_t r = compute_pac(x, mod);
  printf("pacda(x, 0x%llx) = 0x%llx\n", mod, r);
  return 0;
}
