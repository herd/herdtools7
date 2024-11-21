
main:	file format mach-o arm64

Disassembly of section __TEXT,__text:

0000000100003e74 <_compute_pac>:
100003e74: d10043ff    	sub	sp, sp, #0x10
100003e78: f90007e0    	str	x0, [sp, #0x8]
100003e7c: f90003e1    	str	x1, [sp]
100003e80: f94007e8    	ldr	x8, [sp, #0x8]
100003e84: f94003e9    	ldr	x9, [sp]
100003e88: dac10928    	pacda	x8, x9
100003e8c: f90007e8    	str	x8, [sp, #0x8]
100003e90: f94007e0    	ldr	x0, [sp, #0x8]
100003e94: 910043ff    	add	sp, sp, #0x10
100003e98: d65f03c0    	ret

0000000100003e9c <_compute_aut>:
100003e9c: d10043ff    	sub	sp, sp, #0x10
100003ea0: f90007e0    	str	x0, [sp, #0x8]
100003ea4: f90003e1    	str	x1, [sp]
100003ea8: f94007e8    	ldr	x8, [sp, #0x8]
100003eac: f94003e9    	ldr	x9, [sp]
100003eb0: dac11928    	autda	x8, x9
100003eb4: f90007e8    	str	x8, [sp, #0x8]
100003eb8: f94007e0    	ldr	x0, [sp, #0x8]
100003ebc: 910043ff    	add	sp, sp, #0x10
100003ec0: d65f03c0    	ret

0000000100003ec4 <_compute_xpacd>:
100003ec4: d10043ff    	sub	sp, sp, #0x10
100003ec8: f90007e0    	str	x0, [sp, #0x8]
100003ecc: f94007e8    	ldr	x8, [sp, #0x8]
100003ed0: dac147e8    	xpacd	x8
100003ed4: f90007e8    	str	x8, [sp, #0x8]
100003ed8: f94007e0    	ldr	x0, [sp, #0x8]
100003edc: 910043ff    	add	sp, sp, #0x10
100003ee0: d65f03c0    	ret

0000000100003ee4 <_main>:
100003ee4: d10143ff    	sub	sp, sp, #0x50
100003ee8: a9047bfd    	stp	x29, x30, [sp, #0x40]
100003eec: 910103fd    	add	x29, sp, #0x40
100003ef0: 52800008    	mov	w8, #0x0                ; =0
100003ef4: b9001fe8    	str	w8, [sp, #0x1c]
100003ef8: b81fc3bf    	stur	wzr, [x29, #-0x4]
100003efc: d28acf08    	mov	x8, #0x5678             ; =22136
100003f00: f2a24688    	movk	x8, #0x1234, lsl #16
100003f04: f81f03a8    	stur	x8, [x29, #-0x10]
100003f08: f85f03a8    	ldur	x8, [x29, #-0x10]
100003f0c: 910003e9    	mov	x9, sp
100003f10: f9000128    	str	x8, [x9]
100003f14: 90000000    	adrp	x0, 0x100003000 <_printf+0x100003000>
100003f18: 913dd000    	add	x0, x0, #0xf74
100003f1c: 94000013    	bl	0x100003f68 <_printf+0x100003f68>
100003f20: d2808008    	mov	x8, #0x400              ; =1024
100003f24: f81e83a8    	stur	x8, [x29, #-0x18]
100003f28: f85f03a0    	ldur	x0, [x29, #-0x10]
100003f2c: f85e83a1    	ldur	x1, [x29, #-0x18]
100003f30: 97ffffd1    	bl	0x100003e74 <_compute_pac>
100003f34: f90013e0    	str	x0, [sp, #0x20]
100003f38: f85e83aa    	ldur	x10, [x29, #-0x18]
100003f3c: f94013e8    	ldr	x8, [sp, #0x20]
100003f40: 910003e9    	mov	x9, sp
100003f44: f900012a    	str	x10, [x9]
100003f48: f9000528    	str	x8, [x9, #0x8]
100003f4c: 90000000    	adrp	x0, 0x100003000 <_printf+0x100003000>
100003f50: 913e1000    	add	x0, x0, #0xf84
100003f54: 94000005    	bl	0x100003f68 <_printf+0x100003f68>
100003f58: b9401fe0    	ldr	w0, [sp, #0x1c]
100003f5c: a9447bfd    	ldp	x29, x30, [sp, #0x40]
100003f60: 910143ff    	add	sp, sp, #0x50
100003f64: d65f03c0    	ret

Disassembly of section __TEXT,__stubs:

0000000100003f68 <__stubs>:
100003f68: b0000010    	adrp	x16, 0x100004000 <_printf+0x100004000>
100003f6c: f9400210    	ldr	x16, [x16]
100003f70: d61f0200    	br	x16

Disassembly of section __TEXT,__cstring:

0000000100003f74 <__cstring>:
100003f74: 2074656c    	<unknown>
100003f78: 203d2078    	<unknown>
100003f7c: 6c257830    	stnp	d16, d30, [x1, #-0x1b0]
100003f80: 000a786c    	<unknown>
100003f84: 64636170    	<unknown>
100003f88: 2c782861    	ldnp	s1, s10, [x3, #-0x40]
100003f8c: 25783020    	whilewr	p0.h, x1, x24
100003f90: 29786c6c    	ldp	w12, w27, [x3, #-0x40]
100003f94: 30203d20    	adr	x0, 0x100044739 <_printf+0x100044739>
100003f98: 6c6c2578    	ldnp	d24, d9, [x11, #-0x140]
100003f9c: 78 0a 00    	<unknown>

Disassembly of section __TEXT,__unwind_info:

0000000100003fa0 <__unwind_info>:
100003fa0: 00000001    	udf	#0x1
100003fa4: 0000001c    	udf	#0x1c
100003fa8: 00000000    	udf	#0x0
100003fac: 0000001c    	udf	#0x1c
100003fb0: 00000000    	udf	#0x0
100003fb4: 0000001c    	udf	#0x1c
100003fb8: 00000002    	udf	#0x2
100003fbc: 00003e74    	udf	#0x3e74
100003fc0: 00000040    	udf	#0x40
100003fc4: 00000040    	udf	#0x40
100003fc8: 00003f68    	udf	#0x3f68
100003fcc: 00000000    	udf	#0x0
100003fd0: 00000040    	udf	#0x40
		...
100003fe0: 00000003    	udf	#0x3
100003fe4: 0002000c    	<unknown>
100003fe8: 00020014    	<unknown>
100003fec: 00000000    	udf	#0x0
100003ff0: 01000070    	<unknown>
100003ff4: 02001000    	<unknown>
100003ff8: 04000000    	add	z0.b, p0/m, z0.b, z0.b
100003ffc: 00000000    	udf	#0x0

Disassembly of section __DATA_CONST,__got:

0000000100004000 <__got>:
100004000: 00000000    	udf	#0x0
100004004: 80000000    	<unknown>
