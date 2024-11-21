Pointer Authentication Code is an extension of AArch64 documented in the Arm
Architecture Reference Manual for A-profile architecture. It is compose of
multiple extensions that may or may not be compatible including:

- `FEAT_Pauth` the basic extension for PAC, it introduce a set of new
  instruction to add and check cryptographic signatures to the most significant
  bits of virtual addresses.
- `FEAT_EPAC` change the way we add a PAC field in a virtual address that
  already contain a PAC field by setting it to zeros
- `FEAT_Pauth2`, not retro-compatible with `FEAT_Pauth`, and is not compatible
  with `FEAT_EPAC`. It allow to add multiple signatures to the same virtual
  address using the XOR operation, and change the authentication process in
  consequence.
- `FEAT_FPAC` and `FEAT_FCOMBINED` depends of `FEAT_Pauth2` and change the way
    we fails in case of an incorrect signature during an authentication
    instruction.

This note doesn't look at the address tagging and logical address tagging features
because they change the interraction between address translation and the PAC
field.

A good way to understand the behaviour of PAC is to write tables to view the
effect of each instructions on each pointer types.

To understand PAC it is important to understand some basics of the Stage 1 of
address translation (see D8.2.4 of Arm ARM):
- The virtual address space is composed of two ranges: on from the address 0 to
    $2^{64-TOSZ}-1$, and another from the address $2^{64} - 2^{64-TOSZ}$ to the
    address $2^64$ and the bit 55 of the virtual address is used to determine
    the address range we look at.

As a consequence if the bits 55 to 63 of the virtual address are not the same
(all 0 or all 1), then the translation will produce a `Translation_Falut`
exception. This principe is used everywhere in `FEAT_Pauth` and `FEAT_Pauth2` to
ensures that memory instructions with an incorrect PAC field fails. This may
change if we use address tagging or logical address tagging but I will assume
they are disactivated in this note.

During the rest of the note I will use the term canonical to define an address
with the bits 55 to 63 equal to zeros or ones.

I will use the notation `x`, `y`, `z` to represent canonical pointers,
`x:pac(keys)` to represent a PAC field using a list of keys (ia, da,
ib+modifier...), and the notation `x:non-canonical` to represent a non canonical
pointer with an invalid PAC field (or using an unknown key).

## `FEAT_Pauth`

Here is the table that represent my understanding of the `FEAT_Pauth` extension
(without `FEAT_EPAC`, `FEAT_Pauth2` or address tagging...):

|       | x               | x:non-canonical  | x:pac(da)        | x:pac(ia)        |
|:------|:----------------|:-----------------|:-----------------|:-----------------|
| LDR   | mem[x]          | TranslationFault | TranslationFault | TranslationFault |
| PACDA | x:pad(da)       | x:non-canonical  | x:non-canonical  | x:non-canonical  |
| AUTDA | x:non-canonical | x:non-canonical  | x                | x:non-canonical  |
| XPACD | x               | ?                | x                | x                |

This table doesn't represent the real PAC extension because in reality the pac
function depend of the cryptographic key, AND a context (name modifier in the
doc) used as a salt given to the hash function. As example PACDA take two
registers as input, the first is the virtual address and the second is the
modifier.

Loading from any other address than `x` will automatically fail because reading
from a virtual address that is not all zeros or all ones from the bit 55 return
a translation fault. Running PACDA to something different that `x` will result in
a non-canonical address because PACDA corrupt it's PAC field if the input
virtual address is non-canonical [^1]. AUTDA will return `x:non-canonical` if the PAC
field is different of `pac(da)`.

In almost any case the `XPACD` function return the original pointer, because the
`Strip` function in the pseudocode only depend of `ìs_data` argument if address
tagging or logical address tagging is activated but I assume they are not. And
it use the bit 55 to find the original pointer.

But if the value is non canonical and doesn't have a valid PAC field their is
two different cases:
- if the non-canonical is due to an authentication fault then the bit 55 is
    keep, so `XPACD x:non-canonical` must return `x`
- if the non canonical is due to a PAC field insertion in a non-canonical
    pointer then the `InsertPAC` may forget the bit 55, because it use the bit 63
    to guess the virtual address range and this bit may be overwritten by a
    previous call to `PAC*`. So the output value may have the wrong bit
    extensions (msb may be zeros instead of ones or ones instead of zeros).
    In this case the output is a non-deterministic canonical virtual address.

## `FEAT_Pauth2`

This second version of PAC change the way we add the PAC field to the virtual
address, in particular if a PAC field is already present. Instead of corrupting
it this extension allow to combine the new and the old PAC fields using a XOR.
This mechanism allow to have "multiple PAC fields in the same number of bits".
It give sens to the notation `x:pac(db, 73):pac(da, 42)`.

More specifically the new PAC field is added with the following strategy:

    `result = (ptr<63:56> eor pac<63:56>) : x<55> : (ptr<54:bottom_pac> eor
    pac<54:bottom_pac>) : ptr<bottom_pac - 1:0>`

x is the input and ptr is:
- in Authenticate it is
    `Replicate(x<55>, ...) : x<bottom_pac-1:0>`
- in InserPAC it is
    `Replicate(selbit, ...) : x<bottom_pac-1:0>`
  with selbit eaual to 0 if we only support one VA range, otherwise `x<55>` if
  `FEAT_CONSTPACFIELD` is activated, else `x<63>`

In the following part I assume that the feature `FEAT_CONSPACFIELD` is implemented,
otherwise `x:pac(da):pac(da)` is some times eaual to `x`, sometimes not
depending only of the value of the 63-th bit of `x:pac(da)` as I ignore address
tagging extensions (so the property hold with a probability 1/2, and we clearly
don't want to model this kind of behaviour).

So with `FEAT_CONSTPACFIELD` implemented the pac fields can be interpreted as a
set of pac fields, and adding a new PAC field can either add it to the set if
it not already present, otherwise it is removed from the set.

Then Authenticate has two possible implementation, it may return the same result
as InsertPAC, in this case if the keys doesn't match, then it return a
non-canonical values except for some hash collisions. Or it may raise an
exception if `FEAT_FPAC` is implemented.

We can write the same table:

|       | x               | x:pac(da)        | x:pac(ia)          |
|:------|:----------------|:-----------------|:-------------------|
| LDR   | mem[x]          | TranslationFault | TranslationFault   |
| PACDA | x:pad(da)       | x                | x:pac(da):pac(ia)  |
| AUTDA | x:pad(da)       | x                | x:pac(da):pac(ia)  |
| XPACD | x               | x                | x                  |

In this version I assume that `FEAT_FPAC` is not implemented, otherwise `AUTDA`
must raise a fault except if the input is `x:pac(da)`.

In particular `XPACD` always return the original pointer because the bit 55 is
keep by all the operations on the PAC field (`PAC*` and `AUT*`). In particular
it can't produce a non deterministic pointer.

## Implementation in herd7

I think using `FEAT_Pauth2` with `FEAT_CONSTPACFIELD` in herd7 is
one of the best option because it never produce a non deterministic pointer, or
something of the form `x:non-canonical`. But it is more complexe than
`FEAT_Pauth` because the implementation must represent sets of PAC fields, and
implement the XOR operation. Also because of the same reason we need a bigger
set of unit tests to check each edge-case in the implementation.

Also all the PAC fields in the implementation must contain the offset of the
virtual address at the time we compute the virtual address in addition to the
keys, such that `((x:pac(da) + 42):pac(da) - 42):pac(da) == (x+42):pac(da)` with
`FEAT_Pauth2` and `FEAT_CONSTPACFIELD`.

[^1] see aarch64/functions/pac/addpac/InsertPAC
