This note present multiple possible implementations of the authentication
instruction in ASL and their consequence on the generated dependencies.
 All the presented graph show the result of doing

 ```
AArch64 Aut*
{ 0:x0=...; 0:x1=... }
P0             ;
  autda x0, x1 ;
 ```

Note: Those programs doesn't represent exactly an `aut*` instruction, in
particular the output values are not representative because I simplify the
`Authenticate()` procedure by removing some useless code due to runtime errors,
but the dependencies are the same.

## Current FPAC implementation

The pseudocode of `FEAT_PAuth2` with `FEAT_FPAC` look like this:

```
PAC = ComputePAC(original_ptr, modifier, ...)

result = ptr;
result<63:56> = result<63:56> EOR PAC<63:56>;
if (bottom_PAC_bit < 55) then
    result<54:bottom_PAC_bit> = result<54:bottom_PAC_bit> EOR PAC<54:bottom_PAC_bit>;

if (result<63:bottom_PAC_bit> != Replicate(result<55>, 64-bottom_PAC_bit)) then
    raise a fault;
return result;
```

so in case of success theis no control dependency, only a data dependency:

![`FPAC` success](fpac_without_else_success.png)

Note: the `iico_ctrl` is due to another "if", I think this is the test of the
activation of the key "DA" that I replaced by an `if TRUE then ... else ...`.

## FPAC with an additional iico_ctrl

Here is a modified version of `FEAT_FPAC` to add a control dependency in case of
success, this control dependency has no impact on the final execution because
`pick-basic-dep` is a subrelation of `basic-dep` but it's interesting to see
that this additional `else` branch change the generated dependencies.

```
PAC = ComputePAC(original_ptr, modifier, ...)

result = ptr;
result<63:56> = result<63:56> EOR PAC<63:56>;
if (bottom_PAC_bit < 55) then
    result<54:bottom_PAC_bit> = result<54:bottom_PAC_bit> EOR PAC<54:bottom_PAC_bit>;

if (result<63:bottom_PAC_bit> != Replicate(result<55>, 64-bottom_PAC_bit)) then
    raise a fault;
else
    result = result;
return result;
```

![modified `FPAC` success](fpac_with_result_eq_result_success.png)

## FPAC without iico_data

Something we may expect and observe in the ARM Arm is that with `FEAT_FPAC`,
authentication doesn't generate an `iico_data` dependency if it succede. This
allow to predict if the authentication will pass. And this will also add an
additional `iico_ctrl` dependency using. Here is an example of implementation in
ASL:

```
PAC = ComputePAC(original_ptr, modifier, ...)

result = ptr;
result<63:56> = result<63:56> EOR PAC<63:56>;
if (bottom_PAC_bit < 55) then
    result<54:bottom_PAC_bit> = result<54:bottom_PAC_bit> EOR PAC<54:bottom_PAC_bit>;

if (result<63:bottom_PAC_bit> != Replicate(result<55>, 64-bottom_PAC_bit)) then
    raise a fault;
else
    result = original_ptr;
return result;
```

![predicted `FPAC`](fpac_with_result_eq_target_success.png)

## `FEAT_PAuth2` without `FEAT_FPAC`

The current implementation look like this (the fpac branch can be ignored
because it doesn't have a else branch, so it doesn't affect the memory model):

```
PAC = ComputePAC(original_ptr, modifier, ...)

result = ptr;
result<63:56> = result<63:56> EOR PAC<63:56>;
if (bottom_PAC_bit < 55) then
    result<54:bottom_PAC_bit> = result<54:bottom_PAC_bit> EOR PAC<54:bottom_PAC_bit>;
return result;
```

![`PAuth2` success without `FPAC`](no_fpac_success.png)

But an alternative implementation may "predict" if the authentication will pass
and add an `iico_ctrl` depedency if the test pass, and an `iicp_data` only if
the test fail:

```
PAC = ComputePAC(original_ptr, modifier, ...)

result = ptr;
result<63:56> = result<63:56> EOR PAC<63:56>;
if (bottom_PAC_bit < 55) then
    result<54:bottom_PAC_bit> = result<54:bottom_PAC_bit> EOR PAC<54:bottom_PAC_bit>;

if (result<63:bottom_PAC_bit> != Replicate(result<55>, 64-bottom_PAC_bit)) then
    result = result;
else
    (* Here we predict that the test pass by returning something that doesn't depends on the modifier *)
    result = original_ptr;
return result;
```

![predicted `PAuth2` success](no_fpac_predicted_success.png)
![predicted `PAuth2` failure](no_fpac_predicted_fail.png)


## No `FEAT_PAuth2` but only `FEAT_PAuth`

The current implementation of PAuth1 look like this and only add `iico_ctrl`
dependencies so it can "predict" if the authentication will fail:

```
PAC = ComputePAC(original_ptr, modifier, ...)

if PAC<54:bottom_PAC_bit> == ptr<54:bottom_PAC_bit> && PAC<63:56> == ptr<63:56> then
    result = original_ptr;
else
    error_core = key_number:Not(key_number);
    result = original_ptr<63>:error_core:original_ptr<60:0>;
return result;
```

![`PAuth1` success](pauth1_success.png)
![`PAuth1` failure](pauth1_fail.png)

# Consequences

Some of those implementations represent the dependencies between the read event
of `Xn` (the register containing the modifier) and the final write event in the
`Xd` register (the register containing the pointer).

- If the dependency is an `iico_data` dependency then a `basic-dep` dependency
    is generated because `basic-dep = [Exp & R | Rreg]; dtrm?` with `iico-data`
    a sub-relation of `dtrm`. As a consequence this program is forbidden

```
AArch64 TestBasicDependency (in case of a AUT* success)
{
  0:X1=x; 0:X3=y;
  1:X1=x; 1:X3=y;
}
P0          | P1          ;
            | MOV X4,#1   ;
            | PACDB X1,X4 ; (* X1 contain pacdb(x,1) *)
MOV X0,#1   | LDR X2,[X3] ;
STR X0,[X1] | AUTDB X1,X2 ; (* write x in X1 in case of success *)
DMB ST      |             ;
MOV X2,#1   |             ;
STR X2,[X3] | LDR X0,[X1] ;
exists 1:X2=1 /\ 1:X0=0 /\ ~Fault(P1)
(* Kind: Forbidden, in case of a iico-data *)
(*       Allowed, otherwise *)
```

```
AArch64 TestBasicDependency (in case of a AUT* failure without FEAT_FPAC)
{
  0:X1=x; 0:X3=y;
  1:X1=x; 1:X3=y;
}
P0          | P1          ;
MOV X0,#1   | LDR X2,[X3] ;
STR X0,[X1] | AUTDB X1,X2 ; (* write x:non-canonical in X1 in case of failure *)
DMB ST      | XPACD X1    ; (* make x canonical in X1 *)
MOV X2,#1   |             ;
STR X2,[X3] | LDR X0,[X1] ;
exists 1:X2=1 /\ 1:X0=0 /\ ~Fault(P1)
(* Kind: Forbidden, in case of a iico-data *)
(*       Allowed, otherwise *)
```

- If the dependency is an `iico-ctrl` dependency then the previous test is
    allowed but the following test is forbidden because `iico-ctrl` is a
    sub-relation of `pick-dtrm` and `pick-basic-dep = [Exp&R | Rreg]; pick-dtrm?`:

```
AArch64 TestPickBasicDependency (in case of a AUT* success)
{
  0:X1=x; 0:X3=y;
  1:X1=x; 1:X3=y;
}
P0          | P1          ;
            | MOV X4,#1   ;
            | PACDB X1,X4 ; (* X1 contain pacdb(x,1) *)
MOV X0,#1   | LDR X2,[X3] ;
STR X0,[X1] | AUTDB X1,X2 ; (* write x in X1 in case of success *)
DMB ST      |             ;
MOV X2,#1   | MOV X0,#2   ;
STR X2,[X3] | STR X0,[X1] ;
exists 1:X2=1 /\ ~([x]=2) /\ ~Fault(P1)
(* Kind: Forbidden *)
```

```
AArch64 TestPickBasicDependency (in case of a AUT* failure without FEAT_FPAC)
{
  0:X1=x; 0:X3=y;
  1:X1=x; 1:X3=y;
}
P0          | P1          ;
MOV X0,#1   | LDR X2,[X3] ;
STR X0,[X1] | AUTDB X1,X2 ; (* write a x:non-canonical in X1 in case of faliure *)
DMB ST      | XPACD X1    ; (* make x canonical in X1 *)
MOV X2,#1   | MOV X0,#2   ;
STR X2,[X3] | STR X0,[X1] ;
exists 1:X2=1 /\ ~([x]=2) /\ ~Fault(P1)
(* Kind: Forbidden *)
```

Note that those tests are also forbidded for an `iico-data` dependency because
`pick-basic-dep` is a sub-relation of `basic-dep`.

- The last thing we may test is the behaviour of `AUT*` in case of failure.
