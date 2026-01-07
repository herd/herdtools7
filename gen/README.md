A online tutorial can be found in this [link](https://diy.inria.fr) that covers `diy7`, `diycross7` and `diyone7`.
We document some extra information here.

# Tool `diy7` internal filter
Tool `diy7` enforce some filter by default, that is, function `choice_default` in source code file [gen/alt.ml](alt.ml).
Given an unfinished cycle $[r_1, r_2, r_3, ..., r_n]$, the function defines next allowed primitive relaxation $r_{n+1}$. 
In the case of compound relaxation, it checks against the first primitive relaxation, e.g. $[r_{n+1}, r_{n+2}, ..., r_k]$.
If the filter `choice_default` passes, returning `true`, the new unfinished/finished cycle will be, $[r_1, r_2, r_3, ..., r_n, r_{n+1}]$ or $[r_1, r_2, r_3, ..., r_n, r_{n+1}, r_{n+2}, ..., r_k]$.
Before expaining the filter, let introduce some terminology on relaxations, or specifically, edges, where concrete examples are from `-arch AArch64`:

- internal communication edge, namely `Coi`, `Fri` and `Rfi`
- external (communication) edge, namely `Coe`, `Fre`, `Rfe` and `Hat`
- insert edge, that is, fence without memory events, e.g., `DMB.SY` and `DMB.LD`
- dependency edge, e.g. `DpAddrdW` and `DpCtrlsR`
- program order edge that connects two memory events without any extra restrict, e.g. `PosRW` and `PodWR`
- read-modify-write (RMW) edge, e.g. `LxSx` and `Amo.Cas`.

Given two primitive relaxations, `lhs` and `rhs`, the following cases pass the filter `choice_default lhs rhs`:

| `lhs` | `rhs` |
|-------|-------|
| internal communication edge or insert edge | dependency edge or program order edge between different loations such as `PodWR` |
| dependency edge or program order edge between different loations such as `PodWR` | internal communication edge or insert edge |
| dependency edge between different locations such as `DpAddrdW` | program order edge between different loations such as `PodWR` |
| program order edge between different loations such as `PodWR` | dependency edge between different locations such as `DpAddrdW` |
| internal read-from edge `Rfi` | program order edge between the same location such as `PosWR` |
| program order edge between the same location such as `PosWR` | internal read-from edge `Rfi` |
| RMW edge | any edge |
| any edge | RMW edge |
| external edge | any edge |
| any edge | external edge |
