An online tutorial can be found in this [link](https://diy.inria.fr) that covers `diy7`, `diycross7` and `diyone7`.
We document some extra information here.

# Tool `diy7` internal filter
Tool `diy7` enforces some filter by default, that is, function `choice_default` in source code file [gen/alt.ml](alt.ml).
Given an unfinished cycle $[r_1, r_2, r_3, ..., r_n]$, the function defines next allowed relaxation $r_{n+1}$. 
In the case of composite relaxation, e.g., $[r_{n+1}, r_{n+2}, ..., r_k]$, it checks against the first primitive relaxation, (i.e., non-composite relaxation), $r_{n+1}$.
If the filter `choice_default` passes, returning `true`, the new unfinished/finished cycle will be, $[r_1, r_2, r_3, ..., r_n, r_{n+1}]$ or $[r_1, r_2, r_3, ..., r_n, r_{n+1}, r_{n+2}, ..., r_k]$.
To summarise, given two primitive relaxations, `lhs` and `rhs`, the following cases pass the default filter `choice_default lhs rhs` in `AArch64`:

| `lhs` | `rhs` |
|-------|-------|
| internal communication edge or insert edge | dependency edge or program order edge between different locations such as `PodWR` |
| dependency edge or program order edge between different locations such as `PodWR` | internal communication edge or insert edge |
| dependency edge between different locations such as `DpAddrdW` | program order edge between different locations such as `PodWR`, or same-location write edge such as `PosWW`, which is treated like `Coi` |
| dependency edge between different locations such as `DpAddrdR` | same-location read edge such as `PosRW`, which is treated like `Fri` |
| program order edge between different locations such as `PodWR` | dependency edge between different locations such as `DpAddrdW` |
| same-location write-read edge such as `PosWR`, treated like `Rfi` | dependency edge between different locations such as `DpAddrdW` |
| RMW edge | any edge |
| any edge | RMW edge |
| external edge | any edge |
| any edge | external edge |

The command `diy7 -filter-check <lhs> <rhs>` provides the filter result between `<lhs>` and `<rhs>`.
