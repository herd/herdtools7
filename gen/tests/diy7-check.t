`PosWR` behaves like `Rfi` in the `diy7` filter
  $ diy7 -arch AArch64 -filter-check Rfi DpAddrdW
  Sequence `Rfi` `DpAddrdW` passes the internal filter in mode `default`
  $ diy7 -arch AArch64 -filter-check PosWR DpAddrdW
  Sequence `PosWR` `DpAddrdW` passes the internal filter in mode `default`

`PosRW` behaves like `Fri` in the `diy7` filter
  $ diy7 -arch AArch64 -filter-check DpAddrdR Fri
  Sequence `DpAddrdR` `Fri` passes the internal filter in mode `default`
  $ diy7 -arch AArch64 -filter-check DpAddrdR PosRW
  Sequence `DpAddrdR` `PosRW` passes the internal filter in mode `default`

`PosWW` behaves like `Coi` in the `diy7` filter
  $ diy7 -arch AArch64 -filter-check DpAddrdW Coi
  Sequence `DpAddrdW` `Coi` passes the internal filter in mode `default`
  $ diy7 -arch AArch64 -filter-check DpAddrdW PosWW
  Sequence `DpAddrdW` `PosWW` passes the internal filter in mode `default`

`PosWR` behaves like `Rfi` in `free` mode
  $ diy7 -arch AArch64 -mode free -filter-check Rfi DpAddrdW
  Sequence `Rfi` `DpAddrdW` passes the internal filter in mode `free`
  $ diy7 -arch AArch64 -mode free -filter-check PosWR DpAddrdW
  Sequence `PosWR` `DpAddrdW` passes the internal filter in mode `free`

`PosRW` and `PosWW` follow `Fri` and `Coi` in `sc` mode
  $ diy7 -arch AArch64 -mode sc -filter-check DpAddrdR Fri
  Sequence `DpAddrdR` `Fri` passes the internal filter in mode `sc`
  $ diy7 -arch AArch64 -mode sc -filter-check DpAddrdR PosRW
  Sequence `DpAddrdR` `PosRW` passes the internal filter in mode `sc`
  $ diy7 -arch AArch64 -mode sc -filter-check DpAddrdW Coi
  Sequence `DpAddrdW` `Coi` passes the internal filter in mode `sc`
  $ diy7 -arch AArch64 -mode sc -filter-check DpAddrdW PosWW
  Sequence `DpAddrdW` `PosWW` passes the internal filter in mode `sc`
