Alignment filter behaviour between local `Pos**` and internal communication in `diy7` in `default` mode
  $ diy7 -arch AArch64 -filter-check Rfi DpAddrdW
  Sequence `Rfi` `DpAddrdW` passes the internal filter in mode `default`
  $ diy7 -arch AArch64 -filter-check PosWR DpAddrdW
  Sequence `PosWR` `DpAddrdW` passes the internal filter in mode `default`
  $ diy7 -arch AArch64 -filter-check DpAddrdR Fri
  Sequence `DpAddrdR` `Fri` passes the internal filter in mode `default`
  $ diy7 -arch AArch64 -filter-check DpAddrdR PosRW
  Sequence `DpAddrdR` `PosRW` passes the internal filter in mode `default`
  $ diy7 -arch AArch64 -filter-check DpAddrdW Coi
  Sequence `DpAddrdW` `Coi` passes the internal filter in mode `default`
  $ diy7 -arch AArch64 -filter-check DpAddrdW PosWW
  Sequence `DpAddrdW` `PosWW` passes the internal filter in mode `default`

Alignment filter behaviour between local `Pos**` and internal communication in `diy7` in `free` mode
  $ diy7 -arch AArch64 -mode free -filter-check Rfi DpAddrdW
  Sequence `Rfi` `DpAddrdW` passes the internal filter in mode `free`
  $ diy7 -arch AArch64 -mode free -filter-check PosWR DpAddrdW
  Sequence `PosWR` `DpAddrdW` passes the internal filter in mode `free`
  $ diy7 -arch AArch64 -mode free -filter-check DpAddrdR Fri
  Sequence `DpAddrdR` `Fri` passes the internal filter in mode `free`
  $ diy7 -arch AArch64 -mode free -filter-check DpAddrdR PosRW
  Sequence `DpAddrdR` `PosRW` passes the internal filter in mode `free`
  $ diy7 -arch AArch64 -mode free -filter-check DpAddrdW Coi
  Sequence `DpAddrdW` `Coi` passes the internal filter in mode `free`
  $ diy7 -arch AArch64 -mode free -filter-check DpAddrdW PosWW
  Sequence `DpAddrdW` `PosWW` passes the internal filter in mode `free`

Alignment filter behaviour between local `Pos**` and internal communication in `diy7` in `sc` mode
  $ diy7 -arch AArch64 -mode sc -filter-check Rfi DpAddrdW
  Sequence `Rfi` `DpAddrdW` passes the internal filter in mode `sc`
  $ diy7 -arch AArch64 -mode sc -filter-check PosWR DpAddrdW
  Sequence `PosWR` `DpAddrdW` passes the internal filter in mode `sc`
  $ diy7 -arch AArch64 -mode sc -filter-check DpAddrdR Fri
  Sequence `DpAddrdR` `Fri` passes the internal filter in mode `sc`
  $ diy7 -arch AArch64 -mode sc -filter-check DpAddrdR PosRW
  Sequence `DpAddrdR` `PosRW` passes the internal filter in mode `sc`
  $ diy7 -arch AArch64 -mode sc -filter-check DpAddrdW Coi
  Sequence `DpAddrdW` `Coi` passes the internal filter in mode `sc`
  $ diy7 -arch AArch64 -mode sc -filter-check DpAddrdW PosWW
  Sequence `DpAddrdW` `PosWW` passes the internal filter in mode `sc`
