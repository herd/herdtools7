  $ uoaim7 --def -set-libdir ./libdir data/iico_order.txt
  let iico_order = [M]; ~iico_ctrl & ~iico_data & ??? & same-instance; [M]
  $ uoaim7 --def -set-libdir ./libdir data/pick_dtrm.txt
  let rec pick-dtrm = dtrm | iico_ctrl | pick-dtrm; pick-dtrm
  $ uoaim7 --def -set-libdir ./libdir data/same_loc.txt
  let same-loc = [~??? & M]; ???; [~??? & M] | [???]; ???; [???] | [???]; same-low-order-bits | same-low-order-bits; [???] | same-low-order-bits & (???^-1; ???; ???)
  $ uoaim7 --def -set-libdir ./libdir data/pick_data_dep.txt
  let pick-data-dep = [Exp & R]; ~same-instance & (pick-basic-dep; [DATA]; (iico_data | iico_ctrl | (iico_data | iico_ctrl)+)); [Exp & W]
