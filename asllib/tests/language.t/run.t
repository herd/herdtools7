Utils:
Make sure that paths to stdlib.asl are hidden
  $ export BUILD_PATH_PREFIX_MAP="DUNE_SOURCEROOT=$DUNE_SOURCEROOT:$BUILD_PATH_PREFIX_MAP"

Test Imhys.asl:
  $ aslref Imhys.asl
  File Imhys.asl, line 5, characters 11 to 16:
  ASL Execution error: Assertion failed: FALSE
  [1]

Test Iwxgp.asl:
  $ aslref Iwxgp.asl
  File Iwxgp.asl, line 5, characters 11 to 16:
  ASL Execution error: Assertion failed: FALSE
  [1]

Test Rdpzk.asl:
  $ aslref Rdpzk.asl
  File Rdpzk.asl, line 8, characters 11 to 16:
  ASL Execution error: Assertion failed: FALSE
  [1]

Test Rirnq.asl:
  $ aslref Rirnq.asl
  File Rirnq.asl, line 9, characters 11 to 16:
  ASL Execution error: Assertion failed: FALSE
  [1]
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rirnq.asl
  // CHECK: runtime_exception

Test Rmhfw.asl:
  $ aslref Rmhfw.asl
  File Rmhfw.asl, line 8, characters 11 to 16:
  ASL Execution error: Assertion failed: FALSE
  [1]

Test Rvddg.asl:
  $ aslref Rvddg.asl
  File ASL Standard Library, line 259, characters 9 to 14:
  ASL Execution error: Assertion failed: FALSE
  [1]

Test Ibklj.asl:
  $ aslref Ibklj.asl

Test Icxps.asl:
  $ aslref Icxps.asl
  Hello world
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Icxps.asl
  // CHECK: Hello world

Test Rbsqr.asl
  $ aslref Rbsqr.asl
  File Rbsqr.asl, line 10, characters 4 to 5:
  ASL Typing error: cannot assign to immutable storage "a".
  [1]

Test Rchth.asl:
  $ aslref Rchth.asl
  [1]

Test Rjwph.asl:
  $ aslref Rjwph.asl
  Hello World
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rjwph.asl
  // CHECK: Hello World

Test Rxnsk.asl:
  $ aslref Rxnsk.asl
  File Rxnsk.asl, line 12, characters 10 to 11:
  ASL Error: Undefined identifier: 'a'
  [1]

Test Dgvbk.asl:
  $ aslref Dgvbk.asl

Test Inxkd.asl:
  $ aslref Inxkd.asl

Test Rfrwd.asl:
  $ aslref Rfrwd.asl

Test Rswqd.asl:
  $ aslref Rswqd.asl
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rswqd.asl
  // CHECK-NOT: 10

Test Ihjcd.asl:
  $ aslref Ihjcd.asl
  1000000
  1000000
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Ihjcd.asl
  // CHECK: 1000000
  // CHECK-NEXT: 1000000

Test Ipbpq.asl:
  $ aslref Ipbpq.asl
  TRUE
  TRUE
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Ipbpq.asl
  // CHECK: TRUE
  // CHECK-NEXT: TRUE

Test Iqczx.asl:
  $ aslref Iqczx.asl
  '1111111111111111'
  '1111111111111111'
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Iqczx.asl
  // CHECK: 0xFFFF
  // CHECK-NEXT: 0xFFFF

Test Rhyfh.asl:
  $ aslref Rhyfh.asl
  10
  10
  10
  10
  10
  10
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rhyfh.asl
  // CHECK: 10
  // CHECK-NEXT: 10
  // CHECK-NEXT: 10
  // CHECK-NEXT: 10
  // CHECK-NEXT: 10
  // CHECK-NEXT: 10

Test Rmxps.asl:
  $ aslref Rmxps.asl
  FALSE
  TRUE
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rmxps.asl
  // CHECK: FALSE
  // CHECK-NEXT: TRUE

Test Rpbfk.asl:
  $ aslref Rpbfk.asl
  '0000'
  '1111'
  '0000'
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rpbfk.asl
  // CHECK: 0x0
  // CHECK-NEXT: 0xF
  // CHECK-NEXT: 0x0

Test Rqqbb.asl:
  $ aslref Rqqbb.asl
  10.
  10.
  10.
  10.
  10.
  10.
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rqqbb.asl
  // CHECK: 10.0
  // CHECK-NEXT: 10.0
  // CHECK-NEXT: 10.0
  // CHECK-NEXT: 10.0
  // CHECK-NEXT: 10.0
  // CHECK-NEXT: 10.0

Test Rrymd.asl:
  $ aslref Rrymd.asl

Test Rzrvy.asl:
  $ aslref Rzrvy.asl
  hello
  wor"ld
  te\st
  bre
  ak
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rzrvy.asl
  // CHECK: hello
  // CHECK-NEXT: wor"ld
  // CHECK-NEXT: te\st
  // CHECK-NEXT: bre
  // CHECK-NEXT: ak

Test Ihvlx.asl:
  $ aslref Ihvlx.asl

Test Iscly.asl:
  $ aslref Iscly.asl
  File Iscly.asl, line 5, characters 8 to 12:
  ASL Error: Cannot parse.
  [1]

Test Rhprd.asl:
  $ aslref Rhprd.asl

Test Rjgrk.asl:
  $ aslref Rjgrk.asl

Test Rqmdm.asl:
  $ aslref Rqmdm.asl

Test Rgfsh.asl:
  $ aslref Rgfsh.asl
  File Rgfsh.asl, line 8, characters 4 to 25:
  ASL Error: Undefined identifier: 'String'
  [1]

Test Ddpxj.asl:
  $ aslref Ddpxj.asl

Test Ipgss.asl:
  $ aslref Ipgss.asl
  0
  0
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Ipgss.asl
  // CHECK: 0
  // CHECK-NEXT: 0

Test Rdgjt.asl:
  $ aslref Rdgjt.asl

Test Rhhcd.asl:
  $ aslref Rhhcd.asl

Test Rjjcj.asl:
  $ aslref Rjjcj.asl

Test Rpxrr.asl:
  $ aslref Rpxrr.asl

Test Rwgvr.asl:
  $ aslref Rwgvr.asl
  0
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rwgvr.asl
  // CHECK: 0

Test Ryhnv.asl:
  $ aslref Ryhnv.asl

Test Ihmrk.asl:
  $ aslref Ihmrk.asl
  0
  FALSE
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Ihmrk.asl
  // CHECK: 0
  // CHECK-NEXT: FALSE

Test Imqwb.asl:
  $ aslref Imqwb.asl

Test Rcgwr.asl:
  $ aslref Rcgwr.asl

Test Rjhkl.asl:
  $ aslref Rjhkl.asl
  File Rjhkl.asl, line 7, characters 4 to 5:
  ASL Typing error: integer {10} does not subtype any of: bits(-), record {  },
    exception {  }.
  [1]

Test Rqwsq.asl:
  $ aslref Rqwsq.asl
  0
  0
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rqwsq.asl
  // CHECK: 0
  // CHECK-NEXT: 0

Test Rtvpr.asl:
  $ aslref Rtvpr.asl

Test Dwgqs.asl:
  $ aslref Dwgqs.asl

Test Itfps.asl:
  $ aslref Itfps.asl

Test Rdlxv.asl:
  $ aslref Rdlxv.asl
  10
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rdlxv.asl
  // CHECK: 10

Test Rdxwn.asl:
  $ aslref Rdxwn.asl

Test Rmbrm.asl:
  $ aslref Rmbrm.asl
  0
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rmbrm.asl
  // CHECK: 0

Test Rmbrm-2.asl:
  $ aslref Rmbrm-2.asl
  ASL Error: Undefined identifier: 'main'
  [1]

Test Rmdzd.asl:
  $ aslref Rmdzd.asl
  File Rmdzd.asl, line 6, character 0 to line 9, character 2:
  ASL Typing error: cannot declare already declared element "b".
  [1]

Test Rwfmf.asl:
  $ aslref Rwfmf.asl

Test Dqxyc.asl:
  $ aslref Dqxyc.asl

Test Ihlbl.asl:
  $ aslref Ihlbl.asl

Test Rbwdx.asl:
  $ aslref Rbwdx.asl
  10

Test Rchkr.asl:
  $ aslref Rchkr.asl
  File Rchkr.asl, line 6, character 0 to line 9, character 2:
  ASL Typing error: cannot declare already declared element "b".
  [1]

Test Rkgxl.asl:
  $ aslref Rkgxl.asl

Test Rmghv.asl:
  $ aslref Rmghv.asl

Test Rsvjb.asl:
  $ aslref Rsvjb.asl
  0

Test Rmhwm.asl:
  $ aslref Rmhwm.asl

Test Dknbd.asl:
  $ aslref Dknbd.asl

Test Dnzwt.asl:
  $ aslref Dnzwt.asl

Test Dpqck.asl:
  $ aslref Dpqck.asl

Test Rgvzk.asl:
  $ aslref Rgvzk.asl

Test Rgrvj.asl:
  $ aslref Rgrvj.asl
  File Rgrvj.asl, line 8, characters 11 to 22:
  ASL Error: Cannot parse.
  [1]

Test Gpfrq.asl:
  $ aslref Gpfrq.asl

Test Dztpp.asl:
  $ aslref Dztpp.asl

Test Dzxss.asl:
  $ aslref Dzxss.asl

Test Ighgk.asl:
  $ aslref Ighgk.asl
  File Ighgk.asl, line 11, characters 34 to 35:
  ASL Error: Cannot parse.
  [1]

Test Izddj.asl:
  $ aslref Izddj.asl
  File Izddj.asl, line 6, characters 34 to 35:
  ASL Error: Cannot parse.
  [1]

Test Rbsmk.asl:
  $ aslref Rbsmk.asl
  File Rbsmk.asl, line 8, characters 4 to 25:
  ASL Typing error: constrained integer expected, provided integer
  [1]

Test Rcztx.asl:
  $ aslref Rcztx.asl

Test Rgwcp.asl:
  $ aslref Rgwcp.asl

Test Rhjpn.asl:
  $ aslref Rhjpn.asl

Test Rlsnp.asl:
  $ aslref Rlsnp.asl

Test Rtphr.asl:
  $ aslref Rtphr.asl
  File Rtphr.asl, line 6, characters 34 to 35:
  ASL Error: Cannot parse.
  [1]

Test Rwjyh.asl:
  $ aslref Rwjyh.asl

Test Ibbqr.asl:
  $ aslref Ibbqr.asl

Test Icdvy.asl:
  $ aslref Icdvy.asl

Test Ikfcr.asl:
  $ aslref Ikfcr.asl

Test Iwbwl.asl:
  $ aslref Iwbwl.asl

Test Iwlpj.asl:
  $ aslref Iwlpj.asl

Test Rfwmm.asl:
  $ aslref Rfwmm.asl

Test Rlyds.asl:
  $ aslref Rlyds.asl

Test Rrlqp.asl:
  $ aslref Rrlqp.asl

Test Rsvdj.asl:
  $ aslref Rsvdj.asl

Test Rtznr.asl:
  $ aslref Rtznr.asl

Test Iwvqz.asl:
  $ aslref Iwvqz.asl

Test Rcftd.asl:
  $ aslref Rcftd.asl
  4
  -4
  -4
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rcftd.asl
  // CHECK: 4
  // CHECK-NEXT: -4
  // CHECK-NEXT: 4

Test Rnjdz.asl:
  $ aslref Rnjdz.asl
  0
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rnjdz.asl
  // CHECK: 0

Test Rqggh.asl:
  $ aslref Rqggh.asl

Test Ihjbh.asl:
  $ aslref Ihjbh.asl
  99999999999999999999999999999999999999999999999
  -99999999999999999999999999999999999999999999999
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Ihjbh.asl
  // CHECK: 99999999999999999999999999999999999999999999999
  // CHECK-NEXT: -99999999999999999999999999999999999999999999999

Test Dcqxl.asl:
  $ aslref Dcqxl.asl
  3.14159
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Dcqxl.asl
  // CHECK: 3.141590e+0

Test Ijqpk.asl:
  $ aslref Ijqpk.asl
  10.
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Ijqpk.asl
  // CHECK: 10

Test Iwjcl.asl:
  $ aslref Iwjcl.asl
  1
  2
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Iwjcl.asl
  // CHECK: 1
  // CHECK-NEXT: 2

Test Iyftf.asl:
  $ aslref Iyftf.asl

Test Rgycg.asl:
  $ aslref Rgycg.asl
  0.
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rgycg.asl
  // CHECK: 0.0

Test Rxcjd.asl:
  $ aslref Rxcjd.asl
  1e+69
  -1e+69
  1e-70
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rxcjd.asl
  // CHECK: 1.000000e+69
  // CHECK-NEXT: -1.000000e+69
  // CHECK-NEXT: 1.000000e-70

Test Idmnl.asl:
  $ aslref Idmnl.asl
  TRUE
  TRUE
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Idmnl.asl
  // CHECK: TRUE
  // CHECK-NEXT: TRUE

Test Rwkcy.asl:
  $ aslref Rwkcy.asl
  

For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rwkcy.asl
  // CHECK:

Test Dyzbq.asl:
  $ aslref Dyzbq.asl

Test Imzxl.asl:
  $ aslref Imzxl.asl
  File Imzxl.asl, line 5, character 0 to line 8, character 3:
  ASL Typing error: cannot declare already declared element "A".
  [1]

Test Iprpy.asl:
  $ aslref Iprpy.asl
  File Iprpy.asl, line 8, characters 21 to 27:
  ASL Typing error: Illegal application of operator <= on types enum and enum
  [1]

Test Iqmwt.asl:
  $ aslref Iqmwt.asl

Test Rdwsp.asl:
  $ aslref Rdwsp.asl
  File Rdwsp.asl, line 6, characters 13 to 30:
  ASL Typing error: cannot declare already declared element "A".
  [1]

Test Rhjyj.asl:
  $ aslref Rhjyj.asl

Test Rlccn.asl:
  $ aslref Rlccn.asl
  TRUE
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rlccn.asl
  // CHECK: TRUE

Test Rcpck.asl:
  $ aslref Rcpck.asl
  FALSE
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rcpck.asl
  // CHECK: FALSE

Test Dbvgk.asl:
  $ aslref Dbvgk.asl
  File Dbvgk.asl, line 9, characters 33 to 34:
  ASL Error: Cannot parse.
  [1]

Test Dcbqk.asl:
  $ aslref Dcbqk.asl

Test Dnrwc.asl:
  $ aslref Dnrwc.asl

Test Dwszm.asl:
  $ aslref Dwszm.asl

Test Imrhk.asl:
  $ aslref Imrhk.asl

Test Iybhf.asl:
  $ aslref Iybhf.asl

Test Rfzsd.asl:
  $ aslref Rfzsd.asl
  File Rfzsd.asl, line 8, characters 15 to 16:
  ASL Error: Cannot parse.
  [1]

Test Rljbg.asl:
  $ aslref Rljbg.asl
  File Rljbg.asl, line 7, characters 16 to 17:
  ASL Error: Cannot parse.
  [1]

Test Rnfbn.asl:
  $ aslref Rnfbn.asl
  File Rnfbn.asl, line 9, characters 17 to 18:
  ASL Error: Cannot parse.
  [1]

Test Rqyzd.asl:
  $ aslref Rqyzd.asl
  File Rqyzd.asl, line 8, characters 17 to 18:
  ASL Error: Cannot parse.
  [1]

Test Igqmv.asl:
  $ aslref Igqmv.asl

Test Ijskw.asl:
  $ aslref Ijskw.asl

Test Rghrp.asl:
  $ aslref Rghrp.asl
  File Rghrp.asl, line 8, characters 0 to 18:
  ASL Typing error: constrained integer expected, provided integer
  [1]

Test Rncnq.asl:
  $ aslref Rncnq.asl
  File Rncnq.asl, line 7, characters 18 to 19:
  ASL Error: Cannot parse.
  [1]

Test Rqzjs.asl:
  $ aslref Rqzjs.asl

Test Rskrk.asl:
  $ aslref Rskrk.asl

Test Iqfzh.asl:
  $ aslref Iqfzh.asl
  TRUE
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Iqfzh.asl
  // CHECK: TRUE

Test Irxlg.asl:
  $ aslref Irxlg.asl
  FALSE
  TRUE
  '101'
  '100'
  '010'
  '001'
  '001'
  '001'
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Irxlg.asl
  // CHECK: FALSE
  // CHECK-NEXT: TRUE
  // CHECK-NEXT: 0x5
  // CHECK-NEXT: 0x4
  // CHECK-NEXT: 0x2
  // CHECK-NEXT: 0x1
  // CHECK-NEXT: 0x1

Test Isqvv.asl:
  $ aslref Isqvv.asl

Test Ivmkf.asl:
  $ aslref Ivmkf.asl
  TRUE
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Ivmkf.asl
  // CHECK: TRUE

Test Ivpst.asl:
  $ aslref Ivpst.asl
  File Ivpst.asl, line 3, characters 18 to 19:
  ASL Error: Cannot parse.
  [1]

Test Rdhzt.asl:
  $ aslref Rdhzt.asl

Test Rdkgq.asl:
  $ aslref Rdkgq.asl

Test Rpmqb.asl:
  $ aslref Rpmqb.asl

Test Rqxgw.asl:
  $ aslref Rqxgw.asl
  File Rqxgw.asl, line 9, characters 14 to 15:
  ASL Error: Cannot parse.
  [1]
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rqxgw.asl
  // CHECK: TRUE

Test Rzrwh.asl:
  $ aslref Rzrwh.asl

Test Rzvpt.asl:
  $ aslref Rzvpt.asl
  '00000000000000000000000000000000'
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rzvpt.asl
  // CHECK: 0x0

Test Rzwgh.asl:
  $ aslref Rzwgh.asl

Test Iglwm.asl:
  $ aslref Iglwm.asl
  File Iglwm.asl, line 10, characters 16 to 17:
  ASL Error: Cannot parse.
  [1]

Test Imtwl.asl:
  $ aslref Imtwl.asl

Test Itzvj_a.asl:
  $ aslref Itzvj_a.asl
  File Itzvj_a.asl, line 3, characters 23 to 30:
  ASL Error: Cannot parse.
  [1]

Test Itzvj_b.asl:
  $ aslref Itzvj_b.asl

Test Rvczx.asl:
  $ aslref Rvczx.asl
  File Rvczx.asl, line 10, characters 15 to 16:
  ASL Error: Cannot parse.
  [1]

Test Ieoax.asl:
  $ aslref Ieoax.asl

Test Inhxt.asl:
  $ aslref Inhxt.asl

Test Icgyh.asl:
  $ aslref Icgyh.asl

Test Ifpvz.asl:
  $ aslref Ifpvz.asl
  File Ifpvz.asl, line 7, characters 16 to 17:
  ASL Error: Cannot parse.
  [1]

Test Iqnsd.asl:
  $ aslref Iqnsd.asl
  File Iqnsd.asl, line 18, characters 20 to 21:
  ASL Error: Cannot parse.
  [1]

Test Ibghb.asl:
  $ aslref Ibghb.asl

Test Ifzms.asl:
  $ aslref Ifzms.asl

Test Ijdcc.asl:
  $ aslref Ijdcc.asl
  File Ijdcc.asl, line 6, character 0 to line 8, character 2:
  ASL Static error: Cannot extract from bitvector of length 5 slices 7+:4.
  [1]

Test Ikgmc.asl:
  $ aslref Ikgmc.asl

Test Ikpbx.asl:
  $ aslref Ikpbx.asl

Test Iqdhp.asl:
  $ aslref Iqdhp.asl

Test Rbdjk.asl:
  $ aslref Rbdjk.asl
  File Rbdjk.asl, line 6, character 0 to line 8, character 2:
  ASL Typing error: overlapping slices 3+:(- 1).
  [1]

Test Rcgdg.asl:
  $ aslref Rcgdg.asl

Test Rchbw.asl:
  $ aslref Rchbw.asl

Test Rcnhb.asl:
  $ aslref Rcnhb.asl
  File Rcnhb.asl, line 5, character 0 to line 7, character 2:
  ASL Typing error: overlapping slices 0+:4, 1+:3.
  [1]

Test Rgqvz.asl:
  $ aslref Rgqvz.asl
  TRUE
  TRUE
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rgqvz.asl
  // CHECK: TRUE
  // CHECK-NEXT: TRUE

Test Rlghs.asl:
  $ aslref Rlghs.asl
  File Rlghs.asl, line 9, characters 4 to 5:
  ASL Error: Cannot parse.
  [1]

Test Rmpmg.asl:
  $ aslref Rmpmg.asl
  File Rmpmg.asl, line 6, character 0 to line 9, character 2:
  ASL Typing error: cannot declare already declared element "aa".
  [1]

Test Rmxyq.asl:
  $ aslref Rmxyq.asl

Test Rqcym.asl:
  $ aslref Rqcym.asl

Test Rrmtq.asl:
  $ aslref Rrmtq.asl

Test Ryypn.asl:
  $ aslref Ryypn.asl
  File Ryypn.asl, line 9, characters 27 to 28:
  ASL Error: Cannot parse.
  [1]

Test Rzjsh.asl:
  $ aslref Rzjsh.asl

Test Dwxqv.asl:
  $ aslref Dwxqv.asl

Test Iscbx.asl:
  $ aslref Iscbx.asl
  File Iscbx.asl, line 7, characters 4 to 19:
  ASL Typing error: constrained integer expected, provided integer
  [1]

Test Uxknl.asl:
  $ aslref Uxknl.asl

Test Ibyvl.asl:
  $ aslref Ibyvl.asl

Test Ijrdl.asl:
  $ aslref Ijrdl.asl

Test Iwykz.asl:
  $ aslref Iwykz.asl

Test Ihjrd.asl:
  $ aslref Ihjrd.asl
  File Ihjrd.asl, line 20, characters 0 to 14:
  ASL Typing error: multiple recursive declarations: "a", "a", "a"
  [1]

Test Ilwqq.asl:
  $ aslref Ilwqq.asl

Test Imvnz.asl:
  $ aslref Imvnz.asl
  File Imvnz.asl, line 7, characters 4 to 19:
  ASL Typing error: cannot declare already declared element "a".
  [1]

Test Rdhrc.asl:
  $ aslref Rdhrc.asl

Test Rswvp.asl:
  $ aslref Rswvp.asl

Test Ihsww.asl:
  $ aslref Ihsww.asl
  10
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Ihsww.asl
  // CHECK: 10

Output is non-deterministic
  $ aslref Ijvrm.asl
  File Ijvrm.asl, line 4, characters 0 to 18:
  ASL Typing error: multiple recursive declarations: "b", "a"
  [1]

Test Ikgks.asl:
  $ aslref Ikgks.asl

Test Imtml.asl:
  $ aslref Imtml.asl

Test Rhqzy.asl:
  $ aslref Rhqzy.asl

Test Rlpdl.asl:
  $ aslref Rlpdl.asl
  File Rlpdl.asl, line 6, characters 0 to 29:
  ASL Error: Undefined identifier: 'a'
  [1]

Test Rnxrx.asl:
  $ aslref Rnxrx.asl

Test Rsrhn.asl:
  $ aslref Rsrhn.asl
  File Rsrhn.asl, line 7, characters 0 to 28:
  ASL Typing error: a subtype of a was expected, provided string.
  [1]

Test Rzrkm.asl:
  $ aslref Rzrkm.asl

Test Rzwhp.asl:
  $ aslref Rzwhp.asl

Test Rkdks.asl:
  $ aslref Rkdks.asl

Test Rybwy.asl:
# Output is non-deterministic
  $ aslref Rybwy.asl
  File Rybwy.asl, line 9, characters 0 to 19:
  ASL Typing error: multiple recursive declarations: "other", "base"
  [1]

Test Rybwy-2.asl:  
  $ aslref Rybwy-2.asl
  File Rybwy-2.asl, line 32, characters 2 to 11:
  ASL Typing error: a subtype of TypeA was expected, provided TypeB.
  [1]

Test Rybwy-3.asl:  
  $ aslref Rybwy-3.asl
  File Rybwy-3.asl, line 9, characters 0 to 19:
  ASL Typing error: multiple recursive declarations: "other", "base"
  [1]

Test Rybwy-4.asl:  
  $ aslref Rybwy-4.asl
  ASL Error: Undefined identifier: 'main'
  [1]

Test Iglhk.asl:
  $ aslref Iglhk.asl

Test Ivylk.asl:
  $ aslref Ivylk.asl
  File Ivylk.asl, line 16, characters 16 to 17:
  ASL Error: Cannot parse.
  [1]

Test Rckgp.asl:
  $ aslref Rckgp.asl

Test Rdjmc.asl:
  $ aslref Rdjmc.asl

Test Rdlxt.asl:
  $ aslref Rdlxt.asl

Test Rfkgp.asl:
  $ aslref Rfkgp.asl
  Hello

Test Rfpmt.asl:
  $ aslref Rfpmt.asl
  30
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rfpmt.asl
  // CHECK: 30

Test Rgbnc.asl:
  $ aslref Rgbnc.asl
  0
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rgbnc.asl
  // CHECK: 0

Test Rplyx.asl:
  $ aslref Rplyx.asl

Test Rprzn.asl:
  $ aslref Rprzn.asl
  Hello

Test Rrzll.asl:
  $ aslref Rrzll.asl

Test Ivqhq.asl:
  $ aslref Ivqhq.asl

Test Iypxd.asl:
  $ aslref Iypxd.asl
  File Iypxd.asl, line 14, characters 4 to 5:
  ASL Error: Cannot parse.
  [1]

Test Rbhmy.asl:
  $ aslref Rbhmy.asl

Test Rdbzz.asl:
  $ aslref Rdbzz.asl

Test Rksqp_a.asl:
  $ aslref Rksqp_a.asl
  File Rksqp_a.asl, line 6, characters 14 to 15:
  ASL Error: Cannot parse.
  [1]

Test Rksqp_b.asl:
  $ aslref Rksqp_b.asl
  File Rksqp_b.asl, line 6, characters 19 to 20:
  ASL Error: Cannot parse.
  [1]

Test Rksqp_c.asl:
  $ aslref Rksqp_c.asl
  File Rksqp_c.asl, line 6, characters 17 to 18:
  ASL Error: Cannot parse.
  [1]

Test Rpnqj.asl:
  $ aslref Rpnqj.asl

Test Rsblx.asl:
  $ aslref Rsblx.asl

Test Rtrdj.asl:
  $ aslref Rtrdj.asl

Test Rwzjq.asl:
  $ aslref Rwzjq.asl

Test Rznth.asl:
  $ aslref Rznth.asl

Test Dgwwp.asl:
  $ aslref Dgwwp.asl

Test Ibcww.asl:
  $ aslref Ibcww.asl

Test Ifyfn.asl:
  $ aslref Ifyfn.asl

Test Rsfpm.asl:
  $ aslref Rsfpm.asl

Test Rfrdx.asl:
  $ aslref Rfrdx.asl

Test Rjgvx.asl:
  $ aslref Rjgvx.asl

Test Rrxhx.asl:
  $ aslref Rrxhx.asl

Test Dbhpj.asl:
  $ aslref Dbhpj.asl

Test Dmryb.asl:
  $ aslref Dmryb.asl

Test Ifbvh.asl:
  $ aslref Ifbvh.asl

Test Iqjnf.asl:
  $ aslref Iqjnf.asl

Test Iwhlv.asl:
  $ aslref Iwhlv.asl

Test Rlcsz.asl:
  $ aslref Rlcsz.asl
  File Rlcsz.asl, line 9, character 0 to line 12, character 3:
  ASL Typing error: setter "a" does not have a corresponding getter of
    signature  -> integer
  [1]

Test Rmbzp.asl:
  $ aslref Rmbzp.asl

Test Rmzjj.asl:
  $ aslref Rmzjj.asl

Test Rnctb.asl:
  $ aslref Rnctb.asl

Test Rqkxv.asl:
  $ aslref Rqkxv.asl

Test Rtjrh.asl:
  $ aslref Rtjrh.asl
  File Rtjrh.asl, line 3, characters 0 to 16:
  ASL Typing error: multiple recursive declarations: "_a", "_a"
  [1]

Test Djwkg.asl:
  $ aslref Djwkg.asl

Test Djwxx.asl:
  $ aslref Djwxx.asl

Test Dqmyp.asl:
  $ aslref Dqmyp.asl

Test Dvftv.asl:
  $ aslref Dvftv.asl

Test Idvsm.asl:
  $ aslref Idvsm.asl

Test Igklw.asl:
  $ aslref Igklw.asl

Test Igklw-2.asl:
  $ aslref Igklw.asl

Test Ipfng.asl:
  $ aslref Ipfng.asl

Test Itwjf.asl:
  $ aslref Itwjf.asl
  5
  5
  5
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Itwjf.asl
  // CHECK: 5
  // CHECK-NEXT: 5
  // CHECK-NEXT: 5

Test Rdfwz.asl:
  $ aslref Rdfwz.asl
  File Rdfwz.asl, line 13, character 0 to line 16, character 3:
  ASL Typing error: setter "c" does not have a corresponding getter of
    signature  -> integer
  [1]

Test Rhdgv.asl:
  $ aslref Rhdgv.asl
  File Rhdgv.asl, line 6, characters 0 to 15:
  ASL Typing error: multiple recursive declarations: "a", "a"
  [1]

Test Rjbxs.asl:
  $ aslref Rjbxs.asl

Test Rkcmk.asl:
  $ aslref Rkcmk.asl
  File Rkcmk.asl, line 7, character 0 to line 10, character 3:
  ASL Typing error: cannot declare already declared element "a".
  [1]

Test Rkfgj.asl:
  $ aslref Rkfgj.asl

Test Rptdd.asl:
  $ aslref Rptdd.asl

Test Rqcvm.asl:
  $ aslref Rqcvm.asl

Test Rwkhc_a.asl:
  $ aslref Rwkhc_a.asl

Test Rwkhc_b.asl:
  $ aslref Rwkhc_b.asl
  File Rwkhc_b.asl, line 9, characters 0 to 3:
  ASL Error: Cannot parse.
  [1]

Test Dnmfp.asl:
  $ aslref Dnmfp.asl
  File Dnmfp.asl, line 26, character 0 to line 30, character 3:
  ASL Typing error: setter "h" does not have a corresponding getter of
    signature  -> integer
  [1]

Test Itsxl.asl:
  $ aslref Itsxl.asl
  10
  20
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Itsxl.asl
  // CHECK: 10
  // CHECK-NEXT: 20

Test Ixfpv.asl:
  $ aslref Ixfpv.asl
  File Ixfpv.asl, line 13, characters 12 to 19:
  ASL Error: There are no field 'item2' on type (integer {10}, integer {20}).
  [1]
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Ixfpv.asl
  // CHECK: 10
  // CHECK-NEXT: 20

Test Rjpvl.asl:
  $ aslref Rjpvl.asl
  File Rjpvl.asl, line 16, characters 4 to 5:
  ASL Error: Cannot parse.
  [1]

Test Rkvnx.asl:
  $ aslref Rkvnx.asl
  File Rkvnx.asl, line 10, characters 4 to 5:
  ASL Typing error: integer {4} does not subtype any of: bits(-), record {  },
    exception {  }.
  [1]

Test Isbfk.asl:
  $ aslref Isbfk.asl

Test Rhdds.asl:
  $ aslref Rhdds.asl

Test Rpblf.asl:
  $ aslref Rpblf.asl

Test Igjhs.asl:
  $ aslref Igjhs.asl

Test Rwlch.asl:
  $ aslref Rwlch.asl

Test Rxbmn.asl:
  $ aslref Rxbmn.asl

Test Rycdb.asl:
  $ aslref Rycdb.asl
  10
  5
  3
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rycdb.asl
  // CHECK: 10
  // CHECK-NEXT: 5
  // CHECK-NEXT: 3

Test Iwlnm.asl:
  $ aslref Iwlnm.asl
  File Iwlnm.asl, line 6, characters 8 to 14:
  ASL Error: Cannot parse.
  [1]

Test Rzndl.asl:
  $ aslref Rzndl.asl
  TRUE
  TRUE
  FALSE
  TRUE
  FALSE
  TRUE
  TRUE
  TRUE
  TRUE
  FALSE
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rzndl.asl
  // CHECK-: TRUE
  // CHECK-NEXT: TRUE
  // CHECK-NEXT: FALSE
  // CHECK-NEXT: TRUE
  // CHECK-NEXT: FALSE
  // CHECK-NEXT: TRUE
  // CHECK-NEXT: TRUE
  // CHECK-NEXT: TRUE
  // CHECK-NEXT: TRUE
  // CHECK-NEXT: FALSE

Test Rrcsd.asl:
  $ aslref Rrcsd.asl
  File Rrcsd.asl, line 14, characters 17 to 18:
  ASL Error: Cannot parse.
  [1]

Test Rdyqz.asl:
  $ aslref Rdyqz.asl
  File Rdyqz.asl, line 12, characters 12 to 24:
  ASL Error: Fields mismatch for creating a value of type a
    -- Passed fields are: x
  [1]

Test Rwbcq.asl:
  $ aslref Rwbcq.asl

Test Rkcds.asl:
  $ aslref Rkcds.asl
  File Rkcds.asl, line 12, characters 12 to 24:
  ASL Error: Fields mismatch for creating a value of type a
    -- Passed fields are: x
  [1]

Test Rzwch.asl:
  $ aslref Rzwch.asl

Test Iqslr.asl:
  $ aslref Iqslr.asl

Test Ivgsp.asl:
  $ aslref Ivgsp.asl
  File Ivgsp.asl, line 6, character 0 to line 9, character 3:
  ASL Typing error: cannot declare already declared element "a".
  [1]

Test Iwdmd.asl:
  $ aslref Iwdmd.asl

Test Rlljz.asl:
  $ aslref Rlljz.asl

Test Rqnqv.asl:
  $ aslref Rqnqv.asl

Test Rsppt.asl:
  $ aslref Rsppt.asl

Test Rytky.asl:
  $ aslref Rytky.asl

Test Rzdsj.asl:
  $ aslref Rzdsj.asl

Test Ijejd.asl:
  $ aslref Ijejd.asl

Test Itfsz.asl:
  $ aslref Itfsz.asl

Test Rktbg.asl:
  $ aslref Rktbg.asl

Test Rnhgp.asl:
  $ aslref Rnhgp.asl
  ASL Typing error: constrained integer expected, provided integer
  [1]

Test Rsnqj.asl:
  $ aslref Rsnqj.asl

Test Rwzcs.asl:
  $ aslref Rwzcs.asl
  ASL Dynamic error: Cannot extract from bitvector of length 0 slices 4+:-2.
  [1]

Test Rdvvq.asl:
  $ aslref Rdvvq.asl
  30
  10
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rdvvq.asl
  // CHECK: 30
  // CHECK-NEXT: 10

Test Rgxqh.asl:
  $ aslref Rgxqh.asl
  File Rgxqh.asl, line 13, characters 12 to 15:
  ASL Error: Mismatched use of return value from call to 'a'
  [1]

Test Rztrr.asl:
  $ aslref Rztrr.asl
  File Rztrr.asl, line 8, characters 12 to 18:
  ASL Typing error: a subtype of integer {0..9} was expected,
    provided integer {100}.
  [1]

Test Dkxwt.asl:
  $ aslref Dkxwt.asl

Test Ihsql.asl:
  $ aslref Ihsql.asl

Test Rdgrv.asl:
  $ aslref Rdgrv.asl
  File Rdgrv.asl, line 14, characters 13 to 20:
  ASL Typing error: a subtype of exception {  } was expected, provided integer.
  [1]
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rdgrv.asl
  // CHECK: Hello
  // CHECK-NEXT: World

Test Rgvcc.asl:
  $ aslref Rgvcc.asl
  Uncaught exception: a {}
  [1]

Test Rtxtc.asl:
  $ aslref Rtxtc.asl
  Caught correctly
  Caught incorrectly
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rtxtc.asl
  // CHECK: Caught correctly
  // CHECK-NEXT: Caught incorrectly

Test Dggcq.asl:
  $ aslref Dggcq.asl
  10
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Dggcq.asl
  // CHECK: 10

Test Djtdg.asl:
  $ aslref Djtdg.asl
  10
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Djtdg.asl
  // CHECK: 10

Test Igzvm.asl:
  $ aslref Igzvm.asl
  10
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Igzvm.asl
  // CHECK: 10

Test Rdhkh.asl:
  $ aslref Rdhkh.asl
  File Rdhkh.asl, line 19, characters 23 to 25:
  ASL Typing error: cannot assign to immutable storage "aa".
  [1]

Test Rjzst.asl:
  $ aslref Rjzst.asl
  aa
  bb
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rjzst.asl
  // CHECK: aa
  // CHECK-NEXT: bb

Test Rmkgb.asl:
  $ aslref Rmkgb.asl
  File Rmkgb.asl, line 20, characters 10 to 12:
  ASL Error: Undefined identifier: 'aa'
  [1]

Test Rspnm.asl:
  $ aslref Rspnm.asl
  b
  a
  other
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rspnm.asl
  // CHECK: b
  // CHECK-NEXT: a
  // CHECK-NEXT: other

Test Ryvxf.asl:
  $ aslref Ryvxf.asl
  a
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Ryvxf.asl
  // CHECK: a

Test Rztlb.asl:
  $ aslref Rztlb.asl
  a
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rztlb.asl
  // CHECK: a

Test Iyklf.asl:
  $ aslref Iyklf.asl
  a
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Iyklf.asl
  // CHECK: a

Test Rbrcj.asl:
  $ aslref Rbrcj.asl
  Uncaught exception: implicitely thrown out of a try-catch.
  [1]

Test Rgvks.asl:
  $ aslref Rgvks.asl
  a
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rgvks.asl
  // CHECK: a

Test Rptng.asl:
  $ aslref Rptng.asl

Test Ikbxm.asl:
  $ aslref Ikbxm.asl
  File Ikbxm.asl, line 5, characters 12 to 13:
  ASL Error: Undefined identifier: 'c'
  [1]

Test Rjfrd.asl:
  $ aslref Rjfrd.asl
  File Rjfrd.asl, line 13, characters 12 to 13:
  ASL Error: Undefined identifier: 'a'
  [1]

Test Rlcfd.asl:
  $ aslref Rlcfd.asl
  File Rlcfd.asl, line 10, characters 4 to 14:
  ASL Typing error: cannot declare already declared element "a".
  [1]

Test Ifkjc.asl:
  $ aslref Ifkjc.asl

Test Rjnmr.asl:
  $ aslref Rjnmr.asl
  File Rjnmr.asl, line 12, characters 17 to 20:
  ASL Error: Unsupported expression a().
  [1]

Test Rkszm.asl:
  $ aslref Rkszm.asl
  0
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rkszm.asl
  // CHECK: 0

Test Rtzrv.asl:
  $ aslref Rtzrv.asl

Test Rxylp.asl:
  $ aslref Rxylp.asl
  File Rxylp.asl, line 11, characters 16 to 17:
  ASL Error: Cannot parse.
  [1]

Test Rzxhp.asl:
  $ aslref Rzxhp.asl
  1
  2
  3
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rzxhp.asl
  // CHECK: 1
  // CHECK-NEXT: 2
  // CHECK-NEXT: 3

Test Rbfwl.asl:
  $ aslref Rbfwl.asl

Test Rclqj.asl:
  $ aslref Rclqj.asl

Test Rfmlk.asl:
  $ aslref Rfmlk.asl

Test Rnxsf.asl:
  $ aslref Rnxsf.asl

Test Rqdqd.asl:
  $ aslref Rqdqd.asl
  File Rqdqd.asl, line 9, characters 13 to 14:
  ASL Error: Cannot parse.
  [1]

Test Rsqjj.asl:
  $ aslref Rsqjj.asl

Test Rtfjz.asl:
  $ aslref Rtfjz.asl

Test Rxhpb.asl:
  $ aslref Rxhpb.asl

Test Rxsdc.asl:
  $ aslref Rxsdc.asl

Test Dkcyt.asl:
  $ aslref Dkcyt.asl
  File Dkcyt.asl, line 14, characters 4 to 8:
  ASL Error: Mismatched use of return value from call to 'a'
  [1]

Test Dhtpl.asl:
  $ aslref Dhtpl.asl

Test Rnywh.asl:
  $ aslref Rnywh.asl
  File Rnywh.asl, line 8, characters 4 to 14:
  ASL Typing error: cannot return something from a procedure.
  [1]

Test Rphnz.asl:
  $ aslref Rphnz.asl

Test Ryyfr.asl:
  $ aslref Ryyfr.asl
  File Ryyfr.asl, line 13, characters 4 to 14:
  ASL Typing error: overlapping slices 0+:1, 0+:1.
  [1]

Test Rzhyt.asl:
  $ aslref Rzhyt.asl
  File Rzhyt.asl, line 15, characters 4 to 14:
  ASL Typing error: a subtype of bits(-) was expected, provided a.
  [1]

Test Dwsxy.asl:
  $ aslref Dwsxy.asl
  1
  2
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Dwsxy.asl
  // CHECK: 1
  // CHECK-NEXT: 2

Test Icjvd.asl:
  $ aslref Icjvd.asl

Test Issxj.asl:
  $ aslref Issxj.asl

Test Rzhvh.asl:
  $ aslref Rzhvh.asl
  File Rzhvh.asl, line 9, characters 4 to 27:
  ASL Error: Arity error while calling 'tuple initialization':
    3 arguments expected and 2 provided
  [1]

Test Rydfq.asl:
  $ aslref Rydfq.asl
  4
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rydfq.asl
  // CHECK: 4

Test Dbjny.asl:
  $ aslref Dbjny.asl

Test Dqjyv.asl:
  $ aslref Dqjyv.asl

Test Rwqrn.asl:
  $ aslref Rwqrn.asl

Test Rwzsl.asl:
  $ aslref Rwzsl.asl
  File Rwzsl.asl, line 10, characters 11 to 16:
  ASL Execution error: Assertion failed: FALSE
  [1]

Test Rkztj.asl:
  $ aslref Rkztj.asl
  0
  3
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rkztj.asl
  // CHECK: 0
  // CHECK-NEXT: 3

Test Rtmys.asl:
  $ aslref Rtmys.asl
  3
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rtmys.asl
  // CHECK: 3

Test Rxssl.asl:
  $ aslref Rxssl.asl
  2
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rxssl.asl
  // CHECK: 2

Test Rcwnt.asl:
  $ aslref Rcwnt.asl
  3
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rcwnt.asl
  // CHECK: 3

Test Rjhst.asl:
  $ aslref Rjhst.asl
  3
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rjhst.asl
  // CHECK: 3

Test Rjvtr.asl:
  $ aslref Rjvtr.asl

Test Rrxqb.asl:
  $ aslref Rrxqb.asl
  Otherwise

Test Rzyvw.asl:
  $ aslref Rzyvw.asl
  Otherwise
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rzyvw.asl
  // CHECK: Otherwise

Test Ikfyg.asl:
  $ aslref Ikfyg.asl
  File Ikfyg.asl, line 6, characters 13 to 14:
  ASL Error: Cannot parse.
  [1]

Test Iydbr.asl:
  $ aslref Iydbr.asl
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Iydbr.asl
  // CHECK-NOT: 5

Test Rjqxc.asl:
  $ aslref Rjqxc.asl
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rjqxc.asl
  // CHECK-NOT: Run

Test Rkldr.asl:
  $ aslref Rkldr.asl

Test Rlsvv.asl:
  $ aslref Rlsvv.asl
  1
  2
  3
  4
  5
  6
  7
  8
  9
  10
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rlsvv.asl
  // CHECK: 1
  // CHECK-NEXT: 2
  // CHECK-NEXT: 3
  // CHECK-NEXT: 4
  // CHECK-NEXT: 5
  // CHECK-NEXT: 6
  // CHECK-NEXT: 7
  // CHECK-NEXT: 8
  // CHECK-NEXT: 9
  // CHECK-NEXT: 10

Test Rmhpw.asl:
  $ aslref Rmhpw.asl
  1
  2
  4
  8
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rmhpw.asl
  // CHECK: 1
  // CHECK_NEXT: 2
  // CHECK_NEXT: 4
  // CHECK_NEXT: 8

Test Rnpwr.asl:
  $ aslref Rnpwr.asl
  0
  1
  2
  3
  4
  5
  6
  7
  8
  9
  10
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rnpwr.asl
  // CHECK: 0
  // CHECK-NEXT: 1
  // CHECK-NEXT: 2
  // CHECK-NEXT: 3
  // CHECK-NEXT: 4
  // CHECK-NEXT: 5
  // CHECK-NEXT: 6
  // CHECK-NEXT: 7
  // CHECK-NEXT: 8
  // CHECK-NEXT: 9
  // CHECK-NEXT: 10

Test Rnzgh.asl:
  $ aslref Rnzgh.asl

Test Rrqng.asl:
  $ aslref Rrqng.asl
  File Rrqng.asl, line 8, characters 8 to 9:
  ASL Typing error: cannot assign to immutable storage "x".
  [1]

Test Rssbd.asl:
  $ aslref Rssbd.asl
  10
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rssbd.asl
  // CHECK: 10

Test Rwvqt.asl:
  $ aslref Rwvqt.asl
  0
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rwvqt.asl
  // CHECK-NOT: 1

Test Rwvqt-2.asl:
  $ aslref Rwvqt-2.asl
  File Rwvqt-2.asl, line 3, character 0:
  ASL Error: Unknown symbol.
  [1]

Test Rytnr.asl:
  $ aslref Rytnr.asl

Test Rzsnd.asl:
  $ aslref Rzsnd.asl
  File Rzsnd.asl, line 26, characters 22 to 23:
  ASL Error: Cannot parse.
  [1]

Test Icgrh.asl:
  $ aslref Icgrh.asl

Test Itcst.asl:
  $ aslref Itcst.asl

Test Iyjbb.asl:
  $ aslref Iyjbb.asl

Test Ivqlx.asl:
  $ aslref Ivqlx.asl

Test Rpzzj.asl:
  $ aslref Rpzzj.asl

Test Rycpx.asl:
  $ aslref Rycpx.asl

Test Rycpx-2.asl:
  $ aslref Rycpx-2.asl
  File Rycpx-2.asl, line 9, characters 6 to 10:
  ASL Error: Undefined identifier: 'f1'
  [1]

Test Ibhln.asl:
  $ aslref Ibhln.asl
  File Ibhln.asl, line 19, characters 17 to 18:
  ASL Error: Cannot parse.
  [1]

Test Igjzq.asl:
  $ aslref Igjzq.asl

Test Igqrd.asl:
  $ aslref Igqrd.asl

Test Imkpr.asl:
  $ aslref Imkpr.asl
  File Imkpr.asl, line 31, characters 38 to 39:
  ASL Error: Cannot parse.
  [1]

Test Iszvf.asl:
  $ aslref Iszvf.asl
  File Iszvf.asl, line 9, characters 22 to 23:
  ASL Error: Cannot parse.
  [1]

Test Ixvbg.asl:
  $ aslref Ixvbg.asl

Test Rgyjz.asl:
  $ aslref Rgyjz.asl
  File Rgyjz.asl, line 9, characters 22 to 23:
  ASL Error: Cannot parse.
  [1]

Test Igqyg.asl:
  $ aslref Igqyg.asl
  File Igqyg.asl, line 14, characters 27 to 28:
  ASL Error: Cannot parse.
  [1]

Test Rxnbn.asl:
  $ aslref Rxnbn.asl

Test Xxswl.asl:
  $ aslref Xxswl.asl

Test Isbck.asl:
  $ aslref Isbck.asl

Test Gmtxx.asl:
  $ aslref Gmtxx.asl
  File Gmtxx.asl, line 6, characters 4 to 27:
  ASL Typing error: a subtype of integer {11} was expected,
    provided integer {0..10}.
  [1]

Test Ikjdr.asl:
  $ aslref Ikjdr.asl

Test Dgwxk.asl:
  $ aslref Dgwxk.asl

Test Dvmzx.asl:
  $ aslref Dvmzx.asl

Test Dfxqv.asl:
  $ aslref Dfxqv.asl

Test Dbmgm.asl:
  $ aslref Dbmgm.asl
  File Dbmgm.asl, line 11, characters 17 to 18:
  ASL Error: Cannot parse.
  [1]

Test Ikrll.asl:
  $ aslref Ikrll.asl

Test Rvbll.asl:
  $ aslref Rvbll.asl

Test Rwzvx.asl:
  $ aslref Rwzvx.asl
  File Rwzvx.asl, line 10, characters 12 to 13:
  ASL Execution error: Mismatch type:
    value 4 does not belong to type integer {0..3}.
  [1]

Test Igysk.asl:
  $ aslref Igysk.asl
  File Igysk.asl, line 13, characters 18 to 19:
  ASL Error: Cannot parse.
  [1]

Test Ihswr.asl:
  $ aslref Ihswr.asl
  File Ihswr.asl, line 14, characters 4 to 15:
  ASL Typing error: a subtype of bits(4) was expected, provided bits(2).
  [1]

Test Iknxj.asl:
  $ aslref Iknxj.asl
  File Iknxj.asl, line 15, characters 21 to 22:
  ASL Error: Cannot parse.
  [1]

Test Ikxsd.asl:
  $ aslref Ikxsd.asl
  File Ikxsd.asl, line 3, characters 21 to 22:
  ASL Error: Cannot parse.
  [1]

Test Itwtz.asl:
  $ aslref Itwtz.asl
  File Itwtz.asl, line 6, characters 4 to 23:
  ASL Typing error: a subtype of bits(4) was expected, provided bits(2).
  [1]

Test Imhyb.asl:
  $ aslref Imhyb.asl

Test Isjdc.asl:
  $ aslref Isjdc.asl

Test Inlfd.asl:
  $ aslref Inlfd.asl

Test Rfmxk.asl:
  $ aslref Rfmxk.asl

Test Dvpzz.asl:
  $ aslref Dvpzz.asl

Test Ipqct.asl:
  $ aslref Ipqct.asl

Test Iwzkm.asl:
  $ aslref Iwzkm.asl

Test Dbtbr.asl:
  $ aslref Dbtbr.asl
  File Dbtbr.asl, line 13, character 0 to line 16, character 3:
  ASL Typing error: cannot declare already declared element "testa".
  [1]

Test Ifsfq.asl:
  $ aslref Ifsfq.asl
  File Ifsfq.asl, line 9, character 0 to line 12, character 3:
  ASL Typing error: cannot declare already declared element "testa".
  [1]

Test Ipfgq.asl:
  $ aslref Ipfgq.asl
  File Ipfgq.asl, line 3, character 0 to line 6, character 3:
  ASL Typing error: cannot declare already declared element "a".
  [1]

Test Isctb.asl:
  $ aslref Isctb.asl
  File Isctb.asl, line 7, character 0 to line 10, character 3:
  ASL Typing error: cannot declare already declared element "a".
  [1]

Test Rpgfc.asl:
  $ aslref Rpgfc.asl
  File Rpgfc.asl, line 6, character 0 to line 9, character 3:
  ASL Typing error: cannot declare already declared element "a".
  [1]

Test Dhbcp.asl:
  $ aslref Dhbcp.asl

Output is non-deterministic
  $ aslref Idfml.asl
  File Idfml.asl, line 7, characters 0 to 18:
  ASL Error: Undefined identifier: 'a'
  [1]

Test Ismmh.asl:
  $ aslref Ismmh.asl

Test Rfwqm.asl:
  $ aslref Rfwqm.asl
  File Rfwqm.asl, line 6, characters 18 to 19:
  ASL Error: Undefined identifier: 'b'
  [1]

Test Rhwtv.asl:
  $ aslref Rhwtv.asl

Test Rjbxq.asl:
  $ aslref Rjbxq.asl
  File Rjbxq.asl, line 20, characters 16 to 17:
  ASL Error: Undefined identifier: 'b'
  [1]

Test Rschv.asl:
  $ aslref Rschv.asl
  File Rschv.asl, line 10, characters 4 to 16:
  ASL Error: Undefined identifier: 'ty'
  [1]

Test Rvdpc.asl:
  $ aslref Rvdpc.asl

Test Ryspm.asl:
  $ aslref Ryspm.asl

Test Rftpk.asl:
  $ aslref Rftpk.asl

Test Rftvn.asl:
  $ aslref Rftvn.asl
  File Rftvn.asl, line 10, characters 10 to 12:
  ASL Typing error: a subtype of boolean was expected, provided integer {10}.
  [1]

Test Rjqyf.asl:
  $ aslref Rjqyf.asl
  File Rjqyf.asl, line 8, characters 4 to 15:
  ASL Typing error: a subtype of boolean was expected, provided integer {10}.
  [1]

Test Rnbdj.asl:
  $ aslref Rnbdj.asl
  File Rnbdj.asl, line 8, characters 7 to 9:
  ASL Typing error: a subtype of boolean was expected, provided integer {10}.
  [1]

Test Rnxrc.asl:
  $ aslref Rnxrc.asl
  File Rnxrc.asl, line 8, characters 4 to 13:
  ASL Typing error: a subtype of exception {  } was expected,
    provided integer {10}.
  [1]

Test Rsdjk.asl:
  $ aslref Rsdjk.asl
  File Rsdjk.asl, line 11, characters 13 to 15:
  ASL Error: Cannot parse.
  [1]

Test Rvtjw.asl:
  $ aslref Rvtjw.asl
  File Rvtjw.asl, line 5, character 4 to line 7, character 7:
  ASL Typing error: a subtype of integer was expected, provided real.
  [1]

Test Rwgsy.asl:
  $ aslref Rwgsy.asl
  File Rwgsy.asl, line 9, characters 23 to 24:
  ASL Typing error: a subtype of boolean was expected, provided integer {1}.
  [1]

Test Rwvxs.asl:
  $ aslref Rwvxs.asl

Test Idgwj.asl:
  $ aslref Idgwj.asl
  File Idgwj.asl, line 24, characters 4 to 19:
  ASL Typing error: a subtype of b was expected, provided a.
  [1]

Test Ikkcc.asl:
  $ aslref Ikkcc.asl

Test Immkf.asl:
  $ aslref Immkf.asl
  File Immkf.asl, line 7, characters 4 to 5:
  ASL Typing error: a subtype of integer {0..N} was expected,
    provided integer {0..M}.
  [1]

Test Iyyqx.asl:
  $ aslref Iyyqx.asl

Test Rgnts.asl:
  $ aslref Rgnts.asl
  File Rgnts.asl, line 7, characters 18 to 19:
  ASL Error: Cannot parse.
  [1]

Test Rlxqz.asl:
  $ aslref Rlxqz.asl
  File Rlxqz.asl, line 9, characters 4 to 25:
  ASL Typing error: a subtype of integer was expected, provided real.
  [1]

Test Rwmfv.asl:
  $ aslref Rwmfv.asl

Test Rzcvd.asl:
  $ aslref Rzcvd.asl
  File Rzcvd.asl, line 11, characters 18 to 19:
  ASL Error: Cannot parse.
  [1]

Test Iryrp.asl:
  $ aslref Iryrp.asl

Test Rzjky.asl:
  $ aslref Rzjky.asl

Test Djrxm.asl:
  $ aslref Djrxm.asl

Test Iztmq.asl:
  $ aslref Iztmq.asl

Test Iblvp.asl:
  $ aslref Iblvp.asl
  File Iblvp.asl, line 13, character 0 to line 16, character 3:
  ASL Typing error: explicit parameter "N" does not have a corresponding
    defining argument
  [1]

Test Ibzvb.asl:
  $ aslref Ibzvb.asl

Test Ilfjz.asl:
  $ aslref Ilfjz.asl

Test Ipdkt.asl:
  $ aslref Ipdkt.asl
  File Ipdkt.asl, line 7, characters 21 to 22:
  ASL Error: Cannot parse.
  [1]

Test Irkbv.asl:
  $ aslref Irkbv.asl
  File Irkbv.asl, line 6, characters 21 to 22:
  ASL Error: Cannot parse.
  [1]

Test Irqqb.asl:
  $ aslref Irqqb.asl

Test Itqgh.asl:
  $ aslref Itqgh.asl

Test Izlzc.asl:
  $ aslref Izlzc.asl
  File Izlzc.asl, line 3, character 0 to line 9, character 3:
  ASL Typing error: explicit parameter "N" does not have a corresponding
    defining argument
  [1]

Test Rlvth.asl:
  $ aslref Rlvth.asl
  File Rlvth.asl, line 6, characters 18 to 19:
  ASL Error: Cannot parse.
  [1]

Test Rrhtn.asl:
  $ aslref Rrhtn.asl
  File Rrhtn.asl, line 8, character 0 to line 11, character 3:
  ASL Typing error: constrained integer expected, provided integer
  [1]

Test Rtjkq.asl:
  $ aslref Rtjkq.asl
  File Rtjkq.asl, line 9, characters 4 to 39:
  ASL Typing error: a subtype of integer {32, 64} was expected,
    provided integer.
  [1]

Test Dcfyp.asl:
  $ aslref Dcfyp.asl

Test Dljlw.asl:
  $ aslref Dljlw.asl

Test Dmfbc.asl:
  $ aslref Dmfbc.asl

Test Dpmbl.asl:
  $ aslref Dpmbl.asl

Test Dtrfw.asl:
  $ aslref Dtrfw.asl

Test Dvxkm.asl:
  $ aslref Dvxkm.asl

Test Ibtmt.asl:
  $ aslref Ibtmt.asl
  File Ibtmt.asl, line 7, character 0 to line 10, character 3:
  ASL Typing error: cannot declare already declared element "test".
  [1]

Test Icmlp.asl:
  $ aslref Icmlp.asl
  File Icmlp.asl, line 15, characters 4 to 20:
  ASL Typing error: constrained integer expected, provided integer
  [1]

Test Iflkf.asl:
  $ aslref Iflkf.asl
  File Iflkf.asl, line 15, characters 4 to 20:
  ASL Typing error: a subtype of bits(2) was expected, provided bits(1).
  [1]

Test Iktjn.asl:
  $ aslref Iktjn.asl
  File Iktjn.asl, line 21, characters 4 to 6:
  ASL Typing error: a subtype of bits(N) was expected, provided bits(wid).
  [1]

Test Isbwr.asl:
  $ aslref Isbwr.asl
  File Isbwr.asl, line 10, characters 18 to 19:
  ASL Error: Cannot parse.
  [1]

Test Ivfdp.asl:
  $ aslref Ivfdp.asl
  File Ivfdp.asl, line 5, characters 4 to 13:
  ASL Typing error: a subtype of integer {N} was expected,
    provided integer {5..10}.
  [1]

Test Iymhx.asl:
  $ aslref Iymhx.asl
  File Iymhx.asl, line 5, characters 4 to 18:
  ASL Typing error: a subtype of bits(M) was expected, provided bits((M + 1)).
  [1]

Test Rbqjg.asl:
  $ aslref Rbqjg.asl

Test Rccvd.asl:
  $ aslref Rccvd.asl
  ASL Typing error: a pure expression was expected, found global
  [1]

Test Rkmbd.asl:
  $ aslref Rkmbd.asl

Test Rmwbn.asl:
  $ aslref Rmwbn.asl
  File Rmwbn.asl, line 7, characters 18 to 19:
  ASL Error: Cannot parse.
  [1]

Test Rpfwq.asl:
  $ aslref Rpfwq.asl
  File Rpfwq.asl, line 13, characters 4 to 21:
  ASL Typing error: a subtype of bits(10) was expected, provided bits(4).
  [1]

Test Rqybh.asl:
  $ aslref Rqybh.asl

Test Rrtcf.asl:
  $ aslref Rrtcf.asl
  File Rrtcf.asl, line 13, characters 4 to 13:
  ASL Typing error: No subprogram declaration matches the invocation:
    test(integer {10})
  [1]

Test Rtcdl.asl:
  $ aslref Rtcdl.asl

Test Rtzsp.asl:
  $ aslref Rtzsp.asl
  File Rtzsp.asl, line 21, characters 4 to 12:
  ASL Error: Undefined identifier: 'test2'
  [1]

Test Rzlwd.asl:
  $ aslref Rzlwd.asl
  File Rzlwd.asl, line 14, characters 4 to 15:
  ASL Typing error: a subtype of integer {5..10} was expected,
    provided integer {2}.
  [1]

Test Rbknt.asl:
  $ aslref Rbknt.asl

Test Rjgwf.asl:
  $ aslref Rjgwf.asl

Test Rttgq.asl:
  $ aslref Rttgq.asl
  File Rttgq.asl, line 13, characters 12 to 21:
  ASL Typing error: Illegal application of operator || on types a and integer
  [1]

Test Ivmzf.asl:
  $ aslref Ivmzf.asl

Test Iyhml.asl:
  $ aslref Iyhml.asl

Test Iyhrp.asl:
  $ aslref Iyhrp.asl
  File Iyhrp.asl, line 8, characters 12 to 19:
  ASL Typing error: Illegal application of operator DIV on types integer {2, 4}
    and integer {(- 1)..1}
  [1]

Test Iyxsy.asl:
  $ aslref Iyxsy.asl

Test Rbzkw.asl:
  $ aslref Rbzkw.asl
  File Rbzkw.asl, line 7, characters 21 to 22:
  ASL Error: Cannot parse.
  [1]

Test Rkfys.asl:
  $ aslref Rkfys.asl

Test Rzywy.asl:
  $ aslref Rzywy.asl

Test Ilghj.asl:
  $ aslref Ilghj.asl

Test Rkxmr.asl:
  $ aslref Rkxmr.asl
  File Rkxmr.asl, line 10, characters 15 to 21:
  ASL Typing error: Illegal application of operator == on types bits(M)
    and bits(8)
  [1]

Test Rxzvt.asl:
  $ aslref Rxzvt.asl

Test Rxzvt-2.asl:
  $ aslref Rxzvt-2.asl
  ASL Error: Undefined identifier: 'main'
  [1]

Test Rxzvt-3.asl:
  $ aslref Rxzvt-3.asl
  ASL Error: Undefined identifier: 'main'
  [1]

Test Rxzvt-4.asl:
  $ aslref Rxzvt-4.asl
  ASL Error: Undefined identifier: 'main'
  [1]

Test Rmrht.asl:
  $ aslref Rmrht.asl
  File Rmrht.asl, line 10, characters 12 to 18:
  ASL Typing error: Illegal application of operator == on types bits(1)
    and bits(11)
  [1]

Test Rmrht-2.asl:
  $ aslref Rmrht-2.asl
  File Rmrht-2.asl, line 17, characters 9 to 21:
  ASL Typing error: Illegal application of operator == on types bits(int1)
    and bits(int2)
  [1]

Test Rsqxn.asl:
  $ aslref Rsqxn.asl

Test Rkczs.asl:
  $ aslref Rkczs.asl

Test Rnynk.asl:
  $ aslref Rnynk.asl

Test Rfhyz.asl:
  $ aslref Rfhyz.asl
  File Rfhyz.asl, line 7, characters 21 to 22:
  ASL Error: Cannot parse.
  [1]

Test Rvbmx.asl:
  $ aslref Rvbmx.asl

Test Rxvwk.asl:
  $ aslref Rxvwk.asl

Test Imjwm.asl:
  $ aslref Imjwm.asl

Test Ildnp.asl:
  $ aslref Ildnp.asl

Test Dcwvh.asl:
  $ aslref Dcwvh.asl

Test Dhlqc.asl:
  $ aslref Dhlqc.asl

Test Dyydw.asl:
  $ aslref Dyydw.asl

Test Ilzcx.asl:
  $ aslref Ilzcx.asl
  File Ilzcx.asl, line 4, characters 32 to 37:
  ASL Error: Undefined identifier: 'add'
  [1]

Test Ipkxk.asl:
  $ aslref Ipkxk.asl

Test Ilhlr.asl:
  $ aslref Ilhlr.asl

Test Rrfqp.asl:
  $ aslref Rrfqp.asl

Test Rvnkt.asl:
  $ aslref Rvnkt.asl
  ASL Execution error: Illegal application of operator DIV for values 10 and 4.
  [1]

Test Dfxst.asl:
  $ aslref Dfxst.asl

Test Rwdgq.asl:
  $ aslref Rwdgq.asl
  File Rwdgq.asl, line 9, characters 4 to 5:
  ASL Typing error: cannot assign to immutable storage "x".
  [1]

Test Rzdkc.asl:
  $ aslref Rzdkc.asl
  File Rzdkc.asl, line 12, characters 15 to 16:
  ASL Error: Cannot parse.
  [1]

Test Dccty.asl:
  $ aslref Dccty.asl

Test Dcsft.asl:
  $ aslref Dcsft.asl

Test Ihybt.asl:
  $ aslref Ihybt.asl

Test Ilykd.asl:
  $ aslref Ilykd.asl

Test Inxjr.asl:
  $ aslref Inxjr.asl

Test Dkckx.asl:
  $ aslref Dkckx.asl

Test Dqnhm.asl:
  $ aslref Dqnhm.asl

Test Imszt.asl:
  $ aslref Imszt.asl

Test Intyz.asl:
  $ aslref Intyz.asl

Test Izpwm.asl:
  $ aslref Izpwm.asl

Test Dzpmf.asl:
  $ aslref Dzpmf.asl

Test Ixykc.asl:
  $ aslref Ixykc.asl

Test Dxrbt.asl:
  $ aslref Dxrbt.asl

Test Ixsfy.asl:
  $ aslref Ixsfy.asl

Test Djljd.asl:
  $ aslref Djljd.asl
  File Djljd.asl, line 8, characters 27 to 28:
  ASL Error: Cannot parse.
  [1]

Test Iwvgg.asl:
  $ aslref Iwvgg.asl

Test Dmtqj.asl:
  $ aslref Dmtqj.asl

Test Ikkqy.asl:
  $ aslref Ikkqy.asl

Test Iybgl.asl:
  $ aslref Iybgl.asl

Test Igfzt.asl:
  $ aslref Igfzt.asl
  TRUE
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Igfzt.asl
  // CHECK: TRUE

Test Iqjtn_a.asl:
  $ aslref Iqjtn_a.asl

Test Iqjtn_b.asl:
  $ aslref Iqjtn_b.asl

Test Iqjtn_c.asl:
  $ aslref Iqjtn_c.asl
  Uncaught exception: exc {err_code: 0}
  [1]
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Iqjtn_c.asl
  // CHECK-NOT: Exception

Test Iqjtn_d.asl:
  $ aslref Iqjtn_d.asl
  Uncaught exception: exc {err_code: 0}
  [1]
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Iqjtn_d.asl
  // CHECK-NOT: Exception

Test Iqrxp.asl:
  $ aslref Iqrxp.asl
  HELLO
  WORLD
  HELLO
  HELLO
  WORLD
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Iqrxp.asl
  // CHECK: HELLO
  // CHECK-NEXT: WORLD
  // CHECK-NEXT: HELLO
  // CHECK-NEXT: HELLO
  // CHECK-NEXT: WORLD

Test Rxkgc.asl:
  $ aslref Rxkgc.asl

Test Rgqnl.asl:
  $ aslref Rgqnl.asl
  TRUE
  TRUE
  FALSE
  TRUE
  Hello
  World
  FALSE
  World
  TRUE
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rgqnl.asl
  // CHECK: TRUE
  // CHECK-NEXT: TRUE
  // CHECK-NEXT: FALSE
  // CHECK-NEXT: TRUE
  // CHECK-NEXT: Hello
  // CHECK-NEXT: World
  // CHECK-NEXT: FALSE
  // CHECK-NEXT: World
  // CHECK-NEXT: TRUE

Test Rlrhd.asl:
  $ aslref Rlrhd.asl
  TRUE
  FALSE
  FALSE
  TRUE
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rlrhd.asl
  // CHECK: TRUE
  // CHECK-NEXT: FALSE
  // CHECK-NEXT: FALSE
  // CHECK-NEXT: TRUE

Test Rbncy.asl:
  $ aslref Rbncy.asl
  File Rbncy.asl, line 14, characters 10 to 25:
  ASL Error: Undefined identifier: 'exp_real'
  [1]
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rbncy.asl
  // CHECK: 1000
  // CHECK-NEXT: 32

Test Inbct.asl:
  $ aslref Inbct.asl

Test Rcrqj.asl:
  $ aslref Rcrqj.asl
  File Rcrqj.asl, line 9, characters 21 to 35:
  ASL Error: Undefined identifier: 'div_int'
  [1]

Test Rghxr_a.asl:
  $ aslref Rghxr_a.asl
  File Rghxr_a.asl, line 11, characters 10 to 24:
  ASL Error: Undefined identifier: 'frem_int'
  [1]
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rghxr_a.asl
  // CHECK: 0
  // CHECK-NEXT: 1

Test Rghxr_b.asl:
  $ aslref Rghxr_b.asl
  File Rghxr_b.asl, line 9, characters 10 to 25:
  ASL Error: Undefined identifier: 'frem_int'
  [1]

Test Rncwm.asl:
  $ aslref Rncwm.asl
  File Rncwm.asl, line 14, characters 10 to 24:
  ASL Error: Undefined identifier: 'exp_int'
  [1]
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rncwm.asl
  // CHECK: 1000
  // CHECK-NEXT: 32

Test Rsvmm.asl:
  $ aslref Rsvmm.asl
  File Rsvmm.asl, line 7, characters 10 to 24:
  ASL Error: Undefined identifier: 'fdiv_int'
  [1]
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rsvmm.asl
  // CHECK: 2
  // CHECK-NEXT: -2

Test Rthsv.asl:
  $ aslref Rthsv.asl
  File Rthsv.asl, line 8, characters 10 to 32:
  ASL Error: Undefined identifier: 'shiftleft_int'
  [1]

Test Rvgzf.asl:
  $ aslref Rvgzf.asl
  File Rvgzf.asl, line 14, characters 10 to 30:
  ASL Error: Undefined identifier: 'shiftleft_int'
  [1]
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rvgzf.asl
  // CHECK: 80
  // CHECK-NEXT: 96
  // CHECK-NEXT: 3
  // CHECK-NEXT: 6

Test Rwwtv_a.asl:
  $ aslref Rwwtv_a.asl
  File Rwwtv_a.asl, line 7, characters 10 to 24:
  ASL Error: Undefined identifier: 'div_int'
  [1]

Test Rwwtv_b.asl:
  $ aslref Rwwtv_b.asl
  File Rwwtv_b.asl, line 7, characters 10 to 24:
  ASL Error: Undefined identifier: 'fdiv_int'
  [1]

Test Rztjn_a.asl:
  $ aslref Rztjn_a.asl
  File Rztjn_a.asl, line 11, characters 10 to 23:
  ASL Error: Undefined identifier: 'div_int'
  [1]
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rztjn_a.asl
  // CHECK: 2
  // CHECK-NEXT: -2

Test Rztjn_b.asl:
  $ aslref Rztjn_b.asl
  File Rztjn_b.asl, line 9, characters 10 to 23:
  ASL Error: Undefined identifier: 'div_int'
  [1]

Test Rbrcm.asl:
  $ aslref Rbrcm.asl
  TRUE
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rbrcm.asl
  // CHECK: TRUE

Test Rrxyn.asl:
  $ aslref Rrxyn.asl
  240
  -16
For reference, the test writter intention was that this output matched:
  $ grep '^// CHECK' Rrxyn.asl
  // CHECK: 240
  // CHECK-NEXT: -16

Test Rdgbm.asl:
  $ aslref Rdgbm.asl

Test Dbmgm-2.asl:
  $ aslref Dbmgm-2.asl

Test Iglwm-2.asl:
  $ aslref Iglwm-2.asl

Test Ilghj-2.asl:
  $ aslref Ilghj-2.asl

Test Irxlg-2.asl:
  $ aslref Irxlg-2.asl
  FALSE
  TRUE
  '101'
  '100'
  '010'
  '001'
  '001'
  '001'

Test Iypxd-2.asl:
  $ aslref Iypxd-2.asl
  File Iypxd-2.asl, line 12, characters 4 to 5:
  ASL Error: Cannot parse.
  [1]

Test Rfhyz-2.asl:
  $ aslref Rfhyz-2.asl

Test Rmwbn-2.asl:
  $ aslref Rmwbn-2.asl

Test Rqxgw-2.asl:
  $ aslref Rqxgw-2.asl
  TRUE

Test Rwzvx-2.asl:
  $ aslref Rwzvx-2.asl
  File Rwzvx-2.asl, line 10, characters 12 to 13:
  ASL Execution error: Mismatch type:
    value 4 does not belong to type integer {0..3}.
  [1]


Test Dbvgk-2.asl:
  $ aslref Dbvgk-2.asl

Test Igqyg-2.asl:
  $ aslref Igqyg-2.asl
  File Igqyg-2.asl, line 41, characters 4 to 31:
  ASL Typing error: constrained integer expected, provided integer
  [1]

Test Igqyg-3.asl:
  $ aslref Igqyg-3.asl
  File Igqyg-3.asl, line 28, characters 11 to 19:
  ASL Typing error: constrained integer expected, provided integer
  [1]

Test Imkpr-2.asl:
  $ aslref Imkpr-2.asl
  File Imkpr-2.asl, line 36, characters 12 to 30:
  ASL Typing error: a subtype of integer {8, 16, 32} was expected,
    provided integer {M}.
  [1]

Test Isbwr-2.asl:
  $ aslref Isbwr-2.asl

Test Izddj-2.asl:
  $ aslref Izddj-2.asl

Test Rfrwd-2.asl:
  $ aslref Rfrwd-2.asl

Test Rncnq-2.asl:
  $ aslref Rncnq-2.asl

Test Rsnqj-2.asl:
  $ aslref Rsnqj-2.asl

Test Rsnqj-3.asl:
  $ aslref Rsnqj-3.asl
  File Rsnqj-3.asl, line 8, characters 24 to 27:
  ASL Error: Undefined identifier: 'f'
  [1]

Test Rxylp-2.asl:
  $ aslref Rxylp-2.asl


Test Djljd-2.asl:
  $ aslref Djljd-2.asl

Test Ihjcd-2.asl:
  $ aslref Ihjcd-2.asl
  1000000
  
  
  1000000

Test Ipdkt-2.asl:
  $ aslref Ipdkt-2.asl

Test Iszvf-2.asl:
  $ aslref Iszvf-2.asl

Test Rbncy-2.asl:
  $ aslref Rbncy-2.asl

Test Rghxr_a-2.asl:
  $ aslref Rghxr_a-2.asl
  File Rghxr_a-2.asl, line 10, characters 11 to 26:
  ASL Execution error: Assertion failed: ((- (5 MOD 3)) == 1)
  [1]

Test Rncwm-2.asl:
  $ aslref Rncwm-2.asl

Test Rsvmm-2.asl:
  $ aslref Rsvmm-2.asl

Test Rydfq-2.asl:
  $ aslref Rydfq-2.asl
  4

Test Rydfq-3.asl:
  $ aslref Rydfq-3.asl
  File Rydfq-3.asl, line 8, characters 0 to 3:
  ASL Error: Cannot parse.
  [1]
 
Test Rydfq-4.asl:
  $ aslref Rydfq-4.asl
  File Rydfq-4.asl, line 9, characters 4 to 9:
  ASL Error: Cannot parse.
  [1]
 
Test Dnmfp-2.asl:
  $ aslref Dnmfp-2.asl

Test Ihmrk-2.asl:
  $ aslref Ihmrk-2.asl
  0
  FALSE

Test Iqjtn_a-2.asl:
  $ aslref Iqjtn_a-2.asl

Test Itsxl-2.asl:
  $ aslref Itsxl-2.asl
  10
  20

Test Rbzkw-2.asl:
  $ aslref Rbzkw-2.asl

Test Rgvks-2.asl:
  $ aslref Rgvks-2.asl
  a

Test Rnzgh-2.asl:
  $ aslref Rnzgh-2.asl

Test Rtphr-2.asl:
  $ aslref Rtphr-2.asl

Test Rytnr-2.asl:
  $ aslref Rytnr-2.asl


Test Ibhln-2.asl:
  $ aslref Ibhln-2.asl

Test Ihvlx-2.asl:
  $ aslref Ihvlx-2.asl

Test Iqjtn_b-2.asl:
  $ aslref Iqjtn_b-2.asl

Test Ivgsp-2.asl:
  $ aslref Ivgsp-2.asl
  File Ivgsp-2.asl, line 6, character 0 to line 9, character 3:
  ASL Typing error: cannot declare already declared element "a".
  [1]

Test Rccvd-2.asl:
  $ aslref Rccvd-2.asl

Test Rhhcd-2.asl:
  $ aslref Rhhcd-2.asl

Test Rphnz-2.asl:
  $ aslref Rphnz-2.asl

Test Rtznr-2.asl:
  $ aslref Rtznr-2.asl

Test Ryyfr-2.asl:
  $ aslref Ryyfr-2.asl
  File Ryyfr-2.asl, line 13, characters 4 to 14:
  ASL Typing error: overlapping slices 0+:1, 0+:1.
  [1]


Test Ibyvl-2.asl:
  $ aslref Ibyvl-2.asl

Test Ijdcc-2.asl:
  $ aslref Ijdcc-2.asl
  File Ijdcc-2.asl, line 6, character 0 to line 8, character 2:
  ASL Static error: Cannot extract from bitvector of length 5 slices 7+:4.
  [1]

Test Iqjtn_c-2.asl:
  $ aslref Iqjtn_c-2.asl
  Uncaught exception: exc {err_code: 0}
  [1]

Test Ivylk-2.asl:
  $ aslref Ivylk-2.asl

Test Rcnhb-2.asl:
  $ aslref Rcnhb-2.asl
  File Rcnhb-2.asl, line 5, character 0 to line 7, character 2:
  ASL Typing error: overlapping slices 0+:4, 1+:3.
  [1]

Test Rhqzy-2.asl:
  $ aslref Rhqzy-2.asl

Test Rprzn-2.asl:
  $ aslref Rprzn-2.asl
  Hello

Test Rvczx-2.asl:
  $ aslref Rvczx-2.asl

Test Rzcvd-2.asl:
  $ aslref Rzcvd-2.asl


Test Icdvy-2.asl:
  $ aslref Icdvy-2.asl

Test Ikfcr-2.asl:
  $ aslref Ikfcr-2.asl

Test Iqjtn_d-2.asl:
  $ aslref Iqjtn_d-2.asl
  Uncaught exception: exc {err_code: 0}
  [1]

Test Iwlnm-2.asl:
  $ aslref Iwlnm-2.asl
  File Iwlnm-2.asl, line 6, characters 8 to 14:
  ASL Error: Cannot parse.
  [1]

Test Rcrqj-2.asl:
  $ aslref Rcrqj-2.asl

Test Rirnq-2.asl:
  $ aslref Rirnq-2.asl
  File Rirnq-2.asl, line 6, characters 11 to 16:
  ASL Execution error: Assertion failed: FALSE
  [1]

Test Rqdqd-2.asl:
  $ aslref Rqdqd-2.asl
  File Rqdqd-2.asl, line 5, characters 13 to 14:
  ASL Error: Cannot parse.
  [1]

Test Rvgzf-2.asl:
  $ aslref Rvgzf-2.asl

Test Rzndl-2.asl:
  $ aslref Rzndl-2.asl


Test Ifpvz-2.asl:
  $ aslref Ifpvz-2.asl

Test Iknxj-2.asl:
  $ aslref Iknxj-2.asl

Test Iqnsd-2.asl:
  $ aslref Iqnsd-2.asl

Test Ixfpv-2.asl:
  $ aslref Ixfpv-2.asl
  File Ixfpv-2.asl, line 13, characters 12 to 19:
  ASL Error: There are no field 'item2' on type (integer {10}, integer {20}).
  [1]

Test Rdfwz-2.asl:
  $ aslref Rdfwz-2.asl

Test Rjpvl-2.asl:
  $ aslref Rjpvl-2.asl
  File Rjpvl-2.asl, line 16, characters 4 to 5:
  ASL Error: Cannot parse.
  [1]

Test Rqqbb-2.asl:
  $ aslref Rqqbb-2.asl

Test Rvnkt-2.asl:
  $ aslref Rvnkt-2.asl

Test Rzsnd-2.asl:
  $ aslref Rzsnd-2.asl


Test Ighgk-2.asl:
  $ aslref Ighgk-2.asl

Test Iktjn-2.asl:
  $ aslref Iktjn-2.asl
  File Iktjn-2.asl, line 21, characters 4 to 6:
  ASL Typing error: a subtype of bits(N) was expected, provided bits(wid).
  [1]

Test Irkbv-2.asl:
  $ aslref Irkbv-2.asl

Test Iyklf-2.asl:
  $ aslref Iyklf-2.asl
  a

Test Rdjmc-2.asl:
  $ aslref Rdjmc-2.asl

Test Rkldr-2.asl:
  $ aslref Rkldr-2.asl

Test Rqwsq-2.asl:
  $ aslref Rqwsq-2.asl
  0
  0

Test Rwqrn-2.asl:
  $ aslref Rwqrn-2.asl

Test Rztjn_a-2.asl:
  $ aslref Rztjn_a-2.asl

