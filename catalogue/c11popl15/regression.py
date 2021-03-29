#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function
import argparse
import copy
import glob
import os
import subprocess
import sys

WIDTH=64
VERBOSE=False
TEST_DIR=os.path.join("tests", "illustrative")
VARIANTS_DIR=os.path.join(TEST_DIR, "templates", "variants")

def path_of_test(test):
  return os.path.join(TEST_DIR, test)

def path_of_variant(test):
  return os.path.join(VARIANTS_DIR, test)

def check(test, RF, SC, RS, ST, args):
  cmd = [ "./genmodel.py",
          "--RF", RF,
          "--SC", SC,
          "--RS", RS,
          "--ST", ST,
          "--",
          test ] + args.herdflags
  if args.herd7:
    cmd = [cmd[0]] + [ '--input', 'c11.cat7-template', '--herd7' ] + cmd[1:]
  if VERBOSE: print(cmd)
  out = subprocess.check_output(cmd)
  return out

def expectFail(test, RF, SC, RS, ST, args):
  _head, tail = os.path.split(test)
  print("{0} is impossible... ".format(tail).ljust(WIDTH), end="")
  out = check(test, RF, SC, RS, ST, args)
  if "No" in out: print("PASS")
  else:
    print("FAIL")
    if VERBOSE: print(out)
 #if args.herdflags == [] and "States 0" not in out and not "Bad executions (0 in total)" in out:
 #  print("POSSIBLE RACE")

def expectPass(test, RF, SC, RS, ST, args):
  _head, tail = os.path.split(test)
  print("{0} is possible...    ".format(tail).ljust(WIDTH), end="")
  out = check(test, RF, SC, RS, ST, args)
  if "Ok" in out: print("PASS")
  else:
    print("FAIL")
    if VERBOSE: print(out)
 #if args.herdflags == [] and "States 0" not in out and not "Bad executions (0 in total)" in out:
 #  print("POSSIBLE RACE")

def expectRace(test, RF, SC, RS, ST, args):
  _head, tail = os.path.split(test)
  print("{0} is racy...        ".format(tail).ljust(WIDTH), end="")
  out = check(test, RF, SC, RS, ST, args)
  if not "Bad executions (0 in total)" in out: print ("PASS")
  else:
    print("FAIL")
    if VERBOSE: print(out)

def setup_models(args):
  global std, naive, arfna, rseq, arf, allmodels
  std   = ("ConsRFna", "SCorig", "RSorig", "STorig", args)
  naive = ("Naive",    "SCorig", "RSorig", "STorig", args)
  arfna = ("Arfna",    "SCorig", "RSorig", "STorig", args)
  rseq  = ("ConsRFna", "SCorig", "RSnew",  "STorig", args)
  arf   = ("Arf",      "SCorig", "RSorig", "STorig", args)
  allmodels = []
  for rf in ["ConsRFna","Naive","Arf","Arfna"]:
    for sc in ["SCorig","SCnew"]:
      for rs in ["RSorig","RSnew"]:
        for st in ["STorig","STnew"]:
          m = (rf, sc, rs, st, args)
          allmodels.append(m)

def print_model(m):
  s = "---+ MODEL {0}_{1}_{2}_{3}".format(m[0], m[1], m[2], m[3])
  if m == std: s += " (std)"
  print(s)

def regression(args):
  ## Section 1
  ## Standard Source-to-Source Transformations are Invalid in C11
  expectPass(path_of_test("lb.litmus"), *std)
  expectPass(path_of_test("cyc.litmus"), *std)
  expectFail(path_of_test("seq.litmus"), *std)
  expectPass(path_of_test("seq2.litmus"), *std)

  # Section 3
  # Strengthening is Unsound
  expectFail(path_of_test("strengthen.litmus"), *std)
  expectPass(path_of_test("strengthen2.litmus"), *std)
  # Roach Motel Reorderings are Unsound
  expectFail(path_of_test("roachmotel.litmus"), *std)
  expectPass(path_of_test("roachmotel2.litmus"), *std)
  # Expression Linearisation is Unsound
  expectFail(path_of_test("linearisation.litmus"), *std)
  expectPass(path_of_test("linearisation2.litmus"), *std)

  # Section 4
  # Resolving Causality Cycles and the ConsRFna Axiom
  # Naive Fix
  expectRace(path_of_test("cyc_na.litmus"), *naive)
  # Arfna
  if not args.skip_fig6:
    args2 = copy.deepcopy(args)
    a = vars(args2)
    a["herdflags"].extend(['-speedcheck', 'true'])
    arfna_speedcheck = ("Arfna", "SCorig", "RSorig", "STorig", args2)
    expectFail(path_of_test("fig6.litmus"), *arfna_speedcheck)
    expectPass(path_of_test("fig6_translated.litmus"), *arfna_speedcheck)
  # Strengthening the Release Sequence Definition
  expectRace(path_of_test("rseq_weak.litmus"), *std)
  expectPass(path_of_test("rseq_weak2.litmus"), *std)
  expectPass(path_of_test("rseq_weak.litmus"), *rseq)
  expectPass(path_of_test("rseq_weak2.litmus"), *rseq)

  if args.skip_variants:
    # Appendix A
    if args.skip_non_std_appendixA: models = [std]
    else: models = allmodels
    for m in models:
      print_model(m)
      expectPass(path_of_test("a1.litmus"), *m)
      expectRace(path_of_test("a1_reorder.litmus"), *m)
      expectPass(path_of_test("a2.litmus"), *m)
      expectRace(path_of_test("a2_reorder.litmus"), *m)
      expectPass(path_of_test("a3.litmus"), *m)
      expectRace(path_of_test("a3_reorder.litmus"), *m)
      expectPass(path_of_test("a3v2.litmus"), *m)
      expectFail(path_of_test("a4.litmus"), *m)
      expectPass(path_of_test("a4_reorder.litmus"), *m)
      expectPass(path_of_test("a5.litmus"), *m)
      expectRace(path_of_test("a5_reorder.litmus"), *m)
      expectPass(path_of_test("a6.litmus"), *m)
      expectRace(path_of_test("a6_reorder.litmus"), *m)
      expectPass(path_of_test("a7.litmus"), *m)
      expectRace(path_of_test("a7_reorder.litmus"), *m)
      expectPass(path_of_test("a8.litmus"), *m)
      expectRace(path_of_test("a8_reorder.litmus"), *m)
      expectPass(path_of_test("a9.litmus"), *m)
      expectRace(path_of_test("a9_reorder.litmus"), *m)

    # Appendix B
    expectFail(path_of_test("b.litmus"), *arf)
    expectPass(path_of_test("b_reorder.litmus"), *arf)
  else:
    variants(args)

  # Appendix C
  expectFail(path_of_test("c.litmus"), *arfna)
  expectRace(path_of_test("c_reorder.litmus"), *arfna)
  expectFail(path_of_test("c_p.litmus"), *arfna)
  expectRace(path_of_test("c_p_reorder.litmus"), *arfna)
  expectFail(path_of_test("c_q.litmus"), *arfna)
  expectRace(path_of_test("c_q_reorder.litmus"), *arfna)
  expectFail(path_of_test("c_pq.litmus"), *arfna)
  expectRace(path_of_test("c_pq_reorder.litmus"), *arfna)

def variants(args):
  # Appendix A variants
  if args.skip_non_std_appendixA: models = [std]
  else: models = allmodels
  for m in models:
    print_model(m)
    for test in glob.glob(os.path.join(VARIANTS_DIR, "a1+*")):
      expectPass(test, *std)
    for test in glob.glob(os.path.join(VARIANTS_DIR, "a1_reorder+*")):
      expectRace(test, *std)
    for test in glob.glob(os.path.join(VARIANTS_DIR, "a2+*")):
      expectPass(test, *std)
    for test in glob.glob(os.path.join(VARIANTS_DIR, "a2_reorder+*")):
      expectRace(test, *std)
    for test in glob.glob(os.path.join(VARIANTS_DIR, "a3+*")):
      expectPass(test, *std)
    for test in glob.glob(os.path.join(VARIANTS_DIR, "a3_reorder+*")):
      expectRace(test, *std)
    for test in glob.glob(os.path.join(VARIANTS_DIR, "a5+*")):
      expectPass(test, *std)
    for test in glob.glob(os.path.join(VARIANTS_DIR, "a5_reorder+*")):
      expectRace(test, *std)
    for test in glob.glob(os.path.join(VARIANTS_DIR, "a6+*")):
      expectPass(test, *std)
    for test in glob.glob(os.path.join(VARIANTS_DIR, "a6_reorder+*")):
      expectRace(test, *std)
    for test in glob.glob(os.path.join(VARIANTS_DIR, "a7+*")):
      expectPass(test, *std)
    for test in glob.glob(os.path.join(VARIANTS_DIR, "a7_reorder+*")):
      expectRace(test, *std)

  # Appendix B variants
  for test in glob.glob(os.path.join(VARIANTS_DIR, "b+*")):
    expectFail(test, *arf)
  for test in glob.glob(os.path.join(VARIANTS_DIR, "b_reorder+*")):
    expectPass(test, *arf)

def main(argv=None):
  if argv is None:
    argv = sys.argv[1:]
  parser = argparse.ArgumentParser(
    description="Run model regressions"
  )
  parser.add_argument("--herd7", action="store_true", help="Use herd7 model")
  parser.add_argument("--skip-fig6", action="store_true", help="Skip (long-running) fig6 tests")
  parser.add_argument("--skip-non-std-appendixA", action="store_true", help="Skip non-standard models for Appendix A tests")
  parser.add_argument("--skip-variants", action="store_true", help="Skip test variants")
  parser.add_argument('herdflags', nargs='*', help="Passed to herd command-line")
  args = parser.parse_args(argv)
  setup_models(args)
  regression(args)
  return 0

if __name__ == '__main__':
  sys.exit(main())
