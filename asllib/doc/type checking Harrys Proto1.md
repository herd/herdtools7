git apply --stat ../patches/0001-asl-SUPPORT-DO-NOT-MERGE-parse-empty-definitions.patch

Use bundler from special branch https://github.com/HadrienRenaud/herdtools7/tree/asl-improvements-to-bundler
It takes XML and produces ASL files.

Manually:
adrp.xml has a bug and we comment it out (Harry knows about this)

> python3 bundler.py -m
Produces code in asl-pseudocode, which contains ISA_A32_xml_...

Run similar command on shared pseudo-code but without -m

Then aslref all the files with appropriate -patch0 arguments.
