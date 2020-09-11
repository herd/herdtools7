#!/bin/sh

set -eu

BUCKET=c11popl15

CATS=${BUCKET}/c11/cats
mkdir -p ${CATS}

cp c11popl15.shelf ${BUCKET}
cat c11.cfg | grep -v "gen.cat" > ${BUCKET}/c11popl15.cfg

# generate all models
for RF in ConsRFna Naive Arf Arfna; do
  for SC in SCorig SCnew; do
    for RS in RSorig RSnew; do
      for ST in STorig STnew; do
      CATNAME=${RF}_${SC}_${RS}_${ST}.cat
      ./genmodel.py ${*:1} --RF $RF --SC $SC --RS $RS --ST $ST --output ${CATS}/${CATNAME}
      # remove double-empty lines using [unix] cat ;-)
      cat -s ${CATS}/${CATNAME} > ${CATS}/${CATNAME}.tmp
      mv ${CATS}/${CATNAME}.tmp ${CATS}/${CATNAME}
      done;
    done;
  done;
done

# generate all tests
TESTS=${BUCKET}/c11/tests
mkdir -p ${TESTS}
cd tests/illustrative
cp *.litmus ../../${TESTS}
cd templates
for f in *.litmus-template; do
  ./genlitmus.py --input $f --output_base ../../../${TESTS}/z_${f/.litmus-template};
done
