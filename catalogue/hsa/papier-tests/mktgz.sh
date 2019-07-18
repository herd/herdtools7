#! /bin/sh
##Build a fake source distribution (for jocaml driver)
set +e
DIR=`dirname $0`
TMP=/var/tmp/dir.$$
mkdir -p $TMP/src
(
cat <<'EOF'
echo Do nothing
EOF
) > $TMP/src/build.sh
(
cat <<EOF
#! /bin/sh
DIR=\$(dirname \$0)
herd7 -I \$DIR -bell hsa.bell \$*
EOF
) > $TMP/src/run.sh
chmod +x $TMP/src/run.sh
(
cat <<'EOF'
#! /bin/sh
DEST=$1
for f in run.sh *.cat *.bell
do
  if [ -f $f ]
  then
    cp $f $DEST
  fi
done
EOF
) > $TMP/src/install.sh
cp ../../../papier/hsa-cats/*.cat $TMP/src
cp ./hsa.bell $TMP/src
#build tar
( cd $TMP && tar zcf hsa.tgz src )
cp $TMP/hsa.tgz .
/bin/rm -rf $TMP