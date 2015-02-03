set -e
DIR=`dirname $0`
REPOS=svn+ssh://secsvn@svn-sem.cl.cam.ac.uk/WeakMemory
. $DIR/version.sh
. $DIR/funs.sh
TMP=/var/tmp
EXPORT=$TMP/exp.$$
mkdir $EXPORT
( cd $EXPORT && svn export -N $REPOS/mem.new )
NAME=ibm_compare
FINAL=$TMP/$NAME
/bin/rm -rf $FINAL && mkdir $FINAL
( cd $EXPORT/mem.new && tar chf - . ) | \
( cd $FINAL && tar xf - . ) && \
/bin/rm -rf $EXPORT
(
cat <<'EOF'
OCB=ocamlbuild -classic-display

default:compare.byte

compare.byte compare.native:
	$(OCB)	$@

clean:
	/bin/rm -f compare.byte compare.native *~
	$(OCB) -clean
EOF
) > $FINAL/Makefile
( cleandir $FINAL )
( cd $FINAL && /bin/rm -rf tests *.ref README Makefile-test \@* *.ref *.sh *.mk .svnignore )
( cd $TMP && tar zcf $NAME.tar.gz $NAME )
( mv $TMP/$NAME.tar.gz . && /bin/rm -rf $FINAL )