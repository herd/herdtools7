set -o errexit

mk_exe () {
for d
do
  awk \
    '/names/ { ok = 1 ; getline ;  }\
    /[)]/ { ok = 0 ; }\
        { if (ok) print $0; }' $d/dune |\
   while read line
   do
       for w in $line
       do
           echo $d/$w.exe
       done
   done
done
}
        

#EXE="$(mk_exe  herd litmus gen tools jingle)"

cpdir () {
  FROM=$1
  TO=$2
  rm -rf $TO && mkdir -p $TO && ( cd $FROM && cp -r . $TO )
}

VERSION=$(grep "^version:" herdtools7.opam | cut -d ":" -f 2 | cut -d '"' -f 2)
REV=$(git rev-parse HEAD 2>/dev/null || echo exported)
