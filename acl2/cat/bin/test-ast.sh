#!/bin/bash

catfile="$1"
outbase="$2"

scriptdir="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
acl2catdir=$(cd "$scriptdir"/../; pwd)
herddir=$(cd "$acl2catdir"/../..; pwd)
mkdir -p "$outbase"

outbase_abs=$(cd "$outbase"; pwd)
astfile="${outbase_abs}"/lsp
outfile="${outbase}"/out

dir=`dirname "$catfile"`
base=`basename "$catfile"`

#  -I "$herddir"/herd/libdir -I "$herddir"/herd-www/cat_includes
if ! ( ( cd $dir; cat2lisp "$base" ) > "$astfile" ); then
    echo "cat2lisp error"
    exit 1;
fi

echo "(include-book \"${acl2catdir}/ast\") (cat::test-ast-file \"${astfile}\")" | acl2 > "$outfile"

status=$?

# note: ACL2 can exit with status 0 for many reasons. Therefore, we
# exit with 43 on success, so 0 really means some unknown failure.
if [[ "$status"==43 ]]; then
    touch "$outbase"/ok
    status=0
elif [[ "$status"==0 ]]; then
    status=3
fi

errmsg_pattern="@#@#@"

fgrep "$errmsg_pattern" "$outfile"

exit $status
