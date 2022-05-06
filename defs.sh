set -o errexit

# Extract binary names from dune files.
binaries_of_dune () {
  local readonly kind="${1}"; shift
  local readonly dune_file="${1}"; shift
  for bin in $(internal/binaries_of_dune "${kind}" "${dune_file}")
  do
    echo "${bin}.native"
  done
}

# List all dune files in the project, excluding duplicates in _build.
all_dune_files () {
  find . \
    -path ./_build -prune -false -o \
    -name 'dune'
}

HERD="$(binaries_of_dune executables herd/dune)"
LITMUS="$(binaries_of_dune executables litmus/dune)"
TOOLS="$(binaries_of_dune executables tools/dune)"
GEN="$(binaries_of_dune executables gen/dune)"
JINGLE="$(binaries_of_dune executables jingle/dune)"

NATIVE="$HERD $LITMUS $TOOLS $GEN $JINGLE"

# Internal-only, not installed.
INTERNAL="$(binaries_of_dune executables internal/dune)"

TESTS="$(all_dune_files | while read f; do binaries_of_dune tests "${f}"; done)"

mk_exe () {
  D=$1
  shift
  for n
  do
    echo $n | sed -e "s|\(.*\).native|$D/\1.exe|g"
  done
}

EXE="$(mk_exe herd $HERD) $(mk_exe litmus $LITMUS) $(mk_exe gen $GEN) $(mk_exe jingle $JINGLE) $(mk_exe tools $TOOLS)"

cpdir () {
  if [ "$#" -ne 2 ]
  then
    echo "Usage: cpdir <from> <to>"
    exit 1
  fi

  local from="${1}"
  local to="${2}"

  rm -rf "${to}" && mkdir -p "${to}" && ( cd "${from}" && cp -r . "${to}" )
}

VERSION=$(cat VERSION.txt)
REV=$(git rev-parse HEAD 2>/dev/null || echo exported)
