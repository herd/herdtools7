set -o errexit

. ./defs-mini.sh

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
