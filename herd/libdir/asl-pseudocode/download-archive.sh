#!/usr/bin/env sh
# SPDX-License-Identifier: BSD-3-Clause

check_archive() (
  filename=$1 expected=$2
  if command -v shasum >/dev/null 2>&1; then
    actual=$(shasum -a 256 "$filename") || return
  else
    actual=$(sha256sum "$filename") || return
  fi
  if [ "${actual%% *}" != "$expected" ]; then
    echo "SHA-256 mismatch for $filename" >&2
    return 1
  fi
)

download_archive() (
  set -eu
  directory=$1 filename=$2 expected=$3
  if [ -f "$filename" ]; then
    check_archive "$filename" "$expected"
    return
  fi
  temporary=$(mktemp -d ./asl-download.XXXXXX)
  trap 'rm -rf "$temporary"' 0
  curl --fail --location --retry 3 \
    "$BASE_URL/$directory/$filename" --output "$temporary/$filename"
  check_archive "$temporary/$filename" "$expected"
  mv "$temporary/$filename" "$filename"
)
