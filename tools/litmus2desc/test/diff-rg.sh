#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname "${BASH_SOURCE[0]}")" && pwd)
REPO_ROOT=$(cd -- "$SCRIPT_DIR/../../.." && pwd)
READERS_GUIDE="$SCRIPT_DIR/readers-guide.txt"
LITMUS_DIR="$SCRIPT_DIR/litmus_tests"
DIFF_TOOL="wdiff"
FILTER_TEST=""

usage() {
  cat <<'EOF'
Usage: diff-rg.sh [--diff-tool wdiff|diff] [TEST_NAME]

  --diff-tool   Select the diff tool to use (default: wdiff)
  TEST_NAME     Optional test name to filter the readers guide
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --diff-tool)
      if [[ $# -lt 2 ]]; then
        echo "Missing argument for --diff-tool" >&2
        exit 1
      fi
      DIFF_TOOL="$2"
      shift 2
      ;;
    --diff-tool=*)
      DIFF_TOOL="${1#*=}"
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      if [[ -n "$FILTER_TEST" ]]; then
        echo "Unexpected argument: $1" >&2
        exit 1
      fi
      FILTER_TEST="$1"
      shift
      ;;
  esac
done

case "$DIFF_TOOL" in
  wdiff|diff)
    ;;
  *)
    echo "Unsupported diff tool '$DIFF_TOOL'. Allowed values: wdiff, diff" >&2
    exit 1
    ;;
esac

if [[ ! -r "$READERS_GUIDE" ]]; then
  echo "readers-guide.txt not found at $READERS_GUIDE" >&2
  exit 1
fi

if [[ ! -d "$LITMUS_DIR" ]]; then
  echo "litmus_tests directory not found at $LITMUS_DIR" >&2
  exit 1
fi

if [[ -n "$FILTER_TEST" ]]; then
  FILTER_TEST=${FILTER_TEST%$'\r'}
  FILTER_TEST=${FILTER_TEST#"${FILTER_TEST%%[![:space:]]*}"}
  FILTER_TEST=${FILTER_TEST%"${FILTER_TEST##*[![:space:]]}"}
fi

if ! command -v dune >/dev/null; then
  echo "dune not found on PATH" >&2
  exit 1
fi

case "$DIFF_TOOL" in
  wdiff)
    if ! command -v wdiff >/dev/null; then
      echo "wdiff not found on PATH" >&2
      exit 1
    fi
    ;;
  diff)
    if ! command -v diff >/dev/null; then
      echo "diff not found on PATH" >&2
      exit 1
    fi
    ;;
esac

tmp_dir=$(mktemp -d)
trap 'rm -rf "$tmp_dir"' EXIT

status=0
current_test=""
current_prefix=""
skip_leading_blank=1
filter_seen=0

should_run_current() {
  [[ -z "$FILTER_TEST" || "$current_test" == "$FILTER_TEST" ]]
}

run_current() {
  if [[ -z "${current_test:-}" ]]; then
    return
  fi

  if ! should_run_current; then
    return
  fi

  local expected_file="${current_prefix}.expected"
  local actual_file="${current_prefix}.actual"
  local diff_file="${current_prefix}.${DIFF_TOOL}"
  local litmus_file="$LITMUS_DIR/$current_test.litmus"

  if [[ ! -f "$litmus_file" ]]; then
    echo "Missing litmus test file for $current_test" >&2
    status=1
    return
  fi

  if [[ -n "$FILTER_TEST" && "$current_test" == "$FILTER_TEST" ]]; then
    filter_seen=1
  fi

  if ! (cd "$REPO_ROOT" && dune exec litmus2desc -- "$litmus_file") > "$actual_file"; then
    echo "Failed to run litmus2desc for $current_test" >&2
    status=1
    return
  fi

  if [[ "$DIFF_TOOL" == "wdiff" ]]; then
    local filtered_diff="${current_prefix}.filtered"
    if ! wdiff -n "$expected_file" "$actual_file" > "$diff_file"; then
      awk 'index($0,"{+") || index($0,"[-")' "$diff_file" > "$filtered_diff"
      if [[ -s "$filtered_diff" ]]; then
        echo "Comparing $current_test"
        cat "$filtered_diff"
      fi
      status=1
    fi
  else
    local diff_status=0
    if diff -u -- "$expected_file" "$actual_file" > "$diff_file"; then
      diff_status=0
    else
      diff_status=$?
    fi

    case "$diff_status" in
      0)
        return
        ;;
      1)
        echo "Comparing $current_test"
        cat "$diff_file"
        status=1
        ;;
      *)
        echo "diff failed for $current_test" >&2
        status=1
        ;;
    esac
  fi
}

while IFS='' read -r line || [[ -n $line ]]; do
  if [[ "$line" == \#\ * ]]; then
    run_current
    current_test=${line#\# }
    current_test=${current_test%$'\r'}
    current_test=${current_test#"${current_test%%[![:space:]]*}"}
    current_test=${current_test%"${current_test##*[![:space:]]}"}
    current_prefix="$tmp_dir/$(printf '%s' "$current_test" | tr -c 'A-Za-z0-9._+-' '_')"
    : > "${current_prefix}.expected"
    skip_leading_blank=1
    continue
  fi

  if [[ -n "${current_test:-}" ]]; then
    if [[ $skip_leading_blank -eq 1 ]]; then
      if [[ -z "${line//[[:space:]]/}" ]]; then
        continue
      fi
      skip_leading_blank=0
    fi

    printf '%s\n' "$line" >> "${current_prefix}.expected"
  fi

done < "$READERS_GUIDE"

run_current

if [[ -n "$FILTER_TEST" && $filter_seen -eq 0 ]]; then
  echo "Test '$FILTER_TEST' not found in readers-guide.txt" >&2
  status=1
fi

exit "$status"
