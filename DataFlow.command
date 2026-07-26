#!/usr/bin/env bash
set -Eeuo pipefail

on_error() {
  local exit_code=$?
  echo "[ERROR] Line ${BASH_LINENO[0]}: ${BASH_COMMAND}" >&2
  exit "${exit_code}"
}
trap on_error ERR

SCRIPT_DIR="$(cd -P -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"

status=0
"${SCRIPT_DIR}/dataflow" --interactive || status=$?

echo
read -r -p "Press Return to close this window." _
exit "${status}"
