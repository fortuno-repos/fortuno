#!/bin/bash

SCRIPT_DIR="$(readlink -f $(dirname ${BASH_SOURCE[0]}))"

configdir=$1
destdir=$2

if [[ $# != 2 ]]; then
  echo "Need two arguments." >&2
  exit 1
fi

rsync \
  -av \
  --exclude-from=${SCRIPT_DIR}/${configdir}/rsync.ignore \
  --include-from=${SCRIPT_DIR}/${configdir}/rsync.include \
  --exclude='*' \
  ${SCRIPT_DIR}/../../ ${destdir}
cp ${SCRIPT_DIR}/${configdir}/fpm.toml ${destdir}