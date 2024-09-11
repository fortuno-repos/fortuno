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
  --exclude-from=${SCRIPT_DIR}/${configdir}/repo.ignore \
  --include-from=${SCRIPT_DIR}/${configdir}/repo.include \
  --exclude='*' \
  ${SCRIPT_DIR}/../../ ${destdir}
rsync -av ${SCRIPT_DIR}/${configdir}/addons/ ${destdir}