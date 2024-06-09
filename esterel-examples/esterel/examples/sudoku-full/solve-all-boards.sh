#!/bin/bash

SOLVER=${SOLVER:-racket-sudoku.rkt}

# raco make -v "${SOLVER}"

BOARDS=`racket "${SOLVER}" --list | tail -n +2`
for B in ${BOARDS}; do
  printf "\n\n\n\nCalling racket %s --board %s " "${SOLVER}" "${B}"
  echo "$@"
  /usr/bin/time -p racket "${SOLVER}" --board ${B} "$@"
  if [ $? -ne 0 ]; then exit 1; fi
done
