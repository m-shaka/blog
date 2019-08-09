#!/bin/bash

if [ "${CIRCLE_BRANCH}" = "master" ]; then
  DIFF_TARGET="HEAD^ HEAD"
else
  DIFF_TARGET="origin/master"
fi

DIFF_FILES=(`git diff --name-only --relative=${1} ${DIFF_TARGET}`)

if [ ${#DIFF_FILES[@]} -eq 0 ]; then
  exit 1
else
  exit 0
fi
