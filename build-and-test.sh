#!/bin/bash

fpcmake
make -j 4 all FPMAKEOPT="-T 4" "OPT=-Oodfa"

FPC_SRC=$(pwd)
PP=$FPC_SRC/compiler/ppcx64

cd tests || exit
fpcmake -v
make full "TEST_FPC=$PP" TEST_DELTEMP=1 -j 4

cd utils || exit
fpcmake -v
make fpts2junit

cd "$FPC_SRC" || exit
tests/utils/fpts2junit "$FPC_SRC/tests/output/x86_64-linux/" testresult.xml

ERRORS=$(xmllint testresult.xml --xpath "string(/testsuite/@errors)")
FAILURES=$(xmllint testresult.xml --xpath "string(/testsuite/@failures)")

ALL=$((ERRORS + FAILURES))

if [[ $ALL -ne 0 ]]; then
    echo "Numbers of errors: $ALL"
    exit 1
else
    echo "No errors"
fi
