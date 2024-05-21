#!/bin/bash

make clean > /dev/null

FPC_SRC=$(pwd)
PACKDIR="$FPC_SRC/build"
LIBDIR=$PACKDIR/lib/fpc/3.3.1

rm -Rf "$PACKDIR"
mkdir "$PACKDIR"

make -C compiler FPMAKEOPT="-T 4" "OPTS=-O3 -dEXTDEBUG" "INSTALL_PREFIX=$PACKDIR" all
make -C compiler "INSTALL_PREFIX=$PACKDIR" ZIPINSTALL=1 install

mv "$PACKDIR/lib/fpc/3.2.2" "$LIBDIR"
PP=$LIBDIR/ppcx64

make -C rtl "PP=$PP" "FPCDIR=$LIBDIR" "INSTALL_PREFIX=$PACKDIR" all
make -C packages "PP=$PP" "FPCDIR=$LIBDIR" "INSTALL_PREFIX=$PACKDIR" fpmake

make -C rtl "PP=$PP" "FPCDIR=$LIBDIR" "INSTALL_PREFIX=$PACKDIR" clean

make -C rtl FPC_DOTTEDUNITS=1 "PP=$PP" "FPCDIR=$LIBDIR" FPMAKEOPT="-T 4" "OPT=-O3" "INSTALL_PREFIX=$PACKDIR" all
make -C rtl FPC_DOTTEDUNITS=1 "PP=$PP" "FPCDIR=$LIBDIR" FPMAKEOPT="-T 4" "OPT=-O3" "INSTALL_PREFIX=$PACKDIR" install

make -C packages "FPC_DOTTEDUNITS=1" "PP=$PP" "FPCDIR=$LIBDIR" FPMAKEOPT="-T 4" "OPT=-O3" "INSTALL_PREFIX=$PACKDIR" all
make -C packages "FPC_DOTTEDUNITS=1" "PP=$PP" "FPCDIR=$LIBDIR" FPMAKEOPT="-T 4" "OPT=-O3" "INSTALL_PREFIX=$PACKDIR" install

rm tunnel-pascal.tar.gz

pushd build || exit
tar -cf ../tunnel-pascal.tar ./*
popd || exit
gzip tunnel-pascal.tar

rm -Rf ~/fpc
mkdir -p ~/fpc
tar -xzf tunnel-pascal.tar.gz -C ~/fpc

mkdir ~/fpcetc
~/fpc/lib/fpc/3.3.1/samplecfg ~/fpc/lib/fpc/3.3.1 ~/fpcetc
