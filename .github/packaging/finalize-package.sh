#!/bin/sh
#
# Make a freshly installed FPC tree relocatable and self-contained.
#
# Usage: finalize-package.sh <packdir> [fpcversion] [binutils-prefix]
#
# `make install` leaves the compiler in <packdir>/lib/fpc/<version>/ppcXXX and
# the fpc driver in <packdir>/bin/fpc, but nothing connects the two: the driver
# looks for ppcXXX next to itself and then falls back to $PATH. On a machine
# with a system FPC that silently runs the *system* compiler, and on a clean
# machine it fails outright (issue #18). Upstream papers over this with the
# compiler's `installsymlink` target plus a post-install samplecfg run, neither
# of which produces a relocatable tree.
#
# So, here:
#   1. put every ppcXXX next to the driver in bin/, and
#   2. install a config file whose unit paths are relative to the compiler.
#
# This touches only the installed tree, never the sources, so upstream merges
# stay conflict-free.
#
set -eu

PACKDIR=$1
FPCVERSION=${2:-3.3.1}
BINUTILSPREFIX=${3:-}

BINDIR="$PACKDIR/bin"
LIBDIR="$PACKDIR/lib/fpc/$FPCVERSION"
CFG="$(dirname "$0")/fpc.cfg"

[ -d "$LIBDIR" ] || { echo "finalize-package: no $LIBDIR" >&2; exit 1; }
[ -d "$BINDIR" ] || { echo "finalize-package: no $BINDIR" >&2; exit 1; }

# 1. Compilers reachable from bin/. On unix a relative symlink keeps the tree
#    relocatable; a zip cannot store symlinks, so windows gets a copy.
found=0
for pp in "$LIBDIR"/ppc*; do
    [ -f "$pp" ] || continue
    name=$(basename "$pp")
    case $name in
        *.exe) cp -f "$pp" "$BINDIR/$name" ;;
        *)     ln -sf "../lib/fpc/$FPCVERSION/$name" "$BINDIR/$name" ;;
    esac
    echo "finalize-package: installed bin/$name"
    found=$((found + 1))
done
[ "$found" -gt 0 ] || { echo "finalize-package: no ppc* in $LIBDIR" >&2; exit 1; }

# 2. Config file. The compiler looks for it in <exepath>/../etc on unix and in
#    <exepath> elsewhere, where exepath is the directory of the compiler binary
#    (symlinks resolved). Install to every location that can be exepath.
#    A cross package also needs the binutils prefix its RTL was assembled with
#    (upstream's stock "-XP$FPCTARGET-" guesses "arm-linux-", which is not the
#    "arm-linux-gnueabihf-" toolchain we build against). Native compilation in
#    the same package must not pick this up, hence FPC_CROSSCOMPILING.
install_cfg() {
    mkdir -p "$1"
    cp -f "$CFG" "$1/fpc.cfg"
    if [ -n "$BINUTILSPREFIX" ]; then
        {
            echo
            echo "#IFDEF FPC_CROSSCOMPILING"
            echo "-XP$BINUTILSPREFIX"
            echo "#ENDIF"
        } >> "$1/fpc.cfg"
    fi
    echo "finalize-package: wrote ${1#"$PACKDIR"/}/fpc.cfg"
}

if ls "$LIBDIR"/ppc*.exe >/dev/null 2>&1; then
    install_cfg "$BINDIR"
    install_cfg "$LIBDIR"
else
    install_cfg "$PACKDIR/lib/fpc/etc"
fi
