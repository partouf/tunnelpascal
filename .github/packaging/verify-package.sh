#!/bin/sh
#
# Prove that a finalized package drives its own compiler.
#
# Usage: verify-package.sh <packdir> [fpc options...]
#
# Guards issue #18: bin/fpc used to fall back to whatever ppcXXX was on $PATH,
# which on a CI runner with fpc-3.2.2 installed looks like a working build. So
# a directory of poisoned ppcXXX stubs is prepended to $PATH -- if the driver
# ever resolves a compiler through $PATH again it hits one of those and fails
# loudly, instead of quietly compiling with the wrong compiler. ($PATH is left
# otherwise intact; the compiler still needs as/ld from it.)
#
# Extra arguments are passed to fpc (e.g. -Parm for the cross builds). The
# produced binary is only executed when it is built for this host.
#
set -eu

PACKDIR=$(cd "$1" && pwd)
shift

HERE=$(cd "$(dirname "$0")" && pwd)
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

mkdir "$WORK/poison"
for name in ppc386 ppcx64 ppcarm ppca64 ppcrossarm ppcrossa64 ppcrossx64; do
    cat > "$WORK/poison/$name" <<'EOF'
#!/bin/sh
echo "verify-package: bin/fpc resolved $0 through \$PATH instead of the bundled compiler" >&2
exit 1
EOF
    chmod +x "$WORK/poison/$name"
done
PATH="$WORK/poison:$PATH"
export PATH

FPC="$PACKDIR/bin/fpc"

version=$("$FPC" -iV)
echo "verify-package: bin/fpc -iV -> $version"
case $version in
    3.3.1) ;;
    *) echo "verify-package: expected the bundled 3.3.1 compiler, got $version" >&2
       exit 1 ;;
esac

cp "$HERE/smoketest.pas" "$WORK/"
( cd "$WORK" && "$FPC" "$@" smoketest.pas )

if [ $# -eq 0 ]; then
    output=$("$WORK/smoketest")
    echo "verify-package: smoketest -> $output"
    [ "$output" = "big" ] || { echo "verify-package: unexpected output" >&2; exit 1; }
fi

echo "verify-package: OK"
