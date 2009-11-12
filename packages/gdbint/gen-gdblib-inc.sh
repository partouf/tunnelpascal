#!/bin/bash

if [ "$1" != "" ]; then
  destdir=$1
fi
if [ "$PATHEXT" != "" ]; then
  EXEEXT=.exe
else
  EXEEXT=
fi

echo "Deleting gdb${EXEEXT} to force recompile"
rm -f gdb${EXEEXT}
echo "Rebuilding gdb${EXEEXT}"
make gdb${EXEEXT} | tee make.log

gdb_full_version=`sed -n "s:.*version.*\"\(.*\)\".*:\1:p" version.c`
gdb_version1=`sed -n "s:.*version.*\"\([0-9]*\)\.\([0-9]*\).*:\1:p" version.c`
gdb_version2=`sed -n "s:.*version.*\"\([0-9]*\)\.\([0-9]*\).*:\2:p" version.c`
gdb_version=`sed -n "s:.*version.*\"\([0-9]*\)\.\([0-9]*\).*:\1.\2:p" version.c`

echo found full version is ${gdb_full_version}
echo found version is ${gdb_version}
if [ ${gdb_version2} -lt 10 ]; then
  gdbversion=${gdb_version1}0${gdb_version2}
else
  gdbversion=${gdb_version1}${gdb_version2}
fi
if [ "${destdir}" == "" ]; then
  destdir=./libgdb
fi

if ! [ -d ${destdir} ]; then
  mkdir ${destdir}
fi

cat make.log | gawk '
BEGIN {
doprint=0
}

/gcc / { doprint=1; }

{
if ( doprint == 1 ) {
  print $0  
}
}

! /\\$/ { doprint=0; }
' | tee comp-cmd.log
# Try to locate all libraries
echo Creating ./copy-libs.sh script
cat comp-cmd.log |gawk -v destdir=${destdir} '
BEGIN {
  print "#!/bin/bash"
  print "# copy-libs.sh generated by awk script"
  print "if [ \"$1\" != \"\" ]; then"
  print "  destdir=$1"
  print " ginstall -d ${destdir}"
  print "else"
  print "  echo $0 destdir"
  print "  echo destdir should be the location where libgdb.a"
  print "  echo and all other archives should be copied"
  print "  exit"
  print "fi"
  print "# Copy gdblib.inc file"
  print "cp gdblib.inc ${destdir}"
}

{
  nb = split ($0,list);
 
  for (i=1; i<=nb; i++) {
  if ( list[i] ~ /lib[^ ]*\.a/ ) {
  staticlib = gensub (/([^ ]*)(lib[^ ]*\.a)/,"\\1\\2 ","g",list[i]);
  print "cp " staticlib " ${destdir}";
  }
  if ( list[i] ~ /-l/ ) {
  systemlib = gensub (/-l([^ ]*)/,"lib\\1.a ","g",list[i]);
  print "systemlib=`find /lib -name " systemlib "`" ;
  print "if [ \"${systemlib}\" != \"\" ]; then";
  print "echo System lib found: ${systemlib}";
  print "cp ${systemlib} ${destdir}";
  print "else";
  print "echo Library " systemlib " not found, shared library assumed";
  print "fi";
  }
  }
}
' | tee copy-libs.sh
chmod u+x ./copy-libs.sh
# For later
 
echo Creating ./gdblib.inc file
# Generate gdblib.inc file
cat comp-cmd.log |gawk -v destdir=${destdir} -v gdbversion=${gdbversion} '
BEGIN {
  use_mingw=0;
  print "{ libgdb.inc file generated by awk script }"
  print "{$define GDB_V" gdbversion " }"
  print "{$ifdef COMPILING_GDBINT_UNIT }"
}

{
  nb = split ($0,list);
 
  for (i=1; i<=nb; i++) {
  if ( list[i] ~ /lib[^ ]*\.a/ ) {
    staticlib = gensub (/([^ ]*)(lib[^ ]*\.a)/,"{$LINKLIB \\2} { found in \\1 }","g",list[i]);
    print staticlib;
    if ( list[i] ~/mingw/ ) {
    use_mingw=1
    }
  }
  if ( list[i] ~ /-l/ ) {
    systemlib = gensub (/-l([^ ]*)/,"{$LINKLIB \\1} { with -l gcc option}","g",list[i]);
    print systemlib;
  }
  }
}
END {
  print "{$endif COMPILING_GDBINT_UNIT }"
  print "{$undef NotImplemented}"
  if ( use_mingw == 1 ) {
    print "{$define USE_MINGW_GDB}"
  }  
}
' | tee  gdblib.inc
 

