# Compiler
This directory contains the sources of the Free Pascal Compiler.
If you want to compile/modify the compiler, please read first the
programmers manual.

To recompile the compiler, you can use the make utility as following
```shell
      make OS_TARGET="compiler OS target" \
      CPU_TARGET="compiler CPU target" \
      FPCCPUOPT="Optimization level" \
      PP="compiler used to compile FPC" \
      COMPILER_OPTIONS="Options passed to compiler"
```

If an option is omitted, then target CPU/OS will be same as current CPU/OS

## Possibles targets
linux go32v2 win32 os2 freebsd beos netbsd amiga haiku
atari sunos qnx netware openbsd wdosx palmos macos macosx emx

## Possible compiler switches
| Name of switch | Description of switch | Required? |
|---|---|---|
| GDB | support of the GNU Debugger | Yes |
| I386 | generate a compiler for the Intel i386+ | No |
| x86_64 | generate a compiler for the AMD x86-64 architecture | No |
| M68K | generate a compiler for the M68000 | No |
| SPARC | generate a compiler for SPARC | No |
| POWERPC | generate a compiler for the PowerPC | No |
| VIS | generate a compile for the VIS | No |
| DEBUG | version with debug code is generated | No |
| EXTDEBUG | some extra debug code is executed | No |
| SUPPORT_MMX | only i386: releases the compiler switch MMX which allows the compiler to generate MMX instructions | No |
| EXTERN_MSG | Don't compile the msgfiles in the compiler, always use external messagefiles, default for TP | No |
| NOAG386INT | no Intel Assembler output | No |
| NOAG386NSM | no NASM output | No |
| NOAG386BIN | leaves out the binary writer, default for TP | No |
| NORA386DIR | No direct i386 assembler reader | No |
| TEST_GENERIC | Test Generic version of code generator (uses generic RTL calls) | No |
| cpuflags | The target processor has status flags (on by default) |  |
| cpufpemu | The target compiler will also support emitting software floating point operations |  |
| cpu64bitaddr | The targets use a 64-bit address space (pointers and the default integer type are 64 bit) |  |
| cpu64bitalu | The target cpu has 64-bit registers available (unless cpu64bitaddr is also defined, pointers and default integer type remain 32 bit, but the cpu can perform 64 bit calculations directly without needing helpers) |  |

## Examples
- Required switches for a i386 compiler be compiled by Free Pascal Compiler:
    GDB;I386

- To build a compiler to SPARC target using a Win32/i386 you just use:
    make CPU_TARGET=SPARC
