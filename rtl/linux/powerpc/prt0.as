/* Startup code for programs linked with GNU libc.
   Copyright (C) 1998, 1999, 2000, 2001 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */

	.section ".text"
_start:
 	/* Save the stack pointer, in case we're statically linked under Linux.  */
	mr	r9,r1
	/* Set up an initial stack frame, and clear the LR.  */
	clrrwi	r1,r1,4
	li	r0,0
	stwu	r1,-16(r1)
	mtlr	r0
	stw	r0,0(r1)
	b	PASCALMAIN
	
	/* Define a symbol for the first piece of initialized data.  */
	.section ".data"
	.globl	__data_start
__data_start:
data_start:
        .globl  ___fpc_brk_addr         /* heap management */
        .type   ___fpc_brk_addr,@object
        .size   ___fpc_brk_addr,4
___fpc_brk_addr:
        .long   0
/*
  $Log$
  Revision 1.3  2002-08-19 21:19:15  florian
    * small fixes

  Revision 1.2  2002/07/26 17:09:44  florian
    * log fixed

  Revision 1.1  2002/07/26 16:57:40  florian
    + initial version, plain copy from glibc/sysdeps/powerpc/elf/start.S
*/