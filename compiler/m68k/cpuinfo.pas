{
    $Id$
    Copyright (c) 1998-2002 by the Free Pascal development team

    Basic Processor information for the PowerPC

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Unit CPUInfo;

Interface

  uses
    globtype;

Type
   bestreal = real;
   ts32real = single;
   ts64real = double;
   ts80real = extended;
   ts128real = type extended;
   ts64comp = extended;

   pbestreal=^bestreal;

   { possible supported processors for this target }
   tprocessors =
      (no_processor,
       MC68000,
       MC68020,
       Coldfire
      );

   tfputype =
     (no_fpuprocessor,
      fpu_soft,
      fpu_libgcc,
      fpu_68881
     );

Const
   { calling conventions supported by the code generator }
   supported_calling_conventions : tproccalloptions = [
     pocall_internproc,
     pocall_compilerproc,
     pocall_inline,
     pocall_stdcall,
     { the difference to stdcall is only the name mangling }
     pocall_cdecl,
     { the difference to stdcall is only the name mangling }
     pocall_cppdecl,
     { this used by the PalmOS port only }
     pocall_syscall
   ];

   processorsstr : array[tprocessors] of string[5] = ('',
     '68000',
     '68020',
     'COLDFIRE'
   );

   fputypestr : array[tfputype] of string[6] = ('',
     'SOFT',
     'LIBGCC',
     '68881'
   );

Implementation

end.
{
  $Log$
  Revision 1.15  2005-02-14 17:13:10  peter
    * truncate log

}
