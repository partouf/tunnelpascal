{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements an asmoutput class for i386 AT&T syntax

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
{ This unit implements an asmoutput class for i386 AT&T syntax
}
unit agx86att;

{$i fpcdefs.inc}

interface

    uses
      cclasses,cpubase,
      globals,
      aasmbase,aasmtai,assemble,aggas;

    type
      Tx86ATTAssembler=class(TGNUassembler)
      private
        procedure WriteReference(var ref : treference);
        procedure WriteOper(const o:toper);
        procedure WriteOper_jmp(const o:toper);
      public
        procedure WriteInstruction(hp: tai);override;
      end;


  implementation

    uses
      cutils,systems,
      verbose,
      itcpugas,
      cpuinfo,
      cgbase,
      aasmcpu;


{****************************************************************************
                            TX86ATTASMOUTPUT
 ****************************************************************************}

    procedure Tx86AttAssembler.WriteReference(var ref : treference);
      begin
        with ref do
         begin
           { have we a segment prefix ? }
           { These are probably not correctly handled under GAS }
           { should be replaced by coding the segment override  }
           { directly! - DJGPP FAQ                              }
           if segment<>NR_NO then
             AsmWrite(gas_regname(segment)+':');
           if assigned(symbol) then
             AsmWrite(symbol.name);
           if offset<0 then
             AsmWrite(tostr(offset))
           else
            if (offset>0) then
             begin
               if assigned(symbol) then
                AsmWrite('+'+tostr(offset))
               else
                AsmWrite(tostr(offset));
             end
           else if (index=NR_NO) and (base=NR_NO) and (not assigned(symbol)) then
             AsmWrite('0');
           if (index<>NR_NO) and (base=NR_NO) then
            begin
              AsmWrite('(,'+gas_regname(index));
              if scalefactor<>0 then
               AsmWrite(','+tostr(scalefactor)+')')
              else
               AsmWrite(')');
            end
           else
            if (index=NR_NO) and (base<>NR_NO) then
              AsmWrite('('+gas_regname(base)+')')
            else
             if (index<>NR_NO) and (base<>NR_NO) then
              begin
                AsmWrite('('+gas_regname(base)+','+gas_regname(index));
                if scalefactor<>0 then
                 AsmWrite(','+tostr(scalefactor));
                AsmWrite(')');
              end;
         end;
      end;


    procedure Tx86AttAssembler.WriteOper(const o:toper);
      begin
        case o.typ of
          top_reg :
            AsmWrite(gas_regname(o.reg));
          top_ref :
            if o.ref^.refaddr=addr_no then
              WriteReference(o.ref^)
            else
              begin
                AsmWrite('$');
                if assigned(o.ref^.symbol) then
                 AsmWrite(o.ref^.symbol.name);
                if o.ref^.offset>0 then
                 AsmWrite('+'+tostr(o.ref^.offset))
                else
                 if o.ref^.offset<0 then
                  AsmWrite(tostr(o.ref^.offset))
                else
                 if not(assigned(o.ref^.symbol)) then
                   AsmWrite('0');
              end;
          top_const :
              AsmWrite('$'+tostr(o.val));
          else
            internalerror(10001);
        end;
      end;


    procedure Tx86AttAssembler.WriteOper_jmp(const o:toper);
      begin
        case o.typ of
          top_reg :
            AsmWrite('*'+gas_regname(o.reg));
          top_ref :
            begin
              if o.ref^.refaddr=addr_no then
                begin
                  AsmWrite('*');
                  WriteReference(o.ref^);
                end
              else
                begin
                  AsmWrite(o.ref^.symbol.name);
                  if o.ref^.offset>0 then
                   AsmWrite('+'+tostr(o.ref^.offset))
                  else
                   if o.ref^.offset<0 then
                    AsmWrite(tostr(o.ref^.offset));
                end;
            end;
          top_const :
            AsmWrite(tostr(o.val));
          else
            internalerror(10001);
        end;
      end;


    procedure Tx86AttAssembler.WriteInstruction(hp: tai);
      var
       op       : tasmop;
       calljmp  : boolean;
       i        : integer;
      begin
        if hp.typ <> ait_instruction then
          exit;
        taicpu(hp).SetOperandOrder(op_att);
        op:=taicpu(hp).opcode;
        calljmp:=is_calljmp(op);
        AsmWrite(#9);
        { movsd should not be translated to movsl when there
          are (xmm) arguments }
        if (op=A_MOVSD) and (taicpu(hp).ops>0) then
          AsmWrite('movsd')
        else
          AsmWrite(gas_op2str[op]);
        AsmWrite(cond2str[taicpu(hp).condition]);
        { suffix needed ?  fnstsw,fldcw don't support suffixes
          with binutils 2.9.5 under linux }
{        if (Taicpu(hp).oper[0]^.typ=top_reg) and
            (Taicpu(hp).oper[0]^.reg.enum>lastreg) then
          internalerror(200301081);}

        if (not calljmp) and
           (gas_needsuffix[op]<>AttSufNONE) and
           (op<>A_FNSTSW) and
           (op<>A_FSTSW) and
           (op<>A_FNSTCW) and
           (op<>A_FSTCW) and
           (op<>A_FLDCW) and
           not(
               (taicpu(hp).ops<>0) and
               (taicpu(hp).oper[0]^.typ=top_reg) and
               (getregtype(taicpu(hp).oper[0]^.reg)=R_FPUREGISTER)
              ) then
          AsmWrite(gas_opsize2str[taicpu(hp).opsize]);
        { process operands }
        if taicpu(hp).ops<>0 then
          begin
            if calljmp then
             begin
               AsmWrite(#9);
               WriteOper_jmp(taicpu(hp).oper[0]^);
             end
            else
             begin
               for i:=0 to taicpu(hp).ops-1 do
                 begin
                   if i=0 then
                     AsmWrite(#9)
                   else
                     AsmWrite(',');
                   WriteOper(taicpu(hp).oper[i]^);
                 end;
             end;
          end;
        AsmLn;
      end;

{*****************************************************************************
                                  Initialize
*****************************************************************************}

    const
{$ifdef x86_64}
       as_x86_64_as_info : tasminfo =
          (
            id     : as_gas;
            idtxt  : 'AS';
            asmbin : 'as';
            asmcmd : '-o $OBJ $ASM';
            supported_target : system_any;
            flags : [af_allowdirect,af_needar,af_smartlink_sections];
            labelprefix : '.L';
            comment : '# ';
          );
{$else x86_64}
       as_i386_as_info : tasminfo =
          (
            id     : as_gas;
            idtxt  : 'AS';
            asmbin : 'as';
            asmcmd : '-o $OBJ $ASM';
            supported_target : system_any;
            flags : [af_allowdirect,af_needar,af_smartlink_sections];
            labelprefix : '.L';
            comment : '# ';
          );

       as_i386_as_aout_info : tasminfo =
          (
            id           : as_i386_as_aout;
            idtxt  : 'AS_AOUT';
            asmbin : 'as';
            asmcmd : '-o $OBJ $ASM';
            supported_target : system_any;
            flags : [af_allowdirect,af_needar];
            labelprefix : 'L';
            comment : '# ';
          );
{$endif x86_64}

initialization
{$ifdef x86_64}
  RegisterAssembler(as_x86_64_as_info,Tx86ATTAssembler);
{$else x86_64}
  RegisterAssembler(as_i386_as_info,Tx86ATTAssembler);
  RegisterAssembler(as_i386_as_aout_info,Tx86ATTAssembler);
{$endif x86_64}
end.
{
  $Log$
  Revision 1.14  2004-06-16 20:07:11  florian
    * dwarf branch merged

  Revision 1.13.2.6  2004/05/10 21:28:35  peter
    * section_smartlink enabled for gas under linux

  Revision 1.13.2.5  2004/05/01 16:02:10  peter
    * POINTER_SIZE replaced with sizeof(aint)
    * aint,aword,tconst*int moved to globtype

  Revision 1.13.2.4  2004/04/27 18:18:26  peter
    * aword -> aint

  Revision 1.13.2.3  2004/04/26 21:04:04  peter
    * write aint

  Revision 1.13.2.2  2004/04/22 20:20:50  peter
    * fix writing of movsd for xmm

  Revision 1.13.2.1  2004/04/08 18:33:22  peter
    * rewrite of TAsmSection

  Revision 1.13  2004/02/27 10:21:06  florian
    * top_symbol killed
    + refaddr to treference added
    + refsymbol to treference added
    * top_local stuff moved to an extra record to save memory
    + aint introduced
    * tppufile.get/putint64/aint implemented

  Revision 1.12  2003/12/24 00:33:10  florian
    * x86-64 compilation fixed

  Revision 1.11  2003/11/12 16:05:40  florian
    * assembler readers OOPed
    + typed currency constants
    + typed 128 bit float constants if the CPU supports it

  Revision 1.10  2003/10/28 18:46:49  peter
    * fix crash with ops=0

  Revision 1.9  2003/10/21 15:15:36  peter
    * taicpu_abstract.oper[] changed to pointers

  Revision 1.8  2003/10/02 21:18:06  peter
    * remove asw

  Revision 1.7  2003/10/01 20:34:50  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.6  2003/09/23 17:56:06  peter
    * locals and paras are allocated in the code generation
    * tvarsym.localloc contains the location of para/local when
      generating code for the current procedure

  Revision 1.5  2003/09/03 15:55:02  peter
    * NEWRA branch merged

  Revision 1.4.2.1  2003/08/31 15:46:26  peter
    * more updates for tregister

  Revision 1.4  2003/08/18 11:49:47  daniel
    * Made ATT asm writer work with -sr

  Revision 1.3  2003/05/28 23:18:31  florian
    * started to fix and clean up the sparc port

  Revision 1.2  2003/05/22 21:33:31  peter
    * removed some unit dependencies

  Revision 1.1  2003/04/25 12:04:31  florian
    * merged agx64att and ag386att to x86/agx86att

  Revision 1.31  2003/03/23 23:33:10  hajny
    + emx target added

  Revision 1.30  2003/02/19 22:00:15  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.29  2003/01/08 18:43:57  daniel
   * Tregister changed into a record

  Revision 1.28  2003/01/05 13:36:53  florian
    * x86-64 compiles
    + very basic support for float128 type (x86-64 only)

  Revision 1.27  2002/12/24 18:10:34  peter
    * Long symbol names support

  Revision 1.26  2002/08/12 15:08:40  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.25  2002/07/26 21:15:42  florian
    * rewrote the system handling

  Revision 1.24  2002/07/07 09:52:33  florian
    * powerpc target fixed, very simple units can be compiled
    * some basic stuff for better callparanode handling, far from being finished

  Revision 1.23  2002/07/01 18:46:29  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.22  2002/05/18 13:34:21  peter
    * readded missing revisions

  Revision 1.21  2002/05/16 19:46:49  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.19  2002/05/12 16:53:16  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.18  2002/04/15 19:12:10  carl
  + target_info.size_of_pointer -> sizeof(aint)
  + some cleanup of unused types/variables
  * move several constants from cpubase to their specific units
    (where they are used)
  + att_Reg2str -> gas_reg2str
  + int_reg2str -> std_reg2str

  Revision 1.17  2002/04/14 16:58:04  carl
  + move into aggas most of the stuff non-processor specific

  Revision 1.16  2002/04/10 08:07:55  jonas
    * fix for the ie9999 under Linux (patch from Peter)

  Revision 1.15  2002/04/04 19:06:06  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.14  2002/04/04 18:26:55  carl
  + added wdosx patch from Pavel

  Revision 1.13  2002/04/02 17:11:33  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

}
