{
    $Id$
    Copyright (c) 1996,97 by Florian Klaempfl

    This unit implements an asmoutput class for the Nasm assembler with
    Intel syntax for the i386+

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
{$ifdef TP}
  {$N+,E+}
{$endif}
unit ag386nsm;

    interface

    uses aasm,assemble;

    type
      pi386nasmasmlist=^ti386nasmasmlist;
      ti386nasmasmlist = object(tasmlist)
        procedure WriteTree(p:paasmoutput);virtual;
        procedure WriteAsmList;virtual;
        procedure WriteExternals;
      end;

  implementation

    uses
      strings,
      globtype,globals,systems,cobjects,
      files,verbose,cpubase,cpuasm
{$ifdef GDB}
      ,gdb
{$endif GDB}
      ;

    const
      line_length = 64;

{$ifdef EXTTYPE}
      extstr : array[EXT_NEAR..EXT_ABS] of String[8] =
             ('NEAR','FAR','PROC','BYTE','WORD','DWORD',
              'CODEPTR','DATAPTR','FWORD','PWORD','QWORD','TBYTE','ABS');
{$endif}

    function single2str(d : single) : string;
      var
         hs : string;
         p : byte;
      begin
         str(d,hs);
      { nasm expects a lowercase e }
         p:=pos('E',hs);
         if p>0 then
          hs[p]:='e';
         p:=pos('+',hs);
         if p>0 then
          delete(hs,p,1);
         single2str:=lower(hs);
      end;

    function double2str(d : double) : string;
      var
         hs : string;
         p : byte;
      begin
         str(d,hs);
      { nasm expects a lowercase e }
         p:=pos('E',hs);
         if p>0 then
          hs[p]:='e';
         p:=pos('+',hs);
         if p>0 then
          delete(hs,p,1);
         double2str:=lower(hs);
      end;

    function extended2str(e : extended) : string;
      var
         hs : string;
         p : byte;
      begin
         str(e,hs);
      { nasm expects a lowercase e }
         p:=pos('E',hs);
         if p>0 then
          hs[p]:='e';
         p:=pos('+',hs);
         if p>0 then
          delete(hs,p,1);
         extended2str:=lower(hs);
      end;


    function comp2str(d : bestreal) : string;
      type
        pdouble = ^double;
      var
        c  : comp;
        dd : pdouble;
      begin
{$ifdef FPC}
         c:=comp(d);
{$else}
         c:=d;
{$endif}
         dd:=pdouble(@c); { this makes a bitwise copy of c into a double }
         comp2str:=double2str(dd^);
      end;


    function getreferencestring(const ref : treference) : string;
    var
      s     : string;
      first : boolean;
    begin
      if ref.is_immediate then
       begin
         getreferencestring:=tostr(ref.offset);
         exit;
       end
      else
      with ref do
        begin
          first:=true;
          if ref.segment<>R_NO then
           s:='['+int_reg2str[segment]+':'
          else
           s:='[';
         if assigned(symbol) then
          begin
            s:=s+symbol^.name;
            first:=false;
          end;
         if (base<>R_NO) then
          begin
            if not(first) then
             s:=s+'+'
            else
             first:=false;
             s:=s+int_reg2str[base];
          end;
         if (index<>R_NO) then
           begin
             if not(first) then
               s:=s+'+'
             else
               first:=false;
             s:=s+int_reg2str[index];
             if scalefactor<>0 then
               s:=s+'*'+tostr(scalefactor);
           end;
         if offset<0 then
           s:=s+tostr(offset)
         else if (offset>0) then
           s:=s+'+'+tostr(offset);
         s:=s+']';
        end;
       getreferencestring:=s;
     end;

    function sizestr(s:topsize;dest:boolean):string;
      begin
        case s of
           S_B : sizestr:='byte ';
           S_W : sizestr:='word ';
           S_L : sizestr:='dword ';
           S_IS : sizestr:='word ';
           S_IL : sizestr:='dword ';
           S_IQ : sizestr:='qword ';
           S_FS : sizestr:='dword ';
           S_FL : sizestr:='qword ';
           S_FX : sizestr:='tword ';
           S_BW : if dest then
               sizestr:='word '
             else
               sizestr:='byte ';
           S_BL : if dest then
               sizestr:='dword '
             else
               sizestr:='byte ';
           S_WL : if dest then
               sizestr:='dword '
             else
               sizestr:='word ';
        end;
      end;


    function getopstr(const o:toper;s : topsize; opcode: tasmop;ops:longint;dest : boolean) : string;
      var
        hs : string;
      begin
        case o.typ of
          top_reg :
            getopstr:=int_nasmreg2str[o.reg];
          top_const :
            begin
              if (ops=1) and (opcode<>A_RET) then
               getopstr:=sizestr(s,dest)+tostr(o.val)
              else
               getopstr:=tostr(o.val);
            end;
          top_symbol :
            begin
              if assigned(o.sym) then
               hs:='dword '+o.sym^.name
              else
               hs:='dword ';
              if o.symofs>0 then
               hs:=hs+'+'+tostr(o.symofs)
              else
               if o.symofs<0 then
                hs:=hs+tostr(o.symofs)
               else
                if not(assigned(o.sym)) then
                 hs:=hs+'0';
              getopstr:=hs;
            end;
          top_ref :
            begin
              hs:=getreferencestring(o.ref^);
              if not ((opcode = A_LEA) or (opcode = A_LGS) or
                      (opcode = A_LSS) or (opcode = A_LFS) or
                      (opcode = A_LES) or (opcode = A_LDS) or
                      (opcode = A_SHR) or (opcode = A_SHL) or
                      (opcode = A_SAR) or (opcode = A_SAL) or
                      (opcode = A_OUT) or (opcode = A_IN)) then
               begin
                 hs:=sizestr(s,dest)+hs;
               end;
              getopstr:=hs;
            end;
          else
            internalerror(10001);
        end;
      end;

    function getopstr_jmp(const o:toper) : string;
      var
        hs : string;
      begin
        case o.typ of
          top_reg :
            getopstr_jmp:=int_nasmreg2str[o.reg];
          top_ref :
            getopstr_jmp:=getreferencestring(o.ref^);
          top_const :
            getopstr_jmp:=tostr(o.val);
          top_symbol :
            begin
              hs:=o.sym^.name;
              if o.symofs>0 then
               hs:=hs+'+'+tostr(o.symofs)
              else
               if o.symofs<0 then
                hs:=hs+tostr(o.symofs);
              getopstr_jmp:='NEAR '+hs;
            end;
          else
            internalerror(10001);
        end;
      end;


{****************************************************************************
                               Ti386nasmasmlist
 ****************************************************************************}

    var
      LastSec : tsection;

    const
      ait_const2str:array[ait_const_32bit..ait_const_8bit] of string[8]=
        (#9'DD'#9,#9'DW'#9,#9'DB'#9);

    Function PadTabs(const p:string;addch:char):string;
    var
      s : string;
      i : longint;
    begin
      i:=length(p);
      if addch<>#0 then
       begin
         inc(i);
         s:=p+addch;
       end
      else
       s:=p;
      if i<8 then
       PadTabs:=s+#9#9
      else
       PadTabs:=s+#9;
    end;


    procedure ti386nasmasmlist.WriteTree(p:paasmoutput);
    const
      allocstr : array[boolean] of string[10]=(' released',' allocated');
    var
      s,
      prefix,
      suffix   : string;
      hp       : pai;
      counter,
      lines,
      i,j,l    : longint;
      consttyp : tait;
      found,
      quoted   : boolean;
      sep      : char;
    begin
      if not assigned(p) then
       exit;
      hp:=pai(p^.first);
      while assigned(hp) do
       begin
         case hp^.typ of
           ait_comment :
             Begin
               AsmWrite(target_asm.comment);
               AsmWritePChar(pai_asm_comment(hp)^.str);
               AsmLn;
             End;

           ait_regalloc :
             begin
               if (cs_asm_regalloc in aktglobalswitches) then
                 AsmWriteLn(target_asm.comment+'Register '+att_reg2str[pairegalloc(hp)^.reg]+
                   allocstr[pairegalloc(hp)^.allocation]);
             end;

           ait_tempalloc :
             begin
               if (cs_asm_tempalloc in aktglobalswitches) then
                 AsmWriteLn(target_asm.comment+'Temp '+tostr(paitempalloc(hp)^.temppos)+','+
                   tostr(paitempalloc(hp)^.tempsize)+allocstr[paitempalloc(hp)^.allocation]);
             end;

           ait_section :
             begin
               if pai_section(hp)^.sec<>sec_none then
                begin
                  AsmLn;
                  AsmWriteLn('SECTION '+target_asm.secnames[pai_section(hp)^.sec]);
                end;
               LastSec:=pai_section(hp)^.sec;
             end;

           ait_align :
             AsmWriteLn(#9'ALIGN '+tostr(pai_align(hp)^.aligntype));

           ait_datablock :
             begin
               if pai_datablock(hp)^.is_global then
                begin
                  AsmWrite(#9'GLOBAL ');
                  AsmWriteLn(pai_datablock(hp)^.sym^.name);
                end;
               AsmWrite(PadTabs(pai_datablock(hp)^.sym^.name,':'));
               AsmWriteLn('RESB'#9+tostr(pai_datablock(hp)^.size));
             end;

           ait_const_32bit,
           ait_const_16bit,
           ait_const_8bit :
             begin
               AsmWrite(ait_const2str[hp^.typ]+tostr(pai_const(hp)^.value));
               consttyp:=hp^.typ;
               l:=0;
               repeat
                 found:=(not (Pai(hp^.next)=nil)) and (Pai(hp^.next)^.typ=consttyp);
                 if found then
                  begin
                    hp:=Pai(hp^.next);
                    s:=','+tostr(pai_const(hp)^.value);
                    AsmWrite(s);
                    inc(l,length(s));
                  end;
               until (not found) or (l>line_length);
               AsmLn;
             end;

           ait_const_symbol :
             begin
               AsmWrite(#9#9'DD'#9);
               AsmWrite(pai_const_symbol(hp)^.sym^.name);
               if pai_const_symbol(hp)^.offset>0 then
                 AsmWrite('+'+tostr(pai_const_symbol(hp)^.offset))
               else if pai_const_symbol(hp)^.offset<0 then
                 AsmWrite(tostr(pai_const_symbol(hp)^.offset));
               AsmLn;
             end;

           ait_const_rva :
             begin
               AsmWrite(#9#9'RVA'#9);
               AsmWriteLn(pai_const_symbol(hp)^.sym^.name);
             end;

           ait_real_32bit :
             AsmWriteLn(#9#9'DD'#9+single2str(pai_real_32bit(hp)^.value));

           ait_real_64bit :
             AsmWriteLn(#9#9'DQ'#9+double2str(pai_real_64bit(hp)^.value));

           ait_real_80bit :
             AsmWriteLn(#9#9'DT'#9+extended2str(pai_real_80bit(hp)^.value));

           ait_comp_64bit :
             AsmWriteLn(#9#9'DQ'#9+comp2str(pai_real_80bit(hp)^.value));

           ait_string :
             begin
               counter := 0;
               lines := pai_string(hp)^.len div line_length;
             { separate lines in different parts }
               if pai_string(hp)^.len > 0 then
                Begin
                  for j := 0 to lines-1 do
                   begin
                     AsmWrite(#9#9'DB'#9);
                     quoted:=false;
                     for i:=counter to counter+line_length-1 do
                        begin
                          { it is an ascii character. }
                          if (ord(pai_string(hp)^.str[i])>31) and
                             (ord(pai_string(hp)^.str[i])<128) and
                             (pai_string(hp)^.str[i]<>'"') then
                              begin
                                if not(quoted) then
                                    begin
                                      if i>counter then
                                        AsmWrite(',');
                                      AsmWrite('"');
                                    end;
                                AsmWrite(pai_string(hp)^.str[i]);
                                quoted:=true;
                              end { if > 31 and < 128 and ord('"') }
                          else
                              begin
                                  if quoted then
                                      AsmWrite('"');
                                  if i>counter then
                                      AsmWrite(',');
                                  quoted:=false;
                                  AsmWrite(tostr(ord(pai_string(hp)^.str[i])));
                              end;
                       end; { end for i:=0 to... }
                     if quoted then AsmWrite('"');
                       AsmWrite(target_os.newline);
                     inc(counter,line_length);
                  end; { end for j:=0 ... }
                { do last line of lines }
                AsmWrite(#9#9'DB'#9);
                quoted:=false;
                for i:=counter to pai_string(hp)^.len-1 do
                  begin
                    { it is an ascii character. }
                    if (ord(pai_string(hp)^.str[i])>31) and
                       (ord(pai_string(hp)^.str[i])<128) and
                       (pai_string(hp)^.str[i]<>'"') then
                        begin
                          if not(quoted) then
                              begin
                                if i>counter then
                                  AsmWrite(',');
                                AsmWrite('"');
                              end;
                          AsmWrite(pai_string(hp)^.str[i]);
                          quoted:=true;
                        end { if > 31 and < 128 and " }
                    else
                        begin
                          if quoted then
                            AsmWrite('"');
                          if i>counter then
                              AsmWrite(',');
                          quoted:=false;
                          AsmWrite(tostr(ord(pai_string(hp)^.str[i])));
                        end;
                  end; { end for i:=0 to... }
                if quoted then
                  AsmWrite('"');
                end;
               AsmLn;
             end;

           ait_label :
             begin
               if pai_label(hp)^.l^.is_used then
                AsmWriteLn(pai_label(hp)^.l^.name+':');
             end;

           ait_direct :
             begin
               AsmWritePChar(pai_direct(hp)^.str);
               AsmLn;
             end;

           ait_symbol :
             begin
               if pai_symbol(hp)^.is_global then
                begin
                  AsmWrite(#9'GLOBAL ');
                  AsmWriteLn(pai_symbol(hp)^.sym^.name);
                end;
               AsmWrite(pai_symbol(hp)^.sym^.name);
               if assigned(hp^.next) and not(pai(hp^.next)^.typ in
                  [ait_const_32bit,ait_const_16bit,ait_const_8bit,
                   ait_const_symbol,ait_const_rva,
                   ait_real_32bit,ait_real_64bit,ait_real_80bit,ait_comp_64bit,ait_string]) then
                AsmWriteLn(':')
             end;

           ait_symbol_end :
             begin
             end;

           ait_instruction :
             begin
             { We need intel order, no At&t }
               paicpu(hp)^.SwapOperands;
             { Reset }
               suffix:='';
               prefix:='';
               s:='';
               if paicpu(hp)^.ops<>0 then
                begin
                  if is_calljmp(paicpu(hp)^.opcode) then
                   s:=#9+getopstr_jmp(paicpu(hp)^.oper[0])
                  else
                   begin
                     for i:=0to paicpu(hp)^.ops-1 do
                      begin
                        if i=0 then
                         sep:=#9
                        else
                         sep:=',';
                        s:=s+sep+getopstr(paicpu(hp)^.oper[i],paicpu(hp)^.opsize,paicpu(hp)^.opcode,
                          paicpu(hp)^.ops,(i=2));
                      end;
                   end;
                end;
               if paicpu(hp)^.opcode=A_FWAIT then
                AsmWriteln(#9#9'DB'#9'09bh')
               else
                AsmWriteLn(#9#9+prefix+int_op2str[paicpu(hp)^.opcode]+
                  cond2str[paicpu(hp)^.condition]+suffix+s);
             end;
{$ifdef GDB}
           ait_stabn,
           ait_stabs,
           ait_force_line,
           ait_stab_function_name : ;
{$endif GDB}

           ait_cut :
             begin
             { only reset buffer if nothing has changed }
               if AsmSize=AsmStartSize then
                AsmClear
               else
                begin
                  AsmClose;
                  DoAssemble;
                  AsmCreate(pai_cut(hp)^.place);
                end;
             { avoid empty files }
               while assigned(hp^.next) and (pai(hp^.next)^.typ in [ait_cut,ait_section,ait_comment]) do
                begin
                  if pai(hp^.next)^.typ=ait_section then
                    lastsec:=pai_section(hp^.next)^.sec;
                  hp:=pai(hp^.next);
                end;
               if lastsec<>sec_none then
                 AsmWriteLn('SECTION '+target_asm.secnames[lastsec]);
               AsmStartSize:=AsmSize;
             end;

           ait_marker : ;

           else
             internalerror(10000);
         end;
         hp:=pai(hp^.next);
       end;
    end;


    var
      currentasmlist : PAsmList;

    procedure writeexternal(p:pnamedindexobject);{$ifndef FPC}far;{$endif}
      begin
        if pasmsymbol(p)^.typ=AS_EXTERNAL then
         currentasmlist^.AsmWriteln('EXTERN'#9+p^.name);
      end;

    procedure ti386nasmasmlist.WriteExternals;
      begin
        currentasmlist:=@self;
        AsmSymbolList^.foreach({$ifndef TP}@{$endif}writeexternal);
      end;


    procedure ti386nasmasmlist.WriteAsmList;
    begin
{$ifdef EXTDEBUG}
      if assigned(current_module^.mainsource) then
       comment(v_info,'Start writing nasm-styled assembler output for '+current_module^.mainsource^);
{$endif}
      LastSec:=sec_none;
      AsmWriteLn('BITS 32');
      AsmLn;

      countlabelref:=false;

      WriteExternals;

    { Nasm doesn't support stabs
      WriteTree(debuglist);}

      WriteTree(codesegment);
      WriteTree(datasegment);
      WriteTree(consts);
      WriteTree(rttilist);
      WriteTree(resourcestringlist);
      WriteTree(bsssegment);
      countlabelref:=true;

      AsmLn;
{$ifdef EXTDEBUG}
      if assigned(current_module^.mainsource) then
       comment(v_info,'Done writing nasm-styled assembler output for '+current_module^.mainsource^);
{$endif EXTDEBUG}
   end;

end.
{
  $Log$
  Revision 1.54  1999-11-06 14:34:16  peter
    * truncated log to 20 revs

  Revision 1.53  1999/11/02 15:06:56  peter
    * import library fixes for win32
    * alignment works again

  Revision 1.52  1999/09/13 16:27:24  peter
    * fix for jmps to be always near
    * string writing fixed

  Revision 1.51  1999/09/10 15:41:18  peter
    * added symbol_end

  Revision 1.50  1999/09/02 18:47:43  daniel
    * Could not compile with TP, some arrays moved to heap
    * NOAG386BIN default for TP
    * AG386* files were not compatible with TP, fixed.

  Revision 1.49  1999/08/25 11:59:38  jonas
    * changed pai386, paippc and paiapha (same for tai*) to paicpu (taicpu)

  Revision 1.48  1999/08/04 00:22:37  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.47  1999/08/01 18:28:10  florian
    * modifications for the new code generator

  Revision 1.46  1999/07/22 09:37:33  florian
    + resourcestring implemented
    + start of longstring support

  Revision 1.45  1999/07/18 14:47:20  florian
    * bug 487 fixed, (inc(<property>) isn't allowed)
    * more fixes to compile with Delphi

  Revision 1.44  1999/07/18 10:19:41  florian
    * made it compilable with Dlephi 4 again
    + fixed problem with large stack allocations on win32

  Revision 1.43  1999/06/02 22:44:02  pierre
   * previous wrong log corrected

  Revision 1.42  1999/06/02 22:25:27  pierre
  * changed $ifdef FPC @ into $ifndef TP

  Revision 1.41  1999/06/01 14:45:44  peter
    * @procvar is now always needed for FPC

  Revision 1.40  1999/05/27 19:44:02  peter
    * removed oldasm
    * plabel -> pasmlabel
    * -a switches to source writing automaticly
    * assembler readers OOPed
    * asmsymbol automaticly external
    * jumptables and other label fixes for asm readers

  Revision 1.39  1999/05/23 18:41:57  florian
    * better error recovering in typed constants
    * some problems with arrays of const fixed, some problems
      due my previous
       - the location type of array constructor is now LOC_MEM
       - the pushing of high fixed
       - parameter copying fixed
       - zero temp. allocation removed
    * small problem in the assembler writers fixed:
      ref to nil wasn't written correctly

  Revision 1.38  1999/05/21 13:54:43  peter
    * NEWLAB for label as symbol

  Revision 1.37  1999/05/12 00:19:39  peter
    * removed R_DEFAULT_SEG
    * uniform float names

  Revision 1.36  1999/05/11 16:28:16  peter
    * long lines fixed

  Revision 1.35  1999/05/10 15:18:16  peter
    * fixed condition writing

  Revision 1.34  1999/05/08 19:52:34  peter
    + MessagePos() which is enhanced Message() function but also gets the
      position info
    * Removed comp warnings

}
