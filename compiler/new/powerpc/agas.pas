{
    $Id$
    Copyright (c) 1997 by Florian Klaempfl

    This unit implements an asm for the PowerPC

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
unit agas;

  interface

    uses
       dos,globals,systems,errors,cobjects,aasm,alpha,strings,files
{$ifdef GDB}
       ,gdb
{$endif GDB}
       ;

    type
      palphaattasmlist=^talphaattasmlist;
      talphaattasmlist=object(tasmlist)
        procedure WriteTree(p:paasmoutput);virtual;
        procedure WriteAsmList;virtual;
{$ifdef GDB}
        procedure WriteFileLineInfo(var fileinfo : tfileposinfo);
{$endif}
      end;

  implementation

    const
       op2str : array[tasmop] of string[14] = ('<none>',
    'add','add_','addo','addo_','addc','addc_','addco','addco_,
    'adde','adde_','addeo','addeo_','addi','addic','addic_','addis,
    'addme','addme_','addmeo','addmeo_','addze','addze_','addzeo,
    'addzeo_','and','and_','andc','andc_','andi_','andis_','b,
    'ba','bl','bla','bc','bca','bcl','bcla','bcctr','bcctrl','bclr,
    'bclrl','cmp','cmpi','cmpl','cmpli','cntlzw','cntlzw_','crand,
    'crandc','creqv','crnand','crnor','cror','crorc','crxor','dcba,
    'dcbf','dcbi','dcbst','dcbt','divw','divw_','divwo','divwo_,
    'divwu','divwu_','divwuo','divwuo_','eciwx','ecowx','eieio','eqv,
    'eqv_','extsb','extsb_','extsh','extsh_','fabs','fabs_','fadd,
    'fadd_','fadds','fadds_','fcompo','fcmpu','fctiw','fctw_','fctwz,
    'fctwz_','fdiv','fdiv_','fdivs','fdivs_','fmadd','fmadd_','fmadds,
    'fmadds_','fmr','fmsub','fmsub_','fmsubs','fmsubs_','fmul','fmul_,
    'fmuls','fmuls_','fnabs','fnabs_','fneg','fneg_','fnmadd,
    'fnmadd_','fnmadds','fnmadds_','fnmsub','fnmsub_','fnmsubs,
    'fnmsubs_','fres','fres_','frsp','frsp_','frsqrte','frsqrte_,
    'fsel','fsel_','fsqrt','fsqrt_','fsqrts','fsqrts_','fsub','fsub_,
    'fsubs','fsubs_','icbi','isync','lbz','lbzu','lbzux','lbzx,
    'lfd','lfdu','lfdux','lfdx','lfs','lfsu','lfsux','lfsx','lha,
    'lhau','lhaux','lhax','hbrx','lhz','lhzu','lhzux','lhzx','lmw,
    'lswi','lswx','lwarx','lwbrx','lwz','lwzu','lwzux','lwzx','mcrf,
    'mcrfs','lcrxe','mfcr','mffs','maffs_','mfmsr','mfspr','mfsr,
    'mfsrin','mftb','mtfcrf','mtfd0','mtfsb1','mtfsf','mtfsf_,
    'mtfsfi','mtfsfi_','mtmsr','mtspr','mtsr','mtsrin','mulhw,
    'mulhw_','mulhwu','mulhwu_','mulli','mullh','mullw_','mullwo,
    'mullwo_','nand','nand_','neg','neg_','nego','nego_','nor','nor_,
    'or','or_','orc','orc_','ori','oris', 'rfi', 'rlwimi', 'rlwimi_',
    'rlwinm', 'tlwinm_','rlwnm','sc','slw', 'slw_', 'sraw', 'sraw_,
    'srawi', 'srawi_','srw', 'srw_', 'stb', 'stbu', 'stbux','stbx','stfd',
    'stfdu', 'stfdux', 'stfdx', 'stfiwx', 'stfs', 'stfsu', 'stfsux', 'stfsx',
    'sth', 'sthbrx', 'sthu', 'sthux', 'sthx', 'stmw', 'stswi', 'stswx', 'stw',
    'stwbrx', 'stwx_', 'stwu', 'stwux', 'stwx', 'subf', 'subf_', 'subfo',
    'subfo_', 'subfc', 'subc_', 'subfco', 'subfco_', 'subfe', 'subfe_',
    'subfeo', 'subfeo_', 'subfic', 'subfme', 'subfme_', 'subfmeo', 'subfmeo_,
    'subfze', 'subfze_', 'subfzeo', 'subfzeo_', 'sync', 'tlbia', 'tlbie,
    'tlbsync', 'tw', 'twi', 'xor', 'xor_', 'xori', 'xoris',
    { some simplified mnemonics }
    'subi', 'subis', 'subic', 'subic_', 'sub', 'sub_', 'subo', 'subo_',
    'subc', 'subc_', 'subco', '_subco_', 'cmpwi', 'cmpw', 'cmplwi', 'cmplw',
    'extlwi', 'extlwi_', 'extrwi', 'extrwi_', 'inslwi', 'inslwi_', 'insrwi',
    'insrwi_', 'rotlwi', 'rotlwi_', 'rotlw', 'rotlw_', 'slwi', 'slwi_',
    'srwi', 'srwi_', 'clrlwi', 'clrlwi_', 'clrrwi', 'clrrwi_', 'clrslwi',
    'clrslwi_', 'blr', 'bctr', 'blrl', 'bctrl', 'crset', 'crclr', 'crmove',
    'crnot', 'mt', 'mf','nop', 'li', 'la', 'mr','not', 'mtcr');

{$ifdef GDB}
      procedure talphaattasmlist.WriteFileLineInfo(var fileinfo : tfileposinfo);
        var
          curr_n : byte;
        begin
          if not (cs_debuginfo in aktmoduleswitches) then
           exit;
        { file changed ? (must be before line info) }
          if (fileinfo.fileindex<>0) and
             (stabslastfileinfo.fileindex<>fileinfo.fileindex) then
           begin
             infile:=current_module^.sourcefiles^.get_file(fileinfo.fileindex);
             if includecount=0 then
              curr_n:=n_sourcefile
             else
              curr_n:=n_includefile;
             if (infile^.path^<>'') then
              begin
                AsmWriteLn(#9'.stabs "'+lower(BsToSlash(FixPath(infile^.path^,false)))+'",'+
                  tostr(curr_n)+',0,0,'+'Ltext'+ToStr(IncludeCount));
              end;
             AsmWriteLn(#9'.stabs "'+lower(FixFileName(infile^.name^))+'",'+
               tostr(curr_n)+',0,0,'+'Ltext'+ToStr(IncludeCount));
             AsmWriteLn('Ltext'+ToStr(IncludeCount)+':');
             inc(includecount);
           end;
        { line changed ? }
          if (stabslastfileinfo.line<>fileinfo.line) and (fileinfo.line<>0) then
           begin
             if (n_line=n_textline) and assigned(funcname) and
                (target_os.use_function_relative_addresses) then
              begin
                AsmWriteLn(target_asm.labelprefix+'l'+tostr(linecount)+':');
                AsmWrite(#9'.stabn '+tostr(n_line)+',0,'+tostr(fileinfo.line)+','+
                           target_asm.labelprefix+'l'+tostr(linecount)+' - ');
                AsmWritePChar(FuncName);
                AsmLn;
                inc(linecount);
              end
             else
              AsmWriteLn(#9'.stabd'#9+tostr(n_line)+',0,'+tostr(fileinfo.line));
           end;
          stabslastfileinfo:=fileinfo;
        end;
{$endif GDB}

    procedure talphaattasmlist.WriteTree(p:paasmoutput);
    const
      allocstr : array[boolean] of string[10]=(' released',' allocated');
    type
      t64bitarray = array[0..7] of byte;
      t32bitarray = array[0..3] of byte;
    var
      ch       : char;
      hp       : pai;
      consttyp : tait;
      s        : string;
      found    : boolean;
      i,pos,l  : longint;
      co       : comp;
      sin      : single;
      d        : double;
      e        : extended;
      op       : tasmop;
      calljmp,
      do_line  : boolean;
      sep      : char;
    begin
      if not assigned(p) then
       exit;
      do_line:=(cs_debuginfo in aktmoduleswitches) or (cs_asm_source in aktglobalswitches);
      hp:=pai(p^.first);
      while assigned(hp) do
       begin
         aktfilepos:=hp^.fileinfo;
         if do_line then
          begin
          { I think it is better to write stabs before source line PM }
{$ifdef GDB}
          { write stabs }
            if cs_debuginfo in aktmoduleswitches then
             begin
               if not (hp^.typ in  [
                      ait_label,
                      ait_regalloc,ait_tempalloc,
                      ait_stabn,ait_stabs,ait_section,
                      ait_cut,ait_marker,ait_align,ait_stab_function_name]) then
                 begin
                    WriteFileLineInfo(hp^.fileinfo);
                 end;
             end;
{$endif GDB}
          { load infile }
            if lastfileinfo.fileindex<>hp^.fileinfo.fileindex then
             begin
               infile:=current_module^.sourcefiles^.get_file(hp^.fileinfo.fileindex);
               { open only if needed !! }
               if (cs_asm_source in aktglobalswitches) then
                 infile^.open;
               { avoid unnecessary reopens of the same file !! }
               lastfileinfo.fileindex:=hp^.fileinfo.fileindex;
               { be sure to change line !! }
               lastfileinfo.line:=-1;
             end;
          { write source }
            if (cs_asm_source in aktglobalswitches) and
                not (hp^.typ in  [
                      ait_label,
                      ait_stabn,ait_stabs,ait_section,
                      ait_cut,ait_align,ait_stab_function_name]) then
             begin
               if (infile<>lastinfile) and assigned(lastinfile) then
                 begin
                   AsmWriteLn(target_asm.comment+'['+infile^.name^+']');
                   lastinfile^.close;
                 end;
               if (hp^.fileinfo.line<>lastfileinfo.line) and
                  (hp^.fileinfo.line<infile^.maxlinebuf) then
                 begin
                   if (hp^.fileinfo.line<>0) and
                      (infile^.linebuf^[hp^.fileinfo.line]>=0) then
                     AsmWriteLn(target_asm.comment+'['+tostr(hp^.fileinfo.line)+'] '+
                       fixline(infile^.GetLineStr(hp^.fileinfo.line)));
                   { set it to a negative value !
                   to make that is has been read already !! PM }
                   infile^.linebuf^[hp^.fileinfo.line]:=-infile^.linebuf^[hp^.fileinfo.line]-1;
                end;
               lastfileinfo:=hp^.fileinfo;
               lastinfile:=infile;
             end;
          end;

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

           ait_align :
             begin
               AsmWrite(#9'.balign '+tostr(pai_align(hp)^.aligntype));
               if pai_align(hp)^.use_op then
                AsmWrite(','+tostr(pai_align(hp)^.fillop));
               AsmLn;
             end;

           ait_section :
             begin
               if pai_section(hp)^.sec<>sec_none then
                begin
                  AsmLn;
                  AsmWriteLn(ait_section2str(pai_section(hp)^.sec));
{$ifdef GDB}
                  lastfileinfo.line:=-1;
{$endif GDB}
                end;
             end;

           ait_datablock :
             begin
               if pai_datablock(hp)^.is_global then
                AsmWrite(#9'.comm'#9)
               else
                AsmWrite(#9'.lcomm'#9);
               AsmWrite(pai_datablock(hp)^.sym^.name);
               AsmWriteLn(','+tostr(pai_datablock(hp)^.size));
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
               AsmWrite(#9'.long'#9+pai_const_symbol(hp)^.sym^.name);
               if pai_const_symbol(hp)^.offset>0 then
                 AsmWrite('+'+tostr(pai_const_symbol(hp)^.offset))
               else if pai_const_symbol(hp)^.offset<0 then
                 AsmWrite(tostr(pai_const_symbol(hp)^.offset));
               AsmLn;
             end;

           ait_const_rva :
             AsmWriteLn(#9'.rva'#9+pai_const_symbol(hp)^.sym^.name);

           ait_real_64bit :
             begin
               if do_line then
                AsmWriteLn(target_asm.comment+double2str(pai_real_64bit(hp)^.value));
               d:=pai_real_64bit(hp)^.value;
               AsmWrite(#9'.byte'#9);
               for i:=0 to 7 do
                begin
                  if i<>0 then
                   AsmWrite(',');
                  AsmWrite(tostr(t64bitarray(d)[i]));
                end;
               AsmLn;
             end;

           ait_real_32bit :
             begin
               if do_line then
                AsmWriteLn(target_asm.comment+single2str(pai_real_32bit(hp)^.value));
               sin:=pai_real_32bit(hp)^.value;
               AsmWrite(#9'.byte'#9);
               for i:=0 to 3 do
                begin
                  if i<>0 then
                   AsmWrite(',');
                  AsmWrite(tostr(t32bitarray(sin)[i]));
                end;
               AsmLn;
             end;

           ait_comp_64bit :
             begin
               if do_line then
                AsmWriteLn(target_asm.comment+comp2str(pai_comp_64bit(hp)^.value));
               AsmWrite(#9'.byte'#9);
{$ifdef FPC}
               co:=comp(pai_comp_64bit(hp)^.value);
{$else}
               co:=pai_comp_64bit(hp)^.value;
{$endif}
               for i:=0 to 7 do
                begin
                  if i<>0 then
                   AsmWrite(',');
                  AsmWrite(tostr(t64bitarray(co)[i]));
                end;
               AsmLn;
             end;

           ait_direct :
             begin
               AsmWritePChar(pai_direct(hp)^.str);
               AsmLn;
{$IfDef GDB}
               if strpos(pai_direct(hp)^.str,'.data')<>nil then
                 n_line:=n_dataline
               else if strpos(pai_direct(hp)^.str,'.text')<>nil then
                 n_line:=n_textline
               else if strpos(pai_direct(hp)^.str,'.bss')<>nil then
                 n_line:=n_bssline;
{$endif GDB}
             end;

           ait_string :
             begin
               pos:=0;
               for i:=1 to pai_string(hp)^.len do
                begin
                  if pos=0 then
                   begin
                     AsmWrite(#9'.ascii'#9'"');
                     pos:=20;
                   end;
                  ch:=pai_string(hp)^.str[i-1];
                  case ch of
                     #0, {This can't be done by range, because a bug in FPC}
                #1..#31,
             #128..#255 : s:='\'+tostr(ord(ch) shr 6)+tostr((ord(ch) and 63) shr 3)+tostr(ord(ch) and 7);
                    '"' : s:='\"';
                    '\' : s:='\\';
                  else
                   s:=ch;
                  end;
                  AsmWrite(s);
                  inc(pos,length(s));
                  if (pos>line_length) or (i=pai_string(hp)^.len) then
                   begin
                     AsmWriteLn('"');
                     pos:=0;
                   end;
                end;
             end;

           ait_label :
             begin
               if (pai_label(hp)^.l^.is_used) then
                begin
                  if pai_label(hp)^.l^.typ=AS_GLOBAL then
                    AsmWriteLn('.globl'#9+pai_label(hp)^.l^.name);
                  AsmWriteLn(pai_label(hp)^.l^.name+':');
                end;
             end;

           ait_symbol :
             begin
               if pai_symbol(hp)^.is_global then
                AsmWriteLn('.globl'#9+pai_symbol(hp)^.sym^.name);
               if target_info.target=target_i386_linux then
                begin
                   AsmWrite(#9'.type'#9+pai_symbol(hp)^.sym^.name);
                   if assigned(pai(hp^.next)) and
                      (pai(hp^.next)^.typ in [ait_const_symbol,ait_const_rva,
                         ait_const_32bit,ait_const_16bit,ait_const_8bit,ait_datablock,
                         ait_real_32bit,ait_real_64bit,ait_real_80bit,ait_comp_64bit]) then
                    AsmWriteLn(',@object')
                   else
                    AsmWriteLn(',@function');
                   if pai_symbol(hp)^.sym^.size>0 then
                    AsmWriteLn(#9'.size'#9+pai_symbol(hp)^.sym^.name+', '+tostr(pai_symbol(hp)^.sym^.size));
                end;
               AsmWriteLn(pai_symbol(hp)^.sym^.name+':');
             end;

           ait_symbol_end :
             begin
               if target_info.target=target_i386_linux then
                begin
                  s:=target_asm.labelprefix+'e'+tostr(symendcount);
                  inc(symendcount);
                  AsmWriteLn(s+':');
                  AsmWriteLn(#9'.size'#9+pai_symbol(hp)^.sym^.name+', '+s+' - '+pai_symbol(hp)^.sym^.name);
                end;
             end;

           ait_instruction :
             begin

               op:=pai386(hp)^.opcode;
               calljmp:=is_calljmp(op);
             { call maybe not translated to calll }
               s:=#9+att_op2str[op]+cond2str[pai386(hp)^.condition];
               if (not calljmp) and
                  (not att_nosuffix[op]) and
                  not(
                   (pai386(hp)^.oper[0].typ=top_reg) and
                   (pai386(hp)^.oper[0].reg in [R_ST..R_ST7])
                  ) then
                s:=s+att_opsize2str[pai386(hp)^.opsize];
             { process operands }
               if pai386(hp)^.ops<>0 then
                begin
                { call and jmp need an extra handling                          }
                { this code is only called if jmp isn't a labeled instruction }
                  if calljmp then
                   s:=s+#9+getopstr_jmp(pai386(hp)^.oper[0])
                  else
                   begin
                     for i:=0to pai386(hp)^.ops-1 do
                      begin
                        if i=0 then
                         sep:=#9
                        else
                         sep:=',';
                        s:=s+sep+getopstr(pai386(hp)^.oper[i])
                      end;
                   end;
                end;
               AsmWriteLn(s);
             end;

{$ifdef GDB}
           ait_stabs :
             begin
               AsmWrite(#9'.stabs ');
               AsmWritePChar(pai_stabs(hp)^.str);
               AsmLn;
             end;

           ait_stabn :
             begin
               AsmWrite(#9'.stabn ');
               AsmWritePChar(pai_stabn(hp)^.str);
               AsmLn;
             end;

           ait_force_line :
             stabslastfileinfo.line:=0;

           ait_stab_function_name:
             funcname:=pai_stab_function_name(hp)^.str;
{$endif GDB}

           ait_cut :
             begin
               if SmartAsm then
                begin
                { only reset buffer if nothing has changed }
                  if AsmSize=AsmStartSize then
                   AsmClear
                  else
                   begin
                     AsmClose;
                     DoAssemble;
                     if pai_cut(hp)^.EndName then
                      IsEndFile:=true;
                     AsmCreate;
                   end;
                { avoid empty files }
                  while assigned(hp^.next) and (pai(hp^.next)^.typ in [ait_cut,ait_section,ait_comment]) do
                   begin
                     if pai(hp^.next)^.typ=ait_section then
                       lastsec:=pai_section(hp^.next)^.sec;
                     hp:=pai(hp^.next);
                   end;
{$ifdef GDB}
                  { force write of filename }
                  FillChar(stabslastfileinfo,sizeof(stabslastfileinfo),0);
                  includecount:=0;
                  funcname:=nil;
                  WriteFileLineInfo(hp^.fileinfo);
{$endif GDB}
                  if lastsec<>sec_none then
                    AsmWriteLn(ait_section2str(lastsec));
                  AsmStartSize:=AsmSize;
                end;
             end;

           ait_marker :
             ;

           else
             internalerror(10000);
         end;
         hp:=pai(hp^.next);
       end;
    end;


    procedure talphaattasmlist.WriteAsmList;
    var
      p:dirstr;
      n:namestr;
      e:extstr;
{$ifdef GDB}
      fileinfo : tfileposinfo;
{$endif GDB}

    begin
{$ifdef EXTDEBUG}
      if assigned(current_module^.mainsource) then
       Comment(v_info,'Start writing GNU-styled assembler output for '+current_module^.mainsource^);
{$endif}

      LastSec:=sec_none;
{$ifdef GDB}
      FillChar(stabslastfileinfo,sizeof(stabslastfileinfo),0);
{$endif GDB}
      FillChar(lastfileinfo,sizeof(lastfileinfo),0);
      LastInfile:=nil;

      if assigned(current_module^.mainsource) then
       fsplit(current_module^.mainsource^,p,n,e)
      else
       begin
         p:=inputdir;
         n:=inputfile;
         e:=inputextension;
       end;
    { to get symify to work }
      AsmWriteLn(#9'.file "'+FixFileName(n+e)+'"');

{$ifdef GDB}
      n_line:=n_bssline;
      funcname:=nil;
      linecount:=1;
      includecount:=0;
      fileinfo.fileindex:=1;
      fileinfo.line:=1;
      { Write main file }
      WriteFileLineInfo(fileinfo);
{$endif GDB}
      AsmStartSize:=AsmSize;
      symendcount:=0;

      countlabelref:=false;
      If (cs_debuginfo in aktmoduleswitches) then
        WriteTree(debuglist);
      WriteTree(codesegment);
      WriteTree(datasegment);
      WriteTree(consts);
      WriteTree(rttilist);
      Writetree(resourcestringlist);
      WriteTree(bsssegment);
      Writetree(importssection);
      Writetree(exportssection);
      Writetree(resourcesection);
      countlabelref:=true;

      AsmLn;
{$ifdef EXTDEBUG}
      if assigned(current_module^.mainsource) then
       comment(v_info,'Done writing att-styled assembler output for '+current_module^.mainsource^);
{$endif EXTDEBUG}
    end;


end.
{
  $Log$
  Revision 1.1  1999-08-03 23:37:52  jonas
    + initial implementation for PowerPC based on the Alpha stuff

}
