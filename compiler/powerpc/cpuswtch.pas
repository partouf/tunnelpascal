{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl, Pierre Muller

    interprets the commandline options which are powerpc specific

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
unit cpuswtch;

{$i fpcdefs.inc}

interface

uses
  options;

type
  toptionpowerpc=class(toption)
    procedure interpret_proc_specific_options(const opt:string);override;
  end;

implementation

uses
  cutils,globtype,systems,globals;

procedure toptionpowerpc.interpret_proc_specific_options(const opt:string);
var
  more: string;
  j: longint;
begin
  More:=Upper(copy(opt,3,length(opt)-2));
  case opt[2] of
   'O' : Begin
           j := 3;
           While (j <= Length(Opt)) Do
             Begin
               case opt[j] of
                 '-' :
                   begin
                     initglobalswitches:=initglobalswitches-[cs_optimize,cs_fastoptimize,cs_slowoptimize,cs_littlesize,
                       cs_regvars,cs_uncertainopts];
                     FillChar(ParaAlignment,sizeof(ParaAlignment),0);
                   end;
                 'a' :
                   begin
                     UpdateAlignmentStr(Copy(Opt,j+1,255),ParaAlignment);
                     j:=length(Opt);
                   end;
                 'g' : initglobalswitches:=initglobalswitches+[cs_littlesize];
                 'G' : initglobalswitches:=initglobalswitches-[cs_littlesize];
                 'r' :
                   begin
                     initglobalswitches:=initglobalswitches+[cs_regvars];
                     Simplify_ppu:=false;
                   end;
                 'u' : initglobalswitches:=initglobalswitches+[cs_uncertainopts];
                 '1' : initglobalswitches:=initglobalswitches-[cs_fastoptimize,cs_slowoptimize]+[cs_optimize];
                 '2' : initglobalswitches:=initglobalswitches-[cs_slowoptimize]+[cs_optimize,cs_fastoptimize];
                 '3' : initglobalswitches:=initglobalswitches+[cs_optimize,cs_fastoptimize,cs_slowoptimize];
{$ifdef dummy}
                 'p' :
                   Begin
                     If j < Length(Opt) Then
                       Begin
                         Case opt[j+1] Of
                           '1': initoptprocessor := Class386;
                           '2': initoptprocessor := ClassP5;
                           '3': initoptprocessor := ClassP6
                           Else IllegalPara(Opt)
                         End;
                         Inc(j);
                       End
                     Else IllegalPara(opt)
                   End;
{$endif dummy}
                 else IllegalPara(opt);
               End;
               Inc(j)
             end;
         end;
{$ifdef dummy}
   'R' : begin
           if More='GAS' then
            initasmmode:=asmmode_ppc_gas
           else
            if More='MOTOROLA' then
             initasmmode:=asmmode_ppc_motorola
           else
            if More='DIRECT' then
             initasmmode:=asmmode_direct
           else
            IllegalPara(opt);
         end;
{$endif dummy}
  else
   IllegalPara(opt);
  end;
end;


initialization
  coption:=toptionpowerpc;
end.
{
  $Log$
  Revision 1.9  2003-08-09 18:56:54  daniel
    * cs_regalloc renamed to cs_regvars to avoid confusion with register
      allocator
    * Some preventive changes to i386 spillinh code

  Revision 1.8  2002/09/07 17:54:59  florian
    * first part of PowerPC fixes

  Revision 1.7  2002/09/04 16:03:53  jonas
    + parse basic optimization parameters

  Revision 1.6  2002/08/10 14:52:52  carl
    + moved target_cpu_string to cpuinfo
    * renamed asmmode enum.
    * assembler reader has now less ifdef's
    * move from nppcmem.pas -> ncgmem.pas vec. node.

  Revision 1.5  2002/07/28 20:45:23  florian
    + added direct assembler reader for PowerPC

  Revision 1.4  2002/05/18 13:34:26  peter
    * readded missing revisions

  Revision 1.3  2002/05/16 19:46:53  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.1  2002/05/13 19:52:46  peter
    * a ppcppc can be build again
}