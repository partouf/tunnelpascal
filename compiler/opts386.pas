{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl, Pierre Muller

    interprets the commandline options which are i386 specific

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
unit opts386;
interface

uses
  options;

type
  poption386=^toption386;
  toption386=object(toption)
    procedure interpret_proc_specific_options(const opt:string);virtual;
  end;

implementation

uses
  systems,globals;

procedure toption386.interpret_proc_specific_options(const opt:string);
var
  j     : longint;
  More  : string;
begin
  More:=Upper(copy(opt,3,length(opt)-2));
  case opt[2] of
   'O' : Begin
           j := 3;
           While (j <= Length(Opt)) Do
             Begin
               case opt[j] of
                 '-' : initglobalswitches:=initglobalswitches-[cs_optimize,cs_fastoptimize,cs_slowoptimize,cs_littlesize,
                           cs_regalloc,cs_uncertainopts];
                 'g' : initglobalswitches:=initglobalswitches+[cs_littlesize];
                 'G' : initglobalswitches:=initglobalswitches-[cs_littlesize];
                 'r' : initglobalswitches:=initglobalswitches+[cs_optimize,cs_regalloc];
                 'u' : initglobalswitches:=initglobalswitches+[cs_optimize,cs_uncertainopts];
                 '1' : initglobalswitches:=initglobalswitches+[cs_optimize,cs_fastoptimize];
                 '2' : initglobalswitches:=initglobalswitches+[cs_optimize,cs_fastoptimize,cs_slowoptimize];
                 '3' : initglobalswitches:=initglobalswitches+[cs_optimize,cs_fastoptimize,cs_slowoptimize,cs_uncertainopts];
                 'p' :
                   Begin
                     Case opt[j+1] Of
                       '1': initoptprocessor := Class386;
                       '2': initoptprocessor := ClassP5;
                       '3': initoptprocessor := ClassP6
                       Else IllegalPara(Opt)
                     End;
                     Inc(j);
                   End
                 else IllegalPara(opt);
               End;
               Inc(j)
             end;
         end;
   'R' : begin
           if More='ATT' then
            initasmmode:=I386_ATT
           else
            if More='INTEL' then
             initasmmode:=I386_INTEL
           else
            if More='DIRECT' then
             initasmmode:=I386_DIRECT
           else
            IllegalPara(opt);
         end;
  else
   IllegalPara(opt);
  end;
end;

end.
{
  $Log$
  Revision 1.11  1998-09-25 09:57:08  peter
    * moved -A to options.pas, becuase the code is the same

}
