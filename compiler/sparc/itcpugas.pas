{
    $Id$
    Copyright (c) 1998-2002 by Mazen NEIFER

    This unit contains the PowerPC GAS instruction tables

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
unit itcpugas;

{$i fpcdefs.inc}

interface

    uses
      cpubase,cgbase;

    const
      gas_op2str : array[tasmop] of string[14] = ({$INCLUDE strinst.inc});

    function gas_regnum_search(const s:string):Tregister;
    function gas_regname(r:Tregister):string;


implementation

    uses
      cutils,verbose;

    const
      gas_regname_table : array[tregisterindex] of string[7] = (
        {$i rspstd.inc}
      );

      gas_regname_index : array[tregisterindex] of tregisterindex = (
        {$i rspsri.inc}
      );


    function findreg_by_gasname(const s:string):tregisterindex;
      var
        i,p : tregisterindex;
      begin
        {Binary search.}
        p:=0;
        i:=regnumber_count_bsstart;
        repeat
          if (p+i<=high(tregisterindex)) and (gas_regname_table[gas_regname_index[p+i]]<=s) then
            p:=p+i;
          i:=i shr 1;
        until i=0;
        if gas_regname_table[gas_regname_index[p]]=s then
          findreg_by_gasname:=gas_regname_index[p]
        else
          findreg_by_gasname:=0;
      end;


    function gas_regnum_search(const s:string):Tregister;
      begin
        result:=regnumber_table[findreg_by_gasname(s)];
      end;


    function gas_regname(r:Tregister):string;
      var
        p : longint;
      begin
        p:=findreg_by_number(r);
        if p<>0 then
          result:=gas_regname_table[p]
        else
          result:=generic_regname(r);
      end;

end.
{
  $Log$
  Revision 1.1  2003-12-08 13:03:06  mazen
  + support for native sparc assembler reader

  Revision 1.2  2003/11/15 19:00:10  florian
    * fixed ppc assembler reader

  Revision 1.1  2003/11/12 16:05:40  florian
    * assembler readers OOPed
    + typed currency constants
    + typed 128 bit float constants if the CPU supports it

  Revision 1.2  2003/10/01 20:34:49  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.1  2003/09/03 19:35:24  peter
    * powerpc compiles again

}
