{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit contains the i386 AT&T instruction tables

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
unit itx86int;

{$i fpcdefs.inc}

interface

    uses
      cgbase;

    function masm_regnum_search(const s:string):Tregister;
    function masm_regname(r:Tregister):string;


implementation

    uses
      cutils,verbose,
      cpubase;

    const
      int_regname_table : array[tregisterindex] of string[7] = (
        {$i r386int.inc}
      );

      int_regname_index : array[tregisterindex] of tregisterindex = (
        {$i r386iri.inc}
      );


    function findreg_by_intname(const s:string):byte;
      var
        i,p : tregisterindex;
      begin
        {Binary search.}
        p:=0;
        i:=regnumber_count_bsstart;
        repeat
          if (p+i<=high(tregisterindex)) and (int_regname_table[int_regname_index[p+i]]<=s) then
            p:=p+i;
          i:=i shr 1;
        until i=0;
        if int_regname_table[int_regname_index[p]]=s then
          findreg_by_intname:=int_regname_index[p]
        else
          findreg_by_intname:=0;
      end;


    function masm_regnum_search(const s:string):Tregister;
      begin
        result:=regnumber_table[findreg_by_intname(s)];
      end;


    function masm_regname(r:Tregister):string;
      var
        p : tregisterindex;
      begin
        p:=findreg_by_number(r);
        if p<>0 then
          result:=int_regname_table[p]
        else
          result:=generic_regname(r);
      end;

end.
{
  $Log$
  Revision 1.4  2003-10-01 20:34:51  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.3  2003/09/04 14:42:44  peter
    * return 0 instead of $ff when no reg is found

  Revision 1.2  2003/09/03 15:55:02  peter
    * NEWRA branch merged

  Revision 1.1.2.5  2003/08/31 15:46:26  peter
    * more updates for tregister

  Revision 1.1.2.4  2003/08/31 13:50:16  daniel
    * Remove sorting and use pregenerated indexes
    * Some work on making things compile

  Revision 1.1.2.3  2003/08/28 18:35:08  peter
    * tregister changed to cardinal

  Revision 1.1.2.2  2003/08/27 21:06:34  peter
    * more updates

  Revision 1.1.2.1  2003/08/27 19:55:54  peter
    * first tregister patch

}
