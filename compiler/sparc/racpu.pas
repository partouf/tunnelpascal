{
    $Id$
    Copyright (c) 1998-2003 by Mazen NEIFER

    Handles the common Sparc assembler reader routines

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
unit racpu;

{$i fpcdefs.inc}

interface

  uses
    aasmbase,aasmtai,aasmcpu,
    cpubase,rautils,cclasses;

  type
    TSparcOperand=class(TOperand)
    end;

    TSparcInstruction=class(TInstruction)
      delayslot_annulled : boolean;
      { opcode adding }
      function ConcatInstruction(p : taasmoutput) : tai;override;
    end;

implementation

    function TSparcInstruction.ConcatInstruction(p : taasmoutput) : tai;
      begin
        result:=inherited ConcatInstruction(p);
        { delay slot annulled support }
        if (result.typ=ait_instruction) and
           delayslot_annulled then
          taicpu(result).delayslot_annulled:=true;
      end;

end.
{
  $Log$
  Revision 1.2  2004-06-16 20:07:11  florian
    * dwarf branch merged

  Revision 1.1.2.1  2004/05/25 21:38:53  peter
    * assembler reader/writer updates

  Revision 1.1  2003/12/08 13:02:21  mazen
  + support for native sparc assembler reader

}
