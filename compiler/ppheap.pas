{
    $Id$
    Copyright (c) 1998-2000 by Pierre Muller

    Simple unit to add source line and column to each
    memory allocation made with heaptrc unit

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

 ****************************************************************************}
unit ppheap;

  interface

    uses heaptrc;

    { call this function before any memory allocation
      in a unit initialization code (PM) }

    procedure pp_heap_init;

  implementation

    uses
       globtype,globals,files;

    procedure ppextra_info(p : pointer);
      var pl : plongint;
      begin
         longint(p^):=aktfilepos.line;
         pl:=plongint(cardinal(p)+4);
         pl^:=aktfilepos.column;
         pl:=plongint(cardinal(p)+8);
         if assigned(current_module) then
          pl^:=current_module^.unit_index*100000+aktfilepos.fileindex
         else
          pl^:=aktfilepos.fileindex
      end;

  const
     pp_heap_inited : boolean = false;

  procedure pp_heap_init;
    begin
       if not pp_heap_inited then
         begin
            setheaptraceoutput('heap.log');
{$ifndef TP}
            SetExtraInfo(12,@ppextra_info);
{$else TP}
            SetExtraInfo(12,ppextra_info);
{$endif TP}
         end;
       pp_heap_inited:=true;
    end;

  begin
     pp_heap_init;
  end.

{
  $Log$
  Revision 1.9  2000-01-07 01:14:30  peter
    * updated copyright to 2000

  Revision 1.8  1999/11/17 17:05:02  pierre
   * Notes/hints changes

  Revision 1.7  1999/06/08 15:26:49  pierre
   * fix to get it self compiled

  Revision 1.6  1999/05/17 15:09:28  pierre
   * heaptrc output to heap.log file

  Revision 1.5  1999/01/26 11:32:13  pierre
   * ppheap init code can be called before any getmem

}

