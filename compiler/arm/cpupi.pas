{
    $Id$
    Copyright (c) 2002 by Florian Klaempfl

    This unit contains the CPU specific part of tprocinfo

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

{ This unit contains the CPU specific part of tprocinfo. }
unit cpupi;

{$i fpcdefs.inc}

  interface

    uses
       globtype,cutils,
       procinfo,cpuinfo,psub;

    type
       tarmprocinfo = class(tcgprocinfo)
          floatregstart : aword;
          // procedure handle_body_start;override;
          // procedure after_pass1;override;
          procedure set_first_temp_offset;override;
          procedure allocate_push_parasize(size: longint);override;
          function calc_stackframe_size:longint;override;
       end;


  implementation

    uses
       globals,systems,
       cpubase,
       aasmtai,
       tgobj,
       symconst,symsym,paramgr,
       cgbase,
       cgobj;

    procedure tarmprocinfo.set_first_temp_offset;
      begin
        { We allocate enough space to save all registers because we can't determine
          the necessary space because the used registers aren't known before
          secondpass is run. Even worse, patching
          the local offsets after generating the code could cause trouble because
          "shifter" constants could change to non-"shifter" constants. This
          is especially a problem when taking the address of a local. For now,
          this extra memory should hurt less than generating all local contants with offsets
          >256 as non shifter constants }
        tg.setfirsttemp(-12-28);
      end;


    procedure tarmprocinfo.allocate_push_parasize(size:longint);
      begin
        if size>maxpushedparasize then
          maxpushedparasize:=size;
      end;


    function tarmprocinfo.calc_stackframe_size:longint;
      var
         firstfloatreg,lastfloatreg,
         r : byte;
         floatsavesize : aword;
      begin
        maxpushedparasize:=align(maxpushedparasize,max(aktalignment.localalignmin,4));
        firstfloatreg:=RS_NO;
        { save floating point registers? }
        for r:=RS_F0 to RS_F7 do
          if r in cg.rg[R_FPUREGISTER].used_in_proc-paramanager.get_volatile_registers_fpu(pocall_stdcall) then
            begin
              if firstfloatreg=RS_NO then
                firstfloatreg:=r;
              lastfloatreg:=r;
            end;
        if firstfloatreg<>RS_NO then
          floatsavesize:=(lastfloatreg-firstfloatreg+1)*12
        else
          floatsavesize:=0;
        floatsavesize:=align(floatsavesize,max(aktalignment.localalignmin,4));
        result:=Align(tg.direction*tg.lasttemp,max(aktalignment.localalignmin,4))+maxpushedparasize+floatsavesize;
        floatregstart:=-result+maxpushedparasize;
      end;


begin
   cprocinfo:=tarmprocinfo;
end.
{
  $Log$
  Revision 1.8  2004-06-16 20:07:10  florian
    * dwarf branch merged

  Revision 1.7.2.2  2004/06/12 17:01:01  florian
    * fixed compilation of arm compiler

  Revision 1.7.2.1  2004/04/23 22:12:37  florian
    * fixed some potential stack corruption reasons

  Revision 1.7  2004/03/29 19:19:35  florian
    + arm floating point register saving implemented
    * hopefully stabs generation for MacOSX fixed
    + some defines for arm added

  Revision 1.6  2004/03/06 20:35:20  florian
    * fixed arm compilation
    * cleaned up code generation for exported linux procedures

  Revision 1.5  2003/12/03 17:39:05  florian
    * fixed several arm calling conventions issues
    * fixed reference reading in the assembler reader
    * fixed a_loadaddr_ref_reg

  Revision 1.4  2003/11/30 19:35:29  florian
    * fixed several arm related problems

  Revision 1.3  2003/11/24 15:17:37  florian
    * changed some types to prevend range check errors

  Revision 1.2  2003/11/02 14:30:03  florian
    * fixed ARM for new reg. allocation scheme

  Revision 1.1  2003/08/20 15:50:13  florian
    * more arm stuff
}

