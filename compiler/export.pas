{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    This unit implements an uniform export object

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
unit export;

{$i defines.inc}

interface

uses
  cutils,cclasses,
  symtype;

const
   { export options }
   eo_resident = $1;
   eo_index    = $2;
   eo_name     = $4;

type
   texported_item = class(tlinkedlistitem)
      sym : psym;
      index : longint;
      name : pstring;
      options : word;
      is_var : boolean;
      constructor create;
      destructor destroy;override;
   end;

   texportlib=class
   private
      notsupmsg : boolean;
      procedure NotSupported;
   public
      constructor Create;
      destructor Destroy;override;
      procedure preparelib(const s : string);virtual;
      procedure exportprocedure(hp : texported_item);virtual;
      procedure exportvar(hp : texported_item);virtual;
      procedure generatelib;virtual;
   end;

var
   exportlib : texportlib;

procedure InitExport;
procedure DoneExport;

implementation

uses
  systems,verbose,globals
{$ifdef i386}
  {$ifndef NOTARGETLINUX}
    ,t_linux
  {$endif}
  {$ifndef NOTARGETFREEBSD}
    ,t_fbsd
  {$endif}
  {$ifndef NOTARGETOS2}
    ,t_os2
  {$endif}
  {$ifndef NOTARGETSUNOS}
    ,t_sunos
  {$endif}
  {$ifndef NOTARGETWIN32}
    ,t_win32
  {$endif}
  {$ifndef NOTARGETNETWARE}
    ,t_nwm
  {$endif}
  {$ifndef NOTARGETGO32V2}
    ,t_go32v2
  {$endif}
{$endif}
{$ifdef m68k}
  {$ifndef NOTARGETLINUX}
    ,t_linux
  {$endif}
{$endif}
{$ifdef powerpc}
  {$ifndef NOTARGETLINUX}
    ,t_linux
  {$endif}
{$endif}
{$ifdef alpha}
  {$ifndef NOTARGETLINUX}
    ,t_linux
  {$endif}
{$endif}
  ;

{****************************************************************************
                           TImported_procedure
****************************************************************************}

constructor texported_item.Create;
begin
  inherited Create;
  sym:=nil;
  index:=-1;
  name:=nil;
  options:=0;
  is_var:=false;
end;


destructor texported_item.destroy;
begin
  stringdispose(name);
  inherited destroy;
end;


{****************************************************************************
                              TImportLib
****************************************************************************}

constructor texportlib.Create;
begin
  notsupmsg:=false;
end;


destructor texportlib.Destroy;
begin
end;


procedure texportlib.NotSupported;
begin
  { show the message only once }
  if not notsupmsg then
   begin
     Message(exec_e_dll_not_supported);
     notsupmsg:=true;
   end;
end;


procedure texportlib.preparelib(const s:string);
begin
  NotSupported;
end;


procedure texportlib.exportprocedure(hp : texported_item);
begin
  NotSupported;
end;


procedure texportlib.exportvar(hp : texported_item);
begin
  NotSupported;
end;


procedure texportlib.generatelib;
begin
  NotSupported;
end;


procedure DoneExport;
begin
  if assigned(exportlib) then
    exportlib.free;
end;


procedure InitExport;
begin
  case target_info.target of
{$ifdef i386}
  {$ifndef NOTARGETLINUX}
    target_i386_Linux :
      exportlib:=Texportliblinux.Create;
  {$endif NOTARGETLINUX}
  {$ifndef NOTARGETFREEBSD}
    target_i386_freebsd:
      exportlib:=Texportlibfreebsd.Create;
  {$endif NOTARGETFREEBSD}
  {$ifndef NOTARGETSUNOS}
    target_i386_SUNOS:
      exportlib:=Texportlibsunos.Create;
  {$endif NOTARGETSUNOS}
  {$ifndef NOTARGETWIN32}
    target_i386_Win32 :
      exportlib:=Texportlibwin32.Create;
  {$endif NOTARGETWIN32}
  {$ifndef NOTARGETNETWARE}
    target_i386_netware :
      exportlib:=Texportlibnetware.Create;
  {$endif NOTARGETNETWARE}
{
    target_i386_OS2 :
      exportlib:=Texportlibos2.Create;
}
{$endif i386}
{$ifdef m68k}
    target_m68k_Linux :
      exportlib:=Texportliblinux.Create;
{$endif m68k}
{$ifdef alpha}
    target_alpha_Linux :
      exportlib:=Texportliblinux.Create;
{$endif alpha}
{$ifdef powerpc}
    target_alpha_Linux :
      exportlib:=Texportliblinux.Create;
{$endif powerpc}
    else
      exportlib:=Texportlib.Create;
  end;
end;


end.
{
  $Log$
  Revision 1.12  2001-02-26 19:44:52  peter
    * merged generic m68k updates from fixes branch

  Revision 1.11  2001/02/03 00:09:02  peter
    * fixed netware typo in previous commit

  Revision 1.10  2001/02/02 22:43:39  peter
    * add notarget defines

  Revision 1.9  2000/12/25 00:07:25  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.8  2000/11/29 00:30:30  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.7  2000/10/31 22:02:46  peter
    * symtable splitted, no real code changes

  Revision 1.6  2000/09/24 15:06:16  peter
    * use defines.inc

  Revision 1.5  2000/09/16 12:22:52  peter
    * freebsd support merged

  Revision 1.4  2000/09/11 17:00:22  florian
    + first implementation of Netware Module support, thanks to
      Armin Diehl (diehl@nordrhein.de) for providing the patches

  Revision 1.3  2000/08/27 16:11:50  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.2  2000/07/13 11:32:41  michael
  + removed logs

}
