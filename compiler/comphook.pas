{
    $Id$
    Copyright (c) 1998-2002 by Peter Vreman

    This unit handles the compilerhooks for output to external programs

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
unit comphook;

{$i fpcdefs.inc}

interface

uses
  finput;

Const
  { Levels }
  V_None         = $0;
  V_Fatal        = $1;
  V_Error        = $2;
  V_Normal       = $4; { doesn't show a text like Error: }
  V_Warning      = $8;
  V_Note         = $10;
  V_Hint         = $20;
  V_LineInfoMask = $fff;
  { From here by default no line info }
  V_Info         = $1000;
  V_Status       = $2000;
  V_Used         = $4000;
  V_Tried        = $8000;
  V_Conditional  = $10000;
  V_Debug        = $20000;
  V_Executable   = $40000;
  V_LevelMask    = $fffffff;
  V_All          = V_LevelMask;
  V_Default      = V_Fatal + V_Error + V_Normal;
  { Flags }
  V_LineInfo     = $10000000;

const
  { RHIDE expect gcc like error output }
  fatalstr      : string[20] = 'Fatal:';
  errorstr      : string[20] = 'Error:';
  warningstr    : string[20] = 'Warning:';
  notestr       : string[20] = 'Note:';
  hintstr       : string[20] = 'Hint:';

type
  PCompilerStatus = ^TCompilerStatus;
  TCompilerStatus = record
  { Current status }
    currentmodule,
    currentsourcepath,
    currentsource : string;   { filename }
    currentline,
    currentcolumn : longint;  { current line and column }
  { Total Status }
    compiledlines : longint;  { the number of lines which are compiled }
    errorcount    : longint;  { number of generated errors }
  { program info }
    isexe,
    islibrary     : boolean;
  { Settings for the output }
    verbosity     : longint;
    maxerrorcount : longint;
    errorwarning,
    errornote,
    errorhint,
    skip_error,
    use_stderr,
    use_redir,
    use_bugreport,
    use_gccoutput,
    compiling_current : boolean;
  { Redirection support }
    redirfile : text;
  { Special file for bug report }
    reportbugfile : text;
  end;
var
  status : tcompilerstatus;

{ Default Functions }
procedure def_stop(err:longint);
Function  def_status:boolean;
Function  def_comment(Level:Longint;const s:string):boolean;
function  def_internalerror(i:longint):boolean;
procedure def_initsymbolinfo;
procedure def_donesymbolinfo;
procedure def_extractsymbolinfo;
function  def_openinputfile(const filename: string): tinputfile;
Function  def_getnamedfiletime(Const F : String) : Longint;
{$ifdef DEBUG}
{ allow easy stopping in GDB
  using
  b DEF_GDB_STOP
  cond 1 LEVEL <= 8 }
procedure def_gdb_stop(level : longint);
{$endif DEBUG}
{ Function redirecting for IDE support }
type
  tstopprocedure         = procedure(err:longint);
  tstatusfunction        = function:boolean;
  tcommentfunction       = function(Level:Longint;const s:string):boolean;
  tinternalerrorfunction = function(i:longint):boolean;

  tinitsymbolinfoproc = procedure;
  tdonesymbolinfoproc = procedure;
  textractsymbolinfoproc = procedure;
  topeninputfilefunc = function(const filename: string): tinputfile;
  tgetnamedfiletimefunc = function(const filename: string): longint;

const
  do_stop          : tstopprocedure   = {$ifdef FPCPROCVAR}@{$endif}def_stop;
  do_status        : tstatusfunction  = {$ifdef FPCPROCVAR}@{$endif}def_status;
  do_comment       : tcommentfunction = {$ifdef FPCPROCVAR}@{$endif}def_comment;
  do_internalerror : tinternalerrorfunction = {$ifdef FPCPROCVAR}@{$endif}def_internalerror;

  do_initsymbolinfo : tinitsymbolinfoproc = {$ifdef FPCPROCVAR}@{$endif}def_initsymbolinfo;
  do_donesymbolinfo : tdonesymbolinfoproc = {$ifdef FPCPROCVAR}@{$endif}def_donesymbolinfo;
  do_extractsymbolinfo : textractsymbolinfoproc = {$ifdef FPCPROCVAR}@{$endif}def_extractsymbolinfo;

  do_openinputfile : topeninputfilefunc = {$ifdef FPCPROCVAR}@{$endif}def_openinputfile;
  do_getnamedfiletime : tgetnamedfiletimefunc = {$ifdef FPCPROCVAR}@{$endif}def_getnamedfiletime;

implementation

  uses
{$IFDEF USE_SYSUTILS}
    SysUtils,
{$ELSE USE_SYSUTILS}
   dos,
{$ENDIF USE_SYSUTILS}
   cutils
   ;

{****************************************************************************
                          Helper Routines
****************************************************************************}

function gccfilename(const s : string) : string;
var
  i : longint;
begin
  for i:=1to length(s) do
   begin
     case s[i] of
      '\' : gccfilename[i]:='/';
 'A'..'Z' : gccfilename[i]:=chr(ord(s[i])+32);
     else
      gccfilename[i]:=s[i];
     end;
   end;
  gccfilename[0]:=s[0];
end;


function tostr(i : longint) : string;
var
  hs : string;
begin
  str(i,hs);
  tostr:=hs;
end;


{****************************************************************************
                         Predefined default Handlers
****************************************************************************}

{ predefined handler when then compiler stops }
procedure def_stop(err:longint);
begin
  Halt(err);
end;

{$ifdef DEBUG}
{ allow easy stopping in GDB
  using
  b DEF_GDB_STOP
  cond 1 LEVEL <= 8 }
procedure def_gdb_stop(level : longint);
begin
  { Its only a dummy for GDB }
end;
{$endif DEBUG}


function def_status:boolean;
begin
  def_status:=false; { never stop }
{ Status info?, Called every line }
  if ((status.verbosity and V_Status)<>0) then
   begin
{$ifndef Delphi}
     if (status.compiledlines=1) then
       WriteLn(memavail shr 10,' Kb Free');
{$endif Delphi}
     if (status.currentline>0) and (status.currentline mod 100=0) then
{$ifdef FPC}
       WriteLn(status.currentline,' ',DStr(memavail shr 10),'/',DStr(system.heapsize shr 10),' Kb Free');
{$else}
  {$ifndef Delphi}
       WriteLn(status.currentline,' ',DStr(memavail shr 10),' Kb Free');
  {$endif Delphi}
{$endif}
   end
end;


Function def_comment(Level:Longint;const s:string):boolean;
const
  rh_errorstr   = 'error:';
  rh_warningstr = 'warning:';
var
  hs : string;
begin
  def_comment:=false; { never stop }
  hs:='';
  if not(status.use_gccoutput) then
    begin
      if (status.verbosity and Level)=V_Hint then
        hs:=hintstr;
      if (status.verbosity and Level)=V_Note then
        hs:=notestr;
      if (status.verbosity and Level)=V_Warning then
        hs:=warningstr;
      if (status.verbosity and Level)=V_Error then
        hs:=errorstr;
      if (status.verbosity and Level)=V_Fatal then
        hs:=fatalstr;
      if (status.verbosity and Level)=V_Used then
        hs:=PadSpace('('+status.currentmodule+')',10);
    end
  else
    begin
      if (status.verbosity and Level)=V_Hint then
        hs:=rh_warningstr;
      if (status.verbosity and Level)=V_Note then
        hs:=rh_warningstr;
      if (status.verbosity and Level)=V_Warning then
        hs:=rh_warningstr;
      if (status.verbosity and Level)=V_Error then
        hs:=rh_errorstr;
      if (status.verbosity and Level)=V_Fatal then
        hs:=rh_errorstr;
    end;
  { Generate line prefix }
  if ((Level and V_LineInfo)=V_LineInfo) and
     (status.currentsource<>'') and
     (status.currentline>0) then
   begin
     { Adding the column should not confuse RHIDE,
     even if it does not yet use it PM
     but only if it is after error or warning !! PM }
     if status.currentcolumn>0 then
      begin
        if status.use_gccoutput then
          hs:=gccfilename(status.currentsource)+':'+tostr(status.currentline)+': '+hs+' '+
              tostr(status.currentcolumn)+': '+s
        else
          hs:=status.currentsource+'('+tostr(status.currentline)+
              ','+tostr(status.currentcolumn)+') '+hs+' '+s;
      end
     else
      begin
        if status.use_gccoutput then
          hs:=gccfilename(status.currentsource)+': '+hs+' '+tostr(status.currentline)+': '+s
        else
          hs:=status.currentsource+'('+tostr(status.currentline)+') '+hs+' '+s;
      end;
   end
  else
   begin
     if hs<>'' then
      hs:=hs+' '+s
     else
      hs:=s;
   end;

  { Display line }
  if ((status.verbosity and (Level and V_LevelMask))=(Level and V_LevelMask)) then
   begin
{$ifdef FPC}
     if status.use_stderr then
      begin
        writeln(stderr,hs);
        flush(stderr);
      end
     else
{$endif}
      begin
        if status.use_redir then
         writeln(status.redirfile,hs)
        else
         writeln(hs);
      end;
   end;
  { include everything in the bugreport file }
  if status.use_bugreport then
   begin
{$ifdef FPC}
     Write(status.reportbugfile,hexstr(level,8)+':');
     Writeln(status.reportbugfile,hs);
{$endif}
   end;

{$ifdef DEBUG}
  def_gdb_stop(level);
{$endif DEBUG}
end;


function def_internalerror(i : longint) : boolean;
begin
  do_comment(V_Fatal+V_LineInfo,'Internal error '+tostr(i));
{$ifdef EXTDEBUG}
  {$ifdef FPC}
    { Internalerror() and def_internalerror() do not
      have a stackframe }
    dump_stack(stdout,get_caller_frame(get_frame));
  {$endif FPC}
{$endif EXTDEBUG}
  def_internalerror:=true;
end;

procedure def_initsymbolinfo;
begin
end;

procedure def_donesymbolinfo;
begin
end;

procedure def_extractsymbolinfo;
begin
end;

function  def_openinputfile(const filename: string): tinputfile;
begin
  def_openinputfile:=tdosinputfile.create(filename);
end;


Function def_GetNamedFileTime (Const F : String) : Longint;
var
  L : Longint;
{$IFDEF USE_SYSUTILS}
  info : TSearchRec;
{$ELSE USE_SYSUTILS}
  info : SearchRec;
{$ENDIF USE_SYSUTILS}
begin
  l:=-1;
{$IFDEF USE_SYSUTILS}
  if FindFirst (F,faArchive+faReadOnly+faHidden,info) = 0
  then
{$ELSE USE_SYSUTILS}
    FindFirst (F,archive+readonly+hidden,info);
  if DosError=0 then
{$ENDIF USE_SYSUTILS}
   l:=info.time;
  FindClose(info);
  def_GetNamedFileTime:=l;
end;

end.
{
  $Log$
  Revision 1.30  2004-10-14 18:16:17  mazen
  * USE_SYSUTILS merged successfully : cycles with and without defines
  * Need to be optimized in performance

  Revision 1.29  2004/10/14 17:10:15  mazen
  * use SysUtils unit instead of Dos Unit
  + overload Replace to use AnsiString

  Revision 1.28  2004/09/08 11:23:30  michael
  + Check if outputdir exists,  Fix exitcode when displaying help pages

  Revision 1.27  2004/06/20 08:55:29  florian
    * logs truncated

}
