{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Solaris system unit

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit System;

interface

{$define FPC_IS_SYSTEM}

{ include system-independent routine headers }

{$I sysunixh.inc}

implementation

{ OS independant parts}

{$I system.inc}

{*****************************************************************************
                       Misc. System Dependent Functions
*****************************************************************************}

procedure System_exit;
begin
   Fpexit(cint(ExitCode));
End;


Function ParamCount: Longint;
Begin
  Paramcount:=argc-1
End;


function BackPos(c:char; const s: shortstring): integer;
var
 i: integer;
Begin
  for i:=length(s) downto 0 do
    if s[i] = c then break;
  if i=0 then
    BackPos := 0
  else
    BackPos := i;
end;


 { variable where full path and filename and executable is stored }
 { is setup by the startup of the system unit.                    }
var
 execpathstr : shortstring;

function paramstr(l: longint) : string;
 var
  s: string;
  s1: string;
 begin
   { stricly conforming POSIX applications  }
   { have the executing filename as argv[0] }
//   if l=0 then
//     begin
//       paramstr := execpathstr;
//     end
//   else
     paramstr:=strpas(argv[l]);
 end;

Procedure Randomize;
Begin
  randseed:=longint(Fptime(nil));
End;


{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}

function  reenable_signal(sig : longint) : boolean;
var
  e,oe : TSigSet;
  i,j : byte;
begin
  fillchar(e,sizeof(e),#0);
  fillchar(oe,sizeof(oe),#0);
  { set is 1 based PM }
  dec(sig);
  i:=sig mod 32;
  j:=sig div 32;
  e[j]:=1 shl i;
  fpsigprocmask(SIG_UNBLOCK,@e,@oe);
  reenable_signal:=geterrno=0;
end;

{$i sighnd.inc}

var
  act: SigActionRec;

Procedure InstallSignals;
var
  oldact: SigActionRec;
begin
  { Initialize the sigaction structure }
  { all flags and information set to zero }
  FillChar(act, sizeof(SigActionRec),0);
  { initialize handler                    }
  act.sa_handler :=@SignalToRunError;
{$warning TODO SIGINFO}  
  act.sa_flags:=SA_SIGINFO;
  FpSigAction(SIGFPE,act,oldact);
  FpSigAction(SIGSEGV,act,oldact);
  FpSigAction(SIGBUS,act,oldact);
  FpSigAction(SIGILL,act,oldact);
end;


procedure SetupCmdLine;
var
  bufsize,
  len,j,
  size,i : longint;
  found  : boolean;
  buf    : pchar;

  procedure AddBuf;
  begin
    reallocmem(cmdline,size+bufsize);
    move(buf^,cmdline[size],bufsize);
    inc(size,bufsize);
    bufsize:=0;
  end;

begin
  GetMem(buf,ARG_MAX);
  size:=0;
  bufsize:=0;
  i:=0;
  while (i<argc) do
   begin
     len:=strlen(argv[i]);
     if len>ARG_MAX-2 then
      len:=ARG_MAX-2;
     found:=false;
     for j:=1 to len do
      if argv[i][j]=' ' then
       begin
         found:=true;
         break;
       end;
     if bufsize+len>=ARG_MAX-2 then
      AddBuf;
     if found then
      begin
        buf[bufsize]:='"';
        inc(bufsize);
      end;
     move(argv[i]^,buf[bufsize],len);
     inc(bufsize,len);
     if found then
      begin
        buf[bufsize]:='"';
        inc(bufsize);
      end;
     if i<argc then
      buf[bufsize]:=' '
     else
      buf[bufsize]:=#0;
     inc(bufsize);
     inc(i);
   end;
  AddBuf;
  FreeMem(buf,ARG_MAX);
end;


procedure SysInitStdIO;
begin
  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(ErrOutput,fmOutput,StdErrorHandle);
  OpenStdIO(StdOut,fmOutput,StdOutputHandle);
  OpenStdIO(StdErr,fmOutput,StdErrorHandle);
end;


function GetProcessID: SizeUInt;
begin
 GetProcessID := SizeUInt (fpGetPID);
end;


procedure pascalmain; external name 'PASCALMAIN';

{ Main entry point in C style, needed to capture program parameters. }
procedure main(argcparam: Longint; argvparam: ppchar; envpparam: ppchar); cdecl; [public];

begin
  argc:= argcparam;
  argv:= argvparam;
  envp:= envpparam;
  pascalmain;  {run the pascal main program}
end;


Begin
  IsConsole := TRUE;
  IsLibrary := FALSE;
  StackLength := InitialStkLen;
  StackBottom := Sptr - StackLength;
{ Set up signals handlers }
  InstallSignals;
{ Setup heap }
  InitHeap;
  SysInitExceptions;
{ Arguments }
  SetupCmdLine;
{ Setup stdin, stdout and stderr }
  SysInitStdIO;
{ Reset IO Error }
  InOutRes:=0;
  InitSystemThreads;
{$ifdef HASVARIANT}
  initvariantmanager;
{$endif HASVARIANT}
{$ifdef HASWIDESTRING}
  initwidestringmanager;
{$endif HASWIDESTRING}
End.

{
 $Log$
 Revision 1.4  2005-02-13 22:13:20  peter
   * get solaris back in shape

 Revision 1.3  2005/02/13 21:47:56  peter
   * include file cleanup part 2

 Revision 1.2  2005/02/10 17:30:54  peter
   * renamed to solaris

 Revision 1.5  2005/02/07 22:17:26  peter
   * updated for 1.9.x unix rtl

 Revision 1.4  2005/02/01 20:22:50  florian
   * improved widestring infrastructure manager

 Revision 1.3  2004/12/05 14:36:38  hajny
   + GetProcessID added

 Revision 1.2  2004/11/06 22:22:28  florian
   * some sunos stuff from 1.0.x merged
}
