{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004-2011 by Armin Diehl

    FPC Pascal system unit for the netware API.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{*****************************************************************************
                           Directory Handling
*****************************************************************************}

Procedure do_MkDir(s: rawbytestring);
var
    Rc : longint;
begin
  DoDirSeparators(s);
  Rc := _mkdir(PAnsiChar(s));
  if Rc <> 0 then
    SetFileError (Rc);
end;


procedure do_RmDir(s: rawbytestring);
var Rc : longint;
begin
  if s = '.' then
    begin
      InOutRes := 16;
      exit;
    end;
  DoDirSeparators(s);
  Rc := _rmdir(PAnsiChar(s));
  if Rc <> 0 then
    SetFileError(Rc);
end;


procedure do_ChDir(s: rawbytestring);
var RC: longint;
begin
  DoDirSeparators(s);
  RC := _chdir (PAnsiChar(s));
  if Rc <> 0 then
    SetFileError(Rc);
end;


procedure do_getdir(drivenr : byte;var dir : rawbytestring);
VAR P : ARRAY [0..255] OF AnsiChar;
    i : LONGINT;
begin
  P[0] := #0;
  _getcwd (@P, SIZEOF (P));
  i := _strlen (P);
  if i > 0 then
  begin
    SetLength (dir, i);
    Move (P, dir[1], i);
    DoDirSeparators(dir);
    // fix / after volume, the compiler needs that
    // normaly root of a volumes is SERVERNAME/SYS:, change that
    // to SERVERNAME/SYS:/
    i := pos (':',dir);
    if (i > 0) then
      if i = Length (dir) then dir := dir + '/' else
      if dir [i+1] <> '/' then insert ('/',dir,i+1);
    SetCodePage (dir,DefaultFileSystemCodePage,false);
  END ELSE
    InOutRes := 1;
end;

