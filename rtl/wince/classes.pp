{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1998 by Michael Van Canneyt and Florian Klaempfl

    Classes unit for wince

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
{$H+}
{$IF FPC_FULLVERSION>=30301}
{$modeswitch FUNCTIONREFERENCES}
{$define FPC_HAS_REFERENCE_PROCEDURE}
{$endif}


{ determine the type of the resource/form file }
{$define Win16Res}

unit Classes;

interface

uses
  rtlconsts,
  sysutils,
  types,
  sortbase,
{$ifdef FPC_TESTGENERICS}
  fgl,
{$endif}
  typinfo,
  windows;

type
  TWndMethod = procedure(var msg : TMessage) of object;

{$i classesh.inc}

implementation

uses
  sysconst;

{ OS - independent class implementations are in /inc directory. }
{$i classes.inc}

initialization
  CommonInit;

finalization
  CommonCleanup;
end.
