{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by Michael Van Canneyt and Florian Klaempfl

    Classes unit for win32

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}

{ determine the type of the resource/form file }
{$define Win16Res}

unit Classes;

interface

uses
  typinfo,
  rtlconst,
  types,
  sysutils;

{$i classesh.inc}

implementation

{ OS - independent class implementations are in /inc directory. }
{$i classes.inc}

initialization
  CommonInit;

finalization
  CommonCleanup;

end.
{
  $Log$
  Revision 1.3  2004-01-22 17:11:23  peter
    * classes uses types to import TPoint and TRect

  Revision 1.2  2004/01/10 20:13:19  michael
  + Some more fixes to rtlconst. Const strings moved from classes to rtlconst

  Revision 1.1  2003/10/06 21:01:06  peter
    * moved classes unit to rtl

  Revision 1.1  2003/10/06 20:33:58  peter
    * classes moved to rtl for 1.1
    * classes .inc and classes.pp files moved to fcl/classes for
      backwards 1.0.x compatiblity to have it in the fcl

  Revision 1.3  2002/09/07 15:15:24  peter
    * old logs removed and tabs fixed

}
