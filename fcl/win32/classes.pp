{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1998 by Michael Van Canneyt and Florian Klaempfl

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ exceptions aren't implemented yet in the compiler }
{$define NoExceptions}

{ determine the type of the resource/form file }
{$define Win16Res}
unit Classes;

interface

uses
  dos,strings,sysutils;

{$i classesh.inc}

implementation

{$i classes.inc}

end.

{
  $Log$
  Revision 1.4  1998-10-26 23:01:52  florian
    * reference to objpas unit removed, is now done by -S2

  Revision 1.3  1998/05/06 13:00:25  michael
  + Added strings to uses clause, for TStrings class.

  Revision 1.2  1998/05/04 14:31:51  michael
  + Split classes file.

  Revision 1.1  1998/05/04 12:16:01  florian
    + Initial revisions after making a new directory structure

}
