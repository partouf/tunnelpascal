{
    $Id$
    Copyright (c) 2000 by Florian Klaempfl

    This unit contains basic functions for unicode support in the
    compiler, this unit is mainly necessary to bootstrap widestring
    support ...

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
unit widestr;

  interface

{    uses
       charset;
}

    type
       tcompilerwidechar = word;
       tcompilerwidecharptr = ^tcompilerwidechar;
{$ifdef delphi}
       { delphi doesn't allow pointer accessing as array }
       tcompilerwidechararray = array[0..0] of tcompilerwidechar;
       pcompilerwidechar = ^tcompilerwidechararray;
{$else}
       pcompilerwidechar = ^tcompilerwidechar;
{$endif}

       pcompilerwidestring = ^_tcompilerwidestring;
       _tcompilerwidestring = record
          data : pcompilerwidechar;
          maxlen,len : longint;
       end;

    procedure initwidestring(var r : pcompilerwidestring);
    procedure donewidestring(var r : pcompilerwidestring);
    procedure setlengthwidestring(r : pcompilerwidestring;l : longint);
    function getlengthwidestring(r : pcompilerwidestring) : longint;
    procedure concatwidestringchar(r : pcompilerwidestring;c : tcompilerwidechar);
    procedure concatwidestrings(s1,s2 : pcompilerwidestring);
    function comparewidestrings(s1,s2 : pcompilerwidestring) : longint;
    procedure copywidestring(s,d : pcompilerwidestring);
    function asciichar2unicode(c : char) : tcompilerwidechar;
    function unicode2asciichar(c : tcompilerwidechar) : char;
    procedure ascii2unicode(p:pchar; l:longint;r : pcompilerwidestring);
    procedure unicode2ascii(r : pcompilerwidestring;p:pchar);
    function getcharwidestring(r : pcompilerwidestring;l : longint) : tcompilerwidechar;
    function cpavailable(const s : string) : boolean;

  implementation

{    uses
       i8869_1,cp850,cp437; }

    uses
       globals;

    procedure initwidestring(var r : pcompilerwidestring);

      begin
         new(r);
         r^.data:=nil;
         r^.len:=0;
         r^.maxlen:=0;
      end;

    procedure donewidestring(var r : pcompilerwidestring);

      begin
         if assigned(r^.data) then
           freemem(r^.data);
         dispose(r);
         r:=nil;
      end;

    function getcharwidestring(r : pcompilerwidestring;l : longint) : tcompilerwidechar;

      begin
         getcharwidestring:=r^.data[l];
      end;

    function getlengthwidestring(r : pcompilerwidestring) : longint;

      begin
         getlengthwidestring:=r^.len;
      end;

    procedure setlengthwidestring(r : pcompilerwidestring;l : longint);

      begin
         if r^.maxlen>=l then
           exit;
         if assigned(r^.data) then
           reallocmem(r^.data,sizeof(tcompilerwidechar)*l)
         else
           getmem(r^.data,sizeof(tcompilerwidechar)*l);
      end;

    procedure concatwidestringchar(r : pcompilerwidestring;c : tcompilerwidechar);

      begin
         if r^.len>=r^.maxlen then
           setlengthwidestring(r,r^.len+16);
         r^.data[r^.len]:=c;
         inc(r^.len);
      end;

    procedure concatwidestrings(s1,s2 : pcompilerwidestring);
      begin
         setlengthwidestring(s1,s1^.len+s2^.len);
         inc(s1^.len,s2^.len);
         move(s2^.data^,s1^.data[s1^.len],s2^.len*sizeof(tcompilerwidechar));
      end;

    function comparewidestringwidestring(s1,s2 : pcompilerwidestring) : longint;

      begin
        {$ifdef fpc}{$warning todo}{$endif}
        comparewidestringwidestring:=0;
      end;

    procedure copywidestring(s,d : pcompilerwidestring);

      begin
         setlengthwidestring(d,s^.len);
         d^.len:=s^.len;
         move(s^.data^,d^.data^,s^.len*sizeof(tcompilerwidechar));
      end;

    function comparewidestrings(s1,s2 : pcompilerwidestring) : longint;

      begin
         {!!!!!! FIXME }
         comparewidestrings:=0;
      end;

    function asciichar2unicode(c : char) : tcompilerwidechar;
{!!!!!!!!
      var
         m : punicodemap;

      begin
         m:=getmap(aktsourcecodepage);
         asciichar2unicode:=getunicode(c,m);
      end;
}
      begin
        {$ifdef fpc}{$warning todo}{$endif}
        asciichar2unicode:=0;
      end;

    function unicode2asciichar(c : tcompilerwidechar) : char;

      begin
        {$ifdef fpc}{$warning todo}{$endif}
        unicode2asciichar:=#0;
      end;

    procedure ascii2unicode(p:pchar; l:longint;r : pcompilerwidestring);
(*
      var
         m : punicodemap;
         i : longint;

      begin
         m:=getmap(aktsourcecodepage);
         { should be a very good estimation :) }
         setlengthwidestring(r,length(s));
         // !!!! MBCS
         for i:=1 to length(s) do
           begin
           end;
      end;
*)
      var
        source : pchar;
        dest   : tcompilerwidecharptr;
        i      : longint;
      begin
        setlengthwidestring(r,l);
        source:=p;
        r^.len:=l;
        dest:=tcompilerwidecharptr(r^.data);
        for i:=1 to l do
         begin
           if byte(source^)<128 then
            dest^:=tcompilerwidechar(byte(source^))
           else
            dest^:=32;
           inc(dest);
           inc(source);
         end;
      end;


    procedure unicode2ascii(r : pcompilerwidestring;p:pchar);
(*
      var
         m : punicodemap;
         i : longint;

      begin
         m:=getmap(aktsourcecodepage);
         { should be a very good estimation :) }
         setlengthwidestring(r,length(s));
         // !!!! MBCS
         for i:=1 to length(s) do
           begin
           end;
      end;
*)
      var
        source : tcompilerwidecharptr;
        dest   : pchar;
        i      : longint;
      begin
        source:=tcompilerwidecharptr(r^.data);
        dest:=p;
        for i:=1 to r^.len do
         begin
           if word(source^)<128 then
            dest^:=char(word(source^))
           else
            dest^:=' ';
           inc(dest);
           inc(source);
         end;
      end;


    function cpavailable(const s : string) : boolean;
{!!!!!!
      begin
          cpavailable:=mappingavailable(s);
      end;
}

      begin
        cpavailable:=false;
      end;

end.
{
  $Log$
  Revision 1.7  2001-09-02 21:16:25  peter
    * delphi fixes

  Revision 1.6  2001/07/08 21:00:16  peter
    * various widestring updates, it works now mostly without charset
      mapping supported

  Revision 1.5  2001/05/27 14:30:55  florian
    + some widestring stuff added

  Revision 1.4  2001/05/08 21:06:33  florian
    * some more support for widechars commited especially
      regarding type casting and constants

  Revision 1.3  2001/04/13 01:22:17  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.2  2001/04/02 21:20:35  peter
    * resulttype rewrite

  Revision 1.1  2000/11/29 00:30:43  florian
    * unused units removed from uses clause
    * some changes for widestrings

}
