{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Strings unit for PChar (asciiz/C compatible strings) handling

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit strings;
{$S-}
interface

    { Returns the length of a string }
    function strlen(p : pchar) : strlenint;

    { Converts a Pascal string to a null-terminated string }
    function strpcopy(d : pchar;const s : string) : pchar;

    { Converts a null-terminated string to a Pascal string }
    function strpas(p : pchar) : string;

    { Copies source to dest, returns a pointer to dest }
    function strcopy(dest,source : pchar) : pchar;

    { Copies at most maxlen bytes from source to dest. }
    { Returns a pointer to dest }
    function strlcopy(dest,source : pchar;maxlen : strlenint) : pchar;

    { Copies source to dest and returns a pointer to the terminating }
    { null character.    }
    function strecopy(dest,source : pchar) : pchar;

    { Returns a pointer tro the terminating null character of p }
    function strend(p : pchar) : pchar;

    { Appends source to dest, returns a pointer do dest}
    function strcat(dest,source : pchar) : pchar;

    { Compares str1 und str2, returns }
    { a value <0 if str1<str2;        }
    {  0 when str1=str2               }
    { and a value >0 if str1>str2     }
    function strcomp(str1,str2 : pchar) : strlenint;

    { The same as strcomp, but at most l characters are compared  }
    function strlcomp(str1,str2 : pchar;l : strlenint) : strlenint;

    { The same as strcomp but case insensitive       }
    function stricomp(str1,str2 : pchar) : strlenint;

    { Copies l characters from source to dest, returns dest. }
    function strmove(dest,source : pchar;l : strlenint) : pchar;

    { Appends at most l characters from source to dest }
    function strlcat(dest,source : pchar;l : strlenint) : pchar;

    { Returns a pointer to the first occurrence of c in p }
    { If c doesn't occur, nil is returned }
    function strscan(p : pchar;c : char) : pchar;

    { Returns a pointer to the last occurrence of c in p }
    { If c doesn't occur, nil is returned }
    function strrscan(p : pchar;c : char) : pchar;

    { converts p to all-lowercase, returns p   }
    function strlower(p : pchar) : pchar;

    { converts p to all-uppercase, returns p  }
    function strupper(p : pchar) : pchar;

    { The same al stricomp, but at most l characters are compared }
    function strlicomp(str1,str2 : pchar;l : strlenint) : strlenint;

    { Returns a pointer to the first occurrence of str2 in    }
    { str2 Otherwise returns nil                          }
    function strpos(str1,str2 : pchar) : pchar;

    { Makes a copy of p on the heap, and returns a pointer to this copy  }
    function strnew(p : pchar) : pchar;

    { Allocates L bytes on the heap, returns a pchar pointer to it }
    function stralloc(L : strlenint) : pchar;

    { Releases a null-terminated string from the heap  }
    procedure strdispose(p : pchar);

implementation

{$ifdef FPC_USE_LIBC}
{$i cgenstr.inc}
{$endif FPC_USE_LIBC}

{  Read Processor dependent part, shared with sysutils unit }
{$i strings.inc }

{ Read processor denpendent part, NOT shared with sysutils unit }
{$i stringss.inc }

{ Read generic string functions that are not implemented for the processor }
{$i genstr.inc}
{$i genstrs.inc}

{ Functions not in assembler, but shared with sysutils unit  }
{$i stringsi.inc}

{ Functions, different from the one in sysutils }

    function stralloc(L : strlenint) : pchar;

      begin
         StrAlloc:=Nil;
         GetMem (Stralloc,l);
      end;

    function strnew(p : pchar) : pchar;

      var
         len : strlenint;

      begin
         strnew:=nil;
         if (p=nil) or (p^=#0) then
           exit;
         len:=strlen(p)+1;
         getmem(strnew,len);
         if strnew<>nil then
           strmove(strnew,p,len);
      end;

    procedure strdispose(p : pchar);

      begin
         if p<>nil then
          begin
            freemem(p);
            p:=nil;
          end;
      end;

end.

{
  $Log$
  Revision 1.7  2004-05-01 15:26:33  jonas
    * use some more string routines from libc if FPC_USE_LIBC is used

  Revision 1.6  2004/02/18 22:00:59  peter
    * use strlenint instead of longint

  Revision 1.5  2003/07/07 20:22:05  peter
    * generic string routines added

  Revision 1.4  2002/09/07 15:07:46  peter
    * old logs removed and tabs fixed

}
