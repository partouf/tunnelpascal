{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit strings;

  { Zero-terminated (ascii-z) string handling }

  interface
    {$ifndef FPK}
    {$E-}
    {$endif}

    { Returns the length of a string }
    function strlen(p : pchar) : longint;

    { Converts a Pascal string to a null-terminated string }
    function strpcopy(d : pchar;const s : string) : pchar;

    { Converts a null-terminated string to a Pascal string }
    function strpas(p : pchar) : string;

    { Copies source to dest, returns a pointer to dest }
    function strcopy(dest,source : pchar) : pchar;

    { Copies at most maxlen bytes from source to dest. }
    { Returns a pointer to dest }
    function strlcopy(dest,source : pchar;maxlen : longint) : pchar;

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
    function strcomp(str1,str2 : pchar) : longint;

    { The same as strcomp, but at most l characters are compared  }
    function strlcomp(str1,str2 : pchar;l : longint) : longint;

    { The same as strcomp but case insensitive       }
    function stricomp(str1,str2 : pchar) : longint;

    { Copies l characters from source to dest, returns dest. }
    function strmove(dest,source : pchar;l : longint) : pchar;

    { Appends at most l characters from source to dest }
    function strlcat(dest,source : pchar;l : longint) : pchar;

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
    function strlicomp(str1,str2 : pchar;l : longint) : longint;

    { Returns a pointer to the first occurrence of str2 in    }
    { str2 Otherwise returns nil                          }
    function strpos(str1,str2 : pchar) : pchar;

    { Makes a copy of p on the heap, and returns a pointer to this copy  }
    function strnew(p : pchar) : pchar;

    { Allocates L bytes on the heap, returns a pchar pointer to it }
    function stralloc(L : longint) : pchar;

    { Releases a null-terminated string from the heap  }
    procedure strdispose(p : pchar);

  implementation

    function strcopy(dest,source : pchar) : pchar;

      begin
         asm
            cld
            movl 12(%ebp),%edi
            movl $0xffffffff,%ecx
            xorb %al,%al
            repne
            scasb
            not %ecx
            movl 8(%ebp),%edi
            movl 12(%ebp),%esi
            movl %ecx,%eax
            shrl $2,%ecx
            rep
            movsl
            movl %eax,%ecx
            andl $3,%ecx
            rep
            movsb
            movl 8(%ebp),%eax
            leave
            ret $8
         end;
      end;

    function strecopy(dest,source : pchar) : pchar;

      begin
         asm
            cld
            movl 12(%ebp),%edi
            movl $0xffffffff,%ecx
            xorl %eax,%eax
            repne
            scasb
            not %ecx
            movl 8(%ebp),%edi
            movl 12(%ebp),%esi
            movl %ecx,%eax
            shrl $2,%ecx
            rep
            movsl
            movl %eax,%ecx
            andl $3,%ecx
            rep
            movsb
            movl 8(%ebp),%eax
            decl %edi
            movl %edi,%eax
            leave
            ret $8
         end ['EAX','ESI','EDI'];
      end;

    function strlcopy(dest,source : pchar;maxlen : longint) : pchar;

      begin
         asm
            movl 8(%ebp),%edi
            movl 12(%ebp),%esi
            movl 16(%ebp),%ecx
            cld
         .LSTRLCOPY1:
            lodsb
            stosb
            decl %ecx           // Lower maximum
            jz .LSTRLCOPY2      // 0 reached ends
            orb %al,%al
            jnz .LSTRLCOPY1
            movl 8(%ebp),%eax
            leave
            ret $12
        .LSTRLCOPY2:

            xorb %al,%al        // If cutted
            stosb               // add a #0
            movl 8(%ebp),%eax
            leave
            ret $12
         end ['EAX','ECX','ESI','EDI'];
      end;

    function strlen(p : pchar) : longint;
    begin
      asm
        cld
        movl    8(%ebp),%edi
        movl    $0xffffffff,%ecx
        xorl    %eax,%eax
        repne
        scasb
        movl    $0xfffffffe,%eax
        subl    %ecx,%eax
        leave
        ret     $4
      end ['EDI','ECX','EAX'];
    end;

    function strend(p : pchar) : pchar;

      begin
         asm
            cld
            movl 8(%ebp),%edi
            movl $0xffffffff,%ecx
            xorl %eax,%eax
            repne
            scasb
            movl %edi,%eax
            decl %eax
            leave
            ret $4
         end ['EDI','ECX','EAX'];
      end;

    function strpcopy(d : pchar;const s : string) : pchar;

      begin
         asm
            pushl %esi          // Save ESI
            cld
            movl 8(%ebp),%edi   // load destination address
            movl 12(%ebp),%esi   // Load Source adress
            movl %edi,%ebx      // Set return value
            lodsb               // load length in ECX
            movzbl %al,%ecx
            rep
            movsb
            xorb %al,%al        // Set #0
            stosb
            movl %ebx,%eax      // return value to EAX
            popl %esi
            leave               // ... and ready
            ret $8
         end ['EDI','ESI','EBX','EAX','ECX'];
      end;

    function strpas(p : pchar) : string;
    begin
      asm
        cld
        movl    12(%ebp),%edi
        movl    $0xff,%ecx
        xorl    %eax,%eax
        movl    %edi,%esi
        repne
        scasb
        movl    %ecx,%eax

        movl    8(%ebp),%edi
        notb    %al
        decl    %eax
        stosb
        cmpl    $7,%eax
        jl      .LStrPas2
        movl    %edi,%ecx       # Align on 32bits
        negl    %ecx
        andl    $3,%ecx
        subl    %ecx,%eax
        rep
        movsb
        movl    %eax,%ecx
        andl    $3,%eax
        shrl    $2,%ecx
        rep
        movsl
.LStrPas2:
        movl    %eax,%ecx
        rep
        movsb
      end ['ECX','EAX','ESI','EDI'];
    end;

    function strcat(dest,source : pchar) : pchar;

      begin
         strcat:=strcopy(strend(dest),source);
      end;

    function strlcat(dest,source : pchar;l : longint) : pchar;

      var
         destend : pchar;

      begin
         destend:=strend(dest);
         l:=l-(destend-dest);
         strlcat:=strlcopy(destend,source,l);
      end;

    function strcomp(str1,str2 : pchar) : longint;

      begin
         asm
            // Find terminating zero
            movl 12(%ebp),%edi
            movl $0xffffffff,%ecx
            cld
            xorl %eax,%eax
            repne
            scasb
            not %ecx
            movl 12(%ebp),%edi
            movl 8(%ebp),%esi
            repe
            cmpsb
            movb -1(%esi),%al
            movzbl -1(%edi),%ecx
            subl %ecx,%eax
            leave
            ret $8
         end ['EAX','ECX','ESI','EDI'];
      end;

    function strlcomp(str1,str2 : pchar;l : longint) : longint;

      begin
         asm
            // Find terminating zero
            movl 12(%ebp),%edi
            movl $0xffffffff,%ecx
            cld
            xorl %eax,%eax
            repne
            scasb
            not %ecx
            cmpl 16(%ebp),%ecx
            jl .LSTRLCOMP1
            movl 16(%ebp),%ecx
        .LSTRLCOMP1:
            movl 12(%ebp),%edi
            movl 8(%ebp),%esi
            repe
            cmpsb
            movb -1(%esi),%al
            movzbl -1(%edi),%ecx
            subl %ecx,%eax
            leave
            ret $12
         end ['EAX','ECX','ESI','EDI'];
      end;

    function stricomp(str1,str2 : pchar) : longint;

      begin
         asm
            // Find terminating zero
            movl 12(%ebp),%edi
            movl $0xffffffff,%ecx
            cld
            xorl %eax,%eax
            repne
            scasb
            not %ecx
            movl 12(%ebp),%edi
            movl 8(%ebp),%esi
       .LSTRICOMP2:
            repe
            cmpsb
            jz .LSTRICOMP3      // If last reached then exit
            movb (%esi),%al
            movzbl (%edi),%ebx
            cmpb $97,%al
            jb .LSTRICOMP1
            cmpb $122,%al
            ja .LSTRICOMP1
            subb $0x20,%al
        .LSTRICOMP1:
            cmpb $97,%bl
            jb .LSTRICOMP4
            cmpb $122,%bl
            ja .LSTRICOMP4
            subb $0x20,%bl
       .LSTRICOMP4:
            subl %ebx,%eax
            jz .LSTRICOMP2      // If still equal, compare again
       .LSTRICOMP3:
            leave
            ret $8
         end ['EAX','ECX','ESI','EDI'];
      end;

    function strlicomp(str1,str2 : pchar;l : longint) : longint;

      begin
         asm
            // Search terminating zero
            movl 12(%ebp),%edi
            movl $0xffffffff,%ecx
            cld
            xorl %eax,%eax
            repne
            scasb
            not %ecx
            cmpl 16(%ebp),%ecx
            jl .LSTRLICOMP5
            movl 16(%ebp),%ecx
       .LSTRLICOMP5:
            movl 12(%ebp),%edi
            movl 8(%ebp),%esi
       .LSTRLICOMP2:
            repe
            cmpsb
            jz .LSTRLICOMP3     // If last reached, exit
            movb (%esi),%al
            movzbl (%edi),%ebx
            cmpb $97,%al
            jb .LSTRLICOMP1
            cmpb $122,%al
            ja .LSTRLICOMP1
            subb $0x20,%al
        .LSTRLICOMP1:
            cmpb $97,%bl
            jb .LSTRLICOMP4
            cmpb $122,%bl
            ja .LSTRLICOMP4
            subb $0x20,%bl
       .LSTRLICOMP4:
            subl %ebx,%eax
            jz .LSTRLICOMP2

       .LSTRLICOMP3:
            leave
            ret $12
         end ['EAX','ECX','ESI','EDI'];
      end;

    function strmove(dest,source : pchar;l : longint) : pchar;

      begin
         move(source^,dest^,l);
         strmove:=dest;
      end;

    function strscan(p : pchar;c : char) : pchar;

      begin
         asm
            movl 8(%ebp),%edi
            movl $0xffffffff,%ecx
            cld
            xorb %al,%al
            repne
            scasb
            not %ecx
            movb 12(%ebp),%al
            movl 8(%ebp),%edi
            repne
            scasb
            movl $0,%eax
            jnz .LSTRSCAN
            movl %edi,%eax
            decl %eax
        .LSTRSCAN:
            leave
            ret $6
         end;
      end;

    function strrscan(p : pchar;c : char) : pchar;

      begin
         asm
            movl 8(%ebp),%edi
            movl $0xffffffff,%ecx
            cld
            xorb %al,%al
            repne
            scasb
            not %ecx
            movb 12(%ebp),%al
            movl 8(%ebp),%edi
            addl %ecx,%edi
            decl %edi
            std
            repne
            scasb
            movl $0,%eax

            jnz .LSTRRSCAN
            movl %edi,%eax
            incl %eax
        .LSTRRSCAN:
            leave
            ret $6
         end;
      end;

    function strupper(p : pchar) : pchar;

      begin
         asm
            movl 8(%ebp),%esi
            movl %esi,%edi
         .LSTRUPPER1:
            lodsb
            cmpb $97,%al
            jb .LSTRUPPER3
            cmpb $122,%al
            ja .LSTRUPPER3
            subb $0x20,%al
         .LSTRUPPER3:
            stosb
            orb %al,%al
            jnz .LSTRUPPER1
            movl 8(%ebp),%eax
            leave
            ret $4
         end;
      end;

    function strlower(p : pchar) : pchar;

      begin
         asm
            movl 8(%ebp),%esi
            movl %esi,%edi
         .LSTRLOWER1:
            lodsb
            cmpb $65,%al
            jb .LSTRLOWER3
            cmpb $90,%al
            ja .LSTRLOWER3
            addb $0x20,%al
         .LSTRLOWER3:
            stosb
            orb %al,%al
            jnz .LSTRLOWER1
            movl 8(%ebp),%eax
            leave
            ret $4
         end;
      end;

    function strpos(str1,str2 : pchar) : pchar;

      var
         p : pchar;
         lstr2 : longint;

      begin
         strpos:=nil;
         p:=strscan(str1,str2^);
         if p=nil then
           exit;
         lstr2:=strlen(str2);
         while p<>nil do
           begin
              if strlcomp(p,str2,lstr2)=0 then
                begin
                   strpos:=p;
                   exit;
                end;
              inc(longint(p));
              p:=strscan(p,str2^);
           end;
      end;

    procedure strdispose(p : pchar);

      begin
         if p<>nil then
           freemem(p,strlen(p)+1);
      end;

    function strnew(p : pchar) : pchar;

      var
         len : longint;

      begin
         strnew:=nil;
         if (p=nil) or (p^=#0) then
           exit;
         len:=strlen(p)+1;
         getmem(strnew,len);
         if strnew<>nil then
           strmove(strnew,p,len);
      end;

      function stralloc(L : longint) : pchar;

      begin
         StrAlloc:=Nil;
         GetMem (Stralloc,l);
      end;

end.

{
  $Log$
  Revision 1.1.1.1  1998-03-25 11:18:42  root
  * Restored version

  Revision 1.7  1998/02/24 17:50:46  peter
    * upto 100% (255's char is different ;) faster STRCMP
    * faster StrPas from i386.inc also strings.pp

  Revision 1.6  1998/01/26 11:59:12  michael
  + Added log at the end

  revision 1.5
  date: 1998/01/16 16:14:08;  author: michael;  state: Exp;  lines: +11 -1
  + Implemented StrAlloc() function.
  ----------------------------
  revision 1.4
  date: 1997/12/01 18:21:39;  author: pierre;  state: Exp;  lines: +2 -2
     * small bug without importance fixed
       in strrscan a call to a label in strscan was called
  ----------------------------
  revision 1.3
  date: 1997/12/01 12:34:38;  author: michael;  state: Exp;  lines: +11 -4
  + added copyright reference in header.
  ----------------------------
  revision 1.2
  date: 1997/11/28 18:59:10;  author: pierre;  state: Exp;  lines: +42 -42
     local labels prefixed with .L
  ----------------------------
  revision 1.1
  date: 1997/11/27 08:33:47;  author: michael;  state: Exp;
  Initial revision
  ----------------------------
  revision 1.1.1.1
  date: 1997/11/27 08:33:47;  author: michael;  state: Exp;  lines: +0 -0
  FPC RTL CVS start
  =============================================================================

  1.5.1994: Version 0.9
            Unit ist komplett implementiert (noch nicht getestet)
  20.3.1995: Version 0.91
            strmove korriert, f�r system.move m�ssen Pointer
            dereferenziert werden
  24.12.1995: Version 0.92
            strcomp war fehlerhaft; korrigiert
            dito strlcomp
}

