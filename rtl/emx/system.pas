{
 $Id$
 ****************************************************************************

    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2002 by Free Pascal development team

    Free Pascal - EMX runtime library

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

****************************************************************************}

unit {$ifdef VER1_0}sysemx{$else}System{$endif};

interface

{Link the startup code.}
{$ifdef VER1_0}
 {$l prt1.oo2}
{$else}
 {$l prt1.o}
{$endif}

{$I systemh.inc}

{$I heaph.inc}

{Platform specific information}
type
  THandle = Longint;

const
 LineEnding = #13#10;
{ LFNSupport is defined separately below!!! }
 DirectorySeparator = '\';
 DriveSeparator = ':';
 PathSeparator = ';';
{ FileNameCaseSensitive is defined separately below!!! }
 maxExitCode = 255;

type    Tos=(osDOS,osOS2,osDPMI);

var     os_mode:Tos;
        first_meg:pointer;

type    TByteArray = array [0..$ffff] of byte;
        PByteArray = ^TByteArray;

        TSysThreadIB = record
            TID,
            Priority,
            Version: cardinal;
            MCCount,
            MCForceFlag: word;
        end;
        PSysThreadIB = ^TSysThreadIB;

        TThreadInfoBlock = record
            PExChain,
            Stack,
            StackLimit: pointer;
            TIB2: PSysThreadIB;
            Version,
            Ordinal: cardinal;
        end;
        PThreadInfoBlock = ^TThreadInfoBlock;
        PPThreadInfoBlock = ^PThreadInfoBlock;

        TProcessInfoBlock = record
            PID,
            ParentPid,
            Handle: cardinal;
            Cmd,
            Env: PByteArray;
            Status,
            ProcType: cardinal;
        end;
        PProcessInfoBlock = ^TProcessInfoBlock;
        PPProcessInfoBlock = ^PProcessInfoBlock;

const   UnusedHandle=-1;
        StdInputHandle=0;
        StdOutputHandle=1;
        StdErrorHandle=2;

        LFNSupport: boolean = true;
        FileNameCaseSensitive: boolean = false;

        sLineBreak = LineEnding;
        DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsCRLF;

var
{ C-compatible arguments and environment }
  argc  : longint;external name '_argc';
  argv  : ppchar;external name '_argv';
  envp  : ppchar;external name '_environ';
  EnvC: cardinal; external name '_envc';

(* Pointer to the block of environment variables - used e.g. in unit Dos. *)
  Environment: PChar;

var
(* Type / run mode of the current process: *)
(* 0 .. full screen OS/2 session           *)
(* 1 .. DOS session                        *)
(* 2 .. VIO windowable OS/2 session        *)
(* 3 .. Presentation Manager OS/2 session  *)
(* 4 .. detached (background) OS/2 process *)
  ApplicationType: cardinal;

implementation

{$I system.inc}

var
    heap_base: pointer; external name '__heap_base';
    heap_brk: pointer; external name '__heap_brk';
    heap_end: pointer; external name '__heap_end';

(* Maximum heap size - only used if heap is allocated as continuous block. *)
{$IFDEF CONTHEAP}
    BrkLimit: cardinal;
{$ENDIF CONTHEAP}

procedure DosGetInfoBlocks (PATIB: PPThreadInfoBlock;
                            PAPIB: PPProcessInfoBlock); cdecl;
                            external 'DOSCALLS' index 312;

function DosLoadModule (ObjName: PChar; ObjLen: cardinal; DLLName: PChar;
                                        var Handle: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 318;

function DosQueryProcAddr (Handle, Ordinal: cardinal; ProcName: PChar;
                                        var Address: pointer): cardinal; cdecl;
external 'DOSCALLS' index 321;

function DosSetRelMaxFH (var ReqCount: longint; var CurMaxFH: cardinal):
                                                               cardinal; cdecl;
external 'DOSCALLS' index 382;

function DosSetCurrentDir (Name:PChar): cardinal; cdecl;
external 'DOSCALLS' index 255;

function DosSetDefaultDisk (DiskNum:cardinal): cardinal; cdecl;
external 'DOSCALLS' index 220;

{ This is not real prototype, but is close enough }
{ for us (the 2nd parameter is actually a pointer }
{ to a structure).                                }
function DosCreateDir (Name: PChar; P: pointer): cardinal; cdecl;
external 'DOSCALLS' index 270;

function DosDeleteDir (Name: PChar): cardinal; cdecl;
external 'DOSCALLS' index 226;

{This is the correct way to call external assembler procedures.}
procedure syscall; external name '___SYSCALL';

{
procedure syscall; external 'EMX' index 2;

procedure emx_init; external 'EMX' index 1;
}



   { converts an OS/2 error code to a TP compatible error }
   { code. Same thing exists under most other supported   }
   { systems.                                             }
   { Only call for OS/2 DLL imported routines             }
   Procedure Errno2InOutRes;
   Begin
     { errors 1..18 are the same as in DOS }
     case InOutRes of
      { simple offset to convert these error codes }
      { exactly like the error codes in Win32      }
      19..31 : InOutRes := InOutRes + 131;
      { gets a bit more complicated ... }
      32..33 : InOutRes := 5;
      38 : InOutRes := 100;
      39 : InOutRes := 101;
      112 : InOutRes := 101;
      110 : InOutRes := 5;
      114 : InOutRes := 6;
      290 : InOutRes := 290;
     end;
     { all other cases ... we keep the same error code }
   end;


{****************************************************************************

                    Miscellaneous related routines.

****************************************************************************}

{$asmmode intel}
procedure system_exit; assembler;
asm
    mov  ah, 04ch
    mov  al, byte ptr exitcode
    call syscall
end {['EAX']};

{$ASMMODE ATT}

function paramcount:longint;assembler;

asm
    movl argc,%eax
    decl %eax
end {['EAX']};

    function args:pointer;assembler;

    asm
        movl argv,%eax
end {['EAX']};


function paramstr(l:longint):string;

var p:^Pchar;

begin
    { There seems to be a problem with EMX for DOS when trying to }
    { access paramstr(0), and to avoid problems between DOS and   }
    { OS/2 they have been separated.                              }
    if os_Mode = OsOs2 then
    begin
    if L = 0 then
        begin
            GetMem (P, 260);
            p[0] := #0;  { in case of error, initialize to empty string }
{$ASMMODE INTEL}
            asm
                mov edx, P
                mov ecx, 260
                mov eax, 7F33h
                call syscall    { error handle already with empty string }
            end ['eax', 'ecx', 'edx'];
            ParamStr := StrPas (PChar (P));
            FreeMem (P, 260);
        end
    else
        if (l>0) and (l<=paramcount) then
            begin
                p:=args;
                paramstr:=strpas(p[l]);
            end
        else paramstr:='';
    end
   else
    begin
      p:=args;
      paramstr:=strpas(p[l]);
    end;
end;


procedure randomize; assembler;
asm
    mov ah, 2Ch
    call syscall
    mov word ptr [randseed], cx
    mov word ptr [randseed + 2], dx
end {['eax', 'ecx', 'edx']};

{$ASMMODE ATT}

{****************************************************************************

                    Heap management releated routines.

****************************************************************************}


{ this function allows to extend the heap by calling
syscall $7f00 resizes the brk area}

function sbrk(size:longint):pointer;
{$IFDEF DUMPGROW}
var
  L: longword;
begin
  WriteLn ('Trying to grow heap by ', Size, ' to ', HeapSize + Size);
{$IFDEF CONTHEAP}
  WriteLn ('BrkLimit is ', BrkLimit);
{$ENDIF CONTHEAP}
  asm
    movl size,%edx
    movw $0x7f00,%ax
    call syscall     { result directly in EAX }
    inc %eax         { Result in EAX, -1 = error (has to be transformed to 0) }
    jz .LSbrk_End
    dec %eax         { No error - back to previous value }
.LSbrk_End:
    mov  %eax,L
  end ['eax', 'edx'];
  WriteLn ('New heap at ', L);
  Sbrk := pointer (L);
end;
{$ELSE DUMPGROW}
                                     assembler;
asm
{$IFDEF REGCALL}
    movl %eax,%edx
{$ELSE REGCALL}
    movl size,%edx
{$ENDIF REGCALL}
    movw $0x7f00,%ax
    call syscall
    inc %eax         { Result in EAX, -1 = error (has to be transformed to 0) }
    jz .LSbrk_End
    dec %eax         { No error - back to previous value }
.LSbrk_End:
end {['eax', 'edx']};
{$ENDIF DUMPGROW}

function getheapstart:pointer;assembler;

asm
    movl heap_base,%eax
end {['EAX']};

function getheapsize:longint;assembler;
asm
    movl heap_brk,%eax
end {['EAX']};


function SysOSAlloc (Size: ptrint): pointer;
begin
 SysOSAlloc := Sbrk (Size);
end;

{.$define HAS_SYSOSFREE}

procedure SysOSFree (P: pointer; Size: ptrint);
begin
end;


{$i heap.inc}

{****************************************************************************

                          Low Level File Routines

****************************************************************************}

procedure allowslash(p:Pchar);

{Allow slash as backslash.}

var i:longint;

begin
    for i:=0 to strlen(p) do
        if p[i]='/' then p[i]:='\';
end;

procedure do_close (H: THandle);

begin
{ Only three standard handles under real OS/2 }
  if (h > 4) or
     ((os_MODE = osOS2) and (h > 2)) then
   begin
     asm
        pushl %ebx
        movb $0x3e,%ah
        movl h,%ebx
        call syscall
        jnc  .Lnoerror           { error code?            }
        movw  %ax, InOutRes       { yes, then set InOutRes }
     .Lnoerror:
        popl %ebx
     end ['eax'];
   end;
end;

procedure do_erase(p:Pchar);

begin
    allowslash(p);
    asm
        movl P,%edx
        movb $0x41,%ah
        call syscall
        jnc .LERASE1
        movw %ax,inoutres
    .LERASE1:
    end ['eax', 'edx'];
end;

procedure do_rename(p1,p2:Pchar);

begin
    allowslash(p1);
    allowslash(p2);
    asm
        movl P1, %edx
        movl P2, %edi
        movb $0x56,%ah
        call syscall
        jnc .LRENAME1
        movw %ax,inoutres
    .LRENAME1:
    end ['eax', 'edx', 'edi'];
end;

function do_read (H: THandle; Addr: pointer; Len: longint): longint; assembler;
asm
    pushl %ebx
{$IFNDEF REGCALL}
    movl len,%ecx
    movl addr,%edx
    movl %eax,%ebx
{$ELSE REGCALL}
    movl h,%ebx
{$ENDIF REGCALL}
    movb $0x3f,%ah
    call syscall
    jnc .LDOSREAD1
    movw %ax,inoutres
    xorl %eax,%eax
.LDOSREAD1:
    popl %ebx
end {['eax', 'ebx', 'ecx', 'edx']};

function do_write (H: THandle; Addr: pointer; Len: longint): longint;
                                                                     assembler;
asm
    pushl %ebx
{$IFDEF REGCALL}
    movl %eax,%ebx
{$ENDIF REGCALL}
    xorl %eax,%eax
    cmpl $0,len    { 0 bytes to write is undefined behavior }
    jz   .LDOSWRITE1
{$IFNDEF REGCALL}
    movl len,%ecx
    movl addr,%edx
    movl h,%ebx
{$ENDIF REGCALL}
    movb $0x40,%ah
    call syscall
    jnc .LDOSWRITE1
    movw %ax,inoutres
.LDOSWRITE1:
    popl %ebx
end {['eax', 'ebx', 'ecx', 'edx']};

function do_filepos (Handle: THandle): longint; assembler;
asm
    pushl %ebx
{$IFDEF REGCALL}
    movl %eax,%ebx
{$ELSE REGCALL}
    movl handle,%ebx
{$ENDIF REGCALL}
    movw $0x4201,%ax
    xorl %edx,%edx
    call syscall
    jnc .LDOSFILEPOS
    movw %ax,inoutres
    xorl %eax,%eax
.LDOSFILEPOS:
    popl %ebx
end {['eax', 'ebx', 'ecx', 'edx']};

procedure do_seek (Handle: THandle; Pos: longint); assembler;
asm
    pushl %ebx
{$IFDEF REGCALL}
    movl %eax,%ebx
{$ELSE REGCALL}
    movl handle,%ebx
    movl pos,%edx
{$ENDIF REGCALL}
    movw $0x4200,%ax
    call syscall
    jnc .LDOSSEEK1
    movw %ax,inoutres
.LDOSSEEK1:
    popl %ebx
end {['eax', 'ebx', 'ecx', 'edx']};

function do_seekend (Handle: THandle): longint; assembler;
asm
    pushl %ebx
{$IFDEF REGCALL}
    movl %eax,%ebx
{$ELSE REGCALL}
    movl handle,%ebx
{$ENDIF REGCALL}
    movw $0x4202,%ax
    xorl %edx,%edx
    call syscall
    jnc .Lset_at_end1
    movw %ax,inoutres;
    xorl %eax,%eax
.Lset_at_end1:
    popl %ebx
end {['eax', 'ebx', 'ecx', 'edx']};

function do_filesize (Handle: THandle): longint;

var aktfilepos:longint;

begin
    aktfilepos:=do_filepos(handle);
    do_filesize:=do_seekend(handle);
    do_seek(handle,aktfilepos);
end;

procedure do_truncate (Handle: THandle; Pos: longint); assembler;
asm
    pushl %ebx
(* DOS function 40h isn't safe for this according to EMX documentation *)
{$IFDEF REGCALL}
    movl %eax,%ebx
    pushl %eax
{$ELSE REGCALL}
    movl Handle,%ebx
    movl Pos,%edx
{$ENDIF REGCALL}
    movl $0x7F25,%eax
    call syscall
    incl %eax
    movl %ecx, %eax
{$IFDEF REGCALL}
    popl %ebx
{$ENDIF REGCALL}
    jnz .LTruncate1      { compare the value of EAX to verify error }
(* File position is undefined after truncation, move to the end. *)
    movl $0x4202,%eax
{$IFNDEF REGCALL}
    movl Handle,%ebx
{$ENDIF REGCALL}
    movl $0,%edx
    call syscall
    jnc .LTruncate2
.LTruncate1:
    movw %ax,inoutres
.LTruncate2:
    popl %ebx
end {['eax', 'ebx', 'ecx', 'edx']};

const
    FileHandleCount: cardinal = 20;

function Increase_File_Handle_Count: boolean;
var Err: word;
    L1: longint;
    L2: cardinal;
begin
    if os_mode = osOS2 then
        begin
            L1 := 10;
            if DosSetRelMaxFH (L1, L2) <> 0 then
                Increase_File_Handle_Count := false
            else
                if L2 > FileHandleCount then
                    begin
                        FileHandleCount := L2;
                        Increase_File_Handle_Count := true;
                    end
                else
                    Increase_File_Handle_Count := false;
        end
    else
        begin
            Inc (FileHandleCount, 10);
            Err := 0;
            asm
                pushl %ebx
                movl $0x6700, %eax
                movl FileHandleCount, %ebx
                call syscall
                jnc .LIncFHandles
                movw %ax, Err
.LIncFHandles:
                popl %ebx
            end ['eax'];
            if Err <> 0 then
                begin
                    Increase_File_Handle_Count := false;
                    Dec (FileHandleCount, 10);
                end
            else
                Increase_File_Handle_Count := true;
        end;
end;

procedure do_open(var f;p:pchar;flags:longint);

{
  filerec and textrec have both handle and mode as the first items so
  they could use the same routine for opening/creating.
  when (flags and $100)   the file will be append
  when (flags and $1000)  the file will be truncate/rewritten
  when (flags and $10000) there is no check for close (needed for textfiles)
}

var Action: cardinal;

begin
    allowslash(p);
    { close first if opened }
    if ((flags and $10000)=0) then
        begin
            case filerec(f).mode of
                fminput,fmoutput,fminout : Do_Close(filerec(f).handle);
                fmclosed:;
            else
                begin
                    inoutres:=102; {not assigned}
                    exit;
                end;
            end;
       end;
    { reset file handle }
    filerec(f).handle := UnusedHandle;
    Action := 0;
    { convert filemode to filerec modes }
    case (flags and 3) of
        0 : filerec(f).mode:=fminput;
        1 : filerec(f).mode:=fmoutput;
        2 : filerec(f).mode:=fminout;
    end;
    if (flags and $1000)<>0 then
        Action := $50000; (* Create / replace *)
    { empty name is special }
    if p[0]=#0 then
        begin
          case FileRec(f).mode of
            fminput :
              FileRec(f).Handle:=StdInputHandle;
            fminout, { this is set by rewrite }
            fmoutput :
              FileRec(f).Handle:=StdOutputHandle;
            fmappend :
              begin
                FileRec(f).Handle:=StdOutputHandle;
                FileRec(f).mode:=fmoutput; {fool fmappend}
              end;
            end;
            exit;
        end;
    Action := Action or (Flags and $FF);
(* DenyNone if sharing not specified. *)
    if Flags and 112 = 0 then
        Action := Action or 64;
    asm
        pushl %ebx
        movl $0x7f2b, %eax
        movl Action, %ecx
        movl p, %edx
        call syscall
        cmpl $0xffffffff, %eax
        jnz .LOPEN1
        movw %cx, InOutRes
        movl UnusedHandle, %eax
.LOPEN1:
        movl f,%edx         { Warning : This assumes Handle is first }
        movl %eax,(%edx)    { field of FileRec                       }
        popl %ebx
    end ['eax', 'ecx', 'edx'];
    if (InOutRes = 4) and Increase_File_Handle_Count then
(* Trying again after increasing amount of file handles *)
        asm
            pushl %ebx
            movl $0x7f2b, %eax
            movl Action, %ecx
            movl p, %edx
            call syscall
            cmpl $0xffffffff, %eax
            jnz .LOPEN2
            movw %cx, InOutRes
            movl UnusedHandle, %eax
.LOPEN2:
            movl f,%edx
            movl %eax,(%edx)
            popl %ebx
        end ['eax', 'ecx', 'edx'];
      { for systems that have more handles }
    if (FileRec (F).Handle <> UnusedHandle) then
        begin
            if (FileRec (F).Handle > FileHandleCount) then
                                         FileHandleCount := FileRec (F).Handle;
            if ((Flags and $100) <> 0) then
                begin
                    do_seekend (FileRec (F).Handle);
                    FileRec (F).Mode := fmOutput; {fool fmappend}
                end;
        end;
end;

{$ASMMODE INTEL}
function do_isdevice (Handle: THandle): boolean; assembler;
(*
var HT, Attr: longint;
begin
    if os_mode = osOS2 then
        begin
            if DosQueryHType (Handle, HT, Attr) <> 0 then HT := 1;
        end
    else
*)
asm
    push ebx
{$IFDEF REGCALL}
    mov ebx, eax
{$ELSE REGCALL}
    mov ebx, Handle
{$ENDIF REGCALL}
    mov eax, 4400h
    call syscall
    mov eax, 1
    jc @IsDevEnd
    test edx, 80h           { verify if it is a file  }
    jnz @IsDevEnd
    dec eax                 { nope, so result is zero }
@IsDevEnd:
    pop ebx
end {['eax', 'ebx', 'edx']};
{$ASMMODE ATT}


{*****************************************************************************
                           UnTyped File Handling
*****************************************************************************}

{$i file.inc}

{*****************************************************************************
                           Typed File Handling
*****************************************************************************}

{$i typefile.inc}

{*****************************************************************************
                           Text File Handling
*****************************************************************************}

{$DEFINE EOF_CTRLZ}

{$i text.inc}

{****************************************************************************

                          Directory related routines.

****************************************************************************}

{*****************************************************************************
                           Directory Handling
*****************************************************************************}


procedure dosdir(func:byte;const s:string);

var buffer:array[0..255] of char;

begin
    move(s[1],buffer,length(s));
    buffer[length(s)]:=#0;
    allowslash(Pchar(@buffer));
    asm
        leal buffer,%edx
        movb func,%ah
        call syscall
        jnc  .LDOS_DIRS1
        movw %ax,inoutres
    .LDOS_DIRS1:
    end ['eax', 'edx'];
end;


procedure MkDir (const S: string);[IOCHECK];

var buffer:array[0..255] of char;
    Rc : word;

begin
  If (s='') or (InOutRes <> 0) then
   exit;
 if os_mode = osOs2 then
    begin
      move(s[1],buffer,length(s));
      buffer[length(s)]:=#0;
      allowslash(Pchar(@buffer));
      Rc := DosCreateDir(buffer,nil);
      if Rc <> 0 then
       begin
         InOutRes := Rc;
         Errno2Inoutres;
       end;
    end
  else
   begin
     { Under EMX 0.9d DOS this routine call may sometimes fail   }
     { The syscall documentation indicates clearly that this     }
     { routine was NOT tested.                                   }
        DosDir ($39, S);
end;
end;


procedure rmdir(const s : string);[IOCHECK];
var buffer:array[0..255] of char;
    Rc : word;
begin
  if (s = '.' ) then
    InOutRes := 16;
  If (s='') or (InOutRes <> 0) then
   exit;
  if os_mode = osOs2 then
    begin
      move(s[1],buffer,length(s));
      buffer[length(s)]:=#0;
      allowslash(Pchar(@buffer));
      Rc := DosDeleteDir(buffer);
      if Rc <> 0 then
       begin
         InOutRes := Rc;
         Errno2Inoutres;
       end;
    end
  else
   begin
     { Under EMX 0.9d DOS this routine call may sometimes fail   }
     { The syscall documentation indicates clearly that this     }
     { routine was NOT tested.                                   }
        DosDir ($3A, S);
end;
end;

{$ASMMODE INTEL}

procedure ChDir (const S: string);[IOCheck];

var RC: cardinal;
    Buffer: array [0..255] of char;

begin
  If (s='') or (InOutRes <> 0) then
   exit;
(* According to EMX documentation, EMX has only one current directory
   for all processes, so we'll use native calls under OS/2. *)
            if os_Mode = osOS2 then
                begin
                    if (Length (S) >= 2) and (S [2] = ':') then
                        begin
                            RC := DosSetDefaultDisk ((Ord (S [1]) and
                                                             not ($20)) - $40);
                            if RC <> 0 then
                                InOutRes := RC
                            else
                                if Length (S) > 2 then
                                    begin
                                        Move (S [1], Buffer, Length (S));
                                        Buffer [Length (S)] := #0;
                                        AllowSlash (PChar (@Buffer));
                                        RC := DosSetCurrentDir (@Buffer);
                                        if RC <> 0 then
                                         begin
                                            InOutRes := RC;
                                            Errno2InOutRes;
                                         end;
                                    end;
                        end
                    else
                        begin
                            Move (S [1], Buffer, Length (S));
                            Buffer [Length (S)] := #0;
                            AllowSlash (PChar (@Buffer));
                            RC := DosSetCurrentDir (@Buffer);
                            if RC <> 0 then
                             begin
                                  InOutRes:= RC;
                                  Errno2InOutRes;
                             end;
                        end;
                end
            else
                if (Length (S) >= 2) and (S [2] = ':') then
                    begin
                        asm
                            mov esi, S
                            mov al, [esi + 1]
                            and al, not (20h)
                            sub al, 41h
                            mov edx, eax
                            mov ah, 0Eh
                            call syscall
                            mov ah, 19h
                            call syscall
                            cmp al, dl
                            jz @LCHDIR
                            mov InOutRes, 15
@LCHDIR:
                        end ['eax','edx','esi'];
                        if (Length (S) > 2) and (InOutRes <> 0) then
                            { Under EMX 0.9d DOS this routine may sometime }
                            { fail or crash the system.                    }
                            DosDir ($3B, S);
                    end
                else
                    { Under EMX 0.9d DOS this routine may sometime }
                    { fail or crash the system.                    }
                    DosDir ($3B, S);
end;

{$ASMMODE ATT}

procedure GetDir (DriveNr: byte; var Dir: ShortString);

{Written by Michael Van Canneyt.}

var sof:Pchar;
    i:byte;

begin
    Dir [4] := #0;
    { Used in case the specified drive isn't available }
    sof:=pchar(@dir[4]);
    { dir[1..3] will contain '[drivenr]:\', but is not }
    { supplied by DOS, so we let dos string start at   }
    { dir[4]                                           }
    { Get dir from drivenr : 0=default, 1=A etc... }
    asm
        movb drivenr,%dl
        movl sof,%esi
        mov  $0x47,%ah
        call syscall
        jnc .LGetDir
        movw %ax, InOutRes
.LGetDir:
    end [ 'eax','edx','esi'];
    { Now Dir should be filled with directory in ASCIIZ, }
    { starting from dir[4]                               }
    dir[0]:=#3;
    dir[2]:=':';
    dir[3]:='\';
    i:=4;
    {Conversion to Pascal string }
    while (dir[i]<>#0) do
        begin
            { convert path name to DOS }
            if dir[i]='/' then
            dir[i]:='\';
            dir[0]:=char(i);
            inc(i);
        end;
    { upcase the string (FPC function) }
    if drivenr<>0 then   { Drive was supplied. We know it }
        dir[1]:=chr(64+drivenr)
    else
        begin
            { We need to get the current drive from DOS function 19H  }
            { because the drive was the default, which can be unknown }
            asm
                movb $0x19,%ah
                call syscall
                addb $65,%al
                movb %al,i
            end ['eax'];
            dir[1]:=char(i);
        end;
    if not (FileNameCaseSensitive) then dir:=upcase(dir);
end;


{*****************************************************************************

                        System unit initialization.

****************************************************************************}

{****************************************************************************
                    Error Message writing using messageboxes
****************************************************************************}

type
  TWinMessageBox = function (Parent, Owner: cardinal;
         BoxText, BoxTitle: PChar; Identity, Style: cardinal): cardinal; cdecl;
  TWinInitialize = function (Options: cardinal): cardinal; cdecl;
  TWinCreateMsgQueue = function (Handle: cardinal; cmsg: longint): cardinal;
                                                                         cdecl;

const
  ErrorBufferLength = 1024;
  mb_OK = $0000;
  mb_Error = $0040;
  mb_Moveable = $4000;
  MBStyle = mb_OK or mb_Error or mb_Moveable;
  WinInitialize: TWinInitialize = nil;
  WinCreateMsgQueue: TWinCreateMsgQueue = nil;
  WinMessageBox: TWinMessageBox = nil;
  EnvSize: cardinal = 0;

var
  ErrorBuf: array [0..ErrorBufferLength] of char;
  ErrorLen: longint;
  PMWinHandle: cardinal;

function ErrorWrite (var F: TextRec): integer;
{
  An error message should always end with #13#10#13#10
}
var
  P: PChar;
  I: longint;
begin
  if F.BufPos > 0 then
   begin
     if F.BufPos + ErrorLen > ErrorBufferLength then
       I := ErrorBufferLength - ErrorLen
     else
       I := F.BufPos;
     Move (F.BufPtr^, ErrorBuf [ErrorLen], I);
     Inc (ErrorLen, I);
     ErrorBuf [ErrorLen] := #0;
   end;
  if ErrorLen > 3 then
   begin
     P := @ErrorBuf [ErrorLen];
     for I := 1 to 4 do
      begin
        Dec (P);
        if not (P^ in [#10, #13]) then
          break;
      end;
   end;
   if ErrorLen = ErrorBufferLength then
     I := 4;
   if (I = 4) then
    begin
      WinMessageBox (0, 0, @ErrorBuf, PChar ('Error'), 0, MBStyle);
      ErrorLen := 0;
    end;
  F.BufPos := 0;
  ErrorWrite := 0;
end;

function ErrorClose (var F: TextRec): integer;
begin
  if ErrorLen > 0 then
   begin
     WinMessageBox (0, 0, @ErrorBuf, PChar ('Error'), 0, MBStyle);
     ErrorLen := 0;
   end;
  ErrorLen := 0;
  ErrorClose := 0;
end;

function ErrorOpen (var F: TextRec): integer;
begin
  TextRec(F).InOutFunc := @ErrorWrite;
  TextRec(F).FlushFunc := @ErrorWrite;
  TextRec(F).CloseFunc := @ErrorClose;
  ErrorOpen := 0;
end;


procedure AssignError (var T: Text);
begin
  Assign (T, '');
  TextRec (T).OpenFunc := @ErrorOpen;
  Rewrite (T);
end;


procedure DosEnvInit;
var
 Q: PPChar;
 I: cardinal;
begin
(* It's a hack, in fact - DOS stores the environment the same way as OS/2 does,
   but I don't know how to find Program Segment Prefix and thus the environment
   address under EMX, so I'm recreating this structure using EnvP pointer. *)
{$ASMMODE INTEL}
 asm
  cld
  mov ecx, EnvC
  mov esi, EnvP
  xor eax, eax
  xor edx, edx
@L1:
  xchg eax, edx
  push ecx
  mov ecx, -1
  mov edi, [esi]
  repne
  scasb
  neg ecx
  dec ecx
  xchg eax, edx
  add eax, ecx
  pop ecx
  dec ecx
  jecxz @Stop
  inc esi
  inc esi
  inc esi
  inc esi
  jmp @L1
@Stop:
  inc eax
  mov EnvSize, eax
 end ['eax','ecx','edx','esi','edi'];
 Environment := GetMem (EnvSize);
 asm
  cld
  mov ecx, EnvC
  mov edx, EnvP
  mov edi, Environment
@L2:
  mov esi, [edx]
@Copying:
  lodsb
  stosb
  or al, al
  jnz @Copying
  dec ecx
  jecxz @Stop2
  inc edx
  inc edx
  inc edx
  inc edx
  jmp @L2
@Stop2:
  stosb
 end ['eax','ecx','edx','esi','edi'];
end;


procedure SysInitStdIO;
begin
  { Setup stdin, stdout and stderr, for GUI apps redirect stderr,stdout to be
    displayed in a messagebox }
(*
  StdInputHandle := longint(GetStdHandle(cardinal(STD_INPUT_HANDLE)));
  StdOutputHandle := longint(GetStdHandle(cardinal(STD_OUTPUT_HANDLE)));
  StdErrorHandle := longint(GetStdHandle(cardinal(STD_ERROR_HANDLE)));

  if not IsConsole then
    begin
      if (DosLoadModule (nil, 0, 'PMWIN', PMWinHandle) = 0) and
       (DosQueryProcAddr (PMWinHandle, 789, nil, pointer (WinMessageBox)) = 0)
                                                                           and
       (DosQueryProcAddr (PMWinHandle, 763, nil, pointer (WinInitialize)) = 0)
                                                                           and
       (DosQueryProcAddr (PMWinHandle, 716, nil, pointer (WinCreateMsgQueue))
                                                                           = 0)
        then
          begin
            WinInitialize (0);
            WinCreateMsgQueue (0, 0);
          end
        else
          HandleError (2);
     AssignError (StdErr);
     AssignError (StdOut);
     Assign (Output, '');
     Assign (Input, '');
   end
  else
   begin
*)
     OpenStdIO (Input, fmInput, StdInputHandle);
     OpenStdIO (Output, fmOutput, StdOutputHandle);
     OpenStdIO (StdOut, fmOutput, StdOutputHandle);
     OpenStdIO (StdErr, fmOutput, StdErrorHandle);
(*
   end;
*)
end;


function GetFileHandleCount: longint;
var L1: longint;
    L2: cardinal;
begin
    L1 := 0; (* Don't change the amount, just check. *)
    if DosSetRelMaxFH (L1, L2) <> 0 then GetFileHandleCount := 50
                                                 else GetFileHandleCount := L2;
end;

var TIB: PThreadInfoBlock;
    PIB: PProcessInfoBlock;

const
 FatalHeap: array [0..33] of char = 'FATAL: Cannot initialize heap!!'#13#10'$';

begin
    IsLibrary := FALSE;
    {Determine the operating system we are running on.}
{$ASMMODE INTEL}
    asm
        push ebx
        mov os_mode, 0
        mov eax, 7F0Ah
        call syscall
        test bx, 512         {Bit 9 is OS/2 flag.}
        setne byte ptr os_mode
        test bx, 4096
        jz @noRSX
        mov os_mode, 2
    @noRSX:
    {Enable the brk area by initializing it with the initial heap size.}
        mov eax, 7F01h
        mov edx, heap_brk
        add edx, heap_base
        call syscall
        cmp eax, -1
        jnz @heapok
        lea edx, FatalHeap
        mov eax, 900h
        call syscall
        pop ebx
        push dword 204
        call HandleError
    @heapok:
{$IFDEF CONTHEAP}
{ Find out brk limit }
        mov eax, 7F02h
        mov ecx, 3
        call syscall
        jcxz @heaplimitknown
        mov eax, 0
    @heaplimitknown:
        mov BrkLimit, eax
{$ELSE CONTHEAP}
{ Change sbrk behaviour to allocate arbitrary (non-contiguous) memory blocks }
        mov eax, 7F0Fh
        mov ecx, 0Ch
        mov edx, 8
        call syscall
{$ENDIF CONTHEAP}
        pop ebx
    end ['eax', 'ecx', 'edx'];
    { in OS/2 this will always be nil, but in DOS mode }
    { this can be changed.                             }
    first_meg := nil;
    {Now request, if we are running under DOS,
     read-access to the first meg. of memory.}
    if os_mode in [osDOS,osDPMI] then
        asm
            push ebx
            mov eax, 7F13h
            xor ebx, ebx
            mov ecx, 0FFFh
            xor edx, edx
            call syscall
            jc @endmem
            mov first_meg, eax
         @endmem:
            pop ebx
        end ['eax', 'ecx', 'edx']
    else
        begin
    (* Initialize the amount of file handles *)
            FileHandleCount := GetFileHandleCount;
        end;
    {At 0.9.2, case for enumeration does not work.}
    case os_mode of
        osDOS:
            begin
                stackbottom:=pointer(heap_brk);     {In DOS mode, heap_brk is
                                                     also the stack bottom.}
                ApplicationType := 1;   (* Running under DOS. *)
                IsConsole := true;
                ProcessID := 1;
                ThreadID := 1;
            end;
        osOS2:
            begin
                DosGetInfoBlocks (@TIB, @PIB);
                StackBottom := pointer (TIB^.Stack);
                Environment := pointer (PIB^.Env);
                ApplicationType := PIB^.ProcType;
                ProcessID := PIB^.PID;
                ThreadID := TIB^.TIB2^.TID;
                IsConsole := ApplicationType <> 3;
            end;
        osDPMI:
            begin
                stackbottom:=nil;   {Not sure how to get it, but seems to be
                                     always zero.}
                ApplicationType := 1;   (* Running under DOS. *)
                IsConsole := true;
                ProcessID := 1;
                ThreadID := 1;
            end;
    end;
    exitproc:=nil;

    {Initialize the heap.}
    initheap;

    { ... and exceptions }
    SysInitExceptions;

    { ... and I/O }
    SysInitStdIO;

    { no I/O-Error }
    inoutres:=0;

{$ifdef HASVARIANT}
    initvariantmanager;
{$endif HASVARIANT}

    if os_Mode in [osDOS,osDPMI] then
        DosEnvInit;

{$IFDEF DUMPGROW}
 {$IFDEF CONTHEAP}
    WriteLn ('Initial brk size is ', GetHeapSize);
    WriteLn ('Brk limit is ', BrkLimit);
 {$ENDIF CONTHEAP}
{$ENDIF DUMPGROW}
end.
{
  $Log$
  Revision 1.28  2004-09-18 11:12:49  hajny
    * handle type changed to thandle in do_isdevice

  Revision 1.27  2004/09/03 19:25:41  olle
    + added maxExitCode to all System.pp
    * constrained error code to be below maxExitCode in RunError et. al.

  Revision 1.26  2004/07/24 01:15:25  hajny
    * simulated support for new heap manager

  Revision 1.25  2004/05/16 20:39:59  hajny
    * handle in do_* changed to THandle

  Revision 1.24  2004/04/22 21:10:56  peter
    * do_read/do_write addr argument changed to pointer

  Revision 1.23  2004/01/20 23:05:31  hajny
    * ExecuteProcess fixes, ProcessID and ThreadID added

  Revision 1.22  2003/12/26 22:20:44  hajny
    * regcall fixes

  Revision 1.21  2003/12/17 22:52:39  hajny
    * fix for stackbottom change to pointer

  Revision 1.20  2003/11/06 23:21:51  hajny
    * cardinal2pointer changes

  Revision 1.19  2003/11/01 19:25:50  hajny
    * fix of previous mistyping

  Revision 1.18  2003/10/25 22:45:37  hajny
    * file handling related fixes

  Revision 1.17  2003/10/19 12:13:41  hajny
    * UnusedHandle value made the same as with other targets

  Revision 1.16  2003/10/19 09:35:28  hajny
    * fixes from OS/2 merged to EMX

  Revision 1.15  2003/10/16 15:43:13  peter
    * THandle is platform dependent

  Revision 1.14  2003/10/12 18:07:30  hajny
    * wrong use of Intel syntax

  Revision 1.13  2003/10/12 17:59:40  hajny
    * wrong use of Intel syntax

  Revision 1.12  2003/10/12 17:52:28  hajny
    * wrong use of Intel syntax

  Revision 1.11  2003/10/12 10:45:36  hajny
    * sbrk error handling corrected

  Revision 1.10  2003/10/07 21:33:24  hajny
    * stdcall fixes and asm routines cleanup

  Revision 1.9  2003/10/04 17:53:08  hajny
    * stdcall changes merged to EMX

  Revision 1.8  2003/09/29 18:39:59  hajny
    * append fix applied to GO32v2, OS/2 and EMX

  Revision 1.7  2003/09/27 11:52:35  peter
    * sbrk returns pointer

  Revision 1.6  2003/09/24 11:13:09  yuri
  * Cosmetic changes
  * Slightly improved emx.pas

  Revision 1.5  2003/06/26 17:12:29  yuri
  * pmbidi added
  * some cosmetic changes

  Revision 1.4  2003/03/23 23:11:17  hajny
    + emx target added

  Revision 1.3  2002/12/15 22:46:29  hajny
    * First_Meg fixed + Environment initialization under Dos

  Revision 1.2  2002/11/17 22:32:05  hajny
    * type corrections (longing x cardinal)

  Revision 1.1  2002/11/17 16:22:54  hajny
    + RTL for emx target

}
