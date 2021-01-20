unit system;

interface

{$define FPC_IS_SYSTEM}

{$ifdef FULL_RTL}

{$I systemh.inc}

const
  LineEnding = #10;
  LFNSupport = true;
  DirectorySeparator = '/';
  DriveSeparator = '';
  ExtensionSeparator = '.';
  PathSeparator = ':';
  AllowDirectorySeparators : set of char = ['\','/'];
  AllowDriveSeparators : set of char = [];
{  FileNameCaseSensitive and FileNameCasePreserving are defined below! }
  maxExitCode = 65535;
  MaxPathLen = 4096;
  AllFilesMask = '*';

const
  UnusedHandle    = -1;
  StdInputHandle  = 0;
  StdOutputHandle = 1;
  StdErrorHandle  = 2;

  FileNameCaseSensitive : boolean = true;
  FileNameCasePreserving: boolean = true;
  CtrlZMarksEOF: boolean = false; (* #26 not considered as end of file *)

  sLineBreak = LineEnding;
  DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsLF;

{$else FULL_RTL}
type
  integer = longint;
  hresult = integer; 
  ttypekind = integer;
  filerec = integer;
  textrec = integer;
  pbyte = ^byte;
  pchar = ^Char;

procedure fpc_lib_exit; compilerproc;
{$endif FULL_RTL}

procedure DebugWrite(const P: PChar);
procedure DebugWriteLn(const P: PChar);
procedure DebugWriteChar(Ch: Char);
procedure DebugWriteHexDigit(d: Byte);
procedure DebugWriteHexByte(b: Byte);

implementation

type
  P__wasi_size_t = ^__wasi_size_t;
  __wasi_size_t = longint;
  __wasi_fd_t = longint;
  size_t = longint;
  __wasi_errno_t = longint;

  P__wasi_ciovec_t = ^__wasi_ciovec_t;
  __wasi_ciovec_t = record
    buf: pointer;
    buf_len: __wasi_size_t;
  end;

{$ifdef FULL_RTL}

{$I system.inc}

{$else FULL_RTL}
procedure fpc_lib_exit; compilerproc;
begin
end;

function StrLen(P: PChar): size_t;
var
  i: size_t;
begin
  i := 0;
  while p[i]<>#0 do
    Inc(i);
  StrLen := i;
end;
{$endif FULL_RTL}

function fd_write(fd: __wasi_fd_t;
                  iovs: P__wasi_ciovec_t;
                  iovs_len: size_t;
                  nwritten: P__wasi_size_t): __wasi_errno_t; external 'wasi_unstable';

procedure DebugWrite(const P: PChar);
var
  our_iov: __wasi_ciovec_t;
  our_nwritten: longint;
begin
  our_iov.buf := P;
  our_iov.buf_len := StrLen(P);
  fd_write(1, @our_iov, 1, @our_nwritten);
end;

procedure DebugWriteLn(const P: PChar);
begin
  DebugWrite(P);
  DebugWriteChar(#10);
end;

procedure DebugWriteChar(Ch: Char);
var
  CharArr: array [0..1] of Char;
begin
  CharArr[0] := Ch;
  CharArr[1] := #0;
  DebugWrite(@CharArr);
end;

procedure DebugWriteHexDigit(d: Byte);
const
  HexDigits: array [0..15] of Char = '0123456789ABCDEF';
begin
  DebugWriteChar(HexDigits[d]);
end;

procedure DebugWriteHexByte(b: Byte);
begin
  DebugWriteHexDigit(b shr 4);
  DebugWriteHexDigit(b and 15);
end;

end.
