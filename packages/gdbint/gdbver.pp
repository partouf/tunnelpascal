program find_gdb_version;

{$Linklib gdb}

uses
  strings;

var
  v5_version : array[0..0] of char;external name '_version';
  v4_version : pchar;external name '_version';
  version : pchar;
  version_number : longint;
  only_ver : boolean;

begin
  only_ver:=(Paramcount>0) and (ParamStr(1)='-n');
  getmem(version,5);
  strlcopy(version,@v5_version,4);
  if (version[0] in ['4','5','6','7','8','9']) and (version[1]='.') then
    begin
      if not only_ver then
        Writeln('GDB version is ',pchar(@v5_version));
      version_number:=ord(version[0])-ord('0');
    end
  else
    begin
      if not only_ver then
        Writeln('GDB version is ',v4_version);
      version_number:=ord(v4_version[0])-ord('0');
    end;
  freemem(version,5);
  if only_ver then
    Write(version_number);
  Halt(version_number);
end.