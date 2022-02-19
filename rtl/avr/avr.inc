{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by the Free Pascal development team.

    Processor dependent implementation for the system unit for
    AVR

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$asmmode gas}

const
{$i cpuinnr.inc}

{ Reads SREG and then disables interrupts, returns contents of SREG }
function avr_save: byte;[INTERNPROC: in_avr_save];
{ Restores SREG }
procedure avr_restore(old_sreg: byte); [INTERNPROC: in_avr_restore];

{$define FPC_SYSTEM_HAS_FPC_CPUINIT}
procedure fpc_cpuinit;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
  end;


{$if not(defined(CPUAVR_16_REGS)) and defined(CPUAVR_HAS_MOVW)}
{$define FPC_SYSTEM_HAS_MOVE}
procedure Move(const source;var dest;count:SizeInt);[public, alias: 'FPC_MOVE']; assembler; nostackframe;
asm
  push r28
  push r29

  movw r26, r24         // Src=X
  movw r28, r22         // Dest=Y
  movw r30, r20         // Count=Z
  cp r1, r30
  cpc r1, r31
  brge .Lexit           // if 0 >= Count
  cp  r28, r26
  cpc r29, r27
  breq .Lexit           // if dest = source
  brlo .LForwardMove    // if dest < source

  // Add count to both pointers
  add r26, r30
  adc r27, r31
  add r28, r30
  adc r29, r31
.LBackwardMove:
  ld r18, -X
  st -Y, r18
  sbiw r30, 1
  brne .LBackwardMove
  rjmp .Lexit

.LForwardMove:
  ld r18, X+
  st Y+, r18
  sbiw r30, 1
  brne .LForwardMove
.Lexit:

  pop r29
  pop r28
end;
{$endif not(defined(CPUAVR_16_REGS)) and defined(CPUAVR_HAS_MOVW)}

{$define FPC_SYSTEM_HAS_FILLCHAR}
Procedure FillChar(var x;count:SizeInt;value:byte);
var
  pdest,pend : pbyte;
  v : ptruint;
begin
  if count <= 0 then
    exit;
  pdest:=@x;
  pend:=pdest+count;
  while pdest<pend do
    begin
      pdest^:=value;
      inc(pdest);
    end;
end;


{$IFNDEF INTERNAL_BACKTRACE}
{$define FPC_SYSTEM_HAS_GET_FRAME}
{ this is never going to work on avr properly this way, so inline and return nil so the compiler
  can optimize it }
function get_frame:pointer;inline;
  begin
    result:=nil;
  end;
{$ENDIF not INTERNAL_BACKTRACE}


{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
{ this is never going to work on avr properly this way, so inline and return nil so the compiler
  can optimize it }
function get_caller_addr(framebp:pointer;addr:pointer=nil):pointer;inline;
  begin
    result:=nil;
  end;


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
{ this is never going to work on avr properly this way, so inline and return nil so the compiler
  can optimize it }
function get_caller_frame(framebp:pointer;addr:pointer=nil):pointer;inline;
  begin
    result:=nil;
  end;


{$define FPC_SYSTEM_HAS_SPTR}
Function Sptr : pointer;assembler;nostackframe;
  asm
    in r24, 0x3d
    in r25, 0x3e
  end;


function InterLockedDecrement (var Target: longint) : longint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    temp_sreg:=avr_save();

    Result:=Target-1;
    Target:=Result;

    { release interrupts }
    avr_restore(temp_sreg);
  end;


function InterLockedIncrement (var Target: longint) : longint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    temp_sreg:=avr_save();

    Result:=Target+1;
    Target:=Result;

    { release interrupts }
    avr_restore(temp_sreg);
  end;


function InterLockedExchange (var Target: longint;Source : longint) : longint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    temp_sreg:=avr_save();

    Result:=Target;
    Target:=Source;

    { release interrupts }
    avr_restore(temp_sreg);
  end;


function InterlockedCompareExchange(var Target: longint; NewValue: longint; Comperand: longint): longint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    temp_sreg:=avr_save();

    Result:=Target;
    if Result=Comperand then
      Target:=NewValue;

    { release interrupts }
    avr_restore(temp_sreg);
  end;


function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    temp_sreg:=avr_save();

    Result:=Target;
    Target:=Result+Source;

    { release interrupts }
    avr_restore(temp_sreg);
  end;


function InterLockedDecrement (var Target: smallint) : smallint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    temp_sreg:=avr_save();

    Result:=Target-1;
    Target:=Result;

    { release interrupts }
    avr_restore(temp_sreg);
  end;


function InterLockedIncrement (var Target: smallint) : smallint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    temp_sreg:=avr_save();

    Result:=Target+1;
    Target:=Result;

    { release interrupts }
    avr_restore(temp_sreg);
  end;


function InterLockedExchange (var Target: smallint;Source : smallint) : smallint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    temp_sreg:=avr_save();

    Result:=Target;
    Target:=Source;

    { release interrupts }
    avr_restore(temp_sreg);
  end;


function InterlockedCompareExchange(var Target: smallint; NewValue: smallint; Comperand: smallint): smallint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    temp_sreg:=avr_save();

    Result:=Target;
    if Result=Comperand then
      Target:=NewValue;

    { release interrupts }
    avr_restore(temp_sreg);
  end;


function InterLockedExchangeAdd (var Target: smallint;Source : smallint) : smallint;
  var
    temp_sreg : byte;
  begin
    { block interrupts }
    temp_sreg:=avr_save();

    Result:=Target;
    Target:=Result+Source;

    { release interrupts }
    avr_restore(temp_sreg);
  end;

{$define FPC_HAS_SHORTSTR_SHORTSTR_INTERN_CHARMOVE}
procedure fpc_shortstr_shortstr_intern_charmove(const src: shortstring; const srcindex: byte; var dst: shortstring; const dstindex, len: byte); {$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    move(src[srcindex],dst[dstindex],len);
  end;

{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_CONCAT_MULTI}
procedure fpc_shortstr_concat_multi(var dests:shortstring;const sarr:array of pshortstring);compilerproc;
  var
    s2l, tmpindex : byte;
    i,
    Len : ObjpasInt;
    relocatedstringindex: byte;
    p : pshortstring;
  begin
    if high(sarr)=0 then
      begin
        DestS:='';
        exit;
      end;
    { for s := s1 + s + ..., relocate s to the correct first position in dests
      and remember this position for possible subsequent occurences of s }
    Len:=1;
    i:=low(sarr);
    while (i<=high(sarr)) and (@dests<>sarr[i]) and (Len<=high(dests)) do
      begin
        Len:=Len+length(sarr[i]^);
        inc(i);
      end;

    if Len<=high(dests) then
      begin
        relocatedstringindex:=Len;
        s2l:=length(dests);
        if uint16(s2l)+uint16(relocatedstringindex) > high(dests) then
          s2l:=high(dests)-relocatedstringindex+1;
        fpc_shortstr_shortstr_intern_charmove(dests,1,dests,relocatedstringindex,s2l);
      end;

    Len:=0;
    for i:=low(sarr) to high(sarr) do
      begin
        p:=sarr[i];
        if assigned(p) then
          begin
            s2l:=length(p^);
            if Len+s2l>high(dests) then
              s2l:=high(dests)-Len;

            { Use relocated string position if src = dests }
            if (p=@dests) then
              tmpindex:=relocatedstringindex
            else
              tmpindex:=1;

            fpc_shortstr_shortstr_intern_charmove(p^,tmpindex,dests,Len+1,s2l);
            inc(Len,s2l);
          end;
      end;
    dests[0]:=Chr(Len);
  end;

{include hand-optimized assembler code}
{$i math.inc}


{ Only define helpers for controllers without unified address space, or without EEPROM }
{$if not(defined(CPUAVRXMEGA3) or defined(CPUAVRTINY))}

procedure fpc_shortstr_to_shortstr_eeprom(var res:shortstring; constref sstr: shortstring_eeprom); compilerproc;
var
  slen, i : byte;
begin
  slen:=ord(sstr[0]);
  for i := 0 to slen do
    res[i] := sstr[i];
end;

procedure fpc_shortstr_to_shortstr_progmem(var res:shortstring; constref sstr: shortstring_progmem); compilerproc;
var
  slen, i : byte;
begin
  slen:=ord(sstr[0]);
  for i := 0 to slen do
    res[i] := sstr[i];
end;

function fpc_shortstr_copy_from_eeprom(constref s : shortstring_eeprom;index : SizeInt;count : SizeInt): shortstring; compilerproc;
var
  i: byte;
begin
  if count<0 then
   count:=0;
  if index>1 then
   dec(index)
  else
   index:=0;
  if index>ord(s[0]) then
   count:=0
  else
   if count>ord(s[0])-index then
    count:=ord(s[0])-index;

  fpc_shortstr_copy_from_eeprom[0]:=chr(count);
  for i := 1 to count do
    fpc_shortstr_copy_from_eeprom[i] := s[i+index];
end;

function fpc_shortstr_copy_from_progmem(constref s : shortstring_progmem;index : SizeInt;count : SizeInt): shortstring; compilerproc;
var
  i: byte;
begin
  if count<0 then
   count:=0;
  if index>1 then
   dec(index)
  else
   index:=0;
  if index>ord(s[0]) then
   count:=0
  else
   if count>ord(s[0])-index then
    count:=ord(s[0])-index;

  fpc_shortstr_copy_from_progmem[0]:=chr(count);
  for i := 1 to count do
    fpc_shortstr_copy_from_progmem[i] := s[i+index];
end;

procedure fpc_shortstr_concat_progmem(var dests:shortstring;const s1:shortstring; constref s2:shortstring_progmem); compilerproc;
var
  s1l, s2l, i: byte;
begin
  s1l:=length(s1);
  s2l:=length(s2);
  if s1l+s2l>high(dests) then
    begin
      if s1l>high(dests) then
        s1l:=high(dests);
      s2l:=high(dests)-s1l;
    end;
  if @dests=@s1 then
    begin
      for i := 1 to s2l do
        dests[s1l+i]:=s2[i];
    end
  else
    begin
      for i := 1 to s1l do
        dests[i]:=s1[i];

      for i := 1 to s2l do
        dests[s1l+i]:=s2[i];
    end;
  dests[0]:=chr(s1l+s2l);
end;

procedure fpc_shortstr_concat_progmem_progmem(var dests:shortstring;constref s1, s2:shortstring_progmem); compilerproc;
var
  s1l, s2l, i: byte;
begin
  s1l:=length(s1);
  s2l:=length(s2);
  if s1l+s2l>high(dests) then
    begin
      if s1l>high(dests) then
        s1l:=high(dests);
      s2l:=high(dests)-s1l;
    end;
  if @dests=@s1 then
    begin
      for i := 1 to s2l do
        dests[s1l+i]:=s2[i];
    end
  else
    begin
      for i := 1 to s1l do
        dests[i]:=s1[i];

      for i := 1 to s2l do
        dests[s1l+i]:=s2[i];
    end;
  dests[0]:=chr(s1l+s2l);
end;

procedure fpc_shortstr_concat_eeprom(var dests:shortstring;const s1:shortstring; constref s2:shortstring_eeprom); compilerproc;
var
  s1l, s2l, i: byte;
begin
  s1l:=length(s1);
  s2l:=length(s2);
  if s1l+s2l>high(dests) then
    begin
      if s1l>high(dests) then
        s1l:=high(dests);
      s2l:=high(dests)-s1l;
    end;
  if @dests=@s1 then
    begin
      for i := 1 to s2l do
        dests[s1l+i]:=s2[i];
    end
  else
    begin
      for i := 1 to s1l do
        dests[i]:=s1[i];

      for i := 1 to s2l do
        dests[s1l+i]:=s2[i];
    end;
  dests[0]:=chr(s1l+s2l);
end;

procedure fpc_shortstr_concat_eeprom_eeprom(var dests:shortstring;constref s1, s2:shortstring_eeprom); compilerproc;
var
  s1l, s2l, i: byte;
begin
  s1l:=length(s1);
  s2l:=length(s2);
  if s1l+s2l>high(dests) then
    begin
      if s1l>high(dests) then
        s1l:=high(dests);
      s2l:=high(dests)-s1l;
    end;
  if @dests=@s1 then
    begin
      for i := 1 to s2l do
        dests[s1l+i]:=s2[i];
    end
  else
    begin
      for i := 1 to s1l do
        dests[i]:=s1[i];

      for i := 1 to s2l do
        dests[s1l+i]:=s2[i];
    end;
  dests[0]:=chr(s1l+s2l);
end;

function fpc_shortstr_compare_equal_progmem(const left:shortstring; constref right: shortstring_progmem): longint; compilerproc;
var
  i: byte;
begin
  fpc_shortstr_compare_equal_progmem := byte(left[0]) - byte(right[0]);
  if fpc_shortstr_compare_equal_progmem = 0 then
    begin
      i := 0;
      while (i < length(left)) and (left[i] = right[i]) do
        inc(i);

      fpc_shortstr_compare_equal_progmem := byte(left[i]) - byte(right[i]);
    end;
end;

function fpc_shortstr_compare_equal_eeprom(const left:shortstring; constref right: shortstring_eeprom): longint; compilerproc;
var
  i: byte;
begin
  fpc_shortstr_compare_equal_eeprom := byte(left[0]) - byte(right[0]);
  if fpc_shortstr_compare_equal_eeprom = 0 then
    begin
      i := 0;
      while (i < length(left)) and (left[i] = right[i]) do
        inc(i);

      fpc_shortstr_compare_equal_eeprom := byte(left[i]) - byte(right[i]);
    end;
end;

function IndexByte_progmem(Const buf: pointer_progmem;len:SizeInt;b:byte):SizeInt;
var
  psrc,pend : pbyte_progmem;
begin
  psrc:=buf;
  { simulate assembler implementations behaviour, which is expected }
  { fpc_pchar_to_ansistr in astrings.inc                            }
  if (len < 0) or
     (psrc+len < psrc) then
    pend:=pbyte_progmem(high(PtrUInt)-sizeof(byte))
  else
    pend:=psrc+len;
  while (psrc<pend) do
    begin
      if psrc^=b then
        begin
          IndexByte_progmem:=psrc-pbyte_progmem(buf);
          exit;
        end;
      inc(psrc);
    end;
  IndexByte_progmem:=-1;
end;

function IndexByte_eeprom(Const buf: pointer_eeprom;len:SizeInt;b:byte):SizeInt;
var
  psrc,pend : pbyte_eeprom;
begin
  psrc:=buf;
  { simulate assembler implementations behaviour, which is expected }
  { fpc_pchar_to_ansistr in astrings.inc                            }
  if (len < 0) or
     (psrc+len < psrc) then
    pend:=pbyte_eeprom(high(PtrUInt)-sizeof(byte))
  else
    pend:=psrc+len;
  while (psrc<pend) do
    begin
      if psrc^=b then
        begin
          IndexByte_eeprom:=psrc-pbyte_eeprom(buf);
          exit;
        end;
      inc(psrc);
    end;
  IndexByte_eeprom:=-1;
end;

procedure fpc_chararray_to_shortstr_progmem(var res: shortstring; const arr: array of char_progmem; zerobased: boolean = true); compilerproc;
var
  l, index: ObjpasInt;
  len, i: byte;
begin
  l:=high(arr);
  if l>=high(res) then
    l:=high(res)
  else if l<0 then
    l:=0
  else
    inc(l);
  if zerobased then
    begin
      index:=IndexByte_progmem(pointer_progmem(@arr[0]),l,0);
      if index<0 then
        len:=l
      else
        len:=index;
    end
  else
    len:=l;

  //move(arr[0],res[1],len);
  for i := 1 to len do
    res[i] := arr[i-1];

  res[0]:=chr(len);
end;

procedure fpc_chararray_to_shortstr_eeprom(var res: shortstring; const arr: array of char_eeprom; zerobased: boolean); compilerproc;
var
  l, index: ObjpasInt;
  len, i: byte;
begin
  l:=high(arr);
  if l>=high(res) then
    l:=high(res)
  else if l<0 then
    l:=0
  else
    inc(l);
  if zerobased then
    begin
      index:=IndexByte_eeprom(pointer_eeprom(@arr[0]),l,0);
      if index<0 then
        len:=l
      else
        len:=index;
    end
  else
    len:=l;

  //move(arr[0],res[1],len);
  for i := 1 to len do
    res[i] := arr[i-1];

  res[0]:=chr(len);
end;

procedure fpc_pchar_to_shortstr_progmem(var res : shortstring; p: pchar_progmem); compilerproc;
var
  l : ObjpasInt;
  s: shortstring;
  i: byte;
begin
  if p=nil then
    l:=0
  else
    l:=IndexByte_progmem(p,-1,0);//strlen(p);
  if l>high(res) then
    l:=high(res);
  if l>0 then
    //move(p^,s[1],l);
    for i := 1 to l do
      s[i] := p[i-1];

  s[0]:=chr(l);
  res:=s;
end;

procedure fpc_pchar_to_shortstr_eeprom(var res : shortstring; p: pchar_eeprom); compilerproc;
var
  l : ObjpasInt;
  s: shortstring;
  i: byte;
begin
  if p=nil then
    l:=0
  else
    l:=IndexByte_eeprom(p,-1,0);//strlen(p);
  if l>high(res) then
    l:=high(res);
  if l>0 then
    //move(p^,s[1],l);
    for i := 1 to l do
      s[i] := p[i-1];

  s[0]:=chr(l);
  res:=s;
end;

procedure fpc_WriteBuffer(var f:Text;const b: pointer_progmem;len:SizeInt);
var
  p   : pchar_progmem;
  left,
  idx, i : SizeInt;
begin
  p:=pchar_progmem(b);
  idx:=0;
  left:=TextRec(f).BufSize-TextRec(f).BufPos;
  while len>left do
   begin
     //move(p[idx],TextRec(f).Bufptr^[TextRec(f).BufPos],left);
     for i := 0 to left-1 do
       TextRec(f).Bufptr^[TextRec(f).BufPos+i] := char(p[idx+i]);
     dec(len,left);
     inc(idx,left);
     inc(TextRec(f).BufPos,left);
     FileFunc(TextRec(f).InOutFunc)(TextRec(f));
     left:=TextRec(f).BufSize-TextRec(f).BufPos;
   end;
  //move(p[idx],TextRec(f).Bufptr^[TextRec(f).BufPos],len);
  for i := 0 to len-1 do
    TextRec(f).Bufptr^[TextRec(f).BufPos+i] := char(p[idx+i]);
  inc(TextRec(f).BufPos,len);
end;

procedure fpc_WriteBuffer(var f:Text;const b: pointer_eeprom;len:SizeInt);
var
  p   : pchar_eeprom;
  left,
  idx, i : SizeInt;
begin
  p:=pchar_eeprom(b);
  idx:=0;
  left:=TextRec(f).BufSize-TextRec(f).BufPos;
  while len>left do
   begin
     //move(p[idx],TextRec(f).Bufptr^[TextRec(f).BufPos],left);
     for i := 0 to left-1 do
       TextRec(f).Bufptr^[TextRec(f).BufPos+i] := char(p[idx+i]);
     dec(len,left);
     inc(idx,left);
     inc(TextRec(f).BufPos,left);
     FileFunc(TextRec(f).InOutFunc)(TextRec(f));
     left:=TextRec(f).BufSize-TextRec(f).BufPos;
   end;
  //move(p[idx],TextRec(f).Bufptr^[TextRec(f).BufPos],len);
  for i := 0 to len-1 do
    TextRec(f).Bufptr^[TextRec(f).BufPos+i] := char(p[idx+i]);
  inc(TextRec(f).BufPos,len);
end;

Procedure fpc_WriteBlanks(var f:Text;len:longint); forward;

procedure fpc_write_text_shortstr_progmem(Len : longint;var f : text;constref s : shortstring_progmem); compilerproc;
begin
  if (InOutRes<>0) then
   exit;
  case TextRec(f).mode of
    fmOutput { fmAppend gets changed to fmOutPut in do_open (JM) }:
      begin
        if Len>Length(s) then
          fpc_WriteBlanks(f,Len-Length(s));
        fpc_WriteBuffer(f,pointer_progmem(@s[1]),Length(s));
      end;
    fmInput: InOutRes:=105
    else InOutRes:=103;
  end;
end;

procedure fpc_write_text_shortstr_eeprom(Len : longint;var f : text;constref s : shortstring_eeprom); compilerproc;
begin
  if (InOutRes<>0) then
   exit;
  case TextRec(f).mode of
    fmOutput { fmAppend gets changed to fmOutPut in do_open (JM) }:
      begin
        if Len>Length(s) then
          fpc_WriteBlanks(f,Len-Length(s));
        fpc_WriteBuffer(f,pointer_eeprom(@s[1]),Length(s));
      end;
    fmInput: InOutRes:=105
    else InOutRes:=103;
  end;
end;

procedure fpc_write_text_pchar_as_array_progmem(Len : longint;var f: Text; const s: array of char_progmem; zerobased: boolean); iocheck;  compilerproc;
var
  ArrayLen : longint;
  p : pchar_progmem;
Begin
  If (InOutRes<>0) then
   exit;
  case TextRec(f).mode of
    fmOutput { fmAppend gets changed to fmOutPut in do_open (JM) }:
      begin
        p:=pchar_progmem(@s);
        if zerobased then
          begin
            { can't use StrLen, since that one could try to read past the end }
            { of the heap (JM)                                                }
            ArrayLen:=IndexByte_progmem(pbyte_progmem(p),high(s)+1,0);
            { IndexByte returns -1 if not found (JM) }
            if ArrayLen = -1 then
              ArrayLen := high(s)+1;
          end
        else
          ArrayLen := high(s)+1;
        If Len>ArrayLen Then
          fpc_WriteBlanks(f,Len-ArrayLen);
        fpc_WriteBuffer(f,p,ArrayLen);
      end;
    fmInput: InOutRes:=105
    else InOutRes:=103;
  end;
End;

procedure fpc_write_text_pchar_as_array_eeprom(Len : longint;var f: Text; const s: array of char_eeprom; zerobased: boolean); iocheck; compilerproc;
var
  ArrayLen : longint;
  p : pchar_eeprom;
Begin
  If (InOutRes<>0) then
   exit;
  case TextRec(f).mode of
    fmOutput { fmAppend gets changed to fmOutPut in do_open (JM) }:
      begin
        p:=pchar_eeprom(@s);
        if zerobased then
          begin
            { can't use StrLen, since that one could try to read past the end }
            { of the heap (JM)                                                }
            ArrayLen:=IndexByte_eeprom(pbyte_eeprom(p),high(s)+1,0);
            { IndexByte returns -1 if not found (JM) }
            if ArrayLen = -1 then
              ArrayLen := high(s)+1;
          end
        else
          ArrayLen := high(s)+1;
        If Len>ArrayLen Then
          fpc_WriteBlanks(f,Len-ArrayLen);
        fpc_WriteBuffer(f,p,ArrayLen);
      end;
    fmInput: InOutRes:=105
    else InOutRes:=103;
  end;
End;

// sstrings.inc
function fpc_shortstr_copy_progmem(constref s : shortstring_progmem;index : SizeInt;count : SizeInt): shortstring; compilerproc;
var
  i: byte;
begin
  if count<0 then
    count:=0;
  if index>1 then
    dec(index)
  else
    index:=0;
  if index>length(s) then
    count:=0
  else if count>length(s)-index then
    count:=length(s)-index;
  fpc_shortstr_copy_progmem[0]:=chr(Count);

  for i := 1 to count do
    fpc_shortstr_copy_progmem[i] := s[i+index];
end;

function fpc_shortstr_copy_eeprom(constref s : shortstring_eeprom;index : SizeInt;count : SizeInt): shortstring; compilerproc;
var
  i: byte;
begin
  if count<0 then
    count:=0;
  if index>1 then
    dec(index)
  else
    index:=0;
  if index>length(s) then
    count:=0
  else if count>length(s)-index then
    count:=length(s)-index;
  fpc_shortstr_copy_eeprom[0]:=chr(Count);

  for i := 1 to count do
    fpc_shortstr_copy_eeprom[i] := s[i+index];
end;
{$endif not CPUAVRXMEGA3 or CPUAVRTINY}
