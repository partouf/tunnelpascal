{%MainUnit sysutils.pp}
{
    *********************************************************************
    Copyright (C) 1997, 1998 Gertjan Schouten

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

    System Utilities For Free Pascal
}

{

This include file is used in 3 different places for the following functions:

  Function Format (Const Fmt : AnsiString; const Args : Array of const; const FormatSettings: TFormatSettings) : AnsiString;
  Function UnicodeFormat (Const Fmt : UnicodeString; const Args : Array of const; Const FormatSettings: TFormatSettings) : UnicodeString;
  Function WideFormat (Const Fmt : WideString; const Args : Array of const; Const FormatSettings: TFormatSettings) : WideString;

The header is different, but the function remains the same.
It relies on 2 macros:

  TFormatString : one of unicodestring, widestring,ansistring
  TFormatChar : one of unicodechar, widechar or ansichar

}

type TFormatCharLocal=TFormatChar;
     PFormatChar=^TFormatChar;

Var ChPos,LiteralStart,ArgPos,Len,Rp,Ra,RpBeforeArg,Padding : SizeInt;
    DoArg : PVarRec;
    Width,Prec : Longint;
    Left,BoolVal : Boolean;
    Fchar : TFormatChar;
    OuterResult : TFormatString absolute Result;

  function Grow(By: SizeInt): PFormatChar;
  begin
    Rp:=Rp+By;
    if Rp>Ra then
      begin
        Ra:=Len+8*Length(Args); { Allocate heuristically at first. }
        if Rp>Ra then { Will usually trigger in the unlikely case of getting here a second time. }
          Ra:=8+SizeUint(Rp)+SizeUint(Rp) div 2;
        SetLength(OuterResult,Ra);
      end;
    Result:=PFormatChar(Pointer(OuterResult))+(Rp-By);
  end;

  procedure Append(S: PFormatChar; Ns: SizeInt);
  begin
    Move(S^,Grow(Ns)^,Ns*SizeOf(TFormatChar));
  end;

  procedure Append(const S: TFormatString);
  var
    Ns : SizeInt;
  begin
    Ns:=Length(S);
    Move(PFormatChar(Pointer(S))^,Grow(Ns)^,Ns*SizeOf(TFormatChar));
  end;

{$if sizeof(TFormatCharLocal)=1}
  function CompatibleWithRawCopies(Cp: TSystemCodePage): boolean;
  begin
    result:=(Cp=DefaultSystemCodePage) or (Cp=CP_ACP) or (Cp=CP_OEMCP);
  end;

  function Fallback: TFormatString;
  begin
    result:=TFormatString(UnicodeFormat(UnicodeString(Fmt), Args, FormatSettings));
  end;

  procedure Append(const S: shortstring);
  begin
    Move(S[1],Grow(Length(S))^,Length(S)*SizeOf(TFormatChar));
  end;
{$endif}

  procedure FillFormatChar(P: PFormatChar; N: SizeInt; C: TFormatChar); inline;
  begin
{$if sizeof(TFormatCharLocal)=sizeof(uint8)}
    FillChar(P^,N,uint8(C));
{$elseif sizeof(TFormatCharLocal)=sizeof(uint16)}
    FillWord(P^,N,uint16(C));
{$else}
  {$error unknown char size}
{$endif}
  end;

  procedure ThrowAtChPos(ErrCode : Longint);
  begin
    DoFormatError(ErrCode,ansistring(Copy(Fmt,1,ChPos-1))+'|'+ansistring(Copy(Fmt,ChPos,Len-ChPos+1)));
  end;

  {
    ReadFormat reads the format string. It returns the type character,
    might use, set, or advance ArgPos,
    and sets Width, Prec to their correct values, or -1 if not set.
    It sets Left to true if left alignment was requested.
    In case of an error, DoFormatError is called.
  }

  Function ReadFormat : TFormatChar;

  Var Value : longint;

    Procedure ReadInteger;

    var
      OldPos : SizeInt;
      Arg : PVarRec;
    begin
      If Value<>-1 then exit; // Was already read.
      OldPos:=ChPos;
      Value:=0;
      While (ChPos<=Len) and
            (Fmt[ChPos]<='9') and (Fmt[ChPos]>='0') do
        begin
          if Value>(High(Value)-9) div 10 then
            ThrowAtChPos(feInvalidFormat);
          Value:=Value*10+(ord(Fmt[ChPos])-ord('0'));
          inc(ChPos);
        end;
      If ChPos>len then
        ThrowAtChPos(feInvalidFormat);
      If Fmt[ChPos]='*' then
        begin

        If (ChPos>OldPos) or (ArgPos>High(Args)) then
          ThrowAtChPos(feInvalidFormat);

        Arg:=@Args[ArgPos];
        ArgPos:=ArgPos+1;

        if (Arg^.Vtype=vtInteger) and (Arg^.VInteger>=0) then
          Value := Arg^.VInteger
        else if (Arg^.Vtype=vtInt64) and (Arg^.VInt64^>=0) and (Arg^.VInt64^<=High(Value)) then
          Value := Arg^.VInt64^
        else if (Arg^.Vtype=vtQWord) and (Arg^.VQWord^<=High(Value)) then
          Value := Arg^.VQWord^
        else
          ThrowAtChPos(feInvalidFormat);
        Inc(ChPos);
        end
      else
        If (OldPos=ChPos) Then
          Value:=-1;
    end;

    Procedure ReadArgPos;

    begin
      If Fmt[ChPos]<>':' then
        ReadInteger
      else
        value:=0; // Delphi undocumented behaviour, assume 0, #11099
      If Fmt[ChPos]=':' then
        begin
        ArgPos:=Value;
        Value:=-1;
        Inc(ChPos);
        end;
{$ifdef fmtdebug}
      Log ('Read ArgPos');
{$endif}
    end;

    Procedure ReadLeft;

    begin
      If Fmt[ChPos]='-' then
        begin
        left:=True;
        Inc(ChPos);
        end
      else
        Left:=False;
{$ifdef fmtdebug}
      Log ('Read Left');
{$endif}
    end;

    Procedure ReadWidth;

    begin
      ReadInteger;
      If Value<>-1 then
        begin
        Width:=Value;
        Value:=-1;
        end;
{$ifdef fmtdebug}
      Log ('Read width');
{$endif}
    end;

    Procedure ReadPrec;

    begin
      If Fmt[ChPos]='.' then
        begin
        inc(ChPos);
          ReadInteger;
        If Value=-1 then
         Value:=0;
        prec:=Value;
        end;
{$ifdef fmtdebug}
      Log ('Read precision');
{$endif}
    end;

  begin
{$ifdef fmtdebug}
    Log ('Start format');
{$endif}
    Width:=-1;
    Prec:=-1;
    Value:=-1;
    ReadArgPos;
    ReadLeft;
    ReadWidth;
    ReadPrec;
    ReadFormat:=Fmt[ChPos];
    inc(ChPos);
{$ifdef fmtdebug}
    Log ('End format');
{$endif}
end;


{$ifdef fmtdebug}
Procedure DumpFormat (C : TFormatChar);
begin
  Write ('Fmt : ',fmt:10);
  Write (' ArgPos : ',ArgPos:3);
  Write (' Left  : ',left:5);
  Write (' Width : ',Width:3);
  Write (' Prec  : ',prec:3);
  Writeln (' Type  : ',C);
end;
{$endif}

procedure HandleInteger(value: int64; signed: boolean; MaxPrec: SizeInt);
var
  DigitsCount : SizeInt;
{$if sizeof(TFormatCharLocal)=1}
  NumStr : shortstring;
{$else}
  NumStr : TFormatString;
{$endif}
begin
  if signed then
    Str(value,NumStr)
  else
    Str(qword(value),NumStr);
  Width:=Abs(width);
  DigitsCount:=Length(NumStr)-ord(NumStr[1]='-');
  if (Prec<=DigitsCount) or (Prec>MaxPrec) then
    Append(NumStr)
  else
    begin
      if NumStr[1]='-' then
        Grow(1)^:='-';
      FillFormatChar(Grow(Prec-DigitsCount),Prec-DigitsCount,'0');
      Append(@NumStr[Length(NumStr)-DigitsCount+1],DigitsCount);
    end;
end;

{$ifndef FPUNONE}
procedure HandleFloat(ff: TFloatFormat; Precision, Digits: Integer);
begin
  case DoArg^.VType of
    vtExtended: Append(TFormatString(FloatToStrF(DoArg^.VExtended^,ff,Precision,Digits,FormatSettings)));
    vtCurrency: Append(TFormatString(FloatToStrF(DoArg^.VCurrency^,ff,Precision,Digits,FormatSettings)));
    else ThrowAtChPos(feInvalidArgindex);
  end;
end;
{$endif}

procedure HandleHex(Value: QWord; Digits: Longint);
begin
  If Prec<=Digits then
    begin
      // determine minimum needed number of hex digits.
      Digits:=1+BsrQWord(Value or 1) div 4;
      If Digits<Prec then
        Digits:=Prec;
    end;
  Append(HexStr(Value,Digits));
end;

begin
  Result:='';
  Len:=Length(Fmt);
  ChPos:=1;
  LiteralStart:=1;
  ArgPos:=0;
  Rp:=0;
  Ra:=0;
  While ChPos<=len do
    begin
    ChPos:=Pos(TFormatChar('%'),Fmt,ChPos);
    if ChPos=0 then
      begin
        if LiteralStart=1 then
          exit(Fmt); { Unchanged string. }
        Append(PFormatChar(Pointer(Fmt))+(LiteralStart-1),Len-LiteralStart+1);
        break;
      end;
{$if sizeof(TFormatCharLocal)=1}
    if (LiteralStart=1) and not CompatibleWithRawCopies(StringCodePage(Fmt)) then
      exit(Fallback); { Fmt pieces are going to be copied directly. }
{$endif}
    If ChPos>LiteralStart Then
      Append(PFormatChar(Pointer(Fmt))+(LiteralStart-1),ChPos-LiteralStart);
    inc(ChPos);
    if (ChPos>Len) or (Fmt[ChPos] = '%') then
      begin
        LiteralStart:=ChPos; { Include this % next time (will skip % at the end of the string as 'continue' will quit the loop, supposedly a feature as Delphi does the same). }
        ChPos:=LiteralStart+1;
        continue;
      end;
    FChar:=ReadFormat;
{$ifdef fmtdebug}
    DumpFormat(FCHar);
{$endif}
    RpBeforeArg:=Rp; { RpBeforeArg is the start of the piece corresponding to the argument and Rp-RpBeforeArg is its length, to pad it to the Width later. }

    { Point DoArg to the current argument, and advance ArgPos. }
    If not ((ArgPos>=0) and (ArgPos<=High(Args))) then
      ThrowAtChPos(feInvalidArgindex);
    DoArg:=@Args[ArgPos];
    ArgPos:=ArgPos+1;

    Case FChar of
      'b', 'B': begin
            case DoArg^.VType of
              vtInteger: BoolVal:=DoArg^.VInteger<>0;
              vtInt64, vtQWord: BoolVal:=DoArg^.VInt64^<>0;
              vtBoolean: BoolVal:=DoArg^.VBoolean;
              else ThrowAtChPos(feInvalidArgindex);
            end;
            Append(TFormatString(BoolToStr(BoolVal,True)));
            // Top off
            If (Prec<>-1) and (Rp-RpBeforeArg>Prec) then
              Rp:=RpBeforeArg+Prec;
            end;
      'd', 'D':
            case DoArg^.VType of
              vtInteger: HandleInteger(DoArg^.VInteger,true,16);
              vtInt64, vtQWord: HandleInteger(DoArg^.VInt64^,true,32); { qword is type punned }
              else ThrowAtChPos(feInvalidArgindex);
            end;
      'u', 'U':
            case DoArg^.VType of
              vtInteger: HandleInteger(cardinal(DoArg^.VInteger),false,16);
              vtInt64, vtQWord: HandleInteger(DoArg^.VInt64^,false,32); { qword is type punned }
              else ThrowAtChPos(feInvalidArgindex);
            end;
{$ifndef FPUNONE}
      'e', 'E' : HandleFloat(ffexponent,Prec,3);
      'f', 'F' : HandleFloat(ffFixed,9999,Prec);
      'g', 'G' : HandleFloat(ffGeneral,Prec,3);
      'n', 'N' : HandleFloat(ffNumber,9999,Prec);
      'm', 'M' : HandleFloat(ffCurrency,9999,Prec);
{$else}
      'e', 'E','f', 'F','g', 'G','n', 'N','m', 'M':
            RunError(207);
{$endif}
      's', 'S' : begin
            case DoArg^.VType of
              vtString: Append(DoArg^.VString^);
              vtChar: Grow(1)^:=DoArg^.VChar;
{$if sizeof(TFormatCharLocal)=1}
              vtPChar: Append(DoArg^.VPChar,Length(DoArg^.VPChar));
{$else}
              vtPChar: Append(TFormatString(DoArg^.VPChar));
{$endif}
{$if sizeof(TFormatCharLocal)=sizeof(widechar)}
              vtPWideChar: Append(PFormatChar(DoArg^.VPWideChar),Length(DoArg^.VPWideChar));
              vtWideChar: Grow(1)^:=TFormatChar(DoArg^.VWideChar);
              vtWidestring: Append(DoArg^.VWideString,Length(WideString(DoArg^.VWideString)));
{$else}
              vtPWideChar: Append(TFormatString(WideString(DoArg^.VPWideChar)));
              vtWideChar: Append(TFormatString(WideString(DoArg^.VWideChar)));
              vtWidestring: Append(TFormatString(WideString(DoArg^.VWideString)));
{$endif}
              vtAnsiString:
{$if sizeof(TFormatCharLocal)=1}
                if not CompatibleWithRawCopies(StringCodePage(ansistring(DoArg^.VAnsiString))) then
                  exit(Fallback) { throw away the work done and fall back... }
                else
{$endif}
                Append(TFormatString(ansistring(DoArg^.VAnsiString)));
              vtUnicodeString: Append(TFormatString(UnicodeString(DoArg^.VUnicodeString)));
              vtVariant: Append(TFormatString(DoArg^.VVariant^));
              else ThrowAtChPos(feInvalidArgindex);
            end;
            If (Prec<>-1) and (Rp-RpBeforeArg>Prec) then
              Rp:=RpBeforeArg+Prec;
            end;
      'p', 'P' : Begin
            if DoArg^.VType<>vtPointer then
              ThrowAtChPos(feInvalidArgindex);
            Append(HexStr(ptruint(DoArg^.VPointer),sizeof(Ptruint)*2));
            end;
      'x', 'X' :
            case DoArg^.VType of
              vtInteger: HandleHex(Cardinal(DoArg^.VInteger),16);
              vtQWord, vtInt64: HandleHex(DoArg^.VQWord^,32); { int64 is type punned }
              else
                ThrowAtChPos(feInvalidArgindex);
            end;
      else
        ThrowAtChPos(feInvalidFormat);
    end;
    If (Width<>-1) and (Rp-RpBeforeArg<Width) then
      begin
        Padding:=Width-(Rp-RpBeforeArg);
        if Padding>1000 then
          Padding:=1000; { Just a sane limit. }
        If Left then
          FillFormatChar(Grow(Padding),Padding,' ')
        else
          begin
            Grow(Padding);
            Move(PFormatChar(Pointer(Result))[RpBeforeArg],PFormatChar(Pointer(Result))[RpBeforeArg+Padding],Width-Padding);
            FillFormatChar(PFormatChar(Pointer(Result))+RpBeforeArg,Padding,' ');
          end;
      end;
    LiteralStart:=ChPos;
    end;
  SetLength(Result,Rp);
end;
