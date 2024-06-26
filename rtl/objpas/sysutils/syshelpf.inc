{%MainUnit sysutils.pp}
Class Function TFLOATHELPER.IsNan(const AValue: FLOATTYPE): Boolean; overload; inline; static;

begin
  Result:=TFloatRec(AValue).SpecialType=fsNan;
end;

Class Function TFLOATHELPER.IsInfinity(const AValue: FLOATTYPE): Boolean; overload; inline; static;

begin
  Result:=TFloatRec(AValue).SpecialType in [fsInf,fsNinf];
end;

Class Function TFLOATHELPER.IsNegativeInfinity(const AValue: FLOATTYPE): Boolean; overload; inline; static;

begin
  Result:=TFloatRec(AValue).SpecialType=fsNinf;
end;

Class Function TFLOATHELPER.IsPositiveInfinity(const AValue: FLOATTYPE): Boolean; overload; inline; static;

begin
  Result:=TFloatRec(AValue).SpecialType=fsInf;
end;

Class Function TFLOATHELPER.Parse(const AString: string): FLOATTYPE; overload; inline; static;

begin
  Result:=StrToFloat(AString,DefaultFormatSettings);
end;

Class Function TFLOATHELPER.Parse(const AString: string; const AFormatSettings: TFormatSettings): FLOATTYPE; overload; inline; static;

begin
  Result:=StrToFloat(AString,AFormatSettings);
end;

Class Function TFLOATHELPER.Size: Integer; inline; static;

begin
  Result:=SizeOf(FLOATTYPE);
end;

Class Function TFLOATHELPER.ToString(const AValue: FLOATTYPE): string; overload; inline; static;

begin
  Result:=FloatToStr(AValue,DefaultFormatSettings);
end;

Class Function TFLOATHELPER.ToString(const AValue: FLOATTYPE; const AFormatSettings: TFormatSettings): string; overload; inline; static;

begin
  Result:=FloatToStr(AValue,AFormatSettings);
end;

Class Function TFLOATHELPER.ToString(const AValue: FLOATTYPE; const AFormat: TFloatFormat; const APrecision, ADigits: Integer): string; overload; inline; static;

begin
  Result:=FloatToStrF(AValue,AFormat,APrecision,ADigits,DefaultFormatSettings);
end;

Class Function TFLOATHELPER.ToString(const AValue: FLOATTYPE; const AFormat: TFloatFormat; const APrecision, ADigits: Integer;  const AFormatSettings: TFormatSettings): string;
overload; inline; static;

begin
  Result:=FloatToStrF(AValue,AFormat,APrecision,ADigits,AFormatSettings);
end;

Class Function TFLOATHELPER.TryParse(const AString: string; out AValue: FLOATTYPE): Boolean; overload; inline; static;

begin
  Result:=TryStrToFloat(AString,AValue,DefaultFormatSettings);
end;

Class Function TFLOATHELPER.TryParse(const AString: string; out AValue: FLOATTYPE; const AFormatSettings: TFormatSettings): Boolean; overload; inline;
static;

begin
  Result:=TryStrToFloat(AString,AValue,AFormatSettings);
end;


Function TFLOATHELPER.GetB(AIndex: Cardinal): Byte;

begin
  if (AIndex>=SizeOf(FLOATTYPE)) then
    system.Error(reRangeError);
  Result:=PByte(@Self)[AIndex];
end;

Function TFLOATHELPER.GetW(AIndex: Cardinal): Word;

begin
  if (AIndex>=(SizeOf(FLOATTYPE) div SizeOf(Word))) then
    system.Error(reRangeError);
  Result:=PWord(@Self)[AIndex];
end;

Function TFLOATHELPER.GetE: QWord; inline;

begin
  Result:=TFloatRec(Self).Exp;
end;

Function TFLOATHELPER.GetF: QWord; inline;

begin
  Result:=TFloatRec(Self).Frac;
end;

Function TFLOATHELPER.GetS: Boolean; inline;

begin
  Result:=TFloatRec(Self).Sign;
end;

procedure TFLOATHELPER.SetB(AIndex: Cardinal; const AValue: Byte);

begin
  if (AIndex>=SizeOf(FLOATTYPE)) then
    system.Error(reRangeError);
  PByte(@Self)[AIndex]:=AValue;
end;

procedure TFLOATHELPER.SetW(AIndex: Cardinal; const AValue: Word);

begin
  if (AIndex>=(SizeOf(FLOATTYPE) div SizeOf(Word))) then
    system.Error(reRangeError);
  PWord(@Self)[AIndex]:=AValue;
end;

procedure TFLOATHELPER.SetE(AValue: QWord);

begin
  TFloatRec(Self).Exp:=AValue;
end;

procedure TFLOATHELPER.SetF(AValue: QWord);

begin
  TFloatRec(Self).Frac:=AValue;
end;

procedure TFLOATHELPER.SetS(AValue: Boolean);

begin
  TFloatRec(Self).Sign:=AValue;
end;


Procedure TFLOATHELPER.BuildUp(const ASignFlag: Boolean; const AMantissa: QWord; const AExponent: Integer);

begin
  TFloatRec(Self).BuildUp(ASignFlag, AMantissa, AExponent);
end;

Function TFLOATHELPER.Exponent: Integer;

begin
  Result:=TFloatRec(Self).Exponent;
end;

Function TFLOATHELPER.Fraction: Extended;

begin
  Result:=TFloatRec(Self).Fraction;
end;

Function TFLOATHELPER.IsInfinity: Boolean; overload; inline;

begin
  Result:=TFloatRec(Self).SpecialType in [fsInf,fsNinf];
end;

Function TFLOATHELPER.IsNan: Boolean; overload; inline;

begin
  Result:=TFloatRec(Self).SpecialType=fsNan;
end;

Function TFLOATHELPER.IsNegativeInfinity: Boolean; overload; inline;

begin
  Result:=TFloatRec(Self).SpecialType=fsNinf;
end;

Function TFLOATHELPER.IsPositiveInfinity: Boolean; overload; inline;

begin
  Result:=TFloatRec(Self).SpecialType=fsInf;
end;

Function TFLOATHELPER.Mantissa: QWord;

begin
  Result:=TFLoatRec(Self).Mantissa(True);
end;

Function TFLOATHELPER.SpecialType: TFloatSpecial;

begin
  Result:=TFLoatRec(Self).SpecialType;
end;

Function TFLOATHELPER.ToString(const AFormat: TFloatFormat; const APrecision, ADigits: Integer): string; overload; inline;

begin
  Result:=FloatToStrF(Self,AFormat,APrecision,ADigits,DefaultFormatSettings);
end;

Function TFLOATHELPER.ToString(const AFormat: TFloatFormat; const APrecision, ADigits: Integer; const AFormatSettings: TFormatSettings): string; overload; inline;

begin
  Result:=FloatToStrF(Self,AFormat,APrecision,ADigits,AFormatSettings);
end;

Function TFLOATHELPER.ToString(const AFormatSettings: TFormatSettings): string; overload; inline;

begin
  Result:=FloatToStr(Self,AFormatSettings);
end;

Function TFLOATHELPER.ToString: string; overload; inline;

begin
  Result:=FloatToStr(Self,DefaultFormatSettings);
end;
