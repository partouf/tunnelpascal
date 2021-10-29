{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2017 by Michael Van Canneyt

    Unit tests for Pascal-to-Javascript converter class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

 Examples:
   ./testpas2js --suite=TTestOptimizations
   ./testpas2js --suite=TTestOptimizations.TestOmitLocalVar
}
unit TCOptimizations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, testregistry, fppas2js, pastree,
  PScanner, Pas2jsUseAnalyzer, PasResolver, PasResolveEval,
  TCModules;

type

  { TCustomTestOptimizations }

  TCustomTestOptimizations = class(TCustomTestModule)
  private
    FAnalyzerModule: TPas2JSAnalyzer;
    FAnalyzerProgram: TPas2JSAnalyzer;
    FWholeProgramOptimization: boolean;
    function OnConverterIsElementUsed(Sender: TObject; El: TPasElement): boolean;
    function OnConverterIsTypeInfoUsed(Sender: TObject; El: TPasElement): boolean;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure ParseModule; override;
    procedure ParseProgram; override;
    function CreateConverter: TPasToJSConverter; override;
  public
    property AnalyzerModule: TPas2JSAnalyzer read FAnalyzerModule;
    property AnalyzerProgram: TPas2JSAnalyzer read FAnalyzerProgram;
    property WholeProgramOptimization: boolean read FWholeProgramOptimization
        write FWholeProgramOptimization;
  end;

  { TTestOptimizations }

  TTestOptimizations = class(TCustomTestOptimizations)
  published
    // unit optimization: jsshortrefglobals
    procedure TestOptShortRefGlobals_Program;
    procedure TestOptShortRefGlobals_Unit_FromIntfImpl_ToIntfImpl;
    procedure TestOptShortRefGlobals_Enums;
    procedure TestOptShortRefGlobals_Property;
    procedure TestOptShortRefGlobals_ExternalAbstract;
    procedure TestOptShortRefGlobals_Class;
    procedure TestOptShortRefGlobals_GenericFunction;
    procedure TestOptShortRefGlobals_GenericMethod_Call;
    procedure TestOptShortRefGlobals_GenericStaticMethod_Call;
    // ToDo: GenericMethod_CallInherited ObjFPC+Delphi
    procedure TestOptShortRefGlobals_GenericClassHelperMethod;
    procedure TestOptShortRefGlobals_GenericMethod_ProcVar;
    procedure TestOptShortRefGlobals_GenericStaticMethod_ProcVar;
    procedure TestOptShortRefGlobals_SameUnit_EnumType;
    procedure TestOptShortRefGlobals_SameUnit_ClassType;
    procedure TestOptShortRefGlobals_SameUnit_RecordType;
    procedure TestOptShortRefGlobals_Unit_InitNoImpl;

    // Obfuscate Identifiers
    procedure TestObfuscateLocalIdentifiers_Program; // ToDo

    // Whole Program Optimization - omit not used elements
    procedure TestWPO_OmitLocalVar;
    procedure TestWPO_OmitLocalProc;
    procedure TestWPO_OmitLocalProcForward;
    procedure TestWPO_OmitProcLocalVar;
    procedure TestWPO_OmitProcLocalConst;
    procedure TestWPO_OmitProcLocalType;
    procedure TestWPO_OmitProcLocalProc;
    procedure TestWPO_OmitProcLocalForwardProc;
    procedure TestWPO_OmitRecordMember;
    procedure TestWPO_OmitNotUsedTObject;
    procedure TestWPO_TObject;
    procedure TestWPO_Class_Property;
    procedure TestWPO_Class_OmitField;
    procedure TestWPO_Class_OmitMethod;
    procedure TestWPO_Class_OmitClassMethod;
    procedure TestWPO_Class_OmitPropertyGetter1;
    procedure TestWPO_Class_OmitPropertyGetter2;
    procedure TestWPO_Class_OmitPropertySetter1;
    procedure TestWPO_Class_OmitPropertySetter2;
    procedure TestWPO_Class_KeepNewInstance;
    procedure TestWPO_CallInherited;
    procedure TestWPO_UseUnit;
    procedure TestWPO_ArrayOfConst_Use;
    procedure TestWPO_ArrayOfConst_NotUsed;
    procedure TestWPO_Class_PropertyInOtherUnit;
    procedure TestWPO_ProgramPublicDeclaration;
    procedure TestWPO_ConstructorDefaultValueConst;
    procedure TestWPO_RTTI_PublishedField;
    procedure TestWPO_RTTI_TypeInfo;

    // TruncateIntegersOnOverflow 
    procedure TestOptTruncateIntegersOnOverflow_OperatorsByte;
    procedure TestOptTruncateIntegersOnOverflow_OperatorsShortInt;
    procedure TestOptTruncateIntegersOnOverflow_OperatorsWord;
    procedure TestOptTruncateIntegersOnOverflow_OperatorsSmallInt;
    procedure TestOptTruncateIntegersOnOverflow_OperatorsLongWord;
    procedure TestOptTruncateIntegersOnOverflow_OperatorsLongInt;
  end;

implementation

{ TCustomTestOptimizations }

function TCustomTestOptimizations.OnConverterIsElementUsed(Sender: TObject;
  El: TPasElement): boolean;
var
  A: TPas2JSAnalyzer;
begin
  if WholeProgramOptimization then
    A:=AnalyzerProgram
  else if Sender=Converter then
    A:=AnalyzerModule
  else
    begin
    {$IF defined(VerbosePas2JS) or defined(VerbosePasAnalyzer)}
    writeln('TCustomTestOptimizations.OnConverterIsElementUsed El=',GetObjName(El),' WPO=',WholeProgramOptimization,' Sender=',GetObjName(Sender));
    {$ENDIF}
    Fail('converting other unit without WPO');
    end;
  Result:=A.IsUsed(El);
  {$IF defined(VerbosePas2JS) or defined(VerbosePasAnalyzer)}
  writeln('TCustomTestOptimizations.OnConverterIsElementUsed El=',GetObjName(El),' WPO=',WholeProgramOptimization,' Result=',Result);
  {$ENDIF}
end;

function TCustomTestOptimizations.OnConverterIsTypeInfoUsed(Sender: TObject;
  El: TPasElement): boolean;
var
  A: TPas2JSAnalyzer;
begin
  if WholeProgramOptimization then
    A:=AnalyzerProgram
  else if Sender=Converter then
    A:=AnalyzerModule
  else
    begin
    {$IF defined(VerbosePas2JS) or defined(VerbosePasAnalyzer)}
    writeln('TCustomTestOptimizations.OnConverterIsTypeInfoUsed El=',GetObjName(El),' WPO=',WholeProgramOptimization,' Sender=',GetObjName(Sender));
    {$ENDIF}
    Fail('converting other unit without WPO');
    end;
  Result:=A.IsTypeInfoUsed(El);
  {$IF defined(VerbosePas2JS) or defined(VerbosePasAnalyzer)}
  writeln('TCustomTestOptimizations.OnConverterIsTypeInfoUsed El=',GetObjName(El),' WPO=',WholeProgramOptimization,' Result=',Result);
  {$ENDIF}
end;

procedure TCustomTestOptimizations.SetUp;
begin
  inherited SetUp;
  FWholeProgramOptimization:=false;
  FAnalyzerModule:=TPas2JSAnalyzer.Create;
  FAnalyzerModule.Resolver:=ResolverEngine;
  FAnalyzerProgram:=TPas2JSAnalyzer.Create;
  FAnalyzerProgram.Resolver:=ResolverEngine;
end;

procedure TCustomTestOptimizations.TearDown;
begin
  FreeAndNil(FAnalyzerProgram);
  FreeAndNil(FAnalyzerModule);
  inherited TearDown;
end;

procedure TCustomTestOptimizations.ParseModule;
begin
  inherited ParseModule;
  {$IF defined(VerbosePas2JS) or defined(VerbosePasAnalyzer)}
  writeln('TCustomTestOptimizations.ParseModule START');
  {$ENDIF}
  AnalyzerModule.AnalyzeModule(Module);
  {$IF defined(VerbosePas2JS) or defined(VerbosePasAnalyzer)}
  writeln('TCustomTestOptimizations.ParseModule END');
  {$ENDIF}
end;

procedure TCustomTestOptimizations.ParseProgram;
begin
  WholeProgramOptimization:=true;
  inherited ParseProgram;
  {$IF defined(VerbosePas2JS) or defined(VerbosePasAnalyzer)}
  writeln('TCustomTestOptimizations.ParseProgram START');
  {$ENDIF}
  AnalyzerProgram.AnalyzeWholeProgram(Module as TPasProgram);
  {$IF defined(VerbosePas2JS) or defined(VerbosePasAnalyzer)}
  writeln('TCustomTestOptimizations.ParseProgram START');
  {$ENDIF}
end;

function TCustomTestOptimizations.CreateConverter: TPasToJSConverter;
begin
  Result:=inherited CreateConverter;
  Result.OnIsElementUsed:=@OnConverterIsElementUsed;
  Result.OnIsTypeInfoUsed:=@OnConverterIsTypeInfoUsed;
end;

{ TTestOptimizations }

procedure TTestOptimizations.TestOptShortRefGlobals_Program;
begin
  AddModuleWithIntfImplSrc('UnitA.pas',
  LinesToStr([
    'type',
    '  TColor = (red,green,blue);',
    '  TColors = set of TColor;',
    'const',
    '  cRedBlue = [red,blue];',
    'type',
    '  TBird = class',
    '  public',
    '    class var c: word;',
    '    class function Run(w: word): word; virtual; abstract;',
    '  end;',
    '  TRec = record',
    '    x: word;',
    '  end;',
    'var b: TBird;',
    '']),
  LinesToStr([
    '']));

  StartProgram(true,[supTObject]);
  Add([
  '{$optimization JSShortRefGlobals}',
  'uses unita;',
  'type',
  '  TEagle = class(TBird)',
  '    class function Run(w: word = 5): word; override;',
  '  end;',
  'class function TEagle.Run(w: word): word;',
  'begin',
  'end;',
  'var',
  '  e: TEagle;',
  '  r: TRec;',
  '  c: TColors;',
  'begin',
  '  e:=TEagle.Create;',
  '  b:=TBird.Create;',
  '  e.c:=e.c+1;',
  '  r.x:=TBird.c;',
  '  r.x:=b.c;',
  '  r.x:=e.Run;',
  '  r.x:=e.Run();',
  '  r.x:=e.Run(4);',
  '  c:=cRedBlue;',
  '']);
  ConvertProgram;
  CheckSource('TestOptShortRefGlobals_Program',
    LinesToStr([
    'var $lt = null;',
    'var $lm = pas.UnitA;',
    'var $lt1 = $lm.TBird;',
    'var $lt2 = $lm.TRec;',
    'rtl.createClass(this, "TEagle", $lt1, function () {',
    '  $lt = this;',
    '  this.Run = function (w) {',
    '    var Result = 0;',
    '    return Result;',
    '  };',
    '});',
    'this.e = null;',
    'this.r = $lt2.$new();',
    'this.c = {};',
    '']),
    LinesToStr([
    '$mod.e = $lt.$create("Create");',
    '$lm.b = $lt1.$create("Create");',
    '$lt1.c = $mod.e.c + 1;',
    '$mod.r.x = $lt1.c;',
    '$mod.r.x = $lm.b.c;',
    '$mod.r.x = $mod.e.$class.Run(5);',
    '$mod.r.x = $mod.e.$class.Run(5);',
    '$mod.r.x = $mod.e.$class.Run(4);',
    '$mod.c = rtl.refSet($lm.cRedBlue);',
    '']));
end;

procedure TTestOptimizations.TestOptShortRefGlobals_Unit_FromIntfImpl_ToIntfImpl;
begin
  AddModuleWithIntfImplSrc('UnitA.pas',
  LinesToStr([
    'type',
    '  TBird = class',
    '  public Speed: word;',
    '  end;',
    '  TRecA = record',
    '    x: word;',
    '  end;',
    'var Bird: TBird;',
    '']),
  LinesToStr([
    '']));
  AddModuleWithIntfImplSrc('UnitB.pas',
  LinesToStr([
    'type',
    '  TAnt = class',
    '  public Size: word;',
    '  end;',
    '  TRecB = record',
    '    y: word;',
    '  end;',
    '  TBear = class',
    '  end;',
    '  TFrog = class',
    '  end;',
    'var Ant: TAnt;',
    '']),
  LinesToStr([
    '']));
  StartUnit(true,[supTObject]);
  Add([
  '{$optimization JSShortRefGlobals}',
  'interface',
  'uses unita;',
  'type',
  '  TEagle = class(TBird)', // intf-JS to intf-uses
  '    procedure Fly;',
  '  end;',
  'implementation',
  'uses unitb;',
  'type',
  '  TRedAnt = class(TAnt)', // impl-JS to impl-uses
  '    procedure Run;',
  '  end;',
  'procedure TEagle.Fly;',
  'begin',
  '  TRedAnt.Create;', // intf-JS to impl-JS
  '  TAnt.Create;', // intf-JS to impl-uses
  '  TBird.Create;', // intf-JS to intf-uses
  '  TEagle.Create;', // intf-JS to intf-JS
  'end;',
  'procedure TRedAnt.Run;',
  'begin',
  '  TRedAnt.Create;', // impl-JS to impl-JS
  '  TAnt.Create;', // impl-JS to impl-uses
  '  TBird.Create;', // impl-JS to intf-uses
  '  TEagle.Create;', // impl-JS to intf-JS
  '  TBear.Create', // only in impl-JS to impl-uses
  'end;',
  'var',
  '  RedAnt: TRedAnt;',
  '  Ant: TAnt;',
  '  Bird: TBird;',
  '  Eagle: TEagle;',
  'initialization',
  '  RedAnt:=TRedAnt.Create;', // init to impl-JS
  '  Ant:=TAnt.Create;', // init to impl-uses
  '  Bird:=TBird.Create;', // init to intf-uses
  '  Eagle:=TEagle.Create;', // init to intf-JS
  '  TFrog.Create;', // only in init to impl-uses
  '  Eagle.Fly;',
  '  RedAnt.Run;',
  '']);
  ConvertUnit;
  CheckSource('TestOptShortRefGlobals_Unit_FromIntfImpl_ToIntfImpl',
    LinesToStr([
    'var $impl = $mod.$impl;',
    'var $lt = null;',
    'var $lt1 = null;',
    'var $lm = pas.UnitA;',
    'var $lt2 = $lm.TBird;',
    'var $lm1 = null;',
    'var $lt3 = null;',
    'var $lt4 = null;',
    'var $lt5 = null;',
    'rtl.createClass(this, "TEagle", $lt2, function () {',
    '  $lt = this;',
    '  this.Fly = function () {',
    '    $lt1.$create("Create");',
    '    $lt3.$create("Create");',
    '    $lt2.$create("Create");',
    '    $lt.$create("Create");',
    '  };',
    '});',
    '']),
    LinesToStr([
    '$impl.RedAnt = $lt1.$create("Create");',
    '$impl.Ant = $lt3.$create("Create");',
    '$impl.Bird = $lt2.$create("Create");',
    '$impl.Eagle = $lt.$create("Create");',
    '$lt5.$create("Create");',
    '$impl.Eagle.Fly();',
    '$impl.RedAnt.Run();',
    '']),
    LinesToStr([
    '$lm1 = pas.UnitB;',
    '$lt3 = $lm1.TAnt;',
    '$lt4 = $lm1.TBear;',
    '$lt5 = $lm1.TFrog;',
    'rtl.createClass($impl, "TRedAnt", $lt3, function () {',
    '  $lt1 = this;',
    '  this.Run = function () {',
    '    $lt1.$create("Create");',
    '    $lt3.$create("Create");',
    '    $lt2.$create("Create");',
    '    $lt.$create("Create");',
    '    $lt4.$create("Create");',
    '  };',
    '});',
    '$impl.RedAnt = null;',
    '$impl.Ant = null;',
    '$impl.Bird = null;',
    '$impl.Eagle = null;',
    '']));
end;

procedure TTestOptimizations.TestOptShortRefGlobals_Enums;
begin
  AddModuleWithIntfImplSrc('UnitA.pas',
  LinesToStr([
    'type',
    '  TColor = (red,green,blue);',
    '',
    '']),
  LinesToStr([
    '']));
  AddModuleWithIntfImplSrc('UnitB.pas',
  LinesToStr([
    'type',
    '  TSize = (small,big);',
    '',
    '']),
  LinesToStr([
    '']));
  StartUnit(true,[supWriteln]);
  Add([
  '{$optimization JSShortRefGlobals}',
  'interface',
  'uses unita;',
  'const',
  '  ColorRed = TColor.Red;',
  'procedure Fly;',
  'implementation',
  'uses unitb;',
  'const',
  '  SizeSmall = TSize.Small;',
  'procedure Fly;',
  'begin',
  '  writeln(ColorRed);',
  '  writeln(TColor.Blue);',
  '  writeln(SizeSmall);',
  '  writeln(TSize.Big);',
  '  writeln(unitb.TSize.Big);',
  'end;',
  '']);
  ConvertUnit;
  CheckSource('TestOptShortRefGlobals_Enums',
    LinesToStr([
    'var $impl = $mod.$impl;',
    'var $lm = pas.UnitA;',
    'var $lt = $lm.TColor;',
    'var $lt1 = $lt.red;',
    'var $lt2 = $lt.blue;',
    'var $lm1 = null;',
    'var $lt3 = null;',
    'var $lt4 = null;',
    'var $lt5 = null;',
    'this.ColorRed = $lt1;',
    'this.Fly = function () {',
    '  console.log($lt1);',
    '  console.log($lt2);',
    '  console.log($lt4);',
    '  console.log($lt5);',
    '  console.log($lt5);',
    '};',
    '']),
    LinesToStr([
    '']),
    LinesToStr([
    '$lm1 = pas.UnitB;',
    '$lt3 = $lm1.TSize;',
    '$lt4 = $lt3.small;',
    '$lt5 = $lt3.big;',
    '$impl.SizeSmall = $lt4;',
    '']));
end;

procedure TTestOptimizations.TestOptShortRefGlobals_Property;
begin
  AddModuleWithIntfImplSrc('UnitA.pas',
  LinesToStr([
    'type',
    '  TBird = class',
    '    FWing: TObject;',
    '    class var FLeg: TObject;',
    '  public',
    '    property Wing: TObject read FWing write FWing;',
    '    class property Leg: TObject read FLeg write FLeg;',
    '  end;',
    '']),
  LinesToStr([
    '']));
  StartUnit(true,[supTObject]);
  Add([
  '{$optimization JSShortRefGlobals}',
  'interface',
  'uses unita;',
  'type',
  '  TEagle = class(TBird)', // intf-JS to intf-uses
  '    procedure Fly(o: TObject);',
  '  end;',
  'implementation',
  'procedure TEagle.Fly(o: TObject);',
  'begin',
  '  Fly(Wing);',
  '  Fly(Leg);',
  'end;',
  '']);
  ConvertUnit;
  CheckSource('TestOptShortRefGlobals_Property',
    LinesToStr([
    'var $lt = null;',
    'var $lm = pas.UnitA;',
    'var $lt1 = $lm.TBird;',
    'rtl.createClass(this, "TEagle", $lt1, function () {',
    '  $lt = this;',
    '  this.Fly = function (o) {',
    '    this.Fly(this.FWing);',
    '    this.Fly(this.FLeg);',
    '  };',
    '});',
    '']),
    LinesToStr([
    '']),
    LinesToStr([
    '']));
end;

procedure TTestOptimizations.TestOptShortRefGlobals_ExternalAbstract;
begin
  AddModuleWithIntfImplSrc('UnitA.pas',
  LinesToStr([
    'type',
    '  TBird = class',
    '    generic function FlyExt<T>(a: word = 103): T; external name ''Flying'';',
    '    class procedure JumpVirtual(a: word = 104); virtual; abstract;',
    '    class procedure RunStaticExt(a: word = 105); static; external name ''Running'';',
    '  end;',
    'procedure SayExt(a: word = 106); external name ''Saying'';',
    '']),
  LinesToStr([
    '']));
  StartUnit(true,[supTObject]);
  Add([
  '{$optimization JSShortRefGlobals}',
  'interface',
  'uses unita;',
  'type',
  '  TEagle = class(TBird)',
  '    procedure Test;',
  '  end;',
  'implementation',
  'procedure TEagle.Test;',
  'begin',
  '  specialize FlyExt<Word>;',
  '  specialize FlyExt<Word>(1);',
  '  specialize JumpVirtual;',
  '  specialize JumpVirtual(2);',
  '  specialize RunStaticExt;',
  '  specialize RunStaticExt(3);',
  '  specialize SayExt;',
  '  specialize SayExt(4);',
  '  Self.specialize FlyExt<Word>;',
  '  Self.specialize FlyExt<Word>(11);',
  '  Self.specialize JumpVirtual;',
  '  Self.specialize JumpVirtual(12);',
  '  Self.specialize RunStaticExt;',
  '  Self.specialize RunStaticExt(13);',
  '  with Self do begin',
  '    specialize FlyExt<Word>;',
  '    specialize FlyExt<Word>(21);',
  '    specialize JumpVirtual;',
  '    specialize JumpVirtual(22);',
  '    specialize RunStaticExt;',
  '    specialize RunStaticExt(23);',
  '  end;',
  'end;',
  '']);
  ConvertUnit;
  CheckSource('TestOptShortRefGlobals_ExternalAbstract',
    LinesToStr([
    'var $lt = null;',
    'var $lm = pas.UnitA;',
    'var $lt1 = $lm.TBird;',
    'rtl.createClass(this, "TEagle", $lt1, function () {',
    '  $lt = this;',
    '  this.Test = function () {',
    '    this.Flying(103);',
    '    this.Flying(1);',
    '    this.$class.JumpVirtual(104);',
    '    this.$class.JumpVirtual(2);',
    '    this.Running(105);',
    '    this.Running(3);',
    '    Saying(106);',
    '    Saying(4);',
    '    this.Flying(103);',
    '    this.Flying(11);',
    '    this.$class.JumpVirtual(104);',
    '    this.$class.JumpVirtual(12);',
    '    this.Running(105);',
    '    this.Running(13);',
    '    this.Flying(103);',
    '    this.Flying(21);',
    '    this.$class.JumpVirtual(104);',
    '    this.$class.JumpVirtual(22);',
    '    this.Running(105);',
    '    this.Running(23);',
    '  };',
    '});',
    '']),
    LinesToStr([
    '']),
    LinesToStr([
    '']));
end;

procedure TTestOptimizations.TestOptShortRefGlobals_Class;
begin
  AddModuleWithIntfImplSrc('UnitA.pas',
  LinesToStr([
    'type',
    '  TBird = class',
    '  end;',
    '']),
  LinesToStr([
    '']));
  StartUnit(true,[supTObject]);
  Add([
  '{$optimization JSShortRefGlobals}',
  'interface',
  'uses unita;',
  'type',
  '  TEagle = class(TBird)',
  '    Size: TBird;',
  '    class var Color: TBird;',
  '    procedure Fly;',
  '    class procedure Run;',
  '  end;',
  'implementation',
  'procedure TEagle.Fly;',
  'begin',
  '  Size:=Size;',
  '  Self.Size:=Self.Size;',
  '  Color:=Color;',
  '  Self.Color:=Self.Color;',
  'end;',
  'class procedure TEagle.Run;',
  'begin',
  '  Color:=Color;',
  '  Self.Color:=Self.Color;',
  'end;',
  '']);
  ConvertUnit;
  CheckSource('TestOptShortRefGlobals_Class',
    LinesToStr([
    'var $lt = null;',
    'var $lm = pas.UnitA;',
    'var $lt1 = $lm.TBird;',
    'rtl.createClass(this, "TEagle", $lt1, function () {',
    '  $lt = this;',
    '  this.Color = null;',
    '  this.$init = function () {',
    '    $lt1.$init.call(this);',
    '    this.Size = null;',
    '  };',
    '  this.$final = function () {',
    '    this.Size = undefined;',
    '    $lt1.$final.call(this);',
    '  };',
    '  this.Fly = function () {',
    '    this.Size = this.Size;',
    '    this.Size = this.Size;',
    '    $lt.Color = this.Color;',
    '    $lt.Color = this.Color;',
    '  };',
    '  this.Run = function () {',
    '    $lt.Color = this.Color;',
    '    $lt.Color = this.Color;',
    '  };',
    '});',
    '']),
    LinesToStr([
    '']),
    LinesToStr([
    '']));
end;

procedure TTestOptimizations.TestOptShortRefGlobals_GenericFunction;
begin
  AddModuleWithIntfImplSrc('UnitA.pas',
  LinesToStr([
    'generic function Run<T>(a: T): T;',
    '']),
  LinesToStr([
    'generic function Run<T>(a: T): T;',
    'begin',
    'end;',
    '']));
  StartUnit(true,[supTObject]);
  Add([
  '{$optimization JSShortRefGlobals}',
  'interface',
  'uses unita;',
  'type',
  '  TEagle = class',
  '  end;',
  'procedure Fly;',
  'implementation',
  'procedure Fly;',
  'begin',
  '  specialize Run<TEagle>(nil);',
  'end;',
  '']);
  ConvertUnit;
  CheckSource('TestOptShortRefGlobals_GenericFunction',
    LinesToStr([
    'var $lt = null;',
    'var $lm = pas.system;',
    'var $lt1 = $lm.TObject;',
    'var $lm1 = pas.UnitA;',
    'var $lp = $lm1.Run$G1;',
    'rtl.createClass(this, "TEagle", $lt1, function () {',
    '  $lt = this;',
    '});',
    'this.Fly = function () {',
    '  $lp(null);',
    '};',
    '']),
    LinesToStr([
    '']),
    LinesToStr([
    '']));
end;

procedure TTestOptimizations.TestOptShortRefGlobals_GenericMethod_Call;
begin
  AddModuleWithIntfImplSrc('UnitA.pas',
  LinesToStr([
    'type',
    '  TBird = class',
    '    generic function Fly<T>(a: word = 13): T;',
    '    generic class function Jump<T>(b: word = 14): T;',
    '  end;',
    '']),
  LinesToStr([
    'generic function TBird.Fly<T>(a: word): T;',
    'begin',
    'end;',
    'generic class function TBird.Jump<T>(b: word): T;',
    'begin',
    'end;',
    '']));
  StartUnit(true,[supTObject]);
  Add([
  '{$optimization JSShortRefGlobals}',
  'interface',
  'uses unita;',
  'type',
  '  TEagle = class(TBird)',
  '    procedure Test;',
  '    generic function Run<T>(c: word = 25): T;',
  '    generic class function Sing<T>(d: word = 26): T;',
  '  end;',
  'implementation',
  'procedure TEagle.Test;',
  'begin',
  '  specialize Run<Word>;',
  '  specialize Run<Word>(1);',
  '  specialize Sing<Word>;',
  '  specialize Sing<Word>(2);',
  '  specialize Fly<Word>;',
  '  specialize Fly<Word>(3);',
  '  specialize Jump<Word>;',
  '  specialize Jump<Word>(4);',
  '  Self.specialize Fly<Word>;',
  '  Self.specialize Fly<Word>(5);',
  '  Self.specialize Jump<Word>;',
  '  Self.specialize Jump<Word>(6);',
  '  with Self do begin',
  '    specialize Fly<Word>;',
  '    specialize Fly<Word>(7);',
  '    specialize Jump<Word>;',
  '    specialize Jump<Word>(8);',
  '  end;',
  'end;',
  'generic function TEagle.Run<T>(c: word): T;',
  'begin',
  '  specialize Fly<T>;',
  '  specialize Fly<T>(7);',
  'end;',
  'generic class function TEagle.Sing<T>(d: word): T;',
  'begin',
  '  specialize Jump<T>;',
  '  specialize Jump<T>(8);',
  'end;',
  '']);
  ConvertUnit;
  CheckSource('TestOptShortRefGlobals_GenericMethod_Call',
    LinesToStr([
    'var $lt = null;',
    'var $lp = null;',
    'var $lp1 = null;',
    'var $lm = pas.UnitA;',
    'var $lt1 = $lm.TBird;',
    'var $lp2 = $lt1.Fly$G1;',
    'var $lp3 = $lt1.Jump$G1;',
    'rtl.createClass(this, "TEagle", $lt1, function () {',
    '  $lt = this;',
    '  this.Test = function () {',
    '    $lp.apply(this, 25);',
    '    $lp.apply(this, 1);',
    '    $lp1.apply(this.$class, 26);',
    '    $lp1.apply(this.$class, 2);',
    '    $lp2.apply(this, 13);',
    '    $lp2.apply(this, 3);',
    '    $lp3.apply(this.$class, 14);',
    '    $lp3.apply(this.$class, 4);',
    '    $lp2.apply(this, 13);',
    '    $lp2.apply(this, 5);',
    '    $lp3.apply(this.$class, 14);',
    '    $lp3.apply(this, 6);',
    '    $lp2.apply(this, 13);',
    '    $lp2.apply(this, 7);',
    '    $lp3.apply(this.$class, 14);',
    '    $lp3.apply(this.$class, 8);',
    '  };',
    '  this.Run$G1 = $lp = function (c) {',
    '    var Result = 0;',
    '    $lp2.apply(this, 13);',
    '    $lp2.apply(this, 7);',
    '    return Result;',
    '  };',
    '  this.Sing$G1 = $lp1 = function (d) {',
    '    var Result = 0;',
    '    $lp3.apply(this, 14);',
    '    $lp3.apply(this, 8);',
    '    return Result;',
    '  };',
    '});',
    '']),
    LinesToStr([
    '']),
    LinesToStr([
    '']));
end;

procedure TTestOptimizations.TestOptShortRefGlobals_GenericStaticMethod_Call;
begin
  AddModuleWithIntfImplSrc('UnitA.pas',
  LinesToStr([
    'type',
    '  TBird = class',
    '    generic class function Fly<T>(a: word = 13): T; static;',
    '    class function Say(a: word = 13): word; static;',
    '  end;',
    '']),
  LinesToStr([
    'generic class function TBird.Fly<T>(a: word): T;',
    'begin',
    'end;',
    'class function TBird.Say(a: word): word;',
    'begin',
    'end;',
    '']));
  StartUnit(true,[supTObject]);
  Add([
  '{$optimization JSShortRefGlobals}',
  'interface',
  'uses unita;',
  'type',
  '  TEagle = class(TBird)',
  '    procedure Test;',
  '    generic class function Run<T>(c: word = 25): T; static;',
  '    class function Lay(c: word = 25): word; static;',
  '  end;',
  'implementation',
  'procedure TEagle.Test;',
  'begin',
  '  specialize Fly<Word>;',
  '  specialize Fly<Word>(31);',
  '  Say;',
  '  Say(32);',
  '  specialize Run<Word>;',
  '  specialize Run<Word>(33);',
  '  Lay;',
  '  Lay(34);',
  '  self.specialize Fly<Word>;',
  '  self.specialize Fly<Word>(41);',
  '  self.Say;',
  '  self.Say(42);',
  '  self.specialize Run<Word>;',
  '  self.specialize Run<Word>(43);',
  '  with Self do begin',
  '    specialize Fly<Word>;',
  '    specialize Fly<Word>(51);',
  '    Say;',
  '    Say(52);',
  '    specialize Run<Word>;',
  '    specialize Run<Word>(53);',
  '  end;',
  'end;',
  'generic class function TEagle.Run<T>(c: word): T;',
  'begin',
  'end;',
  'class function TEagle.Lay(c: word): word;',
  'begin',
  '  TEagle.specialize Fly<Word>;',
  '  TEagle.specialize Fly<Word>(61);',
  '  TEagle.Say;',
  '  TEagle.Say(62);',
  '  TEagle.specialize Run<Word>;',
  '  specialize Run<Word>(63);',
  '  Lay;',
  '  Lay(64);',
  'end;',
  '']);
  ConvertUnit;
  CheckSource('TestOptShortRefGlobals_GenericStaticMethod_Call',
    LinesToStr([
    'var $lt = null;',
    'var $lp = null;',
    'var $lm = pas.UnitA;',
    'var $lt1 = $lm.TBird;',
    'var $lp1 = $lt1.Fly$G1;',
    'var $lp2 = $lt1.Say;',
    'rtl.createClass(this, "TEagle", $lt1, function () {',
    '  $lt = this;',
    '  this.Test = function () {',
    '    $lp1(13);',
    '    $lp1(31);',
    '    $lp2(13);',
    '    $lp2(32);',
    '    $lp(25);',
    '    $lp(33);',
    '    $lt.Lay(25);',
    '    $lt.Lay(34);',
    '    $lp1(13);',
    '    $lp1(41);',
    '    $lp2(13);',
    '    $lp2(42);',
    '    $lp(25);',
    '    $lp(43);',
    '    $lp1(13);',
    '    $lp1(51);',
    '    $lp2(13);',
    '    $lp2(52);',
    '    $lp(25);',
    '    $lp(53);',
    '  };',
    '  this.Lay = function (c) {',
    '    var Result = 0;',
    '    $lp1(13);',
    '    $lp1(61);',
    '    $lp2(13);',
    '    $lp2(62);',
    '    $lp(25);',
    '    $lp(63);',
    '    $lt.Lay(25);',
    '    $lt.Lay(64);',
    '    return Result;',
    '  };',
    '  this.Run$G1 = $lp = function (c) {',
    '    var Result = 0;',
    '    return Result;',
    '  };',
    '});',
    '']),
    LinesToStr([
    '']),
    LinesToStr([
    '']));
end;

procedure TTestOptimizations.TestOptShortRefGlobals_GenericClassHelperMethod;
begin
  AddModuleWithIntfImplSrc('UnitA.pas',
  LinesToStr([
    'type',
    '  TBird = class',
    '  end;',
    '  TBirdHelper = class helper for TBird',
    '    generic function Fly<T>(a: word = 13): T;',
    '    generic class function Say<T>(a: word = 13): T;',
    '  end;',
    '']),
  LinesToStr([
    'generic function TBirdHelper.Fly<T>(a: word): T;',
    'begin',
    'end;',
    'generic class function TBirdHelper.Say<T>(a: word): T;',
    'begin',
    'end;',
    '']));
  StartUnit(true,[supTObject]);
  Add([
  '{$optimization JSShortRefGlobals}',
  'interface',
  'uses unita;',
  'type',
  '  TEagle = class(TBird)',
  '    procedure Test;',
  '    class procedure Lay;',
  '  end;',
  'implementation',
  'procedure TEagle.Test;',
  'begin',
  '  specialize Fly<Word>;',
  '  specialize Fly<Word>(31);',
  '  specialize Say<word>;',
  '  specialize Say<Word>(32);',
  '  self.specialize Fly<Word>;',
  '  self.specialize Fly<Word>(41);',
  '  self.specialize Say<Word>;',
  '  self.specialize Say<Word>(42);',
  '  with Self do begin',
  '    specialize Fly<Word>;',
  '    specialize Fly<Word>(51);',
  '    specialize Say<Word>;',
  '    specialize Say<Word>(52);',
  '  end;',
  'end;',
  'class procedure TEagle.Lay;',
  'begin',
  '  specialize Say<Word>;',
  '  specialize Say<Word>(32);',
  '  self.specialize Say<Word>;',
  '  self.specialize Say<Word>(42);',
  '  with Self do begin',
  '    specialize Say<Word>;',
  '    specialize Say<Word>(52);',
  '  end;',
  'end;',
  '']);
  ConvertUnit;
  CheckSource('TestOptShortRefGlobals_GenericClassHelperMethod',
    LinesToStr([
    'var $lt = null;',
    'var $lm = pas.UnitA;',
    'var $lt1 = $lm.TBird;',
    'var $lt2 = $lm.TBirdHelper;',
    'var $lp = $lt2.Fly$G1;',
    'var $lp1 = $lt2.Say$G1;',
    'rtl.createClass(this, "TEagle", $lt1, function () {',
    '  $lt = this;',
    '  this.Test = function () {',
    '    $lp.call(this, 13);',
    '    $lp.call(this, 31);',
    '    $lp1.call(this.$class, 13);',
    '    $lp1.call(this.$class, 32);',
    '    $lp.call(this, 13);',
    '    $lp.call(this, 41);',
    '    $lp1.call(this.$class, 13);',
    '    $lp1.call(this.$class, 42);',
    '    $lp.call(this, 13);',
    '    $lp.call(this, 51);',
    '    $lp1.call(this.$class, 13);',
    '    $lp1.call(this.$class, 52);',
    '  };',
    '  this.Lay = function () {',
    '    $lp1.call(this, 13);',
    '    $lp1.call(this, 32);',
    '    $lp1.call(this, 13);',
    '    $lp1.call(this, 42);',
    '    $lp1.call(this, 13);',
    '    $lp1.call(this, 52);',
    '  };',
    '});',
    '']),
    LinesToStr([
    '']),
    LinesToStr([
    '']));
end;

procedure TTestOptimizations.TestOptShortRefGlobals_GenericMethod_ProcVar;
begin
  AddModuleWithIntfImplSrc('UnitA.pas',
  LinesToStr([
    '{$mode delphi}',
    'type',
    '  TBird = class',
    '    function Fly<T>(a: word = 13): T;',
    '    class function Jump<T>(b: word = 14): T;',
    '  end;',
    '']),
  LinesToStr([
    'function TBird.Fly<T>(a: word): T;',
    'begin',
    'end;',
    'class function TBird.Jump<T>(b: word): T;',
    'begin',
    'end;',
    '']));
  StartUnit(true,[supTObject]);
  Add([
  '{$mode delphi}',
  '{$optimization JSShortRefGlobals}',
  'interface',
  'uses unita;',
  'type',
  '  TFunc<T> = function(a: word): T of object;',
  '  TEagle = class(TBird)',
  '    procedure Test;',
  '    function Run<T>(c: word = 25): T;',
  '    class function Sing<T>(d: word = 26): T;',
  '  end;',
  'implementation',
  'procedure TEagle.Test;',
  'var f: TFunc<word>;',
  'begin',
  '  f:=@Run<Word>;',
  '  f:=@Sing<Word>;',
  '  f:=@Fly<Word>;',
  '  f:=@Jump<Word>;',
  '  f:=@Self.Fly<Word>;',
  '  f:=@Self.Jump<Word>;',
  '  with Self do begin',
  '    f:=@Fly<Word>;',
  '    f:=@Jump<Word>;',
  '  end;',
  'end;',
  'function TEagle.Run<T>(c: word): T;',
  'begin',
  'end;',
  'class function TEagle.Sing<T>(d: word): T;',
  'var f: TFunc<T>;',
  'begin',
  '  f:=@Jump<T>;',
  'end;',
  '']);
  ConvertUnit;
  CheckSource('TestOptShortRefGlobals_GenericMethod_ProcVar',
    LinesToStr([
    'var $lt = null;',
    'var $lp = null;',
    'var $lp1 = null;',
    'var $lm = pas.UnitA;',
    'var $lt1 = $lm.TBird;',
    'var $lp2 = $lt1.Fly$G1;',
    'var $lp3 = $lt1.Jump$G1;',
    'rtl.createClass(this, "TEagle", $lt1, function () {',
    '  $lt = this;',
    '  this.Test = function () {',
    '    var f = null;',
    '    f = rtl.createCallback(this, $lp);',
    '    f = rtl.createCallback(this.$class, $lp1);',
    '    f = rtl.createCallback(this, $lp2);',
    '    f = rtl.createCallback(this.$class, $lp3);',
    '    f = rtl.createCallback(this, $lp2);',
    '    f = rtl.createCallback(this.$class, $lp3);',
    '    f = rtl.createCallback(this, $lp2);',
    '    f = rtl.createCallback(this.$class, $lp3);',
    '  };',
    '  this.Run$G1 = $lp = function (c) {',
    '    var Result = 0;',
    '    return Result;',
    '  };',
    '  this.Sing$G1 = $lp1 = function (d) {',
    '    var Result = 0;',
    '    var f = null;',
    '    f = rtl.createCallback(this, $lp3);',
    '    return Result;',
    '  };',
    '});',
    '']),
    LinesToStr([
    '']),
    LinesToStr([
    '']));
end;

procedure TTestOptimizations.TestOptShortRefGlobals_GenericStaticMethod_ProcVar;
begin
  AddModuleWithIntfImplSrc('UnitA.pas',
  LinesToStr([
    'type',
    '  TBird = class',
    '    generic class function Fly<T>(a: word = 13): T; static;',
    '    class function Say(a: word = 13): word; static;',
    '  end;',
    '']),
  LinesToStr([
    'generic class function TBird.Fly<T>(a: word): T;',
    'begin',
    'end;',
    'class function TBird.Say(a: word): word;',
    'begin',
    'end;',
    '']));
  StartUnit(true,[supTObject]);
  Add([
  '{$optimization JSShortRefGlobals}',
  'interface',
  'uses unita;',
  'type',
  '  TFunc = function(a: word): word;',
  '  TEagle = class(TBird)',
  '    procedure Test;',
  '    generic class function Run<T>(c: word = 25): T; static;',
  '    class function Lay(c: word = 25): word; static;',
  '  end;',
  'implementation',
  'procedure TEagle.Test;',
  'var f: TFunc;',
  'begin',
  '  F:=@specialize Fly<Word>;',
  '  F:=@Say;',
  '  F:=@specialize Run<Word>;',
  '  F:=@Lay;',
  '  F:=@self.specialize Fly<Word>;',
  '  F:=@self.Say;',
  '  F:=@self.specialize Run<Word>;',
  '  with Self do begin',
  '    F:=@specialize Fly<Word>;',
  '    F:=@Say;',
  '    F:=@specialize Run<Word>;',
  '  end;',
  'end;',
  'generic class function TEagle.Run<T>(c: word): T;',
  'begin',
  'end;',
  'class function TEagle.Lay(c: word): word;',
  'var f: TFunc;',
  'begin',
  '  f:=@TEagle.specialize Fly<Word>;',
  '  f:=@TEagle.Say;',
  '  f:=@TEagle.specialize Run<Word>;',
  '  f:=@Lay;',
  'end;',
  '']);
  ConvertUnit;
  CheckSource('TestOptShortRefGlobals_GenericStaticMethod_ProcVar',
    LinesToStr([
    'var $lt = null;',
    'var $lp = null;',
    'var $lm = pas.UnitA;',
    'var $lt1 = $lm.TBird;',
    'var $lp1 = $lt1.Fly$G1;',
    'var $lp2 = $lt1.Say;',
    'rtl.createClass(this, "TEagle", $lt1, function () {',
    '  $lt = this;',
    '  this.Test = function () {',
    '    var f = null;',
    '    f = $lp1;',
    '    f = $lp2;',
    '    f = $lp;',
    '    f = $lt.Lay;',
    '    f = $lp1;',
    '    f = $lp2;',
    '    f = $lp;',
    '    f = $lp1;',
    '    f = $lp2;',
    '    f = $lp;',
    '  };',
    '  this.Lay = function (c) {',
    '    var Result = 0;',
    '    var f = null;',
    '    f = $lp1;',
    '    f = $lp2;',
    '    f = $lp;',
    '    f = $lt.Lay;',
    '    return Result;',
    '  };',
    '  this.Run$G1 = $lp = function (c) {',
    '    var Result = 0;',
    '    return Result;',
    '  };',
    '});',
    '']),
    LinesToStr([
    '']),
    LinesToStr([
    '']));
end;

procedure TTestOptimizations.TestOptShortRefGlobals_SameUnit_EnumType;
begin
  StartUnit(true,[supTObject]);
  Add([
  '{$optimization JSShortRefGlobals}',
  'interface',
  'type',
  '  TBird = class',
  '  type',
  '    TFlag = (big,small);',
  '    procedure Fly;',
  '  end;',
  '  TEnum = (red,blue);',
  'var',
  '  e: TEnum;',
  '  f: TBird.TFlag;',
  'procedure Run;',
  'implementation',
  'procedure TBird.Fly;',
  'begin',
  '  e:=blue;',
  '  f:=small;',
  'end;',
  'procedure Run;',
  'type TSub = (left,right);',
  'var s: TSub;',
  'begin',
  '  e:=red;',
  '  s:=right;',
  '  f:=big;',
  'end;',
  '']);
  ConvertUnit;
  CheckSource('TestOptShortRefGlobals_SameUnit_EnumType',
    LinesToStr([
    'var $lt = null;',
    'var $lt1 = null;',
    'var $lt2 = null;',
    'var $lm = pas.system;',
    'var $lt3 = $lm.TObject;',
    'rtl.createClass(this, "TBird", $lt3, function () {',
    '  $lt = this;',
    '  $lt1 = this.TFlag = {',
    '    "0": "big",',
    '    big: 0,',
    '    "1": "small",',
    '    small: 1',
    '  };',
    '  this.Fly = function () {',
    '    $mod.e = $lt2.blue;',
    '    $mod.f = $lt1.small;',
    '  };',
    '});',
    '$lt2 = this.TEnum = {',
    '  "0": "red",',
    '  red: 0,',
    '  "1": "blue",',
    '  blue: 1',
    '};',
    'this.e = 0;',
    'this.f = 0;',
    'var TSub = {',
    '  "0": "left",',
    '  left: 0,',
    '  "1": "right",',
    '  right: 1',
    '};',
    'this.Run = function () {',
    '  var s = 0;',
    '  $mod.e = $lt2.red;',
    '  s = TSub.right;',
    '  $mod.f = $lt1.big;',
    '};',
    '']),
    LinesToStr([
    '']),
    LinesToStr([
    '']));
end;

procedure TTestOptimizations.TestOptShortRefGlobals_SameUnit_ClassType;
begin
  WithTypeInfo:=true;
  StartUnit(true,[supTObject]);
  Add([
  '{$optimization JSShortRefGlobals}',
  'interface',
  'type',
  '  TBird = class;',
  '  TAnt = class',
  '  type',
  '    TLeg = class',
  '    end;',
  '    procedure Run;',
  '  published',
  '    Bird: TBird;',
  '  end;',
  '  TBird = class',
  '    procedure Fly;',
  '  end;',
  'implementation',
  'type',
  '  TFrog = class',
  '  end;',
  'procedure TAnt.Run;',
  'begin',
  '  if typeinfo(TBird)=nil then;',
  '  Bird:=TBird.Create;',
  '  TLeg.Create;',
  '  TFrog.Create;',
  'end;',
  'procedure TBird.Fly;',
  'begin',
  '  if typeinfo(TAnt)=nil then;',
  'end;',
  '']);
  ConvertUnit;
  CheckSource('TestOptShortRefGlobals_SameUnit_ClassType',
    LinesToStr([
    'var $impl = $mod.$impl;',
    'var $lt = null;',
    'var $lt1 = null;',
    'var $lt2 = null;',
    'var $lt3 = null;',
    'var $lm = pas.system;',
    'var $lt4 = $lm.TObject;',
    'this.$rtti.$Class("TBird");',
    'rtl.createClass(this, "TAnt", $lt4, function () {',
    '  $lt = this;',
    '  rtl.createClass(this, "TLeg", $lt4, function () {',
    '    $lt1 = this;',
    '  }, "TAnt.TLeg");',
    '  this.$init = function () {',
    '    $lt4.$init.call(this);',
    '    this.Bird = null;',
    '  };',
    '  this.$final = function () {',
    '    this.Bird = undefined;',
    '    $lt4.$final.call(this);',
    '  };',
    '  this.Run = function () {',
    '    if ($mod.$rtti["TBird"] === null) ;',
    '    this.Bird = $lt2.$create("Create");',
    '    $lt1.$create("Create");',
    '    $lt3.$create("Create");',
    '  };',
    '  var $r = this.$rtti;',
    '  $r.addField("Bird", $mod.$rtti["TBird"]);',
    '});',
    'rtl.createClass(this, "TBird", $lt4, function () {',
    '  $lt2 = this;',
    '  this.Fly = function () {',
    '    if ($mod.$rtti["TAnt"] === null) ;',
    '  };',
    '});',
    '']),
    LinesToStr([
    '']),
    LinesToStr([
    'rtl.createClass($impl, "TFrog", $lt4, function () {',
    '  $lt3 = this;',
    '});',
    '']));
end;

procedure TTestOptimizations.TestOptShortRefGlobals_SameUnit_RecordType;
begin
  StartUnit(true,[supTObject]);
  Add([
  '{$optimization JSShortRefGlobals}',
  '{$modeswitch advancedrecords}',
  'interface',
  'type',
  '  TAnt = record',
  '  type',
  '    TLeg = record',
  '      l: word;',
  '    end;',
  '    procedure Run;',
  '    Leg: TLeg;',
  '  end;',
  'implementation',
  'type',
  '  TBird = record',
  '    b: word;',
  '  end;',
  'procedure TAnt.Run;',
  'type',
  '  TFoot = record',
  '    f: word;',
  '  end;',
  'var',
  '  b: TBird;',
  '  l: TLeg;',
  '  a: TAnt;',
  '  f: TFoot;',
  'begin',
  '  b.b:=1;',
  '  l.l:=2;',
  '  a.Leg.l:=3;',
  '  f.f:=4;',
  'end;',
  '']);
  ConvertUnit;
  CheckSource('TestOptShortRefGlobals_SameUnit_RecordType',
    LinesToStr([
    'var $impl = $mod.$impl;',
    'var $lt = null;',
    'var $lt1 = null;',
    'var $lt2 = null;',
    'rtl.recNewT(this, "TAnt", function () {',
    '  $lt = this;',
    '  rtl.recNewT(this, "TLeg", function () {',
    '    $lt1 = this;',
    '    this.l = 0;',
    '    this.$eq = function (b) {',
    '      return this.l === b.l;',
    '    };',
    '    this.$assign = function (s) {',
    '      this.l = s.l;',
    '      return this;',
    '    };',
    '  });',
    '  this.$new = function () {',
    '    var r = Object.create(this);',
    '    r.Leg = $lt1.$new();',
    '    return r;',
    '  };',
    '  this.$eq = function (b) {',
    '    return this.Leg.$eq(b.Leg);',
    '  };',
    '  this.$assign = function (s) {',
    '    this.Leg.$assign(s.Leg);',
    '    return this;',
    '  };',
    '  var TFoot = rtl.recNewT(null, "", function () {',
    '    this.f = 0;',
    '    this.$eq = function (b) {',
    '      return this.f === b.f;',
    '    };',
    '    this.$assign = function (s) {',
    '      this.f = s.f;',
    '      return this;',
    '    };',
    '  });',
    '  this.Run = function () {',
    '    var b = $lt2.$new();',
    '    var l = $lt1.$new();',
    '    var a = $lt.$new();',
    '    var f = TFoot.$new();',
    '    b.b = 1;',
    '    l.l = 2;',
    '    a.Leg.l = 3;',
    '    f.f = 4;',
    '  };',
    '}, true);',
    '']),
    LinesToStr([
    '']),
    LinesToStr([
    'rtl.recNewT($impl, "TBird", function () {',
    '  $lt2 = this;',
    '  this.b = 0;',
    '  this.$eq = function (b) {',
    '    return this.b === b.b;',
    '  };',
    '  this.$assign = function (s) {',
    '    this.b = s.b;',
    '    return this;',
    '  };',
    '});',
    '']));
end;

procedure TTestOptimizations.TestOptShortRefGlobals_Unit_InitNoImpl;
begin
  AddModuleWithIntfImplSrc('UnitA.pas',
  LinesToStr([
    'var a: word;',
    'procedure Run(w: word);',
    '']),
  LinesToStr([
    'procedure Run(w: word);',
    'begin',
    'end;',
    '']));
  StartUnit(true,[supTObject]);
  Add([
  '{$optimization JSShortRefGlobals}',
  'interface',
  'implementation',
  'uses UnitA;', // empty implementation function
  'begin',
  '  Run(a);',
  '']);
  ConvertUnit;
  CheckSource('TestOptShortRefGlobals_Unit_InitNoImpl',
    LinesToStr([
    'var $impl = $mod.$impl;',
    'var $lm = null;',
    'var $lp = null;',
    '']),
    LinesToStr([
    '$lp($lm.a);',
    '']),
    LinesToStr([
    '$lm = pas.UnitA;',
    '$lp = $lm.Run;',
    '']));
end;

procedure TTestOptimizations.TestObfuscateLocalIdentifiers_Program;
begin
  exit;

  StartProgram(true,[supTObject]);
  Add([
  '{$optimization JSObfuscateLocalIdentifiers}',
  'uses unita;',
  'type',
  '  TEagle = class(TBird)',
  '    class function Run(w: word = 5): word; override;',
  '  end;',
  'class function TEagle.Run(w: word): word;',
  'begin',
  'end;',
  'var',
  '  e: TEagle;',
  '  r: TRec;',
  '  c: TColors;',
  'begin',
  '  e:=TEagle.Create;',
  '  b:=TBird.Create;',
  '  e.c:=e.c+1;',
  '  r.x:=TBird.c;',
  '  r.x:=b.c;',
  '  r.x:=e.Run;',
  '  r.x:=e.Run();',
  '  r.x:=e.Run(4);',
  '  c:=cRedBlue;',
  '']);
  ConvertProgram;
  CheckSource('TestOptShortRefGlobals_Program',
    LinesToStr([
    'var $lt = null;',
    'var $lm = pas.UnitA;',
    'var $lt1 = $lm.TBird;',
    'var $lt2 = $lm.TRec;',
    'rtl.createClass(this, "TEagle", $lt1, function () {',
    '  $lt = this;',
    '  this.Run = function (w) {',
    '    var Result = 0;',
    '    return Result;',
    '  };',
    '});',
    'this.e = null;',
    'this.r = $lt2.$new();',
    'this.c = {};',
    '']),
    LinesToStr([
    '$mod.e = $lt.$create("Create");',
    '$lm.b = $lt1.$create("Create");',
    '$lt1.c = $mod.e.c + 1;',
    '$mod.r.x = $lt1.c;',
    '$mod.r.x = $lm.b.c;',
    '$mod.r.x = $mod.e.$class.Run(5);',
    '$mod.r.x = $mod.e.$class.Run(5);',
    '$mod.r.x = $mod.e.$class.Run(4);',
    '$mod.c = rtl.refSet($lm.cRedBlue);',
    '']));
end;

procedure TTestOptimizations.TestWPO_OmitLocalVar;
begin
  StartProgram(false);
  Add('var');
  Add('  a: longint;');
  Add('  b: longint;');
  Add('begin');
  Add('  b:=3;');
  ConvertProgram;
  CheckSource('TestWPO_OmitLocalVar',
    'this.b = 0;',
    '$mod.b = 3;');
end;

procedure TTestOptimizations.TestWPO_OmitLocalProc;
begin
  StartProgram(false);
  Add('procedure DoIt; begin end;');
  Add('procedure NoIt; begin end;');
  Add('begin');
  Add('  DoIt;');
  ConvertProgram;
  CheckSource('TestWPO_OmitLocalProc',
    LinesToStr([
    'this.DoIt = function () {',
    '};',
    '']),
    LinesToStr([
    '$mod.DoIt();',
    '']));
end;

procedure TTestOptimizations.TestWPO_OmitLocalProcForward;
begin
  StartProgram(false);
  Add('procedure DoIt; forward;');
  Add('procedure NoIt; forward;');
  Add('procedure DoIt; begin end;');
  Add('procedure NoIt; begin end;');
  Add('begin');
  Add('  DoIt;');
  ConvertProgram;
  CheckSource('TestWPO_OmitLocalProcForward',
    LinesToStr([
    'this.DoIt = function () {',
    '};',
    '']),
    LinesToStr([
    '$mod.DoIt();',
    '']));
end;

procedure TTestOptimizations.TestWPO_OmitProcLocalVar;
begin
  StartProgram(false);
  Add('function DoIt: longint;');
  Add('var');
  Add('  a: longint;');
  Add('  b: longint;');
  Add('begin');
  Add('  b:=3;');
  Add('  Result:=b;');
  Add('end;');
  Add('begin');
  Add('  DoIt;');
  ConvertProgram;
  CheckSource('TestWPO_OmitProcLocalVar',
    LinesToStr([
    'this.DoIt = function () {',
    '  var Result = 0;',
    '  var b = 0;',
    '  b = 3;',
    '  Result = b;',
    '  return Result;',
    '};',
    '']),
    LinesToStr([
    '$mod.DoIt();',
    '']));
end;

procedure TTestOptimizations.TestWPO_OmitProcLocalConst;
begin
  StartProgram(false);
  Add('function DoIt: longint;');
  Add('const');
  Add('  a = 3;');
  Add('  b = 4;');
  Add('  c: longint = 5;');
  Add('  d: longint = 6;');
  Add('begin');
  Add('  Result:=b+d;');
  Add('end;');
  Add('begin');
  Add('  DoIt;');
  ConvertProgram;
  CheckSource('TestWPO_OmitProcLocalConst',
    LinesToStr([
    'var b = 4;',
    'var d = 6;',
    'this.DoIt = function () {',
    '  var Result = 0;',
    '  Result = 4 + d;',
    '  return Result;',
    '};',
    '']),
    LinesToStr([
    '$mod.DoIt();',
    '']));
end;

procedure TTestOptimizations.TestWPO_OmitProcLocalType;
begin
  StartProgram(false);
  Add('function DoIt: longint;');
  Add('type');
  Add('  TEnum = (red, green);');
  Add('  TEnums = set of TEnum;');
  Add('begin');
  Add('  Result:=3;');
  Add('end;');
  Add('begin');
  Add('  DoIt;');
  ConvertProgram;
  CheckSource('TestWPO_OmitProcLocalType',
    LinesToStr([
    'this.DoIt = function () {',
    '  var Result = 0;',
    '  Result = 3;',
    '  return Result;',
    '};',
    '']),
    LinesToStr([
    '$mod.DoIt();',
    '']));
end;

procedure TTestOptimizations.TestWPO_OmitProcLocalProc;
begin
  StartProgram(false);
  Add('procedure DoIt;');
  Add('  procedure SubProcA; begin end;');
  Add('  procedure SubProcB; begin end;');
  Add('begin');
  Add('  SubProcB;');
  Add('end;');
  Add('begin');
  Add('  DoIt;');
  ConvertProgram;
  CheckSource('TestWPO_OmitProcLocalProc',
    LinesToStr([
    'this.DoIt = function () {',
    '  function SubProcB() {',
    '  };',
    '  SubProcB();',
    '};',
    '']),
    LinesToStr([
    '$mod.DoIt();',
    '']));
end;

procedure TTestOptimizations.TestWPO_OmitProcLocalForwardProc;
begin
  StartProgram(false);
  Add('procedure DoIt;');
  Add('  procedure SubProcA; forward;');
  Add('  procedure SubProcB; forward;');
  Add('  procedure SubProcA; begin end;');
  Add('  procedure SubProcB; begin end;');
  Add('begin');
  Add('  SubProcB;');
  Add('end;');
  Add('begin');
  Add('  DoIt;');
  ConvertProgram;
  CheckSource('TestWPO_OmitProcLocalForwardProc',
    LinesToStr([
    'this.DoIt = function () {',
    '  function SubProcB() {',
    '  };',
    '  SubProcB();',
    '};',
    '']),
    LinesToStr([
    '$mod.DoIt();',
    '']));
end;

procedure TTestOptimizations.TestWPO_OmitRecordMember;
begin
  StartProgram(false);
  Add('type');
  Add('  TRec = record');
  Add('    a: longint;');
  Add('    b: longint;');
  Add('  end;');
  Add('var r: TRec;');
  Add('begin');
  Add('  r.a:=3;');
  ConvertProgram;
  CheckSource('TestWPO_OmitRecordMember',
    LinesToStr([
    'rtl.recNewT(this, "TRec", function () {',
    '  this.a = 0;',
    '  this.$eq = function (b) {',
    '    return this.a === b.a;',
    '  };',
    '  this.$assign = function (s) {',
    '    this.a = s.a;',
    '    return this;',
    '  };',
    '});',
    'this.r = this.TRec.$new();',
    '']),
    LinesToStr([
    '$mod.r.a = 3;',
    '']));
end;

procedure TTestOptimizations.TestWPO_OmitNotUsedTObject;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class end;');
  Add('var o: TObject;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestWPO_OmitNotUsedTObject',
    LinesToStr([
    '']),
    LinesToStr([
    '']));
end;

procedure TTestOptimizations.TestWPO_TObject;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure AfterConstruction; virtual;');
  Add('    procedure BeforeDestruction; virtual;');
  Add('  end;');
  Add('procedure TObject.AfterConstruction; begin end;');
  Add('procedure TObject.BeforeDestruction; begin end;');
  Add('var o: TObject;');
  Add('begin');
  Add('  o:=nil;');
  ConvertProgram;
  CheckSource('TestWPO_TObject',
    LinesToStr([
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.AfterConstruction = function () {',
    '  };',
    '  this.BeforeDestruction = function () {',
    '  };',
    '});',
    'this.o = null;',
    '']),
    LinesToStr([
    '$mod.o = null;']));
end;

procedure TTestOptimizations.TestWPO_Class_Property;
begin
  StartProgram(false);
  Add([
  'type',
  '  TObject = class',
  '  private',
  '    const CA = 3;',
  '  private',
  '    FA: longint;',
  '    function GetA: longint;',
  '    procedure SetA(Value: longint);',
  '    function IsStoredA: boolean;',
  '    property A: longint read GetA write SetA stored IsStoredA default CA;',
  '  end;',
  'function tobject.geta: longint; begin end;',
  'procedure tobject.seta(value: longint); begin end;',
  'function tobject.isstoreda: boolean; begin end;',
  'var o: TObject;',
  'begin',
  '  o.A:=o.A;']);
  ConvertProgram;
  CheckSource('TestWPO_Class_TObject',
    LinesToStr([
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.GetA = function () {',
    '    var Result = 0;',
    '    return Result;',
    '  };',
    '  this.SetA = function (Value) {',
    '  };',
    '});',
    'this.o = null;',
    '']),
    LinesToStr([
    '$mod.o.SetA($mod.o.GetA());']));
end;

procedure TTestOptimizations.TestWPO_Class_OmitField;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    a: longint;');
  Add('    b: longint;');
  Add('  end;');
  Add('var o: TObject;');
  Add('begin');
  Add('  o.a:=3;');
  ConvertProgram;
  CheckSource('TestWPO_OmitClassField',
    LinesToStr([
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '    this.a = 0;',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'this.o = null;',
    '']),
    LinesToStr([
    '$mod.o.a = 3;']));
end;

procedure TTestOptimizations.TestWPO_Class_OmitMethod;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure ProcA;');
  Add('    procedure ProcB;');
  Add('  end;');
  Add('procedure TObject.ProcA; begin end;');
  Add('procedure TObject.ProcB; begin end;');
  Add('var o: TObject;');
  Add('begin');
  Add('  o.ProcB;');
  ConvertProgram;
  CheckSource('TestWPO_OmitClassMethod',
    LinesToStr([
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.ProcB = function () {',
    '  };',
    '});',
    'this.o = null;',
    '']),
    LinesToStr([
    '$mod.o.ProcB();']));
end;

procedure TTestOptimizations.TestWPO_Class_OmitClassMethod;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    class procedure ProcA;');
  Add('    class procedure ProcB;');
  Add('  end;');
  Add('class procedure TObject.ProcA; begin end;');
  Add('class procedure TObject.ProcB; begin end;');
  Add('var o: TObject;');
  Add('begin');
  Add('  o.ProcB;');
  ConvertProgram;
  CheckSource('TestWPO_OmitClassMethod',
    LinesToStr([
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.ProcB = function () {',
    '  };',
    '});',
    'this.o = null;',
    '']),
    LinesToStr([
    '$mod.o.$class.ProcB();']));
end;

procedure TTestOptimizations.TestWPO_Class_OmitPropertyGetter1;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    FFoo: boolean;');
  Add('    function GetFoo: boolean;');
  Add('    property Foo: boolean read FFoo;');
  Add('    property Foo2: boolean read GetFoo;');
  Add('    FBar: boolean;');
  Add('    function GetBar: boolean;');
  Add('    property Bar: boolean read FBar;');
  Add('    property Bar2: boolean read GetBar;');
  Add('  end;');
  Add('function TObject.GetFoo: boolean; begin Result:=FFoo; end;');
  Add('function TObject.GetBar: boolean; begin Result:=FBar; end;');
  Add('var o: TObject;');
  Add('begin');
  Add('  if o.Foo then;');
  ConvertProgram;
  CheckSource('TestWPO_OmitClassPropertyGetter1',
    LinesToStr([
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '    this.FFoo = false;',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'this.o = null;',
    '']),
    LinesToStr([
    'if ($mod.o.FFoo);',
    '']));
end;

procedure TTestOptimizations.TestWPO_Class_OmitPropertyGetter2;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    FFoo: boolean;');
  Add('    function GetFoo: boolean;');
  Add('    property Foo: boolean read FFoo;');
  Add('    property Foo2: boolean read GetFoo;');
  Add('  end;');
  Add('function TObject.GetFoo: boolean; begin Result:=FFoo; end;');
  Add('var o: TObject;');
  Add('begin');
  Add('  if o.Foo2 then;');
  ConvertProgram;
  CheckSource('TestWPO_OmitClassPropertyGetter2',
    LinesToStr([
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '    this.FFoo = false;',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.GetFoo = function () {',
    '    var Result = false;',
    '    Result = this.FFoo;',
    '    return Result;',
    '  };',
    '});',
    'this.o = null;',
    '']),
    LinesToStr([
    'if ($mod.o.GetFoo()) ;',
    '']));
end;

procedure TTestOptimizations.TestWPO_Class_OmitPropertySetter1;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    FFoo: boolean;');
  Add('    procedure SetFoo(Value: boolean);');
  Add('    property Foo: boolean write FFoo;');
  Add('    property Foo2: boolean write SetFoo;');
  Add('    FBar: boolean;');
  Add('    procedure SetBar(Value: boolean);');
  Add('    property Bar: boolean write FBar;');
  Add('    property Bar2: boolean write SetBar;');
  Add('  end;');
  Add('procedure TObject.SetFoo(Value: boolean); begin FFoo:=Value; end;');
  Add('procedure TObject.SetBar(Value: boolean); begin FBar:=Value; end;');
  Add('var o: TObject;');
  Add('begin');
  Add('  o.Foo:=true;');
  ConvertProgram;
  CheckSource('TestWPO_OmitClassPropertySetter1',
    LinesToStr([
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '    this.FFoo = false;',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'this.o = null;',
    '']),
    LinesToStr([
    '$mod.o.FFoo = true;',
    '']));
end;

procedure TTestOptimizations.TestWPO_Class_OmitPropertySetter2;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    FFoo: boolean;');
  Add('    procedure SetFoo(Value: boolean);');
  Add('    property Foo: boolean write FFoo;');
  Add('    property Foo2: boolean write SetFoo;');
  Add('  end;');
  Add('procedure TObject.SetFoo(Value: boolean); begin FFoo:=Value; end;');
  Add('var o: TObject;');
  Add('begin');
  Add('  o.Foo2:=true;');
  ConvertProgram;
  CheckSource('TestWPO_OmitClassPropertySetter2',
    LinesToStr([
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '    this.FFoo = false;',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.SetFoo = function (Value) {',
    '    this.FFoo = Value;',
    '  };',
    '});',
    'this.o = null;',
    '']),
    LinesToStr([
    '$mod.o.SetFoo(true);',
    '']));
end;

procedure TTestOptimizations.TestWPO_Class_KeepNewInstance;
begin
  StartProgram(false);
  Add([
  '{$modeswitch externalclass}',
  'type',
  '  TExt = class external name ''Object''',
  '  end;',
  '  TBird = class(TExt)',
  '  protected',
  '    class function NewInstance(fnname: string; const paramarray): TBird; virtual;',
  '  public',
  '    constructor Create;',
  '  end;',
  'class function TBird.NewInstance(fnname: string; const paramarray): TBird;',
  'begin',
  '  asm',
  '  Result = Object.create();',
  '  end;',
  'end;',
  'constructor TBird.Create;',
  'begin',
  '  inherited;',
  'end;',
  'begin',
  '  TBird.Create;',
  '']);
  ConvertProgram;
  CheckSource('TestWPO_Class_KeepNewInstance',
    LinesToStr([
    'rtl.createClassExt(this, "TBird", Object, "NewInstance", function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.NewInstance = function (fnname, paramarray) {',
    '    var Result = null;',
    '    Result = Object.create();',
    '    return Result;',
    '  };',
    '  this.Create = function () {',
    '    return this;',
    '  };',
    '});',
    '']),
    LinesToStr([
    '$mod.TBird.$create("Create");',
    '']));
end;

procedure TTestOptimizations.TestWPO_CallInherited;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure DoA;');
  Add('    procedure DoB;');
  Add('  end;');
  Add('  TMobile = class');
  Add('    procedure DoA;');
  Add('    procedure DoC;');
  Add('  end;');
  Add('procedure TObject.DoA; begin end;');
  Add('procedure TObject.DoB; begin end;');
  Add('procedure TMobile.DoA;');
  Add('begin');
  Add('  inherited;');
  Add('end;');
  Add('procedure TMobile.DoC;');
  Add('begin');
  Add('  inherited DoB;');
  Add('end;');
  Add('var o: TMobile;');
  Add('begin');
  Add('  o.DoA;');
  Add('  o.DoC;');
  ConvertProgram;
  CheckSource('TestWPO_CallInherited',
    LinesToStr([
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.DoA = function () {',
    '  };',
    '  this.DoB = function () {',
    '  };',
    '});',
    ' rtl.createClass(this, "TMobile", this.TObject, function () {',
    '  this.DoA$1 = function () {',
    '    $mod.TObject.DoA.call(this);',
    '  };',
    '  this.DoC = function () {',
    '    $mod.TObject.DoB.call(this);',
    '  };',
    '});',
    'this.o = null;',
    '']),
    LinesToStr([
    '$mod.o.DoA$1();',
    '$mod.o.DoC();',
    '']));
end;

procedure TTestOptimizations.TestWPO_UseUnit;
var
  ActualSrc, ExpectedSrc: String;
begin
  AddModuleWithIntfImplSrc('unit1.pp',
    LinesToStr([
    'var i: longint;',
    'procedure DoIt;',
    '']),
    LinesToStr([
    'procedure DoIt; begin end;']));

  AddModuleWithIntfImplSrc('unit2.pp',
    LinesToStr([
    'var j: longint;',
    'procedure DoMore;',
    '']),
    LinesToStr([
    'procedure DoMore; begin end;']));

  StartProgram(true);
  Add('uses unit2;');
  Add('begin');
  Add('  j:=3;');
  ConvertProgram;
  ActualSrc:=ConvertJSModuleToString(JSModule);
  ExpectedSrc:=LinesToStr([
    'rtl.module("program", ["system", "unit2"], function () {',
    '  var $mod = this;',
    '  $mod.$main = function () {',
    '    pas.unit2.j = 3;',
    '  };',
    '});',
    '']);
  CheckDiff('TestWPO_UseUnit',ExpectedSrc,ActualSrc);
end;

procedure TTestOptimizations.TestWPO_ArrayOfConst_Use;
begin
  StartProgram(true,[supTVarRec]);
  Add([
  'procedure Say(arr: array of const);',
  'begin',
  'end;',
  'begin',
  '  Say([true]);']);
  ConvertProgram;
  CheckUnit('system.pp',
  LinesToStr([
  'rtl.module("system", [], function () {',
  '  var $mod = this;',
  '  rtl.recNewT(this, "TVarRec", function () {',
  '    this.VType = 0;',
  '    this.VJSValue = undefined;',
  '    this.$eq = function (b) {',
  '      return (this.VType === b.VType) && (this.VJSValue === b.VJSValue);',
  '    };',
  '    this.$assign = function (s) {',
  '      this.VType = s.VType;',
  '      this.VJSValue = s.VJSValue;',
  '      return this;',
  '    };',
  '  });',
  '  this.VarRecs = function () {',
  '    var Result = [];',
  '    var v = null;',
  '    v.VType = 1;',
  '    v.VJSValue = 2;',
  '    return Result;',
  '  };',
  '});',
  '']));
end;

procedure TTestOptimizations.TestWPO_ArrayOfConst_NotUsed;
begin
  StartProgram(true,[supTVarRec]);
  Add([
  'procedure Say(arr: array of const);',
  'begin',
  'end;',
  'begin']);
  ConvertProgram;
  CheckUnit('system.pp',
  LinesToStr([
  'rtl.module("system", [], function () {',
  '  var $mod = this;',
  '});',
  '']));
end;

procedure TTestOptimizations.TestWPO_Class_PropertyInOtherUnit;
begin
  AddModuleWithIntfImplSrc('unit1.pp',
    LinesToStr([
    'type',
    '  TObject = class',
    '  private',
    '    const CA = 3;',
    '  private',
    '    FOther: string;',
    '    FA: longint;',
    '    function GetA: longint;',
    '    procedure SetA(Value: longint);',
    '    function IsStoredA: boolean;',
    '  public',
    '    property A: longint read GetA write SetA stored IsStoredA default CA;',
    '  end;',
    '']),
    LinesToStr([
    'function TObject.geta: longint;',
    'begin',
    'end;',
    'procedure TObject.seta(value: longint);',
    'begin',
    '  FA:=Value;',
    'end;',
    'function TObject.isstoreda: boolean; begin end;',
    '']));
  StartProgram(true);
  Add([
  'uses unit1;',
  'var o: TObject;',
  'begin',
  '  o.A:=o.A;']);
  ConvertProgram;
  CheckUnit('unit1.pp',
  LinesToStr([
  'rtl.module("unit1", ["system"], function () {',
  '  var $mod = this;',
  '  rtl.createClass(this, "TObject", null, function () {',
  '    this.$init = function () {',
  '      this.FA = 0;',
  '    };',
  '    this.$final = function () {',
  '    };',
  '    this.GetA = function () {',
  '      var Result = 0;',
  '      return Result;',
  '    };',
  '    this.SetA = function (Value) {',
  '      this.FA = Value;',
  '    };',
  '  });',
  '});',
  '']));
end;

procedure TTestOptimizations.TestWPO_ProgramPublicDeclaration;
var
  ActualSrc, ExpectedSrc: String;
begin
  StartProgram(true);
  Add('var');
  Add('  vPublic: longint; public;');
  Add('  vPrivate: longint;');
  Add('procedure DoPublic; public; begin end;');
  Add('procedure DoPrivate; begin end;');
  Add('begin');
  ConvertProgram;
  ActualSrc:=ConvertJSModuleToString(JSModule);
  ExpectedSrc:=LinesToStr([
    'rtl.module("program", ["system"], function () {',
    '  var $mod = this;',
    '  this.vPublic = 0;',
    '  this.DoPublic =function(){',
    '  };',
    '  $mod.$main = function () {',
    '  };',
    '});',
    '']);
  CheckDiff('TestWPO_ProgramPublicDeclaration',ExpectedSrc,ActualSrc);
end;

procedure TTestOptimizations.TestWPO_ConstructorDefaultValueConst;
var
  ActualSrc, ExpectedSrc: String;
begin
  WithTypeInfo:=true;
  StartProgram(true);
  Add([
  'const gcBlack = 0;',
  'type',
  '  TColor = longint;',
  '  TObject = class',
  '  private',
  '    FColor: TColor;',
  '  public',
  '    property Color: TColor read FColor write FColor;',
  '    constructor Create(const AColor: TColor = gcBlack);',
  '  end;',
  'constructor TObject.Create(const AColor: TColor = gcBlack);',
  'begin',
  '  FColor := AColor;',
  'end;',
  'var T: TObject;',
  'begin',
  '  T := TObject.Create;',
  '']);
  ConvertProgram;
  ActualSrc:=ConvertJSModuleToString(JSModule);
  ExpectedSrc:=LinesToStr([
  'rtl.module("program",["system"],function () {',
  '  var $mod = this;',
  '  this.gcBlack = 0;',
  '  rtl.createClass(this,"TObject",null,function () {',
  '    this.$init = function () {',
  '      this.FColor = 0;',
  '    };',
  '    this.$final = function () {',
  '    };',
  '    this.Create = function (AColor) {',
  '      this.FColor = AColor;',
  '      return this;',
  '    };',
  '  });',
  '  this.T = null;',
  '  $mod.$main = function () {',
  '    $mod.T = $mod.TObject.$create("Create",[0]);',
  '  };',
  '});',
  '']);
  CheckDiff('TestWPO_ConstructorDefaultValueConst',ExpectedSrc,ActualSrc);
end;

procedure TTestOptimizations.TestWPO_RTTI_PublishedField;
var
  ActualSrc, ExpectedSrc: String;
begin
  WithTypeInfo:=true;
  StartProgram(true);
  Add([
  'type',
  '  TArrA = array of char;',
  '  TArrB = array of string;',
  '  TObject = class',
  '  public',
  '    PublicA: TArrA;',
  '  published',
  '    PublishedB: TArrB;',
  '  end;',
  'var',
  '  C: TObject;',
  'begin',
  '  C.PublicA:=nil;',
  '  if typeinfo(TObject)=nil then ;',
  '']);
  ConvertProgram;
  ActualSrc:=ConvertJSModuleToString(JSModule);
  ExpectedSrc:=LinesToStr([
    'rtl.module("program", ["system"], function () {',
    '  var $mod = this;',
    '  this.$rtti.$DynArray("TArrB", {',
    '    eltype: rtl.string',
    '  });',
    '  rtl.createClass(this, "TObject", null, function () {',
    '    this.$init = function () {',
    '      this.PublicA = [];',
    '      this.PublishedB = [];',
    '    };',
    '    this.$final = function () {',
    '      this.PublicA = undefined;',
    '      this.PublishedB = undefined;',
    '    };',
    '    var $r = this.$rtti;',
    '    $r.addField("PublishedB", $mod.$rtti["TArrB"]);',
    '  });',
    '  this.C = null;',
    '  $mod.$main = function () {',
    '    $mod.C.PublicA = [];',
    '    if ($mod.$rtti["TObject"] === null) ;',
    '  };',
    '});',
    '']);
  CheckDiff('TestWPO_RTTI_PublishedField',ExpectedSrc,ActualSrc);
end;

procedure TTestOptimizations.TestWPO_RTTI_TypeInfo;
var
  ActualSrc, ExpectedSrc: String;
begin
  WithTypeInfo:=true;
  StartProgram(true);
  Add('type');
  Add('  TArrA = array of char;');
  Add('  TArrB = array of string;');
  Add('var');
  Add('  A: TArrA;');
  Add('  B: TArrB;');
  Add('  p: pointer;');
  Add('begin');
  Add('  A:=nil;');
  Add('  p:=typeinfo(B);');
  ConvertProgram;
  ActualSrc:=ConvertJSModuleToString(JSModule);
  ExpectedSrc:=LinesToStr([
    'rtl.module("program", ["system"], function () {',
    '  var $mod = this;',
    '  this.$rtti.$DynArray("TArrB", {',
    '    eltype: rtl.string',
    '  });',
    '  this.A = [];',
    '  this.B = [];',
    '  this.p = null;',
    '  $mod.$main = function () {',
    '    $mod.A = [];',
    '    $mod.p = $mod.$rtti["TArrB"];',
    '  };',
    '});',
    '']);
  CheckDiff('TestWPO_RTTI_TypeInfo',ExpectedSrc,ActualSrc);
end;

procedure TTestOptimizations.TestOptTruncateIntegersOnOverflow_OperatorsByte;
begin
  StartProgram(false);
  Add([
  '{$modeswitch externalclass}',
  '{$optimization JSTruncateIntegersOnOverflow ON}',
  'procedure IntTypeTest(v: Byte); overload; begin end;',
  'procedure IntTypeTest(v: ShortInt); overload; begin end;',
  'procedure IntTypeTest(v: Word); overload; begin end;',
  'procedure IntTypeTest(v: SmallInt); overload; begin end;',
  'procedure IntTypeTest(v: LongWord); overload; begin end;',
  'procedure IntTypeTest(v: LongInt); overload; begin end;',
  'procedure IntTypeTest(v: NativeUInt); overload; begin end;',
  'procedure IntTypeTest(v: NativeInt); overload; begin end;',
  'type',
  '  TJSUint8Array  = class external name ''Uint8Array''',
  '    private',
  '      function getTypedValue(Index : NativeInt): Byte; external name ''[]'';',
  '      procedure setTypedValue(Index : NativeInt; AValue: Byte);external name ''[]'';',
  '    public',
  '      constructor new (length : NativeInt);',
  '      property values[Index : NativeInt] : Byte Read getTypedValue Write setTypedValue; default;',
  '    end;',
  'var',
  '  a, b, c, d : Byte;',
  '  Buf : array of Byte;',
  '  Buf2 : TJSUint8Array;',
  'begin',
  '  IntTypeTest(a and 1);',
  '  IntTypeTest(a or 1);',
  '  IntTypeTest(a xor 1);',
  '  IntTypeTest(a shr 1);',
  '  IntTypeTest(a shl 1);',
  '  IntTypeTest(a + 1);',
  '  IntTypeTest(a - 1);',
  '  IntTypeTest(a * 1);',
  '  IntTypeTest(a div 1);',
  '  IntTypeTest(a mod 1);',
  '  IntTypeTest(a ** 1);',
  '  IntTypeTest(9 and a);',
  '  IntTypeTest(9 or a);',
  '  IntTypeTest(9 xor a);',
  '  IntTypeTest(9 shr a);',
  '  IntTypeTest(9 shl a);',
  '  IntTypeTest(9 + a);',
  '  IntTypeTest(9 - a);',
  '  IntTypeTest(9 * a);',
  '  IntTypeTest(9 div a);',
  '  IntTypeTest(9 mod a);',
  '  IntTypeTest(9 ** a);',
  '  IntTypeTest(a or $111);',
  '  IntTypeTest(a or $33333);',
  '  IntTypeTest(a or $5555555);',
  '  IntTypeTest(a + $111);',
  '  IntTypeTest(a + $33333);',
  '  IntTypeTest(a + $5555555);',
  '  IntTypeTest(a and b);',
  '  IntTypeTest(a or b);',
  '  IntTypeTest(a xor b);',
  '  IntTypeTest(a shr b);',
  '  IntTypeTest(a shl b);',
  '  IntTypeTest(not a);',
  '  IntTypeTest(a + b);',
  '  IntTypeTest(a - b);',
  '  IntTypeTest(a + -b);',
  '  IntTypeTest(a * b);',
  '  IntTypeTest(a div b);',
  '  IntTypeTest(a mod b);',
  '  IntTypeTest(a ** b);',
  '  IntTypeTest(a and b and c and d);',
  '  IntTypeTest(a or b or c or d);',
  '  IntTypeTest(a xor b xor c xor d);',
  '  IntTypeTest(a shr b shr c shr d);',
  '  IntTypeTest(a shl b shl c shl d);',
  '  IntTypeTest(a + b + c + d);',
  '  IntTypeTest(a - b - c - d);',
  '  IntTypeTest(a * b * c * d);',
  '  IntTypeTest(a and b or c xor d or not c shr 1 or c shl 1);',
  '  IntTypeTest(a + b - c * d);',
  '  a := b;',
  '  b := c xor d;',
  '  c := b + a;',
  '  d := not c;',
  '  Inc(a);',
  '  Inc(a, 10);',
  '  Dec(b);',
  '  Dec(b, 10);',
  '  Buf[0] := a;',
  '  Buf[0] := a shr 1;',
  '  Buf[0] := a + b;',
  '  Buf[0] := a or Buf[0];',
  '  Buf[0] := a - Buf[0];',
  '  Buf2[0] := a;',
  '  Buf2[0] := a shr 1;',
  '  Buf2[0] := a + b;',
  '  Buf2[0] := a or Buf2[0];',
  '']);
  ConvertProgram;
  CheckSource('TestOptTruncateIntegersOnOverflow_OperatorsByte',
    LinesToStr([ // statements
    'this.IntTypeTest = function (v) {',
    '};',
    'this.IntTypeTest$2 = function (v) {',
    '};',
    'this.IntTypeTest$5 = function (v) {',
    '};',
    'this.a = 0;',
    'this.b = 0;',
    'this.c = 0;',
    'this.d = 0;',
    'this.Buf = [];',
    'this.Buf2 = null;',
    '']),
    LinesToStr([ // this.$main
    '$mod.IntTypeTest($mod.a & 1);',
    '$mod.IntTypeTest($mod.a | 1);',
    '$mod.IntTypeTest($mod.a ^ 1);',
    '$mod.IntTypeTest$5($mod.a >>> 1);',
    '$mod.IntTypeTest$5($mod.a << 1);',
    '$mod.IntTypeTest$5(($mod.a + 1) | 0);',
    '$mod.IntTypeTest$5(($mod.a - 1) | 0);',
    '$mod.IntTypeTest$5(($mod.a * 1) | 0);',
    '$mod.IntTypeTest$5(rtl.trunc($mod.a / 1));',
    '$mod.IntTypeTest$5($mod.a % 1);',
    '$mod.IntTypeTest$5(Math.pow($mod.a,1));',
    '$mod.IntTypeTest(9 & $mod.a);',
    '$mod.IntTypeTest(9 | $mod.a);',
    '$mod.IntTypeTest(9 ^ $mod.a);',
    '$mod.IntTypeTest$5(9 >>> $mod.a);',
    '$mod.IntTypeTest$5(9 << $mod.a);',
    '$mod.IntTypeTest$5((9 + $mod.a) | 0);',
    '$mod.IntTypeTest$5((9 - $mod.a) | 0);',
    '$mod.IntTypeTest$5((9 * $mod.a) | 0);',
    '$mod.IntTypeTest$5(rtl.trunc(9 / $mod.a));',
    '$mod.IntTypeTest$5(9 % $mod.a);',
    '$mod.IntTypeTest$5(Math.pow(9,$mod.a));',
    '$mod.IntTypeTest$2($mod.a | 0x111);',
    '$mod.IntTypeTest$5($mod.a | 0x33333);',
    '$mod.IntTypeTest$5($mod.a | 0x5555555);',
    '$mod.IntTypeTest$5(($mod.a + 0x111) | 0);',
    '$mod.IntTypeTest$5(($mod.a + 0x33333) | 0);',
    '$mod.IntTypeTest$5(($mod.a + 0x5555555) | 0);',
    '$mod.IntTypeTest($mod.a & $mod.b);',
    '$mod.IntTypeTest($mod.a | $mod.b);',
    '$mod.IntTypeTest($mod.a ^ $mod.b);',
    '$mod.IntTypeTest$5($mod.a >>> $mod.b);',
    '$mod.IntTypeTest$5($mod.a << $mod.b);',
    '$mod.IntTypeTest($mod.a ^ 255);',
    '$mod.IntTypeTest$5(($mod.a + $mod.b) | 0);',
    '$mod.IntTypeTest$5(($mod.a - $mod.b) | 0);',
    '$mod.IntTypeTest$5(($mod.a + -$mod.b) | 0);',
    '$mod.IntTypeTest$5(($mod.a * $mod.b) | 0);',
    '$mod.IntTypeTest$5(rtl.trunc($mod.a / $mod.b));',
    '$mod.IntTypeTest$5($mod.a % $mod.b);',
    '$mod.IntTypeTest$5(Math.pow($mod.a, $mod.b));',
    '$mod.IntTypeTest($mod.a & $mod.b & $mod.c & $mod.d);',
    '$mod.IntTypeTest($mod.a | $mod.b | $mod.c | $mod.d);',
    '$mod.IntTypeTest($mod.a ^ $mod.b ^ $mod.c ^ $mod.d);',
    '$mod.IntTypeTest$5((($mod.a >>> $mod.b) >>> $mod.c) >>> $mod.d);',
    '$mod.IntTypeTest$5((($mod.a << $mod.b) << $mod.c) << $mod.d);',
    '$mod.IntTypeTest$5(($mod.a + $mod.b + $mod.c + $mod.d) | 0);',
    '$mod.IntTypeTest$5(($mod.a - $mod.b - $mod.c - $mod.d) | 0);',
    '$mod.IntTypeTest$5(((((($mod.a * $mod.b) | 0) * $mod.c) | 0) * $mod.d) | 0);',
    '$mod.IntTypeTest$5(((($mod.a & $mod.b) | $mod.c) ^ $mod.d) | (($mod.c ^ 255) >>> 1) | ($mod.c << 1));',
    '$mod.IntTypeTest$5((($mod.a + $mod.b) - (($mod.c * $mod.d) | 0)) | 0);',
    '$mod.a = $mod.b;',
    '$mod.b = $mod.c ^ $mod.d;',
    '$mod.c = ($mod.b + $mod.a) & 255;',
    '$mod.d = $mod.c ^ 255;',
    '$mod.a = ($mod.a + 1) & 255;',
    '$mod.a = ($mod.a + 10) & 255;',
    '$mod.b = ($mod.b - 1) & 255;',
    '$mod.b = ($mod.b - 10) & 255;',
    '$mod.Buf[0] = $mod.a;',
    '$mod.Buf[0] = ($mod.a >>> 1) & 255;',
    '$mod.Buf[0] = ($mod.a + $mod.b) & 255;',
    '$mod.Buf[0] = $mod.a | $mod.Buf[0];',
    '$mod.Buf[0] = ($mod.a - $mod.Buf[0]) & 255;',
    '$mod.Buf2[0] = $mod.a;',
    '$mod.Buf2[0] = $mod.a >>> 1;',
    '$mod.Buf2[0] = $mod.a + $mod.b;',
    '$mod.Buf2[0] = $mod.a | $mod.Buf2[0];',
    '']));
end;

procedure TTestOptimizations.TestOptTruncateIntegersOnOverflow_OperatorsShortInt;
begin
  StartProgram(false);
  Add([
  '{$modeswitch externalclass}',
  '{$optimization JSTruncateIntegersOnOverflow ON}',
  'procedure IntTypeTest(v: Byte); overload; begin end;',
  'procedure IntTypeTest(v: ShortInt); overload; begin end;',
  'procedure IntTypeTest(v: Word); overload; begin end;',
  'procedure IntTypeTest(v: SmallInt); overload; begin end;',
  'procedure IntTypeTest(v: LongWord); overload; begin end;',
  'procedure IntTypeTest(v: LongInt); overload; begin end;',
  'procedure IntTypeTest(v: NativeUInt); overload; begin end;',
  'procedure IntTypeTest(v: NativeInt); overload; begin end;',
  'type',
  '  TJSInt8Array  = class external name ''Int8Array''',
  '  private',
  '    function getTypedValue(Index : NativeInt): ShortInt; external name ''[]'';',
  '    procedure setTypedValue(Index : NativeInt; AValue: ShortInt);external name ''[]'';',
  '  public',
  '    constructor new (length : NativeInt);',
  '    property values[Index : NativeInt] : ShortInt Read getTypedValue Write setTypedValue; default;',
  '  end;',
  'var',
  '  a, b, c, d : ShortInt;',
  '  vb : Byte;',
  '  Buf : array of ShortInt;',
  '  Buf2 : TJSInt8Array;',
  'begin',
  '  IntTypeTest(a and 1);',
  '  IntTypeTest(a or 1);',
  '  IntTypeTest(a xor 1);',
  '  IntTypeTest(a shr 1);',
  '  IntTypeTest(a shl 1);',
  '  IntTypeTest(a + 1);',
  '  IntTypeTest(a - 1);',
  '  IntTypeTest(a * 1);',
  '  IntTypeTest(a div 1);',
  '  IntTypeTest(a mod 1);',
  '  IntTypeTest(a ** 1);',
  '  IntTypeTest(9 and a);',
  '  IntTypeTest(9 or a);',
  '  IntTypeTest(9 xor a);',
  '  IntTypeTest(9 shr a);',
  '  IntTypeTest(9 shl a);',
  '  IntTypeTest(9 + a);',
  '  IntTypeTest(9 - a);',
  '  IntTypeTest(9 * a);',
  '  IntTypeTest(9 div a);',
  '  IntTypeTest(9 mod a);',
  '  IntTypeTest(9 ** a);',
  '  IntTypeTest(a or $111);',
  '  IntTypeTest(a or $33333);',
  '  IntTypeTest(a or $5555555);',
  '  IntTypeTest(a + $111);',
  '  IntTypeTest(a + $33333);',
  '  IntTypeTest(a + $5555555);',
  '  IntTypeTest(a and b);',
  '  IntTypeTest(a or b);',
  '  IntTypeTest(a xor b);',
  '  IntTypeTest(a shr b);',
  '  IntTypeTest(a shl b);',
  '  IntTypeTest(not a);',
  '  IntTypeTest(a + b);',
  '  IntTypeTest(a - b);',
  '  IntTypeTest(a + -b);',
  '  IntTypeTest(a * b);',
  '  IntTypeTest(a div b);',
  '  IntTypeTest(a mod b);',
  '  IntTypeTest(a ** b);',
  '  IntTypeTest(a and b and c and d);',
  '  IntTypeTest(a or b or c or d);',
  '  IntTypeTest(a xor b xor c xor d);',
  '  IntTypeTest(a shr b shr c shr d);',
  '  IntTypeTest(a shl b shl c shl d);',
  '  IntTypeTest(a + b + c + d);',
  '  IntTypeTest(a - b - c - d);',
  '  IntTypeTest(a * b * c * d);',
  '  IntTypeTest(a and b or c xor d or not c shr 1 or c shl 1);',
  '  IntTypeTest(a + b - c * d);',
  '  IntTypeTest(b or vb);',
  '  IntTypeTest(vb xor b);',
  '  IntTypeTest(b or vb and c);',
  '  IntTypeTest(b + vb);',
  '  IntTypeTest(vb - b);',
  '  IntTypeTest(b + vb * c);',
  '  a := b;',
  '  b := c xor d;',
  '  c := b + a;',
  '  d := not c;',
  '  Inc(a);',
  '  Inc(a, 10);',
  '  Dec(b);',
  '  Dec(b, 10);',
  '  Buf[0] := a;',
  '  Buf[0] := a shr 1;',
  '  Buf[0] := a + b;',
  '  Buf[0] := a or Buf[0];',
  '  Buf[0] := a - Buf[0];',
  '  Buf2[0] := a;',
  '  Buf2[0] := a shr 1;',
  '  Buf2[0] := a + b;',
  '  Buf2[0] := a or Buf2[0];',
  '  a := b or vb;',
  '  a := vb xor b;',
  '  a := b or vb and c;',
  '  a := b + vb;',
  '  a := vb - b;',
  '  a := b + vb * c;',
  '  vb := a;',
  '  vb := a and b;',
  '  vb := a + b;',
  '']);
  ConvertProgram;
  CheckSource('TestOptTruncateIntegersOnOverflow_OperatorsShortInt',
    LinesToStr([ // statements
    'this.IntTypeTest$1 = function (v) {',
    '};',
    'this.IntTypeTest$3 = function (v) {',
    '};',
    'this.IntTypeTest$5 = function (v) {',
    '};',
    'this.a = 0;',
    'this.b = 0;',
    'this.c = 0;',
    'this.d = 0;',
    'this.vb = 0;',
    'this.Buf = [];',
    'this.Buf2 = null;',
    '']),
    LinesToStr([ // this.$main
    '$mod.IntTypeTest$1($mod.a & 1);',
    '$mod.IntTypeTest$1($mod.a | 1);',
    '$mod.IntTypeTest$1($mod.a ^ 1);',
    '$mod.IntTypeTest$5($mod.a >>> 1);',
    '$mod.IntTypeTest$5($mod.a << 1);',
    '$mod.IntTypeTest$5(($mod.a + 1) | 0);',
    '$mod.IntTypeTest$5(($mod.a - 1) | 0);',
    '$mod.IntTypeTest$5(($mod.a * 1) | 0);',
    '$mod.IntTypeTest$5(rtl.trunc($mod.a / 1));',
    '$mod.IntTypeTest$5($mod.a % 1);',
    '$mod.IntTypeTest$5(Math.pow($mod.a, 1));',
    '$mod.IntTypeTest$1(9 & $mod.a);',
    '$mod.IntTypeTest$1(9 | $mod.a);',
    '$mod.IntTypeTest$1(9 ^ $mod.a);',
    '$mod.IntTypeTest$5(9 >>> $mod.a);',
    '$mod.IntTypeTest$5(9 << $mod.a);',
    '$mod.IntTypeTest$5((9 + $mod.a) | 0);',
    '$mod.IntTypeTest$5((9 - $mod.a) | 0);',
    '$mod.IntTypeTest$5((9 * $mod.a) | 0);',
    '$mod.IntTypeTest$5(rtl.trunc(9 / $mod.a));',
    '$mod.IntTypeTest$5(9 % $mod.a);',
    '$mod.IntTypeTest$5(Math.pow(9, $mod.a));',
    '$mod.IntTypeTest$3($mod.a | 0x111);',
    '$mod.IntTypeTest$5($mod.a | 0x33333);',
    '$mod.IntTypeTest$5($mod.a | 0x5555555);',
    '$mod.IntTypeTest$5(($mod.a + 0x111) | 0);',
    '$mod.IntTypeTest$5(($mod.a + 0x33333) | 0);',
    '$mod.IntTypeTest$5(($mod.a + 0x5555555) | 0);',
    '$mod.IntTypeTest$1($mod.a & $mod.b);',
    '$mod.IntTypeTest$1($mod.a | $mod.b);',
    '$mod.IntTypeTest$1($mod.a ^ $mod.b);',
    '$mod.IntTypeTest$5($mod.a >>> $mod.b);',
    '$mod.IntTypeTest$5($mod.a << $mod.b);',
    '$mod.IntTypeTest$1(~$mod.a);',
    '$mod.IntTypeTest$5(($mod.a + $mod.b) | 0);',
    '$mod.IntTypeTest$5(($mod.a - $mod.b) | 0);',
    '$mod.IntTypeTest$5(($mod.a + -$mod.b) | 0);',
    '$mod.IntTypeTest$5(($mod.a * $mod.b) | 0);',
    '$mod.IntTypeTest$5(rtl.trunc($mod.a / $mod.b));',
    '$mod.IntTypeTest$5($mod.a % $mod.b);',
    '$mod.IntTypeTest$5(Math.pow($mod.a, $mod.b));',
    '$mod.IntTypeTest$1($mod.a & $mod.b & $mod.c & $mod.d);',
    '$mod.IntTypeTest$1($mod.a | $mod.b | $mod.c | $mod.d);',
    '$mod.IntTypeTest$1($mod.a ^ $mod.b ^ $mod.c ^ $mod.d);',
    '$mod.IntTypeTest$5((($mod.a >>> $mod.b) >>> $mod.c) >>> $mod.d);',
    '$mod.IntTypeTest$5((($mod.a << $mod.b) << $mod.c) << $mod.d);',
    '$mod.IntTypeTest$5(($mod.a + $mod.b + $mod.c + $mod.d) | 0);',
    '$mod.IntTypeTest$5(($mod.a - $mod.b - $mod.c - $mod.d) | 0);',
    '$mod.IntTypeTest$5(((((($mod.a * $mod.b) | 0) * $mod.c) | 0) * $mod.d) | 0);',
    '$mod.IntTypeTest$5(((($mod.a & $mod.b) | $mod.c) ^ $mod.d) | (~$mod.c >>> 1) | ($mod.c << 1));',
    '$mod.IntTypeTest$5((($mod.a + $mod.b) - (($mod.c * $mod.d) | 0)) | 0);',
    '$mod.IntTypeTest$3($mod.b | $mod.vb);',
    '$mod.IntTypeTest$3($mod.vb ^ $mod.b);',
    '$mod.IntTypeTest$3($mod.b | ($mod.vb & $mod.c));',
    '$mod.IntTypeTest$5(($mod.b + $mod.vb) | 0);',
    '$mod.IntTypeTest$5(($mod.vb - $mod.b) | 0);',
    '$mod.IntTypeTest$5(($mod.b + (($mod.vb * $mod.c) | 0)) | 0);',
    '$mod.a = $mod.b;',
    '$mod.b = $mod.c ^ $mod.d;',
    '$mod.c = ((($mod.b + $mod.a) & 255) << 24) >> 24;',
    '$mod.d = ~$mod.c;',
    '$mod.a = ((($mod.a + 1) & 255) << 24) >> 24;',
    '$mod.a = ((($mod.a + 10) & 255) << 24) >> 24;',
    '$mod.b = ((($mod.b - 1) & 255) << 24) >> 24;',
    '$mod.b = ((($mod.b - 10) & 255) << 24) >> 24;',
    '$mod.Buf[0] = $mod.a;',
    '$mod.Buf[0] = ((($mod.a >>> 1) & 255) << 24) >> 24;',
    '$mod.Buf[0] = ((($mod.a + $mod.b) & 255) << 24) >> 24;',
    '$mod.Buf[0] = $mod.a | $mod.Buf[0];',
    '$mod.Buf[0] = ((($mod.a - $mod.Buf[0]) & 255) << 24) >> 24;',
    '$mod.Buf2[0] = $mod.a;',
    '$mod.Buf2[0] = $mod.a >>> 1;',
    '$mod.Buf2[0] = $mod.a + $mod.b;',
    '$mod.Buf2[0] = $mod.a | $mod.Buf2[0];',
    '$mod.a = ((($mod.b | $mod.vb) & 255) << 24) >> 24;',
    '$mod.a = ((($mod.vb ^ $mod.b) & 255) << 24) >> 24;',
    '$mod.a = ((($mod.b | ($mod.vb & $mod.c)) & 255) << 24) >> 24;',
    '$mod.a = ((($mod.b + $mod.vb) & 255) << 24) >> 24;',
    '$mod.a = ((($mod.vb - $mod.b) & 255) << 24) >> 24;',
    '$mod.a = ((($mod.b + (($mod.vb * $mod.c) | 0)) & 255) << 24) >> 24;',
    '$mod.vb = $mod.a & 255;',
    '$mod.vb = $mod.a & $mod.b & 255;',
    '$mod.vb = ($mod.a + $mod.b) & 255;',
    '']));
end;

procedure TTestOptimizations.TestOptTruncateIntegersOnOverflow_OperatorsWord;
begin
  StartProgram(false);
  Add([
  '{$modeswitch externalclass}',
  '{$optimization JSTruncateIntegersOnOverflow ON}',
  'procedure IntTypeTest(v: Byte); overload; begin end;',
  'procedure IntTypeTest(v: ShortInt); overload; begin end;',
  'procedure IntTypeTest(v: Word); overload; begin end;',
  'procedure IntTypeTest(v: SmallInt); overload; begin end;',
  'procedure IntTypeTest(v: LongWord); overload; begin end;',
  'procedure IntTypeTest(v: LongInt); overload; begin end;',
  'procedure IntTypeTest(v: NativeUInt); overload; begin end;',
  'procedure IntTypeTest(v: NativeInt); overload; begin end;',
  'type',
  '  TJSUint16Array  = class external name ''Uint16Array''',
  '  private',
  '    function getTypedValue(Index : NativeInt): Word; external name ''[]'';',
  '    procedure setTypedValue(Index : NativeInt; AValue: Word);external name ''[]'';',
  '  public',
  '    constructor new (length : NativeInt);',
  '    property values[Index : NativeInt] : Word Read getTypedValue Write setTypedValue; default;',
  '  end;',
  'var',
  '  a, b, c, d : Word;',
  '  vb : Byte;',
  '  vh : ShortInt;',
  '  Buf : array of Word;',
  '  Buf2 : TJSUint16Array;',
  'begin',
  '  IntTypeTest(a and 1);',
  '  IntTypeTest(a or 1);',
  '  IntTypeTest(a xor 1);',
  '  IntTypeTest(a shr 1);',
  '  IntTypeTest(a shl 1);',
  '  IntTypeTest(a + 1);',
  '  IntTypeTest(a - 1);',
  '  IntTypeTest(a * 1);',
  '  IntTypeTest(a div 1);',
  '  IntTypeTest(a mod 1);',
  '  IntTypeTest(a ** 1);',
  '  IntTypeTest(9 and a);',
  '  IntTypeTest(9 or a);',
  '  IntTypeTest(9 xor a);',
  '  IntTypeTest(9 shr a);',
  '  IntTypeTest(9 shl a);',
  '  IntTypeTest(9 + a);',
  '  IntTypeTest(9 - a);',
  '  IntTypeTest(9 * a);',
  '  IntTypeTest(9 div a);',
  '  IntTypeTest(9 mod a);',
  '  IntTypeTest(9 ** a);',
  '  IntTypeTest(a or $111);',
  '  IntTypeTest(a or $33333);',
  '  IntTypeTest(a or $5555555);',
  '  IntTypeTest(a + $111);',
  '  IntTypeTest(a + $33333);',
  '  IntTypeTest(a + $5555555);',
  '  IntTypeTest(a and b);',
  '  IntTypeTest(a or b);',
  '  IntTypeTest(a xor b);',
  '  IntTypeTest(a shr b);',
  '  IntTypeTest(a shl b);',
  '  IntTypeTest(not a);',
  '  IntTypeTest(a + b);',
  '  IntTypeTest(a - b);',
  '  IntTypeTest(a + -b);',
  '  IntTypeTest(a * b);',
  '  IntTypeTest(a div b);',
  '  IntTypeTest(a mod b);',
  '  IntTypeTest(a ** b);',
  '  IntTypeTest(a and b and c and d);',
  '  IntTypeTest(a or b or c or d);',
  '  IntTypeTest(a xor b xor c xor d);',
  '  IntTypeTest(a shr b shr c shr d);',
  '  IntTypeTest(a shl b shl c shl d);',
  '  IntTypeTest(a + b + c + d);',
  '  IntTypeTest(a - b - c - d);',
  '  IntTypeTest(a * b * c * d);',
  '  IntTypeTest(a and b or c xor d or not c shr 1 or c shl 1);',
  '  IntTypeTest(a + b - c * d);',
  '  IntTypeTest(b or vb);',
  '  IntTypeTest(vb xor b);',
  '  IntTypeTest(b or vb and c);',
  '  IntTypeTest(b + vb);',
  '  IntTypeTest(vb - b);',
  '  IntTypeTest(b + vb * c);',
  '  IntTypeTest(b or vh);',
  '  IntTypeTest(vh xor b);',
  '  IntTypeTest(b or vh and c);',
  '  IntTypeTest(b + vh);',
  '  IntTypeTest(vh - b);',
  '  IntTypeTest(b + vh * c);',
  '  a := b;',
  '  b := c xor d;',
  '  c := b + a;',
  '  d := not c;',
  '  Inc(a);',
  '  Inc(a, 10);',
  '  Dec(b);',
  '  Dec(b, 10);',
  '  Buf[0] := a;',
  '  Buf[0] := a shr 1;',
  '  Buf[0] := a + b;',
  '  Buf[0] := a or Buf[0];',
  '  Buf[0] := a - Buf[0];',
  '  Buf2[0] := a;',
  '  Buf2[0] := a shr 1;',
  '  Buf2[0] := a + b;',
  '  Buf2[0] := a or Buf2[0];',
  '  a := b or vb;',
  '  a := vb xor b;',
  '  a := b or vb and c;',
  '  a := b + vb;',
  '  a := vb - b;',
  '  a := b + vb * c;',
  '  vb := a;',
  '  vb := a and b;',
  '  vb := a + b;',
  '  a := b or vh;',
  '  a := vh xor b;',
  '  a := b or vh and c;',
  '  a := b + vh;',
  '  a := vh - b;',
  '  a := b + vh * c;',
  '  vh := a;',
  '  vh := a and b;',
  '  vh := a + b;',
  '']);
  ConvertProgram;
  CheckSource('TestOptTruncateIntegersOnOverflow_OperatorsWord',
    LinesToStr([ // statements
    'this.IntTypeTest$2 = function (v) {',
    '};',
    'this.IntTypeTest$5 = function (v) {',
    '};',
    'this.a = 0;',
    'this.b = 0;',
    'this.c = 0;',
    'this.d = 0;',
    'this.vb = 0;',
    'this.vh = 0;',
    'this.Buf = [];',
    'this.Buf2 = null;',
    '']),
    LinesToStr([ // this.$main
    '$mod.IntTypeTest$2($mod.a & 1);',
    '$mod.IntTypeTest$2($mod.a | 1);',
    '$mod.IntTypeTest$2($mod.a ^ 1);',
    '$mod.IntTypeTest$5($mod.a >>> 1);',
    '$mod.IntTypeTest$5($mod.a << 1);',
    '$mod.IntTypeTest$5(($mod.a + 1) | 0);',
    '$mod.IntTypeTest$5(($mod.a - 1) | 0);',
    '$mod.IntTypeTest$5(($mod.a * 1) | 0);',
    '$mod.IntTypeTest$5(rtl.trunc($mod.a / 1));',
    '$mod.IntTypeTest$5($mod.a % 1);',
    '$mod.IntTypeTest$5(Math.pow($mod.a, 1));',
    '$mod.IntTypeTest$2(9 & $mod.a);',
    '$mod.IntTypeTest$2(9 | $mod.a);',
    '$mod.IntTypeTest$2(9 ^ $mod.a);',
    '$mod.IntTypeTest$5(9 >>> $mod.a);',
    '$mod.IntTypeTest$5(9 << $mod.a);',
    '$mod.IntTypeTest$5((9 + $mod.a) | 0);',
    '$mod.IntTypeTest$5((9 - $mod.a) | 0);',
    '$mod.IntTypeTest$5((9 * $mod.a) | 0);',
    '$mod.IntTypeTest$5(rtl.trunc(9 / $mod.a));',
    '$mod.IntTypeTest$5(9 % $mod.a);',
    '$mod.IntTypeTest$5(Math.pow(9, $mod.a));',
    '$mod.IntTypeTest$2($mod.a | 0x111);',
    '$mod.IntTypeTest$5($mod.a | 0x33333);',
    '$mod.IntTypeTest$5($mod.a | 0x5555555);',
    '$mod.IntTypeTest$5(($mod.a + 0x111) | 0);',
    '$mod.IntTypeTest$5(($mod.a + 0x33333) | 0);',
    '$mod.IntTypeTest$5(($mod.a + 0x5555555) | 0);',
    '$mod.IntTypeTest$2($mod.a & $mod.b);',
    '$mod.IntTypeTest$2($mod.a | $mod.b);',
    '$mod.IntTypeTest$2($mod.a ^ $mod.b);',
    '$mod.IntTypeTest$5($mod.a >>> $mod.b);',
    '$mod.IntTypeTest$5($mod.a << $mod.b);',
    '$mod.IntTypeTest$2($mod.a ^ 65535);',
    '$mod.IntTypeTest$5(($mod.a + $mod.b) | 0);',
    '$mod.IntTypeTest$5(($mod.a - $mod.b) | 0);',
    '$mod.IntTypeTest$5(($mod.a + -$mod.b) | 0);',
    '$mod.IntTypeTest$5(($mod.a * $mod.b) | 0);',
    '$mod.IntTypeTest$5(rtl.trunc($mod.a / $mod.b));',
    '$mod.IntTypeTest$5($mod.a % $mod.b);',
    '$mod.IntTypeTest$5(Math.pow($mod.a, $mod.b));',
    '$mod.IntTypeTest$2($mod.a & $mod.b & $mod.c & $mod.d);',
    '$mod.IntTypeTest$2($mod.a | $mod.b | $mod.c | $mod.d);',
    '$mod.IntTypeTest$2($mod.a ^ $mod.b ^ $mod.c ^ $mod.d);',
    '$mod.IntTypeTest$5((($mod.a >>> $mod.b) >>> $mod.c) >>> $mod.d);',
    '$mod.IntTypeTest$5((($mod.a << $mod.b) << $mod.c) << $mod.d);',
    '$mod.IntTypeTest$5(($mod.a + $mod.b + $mod.c + $mod.d) | 0);',
    '$mod.IntTypeTest$5(($mod.a - $mod.b - $mod.c - $mod.d) | 0);',
    '$mod.IntTypeTest$5(((((($mod.a * $mod.b) | 0) * $mod.c) | 0) * $mod.d) | 0);',
    '$mod.IntTypeTest$5(((($mod.a & $mod.b) | $mod.c) ^ $mod.d) | (($mod.c ^ 65535) >>> 1) | ($mod.c << 1));',
    '$mod.IntTypeTest$5((($mod.a + $mod.b) - (($mod.c * $mod.d) | 0)) | 0);',
    '$mod.IntTypeTest$2($mod.b | $mod.vb);',
    '$mod.IntTypeTest$2($mod.vb ^ $mod.b);',
    '$mod.IntTypeTest$2($mod.b | ($mod.vb & $mod.c));',
    '$mod.IntTypeTest$5(($mod.b + $mod.vb) | 0);',
    '$mod.IntTypeTest$5(($mod.vb - $mod.b) | 0);',
    '$mod.IntTypeTest$5(($mod.b + (($mod.vb * $mod.c) | 0)) | 0);',
    '$mod.IntTypeTest$5($mod.b | $mod.vh);',
    '$mod.IntTypeTest$5($mod.vh ^ $mod.b);',
    '$mod.IntTypeTest$5($mod.b | ($mod.vh & $mod.c));',
    '$mod.IntTypeTest$5(($mod.b + $mod.vh) | 0);',
    '$mod.IntTypeTest$5(($mod.vh - $mod.b) | 0);',
    '$mod.IntTypeTest$5(($mod.b + (($mod.vh * $mod.c) | 0)) | 0);',
    '$mod.a = $mod.b;',
    '$mod.b = $mod.c ^ $mod.d;',
    '$mod.c = ($mod.b + $mod.a) & 65535;',
    '$mod.d = $mod.c ^ 65535;',
    '$mod.a = ($mod.a + 1) & 65535;',
    '$mod.a = ($mod.a + 10) & 65535;',
    '$mod.b = ($mod.b - 1) & 65535;',
    '$mod.b = ($mod.b - 10) & 65535;',
    '$mod.Buf[0] = $mod.a;',
    '$mod.Buf[0] = ($mod.a >>> 1) & 65535;',
    '$mod.Buf[0] = ($mod.a + $mod.b) & 65535;',
    '$mod.Buf[0] = $mod.a | $mod.Buf[0];',
    '$mod.Buf[0] = ($mod.a - $mod.Buf[0]) & 65535;',
    '$mod.Buf2[0] = $mod.a;',
    '$mod.Buf2[0] = $mod.a >>> 1;',
    '$mod.Buf2[0] = $mod.a + $mod.b;',
    '$mod.Buf2[0] = $mod.a | $mod.Buf2[0];',
    '$mod.a = $mod.b | $mod.vb;',
    '$mod.a = $mod.vb ^ $mod.b;',
    '$mod.a = $mod.b | ($mod.vb & $mod.c);',
    '$mod.a = ($mod.b + $mod.vb) & 65535;',
    '$mod.a = ($mod.vb - $mod.b) & 65535;',
    '$mod.a = ($mod.b + (($mod.vb * $mod.c) | 0)) & 65535;',
    '$mod.vb = $mod.a & 255;',
    '$mod.vb = $mod.a & $mod.b & 255;',
    '$mod.vb = ($mod.a + $mod.b) & 255;',
    '$mod.a = ($mod.b | $mod.vh) & 65535;',
    '$mod.a = ($mod.vh ^ $mod.b) & 65535;',
    '$mod.a = ($mod.b | ($mod.vh & $mod.c)) & 65535;',
    '$mod.a = ($mod.b + $mod.vh) & 65535;',
    '$mod.a = ($mod.vh - $mod.b) & 65535;',
    '$mod.a = ($mod.b + (($mod.vh * $mod.c) | 0)) & 65535;',
    '$mod.vh = (($mod.a & 255) << 24) >> 24;',
    '$mod.vh = (($mod.a & $mod.b & 255) << 24) >> 24;',
    '$mod.vh = ((($mod.a + $mod.b) & 255) << 24) >> 24;',
    '']));
end;

procedure TTestOptimizations.TestOptTruncateIntegersOnOverflow_OperatorsSmallInt;
begin
  StartProgram(false);
  Add([
  '{$modeswitch externalclass}',
  '{$optimization JSTruncateIntegersOnOverflow ON}',
  'procedure IntTypeTest(v: Byte); overload; begin end;',
  'procedure IntTypeTest(v: ShortInt); overload; begin end;',
  'procedure IntTypeTest(v: Word); overload; begin end;',
  'procedure IntTypeTest(v: SmallInt); overload; begin end;',
  'procedure IntTypeTest(v: LongWord); overload; begin end;',
  'procedure IntTypeTest(v: LongInt); overload; begin end;',
  'procedure IntTypeTest(v: NativeUInt); overload; begin end;',
  'procedure IntTypeTest(v: NativeInt); overload; begin end;',
  'type',
  '  TJSInt16Array  = class external name ''Int16Array''',
  '  private',
  '    function getTypedValue(Index : NativeInt): SmallInt; external name ''[]'';',
  '    procedure setTypedValue(Index : NativeInt; AValue: SmallInt);external name ''[]'';',
  '  public',
  '    constructor new (length : NativeInt);',
  '    property values[Index : NativeInt] : SmallInt Read getTypedValue Write setTypedValue; default;',
  '  end;',
  'var',
  '  a, b, c, d : SmallInt;',
  '  vb : Byte;',
  '  vh : ShortInt;',
  '  vw : Word;',
  '  Buf : array of SmallInt;',
  '  Buf2 : TJSInt16Array;',
  'begin',
  '  IntTypeTest(a and 1);',
  '  IntTypeTest(a or 1);',
  '  IntTypeTest(a xor 1);',
  '  IntTypeTest(a shr 1);',
  '  IntTypeTest(a shl 1);',
  '  IntTypeTest(a + 1);',
  '  IntTypeTest(a - 1);',
  '  IntTypeTest(a * 1);',
  '  IntTypeTest(a div 1);',
  '  IntTypeTest(a mod 1);',
  '  IntTypeTest(a ** 1);',
  '  IntTypeTest(9 and a);',
  '  IntTypeTest(9 or a);',
  '  IntTypeTest(9 xor a);',
  '  IntTypeTest(9 shr a);',
  '  IntTypeTest(9 shl a);',
  '  IntTypeTest(9 + a);',
  '  IntTypeTest(9 - a);',
  '  IntTypeTest(9 * a);',
  '  IntTypeTest(9 div a);',
  '  IntTypeTest(9 mod a);',
  '  IntTypeTest(9 ** a);',
  '  IntTypeTest(a or $111);',
  '  IntTypeTest(a or $33333);',
  '  IntTypeTest(a or $5555555);',
  '  IntTypeTest(a + $111);',
  '  IntTypeTest(a + $33333);',
  '  IntTypeTest(a + $5555555);',
  '  IntTypeTest(a and b);',
  '  IntTypeTest(a or b);',
  '  IntTypeTest(a xor b);',
  '  IntTypeTest(a shr b);',
  '  IntTypeTest(a shl b);',
  '  IntTypeTest(not a);',
  '  IntTypeTest(a + b);',
  '  IntTypeTest(a - b);',
  '  IntTypeTest(a + -b);',
  '  IntTypeTest(a * b);',
  '  IntTypeTest(a div b);',
  '  IntTypeTest(a mod b);',
  '  IntTypeTest(a ** b);',
  '  IntTypeTest(a and b and c and d);',
  '  IntTypeTest(a or b or c or d);',
  '  IntTypeTest(a xor b xor c xor d);',
  '  IntTypeTest(a shr b shr c shr d);',
  '  IntTypeTest(a shl b shl c shl d);',
  '  IntTypeTest(a + b + c + d);',
  '  IntTypeTest(a - b - c - d);',
  '  IntTypeTest(a * b * c * d);',
  '  IntTypeTest(a and b or c xor d or not c shr 1 or c shl 1);',
  '  IntTypeTest(a + b - c * d);',
  '  IntTypeTest(b or vb);',
  '  IntTypeTest(vb xor b);',
  '  IntTypeTest(b or vb and c);',
  '  IntTypeTest(b + vb);',
  '  IntTypeTest(vb - b);',
  '  IntTypeTest(b + vb * c);',
  '  IntTypeTest(b or vh);',
  '  IntTypeTest(vh xor b);',
  '  IntTypeTest(b or vh and c);',
  '  IntTypeTest(b + vh);',
  '  IntTypeTest(vh - b);',
  '  IntTypeTest(b + vh * c);',
  '  IntTypeTest(b or vw);',
  '  IntTypeTest(vw xor b);',
  '  IntTypeTest(b or vw and c);',
  '  IntTypeTest(b + vw);',
  '  IntTypeTest(vw - b);',
  '  IntTypeTest(b + vw * c);',
  '  a := b;',
  '  b := c xor d;',
  '  c := b + a;',
  '  d := not c;',
  '  Inc(a);',
  '  Inc(a, 10);',
  '  Dec(b);',
  '  Dec(b, 10);',
  '  Buf[0] := a;',
  '  Buf[0] := a shr 1;',
  '  Buf[0] := a + b;',
  '  Buf[0] := a or Buf[0];',
  '  Buf[0] := a - Buf[0];',
  '  Buf2[0] := a;',
  '  Buf2[0] := a shr 1;',
  '  Buf2[0] := a + b;',
  '  Buf2[0] := a or Buf2[0];',
  '  a := b or vb;',
  '  a := vb xor b;',
  '  a := b or vb and c;',
  '  a := b + vb;',
  '  a := vb - b;',
  '  a := b + vb * c;',
  '  vb := a;',
  '  vb := a and b;',
  '  vb := a + b;',
  '  a := b or vh;',
  '  a := vh xor b;',
  '  a := b or vh and c;',
  '  a := b + vh;',
  '  a := vh - b;',
  '  a := b + vh * c;',
  '  vh := a;',
  '  vh := a and b;',
  '  vh := a + b;',
  '  a := b or vw;',
  '  a := vw xor b;',
  '  a := b or vw and c;',
  '  a := b + vw;',
  '  a := vw - b;',
  '  a := b + vw * c;',
  '  vw := a;',
  '  vw := a and b;',
  '  vw := a + b;',
  '']);
  ConvertProgram;
  CheckSource('TestOptTruncateIntegersOnOverflow_OperatorsSmallInt',
    LinesToStr([ // statements
    'this.IntTypeTest$3 = function (v) {',
    '};',
    'this.IntTypeTest$5 = function (v) {',
    '};',
    'this.a = 0;',
    'this.b = 0;',
    'this.c = 0;',
    'this.d = 0;',
    'this.vb = 0;',
    'this.vh = 0;',
    'this.vw = 0;',
    'this.Buf = [];',
    'this.Buf2 = null;',
    '']),
    LinesToStr([ // this.$main
    '$mod.IntTypeTest$3($mod.a & 1);',
    '$mod.IntTypeTest$3($mod.a | 1);',
    '$mod.IntTypeTest$3($mod.a ^ 1);',
    '$mod.IntTypeTest$5($mod.a >>> 1);',
    '$mod.IntTypeTest$5($mod.a << 1);',
    '$mod.IntTypeTest$5(($mod.a + 1) | 0);',
    '$mod.IntTypeTest$5(($mod.a - 1) | 0);',
    '$mod.IntTypeTest$5(($mod.a * 1) | 0);',
    '$mod.IntTypeTest$5(rtl.trunc($mod.a / 1));',
    '$mod.IntTypeTest$5($mod.a % 1);',
    '$mod.IntTypeTest$5(Math.pow($mod.a, 1));',
    '$mod.IntTypeTest$3(9 & $mod.a);',
    '$mod.IntTypeTest$3(9 | $mod.a);',
    '$mod.IntTypeTest$3(9 ^ $mod.a);',
    '$mod.IntTypeTest$5(9 >>> $mod.a);',
    '$mod.IntTypeTest$5(9 << $mod.a);',
    '$mod.IntTypeTest$5((9 + $mod.a) | 0);',
    '$mod.IntTypeTest$5((9 - $mod.a) | 0);',
    '$mod.IntTypeTest$5((9 * $mod.a) | 0);',
    '$mod.IntTypeTest$5(rtl.trunc(9 / $mod.a));',
    '$mod.IntTypeTest$5(9 % $mod.a);',
    '$mod.IntTypeTest$5(Math.pow(9, $mod.a));',
    '$mod.IntTypeTest$3($mod.a | 0x111);',
    '$mod.IntTypeTest$5($mod.a | 0x33333);',
    '$mod.IntTypeTest$5($mod.a | 0x5555555);',
    '$mod.IntTypeTest$5(($mod.a + 0x111) | 0);',
    '$mod.IntTypeTest$5(($mod.a + 0x33333) | 0);',
    '$mod.IntTypeTest$5(($mod.a + 0x5555555) | 0);',
    '$mod.IntTypeTest$3($mod.a & $mod.b);',
    '$mod.IntTypeTest$3($mod.a | $mod.b);',
    '$mod.IntTypeTest$3($mod.a ^ $mod.b);',
    '$mod.IntTypeTest$5($mod.a >>> $mod.b);',
    '$mod.IntTypeTest$5($mod.a << $mod.b);',
    '$mod.IntTypeTest$3(~$mod.a);',
    '$mod.IntTypeTest$5(($mod.a + $mod.b) | 0);',
    '$mod.IntTypeTest$5(($mod.a - $mod.b) | 0);',
    '$mod.IntTypeTest$5(($mod.a + -$mod.b) | 0);',
    '$mod.IntTypeTest$5(($mod.a * $mod.b) | 0);',
    '$mod.IntTypeTest$5(rtl.trunc($mod.a / $mod.b));',
    '$mod.IntTypeTest$5($mod.a % $mod.b);',
    '$mod.IntTypeTest$5(Math.pow($mod.a, $mod.b));',
    '$mod.IntTypeTest$3($mod.a & $mod.b & $mod.c & $mod.d);',
    '$mod.IntTypeTest$3($mod.a | $mod.b | $mod.c | $mod.d);',
    '$mod.IntTypeTest$3($mod.a ^ $mod.b ^ $mod.c ^ $mod.d);',
    '$mod.IntTypeTest$5((($mod.a >>> $mod.b) >>> $mod.c) >>> $mod.d);',
    '$mod.IntTypeTest$5((($mod.a << $mod.b) << $mod.c) << $mod.d);',
    '$mod.IntTypeTest$5(($mod.a + $mod.b + $mod.c + $mod.d) | 0);',
    '$mod.IntTypeTest$5(($mod.a - $mod.b - $mod.c - $mod.d) | 0);',
    '$mod.IntTypeTest$5(((((($mod.a * $mod.b) | 0) * $mod.c) | 0) * $mod.d) | 0);',
    '$mod.IntTypeTest$5(((($mod.a & $mod.b) | $mod.c) ^ $mod.d) | (~$mod.c >>> 1) | ($mod.c << 1));',
    '$mod.IntTypeTest$5((($mod.a + $mod.b) - (($mod.c * $mod.d) | 0)) | 0);',
    '$mod.IntTypeTest$3($mod.b | $mod.vb);',
    '$mod.IntTypeTest$3($mod.vb ^ $mod.b);',
    '$mod.IntTypeTest$3($mod.b | ($mod.vb & $mod.c));',
    '$mod.IntTypeTest$5(($mod.b + $mod.vb) | 0);',
    '$mod.IntTypeTest$5(($mod.vb - $mod.b) | 0);',
    '$mod.IntTypeTest$5(($mod.b + (($mod.vb * $mod.c) | 0)) | 0);',
    '$mod.IntTypeTest$3($mod.b | $mod.vh);',
    '$mod.IntTypeTest$3($mod.vh ^ $mod.b);',
    '$mod.IntTypeTest$3($mod.b | ($mod.vh & $mod.c));',
    '$mod.IntTypeTest$5(($mod.b + $mod.vh) | 0);',
    '$mod.IntTypeTest$5(($mod.vh - $mod.b) | 0);',
    '$mod.IntTypeTest$5(($mod.b + (($mod.vh * $mod.c) | 0)) | 0);',
    '$mod.IntTypeTest$5($mod.b | $mod.vw);',
    '$mod.IntTypeTest$5($mod.vw ^ $mod.b);',
    '$mod.IntTypeTest$5($mod.b | ($mod.vw & $mod.c));',
    '$mod.IntTypeTest$5(($mod.b + $mod.vw) | 0);',
    '$mod.IntTypeTest$5(($mod.vw - $mod.b) | 0);',
    '$mod.IntTypeTest$5(($mod.b + (($mod.vw * $mod.c) | 0)) | 0);',
    '$mod.a = $mod.b;',
    '$mod.b = $mod.c ^ $mod.d;',
    '$mod.c = ((($mod.b + $mod.a) & 65535) << 16) >> 16;',
    '$mod.d = ~$mod.c;',
    '$mod.a = ((($mod.a + 1) & 65535) << 16) >> 16;',
    '$mod.a = ((($mod.a + 10) & 65535) << 16) >> 16;',
    '$mod.b = ((($mod.b - 1) & 65535) << 16) >> 16;',
    '$mod.b = ((($mod.b - 10) & 65535) << 16) >> 16;',
    '$mod.Buf[0] = $mod.a;',
    '$mod.Buf[0] = ((($mod.a >>> 1) & 65535) << 16) >> 16;',
    '$mod.Buf[0] = ((($mod.a + $mod.b) & 65535) << 16) >> 16;',
    '$mod.Buf[0] = $mod.a | $mod.Buf[0];',
    '$mod.Buf[0] = ((($mod.a - $mod.Buf[0]) & 65535) << 16) >> 16;',
    '$mod.Buf2[0] = $mod.a;',
    '$mod.Buf2[0] = $mod.a >>> 1;',
    '$mod.Buf2[0] = $mod.a + $mod.b;',
    '$mod.Buf2[0] = $mod.a | $mod.Buf2[0];',
    '$mod.a = $mod.b | $mod.vb;',
    '$mod.a = $mod.vb ^ $mod.b;',
    '$mod.a = $mod.b | ($mod.vb & $mod.c);',
    '$mod.a = ((($mod.b + $mod.vb) & 65535) << 16) >> 16;',
    '$mod.a = ((($mod.vb - $mod.b) & 65535) << 16) >> 16;',
    '$mod.a = ((($mod.b + (($mod.vb * $mod.c) | 0)) & 65535) << 16) >> 16;',
    '$mod.vb = $mod.a & 255;',
    '$mod.vb = $mod.a & $mod.b & 255;',
    '$mod.vb = ($mod.a + $mod.b) & 255;',
    '$mod.a = $mod.b | $mod.vh;',
    '$mod.a = $mod.vh ^ $mod.b;',
    '$mod.a = $mod.b | ($mod.vh & $mod.c);',
    '$mod.a = ((($mod.b + $mod.vh) & 65535) << 16) >> 16;',
    '$mod.a = ((($mod.vh - $mod.b) & 65535) << 16) >> 16;',
    '$mod.a = ((($mod.b + (($mod.vh * $mod.c) | 0)) & 65535) << 16) >> 16;',
    '$mod.vh = (($mod.a & 255) << 24) >> 24;',
    '$mod.vh = (($mod.a & $mod.b & 255) << 24) >> 24;',
    '$mod.vh = ((($mod.a + $mod.b) & 255) << 24) >> 24;',
    '$mod.a = ((($mod.b | $mod.vw) & 65535) << 16) >> 16;',
    '$mod.a = ((($mod.vw ^ $mod.b) & 65535) << 16) >> 16;',
    '$mod.a = ((($mod.b | ($mod.vw & $mod.c)) & 65535) << 16) >> 16;',
    '$mod.a = ((($mod.b + $mod.vw) & 65535) << 16) >> 16;',
    '$mod.a = ((($mod.vw - $mod.b) & 65535) << 16) >> 16;',
    '$mod.a = ((($mod.b + (($mod.vw * $mod.c) | 0)) & 65535) << 16) >> 16;',
    '$mod.vw = $mod.a & 65535;',
    '$mod.vw = $mod.a & $mod.b & 65535;',
    '$mod.vw = ($mod.a + $mod.b) & 65535;',
    '']));
end;

procedure TTestOptimizations.TestOptTruncateIntegersOnOverflow_OperatorsLongWord;
begin
  StartProgram(false);
  Add([
  '{$modeswitch externalclass}',
  '{$optimization JSTruncateIntegersOnOverflow ON}',
  'procedure IntTypeTest(v: Byte); overload; begin end;',
  'procedure IntTypeTest(v: ShortInt); overload; begin end;',
  'procedure IntTypeTest(v: Word); overload; begin end;',
  'procedure IntTypeTest(v: SmallInt); overload; begin end;',
  'procedure IntTypeTest(v: LongWord); overload; begin end;',
  'procedure IntTypeTest(v: LongInt); overload; begin end;',
  'procedure IntTypeTest(v: NativeUInt); overload; begin end;',
  'procedure IntTypeTest(v: NativeInt); overload; begin end;',
  'type',
  '  TJSUint32Array  = class external name ''Uint32Array''',
  '  private',
  '    function getTypedValue(Index : NativeInt): LongWord; external name ''[]'';',
  '    procedure setTypedValue(Index : NativeInt; AValue: LongWord);external name ''[]'';',
  '  public',
  '    constructor new (length : NativeInt);',
  '    property values[Index : NativeInt] : LongWord Read getTypedValue Write setTypedValue; default;',
  '  end;',
  'var',
  '  a, b, c, d : LongWord;',
  '  vb : Byte;',
  '  vh : ShortInt;',
  '  vw : Word;',
  '  vm : SmallInt;',
  '  Buf : array of LongWord;',
  '  Buf2 : TJSUint32Array;',
  'begin',
  '  IntTypeTest(a and 1);',
  '  IntTypeTest(a or 1);',
  '  IntTypeTest(a xor 1);',
  '  IntTypeTest(a shr 1);',
  '  IntTypeTest(a shl 1);',
  '  IntTypeTest(a + 1);',
  '  IntTypeTest(a - 1);',
  '  IntTypeTest(a * 1);',
  '  IntTypeTest(a div 1);',
  '  IntTypeTest(a mod 1);',
  '  IntTypeTest(a ** 1);',
  '  IntTypeTest(9 and a);',
  '  IntTypeTest(9 or a);',
  '  IntTypeTest(9 xor a);',
  '  IntTypeTest(9 shr a);',
  '  IntTypeTest(9 shl a);',
  '  IntTypeTest(9 + a);',
  '  IntTypeTest(9 - a);',
  '  IntTypeTest(9 * a);',
  '  IntTypeTest(9 div a);',
  '  IntTypeTest(9 mod a);',
  '  IntTypeTest(9 ** a);',
  '  IntTypeTest(a or $111);',
  '  IntTypeTest(a or $33333);',
  '  IntTypeTest(a or $5555555);',
  '  IntTypeTest(a + $111);',
  '  IntTypeTest(a + $33333);',
  '  IntTypeTest(a + $5555555);',
  '  IntTypeTest(a and b);',
  '  IntTypeTest(a or b);',
  '  IntTypeTest(a xor b);',
  '  IntTypeTest(a shr b);',
  '  IntTypeTest(a shl b);',
  '  IntTypeTest(not a);',
  '  IntTypeTest(a + b);',
  '  IntTypeTest(a - b);',
  '  IntTypeTest(a + -b);',
  '  IntTypeTest(a * b);',
  '  IntTypeTest(a div b);',
  '  IntTypeTest(a mod b);',
  '  IntTypeTest(a ** b);',
  '  IntTypeTest(a and b and c and d);',
  '  IntTypeTest(a or b or c or d);',
  '  IntTypeTest(a xor b xor c xor d);',
  '  IntTypeTest(a shr b shr c shr d);',
  '  IntTypeTest(a shl b shl c shl d);',
  '  IntTypeTest(a + b + c + d);',
  '  IntTypeTest(a - b - c - d);',
  '  IntTypeTest(a * b * c * d);',
  '  IntTypeTest(a and b or c xor d or not c shr 1 or c shl 1);',
  '  IntTypeTest(a + b - c * d);',
  '  IntTypeTest(b or vb);',
  '  IntTypeTest(vb xor b);',
  '  IntTypeTest(b or vb and c);',
  '  IntTypeTest(b + vb);',
  '  IntTypeTest(vb - b);',
  '  IntTypeTest(b + vb * c);',
  '  IntTypeTest(b or vh);',
  '  IntTypeTest(vh xor b);',
  '  IntTypeTest(b or vh and c);',
  '  IntTypeTest(b + vh);',
  '  IntTypeTest(vh - b);',
  '  IntTypeTest(b + vh * c);',
  '  IntTypeTest(b or vw);',
  '  IntTypeTest(vw xor b);',
  '  IntTypeTest(b or vw and c);',
  '  IntTypeTest(b + vw);',
  '  IntTypeTest(vw - b);',
  '  IntTypeTest(b + vw * c);',
  '  IntTypeTest(b or vm);',
  '  IntTypeTest(vm xor b);',
  '  IntTypeTest(b or vm and c);',
  '  IntTypeTest(b + vm);',
  '  IntTypeTest(vm - b);',
  '  IntTypeTest(b + vm * c);',
  '  a := b;',
  '  b := c xor d;',
  '  c := b + a;',
  '  d := not c;',
  '  Inc(a);',
  '  Inc(a, 10);',
  '  Dec(b);',
  '  Dec(b, 10);',
  '  Buf[0] := a;',
  '  Buf[0] := a shr 1;',
  '  Buf[0] := a + b;',
  '  Buf[0] := a or Buf[0];',
  '  Buf[0] := a - Buf[0];',
  '  Buf2[0] := a;',
  '  Buf2[0] := a shr 1;',
  '  Buf2[0] := a + b;',
  '  Buf2[0] := a or Buf2[0];',
  '  a := b or vb;',
  '  a := vb xor b;',
  '  a := b or vb and c;',
  '  a := b + vb;',
  '  a := vb - b;',
  '  a := b + vb * c;',
  '  vb := a;',
  '  vb := a and b;',
  '  vb := a + b;',
  '  a := b or vh;',
  '  a := vh xor b;',
  '  a := b or vh and c;',
  '  a := b + vh;',
  '  a := vh - b;',
  '  a := b + vh * c;',
  '  vh := a;',
  '  vh := a and b;',
  '  vh := a + b;',
  '  a := b or vw;',
  '  a := vw xor b;',
  '  a := b or vw and c;',
  '  a := b + vw;',
  '  a := vw - b;',
  '  a := b + vw * c;',
  '  vw := a;',
  '  vw := a and b;',
  '  vw := a + b;',
  '  a := b or vm;',
  '  a := vm xor b;',
  '  a := b or vm and c;',
  '  a := b + vm;',
  '  a := vm - b;',
  '  a := b + vm * c;',
  '  vm := a;',
  '  vm := a and b;',
  '  vm := a + b;',
  '']);
  ConvertProgram;
  CheckSource('TestOptTruncateIntegersOnOverflow_OperatorsLongWord',
    LinesToStr([ // statements
    'this.IntTypeTest$4 = function (v) {',
    '};',
    'this.IntTypeTest$5 = function (v) {',
    '};',
    'this.IntTypeTest$7 = function (v) {',
    '};',
    'this.a = 0;',
    'this.b = 0;',
    'this.c = 0;',
    'this.d = 0;',
    'this.vb = 0;',
    'this.vh = 0;',
    'this.vw = 0;',
    'this.vm = 0;',
    'this.Buf = [];',
    'this.Buf2 = null;',
    '']),
    LinesToStr([ // this.$main
    '$mod.IntTypeTest$4($mod.a & 1);',
    '$mod.IntTypeTest$4(($mod.a | 1) >>> 0);',
    '$mod.IntTypeTest$4(($mod.a ^ 1) >>> 0);',
    '$mod.IntTypeTest$4($mod.a >>> 1);',
    '$mod.IntTypeTest$4(($mod.a << 1) >>> 0);',
    '$mod.IntTypeTest$4(($mod.a + 1) >>> 0);',
    '$mod.IntTypeTest$4(($mod.a - 1) >>> 0);',
    '$mod.IntTypeTest$4(($mod.a * 1) >>> 0);',
    '$mod.IntTypeTest$4(rtl.trunc($mod.a / 1));',
    '$mod.IntTypeTest$4($mod.a % 1);',
    '$mod.IntTypeTest$4(Math.pow($mod.a, 1));',
    '$mod.IntTypeTest$4(9 & $mod.a);',
    '$mod.IntTypeTest$4((9 | $mod.a) >>> 0);',
    '$mod.IntTypeTest$4((9 ^ $mod.a) >>> 0);',
    '$mod.IntTypeTest$5(9 >>> $mod.a);',
    '$mod.IntTypeTest$5(9 << $mod.a);',
    '$mod.IntTypeTest$4((9 + $mod.a) >>> 0);',
    '$mod.IntTypeTest$4((9 - $mod.a) >>> 0);',
    '$mod.IntTypeTest$4((9 * $mod.a) >>> 0);',
    '$mod.IntTypeTest$4(rtl.trunc(9 / $mod.a));',
    '$mod.IntTypeTest$4(9 % $mod.a);',
    '$mod.IntTypeTest$4(Math.pow(9, $mod.a));',
    '$mod.IntTypeTest$4(($mod.a | 0x111) >>> 0);',
    '$mod.IntTypeTest$4(($mod.a | 0x33333) >>> 0);',
    '$mod.IntTypeTest$4(($mod.a | 0x5555555) >>> 0);',
    '$mod.IntTypeTest$4(($mod.a + 0x111) >>> 0);',
    '$mod.IntTypeTest$4(($mod.a + 0x33333) >>> 0);',
    '$mod.IntTypeTest$4(($mod.a + 0x5555555) >>> 0);',
    '$mod.IntTypeTest$4(($mod.a & $mod.b) >>> 0);',
    '$mod.IntTypeTest$4(($mod.a | $mod.b) >>> 0);',
    '$mod.IntTypeTest$4(($mod.a ^ $mod.b) >>> 0);',
    '$mod.IntTypeTest$4($mod.a >>> $mod.b);',
    '$mod.IntTypeTest$4(($mod.a << $mod.b) >>> 0);',
    '$mod.IntTypeTest$4(~$mod.a >>> 0);',
    '$mod.IntTypeTest$4(($mod.a + $mod.b) >>> 0);',
    '$mod.IntTypeTest$4(($mod.a - $mod.b) >>> 0);',
    '$mod.IntTypeTest$7($mod.a + -$mod.b);',
    '$mod.IntTypeTest$4(($mod.a * $mod.b) >>> 0);',
    '$mod.IntTypeTest$4(rtl.trunc($mod.a / $mod.b));',
    '$mod.IntTypeTest$4($mod.a % $mod.b);',
    '$mod.IntTypeTest$4(Math.pow($mod.a, $mod.b));',
    '$mod.IntTypeTest$4(($mod.a & $mod.b & $mod.c & $mod.d) >>> 0);',
    '$mod.IntTypeTest$4(($mod.a | $mod.b | $mod.c | $mod.d) >>> 0);',
    '$mod.IntTypeTest$4(($mod.a ^ $mod.b ^ $mod.c ^ $mod.d) >>> 0);',
    '$mod.IntTypeTest$4((($mod.a >>> $mod.b) >>> $mod.c) >>> $mod.d);',
    '$mod.IntTypeTest$4(((($mod.a << $mod.b) << $mod.c) << $mod.d) >>> 0);',
    '$mod.IntTypeTest$4(($mod.a + $mod.b + $mod.c + $mod.d) >>> 0);',
    '$mod.IntTypeTest$4(($mod.a - $mod.b - $mod.c - $mod.d) >>> 0);',
    '$mod.IntTypeTest$4(((((($mod.a * $mod.b) >>> 0) * $mod.c) >>> 0) * $mod.d) >>> 0);',
    '$mod.IntTypeTest$4((((($mod.a & $mod.b) | $mod.c) ^ $mod.d) | ((~$mod.c >>> 0) >>> 1) | ($mod.c << 1)) >>> 0);',
    '$mod.IntTypeTest$4((($mod.a + $mod.b) - (($mod.c * $mod.d) >>> 0)) >>> 0);',
    '$mod.IntTypeTest$4(($mod.b | $mod.vb) >>> 0);',
    '$mod.IntTypeTest$4(($mod.vb ^ $mod.b) >>> 0);',
    '$mod.IntTypeTest$4(($mod.b | ($mod.vb & $mod.c)) >>> 0);',
    '$mod.IntTypeTest$4(($mod.b + $mod.vb) >>> 0);',
    '$mod.IntTypeTest$4(($mod.vb - $mod.b) >>> 0);',
    '$mod.IntTypeTest$4(($mod.b + (($mod.vb * $mod.c) >>> 0)) >>> 0);',
    '$mod.IntTypeTest$4(($mod.b | $mod.vh) >>> 0);',
    '$mod.IntTypeTest$4(($mod.vh ^ $mod.b) >>> 0);',
    '$mod.IntTypeTest$4(($mod.b | ($mod.vh & $mod.c)) >>> 0);',
    '$mod.IntTypeTest$7($mod.b + $mod.vh);',
    '$mod.IntTypeTest$7($mod.vh - $mod.b);',
    '$mod.IntTypeTest$7($mod.b + ($mod.vh * $mod.c));',
    '$mod.IntTypeTest$4(($mod.b | $mod.vw) >>> 0);',
    '$mod.IntTypeTest$4(($mod.vw ^ $mod.b) >>> 0);',
    '$mod.IntTypeTest$4(($mod.b | ($mod.vw & $mod.c)) >>> 0);',
    '$mod.IntTypeTest$4(($mod.b + $mod.vw) >>> 0);',
    '$mod.IntTypeTest$4(($mod.vw - $mod.b) >>> 0);',
    '$mod.IntTypeTest$4(($mod.b + (($mod.vw * $mod.c) >>> 0)) >>> 0);',
    '$mod.IntTypeTest$4(($mod.b | $mod.vm) >>> 0);',
    '$mod.IntTypeTest$4(($mod.vm ^ $mod.b) >>> 0);',
    '$mod.IntTypeTest$4(($mod.b | ($mod.vm & $mod.c)) >>> 0);',
    '$mod.IntTypeTest$7($mod.b + $mod.vm);',
    '$mod.IntTypeTest$7($mod.vm - $mod.b);',
    '$mod.IntTypeTest$7($mod.b + ($mod.vm * $mod.c));',
    '$mod.a = $mod.b;',
    '$mod.b = ($mod.c ^ $mod.d) >>> 0;',
    '$mod.c = ($mod.b + $mod.a) >>> 0;',
    '$mod.d = ~$mod.c >>> 0;',
    '$mod.a = ($mod.a + 1) >>> 0;',
    '$mod.a = ($mod.a + 10) >>> 0;',
    '$mod.b = ($mod.b - 1) >>> 0;',
    '$mod.b = ($mod.b - 10) >>> 0;',
    '$mod.Buf[0] = $mod.a;',
    '$mod.Buf[0] = $mod.a >>> 1;',
    '$mod.Buf[0] = ($mod.a + $mod.b) >>> 0;',
    '$mod.Buf[0] = ($mod.a | $mod.Buf[0]) >>> 0;',
    '$mod.Buf[0] = ($mod.a - $mod.Buf[0]) >>> 0;',
    '$mod.Buf2[0] = $mod.a;',
    '$mod.Buf2[0] = $mod.a >>> 1;',
    '$mod.Buf2[0] = $mod.a + $mod.b;',
    '$mod.Buf2[0] = $mod.a | $mod.Buf2[0];',
    '$mod.a = ($mod.b | $mod.vb) >>> 0;',
    '$mod.a = ($mod.vb ^ $mod.b) >>> 0;',
    '$mod.a = ($mod.b | ($mod.vb & $mod.c)) >>> 0;',
    '$mod.a = ($mod.b + $mod.vb) >>> 0;',
    '$mod.a = ($mod.vb - $mod.b) >>> 0;',
    '$mod.a = ($mod.b + (($mod.vb * $mod.c) >>> 0)) >>> 0;',
    '$mod.vb = $mod.a & 255;',
    '$mod.vb = $mod.a & $mod.b & 255;',
    '$mod.vb = ($mod.a + $mod.b) & 255;',
    '$mod.a = ($mod.b | $mod.vh) >>> 0;',
    '$mod.a = ($mod.vh ^ $mod.b) >>> 0;',
    '$mod.a = ($mod.b | ($mod.vh & $mod.c)) >>> 0;',
    '$mod.a = ($mod.b + $mod.vh) >>> 0;',
    '$mod.a = ($mod.vh - $mod.b) >>> 0;',
    '$mod.a = ($mod.b + ($mod.vh * $mod.c)) >>> 0;',
    '$mod.vh = (($mod.a & 255) << 24) >> 24;',
    '$mod.vh = (($mod.a & $mod.b & 255) << 24) >> 24;',
    '$mod.vh = ((($mod.a + $mod.b) & 255) << 24) >> 24;',
    '$mod.a = ($mod.b | $mod.vw) >>> 0;',
    '$mod.a = ($mod.vw ^ $mod.b) >>> 0;',
    '$mod.a = ($mod.b | ($mod.vw & $mod.c)) >>> 0;',
    '$mod.a = ($mod.b + $mod.vw) >>> 0;',
    '$mod.a = ($mod.vw - $mod.b) >>> 0;',
    '$mod.a = ($mod.b + (($mod.vw * $mod.c) >>> 0)) >>> 0;',
    '$mod.vw = $mod.a & 65535;',
    '$mod.vw = $mod.a & $mod.b & 65535;',
    '$mod.vw = ($mod.a + $mod.b) & 65535;',
    '$mod.a = ($mod.b | $mod.vm) >>> 0;',
    '$mod.a = ($mod.vm ^ $mod.b) >>> 0;',
    '$mod.a = ($mod.b | ($mod.vm & $mod.c)) >>> 0;',
    '$mod.a = ($mod.b + $mod.vm) >>> 0;',
    '$mod.a = ($mod.vm - $mod.b) >>> 0;',
    '$mod.a = ($mod.b + ($mod.vm * $mod.c)) >>> 0;',
    '$mod.vm = (($mod.a & 65535) << 16) >> 16;',
    '$mod.vm = (($mod.a & $mod.b & 65535) << 16) >> 16;',
    '$mod.vm = ((($mod.a + $mod.b) & 65535) << 16) >> 16;',
    '']));
end;

procedure TTestOptimizations.TestOptTruncateIntegersOnOverflow_OperatorsLongInt;
begin
  StartProgram(false);
  Add([
  '{$modeswitch externalclass}',
  '{$optimization JSTruncateIntegersOnOverflow ON}',
  'procedure IntTypeTest(v: Byte); overload; begin end;',
  'procedure IntTypeTest(v: ShortInt); overload; begin end;',
  'procedure IntTypeTest(v: Word); overload; begin end;',
  'procedure IntTypeTest(v: SmallInt); overload; begin end;',
  'procedure IntTypeTest(v: LongWord); overload; begin end;',
  'procedure IntTypeTest(v: LongInt); overload; begin end;',
  'procedure IntTypeTest(v: NativeUInt); overload; begin end;',
  'procedure IntTypeTest(v: NativeInt); overload; begin end;',
  'type',
  '  TJSInt32Array  = class external name ''Int32Array''',
  '  private',
  '    function getTypedValue(Index : NativeInt): LongInt; external name ''[]'';',
  '    procedure setTypedValue(Index : NativeInt; AValue: LongInt);external name ''[]'';',
  '  public',
  '    constructor new (length : NativeInt);',
  '    property values[Index : NativeInt] : LongInt Read getTypedValue Write setTypedValue; default;',
  '  end;',
  'var',
  '  a, b, c, d : LongInt;',
  '  vb : Byte;',
  '  vh : ShortInt;',
  '  vw : Word;',
  '  vm : SmallInt;',
  '  vl : LongWord;',
  '  Buf : array of LongInt;',
  '  Buf2 : TJSInt32Array;',
  'begin',
  '  IntTypeTest(a and 1);',
  '  IntTypeTest(a or 1);',
  '  IntTypeTest(a xor 1);',
  '  IntTypeTest(a shr 1);',
  '  IntTypeTest(a shl 1);',
  '  IntTypeTest(a + 1);',
  '  IntTypeTest(a - 1);',
  '  IntTypeTest(a * 1);',
  '  IntTypeTest(a div 1);',
  '  IntTypeTest(a mod 1);',
  '  IntTypeTest(a ** 1);',
  '  IntTypeTest(9 and a);',
  '  IntTypeTest(9 or a);',
  '  IntTypeTest(9 xor a);',
  '  IntTypeTest(9 shr a);',
  '  IntTypeTest(9 shl a);',
  '  IntTypeTest(9 + a);',
  '  IntTypeTest(9 - a);',
  '  IntTypeTest(9 * a);',
  '  IntTypeTest(9 div a);',
  '  IntTypeTest(9 mod a);',
  '  IntTypeTest(9 ** a);',
  '  IntTypeTest(a or $111);',
  '  IntTypeTest(a or $33333);',
  '  IntTypeTest(a or $5555555);',
  '  IntTypeTest(a + $111);',
  '  IntTypeTest(a + $33333);',
  '  IntTypeTest(a + $5555555);',
  '  IntTypeTest(a and b);',
  '  IntTypeTest(a or b);',
  '  IntTypeTest(a xor b);',
  '  IntTypeTest(a shr b);',
  '  IntTypeTest(a shl b);',
  '  IntTypeTest(not a);',
  '  IntTypeTest(a + b);',
  '  IntTypeTest(a - b);',
  '  IntTypeTest(a + -b);',
  '  IntTypeTest(a * b);',
  '  IntTypeTest(a div b);',
  '  IntTypeTest(a mod b);',
  '  IntTypeTest(a ** b);',
  '  IntTypeTest(a and b and c and d);',
  '  IntTypeTest(a or b or c or d);',
  '  IntTypeTest(a xor b xor c xor d);',
  '  IntTypeTest(a shr b shr c shr d);',
  '  IntTypeTest(a shl b shl c shl d);',
  '  IntTypeTest(a + b + c + d);',
  '  IntTypeTest(a - b - c - d);',
  '  IntTypeTest(a * b * c * d);',
  '  IntTypeTest(a and b or c xor d or not c shr 1 or c shl 1);',
  '  IntTypeTest(a + b - c * d);',
  '  IntTypeTest(b or vb);',
  '  IntTypeTest(vb xor b);',
  '  IntTypeTest(b or vb and c);',
  '  IntTypeTest(b + vb);',
  '  IntTypeTest(vb - b);',
  '  IntTypeTest(b + vb * c);',
  '  IntTypeTest(b or vh);',
  '  IntTypeTest(vh xor b);',
  '  IntTypeTest(b or vh and c);',
  '  IntTypeTest(b + vh);',
  '  IntTypeTest(vh - b);',
  '  IntTypeTest(b + vh * c);',
  '  IntTypeTest(b or vw);',
  '  IntTypeTest(vw xor b);',
  '  IntTypeTest(b or vw and c);',
  '  IntTypeTest(b + vw);',
  '  IntTypeTest(vw - b);',
  '  IntTypeTest(b + vw * c);',
  '  IntTypeTest(b or vm);',
  '  IntTypeTest(vm xor b);',
  '  IntTypeTest(b or vm and c);',
  '  IntTypeTest(b + vm);',
  '  IntTypeTest(vm - b);',
  '  IntTypeTest(b + vm * c);',
  '  IntTypeTest(b or vl);',
  '  IntTypeTest(vl xor b);',
  '  IntTypeTest(b or vl and c);',
  '  IntTypeTest(b + vl);',
  '  IntTypeTest(vl - b);',
  '  IntTypeTest(b + vl * c);',
  '  a := b;',
  '  b := c xor d;',
  '  c := b + a;',
  '  d := not c;',
  '  Inc(a);',
  '  Inc(a, 10);',
  '  Dec(b);',
  '  Dec(b, 10);',
  '  Buf[0] := a;',
  '  Buf[0] := a shr 1;',
  '  Buf[0] := a + b;',
  '  Buf[0] := a or Buf[0];',
  '  Buf[0] := a - Buf[0];',
  '  Buf2[0] := a;',
  '  Buf2[0] := a shr 1;',
  '  Buf2[0] := a + b;',
  '  Buf2[0] := a or Buf2[0];',
  '  a := b or vb;',
  '  a := vb xor b;',
  '  a := b or vb and c;',
  '  a := b + vb;',
  '  a := vb - b;',
  '  a := b + vb * c;',
  '  vb := a;',
  '  vb := a and b;',
  '  vb := a + b;',
  '  a := b or vh;',
  '  a := vh xor b;',
  '  a := b or vh and c;',
  '  a := b + vh;',
  '  a := vh - b;',
  '  a := b + vh * c;',
  '  vh := a;',
  '  vh := a and b;',
  '  vh := a + b;',
  '  a := b or vw;',
  '  a := vw xor b;',
  '  a := b or vw and c;',
  '  a := b + vw;',
  '  a := vw - b;',
  '  a := b + vw * c;',
  '  vw := a;',
  '  vw := a and b;',
  '  vw := a + b;',
  '  a := b or vm;',
  '  a := vm xor b;',
  '  a := b or vm and c;',
  '  a := b + vm;',
  '  a := vm - b;',
  '  a := b + vm * c;',
  '  vm := a;',
  '  vm := a and b;',
  '  vm := a + b;',
  '  a := b or vl;',
  '  a := vl xor b;',
  '  a := b or vl and c;',
  '  a := b + vl;',
  '  a := vl - b;',
  '  a := b + vl * c;',
  '  vl := a;',
  '  vl := a and b;',
  '  vl := a + b;',
  '']);
  ConvertProgram;
  CheckSource('TestOptTruncateIntegersOnOverflow_OperatorsLongInt',
    LinesToStr([ // statements
    'this.IntTypeTest$4 = function (v) {',
    '};',
    'this.IntTypeTest$5 = function (v) {',
    '};',
    'this.IntTypeTest$7 = function (v) {',
    '};',
    'this.a = 0;',
    'this.b = 0;',
    'this.c = 0;',
    'this.d = 0;',
    'this.vb = 0;',
    'this.vh = 0;',
    'this.vw = 0;',
    'this.vm = 0;',
    'this.vl = 0;',
    'this.Buf = [];',
    'this.Buf2 = null;',
    '']),
    LinesToStr([ // this.$main
    '$mod.IntTypeTest$5($mod.a & 1);',
    '$mod.IntTypeTest$5($mod.a | 1);',
    '$mod.IntTypeTest$5($mod.a ^ 1);',
    '$mod.IntTypeTest$5($mod.a >>> 1);',
    '$mod.IntTypeTest$5($mod.a << 1);',
    '$mod.IntTypeTest$5(($mod.a + 1) | 0);',
    '$mod.IntTypeTest$5(($mod.a - 1) | 0);',
    '$mod.IntTypeTest$5(($mod.a * 1) | 0);',
    '$mod.IntTypeTest$5(rtl.trunc($mod.a / 1));',
    '$mod.IntTypeTest$5($mod.a % 1);',
    '$mod.IntTypeTest$5(Math.pow($mod.a, 1));',
    '$mod.IntTypeTest$5(9 & $mod.a);',
    '$mod.IntTypeTest$5(9 | $mod.a);',
    '$mod.IntTypeTest$5(9 ^ $mod.a);',
    '$mod.IntTypeTest$5(9 >>> $mod.a);',
    '$mod.IntTypeTest$5(9 << $mod.a);',
    '$mod.IntTypeTest$5((9 + $mod.a) | 0);',
    '$mod.IntTypeTest$5((9 - $mod.a) | 0);',
    '$mod.IntTypeTest$5((9 * $mod.a) | 0);',
    '$mod.IntTypeTest$5(rtl.trunc(9 / $mod.a));',
    '$mod.IntTypeTest$5(9 % $mod.a);',
    '$mod.IntTypeTest$5(Math.pow(9, $mod.a));',
    '$mod.IntTypeTest$5($mod.a | 0x111);',
    '$mod.IntTypeTest$5($mod.a | 0x33333);',
    '$mod.IntTypeTest$5($mod.a | 0x5555555);',
    '$mod.IntTypeTest$5(($mod.a + 0x111) | 0);',
    '$mod.IntTypeTest$5(($mod.a + 0x33333) | 0);',
    '$mod.IntTypeTest$5(($mod.a + 0x5555555) | 0);',
    '$mod.IntTypeTest$5($mod.a & $mod.b);',
    '$mod.IntTypeTest$5($mod.a | $mod.b);',
    '$mod.IntTypeTest$5($mod.a ^ $mod.b);',
    '$mod.IntTypeTest$5($mod.a >>> $mod.b);',
    '$mod.IntTypeTest$5($mod.a << $mod.b);',
    '$mod.IntTypeTest$5(~$mod.a);',
    '$mod.IntTypeTest$5(($mod.a + $mod.b) | 0);',
    '$mod.IntTypeTest$5(($mod.a - $mod.b) | 0);',
    '$mod.IntTypeTest$5(($mod.a + -$mod.b) | 0);',
    '$mod.IntTypeTest$5(($mod.a * $mod.b) | 0);',
    '$mod.IntTypeTest$5(rtl.trunc($mod.a / $mod.b));',
    '$mod.IntTypeTest$5($mod.a % $mod.b);',
    '$mod.IntTypeTest$5(Math.pow($mod.a, $mod.b));',
    '$mod.IntTypeTest$5($mod.a & $mod.b & $mod.c & $mod.d);',
    '$mod.IntTypeTest$5($mod.a | $mod.b | $mod.c | $mod.d);',
    '$mod.IntTypeTest$5($mod.a ^ $mod.b ^ $mod.c ^ $mod.d);',
    '$mod.IntTypeTest$5((($mod.a >>> $mod.b) >>> $mod.c) >>> $mod.d);',
    '$mod.IntTypeTest$5((($mod.a << $mod.b) << $mod.c) << $mod.d);',
    '$mod.IntTypeTest$5(($mod.a + $mod.b + $mod.c + $mod.d) | 0);',
    '$mod.IntTypeTest$5(($mod.a - $mod.b - $mod.c - $mod.d) | 0);',
    '$mod.IntTypeTest$5(((((($mod.a * $mod.b) | 0) * $mod.c) | 0) * $mod.d) | 0);',
    '$mod.IntTypeTest$5(((($mod.a & $mod.b) | $mod.c) ^ $mod.d) | (~$mod.c >>> 1) | ($mod.c << 1));',
    '$mod.IntTypeTest$5((($mod.a + $mod.b) - (($mod.c * $mod.d) | 0)) | 0);',
    '$mod.IntTypeTest$5($mod.b | $mod.vb);',
    '$mod.IntTypeTest$5($mod.vb ^ $mod.b);',
    '$mod.IntTypeTest$5($mod.b | ($mod.vb & $mod.c));',
    '$mod.IntTypeTest$5(($mod.b + $mod.vb) | 0);',
    '$mod.IntTypeTest$5(($mod.vb - $mod.b) | 0);',
    '$mod.IntTypeTest$5(($mod.b + (($mod.vb * $mod.c) | 0)) | 0);',
    '$mod.IntTypeTest$5($mod.b | $mod.vh);',
    '$mod.IntTypeTest$5($mod.vh ^ $mod.b);',
    '$mod.IntTypeTest$5($mod.b | ($mod.vh & $mod.c));',
    '$mod.IntTypeTest$5(($mod.b + $mod.vh) | 0);',
    '$mod.IntTypeTest$5(($mod.vh - $mod.b) | 0);',
    '$mod.IntTypeTest$5(($mod.b + (($mod.vh * $mod.c) | 0)) | 0);',
    '$mod.IntTypeTest$5($mod.b | $mod.vw);',
    '$mod.IntTypeTest$5($mod.vw ^ $mod.b);',
    '$mod.IntTypeTest$5($mod.b | ($mod.vw & $mod.c));',
    '$mod.IntTypeTest$5(($mod.b + $mod.vw) | 0);',
    '$mod.IntTypeTest$5(($mod.vw - $mod.b) | 0);',
    '$mod.IntTypeTest$5(($mod.b + (($mod.vw * $mod.c) | 0)) | 0);',
    '$mod.IntTypeTest$5($mod.b | $mod.vm);',
    '$mod.IntTypeTest$5($mod.vm ^ $mod.b);',
    '$mod.IntTypeTest$5($mod.b | ($mod.vm & $mod.c));',
    '$mod.IntTypeTest$5(($mod.b + $mod.vm) | 0);',
    '$mod.IntTypeTest$5(($mod.vm - $mod.b) | 0);',
    '$mod.IntTypeTest$5(($mod.b + (($mod.vm * $mod.c) | 0)) | 0);',
    '$mod.IntTypeTest$4(($mod.b | $mod.vl) >>> 0);',
    '$mod.IntTypeTest$4(($mod.vl ^ $mod.b) >>> 0);',
    '$mod.IntTypeTest$4(($mod.b | ($mod.vl & $mod.c)) >>> 0);',
    '$mod.IntTypeTest$7($mod.b + $mod.vl);',
    '$mod.IntTypeTest$7($mod.vl - $mod.b);',
    '$mod.IntTypeTest$7($mod.b + ($mod.vl * $mod.c));',
    '$mod.a = $mod.b;',
    '$mod.b = $mod.c ^ $mod.d;',
    '$mod.c = ($mod.b + $mod.a) | 0;',
    '$mod.d = ~$mod.c;',
    '$mod.a = ($mod.a + 1) | 0;',
    '$mod.a = ($mod.a + 10) | 0;',
    '$mod.b = ($mod.b - 1) | 0;',
    '$mod.b = ($mod.b - 10) | 0;',
    '$mod.Buf[0] = $mod.a;',
    '$mod.Buf[0] = $mod.a >>> 1;',
    '$mod.Buf[0] = ($mod.a + $mod.b) | 0;',
    '$mod.Buf[0] = $mod.a | $mod.Buf[0];',
    '$mod.Buf[0] = ($mod.a - $mod.Buf[0]) | 0;',
    '$mod.Buf2[0] = $mod.a;',
    '$mod.Buf2[0] = $mod.a >>> 1;',
    '$mod.Buf2[0] = $mod.a + $mod.b;',
    '$mod.Buf2[0] = $mod.a | $mod.Buf2[0];',
    '$mod.a = $mod.b | $mod.vb;',
    '$mod.a = $mod.vb ^ $mod.b;',
    '$mod.a = $mod.b | ($mod.vb & $mod.c);',
    '$mod.a = ($mod.b + $mod.vb) | 0;',
    '$mod.a = ($mod.vb - $mod.b) | 0;',
    '$mod.a = ($mod.b + (($mod.vb * $mod.c) | 0)) | 0;',
    '$mod.vb = $mod.a & 255;',
    '$mod.vb = $mod.a & $mod.b & 255;',
    '$mod.vb = ($mod.a + $mod.b) & 255;',
    '$mod.a = $mod.b | $mod.vh;',
    '$mod.a = $mod.vh ^ $mod.b;',
    '$mod.a = $mod.b | ($mod.vh & $mod.c);',
    '$mod.a = ($mod.b + $mod.vh) | 0;',
    '$mod.a = ($mod.vh - $mod.b) | 0;',
    '$mod.a = ($mod.b + (($mod.vh * $mod.c) | 0)) | 0;',
    '$mod.vh = (($mod.a & 255) << 24) >> 24;',
    '$mod.vh = (($mod.a & $mod.b & 255) << 24) >> 24;',
    '$mod.vh = ((($mod.a + $mod.b) & 255) << 24) >> 24;',
    '$mod.a = $mod.b | $mod.vw;',
    '$mod.a = $mod.vw ^ $mod.b;',
    '$mod.a = $mod.b | ($mod.vw & $mod.c);',
    '$mod.a = ($mod.b + $mod.vw) | 0;',
    '$mod.a = ($mod.vw - $mod.b) | 0;',
    '$mod.a = ($mod.b + (($mod.vw * $mod.c) | 0)) | 0;',
    '$mod.vw = $mod.a & 65535;',
    '$mod.vw = $mod.a & $mod.b & 65535;',
    '$mod.vw = ($mod.a + $mod.b) & 65535;',
    '$mod.a = $mod.b | $mod.vm;',
    '$mod.a = $mod.vm ^ $mod.b;',
    '$mod.a = $mod.b | ($mod.vm & $mod.c);',
    '$mod.a = ($mod.b + $mod.vm) | 0;',
    '$mod.a = ($mod.vm - $mod.b) | 0;',
    '$mod.a = ($mod.b + (($mod.vm * $mod.c) | 0)) | 0;',
    '$mod.vm = (($mod.a & 65535) << 16) >> 16;',
    '$mod.vm = (($mod.a & $mod.b & 65535) << 16) >> 16;',
    '$mod.vm = ((($mod.a + $mod.b) & 65535) << 16) >> 16;',
    '$mod.a = $mod.b | $mod.vl | 0;',
    '$mod.a = ($mod.vl ^ $mod.b) | 0;',
    '$mod.a = $mod.b | ($mod.vl & $mod.c) | 0;',
    '$mod.a = ($mod.b + $mod.vl) | 0;',
    '$mod.a = ($mod.vl - $mod.b) | 0;',
    '$mod.a = ($mod.b + ($mod.vl * $mod.c)) | 0;',
    '$mod.vl = $mod.a >>> 0;',
    '$mod.vl = ($mod.a & $mod.b) >>> 0;',
    '$mod.vl = ($mod.a + $mod.b) >>> 0;',
    '']));
end;

Initialization
  RegisterTests([TTestOptimizations]);
end.

