{ Scope lifetime for inline variables declared as the *sole* statement of a
  control-flow construct, i.e. without an enclosing begin/end block.

  Each of these opens a nested scope of its own, so a managed inline variable
  declared directly in one must be initialised and finalised around that
  construct. Before scope lifetimes these symbols were initialised by nothing
  at all in a program's main block, which faulted with Runtime error 216 (and
  only appeared to work when the stack slot happened to be zero).

  Companion to tinlinevarscope1, which covers begin/end blocks.

  Exit code 0 = pass; halt(N) marks which check failed. }

program tinlinevarscope2;

{$mode delphi}

uses
  sysutils;

type
  ITest = interface
    function Value: Integer;
  end;

  TTest = class(TInterfacedObject, ITest)
  private
    FName: string;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;
    function Value: Integer;
  end;

var
  log: string;

procedure Note(const s: string);
begin
  log := log + s + ';';
end;

constructor TTest.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
  Note('+' + FName);
end;

destructor TTest.Destroy;
begin
  Note('-' + FName);
  inherited Destroy;
end;

function TTest.Value: Integer;
begin
  Result := 42;
end;

procedure Check(const aWhat, aExpected: string; aCode: Integer);
begin
  if log <> aExpected then
    begin
      WriteLn('FAIL ', aWhat);
      WriteLn('  expected: ', aExpected);
      WriteLn('  actual  : ', log);
      halt(aCode);
    end;
end;

procedure IfBranch;
begin
  if 1 = 1 then
    var t: ITest := TTest.Create('IF');
  Note('after');
end;

procedure ElseBranch;
var
  n: Integer;
begin
  n := 2;                 { via a variable so the branch is not folded away }
  if n = 1 then
    Note('never')
  else
    var t: ITest := TTest.Create('ELSE');
  Note('after');
end;

procedure CaseBranch;
var
  n: Integer;
begin
  n := 1;
  case n of
    1: var t: ITest := TTest.Create('CASE');
  else
    Note('never');
  end;
  Note('after');
end;

procedure CaseElse;
var
  n: Integer;
begin
  n := 9;
  case n of
    1: Note('never');
  else
    var t: ITest := TTest.Create('CASEELSE');
  end;
  Note('after');
end;

procedure WhileBody;
var
  i: Integer;
begin
  i := 0;
  while i < 2 do
  begin
    Inc(i);
    var t: ITest := TTest.Create('W');
  end;
  Note('after');
end;

procedure RepeatBody;
var
  i: Integer;
begin
  i := 0;
  repeat
    Inc(i);
    var t: ITest := TTest.Create('R');
  until i >= 2;
  Note('after');
end;

type
  THolder = class
    Field: Integer;
  end;

procedure WithBody;
var
  h: THolder;
begin
  h := THolder.Create;
  try
    with h do
      var t: ITest := TTest.Create('WITH');
    Note('after');
  finally
    h.Free;
  end;
end;

procedure TryAndFinallyBlocks;
begin
  try
    var t: ITest := TTest.Create('TRY');
    Note('inbody');
  finally
    var f: ITest := TTest.Create('FIN');
    Note('infinally');
  end;
  Note('after');
end;

procedure ExceptHandler;
begin
  try
    raise Exception.Create('boom');
  except
    var t: ITest := TTest.Create('EXC');
    Note('handled');
  end;
  Note('after');
end;

procedure OnHandler;
begin
  try
    raise Exception.Create('boom');
  except
    on E: Exception do
      var t: ITest := TTest.Create('ON');
  end;
  Note('after');
end;

begin
  log := ''; IfBranch;
  Check('IfBranch', '+IF;-IF;after;', 1);

  log := ''; ElseBranch;
  Check('ElseBranch', '+ELSE;-ELSE;after;', 2);

  log := ''; CaseBranch;
  Check('CaseBranch', '+CASE;-CASE;after;', 3);

  log := ''; CaseElse;
  Check('CaseElse', '+CASEELSE;-CASEELSE;after;', 4);

  log := ''; WhileBody;
  Check('WhileBody', '+W;-W;+W;-W;after;', 5);

  log := ''; RepeatBody;
  Check('RepeatBody', '+R;-R;+R;-R;after;', 6);

  log := ''; WithBody;
  Check('WithBody', '+WITH;-WITH;after;', 7);

  log := ''; TryAndFinallyBlocks;
  Check('TryAndFinallyBlocks', '+TRY;inbody;-TRY;+FIN;infinally;-FIN;after;', 8);

  log := ''; ExceptHandler;
  Check('ExceptHandler', '+EXC;handled;-EXC;after;', 9);

  log := ''; OnHandler;
  Check('OnHandler', '+ON;-ON;after;', 10);

  WriteLn('ok');
end.
