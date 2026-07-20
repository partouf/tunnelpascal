{ Inline variables must have *scope* lifetime, not procedure lifetime.

  A managed inline variable (here an interface, whose refcount drop is
  observable via the destructor) must be finalised when its enclosing block
  ends — not deferred to the end of the surrounding routine.

  Covers:
   - two sibling blocks: the first object dies before the second is created,
     so the two are never simultaneously alive
   - nested blocks: inner finalised before outer (reverse declaration order)
   - loop body: one construct/destruct pair per iteration
   - exception escaping a block still finalises (implicit try/finally)
   - `exit` out of a block still finalises
   - unmanaged inline vars are unaffected

  Each case builds an event log and compares it to the expected sequence.

  Exit code 0 = pass; halt(N) marks which check failed. }

program tinlinevarscope1;

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

{ two sibling blocks must not overlap }
procedure SiblingBlocks;
begin
  begin
    var t: ITest := TTest.Create('A');
    Note('use' + IntToStr(t.Value));
  end;
  Note('betweeen');
  begin
    var t: ITest := TTest.Create('B');
    Note('use' + IntToStr(t.Value));
  end;
  Note('after');
end;

{ Inner block finalised before outer. The outer variable is declared directly
  in the procedure *body* block — the same code path a program's main block,
  and a unit's initialization/finalization section, go through — rather than
  in a redundant extra begin/end. }
procedure NestedBlocks;
begin
  var outerv: ITest := TTest.Create('O');

  begin
    var innerv: ITest := TTest.Create('I');
    Note('inner');
  end;

  Note('outer');
end;

{ as above, but the body-level variable is declared *after* the nested block,
  so the declaration order is reversed relative to the block nesting }
procedure NestedBlocksDeclaredAfter;
begin
  begin
    var innerv: ITest := TTest.Create('I2');
    Note('inner2');
  end;

  var outerv: ITest := TTest.Create('O2');
  Note('outer2');
end;

{ one pair per iteration }
procedure LoopBody;
var
  i: Integer;
begin
  for i := 1 to 2 do
  begin
    var t: ITest := TTest.Create('L');
    Note('iter');
  end;
  Note('done');
end;

{ exception escaping the block must still finalise }
procedure ExceptionEscape;
begin
  try
    begin
      var t: ITest := TTest.Create('E');
      raise Exception.Create('boom');
    end;
  except
    Note('caught');
  end;
end;

{ exit out of a block must still finalise }
procedure ExitEscape;
begin
  begin
    var t: ITest := TTest.Create('X');
    Note('before-exit');
    exit;
  end;
end;

{ Classic procedure-level vars keep *procedure* lifetime even when they are
  assigned inside a nested block: Pascal has no block-level var section, so
  the declaration belongs to the routine. Scope lifetime must apply only to
  inline vars, so both objects here stay alive until the routine returns. }
procedure ClassicVarsUnchanged;
var
  a, b: ITest;
begin
  begin
    a := TTest.Create('CA');
    Note('use-a');
  end;
  begin
    b := TTest.Create('CB');
    Note('use-b');
  end;
  Note('routine-end');
end;

{ unmanaged inline vars need no finalisation and must not be disturbed }
procedure Unmanaged;
var
  total: Integer;
begin
  total := 0;
  begin
    var n: Integer := 5;
    var c: Char := 'x';
    total := total + n;
    if c <> 'x' then
      halt(90);
  end;
  if total <> 5 then
    halt(91);
  Note('unmanaged-ok');
end;

begin
  log := '';
  SiblingBlocks;
  Check('SiblingBlocks', '+A;use42;-A;betweeen;+B;use42;-B;after;', 1);

  log := '';
  NestedBlocks;
  Check('NestedBlocks', '+O;+I;inner;-I;outer;-O;', 2);

  log := '';
  NestedBlocksDeclaredAfter;
  Check('NestedBlocksDeclaredAfter', '+I2;inner2;-I2;+O2;outer2;-O2;', 8);

  log := '';
  LoopBody;
  Check('LoopBody', '+L;iter;-L;+L;iter;-L;done;', 3);

  log := '';
  ExceptionEscape;
  Check('ExceptionEscape', '+E;-E;caught;', 4);

  log := '';
  ExitEscape;
  Check('ExitEscape', '+X;before-exit;-X;', 5);

  log := '';
  Unmanaged;
  Check('Unmanaged', 'unmanaged-ok;', 6);

  { contrast: classic vars are NOT block scoped, both die at routine exit }
  log := '';
  ClassicVarsUnchanged;
  Check('ClassicVarsUnchanged', '+CA;use-a;+CB;use-b;routine-end;-CA;-CB;', 7);

  WriteLn('ok');
end.
