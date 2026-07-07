{ Delphi 13 NameOf intrinsic - positive runtime test.

  NameOf(X) yields the *unqualified* declared name of X as a compile-time
  string constant (original source casing), without evaluating X.

  Covers: local var, global var, type name, field via instance, and use in
  a constant declaration (proving it folds to a string constant). Results are
  routed through runtime string variables so the comparisons are not
  constant-folded (see tnameof3 for the fold assertion).

  Exit code 0 = pass; halt(N) marks which check failed. }

program tnameof1;

{$mode delphi}

type
  TFoo = class
    Bar: Integer;
  end;

var
  gCounter: Integer;

const
  { NameOf yields a compile-time string constant, usable in const context }
  CName = NameOf(gCounter);

procedure Check;
var
  localVar: Integer;
  f: TFoo;
  s: string;
begin
  s := NameOf(localVar);  if s <> 'localVar' then halt(1);
  s := NameOf(f);         if s <> 'f' then halt(2);
  { unqualified: only the last component is returned }
  s := NameOf(f.Bar);     if s <> 'Bar' then halt(3);
end;

var
  s: string;
begin
  s := NameOf(gCounter);  if s <> 'gCounter' then halt(4);
  s := NameOf(TFoo);      if s <> 'TFoo' then halt(5);
  s := CName;             if s <> 'gCounter' then halt(6);
  Check;
  WriteLn('ok');
end.
