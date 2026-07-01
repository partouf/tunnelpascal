{ %FAIL }

{ Delphi 13 `interface` generic constraint: the type argument must be an
  interface type. Specializing on a non-interface (here Integer) must be a
  compile-time error. }

program tgenconstr2;

{$mode delphi}

type
  TWrap<T: interface> = class
    FIntf: T;
  end;

var
  w: TWrap<Integer>;   { Integer is not an interface -> must fail }
begin
  w := nil;
end.
