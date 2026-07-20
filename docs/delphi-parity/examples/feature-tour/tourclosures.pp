{ Function references, closures, and inline variables.

  - `reference to function` + anonymous method capturing an enclosing local.
  - Inline `var` declarations with type inference, including a loop variable.

  CAUTION: an inline var of a *managed* type (string, dynamic array, …)
  declared directly in the main `begin`/`end.` block of a program miscompiles
  on 1.9.1 — it faults with a runaway "Runtime error 216" loop. Inside a
  routine, as below, it is fine. Repro: https://compiler-explorer.com/z/nrMefx7n3 }

unit tourclosures;

interface

procedure Run;

type
  TFn = reference to function(x: Integer): Integer;

implementation

function MakeAdder(n: Integer): TFn;
begin
  Result := function(x: Integer): Integer
            begin
              Result := x + n;   // captures n
            end;
end;

procedure Run;
var
  add5: TFn;
begin
  WriteLn('7. Function references + closures');
  add5 := MakeAdder(5);
  WriteLn('    MakeAdder(5)(10) = ', add5(10));

  WriteLn('8. Inline variable declarations + inference');
  var msg := 'inferred string';
  var total := 0;
  for var i := 1 to 10 do
    total := total + i;
  WriteLn('    ', msg, ', sum 1..10 = ', total);
end;

end.
