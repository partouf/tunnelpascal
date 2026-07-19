program smoketest;
{$mode delphi}

{ Compiled by verify-package.sh against the *packaged* bin/fpc, with any system
  FPC scrubbed from $PATH. It deliberately uses no unit, so it works against
  both the regular and the dotted (bleeding-edge) RTL, while still needing the
  system unit -- which proves the shipped fpc.cfg resolves the unit paths.

  The inline "if" expression is a TunnelPascal extension (PR #10), so this only
  compiles if bin/fpc really drove the bundled 3.3.1 compiler and not a stock
  3.2.2 that happened to be installed on the machine. }

var
  x: Integer = 5;
begin
  Writeln(if x > 3 then 'big' else 'small');
end.
