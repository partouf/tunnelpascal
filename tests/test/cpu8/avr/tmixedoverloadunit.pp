{ %cpu=avr }
{ %target=embedded }
{ %OPT=-B } // to force rebuild of unit

program tmixedoverloadunit;

uses
  umixedoverload;

var
  tmp: byte;
  b1: byte_e;

begin
  writeTest(tmp);
  if tmp <> 123 then
    halt(1);

  writeTest(b1);
  if b1 <> 234 then
    halt(2);
end.
