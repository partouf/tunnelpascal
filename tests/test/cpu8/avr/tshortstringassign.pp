{ %cpu=avr }
{ %target=embedded }

program tshortstringassign;

{$writeableconst off}
const
  sf: shortstring = 'trewq'; section '.progmem';

var
  se: shortstring = 'qweryt'; section '.eeprom';
  s: shortstring = '';

begin
  s := se;
  if s[1] <> 'q' then
    halt(1);
  if length(s) <> 6 then
    halt(2);

  s := sf;
  if s[1] <> 't' then
    halt(3);
  if length(s) <> 5 then
    halt(4);
end.
