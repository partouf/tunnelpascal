{ %cpu=avr }
{ %target=embedded }
{ %fail }

program tfailwritetoflash;

type
  myword_progmem = type word; section '.progmem';

// Should fail here first
var
  wp1: myword_progmem = 1234;
  
  b: byte;

begin
  // Write to flash not supported
  wp1 := 1;
end.

