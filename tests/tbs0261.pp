program bug0261;

{ test for operator overloading }
{ Copyright (c) 1999 Lourens Veen }
{ why doesn't this work? }
uses
  erroru,
  tbs0261a;


var a : mythingy;
    b : myotherthingy;
    c : mythirdthingy;
begin
  a.x:=55;
  a.y:=45;
  a.c:=7;
  b:=a;
  c:=a;
  if b.d<>c.e then
    begin
      Writeln('Error in assignment overloading');
      Halt(1);
    end;
  if b<>c then
    begin
      Writeln('Error in equal overloading');
      Halt(1);
    end;
  Writeln('Sizeof(mythirdthingy)=',sizeof(mythirdthingy));
  Writeln('Sizeof(mynewthingy)=',sizeof(mynewthingy));
end.
