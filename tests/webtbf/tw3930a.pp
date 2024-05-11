{ Gives under Kylix:

tw3930a.pp(22) Error: Incompatible types: 'TMyStringList' and 'TStringList'

Used to not compile, now should compile
}

{$ifdef fpc}
{$mode objfpc}
{$endif}
uses
  Classes;
  
type
  TMyStringList = type TStringlist;
  
var
  list : TMyStringList;

begin
  list:=TMyStringList.Create;
end.

    
