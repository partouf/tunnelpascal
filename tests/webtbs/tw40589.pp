{ %RESULT=202 }
{ %opt=gl }

{$mode objfpc}

type
  TForm1 = class
    procedure Button1Click(Sender: TObject);
  end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Button1Click(self);
end;

var
  Form1 : TForm1;

begin
  Form1:=TForm1.Create;
  Form1.Button1Click(nil);
end.
