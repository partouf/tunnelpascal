{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}
program TestAnonFunc73;

type
  TProcWithArgs = reference to procedure(AArg:Integer);
  TProc = reference to procedure;

  { Antitest }

  TSomeObject=class(TObject)
    FValue:String;
    procedure SetValue(APrefix:String);
    procedure execute(AProc:TProc);
  end;

  { TCaller }

  TCaller = class
    public
      procedure execute(AProc:TProcWithArgs);
  end;

  Var
    indent:integer=0;

procedure CallProc(AProc:TProcWithArgs);
begin
  AProc(1);
end;

{ TSomeObject }
procedure TSomeObject.SetValue(APrefix:String);
begin
  FValue:='This ist silly';
  Writeln(' ':indent + 1, 'nesting depth ',indent,': Text=', APrefix+FValue);
end;

procedure TSomeObject.execute(AProc: TProc);
begin
  AProc;
end;

{ TCaller }

procedure TCaller.execute(AProc: TProcWithArgs);
begin
  AProc(1);
end;

function AnonymousTest:integer;
Var
  Caller:TCaller;
  P:TProcWithArgs;
  Count:Integer;
begin
  Count:=0;

  p:=procedure(AInt:Integer)
  begin
    Writeln(' ':indent + 1, 'nesting depth ',indent,': Count(', Count, ')+AInt(', AInt, ')=', Count+AInt);
    Inc(Count, AInt);
  end;

  // inline
  CallProc(procedure(AInt:Integer)
  begin
    P(AInt);    // 1
  end);

  Caller:=TCaller.Create;

  // inline
  Caller.Execute(procedure(AInt:Integer)
    begin
      P(AInt);  // 2
    end);

  // call reference
  Caller.Execute(p);    // 3

  // dito
  With TCaller.Create do
  begin
    Execute(p); // 4
  end;

  // dito
  With TCaller.Create do Execute(p); // 5

  With TCaller.Create do
    Execute(procedure(AInt:Integer)
    begin
      inc(indent);

      P(AInt); // 6
    
      // inline, nested
      With TCaller.Create do
        Execute(procedure(AInt:Integer)
        begin
          inc(indent);
          P(AInt); // 7
          dec(indent);
        end);

      With TCaller.Create do
      begin
        Execute(procedure(AInt:Integer)
        begin
          inc(indent);
          P(AInt); // 8
          dec(indent);
        end);

        // inline, nested
        With TCaller.Create do
        begin
          inc(indent);
          Execute(procedure(AInt:Integer)
          begin
            P(AInt); // 9
          end);

          // call reference, nested
          With TCaller.Create do
          begin
            Execute(P); // 10
          end;
          dec(indent);
        end;
      end;
      P(AInt); // 11
      dec(indent);
    end);

  With TCaller.Create do Execute(p); // 12

  With TSomeObject.Create do
  begin
    inc(indent);
    SetValue('Prefix 1 ');
    With TCaller.Create do
      Execute(procedure(AInt:Integer)
      begin
        inc(indent);
        P(AInt + 1); // 14
        With TSomeObject.Create do
        begin
          SetValue('Prefix 2 ');
          With TCaller.Create do
            Execute(P); // 15
          TCaller.Create.Execute(procedure(AInt:Integer)
          begin
            inc(indent);
            P(AInt + 1); // 17
            dec(indent);
          end);
        end;        
        dec(indent);
      end);
    dec(indent);
  end;

  With TCaller.Create do 
    Execute(procedure(AInt:integer)
    begin
      inc(indent);
      P(AInt+1); // 19

      With TSomeObject.Create do
      begin
        inc(indent);
        SetValue('Prefix 3 ');
        dec(indent);
      end;

      dec(indent);
    end);

  With TSomeObject.Create do execute(procedure
  begin
    With TSomeObject.Create do execute(procedure
    begin
      inc(indent);    
      With TSomeObject.Create do SetValue('Prefix 4 ');
      inc(indent);  
      TSomeObject.Create.SetValue('Prefix 5 ');      
      dec(indent, 2);
    end);
    TSomeObject.Create.SetValue('Prefix 6 ');
  end);

  P(1); // 20
  Result:=Count;
end;

Var 
  RetValue:Integer;
begin
  RetValue:=AnonymousTest;
  if (RetValue <> 20) then 
  Begin
    Writeln('Expected result 20, got ', RetValue);
    Halt(1);
  End;
end.
