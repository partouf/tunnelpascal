{ %RESULT=217 }
{****************************************************************}
{  CODE GENERATOR TEST PROGRAM                                   }
{  By Carl Eric Codere                                           }
{****************************************************************}
{ NODE TESTED : secondraise()                                    }
{****************************************************************}
{ PRE-REQUISITES: secondload()                                   }
{                 secondassign()                                 }
{                 secondtypeconv()                               }
{                 secondtryexcept()                              }
{                 secondcalln()                                  }
{                 secondadd()                                    }
{****************************************************************}
{ DEFINES:                                                       }
{            FPC     = Target is FreePascal compiler             }
{****************************************************************}
{ REMARKS : Tested with Delphi 3 as reference implementation     }
{****************************************************************}
program traise6;

{$ifdef fpc}
{$mode objfpc}
{$endif}

Type
  TAObject = class(TObject)
    a : longint;
    end;
  TBObject = Class(TObject)
    b : longint;
      constructor create(c: longint);
    end;


{ The test cases were taken from the SAL internal architecture manual }

    procedure fail;
    begin
      WriteLn('Failure.');
      halt(1);
    end;


 constructor tbobject.create(c:longint);
  begin
    inherited create;
    b:=c;
  end;


  procedure MyRoutine;
   Begin
     WriteLn('hello world!');
   end;

var
 bobj: TBobject;
 i: integer;
Begin
  i:=$7f;
{$ifdef ver1_0}
  raise TBobject.create(i) at longint(@MyRoutine);
{$else}
  raise TBobject.create(i) at @MyRoutine,pointer($00000001);
{$endif}
end.

{
  $Log$
  Revision 1.3  2003-12-23 22:11:26  peter
    * pointer typecast

  Revision 1.2  2002/09/07 15:40:56  peter
    * old logs removed and tabs fixed

  Revision 1.1  2002/08/10 08:27:44  carl
    + mre tests for cg testuit

}
