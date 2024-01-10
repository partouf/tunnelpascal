unit tccookiereadwrite;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  fphttpclient;

type

  TTestCookieList = class(TTestCase)
  published
    procedure TestParseWriteRead;
  end;

implementation

procedure TTestCookieList.TestParseWriteRead;
const
  Cookie1 = 'Set-Cookie: -http-session-=16::http.session::b3997d393228c57235348486e88227c7; path=/; secure; httponly; SameSite=None';
  Cookie2 = 'Set-Cookie: ABBCX=1507337; path=/; secure; httponly; SameSite=None';
begin
  with TCookieList.Create do
    try
      AddCookie(Cookie1);
      AddCookie(Cookie2);
      AssertEquals(Count, 2);
      AssertEquals(GetCookie(0), '-http-session-=16::http.session::b3997d393228c57235348486e88227c7');
      AssertEquals(GetCookie(1), 'ABBCX=1507337');
    finally
      Free;
    end;
end;



initialization

  RegisterTest(TTestCookieList);
end.

