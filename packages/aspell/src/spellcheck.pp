{$IFNDEF FPC_DOTTEDUNITS}
unit SpellCheck;
{$ENDIF FPC_DOTTEDUNITS}

{ Simple unit to simplify/OOP-ize pascal-style the aspell interface. Currently
  very limited, will be expanded eventually. Use like you wish. }

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, System.Classes, Api.Aspell;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils, Classes, Aspell;
{$ENDIF FPC_DOTTEDUNITS}

type
  TSuggestionArray = array of AnsiString;
  
  TWordError = record
    Word: AnsiString; // the word itself
    Pos: LongWord; // word position in line
    Length: LongWord; // word length
    Suggestions: TSuggestionArray; // suggestions for the given word
  end;
  
  TLineErrors = array of TWordError;
  TLineErrorsArray = array of TLineErrors;

  { TSpellCheck }

  TSpeller = class // abstract class, basis for all checkers
   protected
    FMode: AnsiString;
    FEncoding: AnsiString;
    FLanguage: AnsiString;
    procedure SetEncoding(const AValue: AnsiString);
    procedure SetLanguage(const AValue: AnsiString);
    procedure SetMode(const AValue: AnsiString);
    procedure CreateSpeller; virtual; abstract;
    procedure FreeSpeller; virtual; abstract;
   public
    constructor Create;
    destructor Destroy; override;
   public
    property Mode: AnsiString read FMode write SetMode;
    property Encoding: AnsiString read FEncoding write SetEncoding;
    property Language: AnsiString read FLanguage write SetLanguage;
  end;
  
  { TWordSpeller }

  TWordSpeller = class(TSpeller) // class for simple per-word checking
   private
    FSpeller: PAspellSpeller;
    FLastError: AnsiString;
    function DoCreateSpeller(Lang, Enc, aMode: PAnsiChar): PAspellSpeller;
   protected
    procedure CreateSpeller; override;
    procedure FreeSpeller; override;
   public
    function SpellCheck(const Word: AnsiString): TSuggestionArray; // use to check single words, parsed out by you
  end;
  
  { TDocumentSpeller }

  TDocumentSpeller = class(TWordSpeller)
   private
    FChecker: PAspellDocumentChecker;
    FLineErrors: TLineErrorsArray;
    FNameSuggestions: Boolean;
    function GetLineErrors(i: Integer): TLineErrors;
    function GetLineErrorsCount: Integer;
   protected
    procedure CreateSpeller; override;
    procedure FreeSpeller; override;
    procedure DoNameSuggestions(const Word: AnsiString; var aWordError: TWordError);
   public
    constructor Create;
    function CheckLine(const aLine: AnsiString): TLineErrors;
    function CheckDocument(const FileName: AnsiString): Integer; // returns number of spelling errors found or -1 for error
    function CheckDocument(aStringList: TStringList): Integer; // returns number of spelling errors found or -1 for error
    procedure Reset;
   public
    property LineErrors[i: Integer]: TLineErrors read GetLineErrors;
    property LineErrorsCount: Integer read GetLineErrorsCount;
    property NameSuggestions: Boolean read FNameSuggestions write FNameSuggestions;
  end;

implementation

const
  DEFAULT_ENCODING = 'utf-8';
  DEFAULT_LANGUAGE = 'en';
  DEFAULT_MODE     = '';

function GetDefaultLanguage: AnsiString;
begin
  Result := GetEnvironmentVariable('LANG');
  if Length(Result) = 0 then
    Result := DEFAULT_LANGUAGE;
end;

{ TSpeller }

procedure TSpeller.SetEncoding(const AValue: AnsiString);
begin
  FEncoding := aValue;
  CreateSpeller;
end;

procedure TSpeller.SetLanguage(const AValue: AnsiString);
begin
  FLanguage := aValue;
  CreateSpeller;
end;

procedure TSpeller.SetMode(const AValue: AnsiString);
begin
  FMode := aValue;
  CreateSpeller;
end;

constructor TSpeller.Create;
begin
  FEncoding := DEFAULT_ENCODING;
  FLanguage := GetDefaultLanguage;
  FMode := DEFAULT_MODE;

  CreateSpeller;
end;

destructor TSpeller.Destroy;
begin
  FreeSpeller;
end;

{ TWordSpeller }

function TWordSpeller.DoCreateSpeller(Lang, Enc, aMode: PAnsiChar): PAspellSpeller;
var
  Error: Paspellcanhaveerror;
begin
  Result := new_aspell_config();

  if Length(FLanguage) > 0 then
    aspell_config_replace(Result, 'lang', Lang);
  if Length(FEncoding) > 0 then
    aspell_config_replace(Result, 'encoding', Enc);
  if Length(FMode) > 0 then
    aspell_config_replace(Result, 'mode', aMode);

  Error := new_aspell_speller(Result);

  delete_aspell_config(Result);

  if aspell_error_number(Error) <> 0 then begin
    FLastError := aspell_error_message(Error);
    delete_aspell_can_have_error(Error);
    Result := nil;
  end else
    Result := to_aspell_speller(Error);
end;

procedure TWordSpeller.CreateSpeller;
begin
  FLastError := '';
  FreeSpeller;

  FSpeller := DoCreateSpeller(PAnsiChar(FLanguage), PAnsiChar(FEncoding), PAnsiChar(FMode));
  if not Assigned(FSpeller) then
    FSpeller := DoCreateSpeller(nil, PAnsiChar(FEncoding), PAnsiChar(FMode));
  if not Assigned(FSpeller) then
    FSpeller := DoCreateSpeller(nil, PAnsiChar(FEncoding), nil);
  if not Assigned(FSpeller) then
    FSpeller := DoCreateSpeller(nil, nil, PAnsiChar(FMode));
  if not Assigned(FSpeller) then
    FSpeller := DoCreateSpeller(nil, nil, nil);

  if not Assigned(FSpeller) then
    raise Exception.Create('Error on speller creation: ' + FLastError);
end;

procedure TWordSpeller.FreeSpeller;
begin
  if Assigned(FSpeller) then begin
    delete_aspell_speller(FSpeller);
    FSpeller := nil;
  end;
end;

function TWordSpeller.SpellCheck(const Word: AnsiString): TSuggestionArray;
var
  sgs: Paspellwordlist;
  elm: Paspellstringenumeration;
  tmp: PAnsiChar;
  i: Integer = 0;
begin
  SetLength(Result, 0);

  if aspell_speller_check(FSpeller, PAnsiChar(Word), Length(Word)) = 0 then begin
    sgs := aspell_speller_suggest(FSpeller, PAnsiChar(Word), Length(Word));
    elm := aspell_word_list_elements(sgs);

    repeat
      if i >= Length(Result) then
        SetLength(Result, Length(Result) + 10);

      tmp := aspell_string_enumeration_next(elm);

      if tmp <> nil then begin
        Result[i] := tmp;
        Inc(i);
      end;
    until tmp = nil;

    SetLength(Result, i);

    delete_aspell_string_enumeration(elm);
  end;
end;

{ TDocumentSpeller }

function TDocumentSpeller.GetLineErrors(i: Integer): TLineErrors;
begin
  Result := FLineErrors[i];
end;

function TDocumentSpeller.GetLineErrorsCount: Integer;
begin
  Result := Length(FLineErrors);
end;

procedure TDocumentSpeller.CreateSpeller;
var
  Error: PAspellCanHaveError;
begin
  inherited CreateSpeller;
  
  Error := new_aspell_document_checker(FSpeller);

  if aspell_error_number(Error) <> 0 then
    raise Exception.Create('Error on checker creation: ' + aspell_error_message(Error))
  else
    FChecker := to_aspell_document_checker(Error);
end;

procedure TDocumentSpeller.FreeSpeller;
begin
  if Assigned(FChecker) then begin
    delete_aspell_document_checker(FChecker);
    FChecker := nil;
  end;

  inherited FreeSpeller;
end;

procedure TDocumentSpeller.DoNameSuggestions(const Word: AnsiString;
  var aWordError: TWordError);
begin
  aWordError.Suggestions := SpellCheck(Word);
end;

constructor TDocumentSpeller.Create;
begin
  inherited Create;
  
  FNameSuggestions := True;
end;

function TDocumentSpeller.CheckLine(const aLine: AnsiString): TLineErrors;
const
  CHUNK_SIZE = 10;
var
  i, Count: Integer;
  Token: AspellToken;
begin
  aspell_document_checker_process(FChecker, PAnsiChar(aLine), Length(aLine));

  SetLength(Result, CHUNK_SIZE);
  i := 0;
  Count := 0;
  repeat
    Token := aspell_document_checker_next_misspelling(FChecker);

    if Token.len > 0 then begin
      if Length(Result) <= i then
        SetLength(Result, Length(Result) + CHUNK_SIZE);

      Result[i].Word := Copy(aLine, Token.offset + 1, Token.len);
      Result[i].Pos := Token.offset + 1; // C goes from 0, we go from 1
      Result[i].Length := Token.len;

      if FNameSuggestions then
        DoNameSuggestions(Copy(aLine, Token.offset + 1, Token.len), Result[i]);
        
      Inc(Count);
    end;

    Inc(i);
  until Token.len = 0;
  
  SetLength(Result, Count);
end;

function TDocumentSpeller.CheckDocument(const FileName: AnsiString): Integer;
var
  s: TStringList;
begin
  Result := 0;
  if FileExists(FileName) then try
    s := TStringList.Create;
    s.LoadFromFile(FileName);
    Result := CheckDocument(s);
  finally
    s.Free;
  end;
end;

function TDocumentSpeller.CheckDocument(aStringList: TStringList): Integer;
var
  i: Integer;
begin
  Result := 0;
  SetLength(FLineErrors, aStringList.Count);

  for i := 0 to aStringList.Count - 1 do begin
    FLineErrors[i] := CheckLine(aStringList[i]);
    Inc(Result, Length(FLineErrors[i]));
  end;
end;

procedure TDocumentSpeller.Reset;
begin
  aspell_document_checker_reset(FChecker);
end;

end.

