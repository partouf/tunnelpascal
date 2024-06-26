program PGtk;

{$IFDEF FPC_DOTTEDUNITS}
Uses System.SysUtils, Objectdef, System.Classes;
{$ELSE FPC_DOTTEDUNITS}
Uses sysutils, ObjectDef, classes;
{$ENDIF FPC_DOTTEDUNITS}

type
  PGtkexception = class (Exception);

procedure DataRead (Filename:AnsiString; var Descr:TObjectDefs);
var StrStream : TFileStream;
    BinStream : TMemoryStream;
begin
  if fileExists (filename) then
    begin
    StrStream := TFileStream.Create(filename, fmOpenRead);
    try
      BinStream := TMemoryStream.Create;
      try
        writeln ('Reading...');
        ObjectTextToBinary(StrStream, BinStream);
        BinStream.Seek(0, soFromBeginning);
        BinStream.ReadComponent(Descr);
      finally
        BinStream.Free;
      end;
    finally
      StrStream.Free;
    end;
    end
  else
    raise PGtkException.Create ('Error: Can''t find file "'+filename+'"');
end;

procedure Convert (DescrFilename, UnitFilename : AnsiString);
var GTK : TObjectDefs;
    l : TStrings;
begin
  l := TStringlist.Create;
  GTK := TObjectdefs.create (nil);
  try
    DataRead (DescrFilename, GTK);
    writeln ('Filling Stringlist');
    GTK.Write (l, nil, nil);
    writeln ('Writing to file');
    L.SaveToFile (UnitFilename);
  finally
    GTK.Free;
    l.Free;
  end;
end;


begin

  if paramcount = 2 then
    Convert (Paramstr(1), Paramstr(2))
  else
    writeln ('Give 2 filenames :'#10#13'   First the object description file'#10#13'   Second the Pascal unit filename');
end.
