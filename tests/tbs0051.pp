program TestPutP;

{$ifdef go32v2}
  {define has_colors_equal}
{$endif go32v2}

{$ifdef go32v2}
{$define OK}
{$endif}
{$ifdef linux}
{$define OK}
{$endif}

{$ifdef OK}
uses  crt,graph;

{$ifndef has_colors_equal}
  function ColorsEqual(c1, c2 : longint) : boolean;
    begin
       ColorsEqual:=((GetMaxColor=$FF) and ((c1 and $FF)=(c2 and $FF))) or
         ((GetMaxColor=$7FFF) and ((c1 and $F8F8F8)=(c2 and $F8F8F8))) or
         ((GetMaxColor=$FFFF) and ((c1 and $F8FCF8)=(c2 and $F8FCF8))) or
         ((GetMaxColor>$10000) and ((c1 and $FFFFFF)=(c2 and $FFFFFF)));
    end;

{$endif not has_colors_equal}

var   gd,gm,gError,yi,i : integer;
      col: longint;
      error : word;

{$endif OK}
BEGIN
{$ifdef OK}
  if paramcount=0 then
    gm:=$111   {640x480/64K  HiColor}
  else
    begin
       val(paramstr(1),gm,error);
       if error<>0 then
         gm:=$111;
    end;
  gd:=VESA;

  InitGraph(gd,gm,'');
  gError := graphResult;
  IF gError <> grOk
  THEN begin
    writeln ('graphDriver=',gd,'  graphMode=',gm,
    #13#10'Graphics error: ',gError);
    halt(1);
  end;

  for i := 0 to 255
  do begin
    col := i shl 16 + (i div 2) shl 8 + (i div 3);
    for yi := 0 to 20 do
      PutPixel (i,yi,col);
    SetColor (col);
    Line (i,22,i,42);
  end;

  for i:=0 to 255 do
   if not ColorsEqual(getpixel(i,15),getpixel(i,30)) then
     Halt(1);
  {readkey;}delay(1000);

  closegraph;
{$endif OK}
END.

{
  $Log$
  Revision 1.5  1999-11-28 12:17:14  jonas
    * changed the requested graphdriver from $FF to VESA (= 10), so the
      test program works again with the new graph unit
    * undefined has_colors_equal for go32v2, because it is not anymore
      in the new graph unit


}