program Test;
{ Show how to seek to an OFFSET (not a line number) in a textfile, }
{ without using asm. Arne de Bruijn, 1994, PD }
uses Dos; { For TextRec and FileRec }
var
 F:text;
 L:longint;
 S:string;
begin
 assign(F,'tbs/tbs0098.pp');           { Assign F to itself }
 reset(F);                             { Open it (as a textfile) }
 ReadLn(F);                            { Just read some lines }
 ReadLn(F);
 ReadLn(F);
 FileRec((@F)^).Mode:=fmInOut;         { Set to binary mode }
  { (The (@F)^ part is to let TP 'forget' the type of the structure, so }
  {  you can type-caste it to everything (note that with and without (@X)^ }
  {  can give a different value, longint(bytevar) gives the same value as }
  {  bytevar, while longint((@bytevar)^) gives the same as }
  {  longint absolute Bytevar (i.e. all 4 bytes in a longint are readed }
  {  from memory instead of 3 filled with zeros))) }
 FileRec((@F)^).RecSize:=1;            { Set record size to 1 (a byte)}
 L:=(FilePos(File((@F)^))-TextRec(F).BufEnd)+TextRec(F).BufPos;
{... This line didn't work the last time I tried, it chokes on the "File"
typecasting thing.}

  { Get the fileposition, subtract the already readed buffer, and add the }
  { position in that buffer }
 TextRec(F).Mode:=fmInput;             { Set back to text mode }
 TextRec(F).BufSize:=SizeOf(TextBuf);  { BufSize overwritten by RecSize }
                                       { Doesn't work with SetTextBuf! }
 ReadLn(F,S);                          { Read the next line }
 WriteLn('Next line:',S);              { Display it }
 FileRec((@F)^).Mode:=fmInOut;         { Set to binary mode }
 FileRec((@F)^).RecSize:=1;            { Set record size to 1 (a byte)}
 Seek(File((@F)^),L);                  { Do the seek }
{... And again here.}

 TextRec(F).Mode:=fmInput;             { Set back to text mode }
 TextRec(F).BufSize:=SizeOf(TextBuf);  { Doesn't work with SetTextBuf! }
 TextRec(F).BufPos:=0; TextRec(F).BufEnd:=0; { Reset buffer counters }
 ReadLn(F,S);                          { Show that it worked, the same }
 WriteLn('That line again:',S);        { line readed again! }
 Close(F);                             { Close it }
end.
