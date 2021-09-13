{ %OPT=-O- -O2 }

function get_sign(d: double): Integer;
  var
    p: pbyte;
  begin
    get_sign:=1;
    p:=pbyte(@d);
<<<<<<< HEAD
{$ifdef FPUARM_HAS_FPA}
    inc(p,4);
{$else}
{$ifdef FPC_LITTLE_ENDIAN}
    inc(p,sizeof(d)-1);
{$endif}
{$endif}
    if (p^ and $80)<>0 then
=======
{$ifdef FPC_LITTLE_ENDIAN}
    inc(p,4);
{$endif}
    if (p^ and $80)=0 then
>>>>>>> 611164197d...   * fix handling of -0.0 in sse/avx code, resolves #39357
      get_sign:=-1;
  end;

const
	NegInfinity: single = -1.0 / 0.0;
var
    zero : Double;
begin
    zero:=0.0;
<<<<<<< HEAD

    if get_sign(1.0)<>1 then
      halt(1);

=======
>>>>>>> 611164197d...   * fix handling of -0.0 in sse/avx code, resolves #39357
	writeln(-zero);
    if get_sign(-zero)<>-1 then
      halt(1);

	writeln(1.0 / (-1.0 / 0.0));
    if get_sign(1.0 / (-1.0 / 0.0))<>-1 then
      halt(1);

	writeln(1.0 / NegInfinity);
    if get_sign(1.0 / NegInfinity)<>-1 then
      halt(1);

    writeln('ok');
end.
