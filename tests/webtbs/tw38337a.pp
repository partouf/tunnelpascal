{ %OPT=-O3 }
{ %CPU=i386,x86_64 }

{ This variant of tw38337 helps to catch out Internal Error 200307043 that
  sometimes got triggered during pure function development (even though the
  test itself doesn't use a pure function). }

program tw38337a;
{$I tw38337.inc}