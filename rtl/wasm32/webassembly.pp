{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2022 by Nikolay Nikolov

    This unit contains some WebAssembly-specific routines

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit WebAssembly;

interface

procedure AtomicFence; inline;

function AtomicAdd(var Mem: Int8; Data: Int8): Int8; inline;
function AtomicAdd(var Mem: UInt8; Data: UInt8): UInt8; inline;
function AtomicAdd(var Mem: Int16; Data: Int16): Int16; inline;
function AtomicAdd(var Mem: UInt16; Data: UInt16): UInt16; inline;
function AtomicAdd(var Mem: Int32; Data: Int32): Int32; inline;
function AtomicAdd(var Mem: UInt32; Data: UInt32): UInt32; inline;
function AtomicAdd(var Mem: Int64; Data: Int64): Int64; inline;
function AtomicAdd(var Mem: UInt64; Data: UInt64): UInt64; inline;

function AtomicSub(var Mem: Int8; Data: Int8): Int8; inline;
function AtomicSub(var Mem: UInt8; Data: UInt8): UInt8; inline;
function AtomicSub(var Mem: Int16; Data: Int16): Int16; inline;
function AtomicSub(var Mem: UInt16; Data: UInt16): UInt16; inline;
function AtomicSub(var Mem: Int32; Data: Int32): Int32; inline;
function AtomicSub(var Mem: UInt32; Data: UInt32): UInt32; inline;
function AtomicSub(var Mem: Int64; Data: Int64): Int64; inline;
function AtomicSub(var Mem: UInt64; Data: UInt64): UInt64; inline;

function AtomicAnd(var Mem: Int8; Data: Int8): Int8; inline;
function AtomicAnd(var Mem: UInt8; Data: UInt8): UInt8; inline;
function AtomicAnd(var Mem: Int16; Data: Int16): Int16; inline;
function AtomicAnd(var Mem: UInt16; Data: UInt16): UInt16; inline;
function AtomicAnd(var Mem: Int32; Data: Int32): Int32; inline;
function AtomicAnd(var Mem: UInt32; Data: UInt32): UInt32; inline;
function AtomicAnd(var Mem: Int64; Data: Int64): Int64; inline;
function AtomicAnd(var Mem: UInt64; Data: UInt64): UInt64; inline;

function AtomicOr(var Mem: Int8; Data: Int8): Int8; inline;
function AtomicOr(var Mem: UInt8; Data: UInt8): UInt8; inline;
function AtomicOr(var Mem: Int16; Data: Int16): Int16; inline;
function AtomicOr(var Mem: UInt16; Data: UInt16): UInt16; inline;
function AtomicOr(var Mem: Int32; Data: Int32): Int32; inline;
function AtomicOr(var Mem: UInt32; Data: UInt32): UInt32; inline;
function AtomicOr(var Mem: Int64; Data: Int64): Int64; inline;
function AtomicOr(var Mem: UInt64; Data: UInt64): UInt64; inline;

function AtomicXor(var Mem: Int8; Data: Int8): Int8; inline;
function AtomicXor(var Mem: UInt8; Data: UInt8): UInt8; inline;
function AtomicXor(var Mem: Int16; Data: Int16): Int16; inline;
function AtomicXor(var Mem: UInt16; Data: UInt16): UInt16; inline;
function AtomicXor(var Mem: Int32; Data: Int32): Int32; inline;
function AtomicXor(var Mem: UInt32; Data: UInt32): UInt32; inline;
function AtomicXor(var Mem: Int64; Data: Int64): Int64; inline;
function AtomicXor(var Mem: UInt64; Data: UInt64): UInt64; inline;

implementation

{$I cpuh.inc}

procedure AtomicFence; inline;
begin
  fpc_wasm32_atomic_fence;
end;

function AtomicAdd(var Mem: Int8; Data: Int8): Int8; inline;
begin
  AtomicAdd:=Int8(fpc_wasm32_i32_atomic_rmw8_add_u(@Mem,Byte(Data)));
end;

function AtomicAdd(var Mem: UInt8; Data: UInt8): UInt8; inline;
begin
  AtomicAdd:=UInt8(fpc_wasm32_i32_atomic_rmw8_add_u(@Mem,Data));
end;

function AtomicAdd(var Mem: Int16; Data: Int16): Int16; inline;
begin
  AtomicAdd:=Int16(fpc_wasm32_i32_atomic_rmw16_add_u(@Mem,Word(Data)));
end;

function AtomicAdd(var Mem: UInt16; Data: UInt16): UInt16; inline;
begin
  AtomicAdd:=UInt16(fpc_wasm32_i32_atomic_rmw16_add_u(@Mem,Data));
end;

function AtomicAdd(var Mem: Int32; Data: Int32): Int32; inline;
begin
  AtomicAdd:=Int32(fpc_wasm32_i32_atomic_rmw_add(@Mem,LongWord(Data)));
end;

function AtomicAdd(var Mem: UInt32; Data: UInt32): UInt32; inline;
begin
  AtomicAdd:=fpc_wasm32_i32_atomic_rmw_add(@Mem,Data);
end;

function AtomicAdd(var Mem: Int64; Data: Int64): Int64; inline;
begin
  AtomicAdd:=Int64(fpc_wasm32_i64_atomic_rmw_add(@Mem,QWord(Data)));
end;

function AtomicAdd(var Mem: UInt64; Data: UInt64): UInt64; inline;
begin
  AtomicAdd:=fpc_wasm32_i64_atomic_rmw_add(@Mem,Data);
end;

function AtomicSub(var Mem: Int8; Data: Int8): Int8; inline;
begin
  AtomicSub:=Int8(fpc_wasm32_i32_atomic_rmw8_sub_u(@Mem,Byte(Data)));
end;

function AtomicSub(var Mem: UInt8; Data: UInt8): UInt8; inline;
begin
  AtomicSub:=UInt8(fpc_wasm32_i32_atomic_rmw8_sub_u(@Mem,Data));
end;

function AtomicSub(var Mem: Int16; Data: Int16): Int16; inline;
begin
  AtomicSub:=Int16(fpc_wasm32_i32_atomic_rmw16_sub_u(@Mem,Word(Data)));
end;

function AtomicSub(var Mem: UInt16; Data: UInt16): UInt16; inline;
begin
  AtomicSub:=UInt16(fpc_wasm32_i32_atomic_rmw16_sub_u(@Mem,Data));
end;

function AtomicSub(var Mem: Int32; Data: Int32): Int32; inline;
begin
  AtomicSub:=Int32(fpc_wasm32_i32_atomic_rmw_sub(@Mem,LongWord(Data)));
end;

function AtomicSub(var Mem: UInt32; Data: UInt32): UInt32; inline;
begin
  AtomicSub:=fpc_wasm32_i32_atomic_rmw_sub(@Mem,Data);
end;

function AtomicSub(var Mem: Int64; Data: Int64): Int64; inline;
begin
  AtomicSub:=Int64(fpc_wasm32_i64_atomic_rmw_sub(@Mem,QWord(Data)));
end;

function AtomicSub(var Mem: UInt64; Data: UInt64): UInt64; inline;
begin
  AtomicSub:=fpc_wasm32_i64_atomic_rmw_sub(@Mem,Data);
end;

function AtomicAnd(var Mem: Int8; Data: Int8): Int8; inline;
begin
  AtomicAnd:=Int8(fpc_wasm32_i32_atomic_rmw8_and_u(@Mem,Byte(Data)));
end;

function AtomicAnd(var Mem: UInt8; Data: UInt8): UInt8; inline;
begin
  AtomicAnd:=UInt8(fpc_wasm32_i32_atomic_rmw8_and_u(@Mem,Data));
end;

function AtomicAnd(var Mem: Int16; Data: Int16): Int16; inline;
begin
  AtomicAnd:=Int16(fpc_wasm32_i32_atomic_rmw16_and_u(@Mem,Word(Data)));
end;

function AtomicAnd(var Mem: UInt16; Data: UInt16): UInt16; inline;
begin
  AtomicAnd:=UInt16(fpc_wasm32_i32_atomic_rmw16_and_u(@Mem,Data));
end;

function AtomicAnd(var Mem: Int32; Data: Int32): Int32; inline;
begin
  AtomicAnd:=Int32(fpc_wasm32_i32_atomic_rmw_and(@Mem,LongWord(Data)));
end;

function AtomicAnd(var Mem: UInt32; Data: UInt32): UInt32; inline;
begin
  AtomicAnd:=fpc_wasm32_i32_atomic_rmw_and(@Mem,Data);
end;

function AtomicAnd(var Mem: Int64; Data: Int64): Int64; inline;
begin
  AtomicAnd:=Int64(fpc_wasm32_i64_atomic_rmw_and(@Mem,QWord(Data)));
end;

function AtomicAnd(var Mem: UInt64; Data: UInt64): UInt64; inline;
begin
  AtomicAnd:=fpc_wasm32_i64_atomic_rmw_and(@Mem,Data);
end;

function AtomicOr(var Mem: Int8; Data: Int8): Int8; inline;
begin
  AtomicOr:=Int8(fpc_wasm32_i32_atomic_rmw8_or_u(@Mem,Byte(Data)));
end;

function AtomicOr(var Mem: UInt8; Data: UInt8): UInt8; inline;
begin
  AtomicOr:=UInt8(fpc_wasm32_i32_atomic_rmw8_or_u(@Mem,Data));
end;

function AtomicOr(var Mem: Int16; Data: Int16): Int16; inline;
begin
  AtomicOr:=Int16(fpc_wasm32_i32_atomic_rmw16_or_u(@Mem,Word(Data)));
end;

function AtomicOr(var Mem: UInt16; Data: UInt16): UInt16; inline;
begin
  AtomicOr:=UInt16(fpc_wasm32_i32_atomic_rmw16_or_u(@Mem,Data));
end;

function AtomicOr(var Mem: Int32; Data: Int32): Int32; inline;
begin
  AtomicOr:=Int32(fpc_wasm32_i32_atomic_rmw_or(@Mem,LongWord(Data)));
end;

function AtomicOr(var Mem: UInt32; Data: UInt32): UInt32; inline;
begin
  AtomicOr:=fpc_wasm32_i32_atomic_rmw_or(@Mem,Data);
end;

function AtomicOr(var Mem: Int64; Data: Int64): Int64; inline;
begin
  AtomicOr:=Int64(fpc_wasm32_i64_atomic_rmw_or(@Mem,QWord(Data)));
end;

function AtomicOr(var Mem: UInt64; Data: UInt64): UInt64; inline;
begin
  AtomicOr:=fpc_wasm32_i64_atomic_rmw_or(@Mem,Data);
end;

function AtomicXor(var Mem: Int8; Data: Int8): Int8; inline;
begin
  AtomicXor:=Int8(fpc_wasm32_i32_atomic_rmw8_xor_u(@Mem,Byte(Data)));
end;

function AtomicXor(var Mem: UInt8; Data: UInt8): UInt8; inline;
begin
  AtomicXor:=UInt8(fpc_wasm32_i32_atomic_rmw8_xor_u(@Mem,Data));
end;

function AtomicXor(var Mem: Int16; Data: Int16): Int16; inline;
begin
  AtomicXor:=Int16(fpc_wasm32_i32_atomic_rmw16_xor_u(@Mem,Word(Data)));
end;

function AtomicXor(var Mem: UInt16; Data: UInt16): UInt16; inline;
begin
  AtomicXor:=UInt16(fpc_wasm32_i32_atomic_rmw16_xor_u(@Mem,Data));
end;

function AtomicXor(var Mem: Int32; Data: Int32): Int32; inline;
begin
  AtomicXor:=Int32(fpc_wasm32_i32_atomic_rmw_xor(@Mem,LongWord(Data)));
end;

function AtomicXor(var Mem: UInt32; Data: UInt32): UInt32; inline;
begin
  AtomicXor:=fpc_wasm32_i32_atomic_rmw_xor(@Mem,Data);
end;

function AtomicXor(var Mem: Int64; Data: Int64): Int64; inline;
begin
  AtomicXor:=Int64(fpc_wasm32_i64_atomic_rmw_xor(@Mem,QWord(Data)));
end;

function AtomicXor(var Mem: UInt64; Data: UInt64): UInt64; inline;
begin
  AtomicXor:=fpc_wasm32_i64_atomic_rmw_xor(@Mem,Data);
end;

end.
