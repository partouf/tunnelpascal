{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Some basic types and constants for the code generation

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
{# This unit exports some types which are used across the code generator }
unit cgbase;

{$i fpcdefs.inc}

interface

    uses
      globtype,
      symconst;

    type
       { Location types where value can be stored }
       TCGLoc=(
         LOC_INVALID,      { added for tracking problems}
         LOC_VOID,         { no value is available }
         LOC_CONSTANT,     { constant value }
         LOC_JUMP,         { boolean results only, jump to false or true label }
         LOC_FLAGS,        { boolean results only, flags are set }
         LOC_REGISTER,     { in a processor register }
         LOC_CREGISTER,    { Constant register which shouldn't be modified }
         LOC_FPUREGISTER,  { FPU stack }
         LOC_CFPUREGISTER, { if it is a FPU register variable on the fpu stack }
         LOC_MMXREGISTER,  { MMX register }
         { MMX register variable }
         LOC_CMMXREGISTER,
         { multimedia register }
         LOC_MMREGISTER,
         { Constant multimedia reg which shouldn't be modified }
         LOC_CMMREGISTER,
         { contiguous subset of bits of an integer register }
         LOC_SUBSETREG,
         LOC_CSUBSETREG,
         { contiguous subset of bits in memory }
         LOC_SUBSETREF,
         LOC_CSUBSETREF,
         { keep these last for range checking purposes }
         LOC_CREFERENCE,   { in memory constant value reference (cannot change) }
         LOC_REFERENCE     { in memory value }
       );

       TCGNonRefLoc=low(TCGLoc)..pred(LOC_CREFERENCE);
       TCGRefLoc=LOC_CREFERENCE..LOC_REFERENCE;

       trefaddr = (
         addr_no,
         addr_full,
         addr_pic,
         addr_pic_no_got
         {$IF defined(POWERPC) or defined(POWERPC64) or defined(SPARC) or defined(MIPS) or defined(SPARC64)}
         ,
         { since we have only 16bit offsets, we need to be able to specify the high
           and lower 16 bits of the address of a symbol of up to 64 bit }
         addr_low,         // bits 48-63
         addr_high,        // bits 32-47
         {$IF defined(POWERPC64)}
         addr_higher,      // bits 16-31
         addr_highest,     // bits 00-15
         {$ENDIF}
         addr_higha        // bits 16-31, adjusted
         {$IF defined(POWERPC64)}
         ,
         addr_highera,     // bits 32-47, adjusted
         addr_highesta     // bits 48-63, adjusted
         {$ENDIF}
         {$ENDIF POWERPC or POWERPC64 or SPARC or MIPS or SPARC64}
         {$IFDEF MIPS}
         ,
         addr_pic_call16,  // like addr_pic, but generates call16 reloc instead of got16
         addr_low_pic,     // for large GOT model, generate got_hi16 and got_lo16 relocs
         addr_high_pic,
         addr_low_call,    // counterpart of two above, generate call_hi16 and call_lo16 relocs
         addr_high_call
         {$ENDIF}
         {$if defined(RISCV32) or defined(RISCV64)}
         ,
         addr_hi20,
         addr_lo12,
         addr_pcrel_hi20,
         addr_pcrel_lo12,
         addr_pcrel,
         addr_got_pcrel_hi,
         addr_plt
         {$endif RISCV}
         {$if defined(LOONGARCH64)}
         ,
         addr_b16, { %b16(sym) }
         addr_b21, { %b21(sym) }
         addr_b26, { %b26(sym) }
         addr_pcrel, { Some times we only use sym like 'bxx rd,rj,sym'. And la.pcrel..sym }
         addr_plt, { %plt(sym) }
         addr_abs_hi20, { %abs_hi20(sym) }
         addr_abs_lo12, { %abs_lo12(sym) }
         addr_abs64_lo20, { %abs_lo20(sym) }
         addr_abs64_hi12, { %abs_hi12(sym) }
         addr_pc_hi20, { %pc_hi20(sym) }
         addr_got_pc_hi20, { %got_pc_hi20(sym) }
         addr_got_pc_lo12, { %got_pc_lo12(sym) }
         addr_pc_lo12, { %pc_lo12(sym) }
         addr_got, { la.got..sym }
         addr_abs, { la.abs..sym }
         addr_reg_reg, { use by [ld/st]x }
         addr_reg_12i, { use by [ld/st] }
         addr_reg_14i, { use by [ldptr/stptr] }
         addr_reg { use by jr.. }
         {$endif LOONGARCH64}
         {$IFDEF AVR}
         ,addr_lo8
         ,addr_lo8_gs
         ,addr_hi8
         ,addr_hi8_gs
         {$ENDIF}
         {$IFDEF Z80}
         ,addr_lo8
         ,addr_hi8
         {$ENDIF}
         {$IFDEF i8086}
         ,addr_dgroup      // the data segment group
         ,addr_fardataseg  // the far data segment of the current pascal module (unit or program)
         ,addr_seg         // used for getting the segment of an object, e.g. 'mov ax, SEG symbol'
         {$ENDIF}
         {$IFDEF AARCH64}
         ,addr_page
         ,addr_pageoffset
         ,addr_gotpage
         ,addr_gotpageoffset
         {$ENDIF AARCH64}
         {$ifdef SPARC64}
         ,addr_gdop_hix22
         ,addr_gdop_lox22
         {$endif SPARC64}
         {$IFDEF ARM}
         ,addr_gottpoff
         ,addr_tpoff
         ,addr_tlsgd
         ,addr_tlsdesc
         ,addr_tlscall
         {$ENDIF}
         {$IFDEF i386}
         ,addr_ntpoff
         ,addr_tlsgd
         {$ENDIF}
{$ifdef x86_64}
          ,addr_tpoff
          ,addr_tlsgd
{$endif x86_64}
{$ifdef wasm32}
          ,addr_got_tls
{$endif wasm32}
         );


       {# Generic opcodes, which must be supported by all processors
       }
       topcg =
       (
          OP_NONE,
          OP_MOVE,      { replaced operation with direct load }
          OP_ADD,       { simple addition          }
          OP_AND,       { simple logical and       }
          OP_DIV,       { simple unsigned division }
          OP_IDIV,      { simple signed division   }
          OP_IMUL,      { simple signed multiply   }
          OP_MUL,       { simple unsigned multiply }
          OP_NEG,       { simple negate            }
          OP_NOT,       { simple logical not       }
          OP_OR,        { simple logical or        }
          OP_SAR,       { arithmetic shift-right   }
          OP_SHL,       { logical shift left       }
          OP_SHR,       { logical shift right      }
          OP_SUB,       { simple subtraction       }
          OP_XOR,       { simple exclusive or      }
          OP_ROL,       { rotate left              }
          OP_ROR        { rotate right             }
        );

       {# Generic flag values - used for jump locations }
       TOpCmp =
       (
          OC_NONE,
          OC_EQ,           { equality comparison              }
          OC_GT,           { greater than (signed)            }
          OC_LT,           { less than (signed)               }
          OC_GTE,          { greater or equal than (signed)   }
          OC_LTE,          { less or equal than (signed)      }
          OC_NE,           { not equal                        }
          OC_BE,           { less or equal than (unsigned)    }
          OC_B,            { less than (unsigned)             }
          OC_AE,           { greater or equal than (unsigned) }
          OC_A             { greater than (unsigned)          }
        );

       { indirect symbol flags }
       tindsymflag = (is_data,is_weak);
       tindsymflags = set of tindsymflag;

       { OS_NO is also used memory references with large data that can
         not be loaded in a register directly }
       TCgSize = (OS_NO,
                  OS_8,   OS_16,   OS_32,   OS_64,   OS_128,
                  OS_S8,  OS_S16,  OS_S32,  OS_S64,  OS_S128,
                 { single, double, extended, comp, float128 }
                  OS_F32, OS_F64,  OS_F80,  OS_C64,  OS_F128,
                 { multi-media sizes, describes only the register size but not how it is split,
                   this information must be passed separately }
                  OS_M8,  OS_M16,  OS_M32,  OS_M64,  OS_M128,  OS_M256,  OS_M512);

      { Register types }
      TRegisterType = (
        R_INVALIDREGISTER, { = 0 }
        R_INTREGISTER,     { = 1 }
        R_FPUREGISTER,     { = 2 }
        { used by Intel only }
        R_MMXREGISTER,     { = 3 }
        R_MMREGISTER,      { = 4 }
        R_SPECIALREGISTER, { = 5 }
        R_ADDRESSREGISTER, { = 6 }
        { used on llvm, every temp gets its own "base register" }
        R_TEMPREGISTER,    { = 7 }
        { used on llvm for tracking metadata (every unique metadata has its own base register) }
        R_METADATAREGISTER,{ = 8 }
        { optional MAC16 (16 bit multiply-accumulate) registers on Xtensa }
        R_MAC16REGISTER    { = 9 }

        { do not add more than 16 elements (ifdef by cpu type if needed)
          so we can store this in one nibble and pack TRegister
          if the supreg width should be extended }
      );

      { Sub registers }
      TSubRegister = (
        R_SUBNONE, { = 0; no sub register possible }
        R_SUBL,    { = 1; 8 bits, Like AL }
        R_SUBH,    { = 2; 8 bits, Like AH }
        R_SUBW,    { = 3; 16 bits, Like AX }
        R_SUBD,    { = 4; 32 bits, Like EAX }
        R_SUBQ,    { = 5; 64 bits, Like RAX }
        { For Sparc floats that use F0:F1 to store doubles }
        R_SUBFS,   { = 6; Float that allocates 1 FPU register }
        R_SUBFD,   { = 7; Float that allocates 2 FPU registers }
        R_SUBFQ,   { = 8; Float that allocates 4 FPU registers }
        R_SUBMMS,  { = 9; single scalar in multi media register }
        R_SUBMMD,  { = 10; double scalar in multi media register }
        R_SUBMMWHOLE,  { = 11; complete MM register, size depends on CPU }
        { For Intel X86 AVX-Register }
        R_SUBMMX,     { = 12; 128 BITS }
        R_SUBMMY,     { = 13; 256 BITS }
        R_SUBMMZ,     { = 14; 512 BITS }
{$ifdef Z80}
        { Subregisters for the flags register (Z80) }
        R_SUBFLAGCARRY,          { = 15; Carry flag }
        R_SUBFLAGADDSUBTRACT,    { = 16; Add/Subtract flag }
        R_SUBFLAGPARITYOVERFLOW, { = 17; Parity/Overflow flag }
        R_SUBFLAGUNUSEDBIT3,     { = 18; Unused flag (bit 3) }
        R_SUBFLAGHALFCARRY,      { = 19; Half Carry flag }
        R_SUBFLAGUNUSEDBIT5,     { = 20; Unused flag (bit 5) }
        R_SUBFLAGZERO,           { = 21; Zero flag }
        R_SUBFLAGSIGN,           { = 22; Sign flag }
{$else Z80}
        { Subregisters for the flags register (x86) }
        R_SUBFLAGCARRY,     { = 15; Carry flag }
        R_SUBFLAGPARITY,    { = 16; Parity flag }
        R_SUBFLAGAUXILIARY, { = 17; Auxiliary flag }
        R_SUBFLAGZERO,      { = 18; Zero flag }
        R_SUBFLAGSIGN,      { = 19; Sign flag }
        R_SUBFLAGOVERFLOW,  { = 20; Overflow flag }
        R_SUBFLAGINTERRUPT, { = 21; Interrupt enable flag }
        R_SUBFLAGDIRECTION, { = 22; Direction flag }
{$endif Z80}
        { subregisters for the metadata register (llvm) }
        R_SUBMETASTRING    { = 23 }
{$ifdef aarch64}
        , R_SUBMM8B          { = 24; for arrangement of v regs on aarch64 }
        , R_SUBMM16B         { = 25; for arrangement of v regs on aarch64 }
        , R_SUBMM4H          { = 26; for arrangement of v regs on aarch64 }
        , R_SUBMM8H          { = 27; for arrangement of v regs on aarch64 }
        , R_SUBMM2S          { = 28; for arrangement of v regs on aarch64 }
        , R_SUBMM4S          { = 29; for arrangement of v regs on aarch64 }
        , R_SUBMM1D          { = 30; for arrangement of v regs on aarch64 }
        , R_SUBMM2D          { = 31; for arrangement of v regs on aarch64 }
        , R_SUBMMB1          { = 32; for arrangement of v regs on aarch64; for use with ldN/stN }
        , R_SUBMMH1          { = 33; for arrangement of v regs on aarch64; for use with ldN/stN }
        , R_SUBMMS1          { = 34; for arrangement of v regs on aarch64; for use with ldN/stN }
        , R_SUBMMD1          { = 35; for arrangement of v regs on aarch64; for use with ldN/stN }
{$endif aarch64}
      );
      TSubRegisterSet = set of TSubRegister;

      TSuperRegister = type word;
      PSuperRegister = ^TSuperRegister;

      {
        The new register coding:

        SuperRegister   (bits 0..15)
        Subregister     (bits 16..23)
        Register type   (bits 24..31)

        TRegister is defined as an enum to make it incompatible
        with TSuperRegister to avoid mixing them
      }
      TRegister = (
        TRegisterLowEnum := Low(longint),
        TRegisterHighEnum := High(longint)
      );
      TRegisterRec=packed record
{$ifdef FPC_BIG_ENDIAN}
         regtype : Tregistertype;
         subreg  : Tsubregister;
         supreg  : Tsuperregister;
{$else FPC_BIG_ENDIAN}
         supreg  : Tsuperregister;
         subreg  : Tsubregister;
         regtype : Tregistertype;
{$endif FPC_BIG_ENDIAN}
      end;

      { A type to store register locations for 64 Bit values. }
{$ifdef cpu64bitalu}
      tregister64 = tregister;
      tregister128 = record
         reglo,reghi : tregister;
      end;
{$else cpu64bitalu}
      tregister64 = record
         reglo,reghi : tregister;
      end;
{$endif cpu64bitalu}

      { Set type definition for registers }
      tsuperregisterset = array[byte] of set of byte;

      pmmshuffle = ^tmmshuffle;

      { this record describes shuffle operations for mm operations; if a pointer a shuffle record
        passed to an mm operation is nil, it means that the whole location is moved }
      tmmshuffle = record
        { describes how many shuffles are actually described, if len=0 then
          moving the scalar with index 0 to the scalar with index 0 is meant,
          if len=-1, then a variable/unknown length is assumed }
        len : Shortint;
        { lower byte of each entry of this array describes index of the source data index while
          the upper byte describes the destination index }
        shuffles : array[1..1] of word;
      end;

      Tsuperregistercomparefunc = function(a, b: TSuperRegister; param: pointer): boolean;

      Tsuperregisterworkhashlist=object
        constructor init;
        constructor copyfrom(const x:Tsuperregisterworkhashlist);
        destructor  done;
        procedure clear;
        procedure add(s:tsuperregister);
        function addnodup(s:tsuperregister): boolean;
        { returns the last element and removes it from the list }
        function get:tsuperregister;
        procedure deleteidx(i:SizeInt);
        function delete(s:tsuperregister):boolean;
        procedure sort(less:Tsuperregistercomparefunc; param:pointer);
      private
        { Either a region is allocated for FItems+h+next, or the table is empty with hmask=0, fake 'h' pointing to a single -1, and distinctive FItems=nil. }
        FItems : Psuperregister;
        h : PInt32; { Count of 'h's is always hmask+1 and is always a power of two. h[i]=-1 means empty, h[i]=hi>=0 references FItems[hi]. }
        next : PInt32; { Chaining to allow duplicates and resolve collisions. }
        FNItems : int32; { signed to allow subtracting one without surprises... }
        hmask,minItems,maxItems : uint32;
        procedure rehash(forItems:SizeUint);
        class function allocateregion(
          nh,amaxitems:uint32; out aitems:Psuperregister; out ah:PInt32; out anext:PInt32):SizeUint; static;
        class procedure rebuildh(aitems:Psuperregister; ah,anext:PInt32; ahmask,anitems:int32); static;
      public
        property length: int32 read FNItems;
        property buf: Psuperregister read FItems;
      end;
      Psuperregisterworkhashlist = ^Tsuperregisterworkhashlist;

    const
       { alias for easier understanding }
       R_SSEREGISTER = R_MMREGISTER;

       { Invalid register number }
       RS_INVALID    = high(tsuperregister);
       NR_INVALID    = tregister($ffffffff);

       tcgsize2size : Array[tcgsize] of integer =
        (0,
         { integer values }
         1,  2,  4,  8, 16,
         1,  2,  4,  8, 16,
         { floating point values }
         4,  8, 10,  8, 16,
         { multimedia values }
         1,  2,  4,  8, 16, 32, 64);

       tfloat2tcgsize: array[tfloattype] of tcgsize =
         (OS_F32,OS_F64,OS_F80,OS_F80,OS_C64,OS_C64,OS_F128);

       tcgsize2tfloat: array[OS_F32..OS_C64] of tfloattype =
         (s32real,s64real,s80real,s64comp);

       tvarregable2tcgloc : array[tvarregable] of tcgloc = (LOC_VOID,
          LOC_CREGISTER,LOC_CFPUREGISTER,LOC_CMMREGISTER,LOC_CREGISTER);

{$if defined(cpu64bitalu)}
       { operand size describing an unsigned value in a pair of int registers }
       OS_PAIR = OS_128;
       { operand size describing an signed value in a pair of int registers }
       OS_SPAIR = OS_S128;
{$elseif defined(cpu32bitalu)}
       { operand size describing an unsigned value in a pair of int registers }
       OS_PAIR = OS_64;
       { operand size describing an signed value in a pair of int registers }
       OS_SPAIR = OS_S64;
{$elseif defined(cpu16bitalu)}
       { operand size describing an unsigned value in a pair of int registers }
       OS_PAIR = OS_32;
       { operand size describing an signed value in a pair of int registers }
       OS_SPAIR = OS_S32;
{$elseif defined(cpu8bitalu)}
       { operand size describing an unsigned value in a pair of int registers }
       OS_PAIR = OS_16;
       { operand size describing an signed value in a pair of int registers }
       OS_SPAIR = OS_S16;
{$endif}

       { Table to convert tcgsize variables to the correspondending
         unsigned types }
       tcgsize2unsigned : array[tcgsize] of tcgsize = (OS_NO,
         OS_8,    OS_16,   OS_32,   OS_64,   OS_128,
         OS_8,    OS_16,   OS_32,   OS_64,   OS_128,

         OS_F32,  OS_F64,  OS_F80,  OS_C64,  OS_F128,
         OS_M8,   OS_M16,  OS_M32,  OS_M64,  OS_M128, OS_M256, OS_M512);


       tcgsize2signed : array[tcgsize] of tcgsize = (OS_NO,
         OS_S8,   OS_S16,  OS_S32,  OS_S64,  OS_S128,
         OS_S8,   OS_S16,  OS_S32,  OS_S64,  OS_S128,

         OS_F32,  OS_F64,  OS_F80,  OS_C64,  OS_F128,
         OS_M8,   OS_M16,  OS_M32,  OS_M64,  OS_M128, OS_M256,OS_M512);


       tcgloc2str : array[TCGLoc] of string[12] = (
            'LOC_INVALID',
            'LOC_VOID',
            'LOC_CONST',
            'LOC_JUMP',
            'LOC_FLAGS',
            'LOC_REG',
            'LOC_CREG',
            'LOC_FPUREG',
            'LOC_CFPUREG',
            'LOC_MMXREG',
            'LOC_CMMXREG',
            'LOC_MMREG',
            'LOC_CMMREG',
            'LOC_SSETREG',
            'LOC_CSSETREG',
            'LOC_SSETREF',
            'LOC_CSSETREF',
            'LOC_CREF',
            'LOC_REF'
            );

    var
       mms_movescalar,
       mms_variable,
       mms_2,
       mms_4,
       mms_8,
       mms_16,
       mms_32 : pmmshuffle;

    procedure supregset_reset(out regs:tsuperregisterset;setall:boolean;
                              maxreg:Tsuperregister);{$ifdef USEINLINE}inline;{$endif}
    procedure supregset_include(var regs:tsuperregisterset;s:tsuperregister);{$ifdef USEINLINE}inline;{$endif}
    procedure supregset_exclude(var regs:tsuperregisterset;s:tsuperregister);{$ifdef USEINLINE}inline;{$endif}
    function supregset_in(const regs:tsuperregisterset;s:tsuperregister):boolean;{$ifdef USEINLINE}inline;{$endif}

    function newreg(rt:tregistertype;sr:tsuperregister;sb:tsubregister):tregister;{$ifdef USEINLINE}inline;{$endif}
    function getsubreg(r:tregister):tsubregister;{$ifdef USEINLINE}inline;{$endif}
    function getsupreg(r:tregister):tsuperregister;{$ifdef USEINLINE}inline;{$endif}
    function getregtype(r:tregister):tregistertype;{$ifdef USEINLINE}inline;{$endif}
    procedure setsubreg(var r:tregister;sr:tsubregister);{$ifdef USEINLINE}inline;{$endif}
    procedure setsupreg(var r:tregister;sr:tsuperregister);{$ifdef USEINLINE}inline;{$endif}
    function generic_regname(r:tregister):string;

    {# From a constant numeric value, return the abstract code generator
       size.
    }
    function int_cgsize(const a: tcgint): tcgsize;{$ifdef USEINLINE}inline;{$endif}
    function int_float_cgsize(const a: tcgint): tcgsize;
    function float_array_cgsize(const a: tcgint): tcgsize;{$ifdef USEINLINE}inline;{$endif}
    function double_array_cgsize(const a: tcgint): tcgsize;{$ifdef USEINLINE}inline;{$endif}

    function tcgsize2str(cgsize: tcgsize):string;

    { return the inverse condition of opcmp }
    function inverse_opcmp(opcmp: topcmp): topcmp;{$ifdef USEINLINE}inline;{$endif}

    { return the opcmp needed when swapping the operands }
    function swap_opcmp(opcmp: topcmp): topcmp;{$ifdef USEINLINE}inline;{$endif}

    { return whether op is commutative }
    function commutativeop(op: topcg): boolean;{$ifdef USEINLINE}inline;{$endif}

    { returns true, if shuffle describes a real shuffle operation and not only a move }
    function realshuffle(shuffle : pmmshuffle) : boolean;

    { returns true, if the shuffle describes only a move of the scalar at index 0 }
    function shufflescalar(shuffle : pmmshuffle) : boolean;

    { removes shuffling from shuffle, this means that the destenation index of each shuffle is copied to
      the source }
    procedure removeshuffles(var shuffle : tmmshuffle);

    function is_float_cgsize(size: tcgsize): boolean;{$ifdef USEINLINE}inline;{$endif}

implementation

    uses
      verbose,
      cutils;


{******************************************************************************
                          tsuperregisterworkhashlist
******************************************************************************}

    const
      SuperRegisterWorkHashList_EmptyH: int32 = -1;


    constructor Tsuperregisterworkhashlist.init;
    begin
      h:=@SuperRegisterWorkHashList_EmptyH;
    end;


    constructor Tsuperregisterworkhashlist.copyfrom(const x:Tsuperregisterworkhashlist);
    var
      datasize : SizeUint;
    begin
      if x.FNItems=0 then
        h:=@SuperRegisterWorkHashList_EmptyH
      else
        begin
          datasize:=allocateregion(1+x.hmask,x.maxItems,FItems,h,next);
          Move(x.FItems^,FItems^,datasize);
          FNItems:=x.FNItems;
          hmask:=x.hmask;
          minItems:=x.minItems;
          maxItems:=x.maxItems;
        end;
    end;


    destructor Tsuperregisterworkhashlist.done;
    begin
      if Assigned(FItems) then
        FreeMem(FItems);
    end;


    procedure Tsuperregisterworkhashlist.clear;
    begin
      if Assigned(FItems) then
        begin
          FreeMem(FItems);
          FItems:=nil;
          h:=@SuperRegisterWorkHashList_EmptyH;
          FNItems:=0;
          hmask:=0;
          minItems:=0;
          maxItems:=0;
        end;
    end;


    procedure Tsuperregisterworkhashlist.add(s:tsuperregister);
    var
      ii : SizeInt;
      hp : PInt32;
    begin
      ii:=FNItems;
      if uint32(ii)=maxItems then
        rehash(ii+1);
      FItems[ii]:=s;
      hp:=h+s and hmask;
      next[ii]:=hp^;
      hp^:=ii;
      FNItems:=ii+1;
    end;


    function Tsuperregisterworkhashlist.addnodup(s:tsuperregister):boolean;
    var
      ii : SizeInt;
    begin
      ii:=h[s and hmask];
      while ii>=0 do
        begin
          if FItems[ii]=s then
            exit(false);
          ii:=next[ii];
        end;
      add(s);
      result:=true;
    end;


    function Tsuperregisterworkhashlist.get:tsuperregister;
    var
      ii : SizeInt;
    begin
      ii:=length-1;
      if ii<0 then
        internalerror(202205030);
      result:=FItems[ii];
      deleteidx(ii);
    end;


    procedure Tsuperregisterworkhashlist.deleteidx(i: SizeInt);
    var
      ii,ilast : SizeInt;
      nextp : PInt32;
    begin
      if (i<0) or (i>=length) then
        internalerror(202205031);
      { Remove #i reference from h/next. }
      nextp:=h+FItems[i] and hmask;
      repeat
        ii:=nextp^;
        if ii=i then
          break;
        nextp:=next+ii;
      until false;
      nextp^:=next[ii];

      { Move item #length-1 = #ilast to #i and fix up its reference in h/next. }
      ilast:=length-1;
      if i<>ilast then
        begin
          nextp:=h+FItems[ilast] and hmask;
          repeat
            ii:=nextp^;
            if ii=ilast then
              break;
            nextp:=next+ii;
          until false;
          nextp^:=i;
          FItems[i]:=FItems[ilast];
          next[i]:=next[ilast];
        end;

      FNItems:=ilast;
      if ilast<SizeInt(minItems) then
        rehash(ilast);
    end;


    function Tsuperregisterworkhashlist.delete(s: tsuperregister): boolean;
    var
      ii : SizeInt;
    begin
      ii:=h[s and hmask];
      while ii>=0 do
        begin
          if FItems[ii]=s then
            begin
              deleteidx(ii);
              exit(true);
            end;
          ii:=next[ii];
        end;
      result:=false;
    end;


    procedure Tsuperregisterworkhashlist.sort(less:Tsuperregistercomparefunc; param:pointer);
    var
      p,ih,i:SizeUint;
      t:Tsuperregister;
    begin
      if length<2 then
        exit;
      p:=SizeUint(1) shl BsrDWord(length-1);
      repeat
        for ih:=p to length-1 do
          begin
            i:=ih;
            t:=buf[i];
            repeat
              if not less(t,buf[i-p],param) then
                break;
              buf[i]:=buf[i-p];
              dec(i,p)
            until i<p;
            buf[i]:=t;
          end;
        p:=p shr 1;
      until p=0;
      rebuildh(FItems,h,next,hmask,FNItems);
    end;


    procedure Tsuperregisterworkhashlist.rehash(forItems: SizeUint);
    var
      newHMask,newMinItems,newMaxItems : int32;
      newItems : Psuperregister;
      newH,newNext : PInt32;
    begin
      if forItems=0 then
        begin
          clear;
          exit;
        end;
      newMaxItems:=4+forItems+forItems div 2;
      newHMask:=1 shl (1+BsrDWord(newMaxItems div 8 or 1))-1; { UpToPow2(newMaxItems div 8)-1. Load factor = newMaxItems/newHMask = 400% to 800%. }
      { Well, the whole hash thing is only to prevent the slowdown in extreme cases like webtbs/tw2242, saner loads aren't even desirable:
        for example, load factors 50..100% increase max Tsuperregisterworkhashlist data size in webtbs/tw2242 to 1 Mb up from 500 Kb without any speedup. }
      newMinItems:=SizeUint(forItems) div 8*4;
      if newMinItems=0 then
        newMinItems:=1; { force rehash(0) on emptying. Not necessarily a good idea... }

      allocateregion(1+newHMask,newMaxItems,newItems,newH,newNext);

      Move(FItems^,newItems^,FNItems*sizeof(Tsuperregister));
      if hmask=newHMask then
        begin
          { Shortcut re-adding items if hash mask hasn't changed. }
          Move(h^,newH^,(1+newHMask)*sizeof(h^));
          Move(next^,newNext^,FNItems*sizeof(next^));
        end
      else
        rebuildh(newItems,newH,newNext,newHMask,FNItems);

      if Assigned(FItems) then
        FreeMem(FItems);
      FItems:=newItems;
      h:=newH;
      next:=newNext;
      hmask:=newHMask;
      minItems:=newMinItems;
      maxItems:=newMaxItems;
    end;


    class function Tsuperregisterworkhashlist.allocateregion(
      nh,amaxitems:uint32; out aitems:Psuperregister; out ah:PInt32; out anext:PInt32):SizeUint;
    var
      hOffset,nextOffset,ofs: SizeUint;
    begin
      ofs:=align(sizeof(Tsuperregister)*amaxitems,sizeof(int32)); { items + align to h }
      hOffset:=ofs;
      ofs:=align(ofs+nh*sizeof(int32),sizeof(int32)); { + h + align to next }
      nextOffset:=ofs;
      result:=ofs+amaxitems*sizeof(int32); { + next }
      aitems:=GetMem(result);
      ah:=pointer(aitems)+hOffset;
      anext:=pointer(aitems)+nextOffset;
    end;


    class procedure Tsuperregisterworkhashlist.rebuildh(aitems:Psuperregister; ah,anext:PInt32; ahmask,anitems:int32);
    var
      ii : SizeInt;
      hp : PInt32;
    begin
      FillDWord(ah^,1+ahmask,dword(-1));
      FillDWord(anext^,anitems,dword(-1));
      for ii:=0 to anitems-1 do
        begin
          hp:=ah+aitems[ii] and ahmask;
          anext[ii]:=hp^;
          hp^:=ii;
        end;
    end;


    procedure supregset_reset(out regs:tsuperregisterset;setall:boolean;
                              maxreg:Tsuperregister);{$ifdef USEINLINE}inline;{$endif}

    begin
      fillchar(regs,(maxreg+7) shr 3,-byte(setall));
    end;


    procedure supregset_include(var regs:tsuperregisterset;s:tsuperregister);{$ifdef USEINLINE}inline;{$endif}
      begin
        include(regs[s shr 8],(s and $ff));
      end;


    procedure supregset_exclude(var regs:tsuperregisterset;s:tsuperregister);{$ifdef USEINLINE}inline;{$endif}
      begin
        exclude(regs[s shr 8],(s and $ff));
      end;


    function supregset_in(const regs:tsuperregisterset;s:tsuperregister):boolean;{$ifdef USEINLINE}inline;{$endif}
      begin
        result:=(s and $ff) in regs[s shr 8];
      end;


    function newreg(rt:tregistertype;sr:tsuperregister;sb:tsubregister):tregister;{$ifdef USEINLINE}inline;{$endif}
      begin
        tregisterrec(result).regtype:=rt;
        tregisterrec(result).supreg:=sr;
        tregisterrec(result).subreg:=sb;
      end;


    function getsubreg(r:tregister):tsubregister;{$ifdef USEINLINE}inline;{$endif}
      begin
        result:=tregisterrec(r).subreg;
      end;


    function getsupreg(r:tregister):tsuperregister;{$ifdef USEINLINE}inline;{$endif}
      begin
        result:=tregisterrec(r).supreg;
      end;


    function getregtype(r:tregister):tregistertype;{$ifdef USEINLINE}inline;{$endif}
      begin
        result:=tregisterrec(r).regtype;
      end;


    procedure setsubreg(var r:tregister;sr:tsubregister);{$ifdef USEINLINE}inline;{$endif}
      begin
        tregisterrec(r).subreg:=sr;
      end;


    procedure setsupreg(var r:tregister;sr:tsuperregister);{$ifdef USEINLINE}inline;{$endif}
      begin
        tregisterrec(r).supreg:=sr;
      end;


    function generic_regname(r:tregister):string;
      var
        nr : string[12];
      begin
        str(getsupreg(r),nr);
        case getregtype(r) of
          R_INTREGISTER:
            result:='ireg'+nr;
          R_FPUREGISTER:
            result:='freg'+nr;
          R_MMREGISTER:
            result:='mreg'+nr;
          R_MMXREGISTER:
            result:='xreg'+nr;
          R_ADDRESSREGISTER:
            result:='areg'+nr;
          R_SPECIALREGISTER:
            result:='sreg'+nr;
          else
            begin
              result:='INVALID';
              exit;
            end;
        end;
        case getsubreg(r) of
          R_SUBNONE:
            ;
          R_SUBL:
            result:=result+'l';
          R_SUBH:
            result:=result+'h';
          R_SUBW:
            result:=result+'w';
          R_SUBD:
            result:=result+'d';
          R_SUBQ:
            result:=result+'q';
          R_SUBFS:
            result:=result+'fs';
          R_SUBFD:
            result:=result+'fd';
          R_SUBMMD:
            result:=result+'md';
          R_SUBMMS:
            result:=result+'ms';
          R_SUBMMWHOLE:
            result:=result+'ma';
          R_SUBMMX:
            result:=result+'mx';
          R_SUBMMY:
            result:=result+'my';
          R_SUBMMZ:
            result:=result+'mz';
{$ifdef aarch64}
          R_SUBMM8B:
            result:=result+'m8b';
          R_SUBMM16B:
            result:=result+'m16b';
          R_SUBMM4H:
            result:=result+'m4h';
          R_SUBMM8H:
            result:=result+'m8h';
          R_SUBMM2S:
            result:=result+'m2s';
          R_SUBMM4S:
            result:=result+'m4s';
          R_SUBMM2D:
            result:=result+'m2d';
          R_SUBMMB1:
            result:=result+'mb1';
          R_SUBMMH1:
            result:=result+'mh1';
          R_SUBMMS1:
            result:=result+'ms1';
          R_SUBMMD1:
            result:=result+'md1';
{$endif}
          else
            internalerror(200308252);
        end;
      end;


    function int_cgsize(const a: tcgint): tcgsize;{$ifdef USEINLINE}inline;{$endif}
      const
        size2cgsize : array[0..8] of tcgsize = (
          OS_NO,OS_8,OS_16,OS_NO,OS_32,OS_NO,OS_NO,OS_NO,OS_64
        );
      begin
{$ifdef cpu64bitalu}
        if a=16 then
          result:=OS_128
        else
{$endif cpu64bitalu}
        if a>8 then
          result:=OS_NO
        else
          result:=size2cgsize[a];
      end;


    function int_float_cgsize(const a: tcgint): tcgsize;
      begin
        case a of
          4 :
            result:=OS_F32;
          8 :
            result:=OS_F64;
          10 :
            result:=OS_F80;
          16 :
            result:=OS_F128;
          else
            internalerror(200603211);
        end;
      end;


    function float_array_cgsize(const a: tcgint): tcgsize;{$ifdef USEINLINE}inline;{$endif}
      begin
        case a of
          4:
            result := OS_M32;
          16:
            result := OS_M128;
          32:
            result := OS_M256;
          64:
            result := OS_M512;
          else
            result := int_cgsize(a);
        end;
      end;

    function double_array_cgsize(const a: tcgint): tcgsize;{$ifdef USEINLINE}inline;{$endif}
      begin
        case a of
          8:
            result := OS_M64;
          16:
            result := OS_M128;
          32:
            result := OS_M256;
          64:
            result := OS_M512;
          else
            result := int_cgsize(a);
        end;
      end;


    function tcgsize2str(cgsize: tcgsize):string;
      begin
        Str(cgsize, Result);
      end;


    function inverse_opcmp(opcmp: topcmp): topcmp;{$ifdef USEINLINE}inline;{$endif}
      const
        list: array[TOpCmp] of TOpCmp =
          (OC_NONE,OC_NE,OC_LTE,OC_GTE,OC_LT,OC_GT,OC_EQ,OC_A,OC_AE,
           OC_B,OC_BE);
      begin
        inverse_opcmp := list[opcmp];
      end;


    function swap_opcmp(opcmp: topcmp): topcmp;{$ifdef USEINLINE}inline;{$endif}
      const
        list: array[TOpCmp] of TOpCmp =
          (OC_NONE,OC_EQ,OC_LT,OC_GT,OC_LTE,OC_GTE,OC_NE,OC_AE,OC_A,
           OC_BE,OC_B);
      begin
        swap_opcmp := list[opcmp];
      end;


    function commutativeop(op: topcg): boolean;{$ifdef USEINLINE}inline;{$endif}
      const
        list: array[topcg] of boolean =
          (true,false,true,true,false,false,true,true,false,false,
           true,false,false,false,false,true,false,false);
      begin
        commutativeop := list[op];
      end;


    function realshuffle(shuffle : pmmshuffle) : boolean;
      var
        i : longint;
      begin
        realshuffle:=true;
        if (shuffle=nil) or (shuffle^.len<1) then
          realshuffle:=false
        else
          begin
            for i:=1 to shuffle^.len do
              begin
                if (shuffle^.shuffles[i] and $ff)<>((shuffle^.shuffles[i] and $ff00) shr 8) then
                  exit;
              end;
            realshuffle:=false;
          end;
      end;


    function shufflescalar(shuffle : pmmshuffle) : boolean;
      begin
        result:=shuffle^.len=0;
      end;


    procedure removeshuffles(var shuffle : tmmshuffle);
      var
        i : longint;
      begin
        if shuffle.len=0 then
          exit;
        for i:=1 to shuffle.len do
          shuffle.shuffles[i]:=(shuffle.shuffles[i] and $f) or ((shuffle.shuffles[i] and $f0) shr 4);
      end;


    function is_float_cgsize(size: tcgsize): boolean;{$ifdef USEINLINE}inline;{$endif}
      begin
        result:=size in [OS_F32..OS_F128];
      end;


   procedure Initmms(var p : pmmshuffle;len : ShortInt);
     var
       i : Integer;
     begin
       Getmem(p,sizeof(tmmshuffle)+(max(len,0)-1)*2);
       p^.len:=len;
       for i:=1 to len do
{$push}
{$R-}
         p^.shuffles[i]:=i;
{$pop}
     end;

initialization
  Initmms(mms_movescalar,0);
  Initmms(mms_variable,-1);
  Initmms(mms_2,2);
  Initmms(mms_4,4);
  Initmms(mms_8,8);
  Initmms(mms_16,16);
  Initmms(mms_32,32);
finalization
  Freemem(mms_movescalar);
  Freemem(mms_variable);
  Freemem(mms_2);
  Freemem(mms_4);
  Freemem(mms_8);
  Freemem(mms_16);
  Freemem(mms_32);
end.

