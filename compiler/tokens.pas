{
    Copyright (c) 1998-2002 by Florian Klaempfl, Pierre Muller

    Tokens used by the compiler

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
unit tokens;

{$i fpcdefs.inc}

interface

uses
  globtype;

type
  ttoken=(NOTOKEN,
    { operators, which can also be overloaded }
    _PLUS,
    _MINUS,
    _STAR,
    _SLASH,
    _EQ,
    _GT,
    _LT,
    _GTE,
    _LTE,
    _NE,
    _SYMDIF,
    _STARSTAR,
    _OP_AS,
    _OP_IN,
    _OP_IS,
    _OP_OR,
    _OP_AND,
    _OP_DIV,
    _OP_MOD,
    _OP_NOT,
    _OP_SHL,
    _OP_SHR,
    _OP_XOR,
    _ASSIGNMENT,
    _OP_EXPLICIT,
    _OP_ENUMERATOR,
    _OP_INITIALIZE,
    _OP_FINALIZE,
    _OP_ADDREF,
    _OP_COPY,
    _OP_INC,
    _OP_DEC,
    { special chars }
    _CARET,
    _LECKKLAMMER, { '[' }
    _RECKKLAMMER, { ']' }
    _POINT,
    _COMMA,
    _LKLAMMER, { '(' }
    _RKLAMMER, { ')' }
    _COLON,
    _SEMICOLON,
    _KLAMMERAFFE, { '@' }
    _POINTPOINT,
    _POINTPOINTPOINT,
    _PIPE,
    _AMPERSAND,
    _EOF,
    _ID,
    _NOID,
    _REALNUMBER,
    _INTCONST,
    _CSTRING,
    _CCHAR,
    _CWSTRING,
    _CWCHAR,
    _LSHARPBRACKET,
    _RSHARPBRACKET,
    { C like operators }
    _PLUSASN,
    _MINUSASN,
    _ANDASN,
    _ORASN,
    _STARASN,
    _SLASHASN,
    _MODASN,
    _DIVASN,
    _NOTASN,
    _XORASN,
    _GENERICSPECIALTOKEN,
    { Normal words -- ATTENTION: These words must be sorted: }
    { first in length order, then in alphabetical order.     }
    _C,
    _AS,
    _AT,
    _DO,
    _IF,
    _IN,
    _IS,
    _OF,
    _ON,
    _OR,
    _TO,
    _ADD,
    _AND,
    _ASM,
    _DEC,
    _DIV,
    _END,
    _FAR,
    _FOR,
    _INC,
    _MOD,
    _NIL,
    _NOT,
    _OUT,
    _SET,
    _SHL,
    _SHR,
    _TRY,
    _VAR,
    _XOR,
    _CASE,
    _COPY,
    _CVAR,
    _ELSE,
    _EXIT,
    _FAIL,
    _FILE,
    _GOTO,
    _HUGE,
    _LAST,
    _NAME,
    _NEAR,
    _READ,
    _SELF,
    _SYSV,
    _THEN,
    _TYPE,
    _UNIT,
    _UNIV,
    _USES,
    _WITH,
    _ALIAS,
    _ARRAY,
    _BEGIN,
    _BREAK,
    _CDECL,
    _CLASS,
    _CONST,
    _EQUAL,
    _FAR16,
    _FINAL,
    _FIRST,
    _INDEX,
    _LABEL,
    _LOCAL,
    _RAISE,
    _UNTIL,
    _WHILE,
    _WRITE,
    _ADDREF,
    _CBLOCK,
    _DISPID,
    _DIVIDE,
    _DOWNTO,
    _EXCEPT,
    _EXPORT,
    _HELPER,
    _INLINE,
    _LEGACY,
    _NESTED,
    _OBJECT,
    _PACKED,
    _PASCAL,
    _PUBLIC,
    _RECORD,
    _REPEAT,
    _RESULT,
    _RETURN,
    _SEALED,
    _STATIC,
    _STORED,
    _STRICT,
    _STRING,
    _SYSTEM,
    _WINAPI,
    _ASMNAME,
    _BASEREG,
    _CPPDECL,
    _DEFAULT,
    _DYNAMIC,
    _EXPORTS,
    _FINALLY,
    _FORWARD,
    _GENERIC,
    _IOCHECK,
    _LIBRARY,
    _MESSAGE,
    _MODULUS,
    _PACKAGE,
    _PRIVATE,
    _PROGRAM,
    _R12BASE,
    _RTLPROC,
    _SECTION,
    _STDCALL,
    _SYSCALL,
    _VARARGS,
    _VIRTUAL,
    _ABSOLUTE,
    _ABSTRACT,
    _BASELAST,
    _BASENONE,
    _BASESYSV,
    _CONSTREF,
    _CONTAINS,
    _CONTINUE,
    _CPPCLASS,
    _EXPLICIT,
    _EXTERNAL,
    _FINALIZE,
    _FUNCTION,
    _IMPLICIT,
    _LESSTHAN,
    _LOCATION,
    _MULTIPLY,
    _MWPASCAL,
    _NEGATIVE,
    _NOINLINE,
    _NORETURN,
    _NOTEQUAL,
    _OPERATOR,
    _OPTIONAL,
    _OVERLOAD,
    _OVERRIDE,
    _PLATFORM,
    _POSITIVE,
    _PROPERTY,
    _READONLY,
    _REGISTER,
    _REQUIRED,
    _REQUIRES,
    _RESIDENT,
    _SAFECALL,
    _SUBTRACT,
    _SYSVBASE,
    _ASSEMBLER,
    _BASEFIRST,
    _BITPACKED,
    _BITWISEOR,
    _HARDFLOAT,
    _INHERITED,
    _INTDIVIDE,
    _INTERFACE,
    _INTERRUPT,
    _LEFTSHIFT,
    _LOGICALOR,
    _NODEFAULT,
    _OBJCCLASS,
    _OTHERWISE,
    _PROCEDURE,
    _PROMISING,
    _PROTECTED,
    _PUBLISHED,
    _REFERENCE,
    _SOFTFLOAT,
    _THREADVAR,
    _WRITEONLY,
    _BITWISEAND,
    _BITWISEXOR,
    _DEPRECATED,
    _DESTRUCTOR,
    _ENUMERATOR,
    _IMPLEMENTS,
    _INITIALIZE,
    _INTERNPROC,
    _LOGICALAND,
    _LOGICALNOT,
    _LOGICALXOR,
    _OLDFPCCALL,
    _OPENSTRING,
    _RIGHTSHIFT,
    _SPECIALIZE,
    _SUSPENDING,
    _VECTORCALL,
    _CONSTRUCTOR,
    _GREATERTHAN,
    _INTERNCONST,
    _REINTRODUCE,
    _SHORTSTRING,
    _WASMFUNCREF,
    _COMPILERPROC,
    _EXPERIMENTAL,
    _FINALIZATION,
    _MS_ABI_CDECL,
    _NOSTACKFRAME,
    _OBJCCATEGORY,
    _OBJCPROTOCOL,
    _WEAKEXTERNAL,
    _DISPINTERFACE,
    _UNIMPLEMENTED,
    _IMPLEMENTATION,
    _INITIALIZATION,
    _MS_ABI_DEFAULT,
    _RESOURCESTRING,
    _SYSV_ABI_CDECL,
    _LESSTHANOREQUAL,
    _SYSV_ABI_DEFAULT,
    _GREATERTHANOREQUAL
  );
  pttoken=^ttoken;

  { sub_expr(opmultiply) is need to get -1 ** 4 to be
    read as - (1**4) and not (-1)**4 PM }
  toperator_precedence=(
    opcompare,
    opaddition,
    opmultiply,
    oppower
  );

const
  tokenlenmin = 1;
  tokenlenmax = 18;

  postfixoperator_tokens = [_CARET,_POINT,_LECKKLAMMER];

  { last operator which can be overloaded, the first_overloaded should
    be declared directly after NOTOKEN }
  first_overloaded = succ(NOTOKEN);
  last_overloaded  = _OP_DEC;
  last_operator = _GENERICSPECIALTOKEN;
  first_managment_operator = _OP_INITIALIZE;
  last_managment_operator = _OP_COPY;

  highest_precedence = oppower;

  { Warning these stay be ordered !! }
  operator_levels:array[Toperator_precedence] of set of NOTOKEN..last_operator=
      ([_LT,_LTE,_GT,_GTE,_EQ,_NE,_OP_IN,_OP_IS],
       [_PLUS,_MINUS,_OP_OR,_PIPE,_OP_XOR],
       [_CARET,_SYMDIF,_STARSTAR,_STAR,_SLASH,
        _OP_AS,_OP_AND,_AMPERSAND,_OP_DIV,_OP_MOD,_OP_SHL,_OP_SHR],
       [_STARSTAR] );

type
  tokenrec=record
    str     : string[tokenlenmax];
    special : boolean;
    keyword : tmodeswitches;
    to_op   : ttoken;
  end;

  ttokenarray=array[ttoken] of tokenrec;
  ptokenarray=^ttokenarray;

  tokenidxrec=record
    first,last : ttoken;
  end;

  ptokenidx=^ttokenidx;
  ttokenidx=array[tokenlenmin..tokenlenmax,'A'..'Z'] of tokenidxrec;

const
  tokeninfo : ttokenarray =(
      (str:''              ;special:true ;keyword:[m_none];                          to_op:NOTOKEN),
    { Operators which can be overloaded }
      (str:'+'             ;special:true ;keyword:[m_none];                          to_op:_PLUS),
      (str:'-'             ;special:true ;keyword:[m_none];                          to_op:_MINUS),
      (str:'*'             ;special:true ;keyword:[m_none];                          to_op:_STAR),
      (str:'/'             ;special:true ;keyword:[m_none];                          to_op:_SLASH),
      (str:'='             ;special:true ;keyword:[m_none];                          to_op:_EQ),
      (str:'>'             ;special:true ;keyword:[m_none];                          to_op:_GT),
      (str:'<'             ;special:true ;keyword:[m_none];                          to_op:_LT),
      (str:'>='            ;special:true ;keyword:[m_none];                          to_op:_GTE),
      (str:'<='            ;special:true ;keyword:[m_none];                          to_op:_LTE),
      (str:'<>'            ;special:true ;keyword:[m_none];                          to_op:_NE),
      (str:'><'            ;special:true ;keyword:[m_none];                          to_op:_SYMDIF),
      (str:'**'            ;special:true ;keyword:[m_none];                          to_op:_STARSTAR),
      (str:'as'            ;special:true ;keyword:[m_none];                          to_op:_OP_AS),
      (str:'in'            ;special:true ;keyword:[m_none];                          to_op:_OP_IN),
      (str:'is'            ;special:true ;keyword:[m_none];                          to_op:_OP_IS),
      (str:'or'            ;special:true ;keyword:[m_none];                          to_op:_OP_OR),
      (str:'and'           ;special:true ;keyword:[m_none];                          to_op:_OP_AND),
      (str:'div'           ;special:true ;keyword:[m_none];                          to_op:_OP_DIV),
      (str:'mod'           ;special:true ;keyword:[m_none];                          to_op:_OP_MOD),
      (str:'not'           ;special:true ;keyword:[m_none];                          to_op:_OP_NOT),
      (str:'shl'           ;special:true ;keyword:[m_none];                          to_op:_OP_SHL),
      (str:'shr'           ;special:true ;keyword:[m_none];                          to_op:_OP_SHR),
      (str:'xor'           ;special:true ;keyword:[m_none];                          to_op:_OP_XOR),
      (str:':='            ;special:true ;keyword:[m_none];                          to_op:_ASSIGNMENT),
      (str:'explicit'      ;special:true ;keyword:[m_none];                          to_op:_OP_EXPLICIT),
      (str:'enumerator'    ;special:true ;keyword:[m_none];                          to_op:_OP_ENUMERATOR),
      (str:'initialize'    ;special:true ;keyword:[m_none];                          to_op:_OP_INITIALIZE),
      (str:'finalize'      ;special:true ;keyword:[m_none];                          to_op:_OP_FINALIZE),
      (str:'addref'        ;special:true ;keyword:[m_none];                          to_op:_OP_ADDREF),
      (str:'copy'          ;special:true ;keyword:[m_none];                          to_op:_OP_COPY),
      (str:'inc'           ;special:true ;keyword:[m_none];                          to_op:_OP_INC),
      (str:'dec'           ;special:true ;keyword:[m_none];                          to_op:_OP_DEC),
    { Special chars }
      (str:'^'             ;special:true ;keyword:[m_none];                          to_op:_CARET),
      (str:'['             ;special:true ;keyword:[m_none];                          to_op:_LECKKLAMMER),
      (str:']'             ;special:true ;keyword:[m_none];                          to_op:_RECKKLAMMER),
      (str:'.'             ;special:true ;keyword:[m_none];                          to_op:_POINT),
      (str:','             ;special:true ;keyword:[m_none];                          to_op:_COMMA),
      (str:'('             ;special:true ;keyword:[m_none];                          to_op:_LKLAMMER),
      (str:')'             ;special:true ;keyword:[m_none];                          to_op:_RKLAMMER),
      (str:':'             ;special:true ;keyword:[m_none];                          to_op:_COLON),
      (str:';'             ;special:true ;keyword:[m_none];                          to_op:_SEMICOLON),
      (str:'@'             ;special:true ;keyword:[m_none];                          to_op:_KLAMMERAFFE),
      (str:'..'            ;special:true ;keyword:[m_none];                          to_op:_POINTPOINT),
      (str:'...'           ;special:true ;keyword:[m_none];                          to_op:_POINTPOINTPOINT),
      (str:'|'             ;special:true ;keyword:[m_none];                          to_op:_PIPE),
      (str:'&'             ;special:true ;keyword:[m_none];                          to_op:_AMPERSAND),
      (str:'end of file'   ;special:true ;keyword:[m_none];                          to_op:_EOF),
      (str:'identifier'    ;special:true ;keyword:[m_none];                          to_op:_ID),
      (str:'non identifier';special:true ;keyword:[m_none];                          to_op:_NOID),
      (str:'const real'    ;special:true ;keyword:[m_none];                          to_op:_REALNUMBER),
      (str:'ordinal const' ;special:true ;keyword:[m_none];                          to_op:_INTCONST),
      (str:'const string'  ;special:true ;keyword:[m_none];                          to_op:_CSTRING),
      (str:'const char'    ;special:true ;keyword:[m_none];                          to_op:_CCHAR),
      (str:'const wstring' ;special:true ;keyword:[m_none];                          to_op:_CWSTRING),
      (str:'const wchar'   ;special:true ;keyword:[m_none];                          to_op:_CWCHAR),
      (str:'<'             ;special:true ;keyword:[m_none];                          to_op:_LSHARPBRACKET),
      (str:'>'             ;special:true ;keyword:[m_none];                          to_op:_RSHARPBRACKET),
    { C like operators }
      (str:'+='            ;special:true ;keyword:[m_none];                          to_op:_PLUSASN),
      (str:'-='            ;special:true ;keyword:[m_none];                          to_op:_MINUSASN),
      (str:'&='            ;special:true ;keyword:[m_none];                          to_op:_ANDASN),
      (str:'|='            ;special:true ;keyword:[m_none];                          to_op:_ORASN),
      (str:'*='            ;special:true ;keyword:[m_none];                          to_op:_STARASN),
      (str:'/='            ;special:true ;keyword:[m_none];                          to_op:_SLASHASN),
      (str:''              ;special:true ;keyword:[m_none];                          to_op:_MODASN),
      (str:''              ;special:true ;keyword:[m_none];                          to_op:_DIVASN),
      (str:''              ;special:true ;keyword:[m_none];                          to_op:_NOTASN),
      (str:''              ;special:true ;keyword:[m_none];                          to_op:_XORASN),
      (str:'gen. spec.'    ;special:true ;keyword:[m_none];                          to_op:_GENERICSPECIALTOKEN),
    { Normal words -- ATTENTION: These words must be sorted: }
    { first in length order, then in alphabetical order.     }
      (str:'C'             ;special:false;keyword:[m_none];                          to_op:_C),
      (str:'AS'            ;special:false;keyword:[m_class];                         to_op:_OP_AS),
      (str:'AT'            ;special:false;keyword:[m_none];                          to_op:_AT),
      (str:'DO'            ;special:false;keyword:alllanguagemodes;                  to_op:_DO),
      (str:'IF'            ;special:false;keyword:alllanguagemodes;                  to_op:_IF),
      (str:'IN'            ;special:false;keyword:alllanguagemodes;                  to_op:_OP_IN),
      (str:'IS'            ;special:false;keyword:[m_class];                         to_op:_OP_IS),
      (str:'OF'            ;special:false;keyword:alllanguagemodes;                  to_op:_OF),
      (str:'ON'            ;special:false;keyword:[m_none];                          to_op:_ON),
      (str:'OR'            ;special:false;keyword:alllanguagemodes;                  to_op:_OP_OR),
      (str:'TO'            ;special:false;keyword:alllanguagemodes;                  to_op:_TO),
      (str:'ADD'           ;special:false;keyword:[m_none];                          to_op:_ADD), { delphi operator name }
      (str:'AND'           ;special:false;keyword:alllanguagemodes;                  to_op:_OP_AND),
      (str:'ASM'           ;special:false;keyword:alllanguagemodes-[m_iso];          to_op:_ASM),
      (str:'DEC'           ;special:false;keyword:[m_none];                          to_op:_DEC), { delphi operator name }
      (str:'DIV'           ;special:false;keyword:alllanguagemodes;                  to_op:_OP_DIV),
      (str:'END'           ;special:false;keyword:alllanguagemodes;                  to_op:_END),
      (str:'FAR'           ;special:false;keyword:[m_none];                          to_op:_FAR),
      (str:'FOR'           ;special:false;keyword:alllanguagemodes;                  to_op:_FOR),
      (str:'INC'           ;special:false;keyword:[m_none];                          to_op:_INC), { delphi operator name }
      (str:'MOD'           ;special:false;keyword:alllanguagemodes;                  to_op:_OP_MOD),
      (str:'NIL'           ;special:false;keyword:alllanguagemodes;                  to_op:_NIL),
      (str:'NOT'           ;special:false;keyword:alllanguagemodes;                  to_op:_OP_NOT),
      (str:'OUT'           ;special:false;keyword:[m_none];                          to_op:_OUT),
      (str:'SET'           ;special:false;keyword:alllanguagemodes;                  to_op:_SET),
      (str:'SHL'           ;special:false;keyword:alllanguagemodes-[m_iso,m_extpas]; to_op:_OP_SHL),
      (str:'SHR'           ;special:false;keyword:alllanguagemodes-[m_iso,m_extpas]; to_op:_OP_SHR),
      (str:'TRY'           ;special:false;keyword:[m_except];                        to_op:_TRY),
      (str:'VAR'           ;special:false;keyword:alllanguagemodes;                  to_op:_VAR),
      (str:'XOR'           ;special:false;keyword:alllanguagemodes;                  to_op:_OP_XOR),
      (str:'CASE'          ;special:false;keyword:alllanguagemodes;                  to_op:_CASE),
      (str:'COPY'          ;special:false;keyword:[m_none];                          to_op:_COPY),
      (str:'CVAR'          ;special:false;keyword:[m_none];                          to_op:_CVAR),
      (str:'ELSE'          ;special:false;keyword:alllanguagemodes;                  to_op:_ELSE),
      (str:'EXIT'          ;special:false;keyword:[m_none];                          to_op:_EXIT),
      (str:'FAIL'          ;special:false;keyword:[m_none];                          to_op:_FAIL), { only set within constructors PM }
      (str:'FILE'          ;special:false;keyword:alllanguagemodes;                  to_op:_FILE),
      (str:'GOTO'          ;special:false;keyword:alllanguagemodes;                  to_op:_GOTO),
      (str:'HUGE'          ;special:false;keyword:[m_none];                          to_op:_HUGE),
      (str:'LAST'          ;special:false;keyword:[m_none];                          to_op:_LAST),
      (str:'NAME'          ;special:false;keyword:[m_none];                          to_op:_NAME),
      (str:'NEAR'          ;special:false;keyword:[m_none];                          to_op:_NEAR),
      (str:'READ'          ;special:false;keyword:[m_none];                          to_op:_READ),
      (str:'SELF'          ;special:false;keyword:[m_none];                          to_op:_SELF), {set inside methods only PM }
      (str:'SYSV'          ;special:false;keyword:[m_none];                          to_op:_SYSV), { Syscall variation on MorphOS }
      (str:'THEN'          ;special:false;keyword:alllanguagemodes;                  to_op:_THEN),
      (str:'TYPE'          ;special:false;keyword:alllanguagemodes;                  to_op:_TYPE),
      (str:'UNIT'          ;special:false;keyword:alllanguagemodes-[m_iso,m_extpas]; to_op:_UNIT),
      (str:'UNIV'          ;special:false;keyword:[m_mac];                           to_op:_UNIV),
      (str:'USES'          ;special:false;keyword:alllanguagemodes-[m_iso,m_extpas]; to_op:_USES),
      (str:'WITH'          ;special:false;keyword:alllanguagemodes;                  to_op:_WITH),
      (str:'ALIAS'         ;special:false;keyword:[m_none];                          to_op:_ALIAS),
      (str:'ARRAY'         ;special:false;keyword:alllanguagemodes;                  to_op:_ARRAY),
      (str:'BEGIN'         ;special:false;keyword:alllanguagemodes;                  to_op:_BEGIN),
      (str:'BREAK'         ;special:false;keyword:[m_none];                          to_op:_BREAK),
      (str:'CDECL'         ;special:false;keyword:[m_none];                          to_op:_CDECL),
      (str:'CLASS'         ;special:false;keyword:[m_class];                         to_op:_CLASS),
      (str:'CONST'         ;special:false;keyword:alllanguagemodes;                  to_op:_CONST),
      (str:'EQUAL'         ;special:false;keyword:[m_none];                          to_op:_EQUAL), { delphi operator name }
      (str:'FAR16'         ;special:false;keyword:[m_none];                          to_op:_FAR16),
      (str:'FINAL'         ;special:false;keyword:[m_none];                          to_op:_FINAL),
      (str:'FIRST'         ;special:false;keyword:[m_none];                          to_op:_FIRST),
      (str:'INDEX'         ;special:false;keyword:[m_none];                          to_op:_INDEX),
      (str:'LABEL'         ;special:false;keyword:alllanguagemodes;                  to_op:_LABEL),
      (str:'LOCAL'         ;special:false;keyword:[m_none];                          to_op:_LOCAL),
      (str:'RAISE'         ;special:false;keyword:[m_except];                        to_op:_RAISE),
      (str:'UNTIL'         ;special:false;keyword:alllanguagemodes;                  to_op:_UNTIL),
      (str:'WHILE'         ;special:false;keyword:alllanguagemodes;                  to_op:_WHILE),
      (str:'WRITE'         ;special:false;keyword:[m_none];                          to_op:_WRITE),
      (str:'ADDREF'        ;special:false;keyword:[m_none];                          to_op:_ADDREF),
      (str:'CBLOCK'        ;special:false;keyword:[m_none];                          to_op:_CBLOCK),
      (str:'DISPID'        ;special:false;keyword:[m_none];                          to_op:_DISPID),
      (str:'DIVIDE'        ;special:false;keyword:[m_none];                          to_op:_DIVIDE), { delphi operator name }
      (str:'DOWNTO'        ;special:false;keyword:alllanguagemodes;                  to_op:_DOWNTO),
      (str:'EXCEPT'        ;special:false;keyword:[m_except];                        to_op:_EXCEPT),
      (str:'EXPORT'        ;special:false;keyword:[m_none];                          to_op:_EXPORT),
      (str:'HELPER'        ;special:false;keyword:[m_none];                          to_op:_HELPER),
      (str:'INLINE'        ;special:false;keyword:[m_tp7];                           to_op:_INLINE),
      (str:'LEGACY'        ;special:false;keyword:[m_none];                          to_op:_LEGACY), { Syscall variation on MorphOS }
      (str:'NESTED'        ;special:false;keyword:[m_none];                          to_op:_NESTED),
      (str:'OBJECT'        ;special:false;keyword:alllanguagemodes-[m_iso,m_extpas]; to_op:_OBJECT),
      (str:'PACKED'        ;special:false;keyword:alllanguagemodes;                  to_op:_PACKED),
      (str:'PASCAL'        ;special:false;keyword:[m_none];                          to_op:_PASCAL),
      (str:'PUBLIC'        ;special:false;keyword:[m_none];                          to_op:_PUBLIC),
      (str:'RECORD'        ;special:false;keyword:alllanguagemodes;                  to_op:_RECORD),
      (str:'REPEAT'        ;special:false;keyword:alllanguagemodes;                  to_op:_REPEAT),
      (str:'RESULT'        ;special:false;keyword:[m_none];                          to_op:_RESULT),
      (str:'RETURN'        ;special:false;keyword:[m_mac];                           to_op:_RETURN),
      (str:'SEALED'        ;special:false;keyword:[m_none];                          to_op:_SEALED),
      (str:'STATIC'        ;special:false;keyword:[m_none];                          to_op:_STATIC),
      (str:'STORED'        ;special:false;keyword:[m_none];                          to_op:_STORED),
      (str:'STRICT'        ;special:false;keyword:[m_none];                          to_op:_STRICT),
      (str:'STRING'        ;special:false;keyword:alllanguagemodes-[m_iso,m_extpas]; to_op:_STRING),
      (str:'SYSTEM'        ;special:false;keyword:[m_none];                          to_op:_SYSTEM),
      (str:'WINAPI'        ;special:false;keyword:[m_none];                          to_op:_WINAPI),
      (str:'ASMNAME'       ;special:false;keyword:[m_none];                          to_op:_ASMNAME),
      (str:'BASEREG'       ;special:false;keyword:[m_none];                          to_op:_BASEREG), { Syscall variation on Amiga-likes }
      (str:'CPPDECL'       ;special:false;keyword:[m_none];                          to_op:_CPPDECL),
      (str:'DEFAULT'       ;special:false;keyword:[m_none];                          to_op:_DEFAULT),
      (str:'DYNAMIC'       ;special:false;keyword:[m_none];                          to_op:_DYNAMIC),
      (str:'EXPORTS'       ;special:false;keyword:alllanguagemodes-[m_iso,m_extpas]; to_op:_EXPORTS),
      (str:'FINALLY'       ;special:false;keyword:[m_except];                        to_op:_FINALLY),
      (str:'FORWARD'       ;special:false;keyword:[m_none];                          to_op:_FORWARD),
      (str:'GENERIC'       ;special:false;keyword:[m_none];                          to_op:_GENERIC),
      (str:'IOCHECK'       ;special:false;keyword:[m_none];                          to_op:_IOCHECK),
      (str:'LIBRARY'       ;special:false;keyword:alllanguagemodes-[m_iso,m_extpas]; to_op:_LIBRARY),
      (str:'MESSAGE'       ;special:false;keyword:[m_none];                          to_op:_MESSAGE),
      (str:'MODULUS'       ;special:false;keyword:[m_none];                          to_op:_MODULUS), { delphi operator name }
      (str:'PACKAGE'       ;special:false;keyword:[m_none];                          to_op:_PACKAGE),
      (str:'PRIVATE'       ;special:false;keyword:[m_none];                          to_op:_PRIVATE),
      (str:'PROGRAM'       ;special:false;keyword:alllanguagemodes;                  to_op:_PROGRAM),
      (str:'R12BASE'       ;special:false;keyword:[m_none];                          to_op:_R12BASE), { Syscall variation on MorphOS }
      (str:'RTLPROC'       ;special:false;keyword:[m_none];                          to_op:_RTLPROC),
      (str:'SECTION'       ;special:false;keyword:[m_none];                          to_op:_SECTION),
      (str:'STDCALL'       ;special:false;keyword:[m_none];                          to_op:_STDCALL),
      (str:'SYSCALL'       ;special:false;keyword:[m_none];                          to_op:_SYSCALL),
      (str:'VARARGS'       ;special:false;keyword:[m_none];                          to_op:_VARARGS),
      (str:'VIRTUAL'       ;special:false;keyword:[m_none];                          to_op:_VIRTUAL),
      (str:'ABSOLUTE'      ;special:false;keyword:[m_none];                          to_op:_ABSOLUTE),
      (str:'ABSTRACT'      ;special:false;keyword:[m_none];                          to_op:_ABSTRACT),
      (str:'BASELAST'      ;special:false;keyword:[m_none];                          to_op:_BASELAST), { Syscall variation on Amiga-likes }
      (str:'BASENONE'      ;special:false;keyword:[m_none];                          to_op:_BASENONE), { Syscall variation on Amiga-likes }
      (str:'BASESYSV'      ;special:false;keyword:[m_none];                          to_op:_BASESYSV), { Syscall variation on MorphOS }
      (str:'CONSTREF'      ;special:false;keyword:[m_none];                          to_op:_CONSTREF),
      (str:'CONTAINS'      ;special:false;keyword:[m_none];                          to_op:_CONTAINS),
      (str:'CONTINUE'      ;special:false;keyword:[m_none];                          to_op:_CONTINUE),
      (str:'CPPCLASS'      ;special:false;keyword:[m_fpc];                           to_op:_CPPCLASS),
      (str:'EXPLICIT'      ;special:false;keyword:[m_none];                          to_op:_EXPLICIT), { delphi operator name }
      (str:'EXTERNAL'      ;special:false;keyword:[m_none];                          to_op:_EXTERNAL),
      (str:'FINALIZE'      ;special:false;keyword:[m_none];                          to_op:_FINALIZE),
      (str:'FUNCTION'      ;special:false;keyword:alllanguagemodes;                  to_op:_FUNCTION),
      (str:'IMPLICIT'      ;special:false;keyword:[m_none];                          to_op:_IMPLICIT), { delphi operator name }
      (str:'LESSTHAN'      ;special:false;keyword:[m_none];                          to_op:_LESSTHAN), { delphi operator name }
      (str:'LOCATION'      ;special:false;keyword:[m_none];                          to_op:_LOCATION),
      (str:'MULTIPLY'      ;special:false;keyword:[m_none];                          to_op:_MULTIPLY), { delphi operator name }
      (str:'MWPASCAL'      ;special:false;keyword:[m_none];                          to_op:_MWPASCAL),
      (str:'NEGATIVE'      ;special:false;keyword:[m_none];                          to_op:_NEGATIVE), { delphi operator name }
      (str:'NOINLINE'      ;special:false;keyword:[m_none];                          to_op:_NOINLINE),
      (str:'NORETURN'      ;special:false;keyword:[m_none];                          to_op:_NORETURN),
      (str:'NOTEQUAL'      ;special:false;keyword:[m_none];                          to_op:_NOTEQUAL), { delphi operator name }
      (str:'OPERATOR'      ;special:false;keyword:[m_fpc];                           to_op:_OPERATOR),
      (str:'OPTIONAL'      ;special:false;keyword:[m_none];                          to_op:_OPTIONAL), { optional methods in an Objective-C protocol }
      (str:'OVERLOAD'      ;special:false;keyword:[m_none];                          to_op:_OVERLOAD),
      (str:'OVERRIDE'      ;special:false;keyword:[m_none];                          to_op:_OVERRIDE),
      (str:'PLATFORM'      ;special:false;keyword:[m_none];                          to_op:_PLATFORM),
      (str:'POSITIVE'      ;special:false;keyword:[m_none];                          to_op:_POSITIVE), { delphi operator name }
      (str:'PROPERTY'      ;special:false;keyword:[m_property];                      to_op:_PROPERTY),
      (str:'READONLY'      ;special:false;keyword:[m_none];                          to_op:_READONLY),
      (str:'REGISTER'      ;special:false;keyword:[m_none];                          to_op:_REGISTER),
      (str:'REQUIRED'      ;special:false;keyword:[m_none];                          to_op:_REQUIRED), { required methods in an Objective-C protocol }
      (str:'REQUIRES'      ;special:false;keyword:[m_none];                          to_op:_REQUIRES),
      (str:'RESIDENT'      ;special:false;keyword:[m_none];                          to_op:_RESIDENT),
      (str:'SAFECALL'      ;special:false;keyword:[m_none];                          to_op:_SAFECALL),
      (str:'SUBTRACT'      ;special:false;keyword:[m_none];                          to_op:_SUBTRACT), { delphi operator name }
      (str:'SYSVBASE'      ;special:false;keyword:[m_none];                          to_op:_SYSVBASE), { Syscall variation on MorphOS }
      (str:'ASSEMBLER'     ;special:false;keyword:[m_none];                          to_op:_ASSEMBLER),
      (str:'BASEFIRST'     ;special:false;keyword:[m_none];                          to_op:_BASEFIRST), { Syscall variation on Amiga-likes }
      (str:'BITPACKED'     ;special:false;keyword:alllanguagemodes-[m_iso,m_extpas]; to_op:_BITPACKED),
      (str:'BITWISEOR'     ;special:false;keyword:[m_none];                          to_op:_BITWISEOR), { delphi operator name }
      (str:'HARDFLOAT'     ;special:false;keyword:[m_none];                          to_op:_HARDFLOAT),
      (str:'INHERITED'     ;special:false;keyword:alllanguagemodes-[m_iso,m_extpas]; to_op:_INHERITED),
      (str:'INTDIVIDE'     ;special:false;keyword:[m_none];                          to_op:_INTDIVIDE), { delphi operator name }
      (str:'INTERFACE'     ;special:false;keyword:alllanguagemodes-[m_iso,m_extpas]; to_op:_INTERFACE),
      (str:'INTERRUPT'     ;special:false;keyword:[m_none];                          to_op:_INTERRUPT),
      (str:'LEFTSHIFT'     ;special:false;keyword:[m_none];                          to_op:_LEFTSHIFT), { delphi operator name }
      (str:'LOGICALOR'     ;special:false;keyword:[m_none];                          to_op:_LOGICALOR), { delphi operator name }
      (str:'NODEFAULT'     ;special:false;keyword:[m_none];                          to_op:_NODEFAULT),
      (str:'OBJCCLASS'     ;special:false;keyword:[m_objectivec1];                   to_op:_OBJCCLASS),
      (str:'OTHERWISE'     ;special:false;keyword:alllanguagemodes-[m_iso];          to_op:_OTHERWISE),
      (str:'PROCEDURE'     ;special:false;keyword:alllanguagemodes;                  to_op:_PROCEDURE),
      (str:'PROMISING'     ;special:false;keyword:[m_none];                          to_op:_PROMISING),
      (str:'PROTECTED'     ;special:false;keyword:[m_none];                          to_op:_PROTECTED),
      (str:'PUBLISHED'     ;special:false;keyword:[m_none];                          to_op:_PUBLISHED),
      (str:'REFERENCE'     ;special:false;keyword:[m_none];                          to_op:_REFERENCE),
      (str:'SOFTFLOAT'     ;special:false;keyword:[m_none];                          to_op:_SOFTFLOAT),
      (str:'THREADVAR'     ;special:false;keyword:alllanguagemodes-[m_iso,m_extpas]; to_op:_THREADVAR),
      (str:'WRITEONLY'     ;special:false;keyword:[m_none];                          to_op:_WRITEONLY),
      (str:'BITWISEAND'    ;special:false;keyword:[m_none];                          to_op:_BITWISEAND), { delphi operator name }
      (str:'BITWISEXOR'    ;special:false;keyword:[m_none];                          to_op:_BITWISEXOR), { delphi operator name }
      (str:'DEPRECATED'    ;special:false;keyword:[m_none];                          to_op:_DEPRECATED),
      (str:'DESTRUCTOR'    ;special:false;keyword:alllanguagemodes-[m_iso,m_extpas]; to_op:_DESTRUCTOR),
      (str:'ENUMERATOR'    ;special:false;keyword:[m_none];                          to_op:_OP_ENUMERATOR),
      (str:'IMPLEMENTS'    ;special:false;keyword:[m_none];                          to_op:_IMPLEMENTS),
      (str:'INITIALIZE'    ;special:false;keyword:[m_none];                          to_op:_INITIALIZE),
      (str:'INTERNPROC'    ;special:false;keyword:[m_none];                          to_op:_INTERNPROC),
      (str:'LOGICALAND'    ;special:false;keyword:[m_none];                          to_op:_LOGICALAND), { delphi operator name }
      (str:'LOGICALNOT'    ;special:false;keyword:[m_none];                          to_op:_LOGICALNOT), { delphi operator name }
      (str:'LOGICALXOR'    ;special:false;keyword:[m_none];                          to_op:_LOGICALXOR), { delphi operator name }
      (str:'OLDFPCCALL'    ;special:false;keyword:[m_none];                          to_op:_OLDFPCCALL),
      (str:'OPENSTRING'    ;special:false;keyword:[m_none];                          to_op:_OPENSTRING),
      (str:'RIGHTSHIFT'    ;special:false;keyword:[m_none];                          to_op:_RIGHTSHIFT), { delphi operator name }
      (str:'SPECIALIZE'    ;special:false;keyword:[m_none];                          to_op:_SPECIALIZE),
      (str:'SUSPENDING'    ;special:false;keyword:[m_none];                          to_op:_SUSPENDING),
      (str:'VECTORCALL'    ;special:false;keyword:[m_none];                          to_op:_VECTORCALL),
      (str:'CONSTRUCTOR'   ;special:false;keyword:alllanguagemodes-[m_iso,m_extpas]; to_op:_CONSTRUCTOR),
      (str:'GREATERTHAN'   ;special:false;keyword:[m_none];                          to_op:_GREATERTHAN), { delphi operator name }
      (str:'INTERNCONST'   ;special:false;keyword:[m_none];                          to_op:_INTERNCONST),
      (str:'REINTRODUCE'   ;special:false;keyword:[m_none];                          to_op:_REINTRODUCE),
      (str:'SHORTSTRING'   ;special:false;keyword:[m_none];                          to_op:_SHORTSTRING),
      (str:'WASMFUNCREF'   ;special:false;keyword:[m_none];                          to_op:_WASMFUNCREF),
      (str:'COMPILERPROC'  ;special:false;keyword:[m_none];                          to_op:_COMPILERPROC),
      (str:'EXPERIMENTAL'  ;special:false;keyword:[m_none];                          to_op:_EXPERIMENTAL),
      (str:'FINALIZATION'  ;special:false;keyword:[m_initfinal];                     to_op:_FINALIZATION),
      (str:'MS_ABI_CDECL'  ;special:false;keyword:[m_none];                          to_op:_MS_ABI_CDECL),
      (str:'NOSTACKFRAME'  ;special:false;keyword:[m_none];                          to_op:_NOSTACKFRAME),
      (str:'OBJCCATEGORY'  ;special:false;keyword:[m_objectivec1];                   to_op:_OBJCCATEGORY), { Objective-C category }
      (str:'OBJCPROTOCOL'  ;special:false;keyword:[m_objectivec1];                   to_op:_OBJCPROTOCOL), { Objective-C protocol }
      (str:'WEAKEXTERNAL'  ;special:false;keyword:[m_none];                          to_op:_WEAKEXTERNAL),
      (str:'DISPINTERFACE' ;special:false;keyword:[m_class];                         to_op:_DISPINTERFACE),
      (str:'UNIMPLEMENTED' ;special:false;keyword:[m_none];                          to_op:_UNIMPLEMENTED),
      (str:'IMPLEMENTATION';special:false;keyword:alllanguagemodes-[m_iso,m_extpas]; to_op:_IMPLEMENTATION),
      (str:'INITIALIZATION';special:false;keyword:[m_initfinal];                     to_op:_INITIALIZATION),
      (str:'MS_ABI_DEFAULT';special:false;keyword:[m_none];                          to_op:_MS_ABI_DEFAULT),
      (str:'RESOURCESTRING';special:false;keyword:alllanguagemodes-[m_iso,m_extpas]; to_op:_RESOURCESTRING),
      (str:'SYSV_ABI_CDECL';special:false;keyword:[m_none];                          to_op:_SYSV_ABI_CDECL),
      (str:'LESSTHANOREQUAL';special:false;keyword:[m_none];                         to_op:_LESSTHANOREQUAL), { delphi operator name }
      (str:'SYSV_ABI_DEFAULT';special:false;keyword:[m_none];                        to_op:_SYSV_ABI_DEFAULT),
      (str:'GREATERTHANOREQUAL';special:false;keyword:[m_none];                      to_op:_GREATERTHANOREQUAL) { delphi operator name }
  );


{$ifdef jvm}
  { reserved JVM tokens: keywords, true/false, and "null"; the commented out
    ones are also Pascal keywords in all modes }
  njvmtokens = 40;
  jvmreservedwords: array[1..njvmtokens] of string[12] =
  (
//    'DO',
//    'IF',
//    'FOR',
    'INT',
    'NEW',
    'TRY',
    'BYTE',
//    'CASE',
    'CHAR',
//    'ELSE',
//    'GOTO',
    'LONG',
    'NULL',
    'THIS',
    'VOID',
    'BREAK',
    'CATCH',
    'CLASS',
//    'CONST',
    'FINAL',
    'FLOAT',
    'SHORT',
    'SUPER',
    'THROW',
//    'WHILE',
    'DOUBLE',
    'IMPORT',
    'NATIVE',
    'PUBLIC',
    'RETURN',
    'STATIC',
    'SWITCH',
    'THROWS',
    'BOOLEAN',
    'DEFAULT',
    'EXTENDS',
    'FINALLY',
    'PACKAGE',
    'PRIVATE',
    'ABSTRACT',
    'CONTINUE',
    'STRICTFP',
    'VOLATILE',
//    'INTERFACE',
    'PROTECTED',
    'TRANSIENT',
    'IMPLEMENTS',
    'INSTANCEOF',
    'SYNCHRONIZED'
  );

  jvmtokenlenmin = 3;
  jvmtokenlenmax = 12;

type
  tjvmtokenidxrec = record
    first, last: longint;
  end;
  tjmvtokenarray=array[1..njvmtokens] of string[12];
  pjvmtokenidx= ^tjvmtokenidx;
  tjvmtokenidx=array[jvmtokenlenmin..jvmtokenlenmax] of tjvmtokenidxrec;
{$endif jvm}

var
  tokenidx:ptokenidx;
{$ifdef jvm}
  jvmtokenidx: pjvmtokenidx;
{$endif jvm}


procedure inittokens;
procedure donetokens;
procedure create_tokenidx;


implementation

procedure create_tokenidx;
{ create an index with the first and last token for every possible token
  length, so a search only will be done in that small part }
var
  t : ttoken;
  i : longint;
  c : char;
{$ifdef jvm}
  j : longint;
{$endif jvm}
begin
  fillchar(tokenidx^,sizeof(tokenidx^),0);
  for t:=low(ttoken) to high(ttoken) do
   begin
     if not tokeninfo[t].special then
      begin
        i:=length(tokeninfo[t].str);
        c:=tokeninfo[t].str[1];
        if ord(tokenidx^[i,c].first)=0 then
         tokenidx^[i,c].first:=t;
        tokenidx^[i,c].last:=t;
      end;
   end;
{$ifdef jvm}
  fillchar(jvmtokenidx^,sizeof(jvmtokenidx^),0);
  for j:=low(jvmreservedwords) to high(jvmreservedwords) do
   begin
     i:=length(jvmreservedwords[j]);
     if jvmtokenidx^[i].first=0 then
      jvmtokenidx^[i].first:=j;
     jvmtokenidx^[i].last:=j;
   end;
{$endif jvm}
end;


procedure inittokens;
begin
  if tokenidx = nil then
  begin
    new(tokenidx);
{$ifdef jvm}
    new(jvmtokenidx);
{$endif jvm}
    create_tokenidx;
  end;
end;


procedure donetokens;
begin
  if tokenidx <> nil then
  begin
    dispose(tokenidx);
    tokenidx:=nil;
{$ifdef jvm}
    dispose(jvmtokenidx);
    jvmtokenidx:=nil;
{$endif jvm}
  end;
end;

end.
