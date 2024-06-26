{
Converted from X11/keysym.h and X11/keysymdef.h

Capital letter consts renamed from XK_... to XKc_...
 (since Pascal isn't case-sensitive)

i.e.
C      Pascal
XK_a   XK_a
XK_A   XKc_A
}

{$IFNDEF FPC_DOTTEDUNITS}
Unit keysym;
{$ENDIF FPC_DOTTEDUNITS}

Interface

{* default keysyms *}
{$DEFINE XK_MISCELLANY}
{$DEFINE XK_XKB_KEYS}
{$DEFINE XK_3270}
{$DEFINE XK_LATIN1}
{$DEFINE XK_LATIN2}
{$DEFINE XK_LATIN3}
{$DEFINE XK_LATIN4}
{$DEFINE XK_LATIN8}
{$DEFINE XK_LATIN9}
{$DEFINE XK_KATAKANA}
{$DEFINE XK_ARABIC}
{$DEFINE XK_CYRILLIC}
{$DEFINE XK_GREEK}
{$DEFINE XK_TECHNICAL}
{$DEFINE XK_SPECIAL}
{$DEFINE XK_PUBLISHING}
{$DEFINE XK_APL}
{$DEFINE XK_HEBREW}
{$DEFINE XK_THAI}
{$DEFINE XK_KOREAN}
{$DEFINE XK_ARMENIAN}
{$DEFINE XK_GEORGIAN}
{$DEFINE XK_CAUCASUS}
{$DEFINE XK_VIETNAMESE}
{$DEFINE XK_CURRENCY}
{$DEFINE XK_MATHEMATICAL}
{$DEFINE XK_BRAILLE}
{$DEFINE XK_SINHALA}

Const
  XK_VoidSymbol         = $FFFFFF;      { void symbol }

{$IFDEF XK_MISCELLANY}
{*
 * TTY Functions, cleverly chosen to map to ascii, for convenience of
 * programming, but could have been arbitrary (at the cost of lookup
 * tables in client code.
 *}

  XK_BackSpace          = $FF08;        { back space, back AnsiChar }
  XK_Tab                = $FF09;
  XK_Linefeed           = $FF0A;        { Linefeed, LF }
  XK_Clear              = $FF0B;
  XK_Return             = $FF0D;        { Return, enter }
  XK_Pause              = $FF13;        { Pause, hold }
  XK_Scroll_Lock        = $FF14;
  XK_Sys_Req            = $FF15;
  XK_Escape             = $FF1B;
  XK_Delete             = $FFFF;        { Delete, rubout }



{ International & multi-key character composition }

  XK_Multi_key          = $FF20;  { Multi-key character compose }
  XK_Codeinput          = $FF37;
  XK_SingleCandidate    = $FF3C;
  XK_MultipleCandidate  = $FF3D;
  XK_PreviousCandidate  = $FF3E;

{ Japanese keyboard support }

  XK_Kanji              = $FF21;        { Kanji, Kanji convert }
  XK_Muhenkan           = $FF22;  { Cancel Conversion }
  XK_Henkan_Mode        = $FF23;  { Start/Stop Conversion }
  XK_Henkan             = $FF23;  { Alias for Henkan_Mode }
  XK_Romaji             = $FF24;  { to Romaji }
  XK_Hiragana           = $FF25;  { to Hiragana }
  XK_Katakana           = $FF26;  { to Katakana }
  XK_Hiragana_Katakana  = $FF27;  { Hiragana/Katakana toggle }
  XK_Zenkaku            = $FF28;  { to Zenkaku }
  XK_Hankaku            = $FF29;  { to Hankaku }
  XK_Zenkaku_Hankaku    = $FF2A;  { Zenkaku/Hankaku toggle }
  XK_Touroku            = $FF2B;  { Add to Dictionary }
  XK_Massyo             = $FF2C;  { Delete from Dictionary }
  XK_Kana_Lock          = $FF2D;  { Kana Lock }
  XK_Kana_Shift         = $FF2E;  { Kana Shift }
  XK_Eisu_Shift         = $FF2F;  { Alphanumeric Shift }
  XK_Eisu_toggle        = $FF30;  { Alphanumeric toggle }
  XK_Kanji_Bangou       = $FF37;  { Codeinput }
  XK_Zen_Koho           = $FF3D;  { Multiple/All Candidate(s) }
  XK_Mae_Koho           = $FF3E;  { Previous Candidate }

{ = $FF31 thru = $FF3F are under XK_KOREAN }

{ Cursor control & motion }

  XK_Home               = $FF50;
  XK_Left               = $FF51;        { Move left, left arrow }
  XK_Up                 = $FF52;        { Move up, up arrow }
  XK_Right              = $FF53;        { Move right, right arrow }
  XK_Down               = $FF54;        { Move down, down arrow }
  XK_Prior              = $FF55;        { Prior, previous }
  XK_Page_Up            = $FF55;
  XK_Next               = $FF56;        { Next }
  XK_Page_Down          = $FF56;
  XK_End                = $FF57;        { EOL }
  XK_Begin              = $FF58;        { BOL }


{ Misc Functions }

  XK_Select             = $FF60;        { Select, mark }
  XK_Print              = $FF61;
  XK_Execute            = $FF62;        { Execute, run, do }
  XK_Insert             = $FF63;        { Insert, insert here }
  XK_Undo               = $FF65;        { Undo, oops }
  XK_Redo               = $FF66;        { redo, again }
  XK_Menu               = $FF67;
  XK_Find               = $FF68;        { Find, search }
  XK_Cancel             = $FF69;        { Cancel, stop, abort, exit }
  XK_Help               = $FF6A;        { Help }
  XK_Break              = $FF6B;
  XK_Mode_switch        = $FF7E;        { Character set switch }
  XK_script_switch      = $FF7E;        { Alias for mode_switch }
  XK_Num_Lock           = $FF7F;

{ Keypad Functions, keypad numbers cleverly chosen to map to ascii }

  XK_KP_Space           = $FF80;        { space }
  XK_KP_Tab             = $FF89;
  XK_KP_Enter           = $FF8D;        { enter }
  XK_KP_F1              = $FF91;        { PF1, KP_A, ... }
  XK_KP_F2              = $FF92;
  XK_KP_F3              = $FF93;
  XK_KP_F4              = $FF94;
  XK_KP_Home            = $FF95;
  XK_KP_Left            = $FF96;
  XK_KP_Up              = $FF97;
  XK_KP_Right           = $FF98;
  XK_KP_Down            = $FF99;
  XK_KP_Prior           = $FF9A;
  XK_KP_Page_Up         = $FF9A;
  XK_KP_Next            = $FF9B;
  XK_KP_Page_Down       = $FF9B;
  XK_KP_End             = $FF9C;
  XK_KP_Begin           = $FF9D;
  XK_KP_Insert          = $FF9E;
  XK_KP_Delete          = $FF9F;
  XK_KP_Equal           = $FFBD;        { equals }
  XK_KP_Multiply        = $FFAA;
  XK_KP_Add             = $FFAB;
  XK_KP_Separator       = $FFAC;        { separator, often comma }
  XK_KP_Subtract        = $FFAD;
  XK_KP_Decimal         = $FFAE;
  XK_KP_Divide          = $FFAF;

  XK_KP_0               = $FFB0;
  XK_KP_1               = $FFB1;
  XK_KP_2               = $FFB2;
  XK_KP_3               = $FFB3;
  XK_KP_4               = $FFB4;
  XK_KP_5               = $FFB5;
  XK_KP_6               = $FFB6;
  XK_KP_7               = $FFB7;
  XK_KP_8               = $FFB8;
  XK_KP_9               = $FFB9;



{*
 * Auxilliary Functions; note the duplicate definitions for left and right
 * function keys;  Sun keyboards and a few other manufactures have such
 * function key groups on the left and/or right sides of the keyboard.
 * We've not found a keyboard with more than 35 function keys total.
 *}

  XK_F1                 = $FFBE;
  XK_F2                 = $FFBF;
  XK_F3                 = $FFC0;
  XK_F4                 = $FFC1;
  XK_F5                 = $FFC2;
  XK_F6                 = $FFC3;
  XK_F7                 = $FFC4;
  XK_F8                 = $FFC5;
  XK_F9                 = $FFC6;
  XK_F10                = $FFC7;
  XK_F11                = $FFC8;
  XK_L1                 = $FFC8;
  XK_F12                = $FFC9;
  XK_L2                 = $FFC9;
  XK_F13                = $FFCA;
  XK_L3                 = $FFCA;
  XK_F14                = $FFCB;
  XK_L4                 = $FFCB;
  XK_F15                = $FFCC;
  XK_L5                 = $FFCC;
  XK_F16                = $FFCD;
  XK_L6                 = $FFCD;
  XK_F17                = $FFCE;
  XK_L7                 = $FFCE;
  XK_F18                = $FFCF;
  XK_L8                 = $FFCF;
  XK_F19                = $FFD0;
  XK_L9                 = $FFD0;
  XK_F20                = $FFD1;
  XK_L10                = $FFD1;
  XK_F21                = $FFD2;
  XK_R1                 = $FFD2;
  XK_F22                = $FFD3;
  XK_R2                 = $FFD3;
  XK_F23                = $FFD4;
  XK_R3                 = $FFD4;
  XK_F24                = $FFD5;
  XK_R4                 = $FFD5;
  XK_F25                = $FFD6;
  XK_R5                 = $FFD6;
  XK_F26                = $FFD7;
  XK_R6                 = $FFD7;
  XK_F27                = $FFD8;
  XK_R7                 = $FFD8;
  XK_F28                = $FFD9;
  XK_R8                 = $FFD9;
  XK_F29                = $FFDA;
  XK_R9                 = $FFDA;
  XK_F30                = $FFDB;
  XK_R10                = $FFDB;
  XK_F31                = $FFDC;
  XK_R11                = $FFDC;
  XK_F32                = $FFDD;
  XK_R12                = $FFDD;
  XK_F33                = $FFDE;
  XK_R13                = $FFDE;
  XK_F34                = $FFDF;
  XK_R14                = $FFDF;
  XK_F35                = $FFE0;
  XK_R15                = $FFE0;

{ Modifiers }

  XK_Shift_L            = $FFE1;        { Left shift }
  XK_Shift_R            = $FFE2;        { Right shift }
  XK_Control_L          = $FFE3;        { Left control }
  XK_Control_R          = $FFE4;        { Right control }
  XK_Caps_Lock          = $FFE5;        { Caps lock }
  XK_Shift_Lock         = $FFE6;        { Shift lock }

  XK_Meta_L             = $FFE7;        { Left meta }
  XK_Meta_R             = $FFE8;        { Right meta }
  XK_Alt_L              = $FFE9;        { Left alt }
  XK_Alt_R              = $FFEA;        { Right alt }
  XK_Super_L            = $FFEB;        { Left super }
  XK_Super_R            = $FFEC;        { Right super }
  XK_Hyper_L            = $FFED;        { Left hyper }
  XK_Hyper_R            = $FFEE;        { Right hyper }
{$ENDIF} { XK_MISCELLANY }

{*
 * Keyboard (XKB) Extension function and modifier keys
 * (from Appendix C of "The X Keyboard Extension: Protocol Specification")
 * Byte 3 = $FE
 *}

{$IFDEF XK_XKB_KEYS}
  XK_ISO_Lock                                   = $FE01;
  XK_ISO_Level2_Latch                           = $FE02;
  XK_ISO_Level3_Shift                           = $FE03;
  XK_ISO_Level3_Latch                           = $FE04;
  XK_ISO_Level3_Lock                            = $FE05;
  XK_ISO_Level5_Shift                           = $FE11;
  XK_ISO_Level5_Latch                           = $FE12;
  XK_ISO_Level5_Lock                            = $FE13;
  XK_ISO_Group_Shift                            = $FF7E;        { Alias for mode_switch }
  XK_ISO_Group_Latch                            = $FE06;
  XK_ISO_Group_Lock                             = $FE07;
  XK_ISO_Next_Group                             = $FE08;
  XK_ISO_Next_Group_Lock                                = $FE09;
  XK_ISO_Prev_Group                             = $FE0A;
  XK_ISO_Prev_Group_Lock                                = $FE0B;
  XK_ISO_First_Group                            = $FE0C;
  XK_ISO_First_Group_Lock                               = $FE0D;
  XK_ISO_Last_Group                             = $FE0E;
  XK_ISO_Last_Group_Lock                                = $FE0F;

  XK_ISO_Left_Tab                                       = $FE20;
  XK_ISO_Move_Line_Up                           = $FE21;
  XK_ISO_Move_Line_Down                         = $FE22;
  XK_ISO_Partial_Line_Up                                = $FE23;
  XK_ISO_Partial_Line_Down                      = $FE24;
  XK_ISO_Partial_Space_Left                     = $FE25;
  XK_ISO_Partial_Space_Right                    = $FE26;
  XK_ISO_Set_Margin_Left                                = $FE27;
  XK_ISO_Set_Margin_Right                               = $FE28;
  XK_ISO_Release_Margin_Left                    = $FE29;
  XK_ISO_Release_Margin_Right                   = $FE2A;
  XK_ISO_Release_Both_Margins                   = $FE2B;
  XK_ISO_Fast_Cursor_Left                               = $FE2C;
  XK_ISO_Fast_Cursor_Right                      = $FE2D;
  XK_ISO_Fast_Cursor_Up                         = $FE2E;
  XK_ISO_Fast_Cursor_Down                               = $FE2F;
  XK_ISO_Continuous_Underline                   = $FE30;
  XK_ISO_Discontinuous_Underline                        = $FE31;
  XK_ISO_Emphasize                              = $FE32;
  XK_ISO_Center_Object                          = $FE33;
  XK_ISO_Enter                                  = $FE34;

  XK_dead_grave                                 = $FE50;
  XK_dead_acute                                 = $FE51;
  XK_dead_circumflex                            = $FE52;
  XK_dead_tilde                                 = $FE53;
  XK_dead_perispomeni                           = $FE53;  { alias for dead_tilde }
  XK_dead_macron                                        = $FE54;
  XK_dead_breve                                 = $FE55;
  XK_dead_abovedot                              = $FE56;
  XK_dead_diaeresis                             = $FE57;
  XK_dead_abovering                             = $FE58;
  XK_dead_doubleacute                           = $FE59;
  XK_dead_caron                                 = $FE5A;
  XK_dead_cedilla                                       = $FE5B;
  XK_dead_ogonek                                        = $FE5C;
  XK_dead_iota                                  = $FE5D;
  XK_dead_voiced_sound                          = $FE5E;
  XK_dead_semivoiced_sound                      = $FE5F;
  XK_dead_belowdot                              = $FE60;
  XK_dead_hook                                  = $FE61;
  XK_dead_horn                                  = $FE62;
  XK_dead_stroke                                = $FE63;
  XK_dead_abovecomma                            = $FE64;
  XK_dead_psili                                 = $FE64;  { alias for dead_abovecomma }
  XK_dead_abovereversedcomma                    = $FE65;
  XK_dead_dasia                                 = $FE65;  { alias for dead_abovereversedcomma }
  XK_dead_doublegrave                           = $FE66;
  XK_dead_belowring                             = $FE67;
  XK_dead_belowmacron                           = $FE68;
  XK_dead_belowcircumflex                       = $FE69;
  XK_dead_belowtilde                            = $FE6A;
  XK_dead_belowbreve                            = $FE6B;
  XK_dead_belowdiaeresis                        = $FE6C;
  XK_dead_invertedbreve                         = $FE6D;
  XK_dead_belowcomma                            = $FE6E;
  XK_dead_currency                              = $FE6F;

{ extra dead elements for German T3 layout }
  XK_dead_lowline                               = $FE90;
  XK_dead_aboveverticalline                     = $FE91;
  XK_dead_belowverticalline                     = $FE92;
  XK_dead_longsolidusoverlay                    = $FE93;

{ dead vowels for universal syllable entry }
  XK_dead_a                                     = $FE80;
  XKc_dead_A                                    = $FE81;
  XK_dead_e                                     = $FE82;
  XKc_dead_E                                    = $FE83;
  XK_dead_i                                     = $FE84;
  XKc_dead_I                                    = $FE85;
  XK_dead_o                                     = $FE86;
  XKc_dead_O                                    = $FE87;
  XK_dead_u                                     = $FE88;
  XKc_dead_U                                    = $FE89;
  XK_dead_small_schwa                           = $FE8A;
  XK_dead_capital_schwa                         = $FE8B;

  XK_dead_greek                                 = $FE8C;

  XK_First_Virtual_Screen                               = $FED0;
  XK_Prev_Virtual_Screen                                = $FED1;
  XK_Next_Virtual_Screen                                = $FED2;
  XK_Last_Virtual_Screen                                = $FED4;
  XK_Terminate_Server                           = $FED5;

  XK_AccessX_Enable                             = $FE70;
  XK_AccessX_Feedback_Enable                    = $FE71;
  XK_RepeatKeys_Enable                          = $FE72;
  XK_SlowKeys_Enable                            = $FE73;
  XK_BounceKeys_Enable                          = $FE74;
  XK_StickyKeys_Enable                          = $FE75;
  XK_MouseKeys_Enable                           = $FE76;
  XK_MouseKeys_Accel_Enable                     = $FE77;
  XK_Overlay1_Enable                            = $FE78;
  XK_Overlay2_Enable                            = $FE79;
  XK_AudibleBell_Enable                         = $FE7A;

  XK_Pointer_Left                                       = $FEE0;
  XK_Pointer_Right                              = $FEE1;
  XK_Pointer_Up                                 = $FEE2;
  XK_Pointer_Down                                       = $FEE3;
  XK_Pointer_UpLeft                             = $FEE4;
  XK_Pointer_UpRight                            = $FEE5;
  XK_Pointer_DownLeft                           = $FEE6;
  XK_Pointer_DownRight                          = $FEE7;
  XK_Pointer_Button_Dflt                                = $FEE8;
  XK_Pointer_Button1                            = $FEE9;
  XK_Pointer_Button2                            = $FEEA;
  XK_Pointer_Button3                            = $FEEB;
  XK_Pointer_Button4                            = $FEEC;
  XK_Pointer_Button5                            = $FEED;
  XK_Pointer_DblClick_Dflt                      = $FEEE;
  XK_Pointer_DblClick1                          = $FEEF;
  XK_Pointer_DblClick2                          = $FEF0;
  XK_Pointer_DblClick3                          = $FEF1;
  XK_Pointer_DblClick4                          = $FEF2;
  XK_Pointer_DblClick5                          = $FEF3;
  XK_Pointer_Drag_Dflt                          = $FEF4;
  XK_Pointer_Drag1                              = $FEF5;
  XK_Pointer_Drag2                              = $FEF6;
  XK_Pointer_Drag3                              = $FEF7;
  XK_Pointer_Drag4                              = $FEF8;
  XK_Pointer_Drag5                              = $FEFD;

  XK_Pointer_EnableKeys                         = $FEF9;
  XK_Pointer_Accelerate                         = $FEFA;
  XK_Pointer_DfltBtnNext                                = $FEFB;
  XK_Pointer_DfltBtnPrev                                = $FEFC;

{ Single-Stroke Multiple-Character N-Graph Keysyms For The X Input Method }

  XKll_ch                                       = $FEA0;
  XKcl_Ch                                       = $FEA1;
  XKcc_CH                                       = $FEA2;
  XKll_c_h                                      = $FEA3;
  XKcl_C_h                                      = $FEA4;
  XKcc_C_H                                      = $FEA5;

{$ENDIF}

{*
 * 3270 Terminal Keys
 * Byte 3 = = $FD
 *}

{$IFDEF XK_3270}
  XK_3270_Duplicate      = $FD01;
  XK_3270_FieldMark      = $FD02;
  XK_3270_Right2         = $FD03;
  XK_3270_Left2          = $FD04;
  XK_3270_BackTab        = $FD05;
  XK_3270_EraseEOF       = $FD06;
  XK_3270_EraseInput     = $FD07;
  XK_3270_Reset          = $FD08;
  XK_3270_Quit           = $FD09;
  XK_3270_PA1            = $FD0A;
  XK_3270_PA2            = $FD0B;
  XK_3270_PA3            = $FD0C;
  XK_3270_Test           = $FD0D;
  XK_3270_Attn           = $FD0E;
  XK_3270_CursorBlink    = $FD0F;
  XK_3270_AltCursor      = $FD10;
  XK_3270_KeyClick       = $FD11;
  XK_3270_Jump           = $FD12;
  XK_3270_Ident          = $FD13;
  XK_3270_Rule           = $FD14;
  XK_3270_Copy           = $FD15;
  XK_3270_Play           = $FD16;
  XK_3270_Setup          = $FD17;
  XK_3270_Record         = $FD18;
  XK_3270_ChangeScreen   = $FD19;
  XK_3270_DeleteWord     = $FD1A;
  XK_3270_ExSelect       = $FD1B;
  XK_3270_CursorSelect   = $FD1C;
  XK_3270_PrintScreen    = $FD1D;
  XK_3270_Enter          = $FD1E;
{$ENDIF}

{*
 * Latin 1
 * (ISO/IEC 8859-1 = Unicode U+0020..U+00FF)
 * Byte 3 = 0
 *}
{$IFDEF XK_LATIN1}
  XK_space               = $0020;  { U+0020 SPACE }
  XK_exclam              = $0021;  { U+0021 EXCLAMATION MARK }
  XK_quotedbl            = $0022;  { U+0022 QUOTATION MARK }
  XK_numbersign          = $0023;  { U+0023 NUMBER SIGN }
  XK_dollar              = $0024;  { U+0024 DOLLAR SIGN }
  XK_percent             = $0025;  { U+0025 PERCENT SIGN }
  XK_ampersand           = $0026;  { U+0026 AMPERSAND }
  XK_apostrophe          = $0027;  { U+0027 APOSTROPHE }
  XK_quoteright          = $0027;  { deprecated }
  XK_parenleft           = $0028;  { U+0028 LEFT PARENTHESIS }
  XK_parenright          = $0029;  { U+0029 RIGHT PARENTHESIS }
  XK_asterisk            = $002a;  { U+002A ASTERISK }
  XK_plus                = $002b;  { U+002B PLUS SIGN }
  XK_comma               = $002c;  { U+002C COMMA }
  XK_minus               = $002d;  { U+002D HYPHEN-MINUS }
  XK_period              = $002e;  { U+002E FULL STOP }
  XK_slash               = $002f;  { U+002F SOLIDUS }
  XK_0                   = $0030;  { U+0030 DIGIT ZERO }
  XK_1                   = $0031;  { U+0031 DIGIT ONE }
  XK_2                   = $0032;  { U+0032 DIGIT TWO }
  XK_3                   = $0033;  { U+0033 DIGIT THREE }
  XK_4                   = $0034;  { U+0034 DIGIT FOUR }
  XK_5                   = $0035;  { U+0035 DIGIT FIVE }
  XK_6                   = $0036;  { U+0036 DIGIT SIX }
  XK_7                   = $0037;  { U+0037 DIGIT SEVEN }
  XK_8                   = $0038;  { U+0038 DIGIT EIGHT }
  XK_9                   = $0039;  { U+0039 DIGIT NINE }
  XK_colon               = $003a;  { U+003A COLON }
  XK_semicolon           = $003b;  { U+003B SEMICOLON }
  XK_less                = $003c;  { U+003C LESS-THAN SIGN }
  XK_equal               = $003d;  { U+003D EQUALS SIGN }
  XK_greater             = $003e;  { U+003E GREATER-THAN SIGN }
  XK_question            = $003f;  { U+003F QUESTION MARK }
  XK_at                  = $0040;  { U+0040 COMMERCIAL AT }
  XKc_A                  = $0041;  { U+0041 LATIN CAPITAL LETTER A }
  XKc_B                  = $0042;  { U+0042 LATIN CAPITAL LETTER B }
  XKc_C                  = $0043;  { U+0043 LATIN CAPITAL LETTER C }
  XKc_D                  = $0044;  { U+0044 LATIN CAPITAL LETTER D }
  XKc_E                  = $0045;  { U+0045 LATIN CAPITAL LETTER E }
  XKc_F                  = $0046;  { U+0046 LATIN CAPITAL LETTER F }
  XKc_G                  = $0047;  { U+0047 LATIN CAPITAL LETTER G }
  XKc_H                  = $0048;  { U+0048 LATIN CAPITAL LETTER H }
  XKc_I                  = $0049;  { U+0049 LATIN CAPITAL LETTER I }
  XKc_J                  = $004a;  { U+004A LATIN CAPITAL LETTER J }
  XKc_K                  = $004b;  { U+004B LATIN CAPITAL LETTER K }
  XKc_L                  = $004c;  { U+004C LATIN CAPITAL LETTER L }
  XKc_M                  = $004d;  { U+004D LATIN CAPITAL LETTER M }
  XKc_N                  = $004e;  { U+004E LATIN CAPITAL LETTER N }
  XKc_O                  = $004f;  { U+004F LATIN CAPITAL LETTER O }
  XKc_P                  = $0050;  { U+0050 LATIN CAPITAL LETTER P }
  XKc_Q                  = $0051;  { U+0051 LATIN CAPITAL LETTER Q }
  XKc_R                  = $0052;  { U+0052 LATIN CAPITAL LETTER R }
  XKc_S                  = $0053;  { U+0053 LATIN CAPITAL LETTER S }
  XKc_T                  = $0054;  { U+0054 LATIN CAPITAL LETTER T }
  XKc_U                  = $0055;  { U+0055 LATIN CAPITAL LETTER U }
  XKc_V                  = $0056;  { U+0056 LATIN CAPITAL LETTER V }
  XKc_W                  = $0057;  { U+0057 LATIN CAPITAL LETTER W }
  XKc_X                  = $0058;  { U+0058 LATIN CAPITAL LETTER X }
  XKc_Y                  = $0059;  { U+0059 LATIN CAPITAL LETTER Y }
  XKc_Z                  = $005a;  { U+005A LATIN CAPITAL LETTER Z }
  XK_bracketleft         = $005b;  { U+005B LEFT SQUARE BRACKET }
  XK_backslash           = $005c;  { U+005C REVERSE SOLIDUS }
  XK_bracketright        = $005d;  { U+005D RIGHT SQUARE BRACKET }
  XK_asciicircum         = $005e;  { U+005E CIRCUMFLEX ACCENT }
  XK_underscore          = $005f;  { U+005F LOW LINE }
  XK_grave               = $0060;  { U+0060 GRAVE ACCENT }
  XK_quoteleft           = $0060;  { deprecated }
  XK_a                   = $0061;  { U+0061 LATIN SMALL LETTER A }
  XK_b                   = $0062;  { U+0062 LATIN SMALL LETTER B }
  XK_c                   = $0063;  { U+0063 LATIN SMALL LETTER C }
  XK_d                   = $0064;  { U+0064 LATIN SMALL LETTER D }
  XK_e                   = $0065;  { U+0065 LATIN SMALL LETTER E }
  XK_f                   = $0066;  { U+0066 LATIN SMALL LETTER F }
  XK_g                   = $0067;  { U+0067 LATIN SMALL LETTER G }
  XK_h                   = $0068;  { U+0068 LATIN SMALL LETTER H }
  XK_i                   = $0069;  { U+0069 LATIN SMALL LETTER I }
  XK_j                   = $006a;  { U+006A LATIN SMALL LETTER J }
  XK_k                   = $006b;  { U+006B LATIN SMALL LETTER K }
  XK_l                   = $006c;  { U+006C LATIN SMALL LETTER L }
  XK_m                   = $006d;  { U+006D LATIN SMALL LETTER M }
  XK_n                   = $006e;  { U+006E LATIN SMALL LETTER N }
  XK_o                   = $006f;  { U+006F LATIN SMALL LETTER O }
  XK_p                   = $0070;  { U+0070 LATIN SMALL LETTER P }
  XK_q                   = $0071;  { U+0071 LATIN SMALL LETTER Q }
  XK_r                   = $0072;  { U+0072 LATIN SMALL LETTER R }
  XK_s                   = $0073;  { U+0073 LATIN SMALL LETTER S }
  XK_t                   = $0074;  { U+0074 LATIN SMALL LETTER T }
  XK_u                   = $0075;  { U+0075 LATIN SMALL LETTER U }
  XK_v                   = $0076;  { U+0076 LATIN SMALL LETTER V }
  XK_w                   = $0077;  { U+0077 LATIN SMALL LETTER W }
  XK_x                   = $0078;  { U+0078 LATIN SMALL LETTER X }
  XK_y                   = $0079;  { U+0079 LATIN SMALL LETTER Y }
  XK_z                   = $007a;  { U+007A LATIN SMALL LETTER Z }
  XK_braceleft           = $007b;  { U+007B LEFT CURLY BRACKET }
  XK_bar                 = $007c;  { U+007C VERTICAL LINE }
  XK_braceright          = $007d;  { U+007D RIGHT CURLY BRACKET }
  XK_asciitilde          = $007e;  { U+007E TILDE }

  XK_nobreakspace        = $00a0;  { U+00A0 NO-BREAK SPACE }
  XK_exclamdown          = $00a1;  { U+00A1 INVERTED EXCLAMATION MARK }
  XK_cent                = $00a2;  { U+00A2 CENT SIGN }
  XK_sterling            = $00a3;  { U+00A3 POUND SIGN }
  XK_currency            = $00a4;  { U+00A4 CURRENCY SIGN }
  XK_yen                 = $00a5;  { U+00A5 YEN SIGN }
  XK_brokenbar           = $00a6;  { U+00A6 BROKEN BAR }
  XK_section             = $00a7;  { U+00A7 SECTION SIGN }
  XK_diaeresis           = $00a8;  { U+00A8 DIAERESIS }
  XK_copyright           = $00a9;  { U+00A9 COPYRIGHT SIGN }
  XK_ordfeminine         = $00aa;  { U+00AA FEMININE ORDINAL INDICATOR }
  XK_guillemotleft       = $00ab;  { U+00AB LEFT-POINTING DOUBLE ANGLE QUOTATION MARK }
  XK_notsign             = $00ac;  { U+00AC NOT SIGN }
  XK_hyphen              = $00ad;  { U+00AD SOFT HYPHEN }
  XK_registered          = $00ae;  { U+00AE REGISTERED SIGN }
  XK_macron              = $00af;  { U+00AF MACRON }
  XK_degree              = $00b0;  { U+00B0 DEGREE SIGN }
  XK_plusminus           = $00b1;  { U+00B1 PLUS-MINUS SIGN }
  XK_twosuperior         = $00b2;  { U+00B2 SUPERSCRIPT TWO }
  XK_threesuperior       = $00b3;  { U+00B3 SUPERSCRIPT THREE }
  XK_acute               = $00b4;  { U+00B4 ACUTE ACCENT }
  XK_mu                  = $00b5;  { U+00B5 MICRO SIGN }
  XK_paragraph           = $00b6;  { U+00B6 PILCROW SIGN }
  XK_periodcentered      = $00b7;  { U+00B7 MIDDLE DOT }
  XK_cedilla             = $00b8;  { U+00B8 CEDILLA }
  XK_onesuperior         = $00b9;  { U+00B9 SUPERSCRIPT ONE }
  XK_masculine           = $00ba;  { U+00BA MASCULINE ORDINAL INDICATOR }
  XK_guillemotright      = $00bb;  { U+00BB RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK }
  XK_onequarter          = $00bc;  { U+00BC VULGAR FRACTION ONE QUARTER }
  XK_onehalf             = $00bd;  { U+00BD VULGAR FRACTION ONE HALF }
  XK_threequarters       = $00be;  { U+00BE VULGAR FRACTION THREE QUARTERS }
  XK_questiondown        = $00bf;  { U+00BF INVERTED QUESTION MARK }
  XKc_Agrave             = $00c0;  { U+00C0 LATIN CAPITAL LETTER A WITH GRAVE }
  XKc_Aacute             = $00c1;  { U+00C1 LATIN CAPITAL LETTER A WITH ACUTE }
  XKc_Acircumflex        = $00c2;  { U+00C2 LATIN CAPITAL LETTER A WITH CIRCUMFLEX }
  XKc_Atilde             = $00c3;  { U+00C3 LATIN CAPITAL LETTER A WITH TILDE }
  XKc_Adiaeresis         = $00c4;  { U+00C4 LATIN CAPITAL LETTER A WITH DIAERESIS }
  XKc_Aring              = $00c5;  { U+00C5 LATIN CAPITAL LETTER A WITH RING ABOVE }
  XKc_AE                 = $00c6;  { U+00C6 LATIN CAPITAL LETTER AE }
  XKc_Ccedilla           = $00c7;  { U+00C7 LATIN CAPITAL LETTER C WITH CEDILLA }
  XKc_Egrave             = $00c8;  { U+00C8 LATIN CAPITAL LETTER E WITH GRAVE }
  XKc_Eacute             = $00c9;  { U+00C9 LATIN CAPITAL LETTER E WITH ACUTE }
  XKc_Ecircumflex        = $00ca;  { U+00CA LATIN CAPITAL LETTER E WITH CIRCUMFLEX }
  XKc_Ediaeresis         = $00cb;  { U+00CB LATIN CAPITAL LETTER E WITH DIAERESIS }
  XKc_Igrave             = $00cc;  { U+00CC LATIN CAPITAL LETTER I WITH GRAVE }
  XKc_Iacute             = $00cd;  { U+00CD LATIN CAPITAL LETTER I WITH ACUTE }
  XKc_Icircumflex        = $00ce;  { U+00CE LATIN CAPITAL LETTER I WITH CIRCUMFLEX }
  XKc_Idiaeresis         = $00cf;  { U+00CF LATIN CAPITAL LETTER I WITH DIAERESIS }
  XKc_ETH                = $00d0;  { U+00D0 LATIN CAPITAL LETTER ETH }
  XKc_Ntilde             = $00d1;  { U+00D1 LATIN CAPITAL LETTER N WITH TILDE }
  XKc_Ograve             = $00d2;  { U+00D2 LATIN CAPITAL LETTER O WITH GRAVE }
  XKc_Oacute             = $00d3;  { U+00D3 LATIN CAPITAL LETTER O WITH ACUTE }
  XKc_Ocircumflex        = $00d4;  { U+00D4 LATIN CAPITAL LETTER O WITH CIRCUMFLEX }
  XKc_Otilde             = $00d5;  { U+00D5 LATIN CAPITAL LETTER O WITH TILDE }
  XKc_Odiaeresis         = $00d6;  { U+00D6 LATIN CAPITAL LETTER O WITH DIAERESIS }
  XK_multiply            = $00d7;  { U+00D7 MULTIPLICATION SIGN }
  XKc_Oslash             = $00d8;  { U+00D8 LATIN CAPITAL LETTER O WITH STROKE }
  XKc_Ooblique           = $00d8;  { U+00D8 LATIN CAPITAL LETTER O WITH STROKE }
  XKc_Ugrave             = $00d9;  { U+00D9 LATIN CAPITAL LETTER U WITH GRAVE }
  XKc_Uacute             = $00da;  { U+00DA LATIN CAPITAL LETTER U WITH ACUTE }
  XKc_Ucircumflex        = $00db;  { U+00DB LATIN CAPITAL LETTER U WITH CIRCUMFLEX }
  XKc_Udiaeresis         = $00dc;  { U+00DC LATIN CAPITAL LETTER U WITH DIAERESIS }
  XKc_Yacute             = $00dd;  { U+00DD LATIN CAPITAL LETTER Y WITH ACUTE }
  XKc_THORN              = $00de;  { U+00DE LATIN CAPITAL LETTER THORN }
  XK_ssharp              = $00df;  { U+00DF LATIN SMALL LETTER SHARP S }
  XK_agrave              = $00e0;  { U+00E0 LATIN SMALL LETTER A WITH GRAVE }
  XK_aacute              = $00e1;  { U+00E1 LATIN SMALL LETTER A WITH ACUTE }
  XK_acircumflex         = $00e2;  { U+00E2 LATIN SMALL LETTER A WITH CIRCUMFLEX }
  XK_atilde              = $00e3;  { U+00E3 LATIN SMALL LETTER A WITH TILDE }
  XK_adiaeresis          = $00e4;  { U+00E4 LATIN SMALL LETTER A WITH DIAERESIS }
  XK_aring               = $00e5;  { U+00E5 LATIN SMALL LETTER A WITH RING ABOVE }
  XK_ae                  = $00e6;  { U+00E6 LATIN SMALL LETTER AE }
  XK_ccedilla            = $00e7;  { U+00E7 LATIN SMALL LETTER C WITH CEDILLA }
  XK_egrave              = $00e8;  { U+00E8 LATIN SMALL LETTER E WITH GRAVE }
  XK_eacute              = $00e9;  { U+00E9 LATIN SMALL LETTER E WITH ACUTE }
  XK_ecircumflex         = $00ea;  { U+00EA LATIN SMALL LETTER E WITH CIRCUMFLEX }
  XK_ediaeresis          = $00eb;  { U+00EB LATIN SMALL LETTER E WITH DIAERESIS }
  XK_igrave              = $00ec;  { U+00EC LATIN SMALL LETTER I WITH GRAVE }
  XK_iacute              = $00ed;  { U+00ED LATIN SMALL LETTER I WITH ACUTE }
  XK_icircumflex         = $00ee;  { U+00EE LATIN SMALL LETTER I WITH CIRCUMFLEX }
  XK_idiaeresis          = $00ef;  { U+00EF LATIN SMALL LETTER I WITH DIAERESIS }
  XK_eth                 = $00f0;  { U+00F0 LATIN SMALL LETTER ETH }
  XK_ntilde              = $00f1;  { U+00F1 LATIN SMALL LETTER N WITH TILDE }
  XK_ograve              = $00f2;  { U+00F2 LATIN SMALL LETTER O WITH GRAVE }
  XK_oacute              = $00f3;  { U+00F3 LATIN SMALL LETTER O WITH ACUTE }
  XK_ocircumflex         = $00f4;  { U+00F4 LATIN SMALL LETTER O WITH CIRCUMFLEX }
  XK_otilde              = $00f5;  { U+00F5 LATIN SMALL LETTER O WITH TILDE }
  XK_odiaeresis          = $00f6;  { U+00F6 LATIN SMALL LETTER O WITH DIAERESIS }
  XK_division            = $00f7;  { U+00F7 DIVISION SIGN }
  XK_oslash              = $00f8;  { U+00F8 LATIN SMALL LETTER O WITH STROKE }
  XK_ooblique            = $00f8;  { U+00F8 LATIN SMALL LETTER O WITH STROKE }
  XK_ugrave              = $00f9;  { U+00F9 LATIN SMALL LETTER U WITH GRAVE }
  XK_uacute              = $00fa;  { U+00FA LATIN SMALL LETTER U WITH ACUTE }
  XK_ucircumflex         = $00fb;  { U+00FB LATIN SMALL LETTER U WITH CIRCUMFLEX }
  XK_udiaeresis          = $00fc;  { U+00FC LATIN SMALL LETTER U WITH DIAERESIS }
  XK_yacute              = $00fd;  { U+00FD LATIN SMALL LETTER Y WITH ACUTE }
  XK_thorn               = $00fe;  { U+00FE LATIN SMALL LETTER THORN }
  XK_ydiaeresis          = $00ff;  { U+00FF LATIN SMALL LETTER Y WITH DIAERESIS }
{$ENDIF} { XK_LATIN1 }

{*
 * Latin 2
 * Byte 3 = 1
 *}

{$IFDEF XK_LATIN2}
  XKc_Aogonek            = $01a1;  { U+0104 LATIN CAPITAL LETTER A WITH OGONEK }
  XK_breve               = $01a2;  { U+02D8 BREVE }
  XKc_Lstroke            = $01a3;  { U+0141 LATIN CAPITAL LETTER L WITH STROKE }
  XKc_Lcaron             = $01a5;  { U+013D LATIN CAPITAL LETTER L WITH CARON }
  XKc_Sacute             = $01a6;  { U+015A LATIN CAPITAL LETTER S WITH ACUTE }
  XKc_Scaron             = $01a9;  { U+0160 LATIN CAPITAL LETTER S WITH CARON }
  XKc_Scedilla           = $01aa;  { U+015E LATIN CAPITAL LETTER S WITH CEDILLA }
  XKc_Tcaron             = $01ab;  { U+0164 LATIN CAPITAL LETTER T WITH CARON }
  XKc_Zacute             = $01ac;  { U+0179 LATIN CAPITAL LETTER Z WITH ACUTE }
  XKc_Zcaron             = $01ae;  { U+017D LATIN CAPITAL LETTER Z WITH CARON }
  XKc_Zabovedot          = $01af;  { U+017B LATIN CAPITAL LETTER Z WITH DOT ABOVE }
  XK_aogonek             = $01b1;  { U+0105 LATIN SMALL LETTER A WITH OGONEK }
  XK_ogonek              = $01b2;  { U+02DB OGONEK }
  XK_lstroke             = $01b3;  { U+0142 LATIN SMALL LETTER L WITH STROKE }
  XK_lcaron              = $01b5;  { U+013E LATIN SMALL LETTER L WITH CARON }
  XK_sacute              = $01b6;  { U+015B LATIN SMALL LETTER S WITH ACUTE }
  XK_caron               = $01b7;  { U+02C7 CARON }
  XK_scaron              = $01b9;  { U+0161 LATIN SMALL LETTER S WITH CARON }
  XK_scedilla            = $01ba;  { U+015F LATIN SMALL LETTER S WITH CEDILLA }
  XK_tcaron              = $01bb;  { U+0165 LATIN SMALL LETTER T WITH CARON }
  XK_zacute              = $01bc;  { U+017A LATIN SMALL LETTER Z WITH ACUTE }
  XK_doubleacute         = $01bd;  { U+02DD DOUBLE ACUTE ACCENT }
  XK_zcaron              = $01be;  { U+017E LATIN SMALL LETTER Z WITH CARON }
  XK_zabovedot           = $01bf;  { U+017C LATIN SMALL LETTER Z WITH DOT ABOVE }
  XKc_Racute             = $01c0;  { U+0154 LATIN CAPITAL LETTER R WITH ACUTE }
  XKc_Abreve             = $01c3;  { U+0102 LATIN CAPITAL LETTER A WITH BREVE }
  XKc_Lacute             = $01c5;  { U+0139 LATIN CAPITAL LETTER L WITH ACUTE }
  XKc_Cacute             = $01c6;  { U+0106 LATIN CAPITAL LETTER C WITH ACUTE }
  XKc_Ccaron             = $01c8;  { U+010C LATIN CAPITAL LETTER C WITH CARON }
  XKc_Eogonek            = $01ca;  { U+0118 LATIN CAPITAL LETTER E WITH OGONEK }
  XKc_Ecaron             = $01cc;  { U+011A LATIN CAPITAL LETTER E WITH CARON }
  XKc_Dcaron             = $01cf;  { U+010E LATIN CAPITAL LETTER D WITH CARON }
  XKc_Dstroke            = $01d0;  { U+0110 LATIN CAPITAL LETTER D WITH STROKE }
  XKc_Nacute             = $01d1;  { U+0143 LATIN CAPITAL LETTER N WITH ACUTE }
  XKc_Ncaron             = $01d2;  { U+0147 LATIN CAPITAL LETTER N WITH CARON }
  XKc_Odoubleacute       = $01d5;  { U+0150 LATIN CAPITAL LETTER O WITH DOUBLE ACUTE }
  XKc_Rcaron             = $01d8;  { U+0158 LATIN CAPITAL LETTER R WITH CARON }
  XKc_Uring              = $01d9;  { U+016E LATIN CAPITAL LETTER U WITH RING ABOVE }
  XKc_Udoubleacute       = $01db;  { U+0170 LATIN CAPITAL LETTER U WITH DOUBLE ACUTE }
  XKc_Tcedilla           = $01de;  { U+0162 LATIN CAPITAL LETTER T WITH CEDILLA }
  XK_racute              = $01e0;  { U+0155 LATIN SMALL LETTER R WITH ACUTE }
  XK_abreve              = $01e3;  { U+0103 LATIN SMALL LETTER A WITH BREVE }
  XK_lacute              = $01e5;  { U+013A LATIN SMALL LETTER L WITH ACUTE }
  XK_cacute              = $01e6;  { U+0107 LATIN SMALL LETTER C WITH ACUTE }
  XK_ccaron              = $01e8;  { U+010D LATIN SMALL LETTER C WITH CARON }
  XK_eogonek             = $01ea;  { U+0119 LATIN SMALL LETTER E WITH OGONEK }
  XK_ecaron              = $01ec;  { U+011B LATIN SMALL LETTER E WITH CARON }
  XK_dcaron              = $01ef;  { U+010F LATIN SMALL LETTER D WITH CARON }
  XK_dstroke             = $01f0;  { U+0111 LATIN SMALL LETTER D WITH STROKE }
  XK_nacute              = $01f1;  { U+0144 LATIN SMALL LETTER N WITH ACUTE }
  XK_ncaron              = $01f2;  { U+0148 LATIN SMALL LETTER N WITH CARON }
  XK_odoubleacute        = $01f5;  { U+0151 LATIN SMALL LETTER O WITH DOUBLE ACUTE }
  XK_rcaron              = $01f8;  { U+0159 LATIN SMALL LETTER R WITH CARON }
  XK_uring               = $01f9;  { U+016F LATIN SMALL LETTER U WITH RING ABOVE }
  XK_udoubleacute        = $01fb;  { U+0171 LATIN SMALL LETTER U WITH DOUBLE ACUTE }
  XK_tcedilla            = $01fe;  { U+0163 LATIN SMALL LETTER T WITH CEDILLA }
  XK_abovedot            = $01ff;  { U+02D9 DOT ABOVE }
{$ENDIF} { XK_LATIN2 }

{*
 * Latin 3
 * Byte 3 = 2
 *}

{$IFDEF XK_LATIN3}
  XKc_Hstroke            = $02a1;  { U+0126 LATIN CAPITAL LETTER H WITH STROKE }
  XKc_Hcircumflex        = $02a6;  { U+0124 LATIN CAPITAL LETTER H WITH CIRCUMFLEX }
  XKc_Iabovedot          = $02a9;  { U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE }
  XKc_Gbreve             = $02ab;  { U+011E LATIN CAPITAL LETTER G WITH BREVE }
  XKc_Jcircumflex        = $02ac;  { U+0134 LATIN CAPITAL LETTER J WITH CIRCUMFLEX }
  XK_hstroke             = $02b1;  { U+0127 LATIN SMALL LETTER H WITH STROKE }
  XK_hcircumflex         = $02b6;  { U+0125 LATIN SMALL LETTER H WITH CIRCUMFLEX }
  XK_idotless            = $02b9;  { U+0131 LATIN SMALL LETTER DOTLESS I }
  XK_gbreve              = $02bb;  { U+011F LATIN SMALL LETTER G WITH BREVE }
  XK_jcircumflex         = $02bc;  { U+0135 LATIN SMALL LETTER J WITH CIRCUMFLEX }
  XKc_Cabovedot          = $02c5;  { U+010A LATIN CAPITAL LETTER C WITH DOT ABOVE }
  XKc_Ccircumflex        = $02c6;  { U+0108 LATIN CAPITAL LETTER C WITH CIRCUMFLEX }
  XKc_Gabovedot          = $02d5;  { U+0120 LATIN CAPITAL LETTER G WITH DOT ABOVE }
  XKc_Gcircumflex        = $02d8;  { U+011C LATIN CAPITAL LETTER G WITH CIRCUMFLEX }
  XKc_Ubreve             = $02dd;  { U+016C LATIN CAPITAL LETTER U WITH BREVE }
  XKc_Scircumflex        = $02de;  { U+015C LATIN CAPITAL LETTER S WITH CIRCUMFLEX }
  XK_cabovedot           = $02e5;  { U+010B LATIN SMALL LETTER C WITH DOT ABOVE }
  XK_ccircumflex         = $02e6;  { U+0109 LATIN SMALL LETTER C WITH CIRCUMFLEX }
  XK_gabovedot           = $02f5;  { U+0121 LATIN SMALL LETTER G WITH DOT ABOVE }
  XK_gcircumflex         = $02f8;  { U+011D LATIN SMALL LETTER G WITH CIRCUMFLEX }
  XK_ubreve              = $02fd;  { U+016D LATIN SMALL LETTER U WITH BREVE }
  XK_scircumflex         = $02fe;  { U+015D LATIN SMALL LETTER S WITH CIRCUMFLEX }
{$ENDIF} { XK_LATIN3 }


{*
 * Latin 4
 * Byte 3 = 3
 *}

{$IFDEF XK_LATIN4}
  XK_kra                 = $03a2;  { U+0138 LATIN SMALL LETTER KRA }
  XK_kappa               = $03a2;  { deprecated }
  XKc_Rcedilla           = $03a3;  { U+0156 LATIN CAPITAL LETTER R WITH CEDILLA }
  XKc_Itilde             = $03a5;  { U+0128 LATIN CAPITAL LETTER I WITH TILDE }
  XKc_Lcedilla           = $03a6;  { U+013B LATIN CAPITAL LETTER L WITH CEDILLA }
  XKc_Emacron            = $03aa;  { U+0112 LATIN CAPITAL LETTER E WITH MACRON }
  XKc_Gcedilla           = $03ab;  { U+0122 LATIN CAPITAL LETTER G WITH CEDILLA }
  XKc_Tslash             = $03ac;  { U+0166 LATIN CAPITAL LETTER T WITH STROKE }
  XK_rcedilla            = $03b3;  { U+0157 LATIN SMALL LETTER R WITH CEDILLA }
  XK_itilde              = $03b5;  { U+0129 LATIN SMALL LETTER I WITH TILDE }
  XK_lcedilla            = $03b6;  { U+013C LATIN SMALL LETTER L WITH CEDILLA }
  XK_emacron             = $03ba;  { U+0113 LATIN SMALL LETTER E WITH MACRON }
  XK_gcedilla            = $03bb;  { U+0123 LATIN SMALL LETTER G WITH CEDILLA }
  XK_tslash              = $03bc;  { U+0167 LATIN SMALL LETTER T WITH STROKE }
  XKc_ENG                = $03bd;  { U+014A LATIN CAPITAL LETTER ENG }
  XK_eng                 = $03bf;  { U+014B LATIN SMALL LETTER ENG }
  XKc_Amacron            = $03c0;  { U+0100 LATIN CAPITAL LETTER A WITH MACRON }
  XKc_Iogonek            = $03c7;  { U+012E LATIN CAPITAL LETTER I WITH OGONEK }
  XKc_Eabovedot          = $03cc;  { U+0116 LATIN CAPITAL LETTER E WITH DOT ABOVE }
  XKc_Imacron            = $03cf;  { U+012A LATIN CAPITAL LETTER I WITH MACRON }
  XKc_Ncedilla           = $03d1;  { U+0145 LATIN CAPITAL LETTER N WITH CEDILLA }
  XKc_Omacron            = $03d2;  { U+014C LATIN CAPITAL LETTER O WITH MACRON }
  XKc_Kcedilla           = $03d3;  { U+0136 LATIN CAPITAL LETTER K WITH CEDILLA }
  XKc_Uogonek            = $03d9;  { U+0172 LATIN CAPITAL LETTER U WITH OGONEK }
  XKc_Utilde             = $03dd;  { U+0168 LATIN CAPITAL LETTER U WITH TILDE }
  XKc_Umacron            = $03de;  { U+016A LATIN CAPITAL LETTER U WITH MACRON }
  XK_amacron             = $03e0;  { U+0101 LATIN SMALL LETTER A WITH MACRON }
  XK_iogonek             = $03e7;  { U+012F LATIN SMALL LETTER I WITH OGONEK }
  XK_eabovedot           = $03ec;  { U+0117 LATIN SMALL LETTER E WITH DOT ABOVE }
  XK_imacron             = $03ef;  { U+012B LATIN SMALL LETTER I WITH MACRON }
  XK_ncedilla            = $03f1;  { U+0146 LATIN SMALL LETTER N WITH CEDILLA }
  XK_omacron             = $03f2;  { U+014D LATIN SMALL LETTER O WITH MACRON }
  XK_kcedilla            = $03f3;  { U+0137 LATIN SMALL LETTER K WITH CEDILLA }
  XK_uogonek             = $03f9;  { U+0173 LATIN SMALL LETTER U WITH OGONEK }
  XK_utilde              = $03fd;  { U+0169 LATIN SMALL LETTER U WITH TILDE }
  XK_umacron             = $03fe;  { U+016B LATIN SMALL LETTER U WITH MACRON }
{$ENDIF} { XK_LATIN4 }

{*
 * Latin 8
 *}
{$IFDEF XK_LATIN8}
  XKc_Wcircumflex        = $1000174;  { U+0174 LATIN CAPITAL LETTER W WITH CIRCUMFLEX }
  XK_wcircumflex         = $1000175;  { U+0175 LATIN SMALL LETTER W WITH CIRCUMFLEX }
  XKc_Ycircumflex        = $1000176;  { U+0176 LATIN CAPITAL LETTER Y WITH CIRCUMFLEX }
  XK_ycircumflex         = $1000177;  { U+0177 LATIN SMALL LETTER Y WITH CIRCUMFLEX }
  XKc_Babovedot          = $1001e02;  { U+1E02 LATIN CAPITAL LETTER B WITH DOT ABOVE }
  XK_babovedot           = $1001e03;  { U+1E03 LATIN SMALL LETTER B WITH DOT ABOVE }
  XKc_Dabovedot          = $1001e0a;  { U+1E0A LATIN CAPITAL LETTER D WITH DOT ABOVE }
  XK_dabovedot           = $1001e0b;  { U+1E0B LATIN SMALL LETTER D WITH DOT ABOVE }
  XKc_Fabovedot          = $1001e1e;  { U+1E1E LATIN CAPITAL LETTER F WITH DOT ABOVE }
  XK_fabovedot           = $1001e1f;  { U+1E1F LATIN SMALL LETTER F WITH DOT ABOVE }
  XKc_Mabovedot          = $1001e40;  { U+1E40 LATIN CAPITAL LETTER M WITH DOT ABOVE }
  XK_mabovedot           = $1001e41;  { U+1E41 LATIN SMALL LETTER M WITH DOT ABOVE }
  XKc_Pabovedot          = $1001e56;  { U+1E56 LATIN CAPITAL LETTER P WITH DOT ABOVE }
  XK_pabovedot           = $1001e57;  { U+1E57 LATIN SMALL LETTER P WITH DOT ABOVE }
  XKc_Sabovedot          = $1001e60;  { U+1E60 LATIN CAPITAL LETTER S WITH DOT ABOVE }
  XK_sabovedot           = $1001e61;  { U+1E61 LATIN SMALL LETTER S WITH DOT ABOVE }
  XKc_Tabovedot          = $1001e6a;  { U+1E6A LATIN CAPITAL LETTER T WITH DOT ABOVE }
  XK_tabovedot           = $1001e6b;  { U+1E6B LATIN SMALL LETTER T WITH DOT ABOVE }
  XKc_Wgrave             = $1001e80;  { U+1E80 LATIN CAPITAL LETTER W WITH GRAVE }
  XK_wgrave              = $1001e81;  { U+1E81 LATIN SMALL LETTER W WITH GRAVE }
  XKc_Wacute             = $1001e82;  { U+1E82 LATIN CAPITAL LETTER W WITH ACUTE }
  XK_wacute              = $1001e83;  { U+1E83 LATIN SMALL LETTER W WITH ACUTE }
  XKc_Wdiaeresis         = $1001e84;  { U+1E84 LATIN CAPITAL LETTER W WITH DIAERESIS }
  XK_wdiaeresis          = $1001e85;  { U+1E85 LATIN SMALL LETTER W WITH DIAERESIS }
  XKc_Ygrave             = $1001ef2;  { U+1EF2 LATIN CAPITAL LETTER Y WITH GRAVE }
  XK_ygrave              = $1001ef3;  { U+1EF3 LATIN SMALL LETTER Y WITH GRAVE }
{$ENDIF} { XK_LATIN8 }

{*
 * Latin 9
 * Byte 3 = $13
 *}

{$IFDEF XK_LATIN9}
  XKc_OE                 = $13bc;  { U+0152 LATIN CAPITAL LIGATURE OE }
  XK_oe                  = $13bd;  { U+0153 LATIN SMALL LIGATURE OE }
  XKc_Ydiaeresis         = $13be;  { U+0178 LATIN CAPITAL LETTER Y WITH DIAERESIS }
{$ENDIF} { XK_LATIN9 }

{*
 * Katakana
 * Byte 3 = 4
 *}

{$IFDEF XK_KATAKANA}
  XK_overline            = $047e;  { U+203E OVERLINE }
  XK_kana_fullstop       = $04a1;  { U+3002 IDEOGRAPHIC FULL STOP }
  XK_kana_openingbracket = $04a2;  { U+300C LEFT CORNER BRACKET }
  XK_kana_closingbracket = $04a3;  { U+300D RIGHT CORNER BRACKET }
  XK_kana_comma          = $04a4;  { U+3001 IDEOGRAPHIC COMMA }
  XK_kana_conjunctive    = $04a5;  { U+30FB KATAKANA MIDDLE DOT }
  XK_kana_middledot      = $04a5;  { deprecated }
  XKc_kana_WO            = $04a6;  { U+30F2 KATAKANA LETTER WO }
  XK_kana_a              = $04a7;  { U+30A1 KATAKANA LETTER SMALL A }
  XK_kana_i              = $04a8;  { U+30A3 KATAKANA LETTER SMALL I }
  XK_kana_u              = $04a9;  { U+30A5 KATAKANA LETTER SMALL U }
  XK_kana_e              = $04aa;  { U+30A7 KATAKANA LETTER SMALL E }
  XK_kana_o              = $04ab;  { U+30A9 KATAKANA LETTER SMALL O }
  XK_kana_ya             = $04ac;  { U+30E3 KATAKANA LETTER SMALL YA }
  XK_kana_yu             = $04ad;  { U+30E5 KATAKANA LETTER SMALL YU }
  XK_kana_yo             = $04ae;  { U+30E7 KATAKANA LETTER SMALL YO }
  XK_kana_tsu            = $04af;  { U+30C3 KATAKANA LETTER SMALL TU }
  XK_kana_tu             = $04af;  { deprecated }
  XK_prolongedsound      = $04b0;  { U+30FC KATAKANA-HIRAGANA PROLONGED SOUND MARK }
  XKc_kana_A             = $04b1;  { U+30A2 KATAKANA LETTER A }
  XKc_kana_I             = $04b2;  { U+30A4 KATAKANA LETTER I }
  XKc_kana_U             = $04b3;  { U+30A6 KATAKANA LETTER U }
  XKc_kana_E             = $04b4;  { U+30A8 KATAKANA LETTER E }
  XKc_kana_O             = $04b5;  { U+30AA KATAKANA LETTER O }
  XKc_kana_KA            = $04b6;  { U+30AB KATAKANA LETTER KA }
  XKc_kana_KI            = $04b7;  { U+30AD KATAKANA LETTER KI }
  XKc_kana_KU            = $04b8;  { U+30AF KATAKANA LETTER KU }
  XKc_kana_KE            = $04b9;  { U+30B1 KATAKANA LETTER KE }
  XKc_kana_KO            = $04ba;  { U+30B3 KATAKANA LETTER KO }
  XKc_kana_SA            = $04bb;  { U+30B5 KATAKANA LETTER SA }
  XKc_kana_SHI           = $04bc;  { U+30B7 KATAKANA LETTER SI }
  XKc_kana_SU            = $04bd;  { U+30B9 KATAKANA LETTER SU }
  XKc_kana_SE            = $04be;  { U+30BB KATAKANA LETTER SE }
  XKc_kana_SO            = $04bf;  { U+30BD KATAKANA LETTER SO }
  XKc_kana_TA            = $04c0;  { U+30BF KATAKANA LETTER TA }
  XKc_kana_CHI           = $04c1;  { U+30C1 KATAKANA LETTER TI }
  XKc_kana_TI            = $04c1;  { deprecated }
  XKc_kana_TSU           = $04c2;  { U+30C4 KATAKANA LETTER TU }
  XKc_kana_TU            = $04c2;  { deprecated }
  XKc_kana_TE            = $04c3;  { U+30C6 KATAKANA LETTER TE }
  XKc_kana_TO            = $04c4;  { U+30C8 KATAKANA LETTER TO }
  XKc_kana_NA            = $04c5;  { U+30CA KATAKANA LETTER NA }
  XKc_kana_NI            = $04c6;  { U+30CB KATAKANA LETTER NI }
  XKc_kana_NU            = $04c7;  { U+30CC KATAKANA LETTER NU }
  XKc_kana_NE            = $04c8;  { U+30CD KATAKANA LETTER NE }
  XKc_kana_NO            = $04c9;  { U+30CE KATAKANA LETTER NO }
  XKc_kana_HA            = $04ca;  { U+30CF KATAKANA LETTER HA }
  XKc_kana_HI            = $04cb;  { U+30D2 KATAKANA LETTER HI }
  XKc_kana_FU            = $04cc;  { U+30D5 KATAKANA LETTER HU }
  XKc_kana_HU            = $04cc;  { deprecated }
  XKc_kana_HE            = $04cd;  { U+30D8 KATAKANA LETTER HE }
  XKc_kana_HO            = $04ce;  { U+30DB KATAKANA LETTER HO }
  XKc_kana_MA            = $04cf;  { U+30DE KATAKANA LETTER MA }
  XKc_kana_MI            = $04d0;  { U+30DF KATAKANA LETTER MI }
  XKc_kana_MU            = $04d1;  { U+30E0 KATAKANA LETTER MU }
  XKc_kana_ME            = $04d2;  { U+30E1 KATAKANA LETTER ME }
  XKc_kana_MO            = $04d3;  { U+30E2 KATAKANA LETTER MO }
  XKc_kana_YA            = $04d4;  { U+30E4 KATAKANA LETTER YA }
  XKc_kana_YU            = $04d5;  { U+30E6 KATAKANA LETTER YU }
  XKc_kana_YO            = $04d6;  { U+30E8 KATAKANA LETTER YO }
  XKc_kana_RA            = $04d7;  { U+30E9 KATAKANA LETTER RA }
  XKc_kana_RI            = $04d8;  { U+30EA KATAKANA LETTER RI }
  XKc_kana_RU            = $04d9;  { U+30EB KATAKANA LETTER RU }
  XKc_kana_RE            = $04da;  { U+30EC KATAKANA LETTER RE }
  XKc_kana_RO            = $04db;  { U+30ED KATAKANA LETTER RO }
  XKc_kana_WA            = $04dc;  { U+30EF KATAKANA LETTER WA }
  XKc_kana_N             = $04dd;  { U+30F3 KATAKANA LETTER N }
  XK_voicedsound         = $04de;  { U+309B KATAKANA-HIRAGANA VOICED SOUND MARK }
  XK_semivoicedsound     = $04df;  { U+309C KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK }
  XK_kana_switch         = $ff7e;  { Alias for mode_switch }
{$ENDIF} { XK_KATAKANA }

{*
 *  Arabic
 *  Byte 3 = 5
 *}

{$IFDEF XK_ARABIC}
  XK_Farsi_0                 = $10006f0;  { U+06F0 EXTENDED ARABIC-INDIC DIGIT ZERO }
  XK_Farsi_1                 = $10006f1;  { U+06F1 EXTENDED ARABIC-INDIC DIGIT ONE }
  XK_Farsi_2                 = $10006f2;  { U+06F2 EXTENDED ARABIC-INDIC DIGIT TWO }
  XK_Farsi_3                 = $10006f3;  { U+06F3 EXTENDED ARABIC-INDIC DIGIT THREE }
  XK_Farsi_4                 = $10006f4;  { U+06F4 EXTENDED ARABIC-INDIC DIGIT FOUR }
  XK_Farsi_5                 = $10006f5;  { U+06F5 EXTENDED ARABIC-INDIC DIGIT FIVE }
  XK_Farsi_6                 = $10006f6;  { U+06F6 EXTENDED ARABIC-INDIC DIGIT SIX }
  XK_Farsi_7                 = $10006f7;  { U+06F7 EXTENDED ARABIC-INDIC DIGIT SEVEN }
  XK_Farsi_8                 = $10006f8;  { U+06F8 EXTENDED ARABIC-INDIC DIGIT EIGHT }
  XK_Farsi_9                 = $10006f9;  { U+06F9 EXTENDED ARABIC-INDIC DIGIT NINE }
  XK_Arabic_percent          = $100066a;  { U+066A ARABIC PERCENT SIGN }
  XK_Arabic_superscript_alef = $1000670;  { U+0670 ARABIC LETTER SUPERSCRIPT ALEF }
  XK_Arabic_tteh             = $1000679;  { U+0679 ARABIC LETTER TTEH }
  XK_Arabic_peh              = $100067e;  { U+067E ARABIC LETTER PEH }
  XK_Arabic_tcheh            = $1000686;  { U+0686 ARABIC LETTER TCHEH }
  XK_Arabic_ddal             = $1000688;  { U+0688 ARABIC LETTER DDAL }
  XK_Arabic_rreh             = $1000691;  { U+0691 ARABIC LETTER RREH }
  XK_Arabic_comma            = $05ac;     { U+060C ARABIC COMMA }
  XK_Arabic_fullstop         = $10006d4;  { U+06D4 ARABIC FULL STOP }
  XK_Arabic_0                = $1000660;  { U+0660 ARABIC-INDIC DIGIT ZERO }
  XK_Arabic_1                = $1000661;  { U+0661 ARABIC-INDIC DIGIT ONE }
  XK_Arabic_2                = $1000662;  { U+0662 ARABIC-INDIC DIGIT TWO }
  XK_Arabic_3                = $1000663;  { U+0663 ARABIC-INDIC DIGIT THREE }
  XK_Arabic_4                = $1000664;  { U+0664 ARABIC-INDIC DIGIT FOUR }
  XK_Arabic_5                = $1000665;  { U+0665 ARABIC-INDIC DIGIT FIVE }
  XK_Arabic_6                = $1000666;  { U+0666 ARABIC-INDIC DIGIT SIX }
  XK_Arabic_7                = $1000667;  { U+0667 ARABIC-INDIC DIGIT SEVEN }
  XK_Arabic_8                = $1000668;  { U+0668 ARABIC-INDIC DIGIT EIGHT }
  XK_Arabic_9                = $1000669;  { U+0669 ARABIC-INDIC DIGIT NINE }
  XK_Arabic_semicolon        = $05bb;     { U+061B ARABIC SEMICOLON }
  XK_Arabic_question_mark    = $05bf;     { U+061F ARABIC QUESTION MARK }
  XK_Arabic_hamza            = $05c1;     { U+0621 ARABIC LETTER HAMZA }
  XK_Arabic_maddaonalef      = $05c2;     { U+0622 ARABIC LETTER ALEF WITH MADDA ABOVE }
  XK_Arabic_hamzaonalef      = $05c3;     { U+0623 ARABIC LETTER ALEF WITH HAMZA ABOVE }
  XK_Arabic_hamzaonwaw       = $05c4;     { U+0624 ARABIC LETTER WAW WITH HAMZA ABOVE }
  XK_Arabic_hamzaunderalef   = $05c5;     { U+0625 ARABIC LETTER ALEF WITH HAMZA BELOW }
  XK_Arabic_hamzaonyeh       = $05c6;     { U+0626 ARABIC LETTER YEH WITH HAMZA ABOVE }
  XK_Arabic_alef             = $05c7;     { U+0627 ARABIC LETTER ALEF }
  XK_Arabic_beh              = $05c8;     { U+0628 ARABIC LETTER BEH }
  XK_Arabic_tehmarbuta       = $05c9;     { U+0629 ARABIC LETTER TEH MARBUTA }
  XK_Arabic_teh              = $05ca;     { U+062A ARABIC LETTER TEH }
  XK_Arabic_theh             = $05cb;     { U+062B ARABIC LETTER THEH }
  XK_Arabic_jeem             = $05cc;     { U+062C ARABIC LETTER JEEM }
  XK_Arabic_hah              = $05cd;     { U+062D ARABIC LETTER HAH }
  XK_Arabic_khah             = $05ce;     { U+062E ARABIC LETTER KHAH }
  XK_Arabic_dal              = $05cf;     { U+062F ARABIC LETTER DAL }
  XK_Arabic_thal             = $05d0;     { U+0630 ARABIC LETTER THAL }
  XK_Arabic_ra               = $05d1;     { U+0631 ARABIC LETTER REH }
  XK_Arabic_zain             = $05d2;     { U+0632 ARABIC LETTER ZAIN }
  XK_Arabic_seen             = $05d3;     { U+0633 ARABIC LETTER SEEN }
  XK_Arabic_sheen            = $05d4;     { U+0634 ARABIC LETTER SHEEN }
  XK_Arabic_sad              = $05d5;     { U+0635 ARABIC LETTER SAD }
  XK_Arabic_dad              = $05d6;     { U+0636 ARABIC LETTER DAD }
  XK_Arabic_tah              = $05d7;     { U+0637 ARABIC LETTER TAH }
  XK_Arabic_zah              = $05d8;     { U+0638 ARABIC LETTER ZAH }
  XK_Arabic_ain              = $05d9;     { U+0639 ARABIC LETTER AIN }
  XK_Arabic_ghain            = $05da;     { U+063A ARABIC LETTER GHAIN }
  XK_Arabic_tatweel          = $05e0;     { U+0640 ARABIC TATWEEL }
  XK_Arabic_feh              = $05e1;     { U+0641 ARABIC LETTER FEH }
  XK_Arabic_qaf              = $05e2;     { U+0642 ARABIC LETTER QAF }
  XK_Arabic_kaf              = $05e3;     { U+0643 ARABIC LETTER KAF }
  XK_Arabic_lam              = $05e4;     { U+0644 ARABIC LETTER LAM }
  XK_Arabic_meem             = $05e5;     { U+0645 ARABIC LETTER MEEM }
  XK_Arabic_noon             = $05e6;     { U+0646 ARABIC LETTER NOON }
  XK_Arabic_ha               = $05e7;     { U+0647 ARABIC LETTER HEH }
  XK_Arabic_heh              = $05e7;     { deprecated }
  XK_Arabic_waw              = $05e8;     { U+0648 ARABIC LETTER WAW }
  XK_Arabic_alefmaksura      = $05e9;     { U+0649 ARABIC LETTER ALEF MAKSURA }
  XK_Arabic_yeh              = $05ea;     { U+064A ARABIC LETTER YEH }
  XK_Arabic_fathatan         = $05eb;     { U+064B ARABIC FATHATAN }
  XK_Arabic_dammatan         = $05ec;     { U+064C ARABIC DAMMATAN }
  XK_Arabic_kasratan         = $05ed;     { U+064D ARABIC KASRATAN }
  XK_Arabic_fatha            = $05ee;     { U+064E ARABIC FATHA }
  XK_Arabic_damma            = $05ef;     { U+064F ARABIC DAMMA }
  XK_Arabic_kasra            = $05f0;     { U+0650 ARABIC KASRA }
  XK_Arabic_shadda           = $05f1;     { U+0651 ARABIC SHADDA }
  XK_Arabic_sukun            = $05f2;     { U+0652 ARABIC SUKUN }
  XK_Arabic_madda_above      = $1000653;  { U+0653 ARABIC MADDAH ABOVE }
  XK_Arabic_hamza_above      = $1000654;  { U+0654 ARABIC HAMZA ABOVE }
  XK_Arabic_hamza_below      = $1000655;  { U+0655 ARABIC HAMZA BELOW }
  XK_Arabic_jeh              = $1000698;  { U+0698 ARABIC LETTER JEH }
  XK_Arabic_veh              = $10006a4;  { U+06A4 ARABIC LETTER VEH }
  XK_Arabic_keheh            = $10006a9;  { U+06A9 ARABIC LETTER KEHEH }
  XK_Arabic_gaf              = $10006af;  { U+06AF ARABIC LETTER GAF }
  XK_Arabic_noon_ghunna      = $10006ba;  { U+06BA ARABIC LETTER NOON GHUNNA }
  XK_Arabic_heh_doachashmee  = $10006be;  { U+06BE ARABIC LETTER HEH DOACHASHMEE }
  XK_Farsi_yeh               = $10006cc;  { U+06CC ARABIC LETTER FARSI YEH }
  XK_Arabic_farsi_yeh        = $10006cc;  { U+06CC ARABIC LETTER FARSI YEH }
  XK_Arabic_yeh_baree        = $10006d2;  { U+06D2 ARABIC LETTER YEH BARREE }
  XK_Arabic_heh_goal         = $10006c1;  { U+06C1 ARABIC LETTER HEH GOAL }
  XK_Arabic_switch           = $ff7e;     { Alias for mode_switch }
{$ENDIF} { XK_ARABIC }

{*
 * Cyrillic
 * Byte 3 = 6
 *}
{$IFDEF XK_CYRILLIC}
  XKc_Cyrillic_GHE_bar          = $1000492;  { U+0492 CYRILLIC CAPITAL LETTER GHE WITH STROKE }
  XK_Cyrillic_ghe_bar           = $1000493;  { U+0493 CYRILLIC SMALL LETTER GHE WITH STROKE }
  XKc_Cyrillic_ZHE_descender    = $1000496;  { U+0496 CYRILLIC CAPITAL LETTER ZHE WITH DESCENDER }
  XK_Cyrillic_zhe_descender     = $1000497;  { U+0497 CYRILLIC SMALL LETTER ZHE WITH DESCENDER }
  XKc_Cyrillic_KA_descender     = $100049a;  { U+049A CYRILLIC CAPITAL LETTER KA WITH DESCENDER }
  XK_Cyrillic_ka_descender      = $100049b;  { U+049B CYRILLIC SMALL LETTER KA WITH DESCENDER }
  XKc_Cyrillic_KA_vertstroke    = $100049c;  { U+049C CYRILLIC CAPITAL LETTER KA WITH VERTICAL STROKE }
  XK_Cyrillic_ka_vertstroke     = $100049d;  { U+049D CYRILLIC SMALL LETTER KA WITH VERTICAL STROKE }
  XKc_Cyrillic_EN_descender     = $10004a2;  { U+04A2 CYRILLIC CAPITAL LETTER EN WITH DESCENDER }
  XK_Cyrillic_en_descender      = $10004a3;  { U+04A3 CYRILLIC SMALL LETTER EN WITH DESCENDER }
  XKc_Cyrillic_U_straight       = $10004ae;  { U+04AE CYRILLIC CAPITAL LETTER STRAIGHT U }
  XK_Cyrillic_u_straight        = $10004af;  { U+04AF CYRILLIC SMALL LETTER STRAIGHT U }
  XKc_Cyrillic_U_straight_bar   = $10004b0;  { U+04B0 CYRILLIC CAPITAL LETTER STRAIGHT U WITH STROKE }
  XK_Cyrillic_u_straight_bar    = $10004b1;  { U+04B1 CYRILLIC SMALL LETTER STRAIGHT U WITH STROKE }
  XKc_Cyrillic_HA_descender     = $10004b2;  { U+04B2 CYRILLIC CAPITAL LETTER HA WITH DESCENDER }
  XK_Cyrillic_ha_descender      = $10004b3;  { U+04B3 CYRILLIC SMALL LETTER HA WITH DESCENDER }
  XKc_Cyrillic_CHE_descender    = $10004b6;  { U+04B6 CYRILLIC CAPITAL LETTER CHE WITH DESCENDER }
  XK_Cyrillic_che_descender     = $10004b7;  { U+04B7 CYRILLIC SMALL LETTER CHE WITH DESCENDER }
  XKc_Cyrillic_CHE_vertstroke   = $10004b8;  { U+04B8 CYRILLIC CAPITAL LETTER CHE WITH VERTICAL STROKE }
  XK_Cyrillic_che_vertstroke    = $10004b9;  { U+04B9 CYRILLIC SMALL LETTER CHE WITH VERTICAL STROKE }
  XKc_Cyrillic_SHHA             = $10004ba;  { U+04BA CYRILLIC CAPITAL LETTER SHHA }
  XK_Cyrillic_shha              = $10004bb;  { U+04BB CYRILLIC SMALL LETTER SHHA }

  XKc_Cyrillic_SCHWA            = $10004d8;  { U+04D8 CYRILLIC CAPITAL LETTER SCHWA }
  XK_Cyrillic_schwa             = $10004d9;  { U+04D9 CYRILLIC SMALL LETTER SCHWA }
  XKc_Cyrillic_I_macron         = $10004e2;  { U+04E2 CYRILLIC CAPITAL LETTER I WITH MACRON }
  XK_Cyrillic_i_macron          = $10004e3;  { U+04E3 CYRILLIC SMALL LETTER I WITH MACRON }
  XKc_Cyrillic_O_bar            = $10004e8;  { U+04E8 CYRILLIC CAPITAL LETTER BARRED O }
  XK_Cyrillic_o_bar             = $10004e9;  { U+04E9 CYRILLIC SMALL LETTER BARRED O }
  XKc_Cyrillic_U_macron         = $10004ee;  { U+04EE CYRILLIC CAPITAL LETTER U WITH MACRON }
  XK_Cyrillic_u_macron          = $10004ef;  { U+04EF CYRILLIC SMALL LETTER U WITH MACRON }

  XK_Serbian_dje                = $06a1;     { U+0452 CYRILLIC SMALL LETTER DJE }
  XK_Macedonia_gje              = $06a2;     { U+0453 CYRILLIC SMALL LETTER GJE }
  XK_Cyrillic_io                = $06a3;     { U+0451 CYRILLIC SMALL LETTER IO }
  XK_Ukrainian_ie               = $06a4;     { U+0454 CYRILLIC SMALL LETTER UKRAINIAN IE }
  XK_Ukranian_je                = $06a4;     { deprecated }
  XK_Macedonia_dse              = $06a5;     { U+0455 CYRILLIC SMALL LETTER DZE }
  XK_Ukrainian_i                = $06a6;     { U+0456 CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I }
  XK_Ukranian_i                 = $06a6;     { deprecated }
  XK_Ukrainian_yi               = $06a7;     { U+0457 CYRILLIC SMALL LETTER YI }
  XK_Ukranian_yi                = $06a7;     { deprecated }
  XK_Cyrillic_je                = $06a8;     { U+0458 CYRILLIC SMALL LETTER JE }
  XK_Serbian_je                 = $06a8;     { deprecated }
  XK_Cyrillic_lje               = $06a9;     { U+0459 CYRILLIC SMALL LETTER LJE }
  XK_Serbian_lje                = $06a9;     { deprecated }
  XK_Cyrillic_nje               = $06aa;     { U+045A CYRILLIC SMALL LETTER NJE }
  XK_Serbian_nje                = $06aa;     { deprecated }
  XK_Serbian_tshe               = $06ab;     { U+045B CYRILLIC SMALL LETTER TSHE }
  XK_Macedonia_kje              = $06ac;     { U+045C CYRILLIC SMALL LETTER KJE }
  XK_Ukrainian_ghe_with_upturn  = $06ad;     { U+0491 CYRILLIC SMALL LETTER GHE WITH UPTURN }
  XK_Byelorussian_shortu        = $06ae;     { U+045E CYRILLIC SMALL LETTER SHORT U }
  XK_Cyrillic_dzhe              = $06af;     { U+045F CYRILLIC SMALL LETTER DZHE }
  XK_Serbian_dze                = $06af;     { deprecated }
  XK_numerosign                 = $06b0;     { U+2116 NUMERO SIGN }
  XKc_Serbian_DJE               = $06b1;     { U+0402 CYRILLIC CAPITAL LETTER DJE }
  XKc_Macedonia_GJE             = $06b2;     { U+0403 CYRILLIC CAPITAL LETTER GJE }
  XKc_Cyrillic_IO               = $06b3;     { U+0401 CYRILLIC CAPITAL LETTER IO }
  XKc_Ukrainian_IE              = $06b4;     { U+0404 CYRILLIC CAPITAL LETTER UKRAINIAN IE }
  XKc_Ukranian_JE               = $06b4;     { deprecated }
  XKc_Macedonia_DSE             = $06b5;     { U+0405 CYRILLIC CAPITAL LETTER DZE }
  XKc_Ukrainian_I               = $06b6;     { U+0406 CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I }
  XKc_Ukranian_I                = $06b6;     { deprecated }
  XKc_Ukrainian_YI              = $06b7;     { U+0407 CYRILLIC CAPITAL LETTER YI }
  XKc_Ukranian_YI               = $06b7;     { deprecated }
  XKc_Cyrillic_JE               = $06b8;     { U+0408 CYRILLIC CAPITAL LETTER JE }
  XKc_Serbian_JE                = $06b8;     { deprecated }
  XKc_Cyrillic_LJE              = $06b9;     { U+0409 CYRILLIC CAPITAL LETTER LJE }
  XKc_Serbian_LJE               = $06b9;     { deprecated }
  XKc_Cyrillic_NJE              = $06ba;     { U+040A CYRILLIC CAPITAL LETTER NJE }
  XKc_Serbian_NJE               = $06ba;     { deprecated }
  XKc_Serbian_TSHE              = $06bb;     { U+040B CYRILLIC CAPITAL LETTER TSHE }
  XKc_Macedonia_KJE             = $06bc;     { U+040C CYRILLIC CAPITAL LETTER KJE }
  XKc_Ukrainian_GHE_WITH_UPTURN = $06bd;     { U+0490 CYRILLIC CAPITAL LETTER GHE WITH UPTURN }
  XKc_Byelorussian_SHORTU       = $06be;     { U+040E CYRILLIC CAPITAL LETTER SHORT U }
  XKc_Cyrillic_DZHE             = $06bf;     { U+040F CYRILLIC CAPITAL LETTER DZHE }
  XKc_Serbian_DZE               = $06bf;     { deprecated }
  XK_Cyrillic_yu                = $06c0;     { U+044E CYRILLIC SMALL LETTER YU }
  XK_Cyrillic_a                 = $06c1;     { U+0430 CYRILLIC SMALL LETTER A }
  XK_Cyrillic_be                = $06c2;     { U+0431 CYRILLIC SMALL LETTER BE }
  XK_Cyrillic_tse               = $06c3;     { U+0446 CYRILLIC SMALL LETTER TSE }
  XK_Cyrillic_de                = $06c4;     { U+0434 CYRILLIC SMALL LETTER DE }
  XK_Cyrillic_ie                = $06c5;     { U+0435 CYRILLIC SMALL LETTER IE }
  XK_Cyrillic_ef                = $06c6;     { U+0444 CYRILLIC SMALL LETTER EF }
  XK_Cyrillic_ghe               = $06c7;     { U+0433 CYRILLIC SMALL LETTER GHE }
  XK_Cyrillic_ha                = $06c8;     { U+0445 CYRILLIC SMALL LETTER HA }
  XK_Cyrillic_i                 = $06c9;     { U+0438 CYRILLIC SMALL LETTER I }
  XK_Cyrillic_shorti            = $06ca;     { U+0439 CYRILLIC SMALL LETTER SHORT I }
  XK_Cyrillic_ka                = $06cb;     { U+043A CYRILLIC SMALL LETTER KA }
  XK_Cyrillic_el                = $06cc;     { U+043B CYRILLIC SMALL LETTER EL }
  XK_Cyrillic_em                = $06cd;     { U+043C CYRILLIC SMALL LETTER EM }
  XK_Cyrillic_en                = $06ce;     { U+043D CYRILLIC SMALL LETTER EN }
  XK_Cyrillic_o                 = $06cf;     { U+043E CYRILLIC SMALL LETTER O }
  XK_Cyrillic_pe                = $06d0;     { U+043F CYRILLIC SMALL LETTER PE }
  XK_Cyrillic_ya                = $06d1;     { U+044F CYRILLIC SMALL LETTER YA }
  XK_Cyrillic_er                = $06d2;     { U+0440 CYRILLIC SMALL LETTER ER }
  XK_Cyrillic_es                = $06d3;     { U+0441 CYRILLIC SMALL LETTER ES }
  XK_Cyrillic_te                = $06d4;     { U+0442 CYRILLIC SMALL LETTER TE }
  XK_Cyrillic_u                 = $06d5;     { U+0443 CYRILLIC SMALL LETTER U }
  XK_Cyrillic_zhe               = $06d6;     { U+0436 CYRILLIC SMALL LETTER ZHE }
  XK_Cyrillic_ve                = $06d7;     { U+0432 CYRILLIC SMALL LETTER VE }
  XK_Cyrillic_softsign          = $06d8;     { U+044C CYRILLIC SMALL LETTER SOFT SIGN }
  XK_Cyrillic_yeru              = $06d9;     { U+044B CYRILLIC SMALL LETTER YERU }
  XK_Cyrillic_ze                = $06da;     { U+0437 CYRILLIC SMALL LETTER ZE }
  XK_Cyrillic_sha               = $06db;     { U+0448 CYRILLIC SMALL LETTER SHA }
  XK_Cyrillic_e                 = $06dc;     { U+044D CYRILLIC SMALL LETTER E }
  XK_Cyrillic_shcha             = $06dd;     { U+0449 CYRILLIC SMALL LETTER SHCHA }
  XK_Cyrillic_che               = $06de;     { U+0447 CYRILLIC SMALL LETTER CHE }
  XK_Cyrillic_hardsign          = $06df;     { U+044A CYRILLIC SMALL LETTER HARD SIGN }
  XKc_Cyrillic_YU               = $06e0;     { U+042E CYRILLIC CAPITAL LETTER YU }
  XKc_Cyrillic_A                = $06e1;     { U+0410 CYRILLIC CAPITAL LETTER A }
  XKc_Cyrillic_BE               = $06e2;     { U+0411 CYRILLIC CAPITAL LETTER BE }
  XKc_Cyrillic_TSE              = $06e3;     { U+0426 CYRILLIC CAPITAL LETTER TSE }
  XKc_Cyrillic_DE               = $06e4;     { U+0414 CYRILLIC CAPITAL LETTER DE }
  XKc_Cyrillic_IE               = $06e5;     { U+0415 CYRILLIC CAPITAL LETTER IE }
  XKc_Cyrillic_EF               = $06e6;     { U+0424 CYRILLIC CAPITAL LETTER EF }
  XKc_Cyrillic_GHE              = $06e7;     { U+0413 CYRILLIC CAPITAL LETTER GHE }
  XKc_Cyrillic_HA               = $06e8;     { U+0425 CYRILLIC CAPITAL LETTER HA }
  XKc_Cyrillic_I                = $06e9;     { U+0418 CYRILLIC CAPITAL LETTER I }
  XKc_Cyrillic_SHORTI           = $06ea;     { U+0419 CYRILLIC CAPITAL LETTER SHORT I }
  XKc_Cyrillic_KA               = $06eb;     { U+041A CYRILLIC CAPITAL LETTER KA }
  XKc_Cyrillic_EL               = $06ec;     { U+041B CYRILLIC CAPITAL LETTER EL }
  XKc_Cyrillic_EM               = $06ed;     { U+041C CYRILLIC CAPITAL LETTER EM }
  XKc_Cyrillic_EN               = $06ee;     { U+041D CYRILLIC CAPITAL LETTER EN }
  XKc_Cyrillic_O                = $06ef;     { U+041E CYRILLIC CAPITAL LETTER O }
  XKc_Cyrillic_PE               = $06f0;     { U+041F CYRILLIC CAPITAL LETTER PE }
  XKc_Cyrillic_YA               = $06f1;     { U+042F CYRILLIC CAPITAL LETTER YA }
  XKc_Cyrillic_ER               = $06f2;     { U+0420 CYRILLIC CAPITAL LETTER ER }
  XKc_Cyrillic_ES               = $06f3;     { U+0421 CYRILLIC CAPITAL LETTER ES }
  XKc_Cyrillic_TE               = $06f4;     { U+0422 CYRILLIC CAPITAL LETTER TE }
  XKc_Cyrillic_U                = $06f5;     { U+0423 CYRILLIC CAPITAL LETTER U }
  XKc_Cyrillic_ZHE              = $06f6;     { U+0416 CYRILLIC CAPITAL LETTER ZHE }
  XKc_Cyrillic_VE               = $06f7;     { U+0412 CYRILLIC CAPITAL LETTER VE }
  XKc_Cyrillic_SOFTSIGN         = $06f8;     { U+042C CYRILLIC CAPITAL LETTER SOFT SIGN }
  XKc_Cyrillic_YERU             = $06f9;     { U+042B CYRILLIC CAPITAL LETTER YERU }
  XKc_Cyrillic_ZE               = $06fa;     { U+0417 CYRILLIC CAPITAL LETTER ZE }
  XKc_Cyrillic_SHA              = $06fb;     { U+0428 CYRILLIC CAPITAL LETTER SHA }
  XKc_Cyrillic_E                = $06fc;     { U+042D CYRILLIC CAPITAL LETTER E }
  XKc_Cyrillic_SHCHA            = $06fd;     { U+0429 CYRILLIC CAPITAL LETTER SHCHA }
  XKc_Cyrillic_CHE              = $06fe;     { U+0427 CYRILLIC CAPITAL LETTER CHE }
  XKc_Cyrillic_HARDSIGN         = $06ff;     { U+042A CYRILLIC CAPITAL LETTER HARD SIGN }
{$ENDIF} { XK_CYRILLIC }

{*
 * Greek
 * (based on an early draft of, and not quite identical to, ISO/IEC 8859-7)
 * Byte 3 = 7
 *}

{$IFDEF XK_GREEK}
  XKc_Greek_ALPHAaccent          = $07a1;  { U+0386 GREEK CAPITAL LETTER ALPHA WITH TONOS }
  XKc_Greek_EPSILONaccent        = $07a2;  { U+0388 GREEK CAPITAL LETTER EPSILON WITH TONOS }
  XKc_Greek_ETAaccent            = $07a3;  { U+0389 GREEK CAPITAL LETTER ETA WITH TONOS }
  XKc_Greek_IOTAaccent           = $07a4;  { U+038A GREEK CAPITAL LETTER IOTA WITH TONOS }
  XKc_Greek_IOTAdieresis         = $07a5;  { U+03AA GREEK CAPITAL LETTER IOTA WITH DIALYTIKA }
  XKc_Greek_IOTAdiaeresis        = $07a5;  { old typo }
  XKc_Greek_OMICRONaccent        = $07a7;  { U+038C GREEK CAPITAL LETTER OMICRON WITH TONOS }
  XKc_Greek_UPSILONaccent        = $07a8;  { U+038E GREEK CAPITAL LETTER UPSILON WITH TONOS }
  XKc_Greek_UPSILONdieresis      = $07a9;  { U+03AB GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA }
  XKc_Greek_OMEGAaccent          = $07ab;  { U+038F GREEK CAPITAL LETTER OMEGA WITH TONOS }
  XK_Greek_accentdieresis        = $07ae;  { U+0385 GREEK DIALYTIKA TONOS }
  XK_Greek_horizbar              = $07af;  { U+2015 HORIZONTAL BAR }
  XK_Greek_alphaaccent           = $07b1;  { U+03AC GREEK SMALL LETTER ALPHA WITH TONOS }
  XK_Greek_epsilonaccent         = $07b2;  { U+03AD GREEK SMALL LETTER EPSILON WITH TONOS }
  XK_Greek_etaaccent             = $07b3;  { U+03AE GREEK SMALL LETTER ETA WITH TONOS }
  XK_Greek_iotaaccent            = $07b4;  { U+03AF GREEK SMALL LETTER IOTA WITH TONOS }
  XK_Greek_iotadieresis          = $07b5;  { U+03CA GREEK SMALL LETTER IOTA WITH DIALYTIKA }
  XK_Greek_iotaaccentdieresis    = $07b6;  { U+0390 GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS }
  XK_Greek_omicronaccent         = $07b7;  { U+03CC GREEK SMALL LETTER OMICRON WITH TONOS }
  XK_Greek_upsilonaccent         = $07b8;  { U+03CD GREEK SMALL LETTER UPSILON WITH TONOS }
  XK_Greek_upsilondieresis       = $07b9;  { U+03CB GREEK SMALL LETTER UPSILON WITH DIALYTIKA }
  XK_Greek_upsilonaccentdieresis = $07ba;  { U+03B0 GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS }
  XK_Greek_omegaaccent           = $07bb;  { U+03CE GREEK SMALL LETTER OMEGA WITH TONOS }
  XKc_Greek_ALPHA                = $07c1;  { U+0391 GREEK CAPITAL LETTER ALPHA }
  XKc_Greek_BETA                 = $07c2;  { U+0392 GREEK CAPITAL LETTER BETA }
  XKc_Greek_GAMMA                = $07c3;  { U+0393 GREEK CAPITAL LETTER GAMMA }
  XKc_Greek_DELTA                = $07c4;  { U+0394 GREEK CAPITAL LETTER DELTA }
  XKc_Greek_EPSILON              = $07c5;  { U+0395 GREEK CAPITAL LETTER EPSILON }
  XKc_Greek_ZETA                 = $07c6;  { U+0396 GREEK CAPITAL LETTER ZETA }
  XKc_Greek_ETA                  = $07c7;  { U+0397 GREEK CAPITAL LETTER ETA }
  XKc_Greek_THETA                = $07c8;  { U+0398 GREEK CAPITAL LETTER THETA }
  XKc_Greek_IOTA                 = $07c9;  { U+0399 GREEK CAPITAL LETTER IOTA }
  XKc_Greek_KAPPA                = $07ca;  { U+039A GREEK CAPITAL LETTER KAPPA }
  XKc_Greek_LAMDA                = $07cb;  { U+039B GREEK CAPITAL LETTER LAMDA }
  XKc_Greek_LAMBDA               = $07cb;  { U+039B GREEK CAPITAL LETTER LAMDA }
  XKc_Greek_MU                   = $07cc;  { U+039C GREEK CAPITAL LETTER MU }
  XKc_Greek_NU                   = $07cd;  { U+039D GREEK CAPITAL LETTER NU }
  XKc_Greek_XI                   = $07ce;  { U+039E GREEK CAPITAL LETTER XI }
  XKc_Greek_OMICRON              = $07cf;  { U+039F GREEK CAPITAL LETTER OMICRON }
  XKc_Greek_PI                   = $07d0;  { U+03A0 GREEK CAPITAL LETTER PI }
  XKc_Greek_RHO                  = $07d1;  { U+03A1 GREEK CAPITAL LETTER RHO }
  XKc_Greek_SIGMA                = $07d2;  { U+03A3 GREEK CAPITAL LETTER SIGMA }
  XKc_Greek_TAU                  = $07d4;  { U+03A4 GREEK CAPITAL LETTER TAU }
  XKc_Greek_UPSILON              = $07d5;  { U+03A5 GREEK CAPITAL LETTER UPSILON }
  XKc_Greek_PHI                  = $07d6;  { U+03A6 GREEK CAPITAL LETTER PHI }
  XKc_Greek_CHI                  = $07d7;  { U+03A7 GREEK CAPITAL LETTER CHI }
  XKc_Greek_PSI                  = $07d8;  { U+03A8 GREEK CAPITAL LETTER PSI }
  XKc_Greek_OMEGA                = $07d9;  { U+03A9 GREEK CAPITAL LETTER OMEGA }
  XK_Greek_alpha                 = $07e1;  { U+03B1 GREEK SMALL LETTER ALPHA }
  XK_Greek_beta                  = $07e2;  { U+03B2 GREEK SMALL LETTER BETA }
  XK_Greek_gamma                 = $07e3;  { U+03B3 GREEK SMALL LETTER GAMMA }
  XK_Greek_delta                 = $07e4;  { U+03B4 GREEK SMALL LETTER DELTA }
  XK_Greek_epsilon               = $07e5;  { U+03B5 GREEK SMALL LETTER EPSILON }
  XK_Greek_zeta                  = $07e6;  { U+03B6 GREEK SMALL LETTER ZETA }
  XK_Greek_eta                   = $07e7;  { U+03B7 GREEK SMALL LETTER ETA }
  XK_Greek_theta                 = $07e8;  { U+03B8 GREEK SMALL LETTER THETA }
  XK_Greek_iota                  = $07e9;  { U+03B9 GREEK SMALL LETTER IOTA }
  XK_Greek_kappa                 = $07ea;  { U+03BA GREEK SMALL LETTER KAPPA }
  XK_Greek_lamda                 = $07eb;  { U+03BB GREEK SMALL LETTER LAMDA }
  XK_Greek_lambda                = $07eb;  { U+03BB GREEK SMALL LETTER LAMDA }
  XK_Greek_mu                    = $07ec;  { U+03BC GREEK SMALL LETTER MU }
  XK_Greek_nu                    = $07ed;  { U+03BD GREEK SMALL LETTER NU }
  XK_Greek_xi                    = $07ee;  { U+03BE GREEK SMALL LETTER XI }
  XK_Greek_omicron               = $07ef;  { U+03BF GREEK SMALL LETTER OMICRON }
  XK_Greek_pi                    = $07f0;  { U+03C0 GREEK SMALL LETTER PI }
  XK_Greek_rho                   = $07f1;  { U+03C1 GREEK SMALL LETTER RHO }
  XK_Greek_sigma                 = $07f2;  { U+03C3 GREEK SMALL LETTER SIGMA }
  XK_Greek_finalsmallsigma       = $07f3;  { U+03C2 GREEK SMALL LETTER FINAL SIGMA }
  XK_Greek_tau                   = $07f4;  { U+03C4 GREEK SMALL LETTER TAU }
  XK_Greek_upsilon               = $07f5;  { U+03C5 GREEK SMALL LETTER UPSILON }
  XK_Greek_phi                   = $07f6;  { U+03C6 GREEK SMALL LETTER PHI }
  XK_Greek_chi                   = $07f7;  { U+03C7 GREEK SMALL LETTER CHI }
  XK_Greek_psi                   = $07f8;  { U+03C8 GREEK SMALL LETTER PSI }
  XK_Greek_omega                 = $07f9;  { U+03C9 GREEK SMALL LETTER OMEGA }
  XK_Greek_switch                = $ff7e;  { Alias for mode_switch }
{$ENDIF} { XK_GREEK }

{*
 * Technical
 * (from the DEC VT330/VT420 Technical Character Set, http://vt100.net/charsets/technical.html)
 * Byte 3 = 8
 *}

{$IFDEF XK_TECHNICAL}
  XK_leftradical               = $08a1;  { U+23B7 RADICAL SYMBOL BOTTOM }
  XK_topleftradical            = $08a2;  {(U+250C BOX DRAWINGS LIGHT DOWN AND RIGHT)}
  XK_horizconnector            = $08a3;  {(U+2500 BOX DRAWINGS LIGHT HORIZONTAL)}
  XK_topintegral               = $08a4;  { U+2320 TOP HALF INTEGRAL }
  XK_botintegral               = $08a5;  { U+2321 BOTTOM HALF INTEGRAL }
  XK_vertconnector             = $08a6;  {(U+2502 BOX DRAWINGS LIGHT VERTICAL)}
  XK_topleftsqbracket          = $08a7;  { U+23A1 LEFT SQUARE BRACKET UPPER CORNER }
  XK_botleftsqbracket          = $08a8;  { U+23A3 LEFT SQUARE BRACKET LOWER CORNER }
  XK_toprightsqbracket         = $08a9;  { U+23A4 RIGHT SQUARE BRACKET UPPER CORNER }
  XK_botrightsqbracket         = $08aa;  { U+23A6 RIGHT SQUARE BRACKET LOWER CORNER }
  XK_topleftparens             = $08ab;  { U+239B LEFT PARENTHESIS UPPER HOOK }
  XK_botleftparens             = $08ac;  { U+239D LEFT PARENTHESIS LOWER HOOK }
  XK_toprightparens            = $08ad;  { U+239E RIGHT PARENTHESIS UPPER HOOK }
  XK_botrightparens            = $08ae;  { U+23A0 RIGHT PARENTHESIS LOWER HOOK }
  XK_leftmiddlecurlybrace      = $08af;  { U+23A8 LEFT CURLY BRACKET MIDDLE PIECE }
  XK_rightmiddlecurlybrace     = $08b0;  { U+23AC RIGHT CURLY BRACKET MIDDLE PIECE }
  XK_topleftsummation          = $08b1;
  XK_botleftsummation          = $08b2;
  XK_topvertsummationconnector = $08b3;
  XK_botvertsummationconnector = $08b4;
  XK_toprightsummation         = $08b5;
  XK_botrightsummation         = $08b6;
  XK_rightmiddlesummation      = $08b7;
  XK_lessthanequal             = $08bc;  { U+2264 LESS-THAN OR EQUAL TO }
  XK_notequal                  = $08bd;  { U+2260 NOT EQUAL TO }
  XK_greaterthanequal          = $08be;  { U+2265 GREATER-THAN OR EQUAL TO }
  XK_integral                  = $08bf;  { U+222B INTEGRAL }
  XK_therefore                 = $08c0;  { U+2234 THEREFORE }
  XK_variation                 = $08c1;  { U+221D PROPORTIONAL TO }
  XK_infinity                  = $08c2;  { U+221E INFINITY }
  XK_nabla                     = $08c5;  { U+2207 NABLA }
  XK_approximate               = $08c8;  { U+223C TILDE OPERATOR }
  XK_similarequal              = $08c9;  { U+2243 ASYMPTOTICALLY EQUAL TO }
  XK_ifonlyif                  = $08cd;  { U+21D4 LEFT RIGHT DOUBLE ARROW }
  XK_implies                   = $08ce;  { U+21D2 RIGHTWARDS DOUBLE ARROW }
  XK_identical                 = $08cf;  { U+2261 IDENTICAL TO }
  XK_radical                   = $08d6;  { U+221A SQUARE ROOT }
  XK_includedin                = $08da;  { U+2282 SUBSET OF }
  XK_includes                  = $08db;  { U+2283 SUPERSET OF }
  XK_intersection              = $08dc;  { U+2229 INTERSECTION }
  XK_union                     = $08dd;  { U+222A UNION }
  XK_logicaland                = $08de;  { U+2227 LOGICAL AND }
  XK_logicalor                 = $08df;  { U+2228 LOGICAL OR }
  XK_partialderivative         = $08ef;  { U+2202 PARTIAL DIFFERENTIAL }
  XK_function                  = $08f6;  { U+0192 LATIN SMALL LETTER F WITH HOOK }
  XK_leftarrow                 = $08fb;  { U+2190 LEFTWARDS ARROW }
  XK_uparrow                   = $08fc;  { U+2191 UPWARDS ARROW }
  XK_rightarrow                = $08fd;  { U+2192 RIGHTWARDS ARROW }
  XK_downarrow                 = $08fe;  { U+2193 DOWNWARDS ARROW }
{$ENDIF} { XK_TECHNICAL }

{*
 * Special
 * (from the DEC VT100 Special Graphics Character Set)
 * Byte 3 = 9
 *}

{$IFDEF XK_SPECIAL}
  XK_blank          = $09df;
  XK_soliddiamond   = $09e0;  { U+25C6 BLACK DIAMOND }
  XK_checkerboard   = $09e1;  { U+2592 MEDIUM SHADE }
  XK_ht             = $09e2;  { U+2409 SYMBOL FOR HORIZONTAL TABULATION }
  XK_ff             = $09e3;  { U+240C SYMBOL FOR FORM FEED }
  XK_cr             = $09e4;  { U+240D SYMBOL FOR CARRIAGE RETURN }
  XK_lf             = $09e5;  { U+240A SYMBOL FOR LINE FEED }
  XK_nl             = $09e8;  { U+2424 SYMBOL FOR NEWLINE }
  XK_vt             = $09e9;  { U+240B SYMBOL FOR VERTICAL TABULATION }
  XK_lowrightcorner = $09ea;  { U+2518 BOX DRAWINGS LIGHT UP AND LEFT }
  XK_uprightcorner  = $09eb;  { U+2510 BOX DRAWINGS LIGHT DOWN AND LEFT }
  XK_upleftcorner   = $09ec;  { U+250C BOX DRAWINGS LIGHT DOWN AND RIGHT }
  XK_lowleftcorner  = $09ed;  { U+2514 BOX DRAWINGS LIGHT UP AND RIGHT }
  XK_crossinglines  = $09ee;  { U+253C BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL }
  XK_horizlinescan1 = $09ef;  { U+23BA HORIZONTAL SCAN LINE-1 }
  XK_horizlinescan3 = $09f0;  { U+23BB HORIZONTAL SCAN LINE-3 }
  XK_horizlinescan5 = $09f1;  { U+2500 BOX DRAWINGS LIGHT HORIZONTAL }
  XK_horizlinescan7 = $09f2;  { U+23BC HORIZONTAL SCAN LINE-7 }
  XK_horizlinescan9 = $09f3;  { U+23BD HORIZONTAL SCAN LINE-9 }
  XK_leftt          = $09f4;  { U+251C BOX DRAWINGS LIGHT VERTICAL AND RIGHT }
  XK_rightt         = $09f5;  { U+2524 BOX DRAWINGS LIGHT VERTICAL AND LEFT }
  XK_bott           = $09f6;  { U+2534 BOX DRAWINGS LIGHT UP AND HORIZONTAL }
  XK_topt           = $09f7;  { U+252C BOX DRAWINGS LIGHT DOWN AND HORIZONTAL }
  XK_vertbar        = $09f8;  { U+2502 BOX DRAWINGS LIGHT VERTICAL }
{$ENDIF} { XK_SPECIAL }

{*
 * Publishing
 * (these are probably from a long forgotten DEC Publishing
 * font that once shipped with DECwrite)
 * Byte 3 = $0a
 *}

{$IFDEF XK_PUBLISHING}
  XK_emspace              = $0aa1;  { U+2003 EM SPACE }
  XK_enspace              = $0aa2;  { U+2002 EN SPACE }
  XK_em3space             = $0aa3;  { U+2004 THREE-PER-EM SPACE }
  XK_em4space             = $0aa4;  { U+2005 FOUR-PER-EM SPACE }
  XK_digitspace           = $0aa5;  { U+2007 FIGURE SPACE }
  XK_punctspace           = $0aa6;  { U+2008 PUNCTUATION SPACE }
  XK_thinspace            = $0aa7;  { U+2009 THIN SPACE }
  XK_hairspace            = $0aa8;  { U+200A HAIR SPACE }
  XK_emdash               = $0aa9;  { U+2014 EM DASH }
  XK_endash               = $0aaa;  { U+2013 EN DASH }
  XK_signifblank          = $0aac;  {(U+2423 OPEN BOX)}
  XK_ellipsis             = $0aae;  { U+2026 HORIZONTAL ELLIPSIS }
  XK_doubbaselinedot      = $0aaf;  { U+2025 TWO DOT LEADER }
  XK_onethird             = $0ab0;  { U+2153 VULGAR FRACTION ONE THIRD }
  XK_twothirds            = $0ab1;  { U+2154 VULGAR FRACTION TWO THIRDS }
  XK_onefifth             = $0ab2;  { U+2155 VULGAR FRACTION ONE FIFTH }
  XK_twofifths            = $0ab3;  { U+2156 VULGAR FRACTION TWO FIFTHS }
  XK_threefifths          = $0ab4;  { U+2157 VULGAR FRACTION THREE FIFTHS }
  XK_fourfifths           = $0ab5;  { U+2158 VULGAR FRACTION FOUR FIFTHS }
  XK_onesixth             = $0ab6;  { U+2159 VULGAR FRACTION ONE SIXTH }
  XK_fivesixths           = $0ab7;  { U+215A VULGAR FRACTION FIVE SIXTHS }
  XK_careof               = $0ab8;  { U+2105 CARE OF }
  XK_figdash              = $0abb;  { U+2012 FIGURE DASH }
  XK_leftanglebracket     = $0abc;  {(U+27E8 MATHEMATICAL LEFT ANGLE BRACKET)}
  XK_decimalpoint         = $0abd;  {(U+002E FULL STOP)}
  XK_rightanglebracket    = $0abe;  {(U+27E9 MATHEMATICAL RIGHT ANGLE BRACKET)}
  XK_marker               = $0abf;
  XK_oneeighth            = $0ac3;  { U+215B VULGAR FRACTION ONE EIGHTH }
  XK_threeeighths         = $0ac4;  { U+215C VULGAR FRACTION THREE EIGHTHS }
  XK_fiveeighths          = $0ac5;  { U+215D VULGAR FRACTION FIVE EIGHTHS }
  XK_seveneighths         = $0ac6;  { U+215E VULGAR FRACTION SEVEN EIGHTHS }
  XK_trademark            = $0ac9;  { U+2122 TRADE MARK SIGN }
  XK_signaturemark        = $0aca;  {(U+2613 SALTIRE)}
  XK_trademarkincircle    = $0acb;
  XK_leftopentriangle     = $0acc;  {(U+25C1 WHITE LEFT-POINTING TRIANGLE)}
  XK_rightopentriangle    = $0acd;  {(U+25B7 WHITE RIGHT-POINTING TRIANGLE)}
  XK_emopencircle         = $0ace;  {(U+25CB WHITE CIRCLE)}
  XK_emopenrectangle      = $0acf;  {(U+25AF WHITE VERTICAL RECTANGLE)}
  XK_leftsinglequotemark  = $0ad0;  { U+2018 LEFT SINGLE QUOTATION MARK }
  XK_rightsinglequotemark = $0ad1;  { U+2019 RIGHT SINGLE QUOTATION MARK }
  XK_leftdoublequotemark  = $0ad2;  { U+201C LEFT DOUBLE QUOTATION MARK }
  XK_rightdoublequotemark = $0ad3;  { U+201D RIGHT DOUBLE QUOTATION MARK }
  XK_prescription         = $0ad4;  { U+211E PRESCRIPTION TAKE }
  XK_permille             = $0ad5;  { U+2030 PER MILLE SIGN }
  XK_minutes              = $0ad6;  { U+2032 PRIME }
  XK_seconds              = $0ad7;  { U+2033 DOUBLE PRIME }
  XK_latincross           = $0ad9;  { U+271D LATIN CROSS }
  XK_hexagram             = $0ada;
  XK_filledrectbullet     = $0adb;  {(U+25AC BLACK RECTANGLE)}
  XK_filledlefttribullet  = $0adc;  {(U+25C0 BLACK LEFT-POINTING TRIANGLE)}
  XK_filledrighttribullet = $0add;  {(U+25B6 BLACK RIGHT-POINTING TRIANGLE)}
  XK_emfilledcircle       = $0ade;  {(U+25CF BLACK CIRCLE)}
  XK_emfilledrect         = $0adf;  {(U+25AE BLACK VERTICAL RECTANGLE)}
  XK_enopencircbullet     = $0ae0;  {(U+25E6 WHITE BULLET)}
  XK_enopensquarebullet   = $0ae1;  {(U+25AB WHITE SMALL SQUARE)}
  XK_openrectbullet       = $0ae2;  {(U+25AD WHITE RECTANGLE)}
  XK_opentribulletup      = $0ae3;  {(U+25B3 WHITE UP-POINTING TRIANGLE)}
  XK_opentribulletdown    = $0ae4;  {(U+25BD WHITE DOWN-POINTING TRIANGLE)}
  XK_openstar             = $0ae5;  {(U+2606 WHITE STAR)}
  XK_enfilledcircbullet   = $0ae6;  {(U+2022 BULLET)}
  XK_enfilledsqbullet     = $0ae7;  {(U+25AA BLACK SMALL SQUARE)}
  XK_filledtribulletup    = $0ae8;  {(U+25B2 BLACK UP-POINTING TRIANGLE)}
  XK_filledtribulletdown  = $0ae9;  {(U+25BC BLACK DOWN-POINTING TRIANGLE)}
  XK_leftpointer          = $0aea;  {(U+261C WHITE LEFT POINTING INDEX)}
  XK_rightpointer         = $0aeb;  {(U+261E WHITE RIGHT POINTING INDEX)}
  XK_club                 = $0aec;  { U+2663 BLACK CLUB SUIT }
  XK_diamond              = $0aed;  { U+2666 BLACK DIAMOND SUIT }
  XK_heart                = $0aee;  { U+2665 BLACK HEART SUIT }
  XK_maltesecross         = $0af0;  { U+2720 MALTESE CROSS }
  XK_dagger               = $0af1;  { U+2020 DAGGER }
  XK_doubledagger         = $0af2;  { U+2021 DOUBLE DAGGER }
  XK_checkmark            = $0af3;  { U+2713 CHECK MARK }
  XK_ballotcross          = $0af4;  { U+2717 BALLOT X }
  XK_musicalsharp         = $0af5;  { U+266F MUSIC SHARP SIGN }
  XK_musicalflat          = $0af6;  { U+266D MUSIC FLAT SIGN }
  XK_malesymbol           = $0af7;  { U+2642 MALE SIGN }
  XK_femalesymbol         = $0af8;  { U+2640 FEMALE SIGN }
  XK_telephone            = $0af9;  { U+260E BLACK TELEPHONE }
  XK_telephonerecorder    = $0afa;  { U+2315 TELEPHONE RECORDER }
  XK_phonographcopyright  = $0afb;  { U+2117 SOUND RECORDING COPYRIGHT }
  XK_caret                = $0afc;  { U+2038 CARET }
  XK_singlelowquotemark   = $0afd;  { U+201A SINGLE LOW-9 QUOTATION MARK }
  XK_doublelowquotemark   = $0afe;  { U+201E DOUBLE LOW-9 QUOTATION MARK }
  XK_cursor               = $0aff;
{$ENDIF} { XK_PUBLISHING }

{*
 * APL
 * Byte 3 = $0b
 *}

{$IFDEF XK_APL}
  XK_leftcaret  = $0ba3;  {(U+003C LESS-THAN SIGN)}
  XK_rightcaret = $0ba6;  {(U+003E GREATER-THAN SIGN)}
  XK_downcaret  = $0ba8;  {(U+2228 LOGICAL OR)}
  XK_upcaret    = $0ba9;  {(U+2227 LOGICAL AND)}
  XK_overbar    = $0bc0;  {(U+00AF MACRON)}
  XK_downtack   = $0bc2;  { U+22A4 DOWN TACK }
  XK_upshoe     = $0bc3;  {(U+2229 INTERSECTION)}
  XK_downstile  = $0bc4;  { U+230A LEFT FLOOR }
  XK_underbar   = $0bc6;  {(U+005F LOW LINE)}
  XK_jot        = $0bca;  { U+2218 RING OPERATOR }
  XK_quad       = $0bcc;  { U+2395 APL FUNCTIONAL SYMBOL QUAD }
  XK_uptack     = $0bce;  { U+22A5 UP TACK }
  XK_circle     = $0bcf;  { U+25CB WHITE CIRCLE }
  XK_upstile    = $0bd3;  { U+2308 LEFT CEILING }
  XK_downshoe   = $0bd6;  {(U+222A UNION)}
  XK_rightshoe  = $0bd8;  {(U+2283 SUPERSET OF)}
  XK_leftshoe   = $0bda;  {(U+2282 SUBSET OF)}
  XK_lefttack   = $0bdc;  { U+22A3 LEFT TACK }
  XK_righttack  = $0bfc;  { U+22A2 RIGHT TACK }
{$ENDIF} { XK_APL }

{*
 * Hebrew
 * Byte 3 = $0c
 *}

{$IFDEF XK_HEBREW}
  XK_hebrew_doublelowline = $0cdf;  { U+2017 DOUBLE LOW LINE }
  XK_hebrew_aleph         = $0ce0;  { U+05D0 HEBREW LETTER ALEF }
  XK_hebrew_bet           = $0ce1;  { U+05D1 HEBREW LETTER BET }
  XK_hebrew_beth          = $0ce1;  { deprecated }
  XK_hebrew_gimel         = $0ce2;  { U+05D2 HEBREW LETTER GIMEL }
  XK_hebrew_gimmel        = $0ce2;  { deprecated }
  XK_hebrew_dalet         = $0ce3;  { U+05D3 HEBREW LETTER DALET }
  XK_hebrew_daleth        = $0ce3;  { deprecated }
  XK_hebrew_he            = $0ce4;  { U+05D4 HEBREW LETTER HE }
  XK_hebrew_waw           = $0ce5;  { U+05D5 HEBREW LETTER VAV }
  XK_hebrew_zain          = $0ce6;  { U+05D6 HEBREW LETTER ZAYIN }
  XK_hebrew_zayin         = $0ce6;  { deprecated }
  XK_hebrew_chet          = $0ce7;  { U+05D7 HEBREW LETTER HET }
  XK_hebrew_het           = $0ce7;  { deprecated }
  XK_hebrew_tet           = $0ce8;  { U+05D8 HEBREW LETTER TET }
  XK_hebrew_teth          = $0ce8;  { deprecated }
  XK_hebrew_yod           = $0ce9;  { U+05D9 HEBREW LETTER YOD }
  XK_hebrew_finalkaph     = $0cea;  { U+05DA HEBREW LETTER FINAL KAF }
  XK_hebrew_kaph          = $0ceb;  { U+05DB HEBREW LETTER KAF }
  XK_hebrew_lamed         = $0cec;  { U+05DC HEBREW LETTER LAMED }
  XK_hebrew_finalmem      = $0ced;  { U+05DD HEBREW LETTER FINAL MEM }
  XK_hebrew_mem           = $0cee;  { U+05DE HEBREW LETTER MEM }
  XK_hebrew_finalnun      = $0cef;  { U+05DF HEBREW LETTER FINAL NUN }
  XK_hebrew_nun           = $0cf0;  { U+05E0 HEBREW LETTER NUN }
  XK_hebrew_samech        = $0cf1;  { U+05E1 HEBREW LETTER SAMEKH }
  XK_hebrew_samekh        = $0cf1;  { deprecated }
  XK_hebrew_ayin          = $0cf2;  { U+05E2 HEBREW LETTER AYIN }
  XK_hebrew_finalpe       = $0cf3;  { U+05E3 HEBREW LETTER FINAL PE }
  XK_hebrew_pe            = $0cf4;  { U+05E4 HEBREW LETTER PE }
  XK_hebrew_finalzade     = $0cf5;  { U+05E5 HEBREW LETTER FINAL TSADI }
  XK_hebrew_finalzadi     = $0cf5;  { deprecated }
  XK_hebrew_zade          = $0cf6;  { U+05E6 HEBREW LETTER TSADI }
  XK_hebrew_zadi          = $0cf6;  { deprecated }
  XK_hebrew_qoph          = $0cf7;  { U+05E7 HEBREW LETTER QOF }
  XK_hebrew_kuf           = $0cf7;  { deprecated }
  XK_hebrew_resh          = $0cf8;  { U+05E8 HEBREW LETTER RESH }
  XK_hebrew_shin          = $0cf9;  { U+05E9 HEBREW LETTER SHIN }
  XK_hebrew_taw           = $0cfa;  { U+05EA HEBREW LETTER TAV }
  XK_hebrew_taf           = $0cfa;  { deprecated }
  XK_Hebrew_switch        = $ff7e;  { Alias for mode_switch }
{$ENDIF} { XK_HEBREW }

{*
 * Thai
 * Byte 3 = $0d
 *}

{$IFDEF XK_THAI}
  XK_Thai_kokai             = $0da1;  { U+0E01 THAI CHARACTER KO KAI }
  XK_Thai_khokhai           = $0da2;  { U+0E02 THAI CHARACTER KHO KHAI }
  XK_Thai_khokhuat          = $0da3;  { U+0E03 THAI CHARACTER KHO KHUAT }
  XK_Thai_khokhwai          = $0da4;  { U+0E04 THAI CHARACTER KHO KHWAI }
  XK_Thai_khokhon           = $0da5;  { U+0E05 THAI CHARACTER KHO KHON }
  XK_Thai_khorakhang        = $0da6;  { U+0E06 THAI CHARACTER KHO RAKHANG }
  XK_Thai_ngongu            = $0da7;  { U+0E07 THAI CHARACTER NGO NGU }
  XK_Thai_chochan           = $0da8;  { U+0E08 THAI CHARACTER CHO CHAN }
  XK_Thai_choching          = $0da9;  { U+0E09 THAI CHARACTER CHO CHING }
  XK_Thai_chochang          = $0daa;  { U+0E0A THAI CHARACTER CHO CHANG }
  XK_Thai_soso              = $0dab;  { U+0E0B THAI CHARACTER SO SO }
  XK_Thai_chochoe           = $0dac;  { U+0E0C THAI CHARACTER CHO CHOE }
  XK_Thai_yoying            = $0dad;  { U+0E0D THAI CHARACTER YO YING }
  XK_Thai_dochada           = $0dae;  { U+0E0E THAI CHARACTER DO CHADA }
  XK_Thai_topatak           = $0daf;  { U+0E0F THAI CHARACTER TO PATAK }
  XK_Thai_thothan           = $0db0;  { U+0E10 THAI CHARACTER THO THAN }
  XK_Thai_thonangmontho     = $0db1;  { U+0E11 THAI CHARACTER THO NANGMONTHO }
  XK_Thai_thophuthao        = $0db2;  { U+0E12 THAI CHARACTER THO PHUTHAO }
  XK_Thai_nonen             = $0db3;  { U+0E13 THAI CHARACTER NO NEN }
  XK_Thai_dodek             = $0db4;  { U+0E14 THAI CHARACTER DO DEK }
  XK_Thai_totao             = $0db5;  { U+0E15 THAI CHARACTER TO TAO }
  XK_Thai_thothung          = $0db6;  { U+0E16 THAI CHARACTER THO THUNG }
  XK_Thai_thothahan         = $0db7;  { U+0E17 THAI CHARACTER THO THAHAN }
  XK_Thai_thothong          = $0db8;  { U+0E18 THAI CHARACTER THO THONG }
  XK_Thai_nonu              = $0db9;  { U+0E19 THAI CHARACTER NO NU }
  XK_Thai_bobaimai          = $0dba;  { U+0E1A THAI CHARACTER BO BAIMAI }
  XK_Thai_popla             = $0dbb;  { U+0E1B THAI CHARACTER PO PLA }
  XK_Thai_phophung          = $0dbc;  { U+0E1C THAI CHARACTER PHO PHUNG }
  XK_Thai_fofa              = $0dbd;  { U+0E1D THAI CHARACTER FO FA }
  XK_Thai_phophan           = $0dbe;  { U+0E1E THAI CHARACTER PHO PHAN }
  XK_Thai_fofan             = $0dbf;  { U+0E1F THAI CHARACTER FO FAN }
  XK_Thai_phosamphao        = $0dc0;  { U+0E20 THAI CHARACTER PHO SAMPHAO }
  XK_Thai_moma              = $0dc1;  { U+0E21 THAI CHARACTER MO MA }
  XK_Thai_yoyak             = $0dc2;  { U+0E22 THAI CHARACTER YO YAK }
  XK_Thai_rorua             = $0dc3;  { U+0E23 THAI CHARACTER RO RUA }
  XK_Thai_ru                = $0dc4;  { U+0E24 THAI CHARACTER RU }
  XK_Thai_loling            = $0dc5;  { U+0E25 THAI CHARACTER LO LING }
  XK_Thai_lu                = $0dc6;  { U+0E26 THAI CHARACTER LU }
  XK_Thai_wowaen            = $0dc7;  { U+0E27 THAI CHARACTER WO WAEN }
  XK_Thai_sosala            = $0dc8;  { U+0E28 THAI CHARACTER SO SALA }
  XK_Thai_sorusi            = $0dc9;  { U+0E29 THAI CHARACTER SO RUSI }
  XK_Thai_sosua             = $0dca;  { U+0E2A THAI CHARACTER SO SUA }
  XK_Thai_hohip             = $0dcb;  { U+0E2B THAI CHARACTER HO HIP }
  XK_Thai_lochula           = $0dcc;  { U+0E2C THAI CHARACTER LO CHULA }
  XK_Thai_oang              = $0dcd;  { U+0E2D THAI CHARACTER O ANG }
  XK_Thai_honokhuk          = $0dce;  { U+0E2E THAI CHARACTER HO NOKHUK }
  XK_Thai_paiyannoi         = $0dcf;  { U+0E2F THAI CHARACTER PAIYANNOI }
  XK_Thai_saraa             = $0dd0;  { U+0E30 THAI CHARACTER SARA A }
  XK_Thai_maihanakat        = $0dd1;  { U+0E31 THAI CHARACTER MAI HAN-AKAT }
  XK_Thai_saraaa            = $0dd2;  { U+0E32 THAI CHARACTER SARA AA }
  XK_Thai_saraam            = $0dd3;  { U+0E33 THAI CHARACTER SARA AM }
  XK_Thai_sarai             = $0dd4;  { U+0E34 THAI CHARACTER SARA I }
  XK_Thai_saraii            = $0dd5;  { U+0E35 THAI CHARACTER SARA II }
  XK_Thai_saraue            = $0dd6;  { U+0E36 THAI CHARACTER SARA UE }
  XK_Thai_sarauee           = $0dd7;  { U+0E37 THAI CHARACTER SARA UEE }
  XK_Thai_sarau             = $0dd8;  { U+0E38 THAI CHARACTER SARA U }
  XK_Thai_sarauu            = $0dd9;  { U+0E39 THAI CHARACTER SARA UU }
  XK_Thai_phinthu           = $0dda;  { U+0E3A THAI CHARACTER PHINTHU }
  XK_Thai_maihanakat_maitho = $0dde;
  XK_Thai_baht              = $0ddf;  { U+0E3F THAI CURRENCY SYMBOL BAHT }
  XK_Thai_sarae             = $0de0;  { U+0E40 THAI CHARACTER SARA E }
  XK_Thai_saraae            = $0de1;  { U+0E41 THAI CHARACTER SARA AE }
  XK_Thai_sarao             = $0de2;  { U+0E42 THAI CHARACTER SARA O }
  XK_Thai_saraaimaimuan     = $0de3;  { U+0E43 THAI CHARACTER SARA AI MAIMUAN }
  XK_Thai_saraaimaimalai    = $0de4;  { U+0E44 THAI CHARACTER SARA AI MAIMALAI }
  XK_Thai_lakkhangyao       = $0de5;  { U+0E45 THAI CHARACTER LAKKHANGYAO }
  XK_Thai_maiyamok          = $0de6;  { U+0E46 THAI CHARACTER MAIYAMOK }
  XK_Thai_maitaikhu         = $0de7;  { U+0E47 THAI CHARACTER MAITAIKHU }
  XK_Thai_maiek             = $0de8;  { U+0E48 THAI CHARACTER MAI EK }
  XK_Thai_maitho            = $0de9;  { U+0E49 THAI CHARACTER MAI THO }
  XK_Thai_maitri            = $0dea;  { U+0E4A THAI CHARACTER MAI TRI }
  XK_Thai_maichattawa       = $0deb;  { U+0E4B THAI CHARACTER MAI CHATTAWA }
  XK_Thai_thanthakhat       = $0dec;  { U+0E4C THAI CHARACTER THANTHAKHAT }
  XK_Thai_nikhahit          = $0ded;  { U+0E4D THAI CHARACTER NIKHAHIT }
  XK_Thai_leksun            = $0df0;  { U+0E50 THAI DIGIT ZERO }
  XK_Thai_leknung           = $0df1;  { U+0E51 THAI DIGIT ONE }
  XK_Thai_leksong           = $0df2;  { U+0E52 THAI DIGIT TWO }
  XK_Thai_leksam            = $0df3;  { U+0E53 THAI DIGIT THREE }
  XK_Thai_leksi             = $0df4;  { U+0E54 THAI DIGIT FOUR }
  XK_Thai_lekha             = $0df5;  { U+0E55 THAI DIGIT FIVE }
  XK_Thai_lekhok            = $0df6;  { U+0E56 THAI DIGIT SIX }
  XK_Thai_lekchet           = $0df7;  { U+0E57 THAI DIGIT SEVEN }
  XK_Thai_lekpaet           = $0df8;  { U+0E58 THAI DIGIT EIGHT }
  XK_Thai_lekkao            = $0df9;  { U+0E59 THAI DIGIT NINE }
{$ENDIF} { XK_THAI }

{*
 *   Korean
 *   Byte 3 = $0e
 *}

{$IFDEF XK_KOREAN}

  XK_Hangul                     = $ff31;    { Hangul start/stop(toggle) }
  XK_Hangul_Start               = $ff32;    { Hangul start }
  XK_Hangul_End                 = $ff33;    { Hangul end, English start }
  XK_Hangul_Hanja               = $ff34;    { Start Hangul->Hanja Conversion }
  XK_Hangul_Jamo                = $ff35;    { Hangul Jamo mode }
  XK_Hangul_Romaja              = $ff36;    { Hangul Romaja mode }
  XK_Hangul_Codeinput           = $ff37;    { Hangul code input mode }
  XK_Hangul_Jeonja              = $ff38;    { Jeonja mode }
  XK_Hangul_Banja               = $ff39;    { Banja mode }
  XK_Hangul_PreHanja            = $ff3a;    { Pre Hanja conversion }
  XK_Hangul_PostHanja           = $ff3b;    { Post Hanja conversion }
  XK_Hangul_SingleCandidate     = $ff3c;    { Single candidate }
  XK_Hangul_MultipleCandidate   = $ff3d;    { Multiple candidate }
  XK_Hangul_PreviousCandidate   = $ff3e;    { Previous candidate }
  XK_Hangul_Special             = $ff3f;    { Special symbols }
  XK_Hangul_switch              = $ff7e;    { Alias for mode_switch }

{ Hangul Consonant Characters }
  XK_Hangul_Kiyeog              = $0ea1;
  XK_Hangul_SsangKiyeog         = $0ea2;
  XK_Hangul_KiyeogSios          = $0ea3;
  XK_Hangul_Nieun               = $0ea4;
  XK_Hangul_NieunJieuj          = $0ea5;
  XK_Hangul_NieunHieuh          = $0ea6;
  XK_Hangul_Dikeud              = $0ea7;
  XK_Hangul_SsangDikeud         = $0ea8;
  XK_Hangul_Rieul               = $0ea9;
  XK_Hangul_RieulKiyeog         = $0eaa;
  XK_Hangul_RieulMieum          = $0eab;
  XK_Hangul_RieulPieub          = $0eac;
  XK_Hangul_RieulSios           = $0ead;
  XK_Hangul_RieulTieut          = $0eae;
  XK_Hangul_RieulPhieuf         = $0eaf;
  XK_Hangul_RieulHieuh          = $0eb0;
  XK_Hangul_Mieum               = $0eb1;
  XK_Hangul_Pieub               = $0eb2;
  XK_Hangul_SsangPieub          = $0eb3;
  XK_Hangul_PieubSios           = $0eb4;
  XK_Hangul_Sios                = $0eb5;
  XK_Hangul_SsangSios           = $0eb6;
  XK_Hangul_Ieung               = $0eb7;
  XK_Hangul_Jieuj               = $0eb8;
  XK_Hangul_SsangJieuj          = $0eb9;
  XK_Hangul_Cieuc               = $0eba;
  XK_Hangul_Khieuq              = $0ebb;
  XK_Hangul_Tieut               = $0ebc;
  XK_Hangul_Phieuf              = $0ebd;
  XK_Hangul_Hieuh               = $0ebe;

{ Hangul Vowel Characters }
  XK_Hangul_A                   = $0ebf;
  XK_Hangul_AE                  = $0ec0;
  XK_Hangul_YA                  = $0ec1;
  XK_Hangul_YAE                 = $0ec2;
  XK_Hangul_EO                  = $0ec3;
  XK_Hangul_E                   = $0ec4;
  XK_Hangul_YEO                 = $0ec5;
  XK_Hangul_YE                  = $0ec6;
  XK_Hangul_O                   = $0ec7;
  XK_Hangul_WA                  = $0ec8;
  XK_Hangul_WAE                 = $0ec9;
  XK_Hangul_OE                  = $0eca;
  XK_Hangul_YO                  = $0ecb;
  XK_Hangul_U                   = $0ecc;
  XK_Hangul_WEO                 = $0ecd;
  XK_Hangul_WE                  = $0ece;
  XK_Hangul_WI                  = $0ecf;
  XK_Hangul_YU                  = $0ed0;
  XK_Hangul_EU                  = $0ed1;
  XK_Hangul_YI                  = $0ed2;
  XK_Hangul_I                   = $0ed3;

{ Hangul syllable-final (JongSeong) Characters }
  XK_Hangul_J_Kiyeog            = $0ed4;
  XK_Hangul_J_SsangKiyeog       = $0ed5;
  XK_Hangul_J_KiyeogSios        = $0ed6;
  XK_Hangul_J_Nieun             = $0ed7;
  XK_Hangul_J_NieunJieuj        = $0ed8;
  XK_Hangul_J_NieunHieuh        = $0ed9;
  XK_Hangul_J_Dikeud            = $0eda;
  XK_Hangul_J_Rieul             = $0edb;
  XK_Hangul_J_RieulKiyeog       = $0edc;
  XK_Hangul_J_RieulMieum        = $0edd;
  XK_Hangul_J_RieulPieub        = $0ede;
  XK_Hangul_J_RieulSios         = $0edf;
  XK_Hangul_J_RieulTieut        = $0ee0;
  XK_Hangul_J_RieulPhieuf       = $0ee1;
  XK_Hangul_J_RieulHieuh        = $0ee2;
  XK_Hangul_J_Mieum             = $0ee3;
  XK_Hangul_J_Pieub             = $0ee4;
  XK_Hangul_J_PieubSios         = $0ee5;
  XK_Hangul_J_Sios              = $0ee6;
  XK_Hangul_J_SsangSios         = $0ee7;
  XK_Hangul_J_Ieung             = $0ee8;
  XK_Hangul_J_Jieuj             = $0ee9;
  XK_Hangul_J_Cieuc             = $0eea;
  XK_Hangul_J_Khieuq            = $0eeb;
  XK_Hangul_J_Tieut             = $0eec;
  XK_Hangul_J_Phieuf            = $0eed;
  XK_Hangul_J_Hieuh             = $0eee;

{ Ancient Hangul Consonant Characters }
  XK_Hangul_RieulYeorinHieuh    = $0eef;
  XK_Hangul_SunkyeongeumMieum   = $0ef0;
  XK_Hangul_SunkyeongeumPieub   = $0ef1;
  XK_Hangul_PanSios             = $0ef2;
  XK_Hangul_KkogjiDalrinIeung   = $0ef3;
  XK_Hangul_SunkyeongeumPhieuf  = $0ef4;
  XK_Hangul_YeorinHieuh         = $0ef5;

{ Ancient Hangul Vowel Characters }
  XK_Hangul_AraeA               = $0ef6;
  XK_Hangul_AraeAE              = $0ef7;

{ Ancient Hangul syllable-final (JongSeong) Characters }
  XK_Hangul_J_PanSios           = $0ef8;
  XK_Hangul_J_KkogjiDalrinIeung = $0ef9;
  XK_Hangul_J_YeorinHieuh       = $0efa;

{ Korean currency symbol }
  XK_Korean_Won                 = $0eff;  {(U+20A9 WON SIGN)}

{$ENDIF} { XK_KOREAN }

{*
 * Armenian
 *}

{$IFDEF XK_ARMENIAN}
  XK_Armenian_ligature_ew     = $1000587;  { U+0587 ARMENIAN SMALL LIGATURE ECH YIWN }
  XK_Armenian_full_stop       = $1000589;  { U+0589 ARMENIAN FULL STOP }
  XK_Armenian_verjaket        = $1000589;  { U+0589 ARMENIAN FULL STOP }
  XK_Armenian_separation_mark = $100055d;  { U+055D ARMENIAN COMMA }
  XK_Armenian_but             = $100055d;  { U+055D ARMENIAN COMMA }
  XK_Armenian_hyphen          = $100058a;  { U+058A ARMENIAN HYPHEN }
  XK_Armenian_yentamna        = $100058a;  { U+058A ARMENIAN HYPHEN }
  XK_Armenian_exclam          = $100055c;  { U+055C ARMENIAN EXCLAMATION MARK }
  XK_Armenian_amanak          = $100055c;  { U+055C ARMENIAN EXCLAMATION MARK }
  XK_Armenian_accent          = $100055b;  { U+055B ARMENIAN EMPHASIS MARK }
  XK_Armenian_shesht          = $100055b;  { U+055B ARMENIAN EMPHASIS MARK }
  XK_Armenian_question        = $100055e;  { U+055E ARMENIAN QUESTION MARK }
  XK_Armenian_paruyk          = $100055e;  { U+055E ARMENIAN QUESTION MARK }
  XKc_Armenian_AYB            = $1000531;  { U+0531 ARMENIAN CAPITAL LETTER AYB }
  XK_Armenian_ayb             = $1000561;  { U+0561 ARMENIAN SMALL LETTER AYB }
  XKc_Armenian_BEN            = $1000532;  { U+0532 ARMENIAN CAPITAL LETTER BEN }
  XK_Armenian_ben             = $1000562;  { U+0562 ARMENIAN SMALL LETTER BEN }
  XKc_Armenian_GIM            = $1000533;  { U+0533 ARMENIAN CAPITAL LETTER GIM }
  XK_Armenian_gim             = $1000563;  { U+0563 ARMENIAN SMALL LETTER GIM }
  XKc_Armenian_DA             = $1000534;  { U+0534 ARMENIAN CAPITAL LETTER DA }
  XK_Armenian_da              = $1000564;  { U+0564 ARMENIAN SMALL LETTER DA }
  XKc_Armenian_YECH           = $1000535;  { U+0535 ARMENIAN CAPITAL LETTER ECH }
  XK_Armenian_yech            = $1000565;  { U+0565 ARMENIAN SMALL LETTER ECH }
  XKc_Armenian_ZA             = $1000536;  { U+0536 ARMENIAN CAPITAL LETTER ZA }
  XK_Armenian_za              = $1000566;  { U+0566 ARMENIAN SMALL LETTER ZA }
  XKc_Armenian_E              = $1000537;  { U+0537 ARMENIAN CAPITAL LETTER EH }
  XK_Armenian_e               = $1000567;  { U+0567 ARMENIAN SMALL LETTER EH }
  XKc_Armenian_AT             = $1000538;  { U+0538 ARMENIAN CAPITAL LETTER ET }
  XK_Armenian_at              = $1000568;  { U+0568 ARMENIAN SMALL LETTER ET }
  XKc_Armenian_TO             = $1000539;  { U+0539 ARMENIAN CAPITAL LETTER TO }
  XK_Armenian_to              = $1000569;  { U+0569 ARMENIAN SMALL LETTER TO }
  XKc_Armenian_ZHE            = $100053a;  { U+053A ARMENIAN CAPITAL LETTER ZHE }
  XK_Armenian_zhe             = $100056a;  { U+056A ARMENIAN SMALL LETTER ZHE }
  XKc_Armenian_INI            = $100053b;  { U+053B ARMENIAN CAPITAL LETTER INI }
  XK_Armenian_ini             = $100056b;  { U+056B ARMENIAN SMALL LETTER INI }
  XKc_Armenian_LYUN           = $100053c;  { U+053C ARMENIAN CAPITAL LETTER LIWN }
  XK_Armenian_lyun            = $100056c;  { U+056C ARMENIAN SMALL LETTER LIWN }
  XKc_Armenian_KHE            = $100053d;  { U+053D ARMENIAN CAPITAL LETTER XEH }
  XK_Armenian_khe             = $100056d;  { U+056D ARMENIAN SMALL LETTER XEH }
  XKc_Armenian_TSA            = $100053e;  { U+053E ARMENIAN CAPITAL LETTER CA }
  XK_Armenian_tsa             = $100056e;  { U+056E ARMENIAN SMALL LETTER CA }
  XKc_Armenian_KEN            = $100053f;  { U+053F ARMENIAN CAPITAL LETTER KEN }
  XK_Armenian_ken             = $100056f;  { U+056F ARMENIAN SMALL LETTER KEN }
  XKc_Armenian_HO             = $1000540;  { U+0540 ARMENIAN CAPITAL LETTER HO }
  XK_Armenian_ho              = $1000570;  { U+0570 ARMENIAN SMALL LETTER HO }
  XKc_Armenian_DZA            = $1000541;  { U+0541 ARMENIAN CAPITAL LETTER JA }
  XK_Armenian_dza             = $1000571;  { U+0571 ARMENIAN SMALL LETTER JA }
  XKc_Armenian_GHAT           = $1000542;  { U+0542 ARMENIAN CAPITAL LETTER GHAD }
  XK_Armenian_ghat            = $1000572;  { U+0572 ARMENIAN SMALL LETTER GHAD }
  XKc_Armenian_TCHE           = $1000543;  { U+0543 ARMENIAN CAPITAL LETTER CHEH }
  XK_Armenian_tche            = $1000573;  { U+0573 ARMENIAN SMALL LETTER CHEH }
  XKc_Armenian_MEN            = $1000544;  { U+0544 ARMENIAN CAPITAL LETTER MEN }
  XK_Armenian_men             = $1000574;  { U+0574 ARMENIAN SMALL LETTER MEN }
  XKc_Armenian_HI             = $1000545;  { U+0545 ARMENIAN CAPITAL LETTER YI }
  XK_Armenian_hi              = $1000575;  { U+0575 ARMENIAN SMALL LETTER YI }
  XKc_Armenian_NU             = $1000546;  { U+0546 ARMENIAN CAPITAL LETTER NOW }
  XK_Armenian_nu              = $1000576;  { U+0576 ARMENIAN SMALL LETTER NOW }
  XKc_Armenian_SHA            = $1000547;  { U+0547 ARMENIAN CAPITAL LETTER SHA }
  XK_Armenian_sha             = $1000577;  { U+0577 ARMENIAN SMALL LETTER SHA }
  XKc_Armenian_VO             = $1000548;  { U+0548 ARMENIAN CAPITAL LETTER VO }
  XK_Armenian_vo              = $1000578;  { U+0578 ARMENIAN SMALL LETTER VO }
  XKc_Armenian_CHA            = $1000549;  { U+0549 ARMENIAN CAPITAL LETTER CHA }
  XK_Armenian_cha             = $1000579;  { U+0579 ARMENIAN SMALL LETTER CHA }
  XKc_Armenian_PE             = $100054a;  { U+054A ARMENIAN CAPITAL LETTER PEH }
  XK_Armenian_pe              = $100057a;  { U+057A ARMENIAN SMALL LETTER PEH }
  XKc_Armenian_JE             = $100054b;  { U+054B ARMENIAN CAPITAL LETTER JHEH }
  XK_Armenian_je              = $100057b;  { U+057B ARMENIAN SMALL LETTER JHEH }
  XKc_Armenian_RA             = $100054c;  { U+054C ARMENIAN CAPITAL LETTER RA }
  XK_Armenian_ra              = $100057c;  { U+057C ARMENIAN SMALL LETTER RA }
  XKc_Armenian_SE             = $100054d;  { U+054D ARMENIAN CAPITAL LETTER SEH }
  XK_Armenian_se              = $100057d;  { U+057D ARMENIAN SMALL LETTER SEH }
  XKc_Armenian_VEV            = $100054e;  { U+054E ARMENIAN CAPITAL LETTER VEW }
  XK_Armenian_vev             = $100057e;  { U+057E ARMENIAN SMALL LETTER VEW }
  XKc_Armenian_TYUN           = $100054f;  { U+054F ARMENIAN CAPITAL LETTER TIWN }
  XK_Armenian_tyun            = $100057f;  { U+057F ARMENIAN SMALL LETTER TIWN }
  XKc_Armenian_RE             = $1000550;  { U+0550 ARMENIAN CAPITAL LETTER REH }
  XK_Armenian_re              = $1000580;  { U+0580 ARMENIAN SMALL LETTER REH }
  XKc_Armenian_TSO            = $1000551;  { U+0551 ARMENIAN CAPITAL LETTER CO }
  XK_Armenian_tso             = $1000581;  { U+0581 ARMENIAN SMALL LETTER CO }
  XKc_Armenian_VYUN           = $1000552;  { U+0552 ARMENIAN CAPITAL LETTER YIWN }
  XK_Armenian_vyun            = $1000582;  { U+0582 ARMENIAN SMALL LETTER YIWN }
  XKc_Armenian_PYUR           = $1000553;  { U+0553 ARMENIAN CAPITAL LETTER PIWR }
  XK_Armenian_pyur            = $1000583;  { U+0583 ARMENIAN SMALL LETTER PIWR }
  XKc_Armenian_KE             = $1000554;  { U+0554 ARMENIAN CAPITAL LETTER KEH }
  XK_Armenian_ke              = $1000584;  { U+0584 ARMENIAN SMALL LETTER KEH }
  XKc_Armenian_O              = $1000555;  { U+0555 ARMENIAN CAPITAL LETTER OH }
  XK_Armenian_o               = $1000585;  { U+0585 ARMENIAN SMALL LETTER OH }
  XKc_Armenian_FE             = $1000556;  { U+0556 ARMENIAN CAPITAL LETTER FEH }
  XK_Armenian_fe              = $1000586;  { U+0586 ARMENIAN SMALL LETTER FEH }
  XK_Armenian_apostrophe      = $100055a;  { U+055A ARMENIAN APOSTROPHE }
{$ENDIF} { XK_ARMENIAN }

{*
 * Georgian
 *}

{$IFDEF XK_GEORGIAN}
  XK_Georgian_an   = $10010d0;  { U+10D0 GEORGIAN LETTER AN }
  XK_Georgian_ban  = $10010d1;  { U+10D1 GEORGIAN LETTER BAN }
  XK_Georgian_gan  = $10010d2;  { U+10D2 GEORGIAN LETTER GAN }
  XK_Georgian_don  = $10010d3;  { U+10D3 GEORGIAN LETTER DON }
  XK_Georgian_en   = $10010d4;  { U+10D4 GEORGIAN LETTER EN }
  XK_Georgian_vin  = $10010d5;  { U+10D5 GEORGIAN LETTER VIN }
  XK_Georgian_zen  = $10010d6;  { U+10D6 GEORGIAN LETTER ZEN }
  XK_Georgian_tan  = $10010d7;  { U+10D7 GEORGIAN LETTER TAN }
  XK_Georgian_in   = $10010d8;  { U+10D8 GEORGIAN LETTER IN }
  XK_Georgian_kan  = $10010d9;  { U+10D9 GEORGIAN LETTER KAN }
  XK_Georgian_las  = $10010da;  { U+10DA GEORGIAN LETTER LAS }
  XK_Georgian_man  = $10010db;  { U+10DB GEORGIAN LETTER MAN }
  XK_Georgian_nar  = $10010dc;  { U+10DC GEORGIAN LETTER NAR }
  XK_Georgian_on   = $10010dd;  { U+10DD GEORGIAN LETTER ON }
  XK_Georgian_par  = $10010de;  { U+10DE GEORGIAN LETTER PAR }
  XK_Georgian_zhar = $10010df;  { U+10DF GEORGIAN LETTER ZHAR }
  XK_Georgian_rae  = $10010e0;  { U+10E0 GEORGIAN LETTER RAE }
  XK_Georgian_san  = $10010e1;  { U+10E1 GEORGIAN LETTER SAN }
  XK_Georgian_tar  = $10010e2;  { U+10E2 GEORGIAN LETTER TAR }
  XK_Georgian_un   = $10010e3;  { U+10E3 GEORGIAN LETTER UN }
  XK_Georgian_phar = $10010e4;  { U+10E4 GEORGIAN LETTER PHAR }
  XK_Georgian_khar = $10010e5;  { U+10E5 GEORGIAN LETTER KHAR }
  XK_Georgian_ghan = $10010e6;  { U+10E6 GEORGIAN LETTER GHAN }
  XK_Georgian_qar  = $10010e7;  { U+10E7 GEORGIAN LETTER QAR }
  XK_Georgian_shin = $10010e8;  { U+10E8 GEORGIAN LETTER SHIN }
  XK_Georgian_chin = $10010e9;  { U+10E9 GEORGIAN LETTER CHIN }
  XK_Georgian_can  = $10010ea;  { U+10EA GEORGIAN LETTER CAN }
  XK_Georgian_jil  = $10010eb;  { U+10EB GEORGIAN LETTER JIL }
  XK_Georgian_cil  = $10010ec;  { U+10EC GEORGIAN LETTER CIL }
  XK_Georgian_char = $10010ed;  { U+10ED GEORGIAN LETTER AnsiChar }
  XK_Georgian_xan  = $10010ee;  { U+10EE GEORGIAN LETTER XAN }
  XK_Georgian_jhan = $10010ef;  { U+10EF GEORGIAN LETTER JHAN }
  XK_Georgian_hae  = $10010f0;  { U+10F0 GEORGIAN LETTER HAE }
  XK_Georgian_he   = $10010f1;  { U+10F1 GEORGIAN LETTER HE }
  XK_Georgian_hie  = $10010f2;  { U+10F2 GEORGIAN LETTER HIE }
  XK_Georgian_we   = $10010f3;  { U+10F3 GEORGIAN LETTER WE }
  XK_Georgian_har  = $10010f4;  { U+10F4 GEORGIAN LETTER HAR }
  XK_Georgian_hoe  = $10010f5;  { U+10F5 GEORGIAN LETTER HOE }
  XK_Georgian_fi   = $10010f6;  { U+10F6 GEORGIAN LETTER FI }
{$ENDIF} { XK_GEORGIAN }

{*
 * Azeri (and other Turkic or Caucasian languages)
 *}

{$IFDEF XK_CAUCASUS}
{ latin }
  XKc_Xabovedot = $1001e8a;  { U+1E8A LATIN CAPITAL LETTER X WITH DOT ABOVE }
  XKc_Ibreve    = $100012c;  { U+012C LATIN CAPITAL LETTER I WITH BREVE }
  XKc_Zstroke   = $10001b5;  { U+01B5 LATIN CAPITAL LETTER Z WITH STROKE }
  XKc_Gcaron    = $10001e6;  { U+01E6 LATIN CAPITAL LETTER G WITH CARON }
  XKc_Ocaron    = $10001d1;  { U+01D2 LATIN CAPITAL LETTER O WITH CARON }
  XKc_Obarred   = $100019f;  { U+019F LATIN CAPITAL LETTER O WITH MIDDLE TILDE }
  XK_xabovedot  = $1001e8b;  { U+1E8B LATIN SMALL LETTER X WITH DOT ABOVE }
  XK_ibreve     = $100012d;  { U+012D LATIN SMALL LETTER I WITH BREVE }
  XK_zstroke    = $10001b6;  { U+01B6 LATIN SMALL LETTER Z WITH STROKE }
  XK_gcaron     = $10001e7;  { U+01E7 LATIN SMALL LETTER G WITH CARON }
  XK_ocaron     = $10001d2;  { U+01D2 LATIN SMALL LETTER O WITH CARON }
  XK_obarred    = $1000275;  { U+0275 LATIN SMALL LETTER BARRED O }
  XKc_SCHWA     = $100018f;  { U+018F LATIN CAPITAL LETTER SCHWA }
  XK_schwa      = $1000259;  { U+0259 LATIN SMALL LETTER SCHWA }
  XKc_EZH       = $10001b7;  { U+01B7 LATIN CAPITAL LETTER EZH }
  XK_ezh        = $1000292;  { U+0292 LATIN SMALL LETTER EZH }
{ those are not really Caucasus }
{ For Inupiak }
  XKc_Lbelowdot = $1001e36;  { U+1E36 LATIN CAPITAL LETTER L WITH DOT BELOW }
  XK_lbelowdot  = $1001e37;  { U+1E37 LATIN SMALL LETTER L WITH DOT BELOW }
{$ENDIF} { XK_CAUCASUS }

{*
 * Vietnamese
 *}

{$IFDEF XK_VIETNAMESE}
  XKc_Abelowdot           = $1001ea0;  { U+1EA0 LATIN CAPITAL LETTER A WITH DOT BELOW }
  XK_abelowdot            = $1001ea1;  { U+1EA1 LATIN SMALL LETTER A WITH DOT BELOW }
  XKc_Ahook               = $1001ea2;  { U+1EA2 LATIN CAPITAL LETTER A WITH HOOK ABOVE }
  XK_ahook                = $1001ea3;  { U+1EA3 LATIN SMALL LETTER A WITH HOOK ABOVE }
  XKc_Acircumflexacute    = $1001ea4;  { U+1EA4 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND ACUTE }
  XK_acircumflexacute     = $1001ea5;  { U+1EA5 LATIN SMALL LETTER A WITH CIRCUMFLEX AND ACUTE }
  XKc_Acircumflexgrave    = $1001ea6;  { U+1EA6 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND GRAVE }
  XK_acircumflexgrave     = $1001ea7;  { U+1EA7 LATIN SMALL LETTER A WITH CIRCUMFLEX AND GRAVE }
  XKc_Acircumflexhook     = $1001ea8;  { U+1EA8 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE }
  XK_acircumflexhook      = $1001ea9;  { U+1EA9 LATIN SMALL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE }
  XKc_Acircumflextilde    = $1001eaa;  { U+1EAA LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND TILDE }
  XK_acircumflextilde     = $1001eab;  { U+1EAB LATIN SMALL LETTER A WITH CIRCUMFLEX AND TILDE }
  XKc_Acircumflexbelowdot = $1001eac;  { U+1EAC LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND DOT BELOW }
  XK_acircumflexbelowdot  = $1001ead;  { U+1EAD LATIN SMALL LETTER A WITH CIRCUMFLEX AND DOT BELOW }
  XKc_Abreveacute         = $1001eae;  { U+1EAE LATIN CAPITAL LETTER A WITH BREVE AND ACUTE }
  XK_abreveacute          = $1001eaf;  { U+1EAF LATIN SMALL LETTER A WITH BREVE AND ACUTE }
  XKc_Abrevegrave         = $1001eb0;  { U+1EB0 LATIN CAPITAL LETTER A WITH BREVE AND GRAVE }
  XK_abrevegrave          = $1001eb1;  { U+1EB1 LATIN SMALL LETTER A WITH BREVE AND GRAVE }
  XKc_Abrevehook          = $1001eb2;  { U+1EB2 LATIN CAPITAL LETTER A WITH BREVE AND HOOK ABOVE }
  XK_abrevehook           = $1001eb3;  { U+1EB3 LATIN SMALL LETTER A WITH BREVE AND HOOK ABOVE }
  XKc_Abrevetilde         = $1001eb4;  { U+1EB4 LATIN CAPITAL LETTER A WITH BREVE AND TILDE }
  XK_abrevetilde          = $1001eb5;  { U+1EB5 LATIN SMALL LETTER A WITH BREVE AND TILDE }
  XKc_Abrevebelowdot      = $1001eb6;  { U+1EB6 LATIN CAPITAL LETTER A WITH BREVE AND DOT BELOW }
  XK_abrevebelowdot       = $1001eb7;  { U+1EB7 LATIN SMALL LETTER A WITH BREVE AND DOT BELOW }
  XKc_Ebelowdot           = $1001eb8;  { U+1EB8 LATIN CAPITAL LETTER E WITH DOT BELOW }
  XK_ebelowdot            = $1001eb9;  { U+1EB9 LATIN SMALL LETTER E WITH DOT BELOW }
  XKc_Ehook               = $1001eba;  { U+1EBA LATIN CAPITAL LETTER E WITH HOOK ABOVE }
  XK_ehook                = $1001ebb;  { U+1EBB LATIN SMALL LETTER E WITH HOOK ABOVE }
  XKc_Etilde              = $1001ebc;  { U+1EBC LATIN CAPITAL LETTER E WITH TILDE }
  XK_etilde               = $1001ebd;  { U+1EBD LATIN SMALL LETTER E WITH TILDE }
  XKc_Ecircumflexacute    = $1001ebe;  { U+1EBE LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND ACUTE }
  XK_ecircumflexacute     = $1001ebf;  { U+1EBF LATIN SMALL LETTER E WITH CIRCUMFLEX AND ACUTE }
  XKc_Ecircumflexgrave    = $1001ec0;  { U+1EC0 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND GRAVE }
  XK_ecircumflexgrave     = $1001ec1;  { U+1EC1 LATIN SMALL LETTER E WITH CIRCUMFLEX AND GRAVE }
  XKc_Ecircumflexhook     = $1001ec2;  { U+1EC2 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE }
  XK_ecircumflexhook      = $1001ec3;  { U+1EC3 LATIN SMALL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE }
  XKc_Ecircumflextilde    = $1001ec4;  { U+1EC4 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE }
  XK_ecircumflextilde     = $1001ec5;  { U+1EC5 LATIN SMALL LETTER E WITH CIRCUMFLEX AND TILDE }
  XKc_Ecircumflexbelowdot = $1001ec6;  { U+1EC6 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND DOT BELOW }
  XK_ecircumflexbelowdot  = $1001ec7;  { U+1EC7 LATIN SMALL LETTER E WITH CIRCUMFLEX AND DOT BELOW }
  XKc_Ihook               = $1001ec8;  { U+1EC8 LATIN CAPITAL LETTER I WITH HOOK ABOVE }
  XK_ihook                = $1001ec9;  { U+1EC9 LATIN SMALL LETTER I WITH HOOK ABOVE }
  XKc_Ibelowdot           = $1001eca;  { U+1ECA LATIN CAPITAL LETTER I WITH DOT BELOW }
  XK_ibelowdot            = $1001ecb;  { U+1ECB LATIN SMALL LETTER I WITH DOT BELOW }
  XKc_Obelowdot           = $1001ecc;  { U+1ECC LATIN CAPITAL LETTER O WITH DOT BELOW }
  XK_obelowdot            = $1001ecd;  { U+1ECD LATIN SMALL LETTER O WITH DOT BELOW }
  XKc_Ohook               = $1001ece;  { U+1ECE LATIN CAPITAL LETTER O WITH HOOK ABOVE }
  XK_ohook                = $1001ecf;  { U+1ECF LATIN SMALL LETTER O WITH HOOK ABOVE }
  XKc_Ocircumflexacute    = $1001ed0;  { U+1ED0 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND ACUTE }
  XK_ocircumflexacute     = $1001ed1;  { U+1ED1 LATIN SMALL LETTER O WITH CIRCUMFLEX AND ACUTE }
  XKc_Ocircumflexgrave    = $1001ed2;  { U+1ED2 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND GRAVE }
  XK_ocircumflexgrave     = $1001ed3;  { U+1ED3 LATIN SMALL LETTER O WITH CIRCUMFLEX AND GRAVE }
  XKc_Ocircumflexhook     = $1001ed4;  { U+1ED4 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE }
  XK_ocircumflexhook      = $1001ed5;  { U+1ED5 LATIN SMALL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE }
  XKc_Ocircumflextilde    = $1001ed6;  { U+1ED6 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND TILDE }
  XK_ocircumflextilde     = $1001ed7;  { U+1ED7 LATIN SMALL LETTER O WITH CIRCUMFLEX AND TILDE }
  XKc_Ocircumflexbelowdot = $1001ed8;  { U+1ED8 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND DOT BELOW }
  XK_ocircumflexbelowdot  = $1001ed9;  { U+1ED9 LATIN SMALL LETTER O WITH CIRCUMFLEX AND DOT BELOW }
  XKc_Ohornacute          = $1001eda;  { U+1EDA LATIN CAPITAL LETTER O WITH HORN AND ACUTE }
  XK_ohornacute           = $1001edb;  { U+1EDB LATIN SMALL LETTER O WITH HORN AND ACUTE }
  XKc_Ohorngrave          = $1001edc;  { U+1EDC LATIN CAPITAL LETTER O WITH HORN AND GRAVE }
  XK_ohorngrave           = $1001edd;  { U+1EDD LATIN SMALL LETTER O WITH HORN AND GRAVE }
  XKc_Ohornhook           = $1001ede;  { U+1EDE LATIN CAPITAL LETTER O WITH HORN AND HOOK ABOVE }
  XK_ohornhook            = $1001edf;  { U+1EDF LATIN SMALL LETTER O WITH HORN AND HOOK ABOVE }
  XKc_Ohorntilde          = $1001ee0;  { U+1EE0 LATIN CAPITAL LETTER O WITH HORN AND TILDE }
  XK_ohorntilde           = $1001ee1;  { U+1EE1 LATIN SMALL LETTER O WITH HORN AND TILDE }
  XKc_Ohornbelowdot       = $1001ee2;  { U+1EE2 LATIN CAPITAL LETTER O WITH HORN AND DOT BELOW }
  XK_ohornbelowdot        = $1001ee3;  { U+1EE3 LATIN SMALL LETTER O WITH HORN AND DOT BELOW }
  XKc_Ubelowdot           = $1001ee4;  { U+1EE4 LATIN CAPITAL LETTER U WITH DOT BELOW }
  XK_ubelowdot            = $1001ee5;  { U+1EE5 LATIN SMALL LETTER U WITH DOT BELOW }
  XKc_Uhook               = $1001ee6;  { U+1EE6 LATIN CAPITAL LETTER U WITH HOOK ABOVE }
  XK_uhook                = $1001ee7;  { U+1EE7 LATIN SMALL LETTER U WITH HOOK ABOVE }
  XKc_Uhornacute          = $1001ee8;  { U+1EE8 LATIN CAPITAL LETTER U WITH HORN AND ACUTE }
  XK_uhornacute           = $1001ee9;  { U+1EE9 LATIN SMALL LETTER U WITH HORN AND ACUTE }
  XKc_Uhorngrave          = $1001eea;  { U+1EEA LATIN CAPITAL LETTER U WITH HORN AND GRAVE }
  XK_uhorngrave           = $1001eeb;  { U+1EEB LATIN SMALL LETTER U WITH HORN AND GRAVE }
  XKc_Uhornhook           = $1001eec;  { U+1EEC LATIN CAPITAL LETTER U WITH HORN AND HOOK ABOVE }
  XK_uhornhook            = $1001eed;  { U+1EED LATIN SMALL LETTER U WITH HORN AND HOOK ABOVE }
  XKc_Uhorntilde          = $1001eee;  { U+1EEE LATIN CAPITAL LETTER U WITH HORN AND TILDE }
  XK_uhorntilde           = $1001eef;  { U+1EEF LATIN SMALL LETTER U WITH HORN AND TILDE }
  XKc_Uhornbelowdot       = $1001ef0;  { U+1EF0 LATIN CAPITAL LETTER U WITH HORN AND DOT BELOW }
  XK_uhornbelowdot        = $1001ef1;  { U+1EF1 LATIN SMALL LETTER U WITH HORN AND DOT BELOW }
  XKc_Ybelowdot           = $1001ef4;  { U+1EF4 LATIN CAPITAL LETTER Y WITH DOT BELOW }
  XK_ybelowdot            = $1001ef5;  { U+1EF5 LATIN SMALL LETTER Y WITH DOT BELOW }
  XKc_Yhook               = $1001ef6;  { U+1EF6 LATIN CAPITAL LETTER Y WITH HOOK ABOVE }
  XK_yhook                = $1001ef7;  { U+1EF7 LATIN SMALL LETTER Y WITH HOOK ABOVE }
  XKc_Ytilde              = $1001ef8;  { U+1EF8 LATIN CAPITAL LETTER Y WITH TILDE }
  XK_ytilde               = $1001ef9;  { U+1EF9 LATIN SMALL LETTER Y WITH TILDE }
  XKc_Ohorn               = $10001a0;  { U+01A0 LATIN CAPITAL LETTER O WITH HORN }
  XK_ohorn                = $10001a1;  { U+01A1 LATIN SMALL LETTER O WITH HORN }
  XKc_Uhorn               = $10001af;  { U+01AF LATIN CAPITAL LETTER U WITH HORN }
  XK_uhorn                = $10001b0;  { U+01B0 LATIN SMALL LETTER U WITH HORN }

{$ENDIF} { XK_VIETNAMESE }

{$IFDEF XK_CURRENCY}
  XK_EcuSign       = $10020a0;  { U+20A0 EURO-CURRENCY SIGN }
  XK_ColonSign     = $10020a1;  { U+20A1 COLON SIGN }
  XK_CruzeiroSign  = $10020a2;  { U+20A2 CRUZEIRO SIGN }
  XK_FFrancSign    = $10020a3;  { U+20A3 FRENCH FRANC SIGN }
  XK_LiraSign      = $10020a4;  { U+20A4 LIRA SIGN }
  XK_MillSign      = $10020a5;  { U+20A5 MILL SIGN }
  XK_NairaSign     = $10020a6;  { U+20A6 NAIRA SIGN }
  XK_PesetaSign    = $10020a7;  { U+20A7 PESETA SIGN }
  XK_RupeeSign     = $10020a8;  { U+20A8 RUPEE SIGN }
  XK_WonSign       = $10020a9;  { U+20A9 WON SIGN }
  XK_NewSheqelSign = $10020aa;  { U+20AA NEW SHEQEL SIGN }
  XK_DongSign      = $10020ab;  { U+20AB DONG SIGN }
  XK_EuroSign      = $20ac;     { U+20AC EURO SIGN }
{$ENDIF}

{$IFDEF XK_MATHEMATICAL}
{ one, two and three are defined above. }
  XK_zerosuperior     = $1002070;  { U+2070 SUPERSCRIPT ZERO }
  XK_foursuperior     = $1002074;  { U+2074 SUPERSCRIPT FOUR }
  XK_fivesuperior     = $1002075;  { U+2075 SUPERSCRIPT FIVE }
  XK_sixsuperior      = $1002076;  { U+2076 SUPERSCRIPT SIX }
  XK_sevensuperior    = $1002077;  { U+2077 SUPERSCRIPT SEVEN }
  XK_eightsuperior    = $1002078;  { U+2078 SUPERSCRIPT EIGHT }
  XK_ninesuperior     = $1002079;  { U+2079 SUPERSCRIPT NINE }
  XK_zerosubscript    = $1002080;  { U+2080 SUBSCRIPT ZERO }
  XK_onesubscript     = $1002081;  { U+2081 SUBSCRIPT ONE }
  XK_twosubscript     = $1002082;  { U+2082 SUBSCRIPT TWO }
  XK_threesubscript   = $1002083;  { U+2083 SUBSCRIPT THREE }
  XK_foursubscript    = $1002084;  { U+2084 SUBSCRIPT FOUR }
  XK_fivesubscript    = $1002085;  { U+2085 SUBSCRIPT FIVE }
  XK_sixsubscript     = $1002086;  { U+2086 SUBSCRIPT SIX }
  XK_sevensubscript   = $1002087;  { U+2087 SUBSCRIPT SEVEN }
  XK_eightsubscript   = $1002088;  { U+2088 SUBSCRIPT EIGHT }
  XK_ninesubscript    = $1002089;  { U+2089 SUBSCRIPT NINE }
  XK_partdifferential = $1002202;  { U+2202 PARTIAL DIFFERENTIAL }
  XK_emptyset         = $1002205;  { U+2205 NULL SET }
  XK_elementof        = $1002208;  { U+2208 ELEMENT OF }
  XK_notelementof     = $1002209;  { U+2209 NOT AN ELEMENT OF }
  XK_containsas       = $100220B;  { U+220B CONTAINS AS MEMBER }
  XK_squareroot       = $100221A;  { U+221A SQUARE ROOT }
  XK_cuberoot         = $100221B;  { U+221B CUBE ROOT }
  XK_fourthroot       = $100221C;  { U+221C FOURTH ROOT }
  XK_dintegral        = $100222C;  { U+222C DOUBLE INTEGRAL }
  XK_tintegral        = $100222D;  { U+222D TRIPLE INTEGRAL }
  XK_because          = $1002235;  { U+2235 BECAUSE }
  XK_approxeq         = $1002248;  { U+2245 ALMOST EQUAL TO }
  XK_notapproxeq      = $1002247;  { U+2247 NOT ALMOST EQUAL TO }
  XK_notidentical     = $1002262;  { U+2262 NOT IDENTICAL TO }
  XK_stricteq         = $1002263;  { U+2263 STRICTLY EQUIVALENT TO }
{$ENDIF} { XK_MATHEMATICAL }

{$IFDEF XK_BRAILLE}
  XK_braille_dot_1         = $fff1;
  XK_braille_dot_2         = $fff2;
  XK_braille_dot_3         = $fff3;
  XK_braille_dot_4         = $fff4;
  XK_braille_dot_5         = $fff5;
  XK_braille_dot_6         = $fff6;
  XK_braille_dot_7         = $fff7;
  XK_braille_dot_8         = $fff8;
  XK_braille_dot_9         = $fff9;
  XK_braille_dot_10        = $fffa;
  XK_braille_blank         = $1002800;  { U+2800 BRAILLE PATTERN BLANK }
  XK_braille_dots_1        = $1002801;  { U+2801 BRAILLE PATTERN DOTS-1 }
  XK_braille_dots_2        = $1002802;  { U+2802 BRAILLE PATTERN DOTS-2 }
  XK_braille_dots_12       = $1002803;  { U+2803 BRAILLE PATTERN DOTS-12 }
  XK_braille_dots_3        = $1002804;  { U+2804 BRAILLE PATTERN DOTS-3 }
  XK_braille_dots_13       = $1002805;  { U+2805 BRAILLE PATTERN DOTS-13 }
  XK_braille_dots_23       = $1002806;  { U+2806 BRAILLE PATTERN DOTS-23 }
  XK_braille_dots_123      = $1002807;  { U+2807 BRAILLE PATTERN DOTS-123 }
  XK_braille_dots_4        = $1002808;  { U+2808 BRAILLE PATTERN DOTS-4 }
  XK_braille_dots_14       = $1002809;  { U+2809 BRAILLE PATTERN DOTS-14 }
  XK_braille_dots_24       = $100280a;  { U+280a BRAILLE PATTERN DOTS-24 }
  XK_braille_dots_124      = $100280b;  { U+280b BRAILLE PATTERN DOTS-124 }
  XK_braille_dots_34       = $100280c;  { U+280c BRAILLE PATTERN DOTS-34 }
  XK_braille_dots_134      = $100280d;  { U+280d BRAILLE PATTERN DOTS-134 }
  XK_braille_dots_234      = $100280e;  { U+280e BRAILLE PATTERN DOTS-234 }
  XK_braille_dots_1234     = $100280f;  { U+280f BRAILLE PATTERN DOTS-1234 }
  XK_braille_dots_5        = $1002810;  { U+2810 BRAILLE PATTERN DOTS-5 }
  XK_braille_dots_15       = $1002811;  { U+2811 BRAILLE PATTERN DOTS-15 }
  XK_braille_dots_25       = $1002812;  { U+2812 BRAILLE PATTERN DOTS-25 }
  XK_braille_dots_125      = $1002813;  { U+2813 BRAILLE PATTERN DOTS-125 }
  XK_braille_dots_35       = $1002814;  { U+2814 BRAILLE PATTERN DOTS-35 }
  XK_braille_dots_135      = $1002815;  { U+2815 BRAILLE PATTERN DOTS-135 }
  XK_braille_dots_235      = $1002816;  { U+2816 BRAILLE PATTERN DOTS-235 }
  XK_braille_dots_1235     = $1002817;  { U+2817 BRAILLE PATTERN DOTS-1235 }
  XK_braille_dots_45       = $1002818;  { U+2818 BRAILLE PATTERN DOTS-45 }
  XK_braille_dots_145      = $1002819;  { U+2819 BRAILLE PATTERN DOTS-145 }
  XK_braille_dots_245      = $100281a;  { U+281a BRAILLE PATTERN DOTS-245 }
  XK_braille_dots_1245     = $100281b;  { U+281b BRAILLE PATTERN DOTS-1245 }
  XK_braille_dots_345      = $100281c;  { U+281c BRAILLE PATTERN DOTS-345 }
  XK_braille_dots_1345     = $100281d;  { U+281d BRAILLE PATTERN DOTS-1345 }
  XK_braille_dots_2345     = $100281e;  { U+281e BRAILLE PATTERN DOTS-2345 }
  XK_braille_dots_12345    = $100281f;  { U+281f BRAILLE PATTERN DOTS-12345 }
  XK_braille_dots_6        = $1002820;  { U+2820 BRAILLE PATTERN DOTS-6 }
  XK_braille_dots_16       = $1002821;  { U+2821 BRAILLE PATTERN DOTS-16 }
  XK_braille_dots_26       = $1002822;  { U+2822 BRAILLE PATTERN DOTS-26 }
  XK_braille_dots_126      = $1002823;  { U+2823 BRAILLE PATTERN DOTS-126 }
  XK_braille_dots_36       = $1002824;  { U+2824 BRAILLE PATTERN DOTS-36 }
  XK_braille_dots_136      = $1002825;  { U+2825 BRAILLE PATTERN DOTS-136 }
  XK_braille_dots_236      = $1002826;  { U+2826 BRAILLE PATTERN DOTS-236 }
  XK_braille_dots_1236     = $1002827;  { U+2827 BRAILLE PATTERN DOTS-1236 }
  XK_braille_dots_46       = $1002828;  { U+2828 BRAILLE PATTERN DOTS-46 }
  XK_braille_dots_146      = $1002829;  { U+2829 BRAILLE PATTERN DOTS-146 }
  XK_braille_dots_246      = $100282a;  { U+282a BRAILLE PATTERN DOTS-246 }
  XK_braille_dots_1246     = $100282b;  { U+282b BRAILLE PATTERN DOTS-1246 }
  XK_braille_dots_346      = $100282c;  { U+282c BRAILLE PATTERN DOTS-346 }
  XK_braille_dots_1346     = $100282d;  { U+282d BRAILLE PATTERN DOTS-1346 }
  XK_braille_dots_2346     = $100282e;  { U+282e BRAILLE PATTERN DOTS-2346 }
  XK_braille_dots_12346    = $100282f;  { U+282f BRAILLE PATTERN DOTS-12346 }
  XK_braille_dots_56       = $1002830;  { U+2830 BRAILLE PATTERN DOTS-56 }
  XK_braille_dots_156      = $1002831;  { U+2831 BRAILLE PATTERN DOTS-156 }
  XK_braille_dots_256      = $1002832;  { U+2832 BRAILLE PATTERN DOTS-256 }
  XK_braille_dots_1256     = $1002833;  { U+2833 BRAILLE PATTERN DOTS-1256 }
  XK_braille_dots_356      = $1002834;  { U+2834 BRAILLE PATTERN DOTS-356 }
  XK_braille_dots_1356     = $1002835;  { U+2835 BRAILLE PATTERN DOTS-1356 }
  XK_braille_dots_2356     = $1002836;  { U+2836 BRAILLE PATTERN DOTS-2356 }
  XK_braille_dots_12356    = $1002837;  { U+2837 BRAILLE PATTERN DOTS-12356 }
  XK_braille_dots_456      = $1002838;  { U+2838 BRAILLE PATTERN DOTS-456 }
  XK_braille_dots_1456     = $1002839;  { U+2839 BRAILLE PATTERN DOTS-1456 }
  XK_braille_dots_2456     = $100283a;  { U+283a BRAILLE PATTERN DOTS-2456 }
  XK_braille_dots_12456    = $100283b;  { U+283b BRAILLE PATTERN DOTS-12456 }
  XK_braille_dots_3456     = $100283c;  { U+283c BRAILLE PATTERN DOTS-3456 }
  XK_braille_dots_13456    = $100283d;  { U+283d BRAILLE PATTERN DOTS-13456 }
  XK_braille_dots_23456    = $100283e;  { U+283e BRAILLE PATTERN DOTS-23456 }
  XK_braille_dots_123456   = $100283f;  { U+283f BRAILLE PATTERN DOTS-123456 }
  XK_braille_dots_7        = $1002840;  { U+2840 BRAILLE PATTERN DOTS-7 }
  XK_braille_dots_17       = $1002841;  { U+2841 BRAILLE PATTERN DOTS-17 }
  XK_braille_dots_27       = $1002842;  { U+2842 BRAILLE PATTERN DOTS-27 }
  XK_braille_dots_127      = $1002843;  { U+2843 BRAILLE PATTERN DOTS-127 }
  XK_braille_dots_37       = $1002844;  { U+2844 BRAILLE PATTERN DOTS-37 }
  XK_braille_dots_137      = $1002845;  { U+2845 BRAILLE PATTERN DOTS-137 }
  XK_braille_dots_237      = $1002846;  { U+2846 BRAILLE PATTERN DOTS-237 }
  XK_braille_dots_1237     = $1002847;  { U+2847 BRAILLE PATTERN DOTS-1237 }
  XK_braille_dots_47       = $1002848;  { U+2848 BRAILLE PATTERN DOTS-47 }
  XK_braille_dots_147      = $1002849;  { U+2849 BRAILLE PATTERN DOTS-147 }
  XK_braille_dots_247      = $100284a;  { U+284a BRAILLE PATTERN DOTS-247 }
  XK_braille_dots_1247     = $100284b;  { U+284b BRAILLE PATTERN DOTS-1247 }
  XK_braille_dots_347      = $100284c;  { U+284c BRAILLE PATTERN DOTS-347 }
  XK_braille_dots_1347     = $100284d;  { U+284d BRAILLE PATTERN DOTS-1347 }
  XK_braille_dots_2347     = $100284e;  { U+284e BRAILLE PATTERN DOTS-2347 }
  XK_braille_dots_12347    = $100284f;  { U+284f BRAILLE PATTERN DOTS-12347 }
  XK_braille_dots_57       = $1002850;  { U+2850 BRAILLE PATTERN DOTS-57 }
  XK_braille_dots_157      = $1002851;  { U+2851 BRAILLE PATTERN DOTS-157 }
  XK_braille_dots_257      = $1002852;  { U+2852 BRAILLE PATTERN DOTS-257 }
  XK_braille_dots_1257     = $1002853;  { U+2853 BRAILLE PATTERN DOTS-1257 }
  XK_braille_dots_357      = $1002854;  { U+2854 BRAILLE PATTERN DOTS-357 }
  XK_braille_dots_1357     = $1002855;  { U+2855 BRAILLE PATTERN DOTS-1357 }
  XK_braille_dots_2357     = $1002856;  { U+2856 BRAILLE PATTERN DOTS-2357 }
  XK_braille_dots_12357    = $1002857;  { U+2857 BRAILLE PATTERN DOTS-12357 }
  XK_braille_dots_457      = $1002858;  { U+2858 BRAILLE PATTERN DOTS-457 }
  XK_braille_dots_1457     = $1002859;  { U+2859 BRAILLE PATTERN DOTS-1457 }
  XK_braille_dots_2457     = $100285a;  { U+285a BRAILLE PATTERN DOTS-2457 }
  XK_braille_dots_12457    = $100285b;  { U+285b BRAILLE PATTERN DOTS-12457 }
  XK_braille_dots_3457     = $100285c;  { U+285c BRAILLE PATTERN DOTS-3457 }
  XK_braille_dots_13457    = $100285d;  { U+285d BRAILLE PATTERN DOTS-13457 }
  XK_braille_dots_23457    = $100285e;  { U+285e BRAILLE PATTERN DOTS-23457 }
  XK_braille_dots_123457   = $100285f;  { U+285f BRAILLE PATTERN DOTS-123457 }
  XK_braille_dots_67       = $1002860;  { U+2860 BRAILLE PATTERN DOTS-67 }
  XK_braille_dots_167      = $1002861;  { U+2861 BRAILLE PATTERN DOTS-167 }
  XK_braille_dots_267      = $1002862;  { U+2862 BRAILLE PATTERN DOTS-267 }
  XK_braille_dots_1267     = $1002863;  { U+2863 BRAILLE PATTERN DOTS-1267 }
  XK_braille_dots_367      = $1002864;  { U+2864 BRAILLE PATTERN DOTS-367 }
  XK_braille_dots_1367     = $1002865;  { U+2865 BRAILLE PATTERN DOTS-1367 }
  XK_braille_dots_2367     = $1002866;  { U+2866 BRAILLE PATTERN DOTS-2367 }
  XK_braille_dots_12367    = $1002867;  { U+2867 BRAILLE PATTERN DOTS-12367 }
  XK_braille_dots_467      = $1002868;  { U+2868 BRAILLE PATTERN DOTS-467 }
  XK_braille_dots_1467     = $1002869;  { U+2869 BRAILLE PATTERN DOTS-1467 }
  XK_braille_dots_2467     = $100286a;  { U+286a BRAILLE PATTERN DOTS-2467 }
  XK_braille_dots_12467    = $100286b;  { U+286b BRAILLE PATTERN DOTS-12467 }
  XK_braille_dots_3467     = $100286c;  { U+286c BRAILLE PATTERN DOTS-3467 }
  XK_braille_dots_13467    = $100286d;  { U+286d BRAILLE PATTERN DOTS-13467 }
  XK_braille_dots_23467    = $100286e;  { U+286e BRAILLE PATTERN DOTS-23467 }
  XK_braille_dots_123467   = $100286f;  { U+286f BRAILLE PATTERN DOTS-123467 }
  XK_braille_dots_567      = $1002870;  { U+2870 BRAILLE PATTERN DOTS-567 }
  XK_braille_dots_1567     = $1002871;  { U+2871 BRAILLE PATTERN DOTS-1567 }
  XK_braille_dots_2567     = $1002872;  { U+2872 BRAILLE PATTERN DOTS-2567 }
  XK_braille_dots_12567    = $1002873;  { U+2873 BRAILLE PATTERN DOTS-12567 }
  XK_braille_dots_3567     = $1002874;  { U+2874 BRAILLE PATTERN DOTS-3567 }
  XK_braille_dots_13567    = $1002875;  { U+2875 BRAILLE PATTERN DOTS-13567 }
  XK_braille_dots_23567    = $1002876;  { U+2876 BRAILLE PATTERN DOTS-23567 }
  XK_braille_dots_123567   = $1002877;  { U+2877 BRAILLE PATTERN DOTS-123567 }
  XK_braille_dots_4567     = $1002878;  { U+2878 BRAILLE PATTERN DOTS-4567 }
  XK_braille_dots_14567    = $1002879;  { U+2879 BRAILLE PATTERN DOTS-14567 }
  XK_braille_dots_24567    = $100287a;  { U+287a BRAILLE PATTERN DOTS-24567 }
  XK_braille_dots_124567   = $100287b;  { U+287b BRAILLE PATTERN DOTS-124567 }
  XK_braille_dots_34567    = $100287c;  { U+287c BRAILLE PATTERN DOTS-34567 }
  XK_braille_dots_134567   = $100287d;  { U+287d BRAILLE PATTERN DOTS-134567 }
  XK_braille_dots_234567   = $100287e;  { U+287e BRAILLE PATTERN DOTS-234567 }
  XK_braille_dots_1234567  = $100287f;  { U+287f BRAILLE PATTERN DOTS-1234567 }
  XK_braille_dots_8        = $1002880;  { U+2880 BRAILLE PATTERN DOTS-8 }
  XK_braille_dots_18       = $1002881;  { U+2881 BRAILLE PATTERN DOTS-18 }
  XK_braille_dots_28       = $1002882;  { U+2882 BRAILLE PATTERN DOTS-28 }
  XK_braille_dots_128      = $1002883;  { U+2883 BRAILLE PATTERN DOTS-128 }
  XK_braille_dots_38       = $1002884;  { U+2884 BRAILLE PATTERN DOTS-38 }
  XK_braille_dots_138      = $1002885;  { U+2885 BRAILLE PATTERN DOTS-138 }
  XK_braille_dots_238      = $1002886;  { U+2886 BRAILLE PATTERN DOTS-238 }
  XK_braille_dots_1238     = $1002887;  { U+2887 BRAILLE PATTERN DOTS-1238 }
  XK_braille_dots_48       = $1002888;  { U+2888 BRAILLE PATTERN DOTS-48 }
  XK_braille_dots_148      = $1002889;  { U+2889 BRAILLE PATTERN DOTS-148 }
  XK_braille_dots_248      = $100288a;  { U+288a BRAILLE PATTERN DOTS-248 }
  XK_braille_dots_1248     = $100288b;  { U+288b BRAILLE PATTERN DOTS-1248 }
  XK_braille_dots_348      = $100288c;  { U+288c BRAILLE PATTERN DOTS-348 }
  XK_braille_dots_1348     = $100288d;  { U+288d BRAILLE PATTERN DOTS-1348 }
  XK_braille_dots_2348     = $100288e;  { U+288e BRAILLE PATTERN DOTS-2348 }
  XK_braille_dots_12348    = $100288f;  { U+288f BRAILLE PATTERN DOTS-12348 }
  XK_braille_dots_58       = $1002890;  { U+2890 BRAILLE PATTERN DOTS-58 }
  XK_braille_dots_158      = $1002891;  { U+2891 BRAILLE PATTERN DOTS-158 }
  XK_braille_dots_258      = $1002892;  { U+2892 BRAILLE PATTERN DOTS-258 }
  XK_braille_dots_1258     = $1002893;  { U+2893 BRAILLE PATTERN DOTS-1258 }
  XK_braille_dots_358      = $1002894;  { U+2894 BRAILLE PATTERN DOTS-358 }
  XK_braille_dots_1358     = $1002895;  { U+2895 BRAILLE PATTERN DOTS-1358 }
  XK_braille_dots_2358     = $1002896;  { U+2896 BRAILLE PATTERN DOTS-2358 }
  XK_braille_dots_12358    = $1002897;  { U+2897 BRAILLE PATTERN DOTS-12358 }
  XK_braille_dots_458      = $1002898;  { U+2898 BRAILLE PATTERN DOTS-458 }
  XK_braille_dots_1458     = $1002899;  { U+2899 BRAILLE PATTERN DOTS-1458 }
  XK_braille_dots_2458     = $100289a;  { U+289a BRAILLE PATTERN DOTS-2458 }
  XK_braille_dots_12458    = $100289b;  { U+289b BRAILLE PATTERN DOTS-12458 }
  XK_braille_dots_3458     = $100289c;  { U+289c BRAILLE PATTERN DOTS-3458 }
  XK_braille_dots_13458    = $100289d;  { U+289d BRAILLE PATTERN DOTS-13458 }
  XK_braille_dots_23458    = $100289e;  { U+289e BRAILLE PATTERN DOTS-23458 }
  XK_braille_dots_123458   = $100289f;  { U+289f BRAILLE PATTERN DOTS-123458 }
  XK_braille_dots_68       = $10028a0;  { U+28a0 BRAILLE PATTERN DOTS-68 }
  XK_braille_dots_168      = $10028a1;  { U+28a1 BRAILLE PATTERN DOTS-168 }
  XK_braille_dots_268      = $10028a2;  { U+28a2 BRAILLE PATTERN DOTS-268 }
  XK_braille_dots_1268     = $10028a3;  { U+28a3 BRAILLE PATTERN DOTS-1268 }
  XK_braille_dots_368      = $10028a4;  { U+28a4 BRAILLE PATTERN DOTS-368 }
  XK_braille_dots_1368     = $10028a5;  { U+28a5 BRAILLE PATTERN DOTS-1368 }
  XK_braille_dots_2368     = $10028a6;  { U+28a6 BRAILLE PATTERN DOTS-2368 }
  XK_braille_dots_12368    = $10028a7;  { U+28a7 BRAILLE PATTERN DOTS-12368 }
  XK_braille_dots_468      = $10028a8;  { U+28a8 BRAILLE PATTERN DOTS-468 }
  XK_braille_dots_1468     = $10028a9;  { U+28a9 BRAILLE PATTERN DOTS-1468 }
  XK_braille_dots_2468     = $10028aa;  { U+28aa BRAILLE PATTERN DOTS-2468 }
  XK_braille_dots_12468    = $10028ab;  { U+28ab BRAILLE PATTERN DOTS-12468 }
  XK_braille_dots_3468     = $10028ac;  { U+28ac BRAILLE PATTERN DOTS-3468 }
  XK_braille_dots_13468    = $10028ad;  { U+28ad BRAILLE PATTERN DOTS-13468 }
  XK_braille_dots_23468    = $10028ae;  { U+28ae BRAILLE PATTERN DOTS-23468 }
  XK_braille_dots_123468   = $10028af;  { U+28af BRAILLE PATTERN DOTS-123468 }
  XK_braille_dots_568      = $10028b0;  { U+28b0 BRAILLE PATTERN DOTS-568 }
  XK_braille_dots_1568     = $10028b1;  { U+28b1 BRAILLE PATTERN DOTS-1568 }
  XK_braille_dots_2568     = $10028b2;  { U+28b2 BRAILLE PATTERN DOTS-2568 }
  XK_braille_dots_12568    = $10028b3;  { U+28b3 BRAILLE PATTERN DOTS-12568 }
  XK_braille_dots_3568     = $10028b4;  { U+28b4 BRAILLE PATTERN DOTS-3568 }
  XK_braille_dots_13568    = $10028b5;  { U+28b5 BRAILLE PATTERN DOTS-13568 }
  XK_braille_dots_23568    = $10028b6;  { U+28b6 BRAILLE PATTERN DOTS-23568 }
  XK_braille_dots_123568   = $10028b7;  { U+28b7 BRAILLE PATTERN DOTS-123568 }
  XK_braille_dots_4568     = $10028b8;  { U+28b8 BRAILLE PATTERN DOTS-4568 }
  XK_braille_dots_14568    = $10028b9;  { U+28b9 BRAILLE PATTERN DOTS-14568 }
  XK_braille_dots_24568    = $10028ba;  { U+28ba BRAILLE PATTERN DOTS-24568 }
  XK_braille_dots_124568   = $10028bb;  { U+28bb BRAILLE PATTERN DOTS-124568 }
  XK_braille_dots_34568    = $10028bc;  { U+28bc BRAILLE PATTERN DOTS-34568 }
  XK_braille_dots_134568   = $10028bd;  { U+28bd BRAILLE PATTERN DOTS-134568 }
  XK_braille_dots_234568   = $10028be;  { U+28be BRAILLE PATTERN DOTS-234568 }
  XK_braille_dots_1234568  = $10028bf;  { U+28bf BRAILLE PATTERN DOTS-1234568 }
  XK_braille_dots_78       = $10028c0;  { U+28c0 BRAILLE PATTERN DOTS-78 }
  XK_braille_dots_178      = $10028c1;  { U+28c1 BRAILLE PATTERN DOTS-178 }
  XK_braille_dots_278      = $10028c2;  { U+28c2 BRAILLE PATTERN DOTS-278 }
  XK_braille_dots_1278     = $10028c3;  { U+28c3 BRAILLE PATTERN DOTS-1278 }
  XK_braille_dots_378      = $10028c4;  { U+28c4 BRAILLE PATTERN DOTS-378 }
  XK_braille_dots_1378     = $10028c5;  { U+28c5 BRAILLE PATTERN DOTS-1378 }
  XK_braille_dots_2378     = $10028c6;  { U+28c6 BRAILLE PATTERN DOTS-2378 }
  XK_braille_dots_12378    = $10028c7;  { U+28c7 BRAILLE PATTERN DOTS-12378 }
  XK_braille_dots_478      = $10028c8;  { U+28c8 BRAILLE PATTERN DOTS-478 }
  XK_braille_dots_1478     = $10028c9;  { U+28c9 BRAILLE PATTERN DOTS-1478 }
  XK_braille_dots_2478     = $10028ca;  { U+28ca BRAILLE PATTERN DOTS-2478 }
  XK_braille_dots_12478    = $10028cb;  { U+28cb BRAILLE PATTERN DOTS-12478 }
  XK_braille_dots_3478     = $10028cc;  { U+28cc BRAILLE PATTERN DOTS-3478 }
  XK_braille_dots_13478    = $10028cd;  { U+28cd BRAILLE PATTERN DOTS-13478 }
  XK_braille_dots_23478    = $10028ce;  { U+28ce BRAILLE PATTERN DOTS-23478 }
  XK_braille_dots_123478   = $10028cf;  { U+28cf BRAILLE PATTERN DOTS-123478 }
  XK_braille_dots_578      = $10028d0;  { U+28d0 BRAILLE PATTERN DOTS-578 }
  XK_braille_dots_1578     = $10028d1;  { U+28d1 BRAILLE PATTERN DOTS-1578 }
  XK_braille_dots_2578     = $10028d2;  { U+28d2 BRAILLE PATTERN DOTS-2578 }
  XK_braille_dots_12578    = $10028d3;  { U+28d3 BRAILLE PATTERN DOTS-12578 }
  XK_braille_dots_3578     = $10028d4;  { U+28d4 BRAILLE PATTERN DOTS-3578 }
  XK_braille_dots_13578    = $10028d5;  { U+28d5 BRAILLE PATTERN DOTS-13578 }
  XK_braille_dots_23578    = $10028d6;  { U+28d6 BRAILLE PATTERN DOTS-23578 }
  XK_braille_dots_123578   = $10028d7;  { U+28d7 BRAILLE PATTERN DOTS-123578 }
  XK_braille_dots_4578     = $10028d8;  { U+28d8 BRAILLE PATTERN DOTS-4578 }
  XK_braille_dots_14578    = $10028d9;  { U+28d9 BRAILLE PATTERN DOTS-14578 }
  XK_braille_dots_24578    = $10028da;  { U+28da BRAILLE PATTERN DOTS-24578 }
  XK_braille_dots_124578   = $10028db;  { U+28db BRAILLE PATTERN DOTS-124578 }
  XK_braille_dots_34578    = $10028dc;  { U+28dc BRAILLE PATTERN DOTS-34578 }
  XK_braille_dots_134578   = $10028dd;  { U+28dd BRAILLE PATTERN DOTS-134578 }
  XK_braille_dots_234578   = $10028de;  { U+28de BRAILLE PATTERN DOTS-234578 }
  XK_braille_dots_1234578  = $10028df;  { U+28df BRAILLE PATTERN DOTS-1234578 }
  XK_braille_dots_678      = $10028e0;  { U+28e0 BRAILLE PATTERN DOTS-678 }
  XK_braille_dots_1678     = $10028e1;  { U+28e1 BRAILLE PATTERN DOTS-1678 }
  XK_braille_dots_2678     = $10028e2;  { U+28e2 BRAILLE PATTERN DOTS-2678 }
  XK_braille_dots_12678    = $10028e3;  { U+28e3 BRAILLE PATTERN DOTS-12678 }
  XK_braille_dots_3678     = $10028e4;  { U+28e4 BRAILLE PATTERN DOTS-3678 }
  XK_braille_dots_13678    = $10028e5;  { U+28e5 BRAILLE PATTERN DOTS-13678 }
  XK_braille_dots_23678    = $10028e6;  { U+28e6 BRAILLE PATTERN DOTS-23678 }
  XK_braille_dots_123678   = $10028e7;  { U+28e7 BRAILLE PATTERN DOTS-123678 }
  XK_braille_dots_4678     = $10028e8;  { U+28e8 BRAILLE PATTERN DOTS-4678 }
  XK_braille_dots_14678    = $10028e9;  { U+28e9 BRAILLE PATTERN DOTS-14678 }
  XK_braille_dots_24678    = $10028ea;  { U+28ea BRAILLE PATTERN DOTS-24678 }
  XK_braille_dots_124678   = $10028eb;  { U+28eb BRAILLE PATTERN DOTS-124678 }
  XK_braille_dots_34678    = $10028ec;  { U+28ec BRAILLE PATTERN DOTS-34678 }
  XK_braille_dots_134678   = $10028ed;  { U+28ed BRAILLE PATTERN DOTS-134678 }
  XK_braille_dots_234678   = $10028ee;  { U+28ee BRAILLE PATTERN DOTS-234678 }
  XK_braille_dots_1234678  = $10028ef;  { U+28ef BRAILLE PATTERN DOTS-1234678 }
  XK_braille_dots_5678     = $10028f0;  { U+28f0 BRAILLE PATTERN DOTS-5678 }
  XK_braille_dots_15678    = $10028f1;  { U+28f1 BRAILLE PATTERN DOTS-15678 }
  XK_braille_dots_25678    = $10028f2;  { U+28f2 BRAILLE PATTERN DOTS-25678 }
  XK_braille_dots_125678   = $10028f3;  { U+28f3 BRAILLE PATTERN DOTS-125678 }
  XK_braille_dots_35678    = $10028f4;  { U+28f4 BRAILLE PATTERN DOTS-35678 }
  XK_braille_dots_135678   = $10028f5;  { U+28f5 BRAILLE PATTERN DOTS-135678 }
  XK_braille_dots_235678   = $10028f6;  { U+28f6 BRAILLE PATTERN DOTS-235678 }
  XK_braille_dots_1235678  = $10028f7;  { U+28f7 BRAILLE PATTERN DOTS-1235678 }
  XK_braille_dots_45678    = $10028f8;  { U+28f8 BRAILLE PATTERN DOTS-45678 }
  XK_braille_dots_145678   = $10028f9;  { U+28f9 BRAILLE PATTERN DOTS-145678 }
  XK_braille_dots_245678   = $10028fa;  { U+28fa BRAILLE PATTERN DOTS-245678 }
  XK_braille_dots_1245678  = $10028fb;  { U+28fb BRAILLE PATTERN DOTS-1245678 }
  XK_braille_dots_345678   = $10028fc;  { U+28fc BRAILLE PATTERN DOTS-345678 }
  XK_braille_dots_1345678  = $10028fd;  { U+28fd BRAILLE PATTERN DOTS-1345678 }
  XK_braille_dots_2345678  = $10028fe;  { U+28fe BRAILLE PATTERN DOTS-2345678 }
  XK_braille_dots_12345678 = $10028ff;  { U+28ff BRAILLE PATTERN DOTS-12345678 }
{$ENDIF} { XK_BRAILLE }

{*
 * Sinhala (http://unicode.org/charts/PDF/U0D80.pdf)
 * http://www.nongnu.org/sinhala/doc/transliteration/sinhala-transliteration_6.html
 *}

{$IFDEF XK_SINHALA}
  XK_Sinh_ng         = $1000d82;  { U+0D82 SINHALA ANUSVARAYA }
  XK_Sinh_h2         = $1000d83;  { U+0D83 SINHALA VISARGAYA }
  XK_Sinh_a          = $1000d85;  { U+0D85 SINHALA AYANNA }
  XK_Sinh_aa         = $1000d86;  { U+0D86 SINHALA AAYANNA }
  XK_Sinh_ae         = $1000d87;  { U+0D87 SINHALA AEYANNA }
  XK_Sinh_aee        = $1000d88;  { U+0D88 SINHALA AEEYANNA }
  XK_Sinh_i          = $1000d89;  { U+0D89 SINHALA IYANNA }
  XK_Sinh_ii         = $1000d8a;  { U+0D8A SINHALA IIYANNA }
  XK_Sinh_u          = $1000d8b;  { U+0D8B SINHALA UYANNA }
  XK_Sinh_uu         = $1000d8c;  { U+0D8C SINHALA UUYANNA }
  XK_Sinh_ri         = $1000d8d;  { U+0D8D SINHALA IRUYANNA }
  XK_Sinh_rii        = $1000d8e;  { U+0D8E SINHALA IRUUYANNA }
  XK_Sinh_lu         = $1000d8f;  { U+0D8F SINHALA ILUYANNA }
  XK_Sinh_luu        = $1000d90;  { U+0D90 SINHALA ILUUYANNA }
  XK_Sinh_e          = $1000d91;  { U+0D91 SINHALA EYANNA }
  XK_Sinh_ee         = $1000d92;  { U+0D92 SINHALA EEYANNA }
  XK_Sinh_ai         = $1000d93;  { U+0D93 SINHALA AIYANNA }
  XK_Sinh_o          = $1000d94;  { U+0D94 SINHALA OYANNA }
  XK_Sinh_oo         = $1000d95;  { U+0D95 SINHALA OOYANNA }
  XK_Sinh_au         = $1000d96;  { U+0D96 SINHALA AUYANNA }
  XK_Sinh_ka         = $1000d9a;  { U+0D9A SINHALA KAYANNA }
  XK_Sinh_kha        = $1000d9b;  { U+0D9B SINHALA MAHA. KAYANNA }
  XK_Sinh_ga         = $1000d9c;  { U+0D9C SINHALA GAYANNA }
  XK_Sinh_gha        = $1000d9d;  { U+0D9D SINHALA MAHA. GAYANNA }
  XK_Sinh_ng2        = $1000d9e;  { U+0D9E SINHALA KANTAJA NAASIKYAYA }
  XK_Sinh_nga        = $1000d9f;  { U+0D9F SINHALA SANYAKA GAYANNA }
  XK_Sinh_ca         = $1000da0;  { U+0DA0 SINHALA CAYANNA }
  XK_Sinh_cha        = $1000da1;  { U+0DA1 SINHALA MAHA. CAYANNA }
  XK_Sinh_ja         = $1000da2;  { U+0DA2 SINHALA JAYANNA }
  XK_Sinh_jha        = $1000da3;  { U+0DA3 SINHALA MAHA. JAYANNA }
  XK_Sinh_nya        = $1000da4;  { U+0DA4 SINHALA TAALUJA NAASIKYAYA }
  XK_Sinh_jnya       = $1000da5;  { U+0DA5 SINHALA TAALUJA SANYOOGA NAASIKYAYA }
  XK_Sinh_nja        = $1000da6;  { U+0DA6 SINHALA SANYAKA JAYANNA }
  XK_Sinh_tta        = $1000da7;  { U+0DA7 SINHALA TTAYANNA }
  XK_Sinh_ttha       = $1000da8;  { U+0DA8 SINHALA MAHA. TTAYANNA }
  XK_Sinh_dda        = $1000da9;  { U+0DA9 SINHALA DDAYANNA }
  XK_Sinh_ddha       = $1000daa;  { U+0DAA SINHALA MAHA. DDAYANNA }
  XK_Sinh_nna        = $1000dab;  { U+0DAB SINHALA MUURDHAJA NAYANNA }
  XK_Sinh_ndda       = $1000dac;  { U+0DAC SINHALA SANYAKA DDAYANNA }
  XK_Sinh_tha        = $1000dad;  { U+0DAD SINHALA TAYANNA }
  XK_Sinh_thha       = $1000dae;  { U+0DAE SINHALA MAHA. TAYANNA }
  XK_Sinh_dha        = $1000daf;  { U+0DAF SINHALA DAYANNA }
  XK_Sinh_dhha       = $1000db0;  { U+0DB0 SINHALA MAHA. DAYANNA }
  XK_Sinh_na         = $1000db1;  { U+0DB1 SINHALA DANTAJA NAYANNA }
  XK_Sinh_ndha       = $1000db3;  { U+0DB3 SINHALA SANYAKA DAYANNA }
  XK_Sinh_pa         = $1000db4;  { U+0DB4 SINHALA PAYANNA }
  XK_Sinh_pha        = $1000db5;  { U+0DB5 SINHALA MAHA. PAYANNA }
  XK_Sinh_ba         = $1000db6;  { U+0DB6 SINHALA BAYANNA }
  XK_Sinh_bha        = $1000db7;  { U+0DB7 SINHALA MAHA. BAYANNA }
  XK_Sinh_ma         = $1000db8;  { U+0DB8 SINHALA MAYANNA }
  XK_Sinh_mba        = $1000db9;  { U+0DB9 SINHALA AMBA BAYANNA }
  XK_Sinh_ya         = $1000dba;  { U+0DBA SINHALA YAYANNA }
  XK_Sinh_ra         = $1000dbb;  { U+0DBB SINHALA RAYANNA }
  XK_Sinh_la         = $1000dbd;  { U+0DBD SINHALA DANTAJA LAYANNA }
  XK_Sinh_va         = $1000dc0;  { U+0DC0 SINHALA VAYANNA }
  XK_Sinh_sha        = $1000dc1;  { U+0DC1 SINHALA TAALUJA SAYANNA }
  XK_Sinh_ssha       = $1000dc2;  { U+0DC2 SINHALA MUURDHAJA SAYANNA }
  XK_Sinh_sa         = $1000dc3;  { U+0DC3 SINHALA DANTAJA SAYANNA }
  XK_Sinh_ha         = $1000dc4;  { U+0DC4 SINHALA HAYANNA }
  XK_Sinh_lla        = $1000dc5;  { U+0DC5 SINHALA MUURDHAJA LAYANNA }
  XK_Sinh_fa         = $1000dc6;  { U+0DC6 SINHALA FAYANNA }
  XK_Sinh_al         = $1000dca;  { U+0DCA SINHALA AL-LAKUNA }
  XK_Sinh_aa2        = $1000dcf;  { U+0DCF SINHALA AELA-PILLA }
  XK_Sinh_ae2        = $1000dd0;  { U+0DD0 SINHALA AEDA-PILLA }
  XK_Sinh_aee2       = $1000dd1;  { U+0DD1 SINHALA DIGA AEDA-PILLA }
  XK_Sinh_i2         = $1000dd2;  { U+0DD2 SINHALA IS-PILLA }
  XK_Sinh_ii2        = $1000dd3;  { U+0DD3 SINHALA DIGA IS-PILLA }
  XK_Sinh_u2         = $1000dd4;  { U+0DD4 SINHALA PAA-PILLA }
  XK_Sinh_uu2        = $1000dd6;  { U+0DD6 SINHALA DIGA PAA-PILLA }
  XK_Sinh_ru2        = $1000dd8;  { U+0DD8 SINHALA GAETTA-PILLA }
  XK_Sinh_e2         = $1000dd9;  { U+0DD9 SINHALA KOMBUVA }
  XK_Sinh_ee2        = $1000dda;  { U+0DDA SINHALA DIGA KOMBUVA }
  XK_Sinh_ai2        = $1000ddb;  { U+0DDB SINHALA KOMBU DEKA }
  XK_Sinh_o2         = $1000ddc;  { U+0DDC SINHALA KOMBUVA HAA AELA-PILLA}
  XK_Sinh_oo2        = $1000ddd;  { U+0DDD SINHALA KOMBUVA HAA DIGA AELA-PILLA}
  XK_Sinh_au2        = $1000dde;  { U+0DDE SINHALA KOMBUVA HAA GAYANUKITTA }
  XK_Sinh_lu2        = $1000ddf;  { U+0DDF SINHALA GAYANUKITTA }
  XK_Sinh_ruu2       = $1000df2;  { U+0DF2 SINHALA DIGA GAETTA-PILLA }
  XK_Sinh_luu2       = $1000df3;  { U+0DF3 SINHALA DIGA GAYANUKITTA }
  XK_Sinh_kunddaliya = $1000df4;  { U+0DF4 SINHALA KUNDDALIYA }
{$ENDIF} { XK_SINHALA }

Implementation
End.
