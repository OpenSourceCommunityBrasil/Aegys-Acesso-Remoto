{.$DEFINE USE_DEPRECATED} // not used for now

{$IFDEF VER140} // D6
  {$DEFINE DELPHI6_UP}
{$ENDIF}

{$IFDEF VER150} // D7
  {$DEFINE DELPHI6_UP}
{$ENDIF}

{$IFDEF VER190} // 2006
  {$DEFINE DELPHI6_UP}
  {$DEFINE D2006_UP}
{$ENDIF}

{$IFDEF VER200} // 2009
  {$DEFINE DELPHI6_UP}
  {$DEFINE D2006_UP}
{$ENDIF}

{$IFDEF VER210} // 2010
  {$DEFINE DELPHI6_UP}
  {$DEFINE D2006_UP}
  {$DEFINE D2010_UP}
{$ENDIF}

{$IFDEF VER220} // XE
  {$DEFINE DELPHI6_UP}
  {$DEFINE D2006_UP}
  {$DEFINE D2010_UP}
{$ENDIF}

{$IFDEF VER230} // XE2
  {$DEFINE DELPHI6_UP}
  {$DEFINE D2006_UP}
  {$DEFINE D2010_UP}
  {$DEFINE DXE2_UP}
{$ENDIF}

{$IFDEF VER240} // XE3
  {$DEFINE DELPHI6_UP}
  {$DEFINE D2006_UP}
  {$DEFINE D2010_UP}
  {$DEFINE DXE2_UP}
{$ENDIF}

{$IFNDEF DXE2_UP}
  {$DEFINE CPUX86}
Type
  NativeInt   = Integer;
  NativeUInt  = Cardinal;
  PNativeInt  = ^NativeInt;
  PNativeUInt = ^NativeUInt;
  UINT_PTR    = NativeUInt;
  SIZE_T      = UINT_PTR;
{$ENDIF}

{
64-bit assembler:

The first four parameters to inline assembler functions are passed via RCX, RDX, R8, and R9 respectively, except for floating-point arguments which use XMM0, XMM1, XMM2, XMM3. The return value is placed in RAX.
Registers used for function parameters can be modified freely.
RAX, R10, R11, XMM4, XMM5 can be used freely.

The contents of the following registers must be preserved and restored within inline assembly functions: 
R12 - R15, RDI, RSI, RBX, RBP, RSP, XMM6 - XMM15. 

The math coprocessor is not normally used from x64 code. 

http://blogs.msdn.com/b/oldnewthing/archive/2004/01/14/58579.aspx
http://msdn.microsoft.com/en-us/library/9z1stfyw(v=vs.80).aspx
http://docwiki.embarcadero.com/RADStudio/XE3/en/Using_Inline_Assembly_Code#64-bit

Faster floating point for x64/Single type: $EXCESSPRECISION OFF
http://docwiki.embarcadero.com/RADStudio/XE3/en/Floating_point_precision_control_(Delphi_for_x64)
}