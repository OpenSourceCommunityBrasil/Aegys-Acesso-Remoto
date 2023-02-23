unit uAegysDataTypes;

{$I ..\Includes\uAegys.inc}

{
   Aegys Remote Access Project.
  Criado por XyberX (Gilbero Rocha da Silva), o Aegys Remote Access Project tem como objetivo o uso de Acesso remoto
  Gratuito para utilização de pessoas em geral.
   O Aegys Remote Access Project tem como desenvolvedores e mantedores hoje

  Membros do Grupo :

  XyberX (Gilberto Rocha)    - Admin - Criador e Administrador  do pacote.
  Wendel Fassarela           - Devel and Admin
  Mobius One                 - Devel, Tester and Admin.
  Gustavo                    - Devel and Admin.
  Roniery                    - Devel and Admin.
  Alexandre Abbade           - Devel and Admin.
  e Outros como você, venha participar também.
}

Interface

uses
  SysUtils, Classes, Variants;

Type
 TArrayOfPointer   = Array of Pointer;
 PComponent        = ^TComponent;
 TPingSense        = (tpsPing,           tpsPong);
 TInternalCommand  = (ticNone,           ticLogin,
                      ticConnectionList, ticDataStatus,
                      ticFindID,         ticIDExistsReqPass,
                      ticIDNotFound,     ticPing,
                      ticPong,           ticCheckPass,
                      ticAccessGranted,  ticGetMonitorCount,
                      ticChangeMonitor,  ticRelation,
                      ticConnectedPeer,  ticDisconnectedPeer,
                      ticDisconnectPeer, ticDisconnectAllPeers,
                      ticKick,           ticIncommingConnect);
 TDataMode         = (tdmServerCommand,  tdmClientCommand,
                      tdmReply);
 TDataType         = (tdtString,         tdtDataBytes);
 TDataCheck        = (tdcAsync,          tdcSync);
 TCommandType      = (tctScreenCapture,  tctMonitor,
                      tctAudio,          tctVideo,
                      tctKeyboard,       tctMouse,
                      tctFileTransfer,   tctChat,
                      tctClipBoard,      tctNone);
 TAegysBytes       = Array of Byte;
 {$IFDEF FPC}
  AEInteger        = Longint;
  AEInt16          = Integer;
  AEInt64          = Int64;
  AEInt32          = Int32;
  AEFloat          = Real;
  AEBufferSize     = Longint;
  AEUInt16         = Word;
  AEUInt32         = LongWord;
 {$ELSE}
  AEInteger        = Integer;
  AEInt16          = Integer;
  AEInt64          = Int64;
  AEInt32          = Longint;
  AEFloat          = Real;
  AEBufferSize     = Longint;
  AEUInt16         = Word;
  AEUInt32         = LongWord;
 {$ENDIF}
 PAEInt32          = ^AEInt32;
 PAEInt64          = ^AEInt64;
 PAEUInt32         = ^AEInt32;
 PAEUInt16         = ^AEUInt16;
 PAEInt16          = ^AEUInt16;
 {$IFNDEF FPC}
  {$IF (CompilerVersion >= 26) And (CompilerVersion <= 29)}
   {$IF Defined(HAS_FMX)}
    AEString       = String;
    AEWideString   = WideString;
    AEChar         = Char;
   {$ELSE}
    AEString       = Utf8String;
    AEWideString   = WideString;
    AEChar         = Utf8Char;
   {$IFEND}
  {$ELSE}
   {$IF Defined(HAS_FMX)}
    AEString       = Utf8String;
    AEWideString   = String;
    AEChar         = Utf8Char;
   {$ELSE}
    AEString       = AnsiString;
    AEWideString   = WideString;
    AEChar         = AnsiChar;
   {$IFEND}
  {$IFEND}
 {$ELSE}
  AEString         = AnsiString;
  AEWideString     = WideString;
  AEChar           = Char;
 {$ENDIF}
 AEWideChar        = WideChar;
 TAEWideChars      = Array Of AEWideChar;
 PAEChar           = ^AEChar;
 PAEWideChar       = ^AEWideChar;
 PAEWideString     = ^AEWideString;
 PAEString         = ^AEString;

Var
 InitStrPos,
 FinalStrPos   : Integer;

Implementation

Procedure InitializeStrings;
{$IFNDEF FPC}
 {$if CompilerVersion > 24} // Delphi 2010 pra cima
 Var
  s : String;
 {$IFEND}
{$ENDIF}
Begin
 {$IFNDEF FPC}
  {$if CompilerVersion > 24} // Delphi 2010 pra cima
   s := '0';
   If Low(s) = 0 Then
    Begin
     InitStrPos  := 0;
     FinalStrPos := 1;
    End
   Else
    Begin
     InitStrPos  := 1;
     FinalStrPos := 0;
    End;
  {$ELSE}
   InitStrPos  := 1;
   FinalStrPos := 0;
  {$IFEND}
 {$ELSE}
  InitStrPos  := 1;
  FinalStrPos := 0;
 {$ENDIF}
End;

Initialization
 InitializeStrings;

End.
