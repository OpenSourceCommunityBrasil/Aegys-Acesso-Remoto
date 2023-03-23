unit uAegysClientMotor;

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

Uses
 SysUtils, Classes, Variants, {$IF Defined(HAS_FMX)}{$IF Not Defined(HAS_UTF8)}FMX.Forms{$IFEND}
 {$ELSE}Vcl.Forms{$ELSE}Forms{$IFEND}, uAegysBufferPack, uAegysConsts, uAegysTools,
 uAegysBase, uAegysDataTypes;

Type
 TOnPulseData   = Function (aPack         : TAegysBytes;
                            CommandType   : TCommandType = tctScreenCapture) : Boolean Of Object;
 TOnProcessData = Procedure(aPackList     : TPackList;
                            aFullFrame    : Boolean)                                   Of Object;
 TOnAbortData   = Procedure                                                            Of Object;

Type
 TAegysMotorThread = Class(TThread)
 Protected
  Procedure ProcessMessages;
  Procedure Execute;Override;
 Private
  aFullFrame     : Boolean;
  aPackList      : TPackList;
  vDelayThread   : Integer;
  vOnPulseData   : TOnPulseData;
  vOnProcessData : TOnProcessData;
  vAbortData     : TOnAbortData;
 Public
  Procedure   Kill;
  Procedure   GetFullFrame;
  Destructor  Destroy; Override;
  Constructor Create(aDelayThread  : Integer = cDelayThread);
  Property    OnPulseData          : TOnPulseData   Read vOnPulseData   Write vOnPulseData;
  Property    OnProcessData        : TOnProcessData Read vOnProcessData Write vOnProcessData;
  Property    AbortData            : TOnAbortData   Read vAbortData     Write vAbortData;
End;

Implementation

Uses uConstants, Execute.DesktopDuplicationAPI, uFormConexao, uAegysRFBList;

procedure TAegysMotorThread.Execute;
Var
 I       : Integer;
 vInExec : Boolean;
Begin
 vInExec := False;
 While (Not(Terminated)) Do
  Begin
   If Assigned(aPackList) Then
    Begin
     If Not (vInExec) Or (cRFB) Then
      Begin
       vInExec := True;
       Application.Processmessages;
       If aPackList.Count <= cDelayThread Then
        Begin
         If Assigned(vOnProcessData) Then
          vOnProcessData(aPackList, aFullFrame);
         If aFullFrame Then
          aFullFrame := False;
        End;
       Try
        If Not Assigned(aPackList) Then
         Break;
        If (aPackList.Count <= cDelayThread) And
           (aPackList.Count > 0) Then
         Begin
          Try
           //Process Before Execute one Pack
           If Assigned(vOnPulseData) Then
            Begin
             Try
              vOnPulseData(aPackList[0].ToBytes,
                           aPackList[0].CommandType);
              aPackList.Delete(0);
             Finally
              Application.Processmessages;
             End;
            End;
          Except
           Break;
          End;
         End;
       Finally
        vInExec := False;
       End;
      End;
    End
   Else
    Break;
   //Delay Processor
   Application.Processmessages;
   If vDelayThread > 0 Then
    Sleep(vDelayThread div 2);
   Application.Processmessages;
  End;
End;

Procedure TAegysMotorThread.GetFullFrame;
Begin
 aFullFrame := True;
End;

Procedure TAegysMotorThread.Kill;
Begin
 Terminate;
 ProcessMessages;
 If Assigned(vAbortData) Then
  vAbortData;
 ProcessMessages;
End;

Constructor TAegysMotorThread.Create(aDelayThread  : Integer = cDelayThread);
Begin
 Inherited Create(False);
 aFullFrame := False;
 if Not cRFB Then
  Begin
   If Not Assigned(FDuplication) Then
    FDuplication := TDesktopDuplicationWrapper.Create;
  End
 Else
  Begin
   If Not Assigned(FAegysVarredura) Then
    FAegysVarredura := TAegysVarredura.Create;
   FAegysVarredura.Square            := True;
   FAegysVarredura.CaptureCursor     := False;
   FAegysVarredura.ToleranceChange   := 0;
   FAegysVarredura.Pixels            := 16;
   FAegysVarredura.Quality           := 100;
   FAegysVarredura.Zoom              := 100;
  End;
 aPackList             := TPackList.Create;
 vDelayThread          := aDelayThread;
 {$IFNDEF FPC}
  {$IF Defined(HAS_FMX)}
   {$IF Not Defined(HAS_UTF8)}
    Priority           := tpLowest;
   {$IFEND}
  {$ELSE}
   Priority            := tpLowest;
  {$IFEND}
 {$ELSE}
  Priority             := tpLowest;
 {$ENDIF}
End;

Destructor TAegysMotorThread.Destroy;
Begin
 If Not cRFB Then
  Begin
   If Assigned(FDuplication) Then
    FreeAndNil(FDuplication);
  End
 Else
  Begin
   If Assigned(FAegysVarredura) Then
    FreeAndNil(FAegysVarredura);
  End;
 FreeAndNil(aPackList);
 Inherited;
End;

Procedure TAegysMotorThread.ProcessMessages;
Begin
 {$IFNDEF FPC}
  {$IF Defined(HAS_FMX)}{$IF Not Defined(HAS_UTF8)}FMX.Forms.TApplication.ProcessMessages;{$IFEND}
  {$ELSE}Application.Processmessages;{$IFEND}
 {$ELSE}
  Application.Processmessages;
 {$ENDIF}
End;

End.
