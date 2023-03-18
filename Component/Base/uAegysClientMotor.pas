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
 TOnProcessData = Procedure(aPackList     : TPackList)                                 Of Object;
 TOnAbortData   = Procedure                                                            Of Object;

Type
 TAegysMotorThread = Class(TThread)
 Protected
  Procedure ProcessMessages;
  Procedure Execute;Override;
 Private
  aPackList      : TPackList;
  vDelayThread   : Integer;
  vOnPulseData   : TOnPulseData;
  vOnProcessData : TOnProcessData;
  vAbortData     : TOnAbortData;
 Public
  Procedure   Kill;
  Destructor  Destroy; Override;
  Constructor Create(aDelayThread  : Integer = cDelayThread);
  Property    OnPulseData          : TOnPulseData   Read vOnPulseData   Write vOnPulseData;
  Property    OnProcessData        : TOnProcessData Read vOnProcessData Write vOnProcessData;
  Property    AbortData            : TOnAbortData   Read vAbortData     Write vAbortData;
End;

Implementation

Uses uConstants;

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
     If Not vInExec Then
      Begin
       vInExec := True;
       If aPackList.Count <= cDelayThread Then
        Begin
         If Assigned(vOnProcessData) Then
          vOnProcessData(aPackList);
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
             End;
            End;
          Except
           Break;
          End;
         End;
       Finally
        vInExec := False;
       End;
       vInExec := False;
      End;
    End
   Else
    Break;
   //Delay Processor
   If vDelayThread > 0 Then
    Sleep(vDelayThread div 2);
  End;
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
