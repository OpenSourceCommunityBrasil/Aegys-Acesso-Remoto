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
 {$ELSE}Vcl.Forms{$ELSE}Forms{$IFEND}, Windows, uAegysBufferPack, uAegysConsts, uAegysTools,
 uAegysBase, uAegysDataTypes;

Type
 TOnPulseData   = Function (aPack         : TAegysBytes;
                            CommandType   : TCommandType = tctScreenCapture) : Boolean Of Object;
 TOnProcessData = Procedure(aPackList     : TPackList;
                            aFullFrame    : Boolean)                                   Of Object;
 TOnAbortData   = Procedure                                                            Of Object;

Type
 TAegysSubMotorThread = Class(TThread)
 Protected
  Procedure Execute;Override;
 Private
  vDelayThread   : Integer;
  aPackList      : TPackList;
  vOnPulseData   : TOnPulseData;
  vOnProcessData : TOnProcessData;
  vAbortData     : TOnAbortData;
  aActualError   : String;
 Public
  Procedure   Kill;
  Destructor  Destroy; Override;
  Constructor Create(aDelayThread  : Integer = cDelayThread);
  Property    PackList             : TPackList      Read aPackList      Write aPackList;
  Property    OnPulseData          : TOnPulseData   Read vOnPulseData   Write vOnPulseData;
  Property    OnProcessData        : TOnProcessData Read vOnProcessData Write vOnProcessData;
  Property    AbortData            : TOnAbortData   Read vAbortData     Write vAbortData;
End;

Type
 TAegysMotorThread = Class(TThread)
 Protected
  Procedure ProcessMessages;
  Procedure Execute;Override;
 Private
  SubMotorThread : TAegysSubMotorThread;
  aFullFrame     : Boolean;
  aPackList      : TPackList;
  vDelayThread   : Integer;
  vOnPulseData   : TOnPulseData;
  vOnProcessData : TOnProcessData;
  vAbortData     : TOnAbortData;
  aActualError   : String;
  Procedure SetOnPulseData  (aValue : TOnPulseData);
  Procedure SetOnProcessData(aValue : TOnProcessData);
  Procedure SetAbortData    (aValue : TOnAbortData);
 Public
  Procedure   Kill;
  Procedure   GetFullFrame;
  Destructor  Destroy; Override;
  Constructor Create(aDelayThread  : Integer = cDelayThread);
  Property    OnPulseData          : TOnPulseData   Read vOnPulseData   Write SetOnPulseData;
  Property    OnProcessData        : TOnProcessData Read vOnProcessData Write SetOnProcessData;
  Property    AbortData            : TOnAbortData   Read vAbortData     Write SetAbortData;
End;

Const
 cMaxSendFrame = 2;

Implementation

Uses uConstants;

Procedure TAegysMotorThread.SetOnPulseData  (aValue : TOnPulseData);
Begin
 If Assigned(SubMotorThread) Then
  SubMotorThread.OnPulseData := aValue;
 vOnPulseData := aValue;
End;

Procedure TAegysMotorThread.SetOnProcessData(aValue : TOnProcessData);
Begin
 If Assigned(SubMotorThread) Then
  SubMotorThread.OnProcessData := aValue;
 vOnProcessData := aValue;
End;

Procedure TAegysMotorThread.SetAbortData    (aValue : TOnAbortData);
Begin
 If Assigned(SubMotorThread) Then
  SubMotorThread.AbortData := aValue;
 vAbortData := aValue;
End;

procedure TAegysMotorThread.Execute;
Var
 vInExec : Boolean;
Begin
 vInExec      := False;
 aActualError := '';
 SubMotorThread.Resume;
 While (Not(Terminated)) Do
  Begin
   If Assigned(aPackList) Then
    Begin
     Try
      If Assigned(vOnProcessData) Then
       vOnProcessData(aPackList, aFullFrame);
      If aFullFrame Then
       aFullFrame := False;
      Processmessages;
      If (vDelayThread    > 0) And
         (aPackList.Count > 1) Then
       Sleep(vDelayThread div 2);
     Except
      On E : Exception Do
       Begin
        aActualError := E.Message;
       End;
     End;
    End;
   Try
    If Not Assigned(aPackList) Then
     Break;
   Finally
    vInExec := False;
   End;
  End;
End;

Procedure TAegysMotorThread.GetFullFrame;
Begin
 aFullFrame := True;
End;

Procedure TAegysMotorThread.Kill;
Begin
 Try
  If Assigned(SubMotorThread) Then
   SubMotorThread.Kill;
 Finally 
  {$IFDEF FPC}
   WaitForThreadTerminate(SubMotorThread.Handle, 100);
  {$ELSE}
   {$IF Not Defined(HAS_FMX)}
    WaitForSingleObject  (SubMotorThread.Handle, 100);
   {$IFEND}
  {$ENDIF}
  FreeAndNil(SubMotorThread);
 End;
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
 SubMotorThread := TAegysSubMotorThread.Create(aDelayThread);
 SubMotorThread.PackList := aPackList;
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

{ TAegysSubMotorThread }

Constructor TAegysSubMotorThread.Create(aDelayThread: Integer);
Begin
 Inherited Create(False);
 vDelayThread := aDelayThread;
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

Destructor TAegysSubMotorThread.Destroy;
Begin
 Inherited;
End;

Procedure TAegysSubMotorThread.Execute;
Var
 vInExec     : Boolean;
 aBytes      : TAegysBytes;
 CommandType : TCommandType;
Begin
 While (Not(Terminated)) Do
  Begin
   If (aPackList.Count > 0) Then
    Begin
     //Process Before Execute one Pack
     If Assigned(vOnPulseData) Then
      Begin
       Try
        aBytes      := aPackList[0].ToBytes;
        CommandType := aPackList[0].CommandType;
        aPackList.Delete(0);
        Processmessages;
        vOnPulseData(aBytes, CommandType);
       Except
        On E : Exception Do
         Begin
          aActualError := E.Message;
         End;
       End;
      End;
    End
   Else
    Begin 
     //Delay Processor
     Processmessages;
     If vDelayThread > 0 Then
      Sleep(vDelayThread div 2);
    End;
  End;
End;

Procedure TAegysSubMotorThread.Kill;
Begin
 Terminate;
 If Assigned(vAbortData) Then
  vAbortData;
End;

End.
