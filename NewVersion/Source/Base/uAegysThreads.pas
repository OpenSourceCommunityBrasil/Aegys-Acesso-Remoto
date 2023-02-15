unit uAegysThreads;

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
 SysUtils, Classes, Variants,
 {$IF Defined(HAS_FMX)}{$IF Not Defined(HAS_UTF8)}FMX.Forms{$IFEND}
 {$ELSE}Vcl.Forms{$ELSE}Forms{$IFEND}, uAegysBufferPack, uAegysConsts,
 uAegysDataTypes;

Type
 TOnBeforeExecuteData  = Procedure (Var aPackList   : TPackList)   Of Object;
 TOnExecuteData        = Procedure (Var aPackList   : TPackList;
                                    aPackNo         : AeInt64)     Of Object;
 TOnDataReceive        = Procedure (aData           : TAegysBytes) Of Object;
 TOnAbortData          = Procedure                                 Of Object;
 TOnThreadRequestError = Procedure (ErrorCode       : Integer;
                                    MessageError    : String)      Of Object;
 TOnServiceCommands    = Procedure (InternalCommand : TInternalCommand;
                                    Command         : String)      Of Object;

Type
 TAegysThread = Class(TThread)
 Protected
  Procedure ProcessMessages;
  Procedure Execute;Override;
 Private
  pPackList                               : PPackList;
  pAegysClient                            : TComponent;
  vDelayThread                            : Integer;
  vSelf                                   : TComponent;
  vOnBeforeExecuteData                    : TOnBeforeExecuteData;
  vOnDataReceive                          : TOnDataReceive;
  vOnExecuteData                          : TOnExecuteData;
  vAbortData                              : TOnAbortData;
  vOnThreadRequestError                   : TOnThreadRequestError;
  vOnServiceCommands                      : TOnServiceCommands;
 Public
  Procedure   Kill;
  Destructor  Destroy; Override;
  Constructor Create(Var aPackList        : TPackList;
                     Var aAegysClient     : TComponent;
                     aDelayThread         : Integer;
                     OnBeforeExecuteData  : TOnBeforeExecuteData;
                     OnDataReceive        : TOnDataReceive;
                     OnExecuteData        : TOnExecuteData;
                     OnAbortData          : TOnAbortData;
                     OnServiceCommands    : TOnServiceCommands;
                     OnThreadRequestError : TOnThreadRequestError);
End;

Implementation

Uses uAegysBase;

Procedure TAegysThread.ProcessMessages;
Begin
 {$IFNDEF FPC}
  {$IF Defined(HAS_FMX)}{$IF Not Defined(HAS_UTF8)}FMX.Forms.TApplication.ProcessMessages;{$IFEND}
  {$ELSE}Application.Processmessages;{$IFEND}
 {$ELSE}
  Application.Processmessages;
 {$ENDIF}
End;

Destructor TAegysThread.Destroy;
Begin
 Inherited;
End;

Constructor TAegysThread.Create(Var aPackList        : TPackList;
                                Var aAegysClient     : TComponent;
                                aDelayThread         : Integer;
                                OnBeforeExecuteData  : TOnBeforeExecuteData;
                                OnDataReceive        : TOnDataReceive;
                                OnExecuteData        : TOnExecuteData;
                                OnAbortData          : TOnAbortData;
                                OnServiceCommands    : TOnServiceCommands;
                                OnThreadRequestError : TOnThreadRequestError);
Begin
 If Not Assigned(aPackList) Then
  Begin
   Raise Exception.Create(cPackListNotAssigned);
   Exit;
  End;
 Inherited Create(False);
 vDelayThread          := aDelayThread;
 pPackList             := @aPackList;
 pAegysClient          := aAegysClient;
 vOnExecuteData        := OnExecuteData;
 vAbortData            := OnAbortData;
 vOnThreadRequestError := OnThreadRequestError;
 vOnBeforeExecuteData  := OnBeforeExecuteData;
 vOnDataReceive        := OnDataReceive;
 vOnServiceCommands    := OnServiceCommands;
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

Procedure TAegysThread.Execute;
Var
 vPackno    : AeInt64;
 aBuf       : TAegysBytes;
 aPackClass : TPackClass;
 aPackList  : TPackList;
 vInternalC : Boolean;
 vCommand   : String;
 Procedure ParseLogin(aValue : String);
 Var
  vDataC,
  vID,
  vPWD    : String;
 Begin
  vDataC := Copy(aValue, InitStrPos, Pos('|', aValue) -1);
  Delete(aValue, InitStrPos, Pos('|', aValue));
  vID    := Copy(aValue, InitStrPos, Pos('|', aValue) -1);
  Delete(aValue, InitStrPos, Pos('|', aValue));
  If Pos('|', aValue) > 0 Then
   vPWD  := Copy(aValue, InitStrPos, Pos('|', aValue) -1)
  Else
   vPWD  := aValue;
  TAegysClient(pAegysClient).SetSessionData(vDataC, vID, vPWD);
 End;
Begin
 vPackno   := -1;
 aPackList := TPackList.Create;
 While (Not(Terminated)) Do
  Begin
   Try
    //Process Before Execute one Pack
    If Assigned(vOnBeforeExecuteData) Then
     vOnBeforeExecuteData(aPackList);
    ProcessMessages;
    If aPackList.Count > 0 Then
     Begin
      vInternalC := False;
      aPackClass := aPackList.Items[aPackList.Count -1];
      If aPackClass.DataType = tdtString Then
       Begin
        vCommand   := aPackClass.Command;
        vInternalC := (Copy(vCommand, InitStrPos, Length(cStatusDesc)) = cStatusDesc); //Status Login
        If vInternalC Then
         Begin
          Delete(vCommand, InitStrPos, Length(cStatusDesc) +1);
          ParseLogin(vCommand);
          If Assigned(vOnServiceCommands) Then
           vOnServiceCommands(ticLogin, vCommand);
         End;
        If vInternalC Then
         aPackList.Delete(aPackList.Count -1);
       End;
      If Not vInternalC Then
       Begin
        aBuf := aPackList.ReadPack(aPackList.Count -1);
        Try
         If Assigned(vOnDataReceive) Then
          vOnDataReceive(aBuf);
        Finally
         SetLength(aBuf, 0);
        End;
       End;
     End;
    //Try Process one Pack
    If (vPackno > 0)                  And
       (pPackList^.Count > 0)         Then
     Dec(vPackno)
    Else If (pPackList^.Count > 0)    Then
     vPackno := pPackList^.Count -1;
    If (pPackList^.Count > 0)         And
       (vPackno > -1)                 Then
     Begin
      If Assigned(vOnExecuteData)     Then
       vOnExecuteData(pPackList^, vPackno);
     End;
    ProcessMessages;
    //Delay Processor
    If vDelayThread > 0 Then
     Sleep(vDelayThread);
   Except
    On E : Exception Do
     Begin
      If Assigned(vOnThreadRequestError) Then
       vOnThreadRequestError(500, E.Message);
      TAegysClient(pAegysClient).ThreadDisconnect;
      Break;
     End;
   End;
  End;
 FreeAndNil(aPackList);
End;

Procedure TAegysThread.Kill;
Begin
 Terminate;
 ProcessMessages;
 If Assigned(vAbortData) Then
  vAbortData;
 ProcessMessages;
 If Assigned(vOnThreadRequestError) Then
  vOnThreadRequestError(499, cThreadCancel);
 ProcessMessages;
End;

End.
