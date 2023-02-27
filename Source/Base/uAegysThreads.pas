unit uAegysThreads;

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

Uses
 SysUtils, Classes, Variants,
  {$IFNDEF FPC}
   {$IF Defined(HAS_FMX)}
   FMX.Forms
   {$ELSE}
    {$IF CompilerVersion > 27}
     Vcl.Forms
    {$ELSE}Forms{$IFEND}
   {$IFEND}
 {$ELSE}
  Forms
 {$ENDIF},
 uAegysBufferPack, uAegysConsts,
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
 TOnClientCommands     = Procedure (CommandType     : TCommandType;
                                    Connection,
                                    ID,
                                    Command         : String;
                                    aBuf            : TAegysBytes) Of Object;

Type
 TAegysThread = Class(TThread)
 Protected
  Procedure ProcessMessages;
  Procedure Execute;Override;
 Private
  pPackList                               : PPackList;
  pAegysClient                            : TComponent;
  vDelayThread                            : Integer;
  vOnBeforeExecuteData                    : TOnBeforeExecuteData;
  vOnDataReceive                          : TOnDataReceive;
  vOnExecuteData                          : TOnExecuteData;
  vAbortData                              : TOnAbortData;
  vOnThreadRequestError                   : TOnThreadRequestError;
  vOnServiceCommands                      : TOnServiceCommands;
  vOnClientCommands                       : TOnClientCommands;
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
                     OnClientCommands     : TOnClientCommands;
                     OnThreadRequestError : TOnThreadRequestError);
End;

Implementation

Uses uAegysBase, uAegysTools;

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
                                OnClientCommands     : TOnClientCommands;
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
 vOnClientCommands     := OnClientCommands;
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
Type
 TThreadDirection     = (ttdReceive, ttdSend);
 TDirectThreadAction  = Array[0..Integer(High(TThreadDirection))] of Boolean;
Var
 ArrayOfPointer     : TArrayOfPointer;
 A, I               : AeInteger;
 vPackno            : AeInt64;
 bBuf,
 aBuf               : TAegysBytes;
 aPackClass         : TPackClass;
 aPackList          : TPackList;
 vInternalC         : Boolean;
 vBytesOptions,
 vOwner,
 vID,
 vCommand           : String;
 vInternalCommand   : TInternalCommand;
 DirectThreadAction : TDirectThreadAction;
 Procedure ParseLogin(aValue : String);
 Var
  vDataC,
  vID,
  vPWD    : String;
 Begin
  vDataC := Copy(aValue, 1, Pos('&', aValue) -1);
  Delete(aValue, 1, Pos('&', aValue));
  vID    := Copy(aValue, 1, Pos('&', aValue) -1);
  Delete(aValue, 1, Pos('&', aValue));
  If Pos('&', aValue) > 0 Then
   vPWD  := Copy(aValue, 1, Pos('&', aValue) -1)
  Else
   vPWD  := aValue;
  TAegysClient(pAegysClient).SetSessionData(vDataC, vID, vPWD);
 End;
 Procedure BeginActions;
 Var
  X : Integer;
 Begin
  For x := 0 To High(DirectThreadAction) Do
   DirectThreadAction[x] := False;
 End;
Begin
 vPackno   := -1;
 aPackList := TPackList.Create;
 SetLength(bBuf, 0);
 BeginActions;
 While (Not(Terminated)) Do
  Begin
   Try
    Try
     If Not DirectThreadAction[Integer(ttdReceive)] Then
      Begin
       DirectThreadAction[Integer(ttdReceive)] := True;
       ProcessMessages;
       //Process Before Execute one Pack
       If Assigned(vOnBeforeExecuteData) Then
        vOnBeforeExecuteData(aPackList);
       If aPackList.Count > 0 Then
        Begin
         ProcessMessages;
         vInternalC := False;
         aPackClass := aPackList.Items[0];
         If aPackClass.DataType = tdtString Then
          Begin
           vInternalC := True;
           vCommand   := aPackClass.Command;
           ParseCommand(vCommand, vInternalCommand);
           If (aPackClass.CommandType  = tctNone) And
              (vInternalCommand       <> ticNone) Then
            Begin
             If vInternalCommand in [ticLogin,
                                     ticDataStatus] Then
              ParseLogin(vCommand);
             Synchronize(Procedure
                         Begin
                          If Assigned(vOnServiceCommands) Then
                           vOnServiceCommands(vInternalCommand, vCommand);
                         End);
            End
           Else
            Begin
             ArrayOfPointer := [@vOwner, @vID];
             If Not (aPackClass.CommandType in [tctKeyboard, tctMouse]) Then
              ParseValues(vCommand, ArrayOfPointer);
             If (aPackClass.CommandType <> tctNone) Then
              Begin
               Synchronize(Procedure
                           Begin
                            If Assigned(vOnClientCommands) Then
                             vOnClientCommands(aPackClass.CommandType, vOwner, vID, vCommand, bBuf);
                           End);
              End;
            End;
           If vInternalC Then
            aPackList.Delete(0);
          End;
         If Not vInternalC Then
          Begin
           aBuf := aPackClass.DataBytes;
           Try
            ArrayOfPointer := [@vOwner, @vID];
            vBytesOptions  := aPackClass.BytesOptions;
            ParseValues(vBytesOptions, ArrayOfPointer);
            If (aPackClass.CommandType <> tctNone) Then
             Begin
              Synchronize(Procedure
                          Begin
                           If Assigned(vOnClientCommands) Then
                            vOnClientCommands(aPackClass.CommandType, vOwner, vID, vBytesOptions, aBuf)
                           Else If Assigned(vOnDataReceive) Then
                            vOnDataReceive(aBuf);
                          End);
             End;
           Finally
            aPackList.Delete(0);
            SetLength(aBuf, 0);
           End;
          End;
        End;
       DirectThreadAction[Integer(ttdReceive)] := False;
      End;
     If Not DirectThreadAction[Integer(ttdSend)] Then
      Begin
       DirectThreadAction[Integer(ttdSend)] := True;
       ProcessMessages;
       //Try Process one Pack
       vPackno := 0;
       If (pPackList^.Count > 0)         And
          (vPackno > -1)                 Then
        Begin
         A := pPackList^.Count -1;
         For I := A Downto 0 Do
          Begin
           If Terminated Then
            Break;
           vPackno := A - I;
           Synchronize(Procedure
                       Begin
                        If Assigned(vOnExecuteData)     Then
                         vOnExecuteData(pPackList^, vPackno);
                       End);
           Processmessages;
          End;
        End;
       DirectThreadAction[Integer(ttdSend)] := False;
      End;
    Finally
    End;
   Except
    On E : Exception Do
     Begin
      If Assigned(vOnThreadRequestError) Then
       vOnThreadRequestError(500, E.Message);
      TAegysClient(pAegysClient).ThreadDisconnect;
      Break;
     End;
   End;
   //Delay Processor
   If vDelayThread > 0 Then
    Sleep(vDelayThread);
   ProcessMessages;
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
