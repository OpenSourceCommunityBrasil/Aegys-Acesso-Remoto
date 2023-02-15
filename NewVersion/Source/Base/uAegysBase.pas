Unit uAegysBase;

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
 //Delphi Basic Units
 SysUtils, Classes, Variants,
 {$IF Defined(HAS_FMX)}{$IF Not Defined(HAS_UTF8)}FMX.Forms{$IFEND}
 {$ELSE}Vcl.Forms{$ELSE}Forms{$IFEND},
 {$IFDEF MSWINDOWS}Windows,  {$ENDIF}
 //Indy 10.6 Basic Units
 IdContext,         IdTCPConnection, IdTCPClient,     IdComponent,     IdBaseComponent,
 IdCustomTCPServer, IdTCPServer,     IdStack,         IdExceptionCore, IdGlobal,
 //Aegys Basic Units
 uAegysBufferPack,  uAegysConsts,    uAegysDataTypes, uAegysThreads;

Type
 TAegysClientStage   = (csNone, csWaitLogon, csWaitPWD, csLoggedIn, csRejected);
 TAegysSession       = Class;
 TAegysSessionList   = Class(TList)
  Private
   Function  GetRec         (Index         : Integer) : TAegysSession;       Overload;
   Procedure PutRec         (Index         : Integer;
                             Item          : TAegysSession);                 Overload;
   Procedure ClearAll;
  Protected
  Public
   Destructor Destroy; Override;
   Procedure  Delete        (Index         : Integer);                       Overload;
   Function   Add           (Item          : TAegysSession)  : Integer;      Overload;
   Property   Items         [Index         : Integer]        : TAegysSession Read GetRec Write PutRec; Default;
  End;
 TAegysSessionData  = Class(TCollectionItem);
 PAegysSession      = ^TAegysSession;
 TAegysSession      = Class(TAegysSessionData)
 Private
  {$IFNDEF FPC}
   {$IF (DEFINED(OLDINDY))}
    vDataEncoding   : TIdTextEncoding;
   {$ELSE}
    vDataEncoding   : IIdTextEncoding;
   {$IFEND}
  {$ELSE}
   vDataEncoding    : IIdTextEncoding;
  {$ENDIF}
  vAContext         : TIdContext;
  vAegysClientStage : TAegysClientStage;
  vAcceptUnAssist   : Boolean;
  vConnection,
  vSessionPWD,
  vSessionFixedPWD,
  vSessionID        : String;
  vInitalRequest,
  vLastRequest      : TDateTime;
  vLogged           : Boolean;
  vAegysSessionList : TAegysSessionList;
  Procedure   ProcessMessages;
 Public
  Function    ReceiveString            : String;
  Procedure   SendString(S             : String;
                         WaitReply     : Boolean = False);
  Procedure   SendStream(Var aStream   : TMemoryStream;
                         WaitReply     : Boolean = False);
  Procedure   SendBytes (aBuf          : TAegysBytes;
                         WaitReply     : Boolean = False);
  Procedure   Kick      (Gracefully    : Boolean = False);
  Constructor Create;
  Destructor  Destroy;Override;
  Property    Socket          : TIdContext                   Read vAContext               Write vAContext;
  Property    Connection      : String                       Read vConnection             Write vConnection;
  Property    ClientStage     : TAegysClientStage            Read vAegysClientStage;
  Property    SessionID       : String                       Read vSessionID              Write vSessionID;
  Property    SessionPWD      : String                       Read vSessionPWD             Write vSessionPWD;
  Property    AcceptUnAssist  : Boolean                      Read vAcceptUnAssist         Write vAcceptUnAssist;
  Property    SessionFixedPWD : String                       Read vSessionFixedPWD        Write vSessionFixedPWD;
  Property    InitalRequest   : TDateTime                    Read vInitalRequest;
  Property    LastRequest     : TDateTime                    Read vLastRequest;
  Property    Logged          : Boolean                      Read vLogged;
  Property    SessionList     : TAegysSessionList            Read vAegysSessionList       Write vAegysSessionList;
End;

Type
 PSessionList = ^TSessionList;
 TSessionList = Class(TList)
 Private
  Function  GetRec         (Index         : Integer) : TAegysSession;      Overload;
  Procedure PutRec         (Index         : Integer;
                            Item          : TAegysSession);                Overload;
  Procedure ClearAll;
 Protected
 Public
  Destructor Destroy; Override;
  Function   GetConnection (aSocket       : TIdContext)    : TAegysSession;Overload;
  Function   GetConnection (aConnStr      : String)        : TAegysSession;Overload;
  Function   GetConnection (aID, aPass    : String)        : TAegysSession;Overload;
  Procedure  Delete        (Value         : TAegysSession);                Overload;
  Procedure  Delete        (Index         : Integer);                      Overload;
  Function   Add           (Item          : TAegysSession) : Integer;      Overload;
  Property   Items         [Index         : Integer]       : TAegysSession Read GetRec Write PutRec; Default;
End;

Type
 TAegysOnSession           = Procedure (Const Sender      : TAegysSession)  Of Object;
 TAegysOnBeforeConnect     = Procedure (Sender            : TObject;
                                        Var WelcomeString : String)         Of Object;
 TAegysOnConnect           = Procedure (Sender            : TObject)        Of Object;
 TAegysOnDisconnect        = Procedure (Sender            : TObject)        Of Object;
 TAegysOnReceiveCommand    = Procedure (InternalCommand   : TInternalCommand;
                                        Command           : String)         Of Object;
 TAegysOnGetClientDetails  = Procedure (ContextList       : TSessionList;
                                        Value             : String;
                                        Var ClientID,
                                        ClientPassword    : String;
                                        Var Accept        : Boolean)        Of Object;
 TAegysOnReceiveBytes      = Procedure (aBuffer           : TAegysBytes)    Of Object;
 TAegysOnReceiveStream     = Procedure (Const aStream     : TStream;
                                        Var Accept        : Boolean;
                                        Var ErrorMessage  : String)         Of Object;
 TAegysOnReceiveFileStream = Procedure (Const Filename    : String;
                                        Const aStream     : TStream;
                                        Var Accept        : Boolean;
                                        Var ErrorMessage  : String)         Of Object;

Type
 TAegysService = Class(TComponent)
 Protected
  Procedure  Execute(AContext     : TIdContext);
 Private
  vActive                         : Boolean;
  vServicePort,
  vServiceTimeout                 : Integer;
  vSessionList                    : TSessionList;
  vIdTCPServer                    : TIdTCPServer;
  vOnClientRequestExecute,
  vOnDisconnect,
  vOnConnect                      : TAegysOnSession;
  vOnGetClientDetails             : TAegysOnGetClientDetails;
  vGarbageTime                    : Integer;
  {$IFDEF FPC}
  vDatabaseCharSet                : TDatabaseCharSet;
  {$ENDIF}
  Procedure  ProcessMessages;
  Procedure  SetActive (Value     : Boolean);
  Procedure  Disconnect(AContext  : TIdContext);
  Procedure  Connect   (AContext  : TIdContext);
 Public
  Constructor Create   (AOwner    : TComponent);Override; //Cria o Componente
  Destructor  Destroy;Override;
  Procedure   Kickuser (aUser     : String);
 Published
  Property Active                 : Boolean                  Read vActive                 Write SetActive;
  {$IFDEF FPC}
  Property DatabaseCharSet        : TDatabaseCharSet         Read vDatabaseCharSet        Write vDatabaseCharSet;
  {$ENDIF}
  Property RequestTimeout         : Integer                  Read vServiceTimeout         Write vServiceTimeout;
  Property ServicePort            : Integer                  Read vServicePort            Write vServicePort Default 9092;
  Property GarbageTime            : Integer                  Read vGarbageTime            Write vGarbageTime;
  Property OnConnect              : TAegysOnSession          Read vOnConnect              Write vOnConnect;
  Property OnDisconnect           : TAegysOnSession          Read vOnDisconnect           Write vOnDisconnect;
  Property OnClientRequestExecute : TAegysOnSession          Read vOnClientRequestExecute Write vOnClientRequestExecute;
  Property OnGetClientDetails     : TAegysOnGetClientDetails Read vOnGetClientDetails     Write vOnGetClientDetails;
End;


Type
 TAegysClientSession     = Class;
 TAegysClientSessionList = Class(TList)
  Private
   Function  GetRec (Index : Integer) : TAegysClientSession;Overload;
   Procedure PutRec (Index : Integer;
                     Item  : TAegysClientSession);          Overload;
   Procedure ClearAll;
  Protected
  Public
   Destructor Destroy;                                      Override;
   Procedure  Delete(Index : Integer);                      Overload;
   Function   Add   (Item  : TAegysClientSession) : Integer;Overload;
   Property   Items [Index : Integer]             : TAegysClientSession Read GetRec Write PutRec; Default;
  End;
 PAegysClientSession     = ^TAegysClientSession;
 TAegysClientSession     = Class
 Private
  vConnection,
  vSessionID   : String;
 Public
  Constructor Create;
  Destructor  Destroy;Override;
  Property    Connection : String  Read vConnection Write vConnection;
  Property    SessionID  : String  Read vSessionID  Write vSessionID;
End;

Type
 TAegysClient = Class(TComponent)
 Protected
  //Variáveis, Procedures e  Funções Protegidas
  vTcpRequest             : TIdTCPClient;
 Private
  //Variáveis, Procedures e Funções Privadas
  vAcceptUnAssist,
  vAcceptStream,
  vAcceptFileStream,
  vActive                 : Boolean;
  vConnection,
  vSessionID,
  vSessionPWD,
  vSessionFixedPWD,
  vHost                   : String;
  vPort,
  vRequestTimeOut,
  vConnectTimeOut         : Integer;
  {$IFDEF FPC}
  vDatabaseCharSet        : TDatabaseCharSet;
  {$ENDIF}
  aPackList               : TPackList;
  vAegysClientSessionList : TAegysClientSessionList;
  vProcessData            : TAegysThread;
  vOnReceiveCommand       : TAegysOnReceiveCommand;
  vOnReceiveBytes         : TAegysOnReceiveBytes;
  vOnReceiveStream        : TAegysOnReceiveStream;
  vOnReceiveFileStream    : TAegysOnReceiveFileStream;
  vOnBeforeConnect        : TAegysOnBeforeConnect;
  vOnConnect              : TAegysOnConnect;
  vOnDisconnect           : TAegysOnDisconnect;
  Procedure KillThread;
  Procedure OnBeforeExecuteData (Var aPackList   : TPackList);
  Procedure OnExecuteData       (Var aPackList   : TPackList;
                                 aPackNo         : AeInt64);
  Procedure OnAbortData;
  Procedure OnThreadRequestError(ErrorCode       : Integer;
                                 MessageError    : String);
  Procedure OnServiceCommands   (InternalCommand : TInternalCommand;
                                 Command         : String);
  Procedure OnRequestDisconnect (Sender          : TObject);
  Procedure SetActive           (Value           : Boolean);
 Public
  //Métodos, Propriedades, Variáveis, Procedures e Funções Publicas
  Constructor Create        (AOwner      : TComponent);      Override;
  Destructor  Destroy;                                       Override;
  Procedure   ThreadDisconnect;
  Procedure   SetSessionData(ConnectionString,
                             ID,
                             PWD         : String);
  Procedure   Connect;
  Procedure   Disconnect;
  Procedure   GetConnectedList;
  Procedure   Join       (aID,
                          aPass,
                          aVideoQ        : String);
  Procedure   SendMessage(Value          : String);         Overload;
  Procedure   SendMessage(aID, Value     : String);         Overload;
  Procedure   SendStream (aID            : String;
                          Const aStream  : TStream);        Overload;
  Procedure   SendStream (Const aStream  : TStream);        Overload;
  Procedure   SendBytes  (aID            : String;
                          aBuffer        : TAegysBytes);    Overload;
  Procedure   SendBytes  (aBuffer        : TAegysBytes);    Overload;
 Published
  //Métodos e Propriedades
  Property    Active                     : Boolean                   Read vActive              Write SetActive;
  Property    AcceptStream               : Boolean                   Read vAcceptStream        Write vAcceptStream;
  Property    AcceptFileStream           : Boolean                   Read vAcceptFileStream    Write vAcceptFileStream;
  Property    Host                       : String                    Read vHost                Write vHost;
  Property    Port                       : Integer                   Read vPort                Write vPort Default 9092;
  Property    Connection                 : String                    Read vConnection          Write vConnection;
  Property    SessionID                  : String                    Read vSessionID           Write vSessionID;
  Property    SessionPWD                 : String                    Read vSessionPWD          Write vSessionPWD;
  Property    AcceptUnAssist             : Boolean                   Read vAcceptUnAssist      Write vAcceptUnAssist;
  Property    SessionFixedPWD            : String                    Read vSessionFixedPWD     Write vSessionFixedPWD;
  Property    RequestTimeOut             : Integer                   Read vRequestTimeOut      Write vRequestTimeOut;
  Property    ConnectTimeOut             : Integer                   Read vConnectTimeOut      Write vConnectTimeOut;
  {$IFDEF FPC}
  Property    DatabaseCharSet            : TDatabaseCharSet          Read vDatabaseCharSet     Write vDatabaseCharSet;
  {$ENDIF}
  Property    OnReceiveCommand           : TAegysOnReceiveCommand    Read vOnReceiveCommand    Write vOnReceiveCommand;
  Property    OnReceiveBytes             : TAegysOnReceiveBytes      Read vOnReceiveBytes      Write vOnReceiveBytes;
  Property    OnReceiveStream            : TAegysOnReceiveStream     Read vOnReceiveStream     Write vOnReceiveStream;
  Property    OnReceiveFileStream        : TAegysOnReceiveFileStream Read vOnReceiveFileStream Write vOnReceiveFileStream;
  Property    OnBeforeConnect            : TAegysOnBeforeConnect     Read vOnBeforeConnect     Write vOnBeforeConnect;
  Property    OnConnect                  : TAegysOnConnect           Read vOnConnect           Write vOnConnect;
  Property    OnDisconnect               : TAegysOnDisconnect        Read vOnDisconnect        Write vOnDisconnect;
End;

Implementation

Procedure TAegysService.Connect(AContext : TIdContext);
Var
 AegysSession : TAegysSession;
Begin
 AegysSession                 := TAegysSession.Create;
 AegysSession.Connection      := AContext.Connection.Socket.Binding.PeerIP + ':' + IntToStr(AContext.Connection.Socket.Binding.PeerPort);
 AegysSession.Socket          := AContext;
 {$IFNDEF FPC}
  {$IF Not Defined(HAS_FMX)}
   AContext.Data              := AegysSession;
  {$ELSE}
   {$IFDEF HAS_UTF8}
    {$IF CompilerVersion > 33}AContext.Data{$ELSE}AContext.DataObject{$IFEND} := RESTDwSession;
   {$ELSE}
    AContext.DataObject        := RESTDwSession;
   {$ENDIF}
  {$IFEND}
 {$ELSE}
  AContext.Data                := RESTDwSession;
 {$ENDIF}
 AegysSession.vInitalRequest  := Now;
 AegysSession.vLastRequest    := AegysSession.vInitalRequest;
// AegysSession.OnSessionError  := vRESTDwSessionError;
 AegysSession.vLogged         := True;
 If AegysSession.vLogged Then
  AegysSession.vAegysClientStage := csWaitLogon
 Else
  AegysSession.vAegysClientStage := csNone;
 vSessionList.Add(AegysSession);
 If Assigned(vOnConnect) Then
  vOnConnect(AegysSession);
End;

Constructor TAegysService.Create(AOwner: TComponent);
Begin
 Inherited;
 vOnGetClientDetails        := Nil;
 vGarbageTime               := 60000;
 vServiceTimeout            := 5000;
 vServicePort               := 9092;
 vIdTCPServer               := TIdTCPServer.Create(Nil);
 vSessionList               := TSessionList.Create;
 {$IFDEF FPC}
  vIdTCPServer.OnExecute    := @Execute;
  vIdTCPServer.OnDisconnect := @Disconnect;
  vIdTCPServer.OnConnect    := @Connect;
  vEncoding                 := esUtf8;
  vDatabaseCharSet          := csUndefined;
 {$ELSE}
  vIdTCPServer.OnExecute    := Execute;
  vIdTCPServer.OnDisconnect := Disconnect;
  vIdTCPServer.OnConnect    := Connect;
 {$ENDIF}
End;

Destructor TAegysService.Destroy;
Begin
 vIdTCPServer.Active := False;
 FreeAndNil(vSessionList);
 FreeAndNil(vIdTCPServer);
 Inherited;
End;

Procedure TAegysService.Disconnect(AContext: TIdContext);
Begin
 {$IFNDEF FPC}
  {$IF Not Defined(HAS_FMX)}
 If Assigned(AContext.Data) Then
  {$ELSE}
   {$IFDEF HAS_UTF8}
    If Assigned({$IF CompilerVersion > 33}AContext.Data{$ELSE}AContext.DataObject{$IFEND}) Then
   {$ELSE}
    If Assigned(AContext.DataObject) Then
   {$ENDIF}
  {$IFEND}
 {$ELSE}
 If Assigned(AContext.Data) Then
 {$ENDIF}
  Begin
   //TODO Delete my connection from others lists
   vSessionList.Delete(TAegysSession(AContext.Data));
   //Release my connection
   {$IFNDEF FPC}
    {$IF Not Defined(HAS_FMX)}
     If Assigned(vOnDisconnect) Then
      vOnDisconnect(TAegysSession(AContext.Data));
     TAegysSession(AContext.Data).Free;
     AContext.Data        := Nil;
    {$ELSE}
     {$IFDEF HAS_UTF8}
      If Assigned(vOnDisconnect) Then
       vOnDisconnect(TAegysSessionData(TAegysSession({$IF CompilerVersion > 33}AContext.Data{$ELSE}AContext.DataObject{$IFEND})));
      TAegysSession({$IF CompilerVersion > 33}AContext.Data{$ELSE}AContext.DataObject{$IFEND}).DisposeOf;
      {$IF CompilerVersion > 33}AContext.Data{$ELSE}AContext.DataObject{$IFEND} := Nil;
     {$ELSE}
      If Assigned(vOnDisconnect) Then
       vOnDisconnect(TAegysSessionData(TAegysSession(AContext.DataObject)));
      TAegysSession(AContext.DataObject).DisposeOf;
      AContext.DataObject := Nil;
     {$ENDIF}
    {$IFEND}
   {$ELSE}
    If Assigned(vOnDisconnect) Then
     vOnDisconnect(TAegysSessionData(TAegysSession(AContext.Data)));
    TAegysSession(AContext.Data).Free;
    AContext.Data         := Nil;
   {$ENDIF}
  End;
End;

Function TSessionList.GetConnection(aSocket : TIdContext) : TAegysSession;
Var
 I : Integer;
Begin
 Result := Nil;
 For I := Count -1 DownTo 0 Do
  Begin
   If Items[I].Socket = aSocket Then
    Begin
     Result := Items[I];
     Break;
    End;
  End;
End;

Function TSessionList.GetConnection(aConnStr   : String)  : TAegysSession;
Var
 I : Integer;
Begin
 Result := Nil;
 For I := Count -1 DownTo 0 Do
  Begin
   If Items[I].Connection = aConnStr Then
    Begin
     Result := Items[I];
     Break;
    End;
  End;
End;

Function TSessionList.GetConnection(aID, aPass : String)  : TAegysSession;
Var
 I : Integer;
Begin
 Result := Nil;
 For I := Count -1 DownTo 0 Do
  Begin
   If (Items[I].SessionID  = aID)   And
      (Items[I].SessionPWD = aPass) Then
    Begin
     Result := Items[I];
     Break;
    End;
  End;
End;

Function TSessionList.GetRec(Index  : Integer) : TAegysSession;
Begin
 Result := Nil;
 If (Index < Self.Count) And (Index > -1) Then
  Result := TAegysSession(TList(Self).Items[Index]^);
End;

Procedure TSessionList.PutRec(Index  : Integer;
                              Item   : TAegysSession);
Begin
 If (Index < Self.Count) And (Index > -1) Then
  TAegysSession(TList(Self).Items[Index]^) := Item;
End;

Destructor TSessionList.Destroy;
Begin
 ClearAll;
 Inherited;
End;

Function TSessionList.Add(Item : TAegysSession)    : Integer;
Var
 vItem: PAegysSession;
Begin
 New(vItem);
 vItem^        := Item;
 Result        := Inherited Add(vItem);
End;

Procedure TSessionList.Delete (Value  : TAegysSession);
Var
 I : Integer;
Begin
 For I := Count -1 Downto 0 Do
  Begin
   If Items[I] = Value Then
    Begin
     Delete(I);
     Break;
    End;
  End;
End;

Procedure TSessionList.Delete (Index  : Integer);
Begin
 If (Index > -1)        And
    (Index <= Count -1) Then
  Begin
   Try
    {$IFDEF FPC}
     Dispose(PAegysSession(TList(Self).Items[Index]));
    {$ELSE}
     Dispose(TList(Self).Items[Index]);
    {$ENDIF}
   Except
   End;
   TList(Self).Delete(Index);
  End;
End;

Procedure TSessionList.ClearAll;
Var
 I : Integer;
Begin
 I := Count - 1;
 While I > -1 Do
  Begin
   Delete(I);
   Dec(I);
  End;
 Inherited Clear;
End;

Procedure TAegysService.Execute(AContext : TIdContext);
Type
 TArrayOfPointer = Array of Pointer;
Var
 vMySession,
 vYouSession,
 vAegysSession    : TAegysSession;
 vAccept          : Boolean;
 aBuf             : TAegysBytes;
 aPackClass       : TPackClass;
 vCommand,
 vMyID,
 vYouID,
 vYouPass,
 vBestQ,
 vUser,
 vPassword        : String;
 vInternalCommand : TInternalCommand;
 ArrayOfPointer   : TArrayOfPointer;
 Procedure ParseCommand(Var Command         : String;
                        Var InternalCommand : TInternalCommand);
 Begin
  InternalCommand := ticNone;
  If Copy(Command, InitStrPos, Length(cStatusDesc))           = cStatusDesc      Then
   Begin
    InternalCommand := ticDataStatus;
    Delete(Command, InitStrPos, Length(cStatusDesc));
   End
  Else If Copy(Command, InitStrPos, Length(cFindID))          = cFindID          Then
   Begin
    InternalCommand := ticFindID;
    Delete(Command, InitStrPos, Length(cFindID));
   End
  Else If Copy(Command, InitStrPos, Length(cPing))            = cPing            Then
   Begin
    InternalCommand := ticPing;
    Delete(Command, InitStrPos, Length(cPing));
   End
  Else If Copy(Command, InitStrPos, Length(cCheckPass))       = cCheckPass       Then
   Begin
    InternalCommand := ticCheckPass;
    Delete(Command, InitStrPos, Length(cCheckPass));
   End
  Else If Copy(Command, InitStrPos, Length(cGetMonitorCount)) = cGetMonitorCount Then
   Begin
    InternalCommand := ticGetMonitorCount;
    Delete(Command, InitStrPos, Length(cGetMonitorCount));
   End
  Else If Copy(Command, InitStrPos, Length(cChangeMonitor))   = cChangeMonitor   Then
   Begin
    InternalCommand := ticChangeMonitor;
    Delete(Command, InitStrPos, Length(cChangeMonitor));
   End
  Else If Copy(Command, InitStrPos, Length(cRelation))        = cRelation        Then
   Begin
    InternalCommand := ticRelation;
    Delete(Command, InitStrPos, Length(cRelation));
   End
 End;
 Procedure ParseValues(Source : String;
                       Values : TArrayOfPointer);
 Var
  I : Integer;
  PString : ^String;
 Begin
  For I := 0 To Length(Values)-1 Do
   Begin
    PString := Values[I];
    If Pos('&', Source) > 0 then
     Begin
      PString^ := Copy(Source, InitStrPos, Pos('&', Source) -1);
      Delete(Source, InitStrPos, Pos('&', Source));
     End
    Else
     Begin
      PString^ := Copy(Source, InitStrPos, Length(Source));
      Delete(Source, InitStrPos, Length(Source));
     End;
    If Source = '' Then
     Break;
   End;
 End;
Begin
 {$IFNDEF FPC}
  {$IF Not Defined(HAS_FMX)}
   vAegysSession := TAegysSession(AContext.Data);
  {$ELSE}
   {$IFDEF HAS_UTF8}
    vAegysSession := TAegysSession({$IF CompilerVersion > 33}AContext.Data{$ELSE}AContext.DataObject{$IFEND});
   {$ELSE}
    vAegysSession := TAegysSession(AContext.DataObject);
   {$ENDIF}
  {$IFEND}
 {$ELSE}
  vAegysSession := TAegysSession(AContext.Data);
 {$ENDIF}
 Try
  If Assigned(vAegysSession) Then
   Begin
    AContext.Connection.IOHandler.CheckForDisconnect;
    vAccept := True;
    AContext.Connection.IOHandler.ReadBytes(TIdBytes(aBuf), -1);
    If vAegysSession.vAegysClientStage = csWaitLogon Then
     Begin
      If Length(aBuf) > 0 Then
       Begin
        aPackClass := TPackClass.Create;
        Try
         aPackClass.FromBytes(aBuf);
         If Assigned(vOnGetClientDetails) Then
          vOnGetClientDetails(vSessionList, aPackClass.Command, vUser, vPassword, vAccept);
        Finally
         FreeAndNil(aPackClass);
        End;
        If vAccept Then
         Begin
          vAegysSession.vSessionID        := vUser;
          vAegysSession.vSessionPWD       := vPassword;
          vAegysSession.vAegysClientStage := csLoggedIn;
          aPackClass                      := TPackClass.Create;
          Try
           aPackClass.DataMode  := tdmServerCommand;
           aPackClass.DataCheck := tdcAsync;
           aPackClass.Command   := cStatusDesc              + '|' +
                                   vAegysSession.Connection + '|' +
                                   vAegysSession.vSessionID + '|' +
                                   vAegysSession.vSessionPWD;
           aBuf                 := aPackClass.ToBytes;
           vAegysSession.SendBytes(aBuf);
          Finally
           FreeAndNil(aPackClass);
          End;
         End
        Else
         vAegysSession.vAegysClientStage  := csRejected;
       End;
     End
    Else If vAegysSession.vAegysClientStage = csLoggedIn Then
     Begin
      If Length(aBuf) > 0 Then
       Begin
        aPackClass := TPackClass.Create;
        Try
         aPackClass.FromBytes(aBuf);
         If aPackClass.DataMode      = tdmServerCommand Then
          Begin
           If aPackClass.DataType      = tdtString Then
            Begin
             vCommand := aPackClass.Command;
             ParseCommand(vCommand, vInternalCommand);
             If vInternalCommand <> ticNone Then
              Begin
               Case vInternalCommand Of
                ticLogin           : Begin

                                     End;
                ticConnectionList  : Begin

                                     End;
                ticConnections     : Begin

                                     End;
                ticDataStatus      : Begin

                                     End;
                ticFindID          : Begin

                                     End;
                ticPing            : Begin

                                     End;
                ticCheckPass       : Begin

                                     End;
                ticGetMonitorCount : Begin

                                     End;
                ticChangeMonitor   : Begin

                                     End;
                ticRelation        : Begin
                                      ArrayOfPointer := [@vMyID, @vYouID, @vYouPass, @vBestQ];
                                      ParseValues(vCommand, ArrayOfPointer);
                                      vMySession  := vSessionList.GetConnection(vAegysSession.Socket);
                                      vYouSession := vSessionList.GetConnection(vYouID, vYouPass);
                                      If (vYouSession = Nil) Then //Error defined, no have connection
                                       Begin
                                        aPackClass := TPackClass.Create;
                                        Try
                                         aPackClass.DataMode  := tdmServerCommand;
                                         aPackClass.DataCheck := tdcAsync;
                                         aPackClass.Command   := cIDNotFound;
                                         aBuf                 := aPackClass.ToBytes;
                                         vAegysSession.SendBytes(aBuf);
                                        Finally
                                         FreeAndNil(aPackClass);
                                        End;
                                       End
                                      Else //Continue to Relation
                                       Begin
                                        vMySession.SessionList.Add (vYouSession);
                                        vYouSession.SessionList.Add(vMySession);
                                        aPackClass := TPackClass.Create;
                                        Try
                                         aPackClass.DataMode  := tdmServerCommand;
                                         aPackClass.DataCheck := tdcAsync;
                                         aPackClass.Command   := cConnectedPeer + Format('%s&%s&%s', [vYouSession.Connection,
                                                                                                      vYouSession.vSessionID,
                                                                                                      vYouSession.SessionPWD]);
                                         aBuf                 := aPackClass.ToBytes;
                                         vMySession.SendBytes(aBuf);
                                         aPackClass.Command   := cConnectedPeer + Format('%s&%s&%s', [vMySession.Connection,
                                                                                                      vMySession.vSessionID,
                                                                                                      vMySession.SessionPWD]);
                                         aBuf                 := aPackClass.ToBytes;
                                         vYouSession.SendBytes(aBuf);
                                        Finally
                                         FreeAndNil(aPackClass);
                                        End;
                                       End
                                     End;
               End;
              End;
            End
           Else //Byte Commands
            Begin

            End;
          End
         Else //Other Options
          Begin

          End;
        Finally
         FreeAndNil(aPackClass);
        End;
        If Assigned(vOnClientRequestExecute) Then
         vOnClientRequestExecute(vAegysSession);
       End;
     End;
   End;
  If vAegysSession.vAegysClientStage = csRejected Then
   AContext.Connection.Disconnect
  Else
   AContext.Connection.IOHandler.CheckForDisconnect(True, True);
 Except
  On E : EIdSocketError Do
   Begin
//     If pos('10053', E.Message) > 0 Then
//      ThreadLogMessage(lmtInformation, ldNone, 'Client disconnected')
//     Else
//      ThreadLogMessage(lmtError, ldNone, E.Message);
    Raise;
   End;
  On E : EIdReadTimeout Do ;
  On E : Exception      Do
   Begin
    Abort;
//    If Pos('CONNECTION CLOSED GRACEFULLY', uppercase(e.Message)) > 0 Then
//     Begin
//      Raise;
//     End
//    Else
//     Begin
//      Raise;
//     End;
   End;
 End;
 Sleep(cDelayThread);
End;

Procedure TAegysService.Kickuser(aUser : String);
Begin

End;

Procedure TAegysService.ProcessMessages;
Begin
 {$IFNDEF FPC}
  {$IF Defined(HAS_FMX)}{$IF Not Defined(HAS_UTF8)}FMX.Forms.TApplication.ProcessMessages;{$IFEND}
  {$ELSE}Application.Processmessages;{$IFEND}
 {$ELSE}
  Application.Processmessages;
 {$ENDIF}
End;

Procedure TAegysService.SetActive(Value : Boolean);
Begin
 vIdTCPServer.DefaultPort := vServicePort;
 If Value Then
  vIdTCPServer.Bindings.DefaultPort := vServicePort;
 Try
  If (vIdTCPServer.Active) And (Not (Value)) Then
   Begin
    Try
     vIdTCPServer.Active := False;// .StopListening;
     vIdTCPServer.Bindings.Clear;
     vIdTCPServer.Contexts.Clear;
    Finally
     ProcessMessages;
//     vIdTCPServer.Active := Value;
    End;
   End
  Else
   vIdTCPServer.Active  := Value;
 Except
  On E : Exception Do
   Raise Exception.Create(E.Message);
 End;
 vActive                   := vIdTCPServer.Active;
End;

constructor TAegysSession.Create;
begin
 vAContext           := Nil;
 vSessionID          := '';
 vSessionPWD         := '';
 vSessionFixedPWD    := '';
 vAcceptUnAssist     := False;
 vLastRequest        := Now;
 vLogged             := False;
 {$IFNDEF FPC}
  {$IF (DEFINED(OLDINDY))}
   vDataEncoding := enDefault;
  {$ELSE}
   vDataEncoding := IndyTextEncoding_UTF8;
  {$IFEND}
 {$ELSE}
  vDataEncoding := IndyTextEncoding_UTF8;
 {$ENDIF};
 vAegysSessionList := TAegysSessionList.Create;
end;

Destructor TAegysSession.Destroy;
Begin
 FreeAndNil(vAegysSessionList);
 Inherited;
End;

procedure TAegysSession.Kick(Gracefully: Boolean);
begin
 Try
  If Assigned(vAContext) Then
   Begin
    If Not Gracefully Then
     vAContext.Connection.IOHandler.Close
    Else
     Begin
      vAContext.Connection.IOHandler.CloseGracefully;
      vAContext.Connection.IOHandler.CheckForDisconnect(False, False);
     End;
   End;
 Except
  On e : Exception Do
   Begin
   End;
 End;
end;

Function TAegysSessionList.GetRec (Index  : Integer) : TAegysSession;
Begin
 Result := Nil;
 If (Index < Self.Count) And (Index > -1) Then
  Result := TAegysSession(TList(Self).Items[Index]^);
End;

Procedure TAegysSessionList.PutRec(Index  : Integer;
                                   Item   : TAegysSession);
Begin
 If (Index < Self.Count) And (Index > -1) Then
  TAegysSession(TList(Self).Items[Index]^) := Item;
End;

Procedure TAegysSessionList.ClearAll;
Var
 I : Integer;
Begin
 I := Count - 1;
 While I > -1 Do
  Begin
   Delete(I);
   Dec(I);
  End;
 Inherited Clear;
End;

Destructor TAegysSessionList.Destroy;
Begin
 ClearAll;
 Inherited;
End;

Function TAegysSessionList.Add    (Item   : TAegysSession)    : Integer;
Var
 vItem: PAegysSession;
Begin
 New(vItem);
 vItem^        := Item;
 Result        := Inherited Add(vItem);
End;

Procedure TAegysSessionList.Delete(Index  : Integer);
Begin
 If (Index > -1)        And
    (Index <= Count -1) Then
  Begin
   Try
    {$IFDEF FPC}
     Dispose(PAegysSession(TList(Self).Items[Index]));
    {$ELSE}
     Dispose(TList(Self).Items[Index]);
    {$ENDIF}
   Except
   End;
   TList(Self).Delete(Index);
  End;
End;

procedure TAegysSession.ProcessMessages;
begin
 {$IFNDEF FPC}
  {$IF Defined(HAS_FMX)}{$IF Not Defined(HAS_UTF8)}FMX.Forms.TApplication.ProcessMessages;{$IFEND}
  {$ELSE}Application.Processmessages;{$IFEND}
 {$ELSE}
  Application.Processmessages;
 {$ENDIF}
end;

Function TAegysSession.ReceiveString : String;
Begin
 If Assigned(vAContext) then
  Result := vAContext.Connection.IOHandler.ReadLn;
End;

Procedure TAegysSession.SendBytes(aBuf      : TAegysBytes;
                                  WaitReply : Boolean);
Begin
 If Assigned(vAContext) then
  Begin
   vAContext.Connection.IOHandler.WriteDirect(TIdBytes(aBuf), -1);
   If Not WaitReply Then
    vAContext.Connection.IOHandler.WriteBufferFlush;
   ProcessMessages;
  End;
End;

Procedure TAegysSession.SendStream(Var aStream : TMemoryStream;
                                   WaitReply   : Boolean);
Var
 aBuf      : TIdBytes;
 vSizeFile : AeInt64;
 bStream   : TMemoryStream;
Begin
 If Assigned(vAContext) then
  Begin
   SetLength(aBuf, aStream.Size);
   vSizeFile := Length(aBuf);
   bStream   := TMemoryStream.Create;
   Try
    bStream.CopyFrom(aStream, aStream.Size);
    bStream.Position := 0;
    bStream.Read(Pointer(aBuf)^, Length(aBuf));
    vAContext.Connection.IOHandler.Write(ToBytes(vSizeFile), SizeOf(AeInt64));
    vAContext.Connection.IOHandler.WriteBufferFlush;
    vAContext.Connection.IOHandler.Write(aBuf);
    vAContext.Connection.IOHandler.WriteBufferFlush;
   Finally
    SetLength(aBuf, 0);
    FreeAndNil(bStream);
    ProcessMessages;
   End;
  End;
End;

procedure TAegysSession.SendString(S : String; WaitReply: Boolean);
Var
 aBuf : TIdBytes;
Begin
 If Assigned(vAContext) then
  Begin
   aBuf := ToBytes(S, vDataEncoding);
   SendBytes (TAegysBytes(aBuf), WaitReply);
   SetLength(aBuf, 0);
  End;
End;

{ TAegysClient }

Procedure TAegysClient.Connect;
Begin
 SetActive(True);
End;

Constructor TAegysClient.Create(AOwner: TComponent);
Begin
 Inherited;
 vTcpRequest  := TIdTCPClient.Create(Nil);
 aPackList    := TPackList.Create;
 vProcessData := Nil;
End;

Destructor TAegysClient.Destroy;
Begin
 SetActive(False);
 FreeAndNil(vTcpRequest);
 FreeAndNil(aPackList);
 Inherited;
End;

Procedure TAegysClient.Disconnect;
Begin
 SetActive(False);
End;

Procedure TAegysClient.GetConnectedList;
Begin

End;
Procedure TAegysClient.Join(aID, aPass, aVideoQ : String);
Begin
 If vTcpRequest.Connected Then
  aPackList.Add(vConnection, '', tdmServerCommand, tdcAsync, Format(cRelation + '%s&%s&%s', [vSessionID, aID, aPass, aVideoQ]))
 Else
  Raise Exception.Create(cCantJoinDisconnected);
End;

Procedure TAegysClient.OnServiceCommands(InternalCommand : TInternalCommand;
                                         Command         : String);
Begin
 Case InternalCommand Of
  ticLogin : Begin
              If Assigned(vOnReceiveCommand) Then
               vOnReceiveCommand(InternalCommand, Command);
             End;
 End;
End;

Procedure TAegysClient.OnAbortData;
Begin
 aPackList.Clear;
End;

Procedure TAegysClient.OnBeforeExecuteData (Var aPackList : TPackList);
 Procedure ReceiveStreamClient;
 Var
  aBuf  : TAegysBytes;
 Begin
  SetLength(aBuf, 0);
  If vTcpRequest.Connected Then
   Begin
    If Not (vTcpRequest.IOHandler.InputBufferIsEmpty) Then
     Begin
      Try
       vTcpRequest.IOHandler.InputBuffer.ExtractToBytes(tIdBytes(aBuf), -1);
       If (Length(aBuf) > 0) Then
        aPackList.Add(aBuf);
      Finally
       SetLength(aBuf, 0);
      End;
     End;
   End
  Else
   Raise Exception.Create(cDisconnectedByServer);
 End;
Begin
 If Assigned(aPackList) Then
  ReceiveStreamClient;
End;

Procedure TAegysClient.OnExecuteData       (Var aPackList : TPackList;
                                            aPackNo       : AeInt64);
Var
 aBuffer : TAegysBytes;
Begin
 If aPackNo <= aPackList.Count -1 Then
  aBuffer := aPackList.ReadPack(aPackNo);
 If Length(aBuffer) > 0 Then
  SendBytes(aBuffer);
End;

Procedure TAegysClient.OnRequestDisconnect(Sender : TObject);
Begin
 SetActive(False);
 vSessionID  := '';
 vSessionPWD := '';
 If Assigned(vOnDisconnect) Then
  vOnDisconnect(Self);
End;

Procedure TAegysClient.OnThreadRequestError(ErrorCode     : Integer;
                                            MessageError  : String);
Begin

End;

Procedure TAegysClient.SendBytes           (aID           : String;
                                            aBuffer       : TAegysBytes);
Begin

End;

Procedure TAegysClient.SendBytes           (aBuffer       : TAegysBytes);
Begin
 If vTcpRequest.Connected Then
  Begin
   vTcpRequest.IOHandler.WriteDirect(TIdBytes(aBuffer), Length(aBuffer));
   vTcpRequest.IOHandler.WriteBufferFlush;
  End;
End;

Procedure TAegysClient.SendMessage         (Value         : String);
Begin

End;

Procedure TAegysClient.SendMessage         (aID,
                                            Value         : String);
Begin

End;

Procedure TAegysClient.SendStream          (aID           : String;
                                            Const aStream : TStream);
Begin

End;

Procedure TAegysClient.SendStream          (Const aStream : TStream);
Begin

End;

Procedure TAegysClient.ThreadDisconnect;
Begin
 Try
  vTcpRequest.OnDisconnected := Nil;
  vTcpRequest.Disconnect;
 Finally
  vSessionID  := '';
  vSessionPWD := '';
  vActive     := False;
  If Assigned(vOnDisconnect) Then
   vOnDisconnect(Self);
 End;
End;

Procedure TAegysClient.KillThread;
Begin
 If Assigned(vProcessData) Then
  Begin
   Try
    vProcessData.Kill;
   Except
   End;
   {$IFDEF FPC}
    WaitForThreadTerminate(vProcessData.Handle, 100);
   {$ELSE}
    {$IF Not Defined(HAS_FMX)}
     WaitForSingleObject  (vProcessData.Handle, 100);
    {$IFEND}
   {$ENDIF}
   FreeAndNil(vProcessData);
   vActive := False;
  End;
End;

Procedure TAegysClient.SetActive(Value : Boolean);
Var
 vWelcomeMessage : String;
Begin
 KillThread;
 If (Value) Then
  Begin
   If Not vTcpRequest.Connected Then
    Begin
     vTcpRequest.Host           := vHost;
     vTcpRequest.Port           := vPort;
     vTcpRequest.ReadTimeout    := vRequestTimeOut;
     vTcpRequest.OnDisconnected := OnRequestDisconnect;
     Try
      vTcpRequest.Connect;
      If Assigned(vOnBeforeConnect) Then
       Begin
        vOnBeforeConnect(Self, vWelcomeMessage);
        aPackList.Add('', '', tdmServerCommand, tdcAsync, vWelcomeMessage);
       End;
      If vTcpRequest.Connected Then
       Begin
        vProcessData         := TAegysThread.Create(aPackList,
                                                    TComponent(Self),
                                                    cDelayThread,
                                                    OnBeforeExecuteData,
                                                    OnReceiveBytes,
                                                    OnExecuteData,
                                                    OnAbortData,
                                                    vOnReceiveCommand,
                                                    OnThreadRequestError);
        vProcessData.Resume;
        vActive := Value;
        If Assigned(vOnConnect) Then
         vOnConnect(Self);
       End;
     Except
      On E : Exception Do
       Begin
        vActive := False;
        Raise Exception.Create(E.Message);
       End;
     End;
    End;
  End
 Else
  vTcpRequest.Disconnect;
End;

Procedure TAegysClient.SetSessionData(ConnectionString, ID, PWD: String);
Begin
 vSessionID  := ID;
 vSessionPWD := PWD;
 vConnection := ConnectionString;
End;

{ TAegysClientSession }

Constructor TAegysClientSession.Create;
Begin
 vConnection := '';
 vSessionID  := '';
End;

Destructor TAegysClientSession.Destroy;
Begin
 vConnection := '';
 vSessionID  := '';
 Inherited;
End;

{ TAegysClientSessionList }

Function TAegysClientSessionList.Add(Item : TAegysClientSession): Integer;
Var
 vItem: PAegysClientSession;
Begin
 New(vItem);
 vItem^        := Item;
 Result        := Inherited Add(vItem);
End;

procedure TAegysClientSessionList.ClearAll;
Var
 I : Integer;
Begin
 I := Count - 1;
 While I > -1 Do
  Begin
   Delete(I);
   Dec(I);
  End;
 Inherited Clear;
End;

Procedure TAegysClientSessionList.Delete(Index : Integer);
Begin
 If (Index > -1)        And
    (Index <= Count -1) Then
  Begin
   Try
    If Assigned(TList(Self).Items[Index]) Then
     Begin
      If Assigned(TAegysClientSession(TList(Self).Items[Index]^)) Then
       Begin
       {$IFDEF FPC}
        FreeAndNil(TList(Self).Items[Index]^);
       {$ELSE}
        {$IF CompilerVersion > 33}
         FreeAndNil(TAegysClientSession(TList(Self).Items[Index]^));
        {$ELSE}
         FreeAndNil(TList(Self).Items[Index]^);
        {$IFEND}
       {$ENDIF}
       End;
     End;
    {$IFDEF FPC}
     Dispose(PAegysClientSession(TList(Self).Items[Index]));
    {$ELSE}
     Dispose(TList(Self).Items[Index]);
    {$ENDIF}
   Except
   End;
   TList(Self).Delete(Index);
  End;
End;

Destructor TAegysClientSessionList.Destroy;
Begin
 ClearAll;
 Inherited;
End;

Function TAegysClientSessionList.GetRec(Index : Integer): TAegysClientSession;
Begin
 Result := Nil;
 If (Index < Self.Count) And (Index > -1) Then
  Result := TAegysClientSession(TList(Self).Items[Index]^);
End;

Procedure TAegysClientSessionList.PutRec(Index : Integer;
                                         Item  : TAegysClientSession);
Begin
 If (Index < Self.Count) And (Index > -1) Then
  TAegysClientSession(TList(Self).Items[Index]^) := Item;
End;

end.
