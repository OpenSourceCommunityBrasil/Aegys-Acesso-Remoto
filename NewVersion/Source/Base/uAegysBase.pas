Unit uAegysBase;

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
 //Delphi Basic Units
 SysUtils, Classes, Variants,
 {$IF Defined(HAS_FMX)}{$IF Not Defined(HAS_UTF8)}FMX.Forms{$IFEND}
 {$ELSE}Vcl.Forms{$ELSE}Forms{$IFEND}
 {$IFDEF MSWINDOWS}, Windows,{$ENDIF}
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
   Function  GetRec         (Index         : Integer)        : TAegysSession;Overload;
   Procedure PutRec         (Index         : Integer;
                             Item          : TAegysSession);                 Overload;
   Procedure ClearAll;
  Protected
  Public
   Destructor Destroy; Override;
   Function   GetConnection (aConnStr      : String)         : TAegysSession;
   Procedure  Delete        (Index         : Integer);                       Overload;
   Function   Add           (Item          : TAegysSession)  : Integer;      Overload;
   Property   Items         [Index         : Integer]        : TAegysSession Read GetRec Write PutRec; Default;
  End;
 TAegysSessionData  = Class(TObject);
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
  vBeginPing,
  vLatency          : Integer;
  vAegysSessionList : TAegysSessionList;
  vPingSense        : TPingSense;
 Public
  Procedure   Ping;
  Function    ReceiveString            : String;
  Procedure   SendString(S             : String);
  Procedure   SendStream(Var aStream   : TMemoryStream;
                         WaitReply     : Boolean = False);
  Procedure   SendBytes (aBuf          : TAegysBytes;
                         ToMyConnList  : Boolean);Overload;
  Procedure   SendBytes (aDest         : String;
                         aBuf          : TAegysBytes);    Overload;
  Procedure   SendBytes (aBuf          : TAegysBytes);    Overload;
  Procedure   Kick      (Gracefully    : Boolean = False);
  Constructor Create;
  Destructor  Destroy;Override;
  Property    Socket                   : TIdContext          Read vAContext               Write vAContext;
  Property    Connection               : String              Read vConnection             Write vConnection;
  Property    ClientStage              : TAegysClientStage   Read vAegysClientStage;
  Property    SessionID                : String              Read vSessionID              Write vSessionID;
  Property    SessionPWD               : String              Read vSessionPWD             Write vSessionPWD;
  Property    AcceptUnAssist           : Boolean             Read vAcceptUnAssist         Write vAcceptUnAssist;
  Property    SessionFixedPWD          : String              Read vSessionFixedPWD        Write vSessionFixedPWD;
  Property    InitalRequest            : TDateTime           Read vInitalRequest;
  Property    LastRequest              : TDateTime           Read vLastRequest;
  Property    Latency                  : Integer             Read vLatency;
  Property    Logged                   : Boolean             Read vLogged;
  Property    SessionList              : TAegysSessionList   Read vAegysSessionList       Write vAegysSessionList;
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
 TAegysOnServerLogin       = Procedure (Sender            : TObject)        Of Object;
 TAegysOnClientCommand     = Procedure (Connection,
                                        ID,
                                        Command         : String)           Of Object;
 TAegysOnClientBuf         = Procedure (Connection,
                                        ID,
                                        Command         : String;
                                        aBuf            : TAegysBytes)      Of Object;
 TAegysOnReceiveCommand    = Procedure (InternalCommand   : TInternalCommand;
                                        Command           : String)         Of Object;
 TAegysOnPeerConnected     = Procedure (Connection        : String;
                                        Var ClientID,
                                        ClientPassword,
                                        Alias             : String)         Of Object;
 TAegysOnPeerDisconnected  = Procedure (Connection        : String;
                                        Var ClientID,
                                        ClientPassword,
                                        Alias             : String)         Of Object;
 TAegysOnBeginTransaction  = Procedure (Connection        : String;
                                        Var ClientID,
                                        Alias             : String)         Of Object;
 TAegysOnBeginTransactionError = Procedure (Connection    : String)         Of Object;
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
  Procedure  SetActive (Value     : Boolean);
  Procedure  Disconnect(AContext  : TIdContext);
  Procedure  Connect   (AContext  : TIdContext);
  Function   GetSessionList       : TSessionList;
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
  Property SessionList            : TSessionList             Read vSessionList;
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
  vTcpRequest              : TIdTCPClient;
 Private
  //Variáveis, Procedures e Funções Privadas
  vAcceptUnAssist,
  vAcceptStream,
  vAcceptFileStream,
  vActive                  : Boolean;
  vConnection,
  vSessionID,
  vSessionPWD,
  vSessionFixedPWD,
  vHost                    : String;
  vPort,
  vSessionTime,
  vRequestTimeOut,
  vConnectTimeOut          : Integer;
  {$IFDEF FPC}
  vDatabaseCharSet         : TDatabaseCharSet;
  {$ENDIF}
  aPackList                : TPackList;
  vProcessData             : TAegysThread;
  vOnÌncommingConnect,
  vOnAccessGranted,
  vOnPeerConnected         : TAegysOnPeerConnected;
  vOnPeerDisconnected      : TAegysOnPeerDisconnected;
  vOnServerLogin           : TAegysOnServerLogin;
  vOnReceiveCommand        : TAegysOnReceiveCommand;
  vOnReceiveBytes          : TAegysOnReceiveBytes;
  vOnReceiveStream         : TAegysOnReceiveStream;
  vOnReceiveFileStream     : TAegysOnReceiveFileStream;
  vOnBeforeConnect         : TAegysOnBeforeConnect;
  vOnConnect               : TAegysOnConnect;
  vOnDisconnect            : TAegysOnDisconnect;
  vOnScreenCapture,
  vOnAudioCapture,
  vOnVideoCapture,
  vOnFileTransfer          : TAegysOnClientBuf;
  vOnKeyboardCapture,
  vOnMouseCapture,
  vOnChatReceive           : TAegysOnClientCommand;
  vOnBeginTransaction      : TAegysOnBeginTransaction;
  vOnBeginTransactionError : TAegysOnBeginTransactionError;
  Procedure KillThread;
  Procedure OnBeforeExecuteData (Var aPackList   : TPackList);
  Procedure OnExecuteData       (Var aPackList   : TPackList;
                                 aPackNo         : AeInt64);
  Procedure OnAbortData;
  Procedure OnThreadRequestError(ErrorCode       : Integer;
                                 MessageError    : String);
  Procedure OnServiceCommands   (InternalCommand : TInternalCommand;
                                 Command         : String);
  Procedure OnClientCommands    (CommandType     : TCommandType;
                                 Connection,
                                 ID,
                                 Command         : String;
                                 aBuf            : TAegysBytes);
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
  Procedure   DisconnectAllPeers;
  Procedure   DisconnectPeer(aID,
                             aPass,
                             aConnection : String);
  Procedure   Join          (aID,
                             aPass,
                             aVideoQ     : String);
  Procedure   SendCommand   (Value       : String);
  Procedure   SendMessage   (Value       : String;
                             aDestMyConn : Boolean;
                             CommandType : TCommandType = tctChat);         Overload;
  Procedure   SendMessage   (aID, Value  : String;
                             CommandType : TCommandType = tctChat);         Overload;
  Procedure   SendMouse     (aID, Value  : String);                         Overload;
  Procedure   SendMouse     (aDestMyConn : Boolean;
                             Value       : String);                         Overload;
  Procedure   SendMonitor   (aID, Value  : String);                         Overload;
  Procedure   SendMonitor   (aDestMyConn : Boolean;
                             Value       : String);                         Overload;
  Procedure   SendKeyboard  (aID, Value  : String);                         Overload;
  Procedure   SendKeyboard  (aDestMyConn : Boolean;
                             Value       : String);                         Overload;
  Procedure   SendBytes     (aID         : String;
                             aBuffer     : TAegysBytes;
                             CommandType : TCommandType = tctScreenCapture);Overload;
  Procedure   SendBytes     (aBuffer     : TAegysBytes;
                             aDestMyConn : Boolean;
                             CommandType : TCommandType = tctScreenCapture);Overload;
  Procedure   SendBytes     (aBuffer     : TAegysBytes);                    Overload;
 Published
  //Métodos e Propriedades
  Property    Active                     : Boolean                       Read vActive                  Write SetActive;
  Property    AcceptStream               : Boolean                       Read vAcceptStream            Write vAcceptStream;
  Property    AcceptFileStream           : Boolean                       Read vAcceptFileStream        Write vAcceptFileStream;
  Property    Host                       : String                        Read vHost                    Write vHost;
  Property    Port                       : Integer                       Read vPort                    Write vPort Default 9092;
  Property    Connection                 : String                        Read vConnection              Write vConnection;
  Property    SessionTime                : Integer                       Read vSessionTime             Write vSessionTime;
  Property    SessionID                  : String                        Read vSessionID               Write vSessionID;
  Property    SessionPWD                 : String                        Read vSessionPWD              Write vSessionPWD;
  Property    AcceptUnAssist             : Boolean                       Read vAcceptUnAssist          Write vAcceptUnAssist;
  Property    SessionFixedPWD            : String                        Read vSessionFixedPWD         Write vSessionFixedPWD;
  Property    RequestTimeOut             : Integer                       Read vRequestTimeOut          Write vRequestTimeOut;
  Property    ConnectTimeOut             : Integer                       Read vConnectTimeOut          Write vConnectTimeOut;
  Property    Connected                  : Boolean                       Read vActive;
  {$IFDEF FPC}
  Property    DatabaseCharSet            : TDatabaseCharSet              Read vDatabaseCharSet         Write vDatabaseCharSet;
  {$ENDIF}
  Property    OnReceiveCommand           : TAegysOnReceiveCommand        Read vOnReceiveCommand        Write vOnReceiveCommand;
  Property    OnReceiveBytes             : TAegysOnReceiveBytes          Read vOnReceiveBytes          Write vOnReceiveBytes;
  Property    OnReceiveStream            : TAegysOnReceiveStream         Read vOnReceiveStream         Write vOnReceiveStream;
  Property    OnReceiveFileStream        : TAegysOnReceiveFileStream     Read vOnReceiveFileStream     Write vOnReceiveFileStream;
  Property    OnBeforeConnect            : TAegysOnBeforeConnect         Read vOnBeforeConnect         Write vOnBeforeConnect;
  Property    OnConnect                  : TAegysOnConnect               Read vOnConnect               Write vOnConnect;
  Property    OnDisconnect               : TAegysOnDisconnect            Read vOnDisconnect            Write vOnDisconnect;
  Property    OnPeerConnected            : TAegysOnPeerConnected         Read vOnPeerConnected         Write vOnPeerConnected;
  Property    OnÌncommingConnect         : TAegysOnPeerConnected         Read vOnÌncommingConnect      Write vOnÌncommingConnect;
  Property    OnPeerDisconnected         : TAegysOnPeerDisconnected      Read vOnPeerDisconnected      Write vOnPeerDisconnected;
  Property    OnServerLogin              : TAegysOnServerLogin           Read vOnServerLogin           Write vOnServerLogin;
  Property    OnScreenCapture            : TAegysOnClientBuf             Read vOnScreenCapture         Write vOnScreenCapture;
  Property    OnAudioCapture             : TAegysOnClientBuf             Read vOnAudioCapture          Write vOnAudioCapture;
  Property    OnVideoCapture             : TAegysOnClientBuf             Read vOnVideoCapture          Write vOnVideoCapture;
  Property    OnFileTransfer             : TAegysOnClientBuf             Read vOnFileTransfer          Write vOnFileTransfer;
  Property    OnKeyboardCapture          : TAegysOnClientCommand         Read vOnKeyboardCapture       Write vOnKeyboardCapture;
  Property    OnMouseCapture             : TAegysOnClientCommand         Read vOnMouseCapture          Write vOnMouseCapture;
  Property    OnChatReceive              : TAegysOnClientCommand         Read vOnChatReceive           Write vOnChatReceive;
  Property    OnBeginTransaction         : TAegysOnBeginTransaction      Read vOnBeginTransaction      Write vOnBeginTransaction;
  Property    OnBeginTransactionError    : TAegysOnBeginTransactionError Read vOnBeginTransactionError Write vOnBeginTransactionError;
  Property    OnAccessGranted            : TAegysOnPeerConnected         Read vOnAccessGranted         Write vOnAccessGranted;
End;

  Procedure   ProcessMessages;

Implementation

Uses uAegysTools;

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
    {$IF CompilerVersion > 33}AContext.Data{$ELSE}AContext.DataObject{$IFEND} := AegysSession;
   {$ELSE}
    AContext.DataObject        := AegysSession;
   {$ENDIF}
  {$IFEND}
 {$ELSE}
  AContext.Data                := AegysSession;
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
 SetActive(False);
 FreeAndNil(vSessionList);
 FreeAndNil(vIdTCPServer);
 Inherited;
End;

Procedure TAegysService.Disconnect(AContext : TIdContext);
 Procedure BroadCastDisconnect(Const AegysSession : TAegysSession);
 Var
  I          : Integer;
  aPackClass : TPackClass;
  aBuf       : TAegysBytes;
 Begin
  aPackClass := TPackClass.Create;
  Try
   aPackClass.DataMode  := tdmServerCommand;
   aPackClass.DataCheck := tdcAsync;
   aPackClass.Command   := cDisconnectedPeer + Format('%s&%s&%s', [AegysSession.Connection,
                                                                   AegysSession.vSessionID,
                                                                   AegysSession.SessionPWD]);
   aBuf                 := aPackClass.ToBytes;
  Finally
   FreeAndNil(aPackClass);
  End;
  For I := 0 To AegysSession.SessionList.Count -1 Do
   Begin
    Try
     AegysSession.SessionList[I].SendBytes(aBuf);
    Except
    End;
   End;
 End;
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
   {$IFNDEF FPC}
    {$IF Not Defined(HAS_FMX)}
     BroadCastDisconnect(TAegysSession(AContext.Data));
     //TODO Delete my connection from others lists
     vSessionList.Delete(TAegysSession(AContext.Data));
    {$ELSE}
     BroadCastDisconnect(TAegysSession({$IF CompilerVersion > 33}AContext.Data{$ELSE}AContext.DataObject{$IFEND}));
     //TODO Delete my connection from others lists
     vSessionList.Delete(TAegysSession({$IF CompilerVersion > 33}AContext.Data{$ELSE}AContext.DataObject{$IFEND}));
    {$IFEND}
   {$ELSE}
     BroadCastDisconnect(TAegysSession(AContext.Data));
     //TODO Delete my connection from others lists
     vSessionList.Delete(TAegysSession(AContext.Data));
   {$ENDIF}
   //Release my connection
   {$IFNDEF FPC}
    {$IF Not Defined(HAS_FMX)}
     If Assigned(vOnDisconnect) Then
      vOnDisconnect(TAegysSession(AContext.Data));
     FreeAndNil(PAegysSession(@AContext.Data)^);
    {$ELSE}
     {$IFDEF HAS_UTF8}
      If Assigned(vOnDisconnect) Then
       vOnDisconnect(TAegysSession({$IF CompilerVersion > 33}AContext.Data{$ELSE}AContext.DataObject{$IFEND}));
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
    FreeAndNil(PAegysSession(@AContext.Data)^);
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
   If (Items[I].Connection = aConnStr) Or
      (Items[I].SessionID  = aConnStr) Then
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
Var
 vMySession,
 vYouSession,
 vAegysSession    : TAegysSession;
 vAccept          : Boolean;
 aBuf             : TAegysBytes;
 aPackClass       : TPackClass;
 aStream          : TStream;
 aFirstBufSize    : AEInteger;
 aBuffSize,
 bPackSize,
 aPackSize        : AEInt64;
 vSockCommand,
 vCommand,
 vMonitor,
 vMyID,
 vYouID,
 vYouPass,
 vBestQ,
 vUser,
 vPassword        : String;
 vInternalCommand : TInternalCommand;
 ArrayOfPointer   : TArrayOfPointer;
 Procedure TestPing;
 Begin
  If vAegysSession.vPingSense = tpsPing Then
   Begin
    {$IFNDEF FPC}
     {$IF Not Defined(HAS_FMX)}
      vAegysSession.vBeginPing := GetTickCount;
     {$ELSE}
      vAegysSession.vBeginPing := System.Classes.TThread.GetTickCount;
     {$IFEND}
    {$ELSE}
     vAegysSession.vBeginPing := GetTickCount;
    {$ENDIF}
    vAegysSession.vPingSense := tpsPong;
   End
  Else
   Begin
    {$IFNDEF FPC}
     {$IF Not Defined(HAS_FMX)}
      vAegysSession.vLatency := GetTickCount - vAegysSession.vBeginPing;
     {$ELSE}
      vAegysSession.vLatency := System.Classes.TThread.GetTickCount - vAegysSession.vBeginPing;
     {$IFEND}
    {$ELSE}
     vAegysSession.vLatency := GetTickCount - vAegysSession.vBeginPing;
    {$ENDIF}
    vAegysSession.vPingSense := tpsPing;
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
    If Not AContext.Connection.IOHandler.InputBufferIsEmpty Then
     Begin
      aBuffSize     := AContext.Connection.IOHandler.InputBuffer.Size;
      AContext.Connection.IOHandler.ReadBytes(TIdBytes(aBuf), SizeOf(aPackSize));
      Move(aBuf[0], aPackSize, SizeOf(aPackSize));
      aFirstBufSize := Length(aBuf);
      bPackSize     := 0;
      While (aPackSize <> Length(aBuf)) Do
       Begin
        If bPackSize = 0 Then
         bPackSize := aBuffSize - SizeOf(aPackSize)
        Else
         Begin
          Try
           Processmessages;
           AContext.Connection.IOHandler.CheckForDataOnSource(-1);
           bPackSize := AContext.Connection.IOHandler.InputBuffer.Size;
          Except
           bPackSize := 0
          End;
         End;
        If (Length(aBuf) + bPackSize) > aPackSize Then
         bPackSize := aPackSize - Length(aBuf);
        If bPackSize > 0 Then
         AContext.Connection.IOHandler.ReadBytes(TIdBytes(aBuf), bPackSize);
       End;
     End;
    If (aPackSize = Length(aBuf)) And (aPackSize > 0) Then
     Begin
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
             aPackClass.Command   := cStatusDesc              +
                                     vAegysSession.Connection + '&' +
                                     vAegysSession.vSessionID + '&' +
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
          TestPing;
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
                  ticConnectionList  : Begin

                                       End;
                  ticFindID          : Begin
                                        ArrayOfPointer := [@vYouID];
                                        ParseValues(vCommand, ArrayOfPointer);
                                        vYouSession := vSessionList.GetConnection(vYouID);
                                        aPackClass  := TPackClass.Create;
                                        Try
                                         aPackClass.DataMode  := tdmServerCommand;
                                         aPackClass.DataCheck := tdcAsync;
                                         If (vYouSession = Nil) Then //Error defined, no have connection
                                          Begin
                                           aPackClass.Command  := cIDNotFound + Format('%s', [vYouID]);
                                           aBuf                 := aPackClass.ToBytes;
                                           vAegysSession.SendBytes(aBuf);
                                          End
                                         Else
                                          Begin
                                           aPackClass.Command := cIncommingConnect + Format('%s&%s&%s', [vAegysSession.Connection,
                                                                                                         vAegysSession.vSessionID,
                                                                                                         vAegysSession.SessionPWD]);
                                           aBuf               := aPackClass.ToBytes;
                                           vYouSession.SendBytes(aBuf);
                                           aPackClass.Command := cIDExistsReqPass + Format('%s&%s', [vYouSession.Connection, vYouSession.SessionID]);
                                           aBuf               := aPackClass.ToBytes;
                                           vAegysSession.SendBytes(aBuf);
                                          End;
                                        Finally
                                         SetLength(aBuf, 0);
                                         FreeAndNil(aPackClass);
                                        End;
                                       End;
                  ticPong            : Begin
                                        {$IFNDEF FPC}
                                         {$IF Not Defined(HAS_FMX)}
                                          vAegysSession.vLatency := GetTickCount - vAegysSession.vBeginPing;
                                         {$ELSE}
                                          vAegysSession.vLatency := System.Classes.TThread.GetTickCount - vAegysSession.vBeginPing;
                                         {$IFEND}
                                        {$ELSE}
                                         vAegysSession.vLatency := GetTickCount - vAegysSession.vBeginPing;
                                        {$ENDIF}
                                       End;
                  ticCheckPass,
                  ticRelation        : Begin
                                        ArrayOfPointer := [@vMyID, @vYouID, @vYouPass];
                                        ParseValues(vCommand, ArrayOfPointer);
                                        vMySession  := vSessionList.GetConnection(vAegysSession.Socket);
                                        vYouSession := vSessionList.GetConnection(vYouID, vYouPass);
                                        If (vYouSession = Nil) Then //Error defined, no have connection
                                         Begin
                                          aPackClass := TPackClass.Create;
                                          Try
                                           aPackClass.DataMode  := tdmServerCommand;
                                           aPackClass.DataCheck := tdcAsync;
                                           If vInternalCommand = ticCheckPass Then
                                            aPackClass.Command  := cAccessDenied
                                           Else
                                            aPackClass.Command  := cIDNotFound;
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
                                           If vInternalCommand = ticCheckPass Then
                                            Begin
                                             aPackClass.Command := cAccessGranted + Format('%s&%s&%s', [vYouSession.Connection,
                                                                                                        vYouSession.vSessionID,
                                                                                                        vYouSession.SessionPWD]);
                                             aBuf               := aPackClass.ToBytes;
                                             vMySession.SendBytes(aBuf);
                                            End
                                           Else
                                            Begin
                                             aPackClass.Command := cConnectedPeer + Format('%s&%s&%s', [vYouSession.Connection,
                                                                                                        vYouSession.vSessionID,
                                                                                                        vYouSession.SessionPWD]);
                                             aBuf               := aPackClass.ToBytes;
                                             vMySession.SendBytes(aBuf);
                                            End;
                                           aPackClass.Command   := cConnectedPeer + Format('%s&%s&%s', [vMySession.Connection,
                                                                                                        vMySession.vSessionID,
                                                                                                        vMySession.SessionPWD]);
                                           aBuf                 := aPackClass.ToBytes;
                                           vYouSession.SendBytes(aBuf);
                                          Finally
                                           FreeAndNil(aPackClass);
                                          End;
                                         End;
                                       End;
                  ticGetMonitorCount : Begin
                                        ArrayOfPointer := [@vSockCommand, @vYouID, @vYouPass];
                                        ParseValues(vCommand, ArrayOfPointer);
                                        vYouSession := vSessionList.GetConnection(vYouID, vYouPass);
                                        aPackClass := TPackClass.Create;
                                        Try
                                         If (vYouSession <> Nil) Then
                                          Begin
                                           aPackClass.DataMode  := tdmClientCommand;
                                           aPackClass.Command   := cMonitors + Format('%s&%s&%s', [vAegysSession.Connection,
                                                                                                   vAegysSession.vSessionID,
                                                                                                   vAegysSession.SessionPWD]);
                                           aBuf                 := aPackClass.ToBytes;
                                           vYouSession.SendBytes(aBuf);
                                          End
                                         Else
                                          Begin
                                           aPackClass.DataMode  := tdmServerCommand;
                                           aPackClass.DataCheck := tdcAsync;
                                           aPackClass.Command   := cIDNotFound;
                                           aBuf                 := aPackClass.ToBytes;
                                           vAegysSession.SendBytes(aBuf);
                                          End;
                                        Finally
                                         FreeAndNil(aPackClass);
                                        End;
                                       End;
                  ticChangeMonitor   : Begin
                                        ArrayOfPointer := [@vSockCommand, @vYouID, @vYouPass, @vMonitor];
                                        ParseValues(vCommand, ArrayOfPointer);
                                        vYouSession := vSessionList.GetConnection(vYouID, vYouPass);
                                        aPackClass := TPackClass.Create;
                                        Try
                                         If (vYouSession <> Nil) Then
                                          Begin
                                           aPackClass.DataMode  := tdmClientCommand;
                                           aPackClass.Command   := cChangeMonitor + Format('%s&%s&%s&%s', [vAegysSession.Connection,
                                                                                                           vAegysSession.vSessionID,
                                                                                                           vAegysSession.SessionPWD,
                                                                                                           vMonitor]);
                                           aBuf                 := aPackClass.ToBytes;
                                           vYouSession.SendBytes(aBuf);
                                          End
                                         Else
                                          Begin
                                           aPackClass.DataMode  := tdmServerCommand;
                                           aPackClass.DataCheck := tdcAsync;
                                           aPackClass.Command   := cIDNotFound;
                                           aBuf                 := aPackClass.ToBytes;
                                           vAegysSession.SendBytes(aBuf);
                                          End;
                                        Finally
                                         FreeAndNil(aPackClass);
                                        End;
                                       End;
                 End;
                End;
              End;
            End
           Else //Client Data
            Begin
             If aPackClass.ProxyToMyConnectionList Then
              vAegysSession.SendBytes(aPackClass.ToBytes, True)
             Else
              vAegysSession.SendBytes(aPackClass.Dest, aPackClass.ToBytes);
            End;
          Finally
           Processmessages;
           FreeAndNil(aPackClass);
          End;
          If Assigned(vOnClientRequestExecute) Then
           vOnClientRequestExecute(vAegysSession);
         End;
       End
     End
    Else If Length(aBuf) = 0 Then
     TestPing;
   End;
  If vAegysSession.vAegysClientStage = csRejected Then
   AContext.Connection.Disconnect
  Else
   AContext.Connection.IOHandler.CheckForDisconnect;
 Except
  On E : EIdSocketError Do ;
  On E : EIdReadTimeout Do ;
  On E : Exception      Do
   Begin
    Abort;
   End;
 End;
 Sleep(cDelayThread);
End;

Function TAegysService.GetSessionList: TSessionList;
Begin
 Result := TSessionList.Create;
 Try
  Result.Assign(vSessionList);
 Finally

 End;
End;

Procedure TAegysService.Kickuser(aUser : String);
Begin

End;

Procedure ProcessMessages;
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
     vIdTCPServer.Active := False;
     vIdTCPServer.Bindings.Clear;
     vIdTCPServer.Contexts.Clear;
    Finally
     ProcessMessages;
    End;
   End
  Else
   vIdTCPServer.Active  := Value;
 Except
  On E : Exception Do
   Raise Exception.Create(E.Message);
 End;
 vActive                := vIdTCPServer.Active;
End;

constructor TAegysSession.Create;
begin
 Inherited;
 vAContext           := Nil;
 vSessionID          := '';
 vSessionPWD         := '';
 vSessionFixedPWD    := '';
 vAcceptUnAssist     := False;
 vLastRequest        := Now;
 vLogged             := False;
 {$IFNDEF FPC}
  {$IF (DEFINED(OLDINDY))}
   vDataEncoding     := enDefault;
  {$ELSE}
   vDataEncoding     := IndyTextEncoding_UTF8;
  {$IFEND}
 {$ELSE}
  vDataEncoding      := IndyTextEncoding_UTF8;
 {$ENDIF};
 vAegysSessionList   := TAegysSessionList.Create;
 vPingSense          := tpsPing;
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
Function TAegysSessionList.GetConnection(aConnStr   : String)  : TAegysSession;
Var
 I : Integer;
Begin
 Result := Nil;
 For I := Count -1 DownTo 0 Do
  Begin
   If (Items[I].Connection = aConnStr) Or
      (Items[I].SessionID  = aConnStr) Then
    Begin
     Result := Items[I];
     Break;
    End;
  End;
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

Function TAegysSession.ReceiveString : String;
Begin
 If Assigned(vAContext) then
  Result := vAContext.Connection.IOHandler.ReadLn;
End;

Procedure TAegysSession.SendBytes(aBuf          : TAegysBytes;
                                  ToMyConnList  : Boolean);
Var
 I             : Integer;
 aBufError     : TAegysBytes;
 aPackClass    : TPackClass;
 aAegysSession : TAegysSession;
Begin
 If ToMyConnList Then
  Begin
   For I := 0 to SessionList.Count -1 Do
    Begin
     Try
      If Assigned(SessionList[I]) Then
       SessionList[I].SendBytes(aBuf);
     Except
      On E : Exception Do
       Begin
        aPackClass := TPackClass.Create;
        Try
         aPackClass.DataMode  := tdmClientCommand;
         aPackClass.DataCheck := tdcAsync;
         aPackClass.Command   := cSendDataError + Format('%s - %s', [SessionList[I].Connection, E.Message]);
         aBufError            := aPackClass.ToBytes;
        Finally
         FreeAndNil(aPackClass);
        End;
        Socket.Connection.IOHandler.WriteDirect(TIdBytes(aBufError), -1);
///        Socket.Connection.IOHandler.WriteBufferFlush;
       End;
     End;
     ProcessMessages;
    End;
  End
 Else
  Begin
   If Assigned(vAContext) then
    Begin
     aAegysSession := SessionList.GetConnection(aPackClass.Dest);
     If Assigned(aAegysSession) Then
      aAegysSession.SendBytes(aBuf)
     Else
      Begin
       aPackClass := TPackClass.Create;
       Try
        aPackClass.DataMode  := tdmClientCommand;
        aPackClass.DataCheck := tdcAsync;
        aPackClass.Command   := cSendDataErrorID + Format('%s - %s', [aPackClass.Dest, 'Not Found']);
        aBufError            := aPackClass.ToBytes;
       Finally
        FreeAndNil(aPackClass);
       End;
       Socket.Connection.IOHandler.WriteDirect(TIdBytes(aBufError), -1);
//       Socket.Connection.IOHandler.WriteBufferFlush;
      End;
     ProcessMessages;
    End;
  End;
End;

Procedure TAegysSession.SendBytes (aDest         : String;
                                   aBuf          : TAegysBytes);
Var
 aBufError     : TAegysBytes;
 aPackClass    : TPackClass;
 aAegysSession : TAegysSession;
Begin
 If Assigned(vAContext) then
  Begin
   aAegysSession := SessionList.GetConnection(aDest);
   If Assigned(aAegysSession) Then
    aAegysSession.SendBytes(aBuf)
   Else
    Begin
     aPackClass := TPackClass.Create;
     Try
      aPackClass.DataMode  := tdmClientCommand;
      aPackClass.DataCheck := tdcAsync;
      aPackClass.Command   := cSendDataErrorID + Format('%s - %s', [aPackClass.Dest, 'Not Found']);
      aBufError            := aPackClass.ToBytes;
     Finally
      FreeAndNil(aPackClass);
     End;
     Socket.Connection.IOHandler.WriteDirect(TIdBytes(aBufError), -1);
//     Socket.Connection.IOHandler.WriteBufferFlush;
    End;
   ProcessMessages;
  End;
End;

Procedure TAegysSession.SendBytes (aBuf          : TAegysBytes);
Begin
 If Assigned(vAContext) then
  Begin
   Try
    vAContext.Connection.CheckForGracefulDisconnect(True);
    If vAContext.Connection.Connected Then
     vAContext.Connection.IOHandler.WriteDirect(TIdBytes(aBuf));
   Except

   End;
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
    vAContext.Connection.IOHandler.WriteDirect(ToBytes(vSizeFile), SizeOf(AeInt64));
    vAContext.Connection.IOHandler.WriteDirect(aBuf);
   Finally
    SetLength(aBuf, 0);
    FreeAndNil(bStream);
    ProcessMessages;
   End;
  End;
End;

procedure TAegysSession.Ping;
Var
 aPackClass : TPackClass;
 aBuf       : TAegysBytes;
Begin
 If Self = Nil Then
  Exit;
 aPackClass := TPackClass.Create;
 Try
  aPackClass.DataMode  := tdmServerCommand;
  aPackClass.DataCheck := tdcAsync;
  aPackClass.Command   := cPing;
  aBuf                 := aPackClass.ToBytes;
  SendBytes(aBuf);
 Finally
  SetLength(aBuf, 0);
  FreeAndNil(aPackClass);
 End;
End;
procedure TAegysSession.SendString(S : String);
Var
 aBuf : TIdBytes;
Begin
 If Assigned(vAContext) then
  Begin
   aBuf := ToBytes(S, vDataEncoding);
   SendBytes (TAegysBytes(aBuf));
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
 vSessionTime := 0;
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

Procedure TAegysClient.DisconnectAllPeers;
Begin

End;

Procedure TAegysClient.DisconnectPeer(aID,
                                      aPass,
                                      aConnection : String);
Begin

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

Procedure TAegysClient.OnClientCommands(CommandType     : TCommandType;
                                        Connection,
                                        ID,
                                        Command         : String;
                                        aBuf            : TAegysBytes);
Begin
 Case CommandType Of
  tctScreenCapture : Begin
                      If Assigned(vOnScreenCapture) Then
                       vOnScreenCapture  (Connection, ID, Command, aBuf);
                     End;
  tctAudio         : Begin
                      If Assigned(vOnAudioCapture) Then
                       vOnAudioCapture   (Connection, ID, Command, aBuf);
                     End;
  tctVideo         : Begin
                      If Assigned(vOnVideoCapture) Then
                       vOnVideoCapture   (Connection, ID, Command, aBuf);
                     End;
  tctFileTransfer  : Begin
                      If Assigned(vOnFileTransfer) Then
                       vOnFileTransfer   (Connection, ID, Command, aBuf);
                     End;
  tctKeyboard      : Begin
                      If Assigned(vOnKeyboardCapture) Then
                       vOnKeyboardCapture(Connection, ID, Command);
                     End;
  tctMouse         : Begin
                      If Assigned(vOnMouseCapture) Then
                       vOnMouseCapture   (Connection, ID, Command);
                     End;
  tctChat          : Begin
                      If Assigned(vOnChatReceive) Then
                       vOnChatReceive    (Connection, ID, Command);
                     End;
 End;
End;

Procedure TAegysClient.OnServiceCommands(InternalCommand : TInternalCommand;
                                         Command         : String);
Var
 ArrayOfPointer   : TArrayOfPointer;
 vConnection,
 vClientID,
 vClientPassword,
 vAlias           : String;
Begin
 Case InternalCommand Of
  ticLogin,
  ticDataStatus       : Begin
                         If Assigned(vOnServerLogin) Then
                          vOnServerLogin(Self);
                        End;
  ticConnectedPeer    : Begin
                         ArrayOfPointer :=  [@vConnection, @vClientID, @vClientPassword, @vAlias];
                         ParseValues(Command, ArrayOfPointer);
                         If Assigned(vOnPeerConnected) Then
                          vOnPeerConnected   (vConnection,  vClientID,  vClientPassword,  vAlias);
                        End;
  ticAccessGranted    : Begin
                         ArrayOfPointer :=  [@vConnection, @vClientID, @vClientPassword, @vAlias];
                         ParseValues(Command, ArrayOfPointer);
                         If Assigned(vOnAccessGranted) Then
                          vOnAccessGranted   (vConnection, vClientID,  vClientPassword,  vAlias);
                        End;
  ticDisconnectedPeer : Begin
                         ArrayOfPointer :=  [@vConnection, @vClientID, @vClientPassword, @vAlias];
                         ParseValues(Command, ArrayOfPointer);
                         If Assigned(vOnPeerDisconnected) Then
                          vOnPeerDisconnected(vConnection,  vClientID,  vClientPassword,  vAlias);
                        End;
  ticIncommingConnect : Begin
                         ArrayOfPointer :=  [@vConnection, @vClientID, @vClientPassword, @vAlias];
                         ParseValues(Command, ArrayOfPointer);
                         If Assigned(vOnÌncommingConnect) Then
                          vOnÌncommingConnect (vConnection,  vClientID,  vClientPassword,  vAlias);
                        End;
  ticIDExistsReqPass  : Begin
                         ArrayOfPointer :=  [@vConnection, @vClientID, @vAlias];
                         ParseValues(Command, ArrayOfPointer);
                         If Assigned(vOnBeginTransaction) Then
                          vOnBeginTransaction(vConnection,  vClientID,  vAlias);
                        End;
  ticIDNotFound       : Begin
                         ArrayOfPointer :=  [@vConnection];
                         ParseValues(Command, ArrayOfPointer);
                         If Assigned(vOnBeginTransactionError) Then
                          vOnBeginTransactionError(vConnection);
                        End;
  Else
   Begin
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
  aBuf          : TAegysBytes;
  aFirstBufSize : AEInteger;
  aBuffSize,
  aPackSize,
  bPackSize     : AEInt64;
 Begin
  SetLength(aBuf, 0);
  If vTcpRequest.Connected Then
   Begin
    Try
     If Not (vTcpRequest.IOHandler.InputBufferIsEmpty) Then
      Begin
       Try
        aBuffSize     := vTcpRequest.IOHandler.InputBuffer.Size;
        vTcpRequest.IOHandler.ReadBytes(TIdBytes(aBuf), SizeOf(aPackSize));
        Move(aBuf[0], aPackSize, SizeOf(aPackSize));
        aFirstBufSize := Length(aBuf);
        bPackSize     := 0;
        While (aPackSize <> Length(aBuf))                   Do
         Begin
          If bPackSize = 0 Then
           bPackSize := aBuffSize - SizeOf(aPackSize)
          Else
           Begin
            Processmessages;
            vTcpRequest.IOHandler.CheckForDataOnSource;
            bPackSize := vTcpRequest.IOHandler.InputBuffer.Size;
           End;
          If (Length(aBuf) + bPackSize) > aPackSize Then
           bPackSize := aPackSize - Length(aBuf);
          vTcpRequest.IOHandler.ReadBytes(TIdBytes(aBuf), bPackSize);
         End;
        If (aPackSize = Length(aBuf)) And (aPackSize > 0) Then
         aPackList.Add(aBuf);
       Finally
        SetLength(aBuf, 0);
       End;
      End;
    Except

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
                                            aBuffer       : TAegysBytes;
                                            CommandType   : TCommandType = tctScreenCapture);
Begin
 If vTcpRequest.Connected Then
  aPackList.Add(vConnection, aID, tdmClientCommand, tdcAsync, CommandType, aBuffer, Format('%s&%s', [vConnection, vSessionID]));
End;

Procedure TAegysClient.SendBytes           (aBuffer       : TAegysBytes;
                                            aDestMyConn   : Boolean;
                                            CommandType   : TCommandType = tctScreenCapture);
Begin
 If vTcpRequest.Connected Then
  aPackList.Add(vConnection, '', tdmClientCommand, tdcAsync, CommandType, aBuffer, Format('%s&%s', [vConnection, vSessionID]), aDestMyConn);
End;

Procedure TAegysClient.SendBytes           (aBuffer       : TAegysBytes);
Begin
 If vTcpRequest.Connected Then
  vTcpRequest.IOHandler.WriteDirect(TIdBytes(aBuffer));
End;

Procedure TAegysClient.SendCommand         (Value         : String);
Begin
 If vTcpRequest.Connected Then
  aPackList.Add(vConnection, '', tdmServerCommand, tdcAsync, Value)
 Else
  Raise Exception.Create(cCantJoinDisconnected);
End;

procedure TAegysClient.SendKeyboard        (aID, Value    : String);
begin

end;

Procedure TAegysClient.SendKeyboard        (aDestMyConn   : Boolean;
                                            Value         : String);
Begin

End;

Procedure TAegysClient.SendMessage         (Value         : String;
                                            aDestMyConn   : Boolean;
                                            CommandType   : TCommandType = tctChat);
Begin
 If vTcpRequest.Connected Then
  aPackList.Add(vConnection, '', tdmClientCommand, tdcAsync, CommandType, Format('%s&%s&%s', [vConnection, vSessionID, Value]), aDestMyConn);
End;

Procedure TAegysClient.SendMessage         (aID,
                                            Value         : String;
                                            CommandType   : TCommandType = tctChat);
Begin
 If vTcpRequest.Connected Then
  aPackList.Add(vConnection, aID, tdmClientCommand, tdcAsync, CommandType, Format('%s&%s&%s', [vConnection, vSessionID, Value]), False);
End;

Procedure TAegysClient.SendMouse  (aID,
                                   Value       : String);
Begin

End;

Procedure TAegysClient.SendMonitor(aID,
                                   Value       : String);
Begin

End;

Procedure TAegysClient.SendMonitor(aDestMyConn : Boolean;
                                   Value       : String);
Begin

End;

Procedure TAegysClient.SendMouse  (aDestMyConn : Boolean;
                                   Value       : String);
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
 vSessionTime := 0;
 KillThread;
 If (Value) Then
  Begin
   If Not vTcpRequest.Connected Then
    Begin
     vTcpRequest.Host           := vHost;
     vTcpRequest.Port           := vPort;
     vTcpRequest.ReadTimeout    := vRequestTimeOut;
     vTcpRequest.ConnectTimeout := vConnectTimeOut;
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
                                                    TComponent(Pointer(@Self)^),
                                                    cDelayThread,
                                                    OnBeforeExecuteData,
                                                    OnReceiveBytes,
                                                    OnExecuteData,
                                                    OnAbortData,
                                                    OnServiceCommands,
                                                    OnClientCommands,
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
