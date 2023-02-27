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
 IdContext,         IdTCPConnection,  IdTCPClient,     IdComponent,     IdBaseComponent,
 IdCustomTCPServer, IdTCPServer,      IdStack,         IdExceptionCore, IdGlobal,
 //Aegys Basic Units
 uAegysBufferPack, uAegysConsts,    uAegysDataTypes, uAegysThreads;

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
   Constructor Create;
   Destructor Destroy; Override;
   Function   GetConnection (aConnStr      : String;
                             Id, PWD        : String)        : TAegysSession;
   Procedure  Delete        (Index         : Integer);                       Overload;
   Procedure  Delete        (aConnStr,
                             Id, PWD       : String);                        Overload;
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
  vBContext,
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
  Property    SocketIn                 : TIdContext          Read vAContext               Write vAContext;
  Property    SocketOut                : TIdContext          Read vBContext               Write vBContext;
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
 TAegysOnClientServiceCommand = Procedure(Command         : String)         Of Object;
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
                                        SpecialData       : String)         Of Object;
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
  Procedure  SetActive         (Value      : Boolean);
  Procedure  Disconnect        (AContext   : TIdContext);
  Procedure  Connect           (AContext   : TIdContext);
  Function   GetSessionList                : TSessionList;
  Function   NewSession        (ID         : String)        : TAegysSession;
  Procedure  SetSocketDirection(NewID       : String;
                                Var aSocket : TIdContext;
                                aDirection  : TPortDirection = tpdInBound);
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
  vTcpReceive,
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
  vWelcomeString,
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
  vOnPeerKick,
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
  vOnMouseCapture          : TAegysOnClientServiceCommand;
  vOnChatReceive           : TAegysOnClientCommand;
  vOnBeginTransaction      : TAegysOnBeginTransaction;
  vOnBeginTransactionError : TAegysOnBeginTransactionError;
  Procedure WriteDirect         (aClient         : TIdTCPClient;
                                 aBuf            : TAegysBytes);
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
  Procedure   NewID         (aValue      : String);
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
  Procedure   SendCommand   (Value       : String);                         Overload;
  Procedure   SendCommand   (aDest       : String;
                             aBuffer     : TAegysBytes);                    Overload;
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
                             aMoreData   : String       = '';
                             CommandType : TCommandType = tctScreenCapture);Overload;
  Procedure   SendBytes     (aBuffer     : TAegysBytes;
                             aDestMyConn : Boolean;
                             aMoreData   : String       = '';
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
  Property    WelcomeString              : String                        Read vWelcomeString;
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
  Property    OnPeerKick                 : TAegysOnPeerDisconnected      Read vOnPeerKick              Write vOnPeerKick;
  Property    OnServerLogin              : TAegysOnServerLogin           Read vOnServerLogin           Write vOnServerLogin;
  Property    OnScreenCapture            : TAegysOnClientBuf             Read vOnScreenCapture         Write vOnScreenCapture;
  Property    OnAudioCapture             : TAegysOnClientBuf             Read vOnAudioCapture          Write vOnAudioCapture;
  Property    OnVideoCapture             : TAegysOnClientBuf             Read vOnVideoCapture          Write vOnVideoCapture;
  Property    OnFileTransfer             : TAegysOnClientBuf             Read vOnFileTransfer          Write vOnFileTransfer;
  Property    OnKeyboardCapture          : TAegysOnClientServiceCommand  Read vOnKeyboardCapture       Write vOnKeyboardCapture;
  Property    OnMouseCapture             : TAegysOnClientServiceCommand  Read vOnMouseCapture          Write vOnMouseCapture;
  Property    OnChatReceive              : TAegysOnClientCommand         Read vOnChatReceive           Write vOnChatReceive;
  Property    OnBeginTransaction         : TAegysOnBeginTransaction      Read vOnBeginTransaction      Write vOnBeginTransaction;
  Property    OnBeginTransactionError    : TAegysOnBeginTransactionError Read vOnBeginTransactionError Write vOnBeginTransactionError;
  Property    OnAccessGranted            : TAegysOnPeerConnected         Read vOnAccessGranted         Write vOnAccessGranted;
End;

  Procedure   ProcessMessages;

Implementation

Uses uAegysTools;

Function TAegysService.NewSession(ID : String) : TAegysSession;
Begin
 Result                   := TAegysSession.Create;
 Result.Connection        := ID + ':' + Formatdatetime('ddmmyyyyhhmmss', Now);
 Result.vInitalRequest    := Now;
 Result.vLastRequest      := Result.vInitalRequest;
 Result.vLogged           := True;
 Result.vAegysClientStage := csWaitLogon;
 vSessionList.Add(Result);
End;

Procedure TAegysService.SetSocketDirection(NewID       : String;
                                           Var aSocket : TIdContext;
                                           aDirection  : TPortDirection = tpdInBound);
Var
 I : Integer;
Begin
 For I := vSessionList.Count -1  Downto 0 Do
  Begin
   If vSessionList[I].Connection = NewID Then
    Begin
     If aDirection = tpdInBound Then
      vSessionList[I].SocketIn   := aSocket
     Else
      vSessionList[I].SocketOut  := aSocket;
     {$IFNDEF FPC}
      {$IF Not Defined(HAS_FMX)}
       aSocket.Data              := vSessionList[I];
      {$ELSE}
       {$IFDEF HAS_UTF8}
        {$IF CompilerVersion > 33}aSocket.Data{$ELSE}aSocket.DataObject{$IFEND} := vSessionList[I];
       {$ELSE}
        aSocket.DataObject       := vSessionList[I];
       {$ENDIF}
      {$IFEND}
     {$ELSE}
      aSocket.Data               := vSessionList[I];
     {$ENDIF}
     Break;
    End;
  End;
End;

Procedure TAegysService.Connect(AContext : TIdContext);
//Var
// AegysSession : TAegysSession;
Begin
// AegysSession                 := TAegysSession.Create;
// AegysSession.Connection      := AContext.Connection.Socket.Binding.PeerIP + ':' + IntToStr(AContext.Connection.Socket.Binding.PeerPort);
// AegysSession.Socket          := AContext;
// {$IFNDEF FPC}
//  {$IF Not Defined(HAS_FMX)}
//   AContext.Data              := AegysSession;
//  {$ELSE}
//   {$IFDEF HAS_UTF8}
//    {$IF CompilerVersion > 33}AContext.Data{$ELSE}AContext.DataObject{$IFEND} := AegysSession;
//   {$ELSE}
//    AContext.DataObject        := AegysSession;
//   {$ENDIF}
//  {$IFEND}
// {$ELSE}
//  AContext.Data                := AegysSession;
// {$ENDIF}
// AegysSession.vInitalRequest  := Now;
// AegysSession.vLastRequest    := AegysSession.vInitalRequest;
//// AegysSession.OnSessionError  := vRESTDwSessionError;
// AegysSession.vLogged         := True;
// If AegysSession.vLogged Then
//  AegysSession.vAegysClientStage := csWaitLogon
// Else
//  AegysSession.vAegysClientStage := csNone;
// vSessionList.Add(AegysSession);
// If Assigned(vOnConnect) Then
//  vOnConnect(AegysSession);
End;

Constructor TAegysService.Create(AOwner: TComponent);
Begin
 Inherited;
 vOnGetClientDetails        := Nil;
 vGarbageTime               := 50000;
 vServiceTimeout            := cServiceTimeout;
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
 Procedure BroadCastDisconnect(AegysSession : TAegysSession);
 Var
  I          : Integer;
  aPackClass : TPackClass;
  aBuf       : TAegysBytes;
 Begin
  If Assigned(AegysSession.SessionList) Then
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
    For I := AegysSession.SessionList.Count -1 DownTo 0 Do
     Begin
      Try
       AegysSession.SessionList[I].SendBytes(aBuf);
       Processmessages;
      Except
      End;
     End;
   End;
 End;
Begin
 Try
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
      If Assigned(vSessionList) Then
       vSessionList.Delete(TAegysSession(AContext.Data));
     {$ELSE}
      BroadCastDisconnect(TAegysSession({$IF CompilerVersion > 33}AContext.Data{$ELSE}AContext.DataObject{$IFEND}));
      //TODO Delete my connection from others lists
      If Assigned(vSessionList) Then
       vSessionList.Delete(TAegysSession({$IF CompilerVersion > 33}AContext.Data{$ELSE}AContext.DataObject{$IFEND}));
     {$IFEND}
    {$ELSE}
      BroadCastDisconnect(TAegysSession(AContext.Data));
      //TODO Delete my connection from others lists
      If Assigned(vSessionList) Then
       vSessionList.Delete(TAegysSession(AContext.Data));
    {$ENDIF}
    //Release my connection
    {$IFNDEF FPC}
     {$IF Not Defined(HAS_FMX)}
      If Assigned(vOnDisconnect) Then
       vOnDisconnect(TAegysSession(AContext.Data));
 //TODO Corrigir Desconexao, XyberX
      If Assigned(AContext.Data) Then
       Begin
        If Assigned(PAegysSession(@AContext.Data)) Then
         Begin
          AContext.Data := Nil;
          If Assigned(PAegysSession(@AContext.Data)^) Then
           FreeAndNil(PAegysSession(@AContext.Data)^);
         End;
       End;
     {$ELSE}
      {$IFDEF HAS_UTF8}
       If Assigned(vOnDisconnect) Then
        vOnDisconnect(TAegysSession({$IF CompilerVersion > 33}AContext.Data{$ELSE}AContext.DataObject{$IFEND}));
       If Assigned(PAegysSession(@{$IF CompilerVersion > 33}AContext.Data{$ELSE}AContext.DataObject{$IFEND})^) Then
        Begin
         If Assigned({$IF CompilerVersion > 33}AContext.Data{$ELSE}AContext.DataObject{$IFEND}) Then
          Begin
           TAegysSession({$IF CompilerVersion > 33}AContext.Data{$ELSE}AContext.DataObject{$IFEND}).DisposeOf;
           {$IF CompilerVersion > 33}AContext.Data{$ELSE}AContext.DataObject{$IFEND} := Nil;
          End;
        End;
      {$ELSE}
       If Assigned(vOnDisconnect) Then
        vOnDisconnect(TAegysSessionData(TAegysSession(AContext.DataObject)));
       If Assigned(PAegysSession(@AContext.Data)^) Then
        Begin
         If Assigned(AContext.Data) Then
          Begin
           TAegysSession(AContext.DataObject).DisposeOf;
           AContext.DataObject := Nil;
          End;
        End;
      {$ENDIF}
     {$IFEND}
    {$ELSE}
     If Assigned(vOnDisconnect) Then
      vOnDisconnect(TAegysSessionData(TAegysSession(AContext.Data)));
     If Assigned(AContext.Data) Then
      Begin
       AContext.Data := Nil;
       If Assigned(PAegysSession(@AContext.Data)^) Then
        FreeAndNil(PAegysSession(@AContext.Data)^);
      End;
    {$ENDIF}
   End;
 Except
  Processmessages;
 End;
End;

Function TSessionList.GetConnection(aSocket : TIdContext) : TAegysSession;
Var
 I : Integer;
Begin
 Result := Nil;
 For I := Count -1 DownTo 0 Do
  Begin
   If (Items[I].SocketIn  = aSocket) Or
      (Items[I].SocketOut = aSocket) Then
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
 I                : Integer;
 vMySession,
 vYouSession,
 vAegysSession    : TAegysSession;
 vDataCommand,
 vAction,
 vAccept          : Boolean;
 aBuf             : TAegysBytes;
 aPackClass       : TPackClass;
 aStream          : TStream;
 aFirstBufSize    : AEInteger;
 aBuffSize,
 bPackSize,
 aPackSize        : AEInt64;
 aMessageError,
 vPorDirection,
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
 Procedure TestPing(Var AegysSession : TAegysSession);
 Begin
  If Assigned(AegysSession) Then
   Begin
    If AegysSession.SocketIn = aContext Then
     Begin
      If AegysSession.vPingSense = tpsPing Then
       Begin
        {$IFNDEF FPC}
         {$IF Not Defined(HAS_FMX)}
          AegysSession.vBeginPing := GetTickCount;
         {$ELSE}
          AegysSession.vBeginPing := System.Classes.TThread.GetTickCount;
         {$IFEND}
        {$ELSE}
         AegysSession.vBeginPing := GetTickCount;
        {$ENDIF}
        AegysSession.vPingSense := tpsPong;
       End
      Else
       Begin
        {$IFNDEF FPC}
         {$IF Not Defined(HAS_FMX)}
          AegysSession.vLatency := GetTickCount - AegysSession.vBeginPing;
         {$ELSE}
          AegysSession.vLatency := System.Classes.TThread.GetTickCount - AegysSession.vBeginPing;
         {$IFEND}
        {$ELSE}
         AegysSession.vLatency := GetTickCount - AegysSession.vBeginPing;
        {$ENDIF}
        AegysSession.vPingSense := tpsPing;
       End;
     End;
   End;
 End;
 Function FindSession(Socket : TIdContext) : TAegysSession;
 Var
  I : Integer;
 Begin
  Result := Nil;
  For I := vSessionList.Count -1 DownTo 0 Do
   Begin
    If (vSessionList[I].vAContext = Socket) or
       (vSessionList[I].vBContext = Socket) Then
     Begin
      Result := vSessionList[I];
      Break;
     End;
   End;
 End;
Begin
 aPackClass := Nil;
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
 vAction        := Assigned(vAegysSession);
 Try
  AContext.Connection.IOHandler.ReadTimeout := RequestTimeout;
  AContext.Connection.IOHandler.CheckForDisconnect;
  vAccept := True;
  If Not AContext.Connection.IOHandler.InputBufferIsEmpty Then
   Begin
    Processmessages;
    aBuffSize     := AContext.Connection.IOHandler.InputBuffer.Size;
    AContext.Connection.IOHandler.ReadBytes(TIdBytes(aBuf), SizeOf(aPackSize));
    Move(aBuf[0], aPackSize, SizeOf(aPackSize));
    aFirstBufSize := Length(aBuf);
    bPackSize     := 0;
    While (aPackSize > Length(aBuf)) Do
     Begin
      If bPackSize = 0 Then
       bPackSize := aBuffSize - SizeOf(aPackSize)
      Else
       Begin
        Try
         Processmessages;
         AContext.Connection.IOHandler.CheckForDataOnSource(cLimitSource);
         bPackSize := AContext.Connection.IOHandler.InputBuffer.Size;
        Except
         Break;//bPackSize := 0
        End;
       End;
      If (Length(aBuf) + bPackSize) > aPackSize Then
       bPackSize := aPackSize - Length(aBuf);
      If bPackSize > 0 Then
       AContext.Connection.IOHandler.ReadBytes(TIdBytes(aBuf), bPackSize);
      Processmessages;
     End;
   End;
  If (aPackSize > 0)             And
     (aPackSize <> Length(aBuf)) Then
   Raise Exception.Create(cInvalidBufferData);
  Processmessages;
  vDataCommand := False;
  If (aPackSize = Length(aBuf)) And
     (aPackSize > 0)            Then
   Begin
    aPackClass := TPackClass.Create;
    Try
     aPackClass.FromBytes(aBuf);
     vCommand := aPackClass.Command;
     If vCommand <> '' Then
      ParseCommand(vCommand, vInternalCommand);
     If vInternalCommand <> ticNone Then
      Begin
       Case vInternalCommand Of
        ticNewID       : Begin
                          If Not vAction Then
                           Begin
                            If Assigned(vOnGetClientDetails) Then
                             vOnGetClientDetails(vSessionList, vCommand, vUser, vPassword, vAccept);
                            If vUser <> '' Then
                             Begin
                              If vAccept Then
                               Begin
                                vAegysSession                   := NewSession(vUser);
                                vAegysSession.vSessionID        := vUser;
                                vAegysSession.vSessionPWD       := vPassword;
                                vAegysSession.vAegysClientStage := csLoggedIn;
                                If Assigned(aPackClass) Then
                                 FreeAndNil(aPackClass);
                                aPackClass                      := TPackClass.Create;
                                Try
                                 aPackClass.DataMode  := tdmServerCommand;
                                 aPackClass.DataCheck := tdcAsync;
                                 aPackClass.Command   := cStatusDesc              +
                                                         vAegysSession.Connection + '&' +
                                                         vAegysSession.vSessionID + '&' +
                                                         vAegysSession.vSessionPWD;
                                 aBuf                 := aPackClass.ToBytes;
                                 If Assigned(vOnConnect) Then
                                  vOnConnect(vAegysSession);
                                 AContext.Connection.IOHandler.WriteDirect(TidBytes(aBuf));
                                 //vAegysSession.SendBytes(aBuf);
                                Finally
                                 FreeAndNil(aPackClass);
                                End;
                               End;
                             End;
                           End;
                         End;
        ticSetPortSend : Begin
                          If (vCommand <> '') Then
                           Begin
                            ArrayOfPointer := [@vSockCommand];
                            ParseValues(vCommand, ArrayOfPointer);
                            SetSocketDirection(vSockCommand, AContext, tpdInBound);
                           End;
                         End;
        ticSetPortRec  : Begin
                          If (vCommand <> '') Then
                           Begin
                            ArrayOfPointer := [@vSockCommand];
                            ParseValues(vCommand, ArrayOfPointer);
                            SetSocketDirection(vSockCommand, AContext, tpdOutBound);
                           End;
                         End;
        Else
         vDataCommand := True;
       End;
       vAegysSession := FindSession(AContext);
       If Assigned(vAegysSession) Then
        Begin
         If (vAegysSession.Connection        <> '')  Then
          Begin
           If Assigned(vAegysSession.SocketIn)  And
              Assigned(vAegysSession.SocketOut) Then
            vAegysSession.vAegysClientStage := csLoggedIn;
          End;
        End;
      End;
    Finally
     vDataCommand := Not(vInternalCommand In [ticNewID, ticSetPortSend, ticSetPortRec]);
    End;
    If (vDataCommand)       And
       (vAction)            And
       Assigned(aPackClass) Then
     Begin
      If vAegysSession.vAegysClientStage = csLoggedIn Then
       Begin
        If Length(aBuf) > 0 Then
         Begin
          TestPing(vAegysSession);
          Try
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
                  ticDisconnectAllPeers : Begin
                                           If Assigned(aPackClass) Then
                                            FreeAndNil(aPackClass);
                                           aPackClass  := TPackClass.Create;
                                           Try
                                            aPackClass.DataMode    := tdmServerCommand;
                                            aPackClass.DataCheck   := tdcAsync;
                                            For I := vAegysSession.SessionList.Count -1 Downto 0 Do
                                             Begin
                                              Try
                                               aPackClass.Command  := cKickPeer + Format('%s&%s&%s', [vAegysSession.Connection,
                                                                                                      vAegysSession.vSessionID,
                                                                                                      vAegysSession.SessionPWD]);
                                               vAegysSession.SessionList[I].SendBytes(aPackClass.ToBytes);
                                              Finally
                                               aPackClass.Command  := cKickPeer + Format('%s&%s&%s', [vAegysSession.SessionList[I].Connection,
                                                                                                      vAegysSession.SessionList[I].vSessionID,
                                                                                                      vAegysSession.SessionList[I].SessionPWD]);
                                               vAegysSession.SendBytes(aPackClass.ToBytes);
                                               vAegysSession.SessionList[I].SessionList.Delete(vAegysSession.Connection,
                                                                                               vAegysSession.vSessionID,
                                                                                               vAegysSession.SessionPWD);
                                               vAegysSession.SessionList.Delete(I);
                                              End;
                                             End;
                                           Finally
                                            FreeAndNil(aPackClass);
                                           End;
                                          End;
                  ticDisconnectPeer     : Begin
                                           ArrayOfPointer := [@vYouID, @vYouPass, @vSockCommand];
                                           ParseValues(vCommand, ArrayOfPointer);
                                           vYouSession := vAegysSession.SessionList.GetConnection(vYouID, vYouPass, vSockCommand);
                                           If Assigned(aPackClass) Then
                                            FreeAndNil(aPackClass);
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
                                             aPackClass  := TPackClass.Create;
                                             Try
                                              aPackClass.DataMode    := tdmServerCommand;
                                              aPackClass.DataCheck   := tdcAsync;
                                              Try
                                               aPackClass.Command  := cKickPeer + Format('%s&%s&%s', [vAegysSession.Connection,
                                                                                                      vAegysSession.vSessionID,
                                                                                                      vAegysSession.SessionPWD]);
                                               vYouSession.SendBytes(aPackClass.ToBytes);
                                              Finally
                                               aPackClass.Command  := cKickPeer + Format('%s&%s&%s', [vAegysSession.SessionList[I].Connection,
                                                                                                                    vAegysSession.SessionList[I].vSessionID,
                                                                                                                    vAegysSession.SessionList[I].SessionPWD]);
                                               vAegysSession.SendBytes(aPackClass.ToBytes);
                                               vYouSession.SessionList.Delete(vAegysSession.Connection,
                                                                              vAegysSession.vSessionID,
                                                                              vAegysSession.SessionPWD);
                                               vAegysSession.SessionList.Delete(vSockCommand, vYouID, vYouPass);
                                              End;
                                             Finally
                                              FreeAndNil(aPackClass);
                                             End;
                                            End;
                                          End;
                  ticCheckPass,
                  ticRelation        : Begin
                                        ArrayOfPointer := [@vMyID, @vYouID, @vYouPass];
                                        ParseValues(vCommand, ArrayOfPointer);
                                        vMySession  := vSessionList.GetConnection(vAegysSession.SocketIn);
                                        vYouSession := vSessionList.GetConnection(vYouID, vYouPass);
                                        If Assigned(aPackClass) Then
                                         FreeAndNil(aPackClass);
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
                                        If Assigned(aPackClass) Then
                                         FreeAndNil(aPackClass);
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
                                        If Assigned(aPackClass) Then
                                         FreeAndNil(aPackClass);
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
           If Assigned(aPackClass) Then
            FreeAndNil(aPackClass);
          End;
          If Assigned(vOnClientRequestExecute) Then
           vOnClientRequestExecute(vAegysSession);
         End;
       End;
     End
    Else If Length(aBuf) = 0 Then
     TestPing(vAegysSession);
   End;
  If Assigned(vAegysSession) Then
   Begin
    If vAegysSession.vAegysClientStage = csRejected Then
     AContext.Connection.Disconnect
    Else
     AContext.Connection.IOHandler.CheckForDisconnect;
    If Length(aBuf) = 0 Then
     TestPing(vAegysSession);
   End
  Else
   AContext.Connection.IOHandler.CheckForDisconnect;
  If Assigned(aPackClass) Then
   FreeAndNil(aPackClass);
 Except
  On E : EIdSocketError Do Abort;
  On E : EIdReadTimeout Do ;
  On E : Exception      Do
   Begin
    If Assigned(aPackClass) Then
     FreeAndNil(aPackClass);
    aMessageError := E.Message;
    Try
     If Not AContext.Connection.IOHandler.Connected Then
      Abort;
    Except
     Abort;
    End;
   End;
 End;
 Processmessages;
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
 vBContext           := Nil;
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
  If Assigned(vBContext) Then
   Begin
    If Not Gracefully Then
     vBContext.Connection.IOHandler.Close
    Else
     Begin
      vBContext.Connection.IOHandler.CloseGracefully;
      vBContext.Connection.IOHandler.CheckForDisconnect(False, False);
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

Constructor TAegysSessionList.Create;
Begin
 Inherited;
End;

Function TAegysSessionList.GetConnection(aConnStr   : String;
                                         Id, PWD        : String)  : TAegysSession;
Var
 I : Integer;
Begin
 Result := Nil;
 For I := Count -1 DownTo 0 Do
  Begin
   If (Items[I].Connection = aConnStr) Or
      ((Items[I].SessionID  = Id)      And
       (Items[I].SessionPWD = PWD))    Then
    Begin
     Result := Items[I];
     Break;
    End;
  End;
End;

Procedure TAegysSessionList.Delete(aConnStr, Id, PWD: String);
Var
 I : Integer;
Begin
 For I := Count -1 Downto 0 Do
  Begin
   If (Items[I].Connection = aConnStr) Or
      ((Items[I].SessionID  = Id)      And
       (Items[I].SessionPWD = PWD))    Then
    Begin
     Delete(I);
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
        vBContext.Connection.IOHandler.WriteDirect(TIdBytes(aBufError), -1);
///        Socket.Connection.IOHandler.WriteBufferFlush;
       End;
     End;
     ProcessMessages;
    End;
  End
 Else
  Begin
   If Assigned(vBContext) then
    Begin
     aAegysSession := SessionList.GetConnection(aPackClass.Dest, aPackClass.Dest, '');
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
       vBContext.Connection.IOHandler.WriteDirect(TIdBytes(aBufError), -1);
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
 If Assigned(vBContext) then
  Begin
   aAegysSession := SessionList.GetConnection(aDest, aDest, '');
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
     vBContext.Connection.IOHandler.WriteDirect(TIdBytes(aBufError), -1);
//     Socket.Connection.IOHandler.WriteBufferFlush;
    End;
   ProcessMessages;
  End;
End;

Procedure TAegysSession.SendBytes (aBuf          : TAegysBytes);
Begin
 If Assigned(vBContext) then
  Begin
   Try
    vBContext.Connection.CheckForGracefulDisconnect(True);
    If vBContext.Connection.Connected Then
     vBContext.Connection.IOHandler.WriteDirect(TIdBytes(aBuf));
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
 If Assigned(vBContext) then
  Begin
   SetLength(aBuf, aStream.Size);
   vSizeFile := Length(aBuf);
   bStream   := TMemoryStream.Create;
   Try
    bStream.CopyFrom(aStream, aStream.Size);
    bStream.Position := 0;
    bStream.Read(Pointer(aBuf)^, Length(aBuf));
    vBContext.Connection.IOHandler.WriteDirect(ToBytes(vSizeFile), SizeOf(AeInt64));
    vBContext.Connection.IOHandler.WriteDirect(aBuf);
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
 vTcpReceive  := TIdTCPClient.Create(Nil);
 aPackList    := TPackList.Create;
 vSessionTime := 0;
 vProcessData := Nil;
End;

Destructor TAegysClient.Destroy;
Begin
 SetActive(False);
 FreeAndNil(vTcpRequest);
 FreeAndNil(vTcpReceive);
 FreeAndNil(aPackList);
 Inherited;
End;

Procedure TAegysClient.Disconnect;
Begin
 SetActive(False);
End;

Procedure TAegysClient.DisconnectAllPeers;
Begin
 If Assigned(vTcpRequest) Then
  Begin
   If vTcpRequest.Connected Then
    aPackList.Add(vConnection, '', tdmServerCommand, tdcAsync, cDisconnectAllPeers)
   Else
    Raise Exception.Create(cCantExecDisconnected);
  End;
End;

Procedure TAegysClient.DisconnectPeer(aID,
                                      aPass,
                                      aConnection : String);
Begin
 If Assigned(vTcpRequest) Then
  Begin
   If vTcpRequest.Connected Then
    aPackList.Add(vConnection, '', tdmServerCommand, tdcAsync, Format(cDisconnectPeer + '%s&%s&%s', [aID, aPass, aConnection]))
   Else
    Raise Exception.Create(cCantExecDisconnected);
  End;
End;

Procedure TAegysClient.GetConnectedList;
Begin

End;

Procedure TAegysClient.Join(aID, aPass, aVideoQ : String);
Begin
 If Assigned(vTcpRequest) Then
  Begin
   If vTcpRequest.Connected Then
    aPackList.Add(vConnection, '', tdmServerCommand, tdcAsync, Format(cRelation + '%s&%s&%s', [vSessionID, aID, aPass, aVideoQ]))
   Else
    Raise Exception.Create(cCantExecDisconnected);
  End;
End;

Procedure TAegysClient.OnClientCommands(CommandType     : TCommandType;
                                        Connection,
                                        ID,
                                        Command         : String;
                                        aBuf            : TAegysBytes);
Var
 aPackClass : TPackClass;
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
                      If Command = '' Then
                       Begin
                        aPackClass := TPackClass.Create;
                        Try
                         aPackClass.FromBytes(aBuf);
                         Command := aPackClass.Command;
                        Finally
                         FreeAndNil(aPackClass);
                        End;
                       End;
                      If Assigned(vOnKeyboardCapture) Then
                       vOnKeyboardCapture(Command);
                     End;
  tctMouse         : Begin
                      If Command = '' Then
                       Begin
                        aPackClass := TPackClass.Create;
                        Try
                         aPackClass.FromBytes(aBuf);
                         Command := aPackClass.Command;
                        Finally
                         FreeAndNil(aPackClass);
                        End;
                       End;
                      If Assigned(vOnMouseCapture) Then
                       vOnMouseCapture   (Command);
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
 Procedure SetPortSocket;
 Var
  aPackClass : TPackClass;
  aBuf       : TAegysBytes;
 Begin
  ArrayOfPointer :=  [@vConnection, @vClientID, @vClientPassword, @vAlias];
  ParseValues(Command, ArrayOfPointer);
  aPackClass                      := TPackClass.Create;
  Try
   aPackClass.DataMode  := tdmServerCommand;
   aPackClass.DataCheck := tdcAsync;
   aPackClass.Command   := cSetPortSend + vConnection;
   aBuf                 := aPackClass.ToBytes;
   WriteDirect(vTcpRequest, aBuf);
   aPackClass.Command   := cSetPortRec + vConnection;
   aBuf                 := aPackClass.ToBytes;
   WriteDirect(vTcpReceive, aBuf);
  Finally
   SetLength(aBuf, 0);
   FreeAndNil(aPackClass);
   Processmessages;
   Sleep(cDelayThread);
  End;
 End;
Begin
 Case InternalCommand Of
  ticLogin,
  ticDataStatus       : Begin
                         If Assigned(vOnServerLogin) Then
                          vOnServerLogin(Self);
                         SetPortSocket;
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
  ticKick             : Begin
                         ArrayOfPointer :=  [@vConnection, @vClientID, @vClientPassword, @vAlias];
                         ParseValues(Command, ArrayOfPointer);
                         If Assigned(vOnPeerKick) Then
                          vOnPeerKick(vConnection,  vClientID,  vClientPassword,  vAlias);
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
  I             : Integer;
  aBuf          : TAegysBytes;
  aFirstBufSize : AEInteger;
  aBuffSize,
  aPackSize,
  bPackSize     : AEInt64;
  vActiveTcp    : TIdTCPClient;
 Begin
  SetLength(aBuf, 0);
  I := 0;
  While I <= 1 Do
   Begin
    If I = 0 then
     vActiveTcp := vTcpReceive
    Else
     vActiveTcp := vTcpRequest;
    Inc(I);
    If Assigned(vActiveTcp) Then
     Begin
      If vActiveTcp.Connected Then
       Begin
        Try
//         vActiveTcp.IOHandler.ReadTimeout := RequestTimeout;
         If Not (vActiveTcp.IOHandler.InputBufferIsEmpty) Then
          Begin
           Try
            aBuffSize     := vActiveTcp.IOHandler.InputBuffer.Size;
            vActiveTcp.IOHandler.ReadBytes(TIdBytes(aBuf), SizeOf(aPackSize));
            Move(aBuf[0], aPackSize, SizeOf(aPackSize));
            aFirstBufSize := Length(aBuf);
            bPackSize     := 0;
            While (aPackSize <> Length(aBuf)) Do
             Begin
              If bPackSize = 0 Then
               bPackSize := aBuffSize - SizeOf(aPackSize)
              Else
               Begin
                Processmessages;
                vActiveTcp.IOHandler.CheckForDataOnSource(-1);
                bPackSize := vActiveTcp.IOHandler.InputBuffer.Size;
               End;
              If (Length(aBuf) + bPackSize) > aPackSize Then
               bPackSize := aPackSize - Length(aBuf);
              vActiveTcp.IOHandler.ReadBytes(TIdBytes(aBuf), bPackSize);
             End;
            If (aPackSize = Length(aBuf)) And
               (aPackSize > 0)            Then
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
    Processmessages;
   End;
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
 If aPackList.Count > 0 Then
  Begin
   aBuffer := aPackList.Items[0].ToBytes; //ReadPack(aPackNo);
   Try
    If Length(aBuffer) > 0 Then
     SendBytes(aBuffer);
   Finally
    aPackList.Delete(0);
   End;
  End;
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
                                            aMoreData     : String       = '';
                                            CommandType   : TCommandType = tctScreenCapture);
Begin
 If Assigned(vTcpRequest) Then
  If vTcpRequest.Connected Then
   Begin
    If aMoreData = '' Then
     aPackList.Add(vConnection, aID, tdmClientCommand, tdcAsync, CommandType, aBuffer, Format('%s&%s',    [vConnection, vSessionID]))
    Else
     aPackList.Add(vConnection, aID, tdmClientCommand, tdcAsync, CommandType, aBuffer, Format('%s&%s&%s', [vConnection, vSessionID, aMoreData]));
   End;
End;

Procedure TAegysClient.SendBytes           (aBuffer       : TAegysBytes;
                                            aDestMyConn   : Boolean;
                                            aMoreData     : String       = '';
                                            CommandType   : TCommandType = tctScreenCapture);
Begin
 If Assigned(vTcpRequest) Then
  If vTcpRequest.Connected Then
   Begin
    If aMoreData = '' Then
     aPackList.Add( vConnection, '', tdmClientCommand, tdcAsync, CommandType, aBuffer, Format('%s&%s',    [vConnection, vSessionID]),            aDestMyConn)
    Else
     aPackList.Add( vConnection, '', tdmClientCommand, tdcAsync, CommandType, aBuffer, Format('%s&%s&%s', [vConnection, vSessionID, aMoreData]), aDestMyConn);
   End;
End;

Procedure TAegysClient.SendBytes           (aBuffer       : TAegysBytes);
Begin
 If Assigned(vTcpRequest) Then
  If vTcpRequest.Connected Then
   Begin
    vTcpRequest.IOHandler.WriteDirect(TIdBytes(aBuffer));
    Processmessages;
   End;
End;

Procedure TAegysClient.SendCommand         (Value         : String);
Begin
 If Assigned(vTcpRequest) Then
  Begin
   If vTcpRequest.Connected Then
    aPackList.Add(vConnection, '', tdmServerCommand, tdcAsync, Value)
   Else
    Raise Exception.Create(cCantExecDisconnected);
  End;
End;

Procedure TAegysClient.SendCommand         (aDest         : String;
                                            aBuffer       : TAegysBytes);
Var
 aPack : TPackClass;
Begin
 If Assigned(vTcpRequest) Then
  Begin
   If vTcpRequest.Connected Then
    Begin
     aPack := TPackClass.Create;
     Try
      aPack.FromBytes(aBuffer);
      aPackList.Add(vConnection,
                    aDest,
                    tdmClientCommand,
                    tdcAsync,
                    aPack.CommandType,
                    aPack.Command,
                    False);
     Finally
      FreeAndNil(aPack);
     End;
    End
   Else
    Raise Exception.Create(cCantExecDisconnected);
  End;
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
 If Assigned(vTcpRequest) Then
  If vTcpRequest.Connected Then
   aPackList.Add(vConnection, '', tdmClientCommand, tdcAsync, CommandType, Format('%s&%s&%s', [vConnection, vSessionID, Value]), aDestMyConn);
End;

Procedure TAegysClient.SendMessage         (aID,
                                            Value         : String;
                                            CommandType   : TCommandType = tctChat);
Begin
 If Assigned(vTcpRequest) Then
  If vTcpRequest.Connected Then
   aPackList.Add(vConnection, aID, tdmClientCommand, tdcAsync, CommandType, Format('%s&%s&%s', [vConnection, vSessionID, Value]), False);
End;

Procedure TAegysClient.SendMouse  (aID,
                                   Value       : String);
Begin

End;

Procedure TAegysClient.SendMouse  (aDestMyConn : Boolean;
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

Procedure TAegysClient.ThreadDisconnect;
Begin
 Try
  vTcpRequest.OnDisconnected := Nil;
  vTcpReceive.OnDisconnected := Nil;
  vTcpRequest.DisconnectNotifyPeer;
  vTcpReceive.DisconnectNotifyPeer;
 Finally
  vSessionID  := '';
  vSessionPWD := '';
  vActive     := False;
  If Assigned(vOnDisconnect) Then
   vOnDisconnect(Self);
 End;
End;

Procedure TAegysClient.WriteDirect(aClient : TIdTCPClient;
                                   aBuf    : TAegysBytes);
Begin
 If Assigned(aClient) Then
  If aClient.Connected Then
   Begin
    aClient.IOHandler.WriteDirect(TIdBytes(aBuf));
//    aClient.IOHandler.WriteBufferFlush;
    Processmessages;
   End
  Else
   Raise Exception.Create(cDisconnectedByServer);
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

Procedure TAegysClient.NewID(aValue : String);
Begin

End;

Procedure TAegysClient.SetActive(Value : Boolean);
Var
 vWelcomeMessage : String;
 Procedure LoginSocket;
 Var
  aPackClass : TPackClass;
  aBuf       : TAegysBytes;
 Begin
  aPackClass                      := TPackClass.Create;
  Try
   aPackClass.DataMode  := tdmServerCommand;
   aPackClass.DataCheck := tdcAsync;
   aPackClass.Command   := Utf8Decode(cNewID + vWelcomeMessage);
   aBuf                 := aPackClass.ToBytes;
  Finally
   FreeAndNil(aPackClass);
  End;
  Try
//   WriteDirect(vTcpRequest, aBuf);
   WriteDirect(vTcpReceive, aBuf);
  Finally
   SetLength(aBuf, 0);
  End;
 End;
Begin
 vSessionTime := 0;
 KillThread;
 If (Value) Then
  Begin
   If Not vTcpRequest.Connected Then
    Begin
     vTcpRequest.Host           := vHost;
     vTcpReceive.Host           := vHost;
     vTcpRequest.Port           := vPort;
     vTcpReceive.Port           := vPort;
     vTcpRequest.ReadTimeout    := vRequestTimeOut;
     vTcpReceive.ReadTimeout    := vRequestTimeOut;
     vTcpRequest.ConnectTimeout := vConnectTimeOut;
     vTcpReceive.ConnectTimeout := vConnectTimeOut;
     vTcpRequest.OnDisconnected := OnRequestDisconnect;
     vTcpReceive.OnDisconnected := OnRequestDisconnect;
     Try
      vTcpRequest.Connect;
      vTcpReceive.Connect;
      If Assigned(vOnBeforeConnect) Then
       Begin
        vOnBeforeConnect(Self, vWelcomeMessage);
        LoginSocket;
       End;
      If (vTcpRequest.Connected) And (vTcpReceive.Connected) Then
       Begin
        vWelcomeString       := vWelcomeMessage;
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
