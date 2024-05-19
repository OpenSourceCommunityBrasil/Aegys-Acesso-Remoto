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
 SysUtils, Classes, Variants, DateUtils,
 {$IFDEF MSWINDOWS}
    Windows,
 {$ENDIF}
 {$IFDEF FPC}
  Forms,
 {$ELSE}
  {$IF DEFINED(HAS_FMX)}
   {$IF NOT(DEFINED(ANDROID)) and NOT(DEFINED(IOS))}
     FMX.Forms,
   {$IFEND}
  {$ELSE}
     vcl.Forms,
  {$IFEND}
 {$ENDIF}
 //Indy 10.6 Basic Units
 IdContext,         IdTCPConnection,  IdTCPClient,     IdComponent,     IdBaseComponent,
 IdCustomTCPServer, IdTCPServer,      IdStack,         IdExceptionCore, IdGlobal,
 IdSocketHandle,    IdStackConsts,
 //Aegys Basic Units
 uAegysBufferPack, uAegysConsts,    uAegysDataTypes, uAegysThreads;

Type
 TAegysListCommandType = (tlct_NewList, tlct_PeerOn, tlct_PeerOff);
 PAegysMyConnection    = ^TAegysMyConnection;
 TAegysMyConnection    = Class
 Private
  vConnectionActive,
  vRemoteAssist       : Boolean;
  vConnectionString,
  vConnectionID,
  vConnectionPass,
  vConnectionName,
  vConnectionAlias,
  vConnectionGroup,
  vConnectionLastShot : String;
 Protected
 Public
  Constructor Create;
  Destructor  Destroy;Override;
  Function    ToString : String;
  Procedure   FromString(aValue : String);
  Property ConnectionActive   : Boolean Read vConnectionActive   Write vConnectionActive;
  Property RemoteAssist       : Boolean Read vRemoteAssist       Write vRemoteAssist;
  Property ConnectionID       : String  Read vConnectionID       Write vConnectionID;
  Property ConnectionString   : String  Read vConnectionString   Write vConnectionString;
  Property ConnectionPass     : String  Read vConnectionPass     Write vConnectionPass;
  Property ConnectionName     : String  Read vConnectionName     Write vConnectionName;
  Property ConnectionAlias    : String  Read vConnectionAlias    Write vConnectionAlias;
  Property ConnectionGroup    : String  Read vConnectionGroup    Write vConnectionGroup;
  Property ConnectionLastShot : String  Read vConnectionLastShot Write vConnectionLastShot;
 End;
 TAegysMyConnectionList  = Class(TList)
 Private
  Function  GetRec         (Index         : Integer)        : TAegysMyConnection;Overload;
  Procedure PutRec         (Index         : Integer;
                            Item          : TAegysMyConnection);                 Overload;
  Procedure ClearAll;
 Protected
 Public
  Constructor Create;
  Destructor Destroy; Override;
  Function   ToString : String;
  Procedure  FromString(aValue      : String;
                        CommandType : TAegysListCommandType = tlct_NewList);
  Function   GetConnection (aConnStr      : String;
                            Id            : String)        : TAegysMyConnection;
  Procedure  Delete        (Index         : Integer);                       Overload;
  Procedure  Delete        (aConnStr, Id  : String);                        Overload;
  Function   Add           (Item          : TAegysMyConnection) : Integer;  Overload;
  Property   Items         [Index         : Integer]        : TAegysMyConnection Read GetRec Write PutRec; Default;
End;

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
    vDataEncoding      : TIdTextEncoding;
   {$ELSE}
    vDataEncoding      : IIdTextEncoding;
   {$IFEND}
  {$ELSE}
   vDataEncoding       : IIdTextEncoding;
  {$ENDIF}
  vBContext,
  vAContext            : TIdContext;
  vAegysClientStage    : TAegysClientStage;
  vAcceptUnAssist      : Boolean;
  vSessionGroup,
  vConnection,
  vSessionPWD,
  vSessionFixedPWD,
  vSessionAlias,
  vSessionID           : String;
  vInitalRequest,
  vLastRequest         : TDateTime;
  vLogged              : Boolean;
  vBeginPing,
  vLatency             : Integer;
  vAegysSessionList    : TAegysSessionList;
  vAegysConnectionList : TAegysMyConnectionList;
  vPingSense           : TPingSense;
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
  Procedure   SendConnectionList;
  Constructor Create;
  Destructor  Destroy;Override;
  Property    SocketIn                 : TIdContext             Read vAContext               Write vAContext;
  Property    SocketOut                : TIdContext             Read vBContext               Write vBContext;
  Property    Connection               : String                 Read vConnection             Write vConnection;
  Property    ClientStage              : TAegysClientStage      Read vAegysClientStage;
  Property    SessionID                : String                 Read vSessionID              Write vSessionID;
  Property    SessionAlias             : String                 Read vSessionAlias           Write vSessionAlias;
  Property    SessionPWD               : String                 Read vSessionPWD             Write vSessionPWD;
  Property    SessionGroup             : String                 Read vSessionGroup           Write vSessionGroup;
  Property    AcceptUnAssist           : Boolean                Read vAcceptUnAssist         Write vAcceptUnAssist;
  Property    SessionFixedPWD          : String                 Read vSessionFixedPWD        Write vSessionFixedPWD;
  Property    InitalRequest            : TDateTime              Read vInitalRequest;
  Property    LastRequest              : TDateTime              Read vLastRequest;
  Property    Latency                  : Integer                Read vLatency;
  Property    Logged                   : Boolean                Read vLogged;
  Property    SessionList              : TAegysSessionList      Read vAegysSessionList       Write vAegysSessionList;
  Property    MyConnections            : TAegysMyConnectionList Read vAegysConnectionList    Write vAegysConnectionList;
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
 TAegysOnSession           = Procedure (Const Sender      : TAegysSession)       Of Object;
 TAegysOnBeforeConnect     = Procedure (Sender            : TObject;
                                        Var WelcomeString : String)              Of Object;
 TAegysOnConnect           = Procedure (Sender            : TObject)             Of Object;
 TAegysOnDisconnect        = Procedure (Sender            : TObject)             Of Object;
 TAegysOnServerLogin       = Procedure (Sender            : TObject)             Of Object;
 TAegysOnClientServiceCommand = Procedure(Command         : String)              Of Object;
 TAegysOnClientCommand     = Procedure (Connection,
                                        ID,
                                        Command           : String)              Of Object;
 TAegysOnInternalCommand   = Procedure (InternalCommand   : TInternalCommand;
                                        Connection,
                                        ID,
                                        Command           : String)              Of Object;
 TAegysOnInternalPeerList  = Procedure (InternalCommand   : TInternalCommand;
                                        PeerCommand       : String)              Of Object;
 TAegysOnClientBuf         = Procedure (Connection,
                                        ID,
                                        Command           : String;
                                        MultiPack         : Boolean;
                                        PackCount         : AeInteger;
                                        aBuf              : TAegysBytes)         Of Object;
 TAegysOnFileTransfer      = Procedure (Connection,
                                        ID,
                                        CommandMessage    : String;
                                        InternalCommand   : TInternalCommand;
                                        MultiPack         : Boolean;
                                        PackCount         : AeInteger;
                                        aBuf              : TAegysBytes)         Of Object;
 TAegysOnSendPackError     = Procedure (aBuf              : TAegysBytes;
                                        aError            : String)              Of Object;
 TAegysOnNewSession        = Procedure (Var vAegysSession    : TAegysSession;
                                        Connection,
                                        ID                   : String;
                                        Var MyConnectionList : TAegysMyConnectionList) Of Object;
 TAegysOnPeerService        = Procedure (Var vAegysSession   : TAegysSession;
                                         Connection,
                                         ID,
                                         aServiceCommand     : String)           Of Object;

 TAegysOnPeerList          = Procedure (InternalCommand      : TInternalCommand;
                                        Command              : String)           Of Object;
 TAegysOnReceiveCommand    = Procedure (InternalCommand      : TInternalCommand;
                                        Command              : String)           Of Object;
 TAegysOnAccessDenied      = Procedure                                           Of Object;
 TAegysOnPeerConnected     = Procedure (Connection           : String;
                                        Var ClientID,
                                        ClientPassword,
                                        SpecialData          : String)           Of Object;
 TAegysOnPeerDisconnected  = Procedure (Connection           : String;
                                        Var ClientID,
                                        ClientPassword,
                                        Alias             : String)              Of Object;
 TAegysOnBeginTransaction  = Procedure (Connection        : String;
                                        Var ClientID,
                                        Alias             : String)              Of Object;
 TAegysOnBeginTransactionError = Procedure (Connection    : String)              Of Object;
 TAegysOnGetClientDetails  = Procedure (ContextList       : TSessionList;
                                        Value             : String;
                                        Var ClientID,
                                        ClientPassword    : String;
                                        Var Accept        : Boolean)             Of Object;
 TAegysOnReceiveBytes      = Procedure (aBuffer           : TAegysBytes)         Of Object;
 TAegysOnReceiveStream     = Procedure (Const aStream     : TStream;
                                        Var Accept        : Boolean;
                                        Var ErrorMessage  : String)              Of Object;
 TAegysOnReceiveFileStream = Procedure (Const Filename    : String;
                                        Const aStream     : TStream;
                                        Var Accept        : Boolean;
                                        Var ErrorMessage  : String)              Of Object;

Type
 TAegysService = Class(TComponent)
 Protected
  Procedure  Execute(AContext     : TIdContext);
 Private
  vActive                         : Boolean;
  vConnectTimeout,
  vServicePort,
  vServiceTimeout                 : Integer;
  vSessionList                    : TSessionList;
  vIdTCPServer                    : TIdTCPServer;
  vAegysOnNewSession              : TAegysOnNewSession;
  vAegysOnUpdateMyConfigs,
  vAegysOnPeerService             : TAegysOnPeerService;
  vOnClientRequestExecute,
  vOnDisconnect,
  vOnConnect                      : TAegysOnSession;
  vOnGetClientDetails             : TAegysOnGetClientDetails;
  vGarbageTime                    : Integer;
  //{$IFDEF FPC}
  //vDatabaseCharSet                : TDatabaseCharSet;
  //{$ENDIF}
  Procedure  SetActive          (Value                : Boolean);
  Procedure  Disconnect         (AContext             : TIdContext);
  Procedure  Connect            (AContext             : TIdContext);
  Function   GetSessionList                           : TSessionList;
  Function   NewSession         (ID                   : String) : TAegysSession;
  Procedure  PulseDisconnectList(ConnectionString,
                                 ID                   : String);
  Procedure  PulseList          (Var AegysSession     : TAegysSession;
                                 ConnectionString,
                                 ID                   : String;
                                 Var MyConnectionList : TAegysMyConnectionList);
  Procedure  SetSocketDirection (NewID                : String;
                                 Var aSocket          : TIdContext;
                                 aDirection           : TPortDirection = tpdInBound);
 Public
  Constructor Create   (AOwner    : TComponent);Override; //Cria o Componente
  Destructor  Destroy;Override;
  Procedure   Kickuser (aUser     : String);
 Published
  Property Active                 : Boolean                  Read vActive                 Write SetActive;
  //{$IFDEF FPC}
  //Property DatabaseCharSet        : TDatabaseCharSet         Read vDatabaseCharSet        Write vDatabaseCharSet;
  //{$ENDIF}
  Property RequestTimeout         : Integer                  Read vServiceTimeout         Write vServiceTimeout;
  Property ConnectTimeout         : Integer                  Read vConnectTimeout         Write vConnectTimeout;
  Property ServicePort            : Integer                  Read vServicePort            Write vServicePort Default 9092;
  Property GarbageTime            : Integer                  Read vGarbageTime            Write vGarbageTime;
  Property OnConnect              : TAegysOnSession          Read vOnConnect              Write vOnConnect;
  Property OnDisconnect           : TAegysOnSession          Read vOnDisconnect           Write vOnDisconnect;
  Property OnClientRequestExecute : TAegysOnSession          Read vOnClientRequestExecute Write vOnClientRequestExecute;
  Property OnGetClientDetails     : TAegysOnGetClientDetails Read vOnGetClientDetails     Write vOnGetClientDetails;
  Property SessionList            : TSessionList             Read vSessionList;
  Property OnNewSession           : TAegysOnNewSession       Read vAegysOnNewSession      Write vAegysOnNewSession;
  Property OnPeerService          : TAegysOnPeerService      Read vAegysOnPeerService     Write vAegysOnPeerService;
  Property OnUpdateMyConfigs      : TAegysOnPeerService      Read vAegysOnUpdateMyConfigs Write vAegysOnUpdateMyConfigs;
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
 PAegysConnections    = ^TAegysConnections;
 TAegysConnections    = Packed Record
  Connection,
  ClientID,
  ClientPassword,
  Alias               : AeString;
  StartConnectionTime : TDateTime;
  Download,
  Upload              : AEInt64;
 End;
 TAegysConnectionList   = Class(TList)
 Private
  Function  GetRec         (Index         : Integer)           : TAegysConnections;Overload;
  Procedure PutRec         (Index         : Integer;
                            Item          : TAegysConnections);                    Overload;
  Procedure ClearAll;
 Protected
 Public
  Constructor Create;
  Destructor Destroy; Override;
  Function   GetConnection (aConnStr      : String;
                            Id, PWD       : String)             : TAegysConnections;
  Procedure  Delete        (Index         : Integer);                              Overload;
  Procedure  Delete        (aConnStr,
                            Id, PWD       : String);                               Overload;
  Function   Add           (Item          : TAegysConnections)  : Integer;         Overload;
  Function   Add           (aConnStr,
                            aId, aPWD,
                            aAlias        : String)             : Integer;         Overload;
  Property   Items         [Index         : Integer]            : TAegysConnections Read GetRec Write PutRec; Default;
 End;

Type
 TRESTDWWriteMode = (twmDirect, twmBuffer);
 TAegysClient = Class(TComponent)
 Protected
  //Variáveis, Procedures e  Funções Protegidas
  aAegysConnectionList     : TAegysConnectionList;
  vTcpReceive,
  vTcpRequest              : TIdTCPClient;
 Private
  //Variáveis, Procedures e Funções Privadas
  aOldTime                 : TDateTime;
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
  //{$IFDEF FPC}
  //vDatabaseCharSet         : TDatabaseCharSet;
  //{$ENDIF}
  aPackList                : TPackList;
  vProcessData             : TAegysThread;
  vFavConnectionList       : TAegysMyConnectionList;
  vOnIncommingConnect,
  vOnAccessGranted,
  vOnPeerConnected         : TAegysOnPeerConnected;
  vOnAccessDenied          : TAegysOnAccessDenied;
  vOnPeerList              : TAegysOnPeerList;
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
  vOnVideoCapture          : TAegysOnClientBuf;
  vOnFileTransfer          : TAegysOnFileTransfer;
  vOnKeyboardCapture,
  vOnMouseCapture          : TAegysOnClientServiceCommand;
  vOnInternalCommand       : TAegysOnInternalCommand;
  vOnChatReceive           : TAegysOnClientCommand;
  vOnBeginTransaction      : TAegysOnBeginTransaction;
  vOnBeginTransactionError : TAegysOnBeginTransactionError;
  vOnSendPackError         : TAegysOnSendPackError;
  Procedure WriteDirect         (aClient         : TIdTCPClient;
                                 aBuf            : TAegysBytes);
  Procedure KillThread;
  Procedure OnBeforeExecuteData (Var abPackList  : TPackList);
  Procedure OnExecuteData       (Var abPackList  : TPackList;
                                 abPackNo        : AeInt64);
  Procedure OnAbortData;
  Procedure OnThreadRequestError(ErrorCode       : Integer;
                                 MessageError    : String);
  Procedure OnServiceCommands   (InternalCommand : TInternalCommand;
                                 Command         : String);
  Procedure OnClientCommands    (CommandType     : TCommandType;
                                 InternalCommand : TInternalCommand;
                                 Connection,
                                 ID,
                                 Command         : String;
                                 MultiPack       : Boolean;
                                 PackCount       : AeInteger;
                                 aBuf            : TAegysBytes);
  Procedure OnRequestDisconnect (Sender          : TObject);
  Procedure SetActive           (Value           : Boolean);
  Procedure OnCheckDisconnect   (aType           : Char);
 Public
  //Métodos, Propriedades, Variáveis, Procedures e Funções Publicas
  Constructor Create        (AOwner       : TComponent);     Override;
  Destructor  Destroy;                                       Override;
  Procedure   ThreadDisconnect;
  Procedure   SetSessionData(ConnectionString,
                             ID,
                             PWD          : String);
  Procedure   NewID         (aValue       : String);
  Function    GetPacketLength: Integer;
  Procedure   Connect;
  Procedure   Disconnect;
  Procedure   DisconnectAllPeers;
  Procedure   DisconnectPeer(aID,
                             aPass,
                             aConnection  : String);
  Procedure   Join          (aID,
                             aPass,
                             aVideoQ      : String);
  Procedure   SendCommand   (Value        : String);                         Overload;
  Procedure   SendCommand   (aDest        : String;
                             aBuffer      : TAegysBytes);                    Overload;
  Procedure   SendMessage   (Value        : String;
                             aDestMyConn  : Boolean;
                             CommandType  : TCommandType = tctChat);         Overload;
  Procedure   SendMessage   (aID, Value   : String;
                             CommandType  : TCommandType = tctChat);         Overload;
  Procedure   SendMouse     (aID, Value   : String);                         Overload;
  Procedure   SendMouse     (aDestMyConn  : Boolean;
                             Value        : String);                         Overload;
  Procedure   SendMonitor   (aID, Value   : String);                         Overload;
  Procedure   SendMonitor   (aDestMyConn  : Boolean;
                             Value        : String);                         Overload;
  procedure   SendQuality   (aID,
                             Value        : String);
  Procedure   SendKeyboard  (aID, Value   : String);                         Overload;
  Procedure   SendKeyboard  (aDestMyConn  : Boolean;
                             Value        : String);                         Overload;
  Procedure   SendBytes     (aID          : String;
                             aBuffer      : TAegysBytes;
                             aMoreData    : String       = '';
                             CommandType  : TCommandType = tctScreenCapture);Overload;
  Procedure   SendBytes     (aBuffer      : TAegysBytes;
                             aDestMyConn  : Boolean;
                             aMoreData    : String       = '';
                             CommandType  : TCommandType = tctScreenCapture);Overload;
  Procedure   SendBytes     (aBuffer      : TAegysBytes;
                             aWriteMode   : TRESTDWWriteMode = twmDirect);                 Overload;
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
  Property    ConnectionList             : TAegysConnectionList          Read aAegysConnectionList     Write aAegysConnectionList;
  Property    FavConnectionList          : TAegysMyConnectionList        Read vFavConnectionList       Write vFavConnectionList;
  //{$IFDEF FPC}
  //Property    DatabaseCharSet            : TDatabaseCharSet              Read vDatabaseCharSet         Write vDatabaseCharSet;
  //{$ENDIF}
  Property    WelcomeString              : String                        Read vWelcomeString;
  Property    OnReceiveCommand           : TAegysOnReceiveCommand        Read vOnReceiveCommand        Write vOnReceiveCommand;
  Property    OnReceiveBytes             : TAegysOnReceiveBytes          Read vOnReceiveBytes          Write vOnReceiveBytes;
  Property    OnReceiveStream            : TAegysOnReceiveStream         Read vOnReceiveStream         Write vOnReceiveStream;
  Property    OnReceiveFileStream        : TAegysOnReceiveFileStream     Read vOnReceiveFileStream     Write vOnReceiveFileStream;
  Property    OnBeforeConnect            : TAegysOnBeforeConnect         Read vOnBeforeConnect         Write vOnBeforeConnect;
  Property    OnConnect                  : TAegysOnConnect               Read vOnConnect               Write vOnConnect;
  Property    OnDisconnect               : TAegysOnDisconnect            Read vOnDisconnect            Write vOnDisconnect;
  Property    OnPeerConnected            : TAegysOnPeerConnected         Read vOnPeerConnected         Write vOnPeerConnected;
  Property    OnIncommingConnect         : TAegysOnPeerConnected         Read vOnIncommingConnect      Write vOnIncommingConnect;
  Property    OnAccessDenied             : TAegysOnAccessDenied          Read vOnAccessDenied          Write vOnAccessDenied;
  Property    OnPeerDisconnected         : TAegysOnPeerDisconnected      Read vOnPeerDisconnected      Write vOnPeerDisconnected;
  Property    OnPeerKick                 : TAegysOnPeerDisconnected      Read vOnPeerKick              Write vOnPeerKick;
  Property    OnServerLogin              : TAegysOnServerLogin           Read vOnServerLogin           Write vOnServerLogin;
  Property    OnScreenCapture            : TAegysOnClientBuf             Read vOnScreenCapture         Write vOnScreenCapture;
  Property    OnAudioCapture             : TAegysOnClientBuf             Read vOnAudioCapture          Write vOnAudioCapture;
  Property    OnVideoCapture             : TAegysOnClientBuf             Read vOnVideoCapture          Write vOnVideoCapture;
  Property    OnFileTransfer             : TAegysOnFileTransfer          Read vOnFileTransfer          Write vOnFileTransfer;
  Property    OnKeyboardCapture          : TAegysOnClientServiceCommand  Read vOnKeyboardCapture       Write vOnKeyboardCapture;
  Property    OnMouseCapture             : TAegysOnClientServiceCommand  Read vOnMouseCapture          Write vOnMouseCapture;
  Property    OnChatReceive              : TAegysOnClientCommand         Read vOnChatReceive           Write vOnChatReceive;
  Property    OnClientInternalCommand    : TAegysOnInternalCommand       Read vOnInternalCommand       Write vOnInternalCommand;
  Property    OnBeginTransaction         : TAegysOnBeginTransaction      Read vOnBeginTransaction      Write vOnBeginTransaction;
  Property    OnBeginTransactionError    : TAegysOnBeginTransactionError Read vOnBeginTransactionError Write vOnBeginTransactionError;
  Property    OnAccessGranted            : TAegysOnPeerConnected         Read vOnAccessGranted         Write vOnAccessGranted;
  Property    OnSendPackError            : TAegysOnSendPackError         Read vOnSendPackError         Write vOnSendPackError;
  Property    OnPeerList                 : TAegysOnPeerList              Read vOnPeerList              Write vOnPeerList;
End;

Procedure ProcessMessages;

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

Procedure TAegysService.PulseDisconnectList(ConnectionString,
                                            ID                   : String);
Var
 I, A       : Integer;
 aPackClass : TPackClass;
 aBuf       : TAegysBytes;
Begin
 For I := vSessionList.Count -1 DownTo 0 Do
  Begin
   For A := vSessionList[I].vAegysConnectionList.Count -1 Downto 0 Do
    Begin
     If (vSessionList[I].vAegysConnectionList[A].vConnectionString = ConnectionString) Or
        (vSessionList[I].vAegysConnectionList[A].vConnectionID     = ID) Then
      Begin
       vSessionList[I].vAegysConnectionList[A].vConnectionActive := False;
       aPackClass := TPackClass.Create;
       Try
        aPackClass.DataMode    := tdmClientCommand;
        aPackClass.CommandType := tctPeerList;
        aPackClass.Command     := Format('%s%s', [cPeerOff, EncodeStrings(ConnectionString + '|' + ID)]);
        aBuf                   := aPackClass.ToBytes;
        vSessionList[I].SendBytes(aBuf);
       Finally
        SetLength(aBuf, 0);
        FreeAndNil(aPackClass);
       End;
       Break;
      End;
    End;
  End;
End;

Procedure TAegysService.PulseList(Var AegysSession     : TAegysSession;
                                  ConnectionString,
                                  ID                   : String;
                                  Var MyConnectionList : TAegysMyConnectionList);
Var
 I, A, X    : Integer;
 aPackClass : TPackClass;
 aBuf       : TAegysBytes;
Begin
 For I := vSessionList.Count -1 DownTo 0 Do
  Begin
   For A := vSessionList[I].vAegysConnectionList.Count -1 Downto 0 Do
    Begin
     If (vSessionList[I].vAegysConnectionList[A].vConnectionString = ConnectionString) Or
        (vSessionList[I].vAegysConnectionList[A].vConnectionID     = ID) Then
      Begin
       vSessionList[I].vAegysConnectionList[A].vConnectionString := ConnectionString;
       vSessionList[I].vAegysConnectionList[A].vConnectionActive := True;
       aPackClass := TPackClass.Create;
       Try
        aPackClass.DataMode    := tdmClientCommand;
        aPackClass.CommandType := tctPeerList;
        aPackClass.Command     := Format('%s%s', [cPeerOn, EncodeStrings(ConnectionString + '|' + ID)]);
        aBuf                   := aPackClass.ToBytes;
        vSessionList[I].SendBytes(aBuf);
       Finally
        SetLength(aBuf, 0);
        FreeAndNil(aPackClass);
       End;
      End;
     For X := AegysSession.vAegysConnectionList.Count -1 Downto 0 Do
      Begin
       If (vSessionList[I].vConnection = AegysSession.vAegysConnectionList[X].vConnectionString) Or
          (vSessionList[I].vSessionID  = AegysSession.vAegysConnectionList[X].vConnectionID) Then
        Begin
         AegysSession.vAegysConnectionList[X].vConnectionActive := True;
         AegysSession.vAegysConnectionList[X].vConnectionString := vSessionList[I].vConnection;
         Break;
        End;
      End;
    End;
  End;
// If AegysSession.vAegysConnectionList.Count > 0 Then
 AegysSession.SendConnectionList;
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
Begin
 AContext.Connection.IOHandler.ConnectTimeout := vConnectTimeout;
 AContext.Connection.IOHandler.ReadTimeout    := vServiceTimeout;
End;

Constructor TAegysService.Create(AOwner: TComponent);
Begin
 Inherited;
 vOnGetClientDetails            := Nil;
 vGarbageTime                   := 7000;
 vConnectTimeout                := 7000;
 vServiceTimeout                := cServiceTimeout;
 vServicePort                   := 9092;
 vIdTCPServer                   := TIdTCPServer.Create(Nil);
 vSessionList                   := TSessionList.Create;
 {$IFDEF FPC}
  vIdTCPServer.OnExecute        := @Execute;
  vIdTCPServer.OnDisconnect     := @Disconnect;
  vIdTCPServer.OnConnect        := @Connect;
  //vEncoding                     := esUtf8;
  //vDatabaseCharSet              := csUndefined;
 {$ELSE}
  vIdTCPServer.OnExecute        := Execute;
  vIdTCPServer.OnDisconnect     := Disconnect;
  vIdTCPServer.OnConnect        := Connect;
 {$ENDIF}
 vIdTCPServer.TerminateWaitTime := vGarbageTime;
End;

Destructor TAegysService.Destroy;
Begin
 SetActive(False);
 FreeAndNil(vSessionList);
 FreeAndNil(vIdTCPServer);
 Inherited;
End;

Procedure TAegysService.Disconnect(AContext : TIdContext);
Var
 aMessageError : String;
 vAegysSession : TAegysSession;
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
      Except
      End;
     End;
   End;
 End;
 Procedure NilContext;
 Begin
  {$IFNDEF FPC}
   {$IF Not Defined(HAS_FMX)}
    AContext.Data := Nil;
   {$ELSE}
    {$IFDEF HAS_UTF8}
     {$IF CompilerVersion > 33}AContext.Data{$ELSE}AContext.DataObject{$IFEND} := Nil;
    {$ELSE}
     AContext.DataObject := Nil;
    {$ENDIF}
   {$IFEND}
  {$ELSE}
   AContext.Data := Nil;
  {$ENDIF}
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
    //Release my connection
    If Assigned(vAegysSession) Then
     Begin
      If vAegysSession.vAContext = AContext Then
       Begin
        PulseDisconnectList(vAegysSession.Connection, vAegysSession.vSessionID);
        If Assigned(vOnDisconnect) Then
         vOnDisconnect(vAegysSession);
        //TODO Delete my connection from others lists
        If Assigned(vSessionList) Then
         vSessionList.Delete(vAegysSession);
        vAegysSession.Free;
        NilContext;
       End
      Else
       NilContext;
     End;
   End;
 Except
  On E : EIdSocketError Do Abort;
  On E : EIdReadTimeout Do ;
  On E : Exception      Do
   Begin
    aMessageError := E.Message;
    Try
     If Assigned(vAegysSession) Then
      Begin
       If Not Assigned(AContext) Then
        Abort;
       If AContext.Connection <> Nil Then
        If AContext.Connection.IOHandler <> Nil Then
         If Not AContext.Connection.IOHandler.Connected Then
          Abort;
      End
     Else
      Abort;
    Except
     aMessageError := E.Message;
    End;
   End;
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
   If (Items[I].SessionID         = aID)    And
      ((Items[I].SessionPWD       = aPass)  Or
       ((Items[I].SessionFixedPWD = aPass)  And
        (Items[I].SessionFixedPWD <> '')))  Then
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
 If ((Index > -1)     And
     (Index < Count)) And
    (Count > 0)       Then
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
 aOldTimeOut,
 aFirstBufSize    : AEInteger;
 aBuffSize,
 bPackSize,
 aPackSize        : AEInt64;
 aMessageError,
 vPorDirection,
 vSockCommand,
 vOldCommand,
 vCommand,
 vMonitor,
 vMyID,
 vYouID,
 vYouPass,
 vBestQ,
 vUser,
 vPassword        : String;
 vInternalCommandOld,
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
 aPackSize  := 0;
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
  vAccept := True;
  Try
   If AContext.Connection.IOHandler.InputBufferIsEmpty Then
    Begin
     AContext.Connection.CheckForGracefulDisconnect(False);
     Sleep(cDelayThread div 2);
     Exit;
    End;
  Except
   AContext.Connection.CheckForGracefulDisconnect(False);
   If Not AContext.Connection.Connected Then
    Raise;
  End;
  If Not AContext.Connection.IOHandler.InputBufferIsEmpty Then
   Begin
    aBuffSize     := AContext.Connection.IOHandler.InputBuffer.Size;
    Try
     AContext.Connection.IOHandler.ReadBytes(TIdBytes(aBuf), SizeOf(aPackSize));
    Except
     AContext.Connection.CheckForGracefulDisconnect(False);
     If Not AContext.Connection.Connected Then
      Raise;
    End;
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
         If AContext.Connection.IOHandler.CheckForDataOnSource(vServiceTimeout) Then
          Begin
           Try
            If Not AContext.Connection.IOHandler.InputBufferIsEmpty Then
             bPackSize := AContext.Connection.IOHandler.InputBuffer.Size;
           Except
            AContext.Connection.CheckForGracefulDisconnect(False);
            If Not AContext.Connection.Connected Then
             Raise;
           End;
          End
         Else
          Break;
        Except
         On E : Exception      Do
          Begin
           aMessageError := E.Message;
           Break;
          End;
        End;
       End;
      If (Length(aBuf) + bPackSize) > aPackSize Then
       bPackSize := aPackSize - Length(aBuf);
      If bPackSize > 0 Then
       Begin
        Try
         AContext.Connection.IOHandler.ReadBytes(TIdBytes(aBuf), bPackSize);
         Processmessages;
        Except
         AContext.Connection.CheckForGracefulDisconnect(False);
         If Not AContext.Connection.Connected Then
          Raise;
        End;
       End
      Else
       Break;
     End;
   End;
  If (aPackSize > 0)             And
     (aPackSize <> Length(aBuf)) Then
   Begin
    SetLength(aBuf, 0);
    Exit;// Raise Exception.Create(cInvalidBufferData);
   End;
  vDataCommand     := False;
  vInternalCommand := ticNone;
  If (aPackSize = Length(aBuf)) And
     (aPackSize > 0)            Then
   Begin
    aPackClass := TPackClass.Create;
    Try
     aPackClass.FromBytes(aBuf);
     vCommand    := aPackClass.Command;
     If vCommand <> '' Then
      ParseCommand(vCommand, vInternalCommand);
     vOldCommand := vCommand;
     Processmessages;
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
                            If Not Assigned(vAegysSession) Then
                             vAegysSession := FindSession(AContext);
                            If Assigned(vAegysSession) Then
                             Begin
                              If Assigned(vAegysOnNewSession) Then
                               vAegysOnNewSession(vAegysSession, vAegysSession.Connection, vAegysSession.vSessionID, vAegysSession.vAegysConnectionList);
                              PulseList(vAegysSession, vAegysSession.Connection, vAegysSession.vSessionID, vAegysSession.vAegysConnectionList);
                             End;
                           End;
                         End;
       End;
       If Not Assigned(vAegysSession) Then
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
       (vAction)               And
       Assigned(aPackClass)    And
       Assigned(vAegysSession) Then
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
                  ticConnectionList, //Edit My Connection
                  ticConnectionEdit  : Begin
                                        vOldCommand := DecodeStrings(vCommand);
                                        vMySession  := vSessionList.GetConnection(vAegysSession.SocketIn);
                                        If Assigned(vMySession) Then
                                         If Assigned(vAegysOnUpdateMyConfigs) Then
                                          vAegysOnUpdateMyConfigs(vMySession, vMySession.vConnection, vMySession.vSessionID, vOldCommand);
                                       End;
                  ticPeerService     : Begin
                                        //Action Peerlist
                                        vOldCommand := vCommand;
                                        vMySession  := vSessionList.GetConnection(vAegysSession.SocketIn);
                                        If Assigned(vMySession) Then
                                         If Assigned(vAegysOnPeerService) Then
                                          vAegysOnPeerService(vMySession, vMySession.vConnection, vMySession.vSessionID, vCommand);
                                        ParseCommand(vOldCommand, vInternalCommandOld);
                                        If vInternalCommandOld In [ticNewPeerData, ticDeletePeerData] Then
                                         Begin
                                          If vInternalCommandOld = ticDeletePeerData Then
                                           vAegysSession.vAegysConnectionList.Delete(vOldCommand, vOldCommand);
                                          PulseList(vAegysSession, vAegysSession.Connection, vAegysSession.vSessionID, vAegysSession.vAegysConnectionList);
                                         End;
                                        vOldCommand := '';
                                        vCommand    := '';
                                       End;
                  ticFindID          : Begin
                                        ArrayOfPointer := [@vYouID];
                                        ParseValues(vCommand, ArrayOfPointer);
                                        vYouSession := vSessionList.GetConnection(vYouID);
                                        If Assigned(aPackClass) Then
                                         FreeAndNil(aPackClass);
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
                                               If Assigned(vAegysSession.SessionList) Then
                                                aPackClass.Command  := cKickPeer + Format('%s&%s&%s', [vAegysSession.SessionList[I].Connection,
                                                                                                       vAegysSession.SessionList[I].vSessionID,
                                                                                                       vAegysSession.SessionList[I].SessionPWD]);
                                               vAegysSession.SendBytes(aPackClass.ToBytes);
                                               If Assigned(vAegysSession.SessionList) Then
                                                Begin
                                                 vAegysSession.SessionList[I].SessionList.Delete(vAegysSession.Connection,
                                                                                                 vAegysSession.vSessionID,
                                                                                                 vAegysSession.SessionPWD);
                                                 vAegysSession.SessionList.Delete(I);
                                                End;
                                              End;
                                             End;
                                           Finally
                                            FreeAndNil(aPackClass);
                                           End;
                                          End;
                  ticDisconnectPeer     : Begin
                                           ArrayOfPointer := [@vYouID, @vYouPass, @vSockCommand];
                                           ParseValues(vCommand, ArrayOfPointer);
                                           If Assigned(vAegysSession.SessionList) Then
                                            vYouSession := vAegysSession.SessionList.GetConnection(vSockCommand, vYouID, vYouPass);
                                           If Assigned(aPackClass) Then
                                            FreeAndNil(aPackClass);
                                           If (vYouSession = Nil) Then //Error defined, no have connection
                                            Begin
                                             If Assigned(aPackClass) Then
                                              FreeAndNil(aPackClass);
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
                                             If Assigned(aPackClass) Then
                                              FreeAndNil(aPackClass);
                                             aPackClass  := TPackClass.Create;
                                             Try
                                              aPackClass.DataMode    := tdmServerCommand;
                                              aPackClass.DataCheck   := tdcAsync;
                                              Try
                                               aPackClass.Command    := cKickPeer + Format('%s&%s&%s', [vAegysSession.Connection,
                                                                                                        vAegysSession.vSessionID,
                                                                                                        vAegysSession.SessionPWD]);
                                               vYouSession.SendBytes(aPackClass.ToBytes);
                                              Finally
                                               If Assigned(vAegysSession.SessionList) Then
                                                aPackClass.Command    := cKickPeer + Format('%s&%s&%s', [vYouSession.Connection,
                                                                                                         vYouSession.vSessionID,
                                                                                                         vYouSession.SessionPWD]);
                                               vAegysSession.SendBytes         (aPackClass.ToBytes);
                                               If Assigned(vYouSession.SessionList) Then
                                                vYouSession.SessionList.Delete  (vAegysSession.Connection,
                                                                                 vAegysSession.vSessionID,
                                                                                 vAegysSession.SessionPWD);
                                               If Assigned(vAegysSession.SessionList) Then
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
                                          If Assigned(aPackClass) Then
                                           FreeAndNil(aPackClass);
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
                                          If Assigned(aPackClass) Then
                                           FreeAndNil(aPackClass);
                                          aPackClass := TPackClass.Create;
                                          Try
                                           aPackClass.DataMode   := tdmServerCommand;
                                           aPackClass.DataCheck  := tdcAsync;
                                           If vInternalCommand    = ticCheckPass Then
                                            Begin
                                             If Assigned(vYouSession) Then
                                              aPackClass.Command := cAccessGranted;
                                            End
                                           Else
                                            Begin
                                             If Assigned(vYouSession) Then
                                              aPackClass.Command := cConnectedPeer;
                                            End;
                                           aPackClass.Command    := aPackClass.Command + Format('%s&%s&%s', [vYouSession.Connection,
                                                                                                             vYouSession.vSessionID,
                                                                                                             vYouSession.SessionPWD]);
                                           aBuf                  := aPackClass.ToBytes;
                                           If Assigned(vMySession)    Then
                                            Begin
                                             vMySession.SendBytes(aBuf);
                                             aPackClass.Command  := cConnectedPeer + Format('%s&%s&%s', [vMySession.Connection,
                                                                                                         vMySession.vSessionID,
                                                                                                         vMySession.SessionPWD]);
                                             aBuf                := aPackClass.ToBytes;
                                             If Assigned(vYouSession) Then
                                              vYouSession.SendBytes(aBuf);
                                            End;
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
                ticChangeImageQuality: Begin
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
              vAegysSession.SendBytes(aPackClass.ToBytes, aPackClass.ProxyToMyConnectionList)
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
  If Assigned(aPackClass) Then
   FreeAndNil(aPackClass);
  If Assigned(vAegysSession) Then
   Begin
    If vAegysSession.vAegysClientStage = csRejected Then
     Begin
      If AContext.Connection <> Nil Then
       If AContext.Connection.IOHandler <> Nil Then
        AContext.Connection.IOHandler.CloseGracefully;
//     AContext.Connection.Disconnect;
      Exit;
     End;
   End;
  Sleep(cDelayThread div 2);
  If AContext.Connection <> Nil Then
   If AContext.Connection.IOHandler <> Nil Then
    Begin
     AContext.Connection.IOHandler.CheckForDisconnect(False);
     If Not AContext.Connection.IOHandler.Connected Then
      Abort;
    End;
  Processmessages;
 Except
  On E : EIdSocketError Do Abort;
  On E : EIdReadTimeout Do ;
  On E : Exception      Do
   Begin
    If Assigned(aPackClass) Then
     FreeAndNil(aPackClass);
    aMessageError := E.Message;
    Try
     If Assigned(vAegysSession) Then
      Begin
       If Not Assigned(AContext) Then
        Begin
         Disconnect(Nil);
         Abort;
        End;
       If AContext.Connection <> Nil Then
        Begin
         If AContext.Connection.IOHandler <> Nil Then
          Begin
           AContext.Connection.IOHandler.CheckForDisconnect;
           If Not AContext.Connection.IOHandler.Connected Then
            Abort
           Else
            Begin
             If vOldCommand <> '' Then
              Begin
               ArrayOfPointer := [@vSockCommand, @vYouID, @vYouPass];
               ParseValues(vOldCommand, ArrayOfPointer);

               aPackClass  := TPackClass.Create;
               aPackClass.DataMode    := tdmServerCommand;
               aPackClass.DataCheck   := tdcAsync;
               Try
                aPackClass.Command    := cKickPeer + Format('%s&%s&%s', [vSockCommand, 
                                                                         vYouID, 
                                                                         vYouPass]);
                vAegysSession.SendBytes         (aPackClass.ToBytes);
               Finally
                If Assigned(vAegysSession.SessionList) Then
                 vAegysSession.SessionList.Delete(vSockCommand, vYouID, vYouPass);
               End;
              End;
            End; 
          End
         Else
          Begin
           Disconnect(AContext);
           Abort;
          End;
        End
       Else
        Begin
         Disconnect(AContext);
         Abort;
        End;
      End
     Else
      Begin
       Disconnect(Nil);
       Abort;
      End;
    Except
     aMessageError := E.Message;
    End;
   End;
 End;
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
  {$IF NOT(DEFINED(ANDROID)) and NOT(DEFINED(IOS))}
   Application.ProcessMessages;
  {$IFEND}
 {$ELSE}
  TThread.Synchronize(Nil, @Application.ProcessMessages);
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
     Processmessages;
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
 vAContext            := Nil;
 vBContext            := Nil;
 vSessionID           := '';
 vSessionPWD          := '';
 vSessionFixedPWD     := '';
 vAcceptUnAssist      := False;
 vLastRequest         := Now;
 vLogged              := False;
 {$IFNDEF FPC}
  {$IF (DEFINED(OLDINDY))}
   vDataEncoding      := enDefault;
  {$ELSE}
   vDataEncoding      := IndyTextEncoding_UTF8;
  {$IFEND}
 {$ELSE}
  vDataEncoding       := IndyTextEncoding_UTF8;
 {$ENDIF};
 vAegysSessionList    := TAegysSessionList.Create;
 vAegysConnectionList := TAegysMyConnectionList.Create;
 vPingSense           := tpsPing;
end;

Destructor TAegysSession.Destroy;
 Procedure BroadCastDisconnect;
 Var
  I          : Integer;
  aPackClass : TPackClass;
  aBuf       : TAegysBytes;
 Begin
  If Assigned(vAegysSessionList) Then
   Begin
    aPackClass := TPackClass.Create;
    Try
     aPackClass.DataMode  := tdmServerCommand;
     aPackClass.DataCheck := tdcAsync;
     aPackClass.Command   := cDisconnectedPeer + Format('%s&%s&%s', [Connection,
                                                                     vSessionID,
                                                                     SessionPWD]);
     aBuf                 := aPackClass.ToBytes;
    Finally
     FreeAndNil(aPackClass);
    End;
    For I := vAegysSessionList.Count -1 DownTo 0 Do
     Begin
      Try
       If Assigned(vAegysSessionList[I].vBContext.Connection) Then
        Begin
         If vAegysSessionList[I].vBContext.Connection.Connected Then
          If Assigned(vAegysSessionList[I].vBContext.Connection.IOHandler) Then
           vAegysSessionList[I].vBContext.Connection.IOHandler.WriteDirect(TIdBytes(aBuf));
        End;
      Except
      End;
     End;
    Processmessages;
   End;
 End;
Begin
 If Assigned(Self) Then
  Begin
   BroadCastDisconnect;
   If Assigned(vAegysSessionList) Then
    FreeAndNil(vAegysSessionList);
   If Assigned(vAegysConnectionList) Then
    FreeAndNil(vAegysConnectionList);
   Inherited;
  End;
End;

procedure TAegysSession.Kick(Gracefully: Boolean);
begin
 Try
  If Assigned(vAContext) Then
   Begin
    If Assigned(vAContext.Connection) Then
     Begin
      Processmessages;
      If Not Gracefully Then
       vAContext.Connection.IOHandler.Close
      Else
       Begin
        vAContext.Connection.IOHandler.CloseGracefully;
        vAContext.Connection.IOHandler.CheckForDisconnect(False, False);
       End;
     End;
   End;
  If Assigned(vBContext) Then
   Begin
    If Assigned(vBContext.Connection) Then
     Begin
      Processmessages;
      If Not Gracefully Then
       vBContext.Connection.IOHandler.Close
      Else
       Begin
        vBContext.Connection.IOHandler.CloseGracefully;
        vBContext.Connection.IOHandler.CheckForDisconnect(False, False);
       End;
     End;
   End;
  Processmessages;
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
                                         Id, PWD    : String)  : TAegysSession;
Var
 I : Integer;
Begin
 Result := Nil;
 If Not Assigned(Self) Then
  Exit;
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
     If Assigned(TAegysSession(TList(Self).Items[Index])) Then
      TAegysSession(TList(Self).Items[Index]).Free;
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
  If Assigned(vAContext.Connection) Then
   Begin
    Processmessages;
    Result := vAContext.Connection.IOHandler.ReadLn;
    Processmessages;
   End;
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
        Processmessages;
        If Assigned(vBContext.Connection) Then
         vBContext.Connection.IOHandler.WriteDirect(TIdBytes(aBufError), -1);
        Processmessages;
       End;
     End;
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
       Try
        If Assigned(vBContext.Connection) Then
         vBContext.Connection.IOHandler.WriteDirect(TIdBytes(aBufError), -1);
       Except
        vBContext.Connection.CheckForGracefulDisconnect(False);
        If Not vBContext.Connection.Connected Then
         Raise;
       End;
      End;
     Processmessages;
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
     Processmessages;
     Try
      If Assigned(vBContext.Connection) Then
       If Assigned(vBContext.Connection.IOHandler) Then
        vBContext.Connection.IOHandler.WriteDirect(TIdBytes(aBufError), -1);
      Processmessages;
     Except
      vBContext.Connection.CheckForGracefulDisconnect(False);
      If Not vBContext.Connection.Connected Then
       Raise;
     End;
    End;
  End;
End;

Procedure TAegysSession.SendBytes (aBuf          : TAegysBytes);
Begin
 If Assigned(vBContext) then
  Begin
   Try
    If Assigned(vBContext.Connection) Then
     Begin
      Processmessages;
      If vBContext.Connection.Connected Then
       If Assigned(vBContext.Connection.IOHandler) Then
        vBContext.Connection.IOHandler.WriteDirect(TIdBytes(aBuf));
      Processmessages;
     End;
   Except
    If Assigned(vBContext.Connection) Then
     Begin
      vBContext.Connection.CheckForGracefulDisconnect(False);
      If Not vBContext.Connection.Connected Then
       Raise;
     End;
   End;
  End
 Else If Assigned(vAContext) then
  Begin
   Try
    If Assigned(vAContext.Connection) Then
     Begin
      Processmessages;
      If vAContext.Connection.Connected Then
       If Assigned(vAContext.Connection.IOHandler) Then
        vAContext.Connection.IOHandler.WriteDirect(TIdBytes(aBuf));
      Processmessages;
     End;
   Except
    vAContext.Connection.CheckForGracefulDisconnect(False);
    If Not vAContext.Connection.Connected Then
     Raise;
   End;
  End;
End;

Procedure TAegysSession.SendConnectionList;
Var
 aPackClass : TPackClass;
 aBuf       : TAegysBytes;
 vNewList   : String;
 Function MyConfigs : String;
 Begin
  Result := EncodeStrings(Format('%s|%d|%s|%s', [vSessionAlias,
                                                 Integer(vAcceptUnAssist),
                                                 vSessionFixedPWD,
                                                 vSessionGroup]));
 End;
Begin
 aPackClass := TPackClass.Create;
 Try
  vNewList               := vAegysConnectionList.ToString;
  aPackClass.DataMode    := tdmClientCommand;
  aPackClass.CommandType := tctPeerList;
  aPackClass.Command     := Format('%s%s', [cNewPeerList, vNewList]) +
                            Format('%s%s', [cMyConfigs,   MyConfigs]);
  aBuf                   := aPackClass.ToBytes;
  vBContext.Connection.IOHandler.WriteDirect(TidBytes(aBuf));
//  SendBytes(aBuf);
 Finally
  SetLength(aBuf, 0);
  FreeAndNil(aPackClass);
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
    If Assigned(vBContext.Connection) Then
     Begin
      Processmessages;
      vBContext.Connection.IOHandler.WriteDirect(ToBytes(vSizeFile), SizeOf(AeInt64));
      vBContext.Connection.IOHandler.WriteDirect(aBuf);
      Processmessages;
     End;
   Finally
    SetLength(aBuf, 0);
    FreeAndNil(bStream);
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
 aAegysConnectionList := TAegysConnectionList.Create;
 vFavConnectionList   := TAegysMyConnectionList.Create;
 aPackList    := TPackList.Create;
 vSessionTime := 0;
 vProcessData := Nil;
 vRequestTimeOut := 10000;
 vConnectTimeOut := 5000;
End;

Destructor TAegysClient.Destroy;
Begin
 SetActive(False);
 If Assigned(vTcpRequest) Then
  Begin
   If vTcpRequest.Connected Then
    vTcpRequest.Disconnect;
   FreeAndNil(vTcpRequest);
  End;
 If Assigned(vTcpReceive) Then
  Begin
   If vTcpReceive.Connected Then
    vTcpReceive.Disconnect;
   FreeAndNil(vTcpReceive);
  End;
 FreeAndNil(aPackList);
 If Assigned(aAegysConnectionList) Then
  FreeAndNil(aAegysConnectionList);
 If Assigned(vFavConnectionList) Then
  FreeAndNil(vFavConnectionList);
 Inherited;
End;

Procedure TAegysClient.Disconnect;
Begin
 SetActive(False);
End;

Procedure TAegysClient.DisconnectAllPeers;
Begin
 If Assigned(Self) Then
  Begin
   If Assigned(vTcpRequest) Then
    Begin
     If vTcpRequest.Connected Then
      Begin
       aPackList.Add(vConnection, '', tdmServerCommand, tdcAsync, cDisconnectAllPeers);
       ProcessMessages;
      End
     Else
      Raise Exception.Create(cCantExecDisconnected);
    End;
  End;
End;

Procedure TAegysClient.DisconnectPeer(aID,
                                      aPass,
                                      aConnection : String);
Begin
 If Assigned(vTcpRequest) Then
  Begin
   If vTcpRequest.Connected Then
    Begin
     aPackList.Add(vConnection, '', tdmServerCommand, tdcAsync, Format(cDisconnectPeer + '%s&%s&%s', [aID, aPass, aConnection]));
     ProcessMessages;
    End
   Else
    Raise Exception.Create(cCantExecDisconnected);
  End;
End;

Function TAegysClient.GetPacketLength: Integer;
Begin
 Result := aPackList.Count;
End;

Procedure TAegysClient.Join(aID, aPass, aVideoQ : String);
Begin
 If Assigned(vTcpRequest) Then
  Begin
   If vTcpRequest.Connected Then
    Begin
     aPackList.Add(vConnection, '', tdmServerCommand, tdcAsync, Format(cRelation + '%s&%s&%s', [vSessionID, aID, aPass, aVideoQ]));
     ProcessMessages;
    End
   Else
    Raise Exception.Create(cCantExecDisconnected);
  End;
End;

Procedure TAegysClient.OnClientCommands(CommandType     : TCommandType;
                                        InternalCommand : TInternalCommand;
                                        Connection,
                                        ID,
                                        Command         : String;
                                        MultiPack       : Boolean;
                                        PackCount       : AeInteger;
                                        aBuf            : TAegysBytes);
Var
 aPackClass : TPackClass;
Begin
 Case CommandType Of
  tctScreenCapture : Begin
                      If Assigned(vOnScreenCapture) Then
                       Begin
                        vOnScreenCapture  (Connection, ID, Command,
                                           MultiPack,
                                           PackCount, aBuf);
                       End;
                     End;
  tctAudio         : Begin
                      If Assigned(vOnAudioCapture) Then
                       Begin
                        vOnAudioCapture   (Connection, ID, Command,
                                           MultiPack,
                                           PackCount, aBuf);
                       End;
                     End;
  tctVideo         : Begin
                      If Assigned(vOnVideoCapture) Then
                       Begin
                        vOnVideoCapture   (Connection, ID, Command,
                                           MultiPack,
                                           PackCount, aBuf);
                       End;
                     End;
  tctFileTransfer  : Begin
                      If Assigned(vOnFileTransfer) Then
                       Begin
                        vOnFileTransfer   (Connection, ID,
                                           Command,
                                           InternalCommand,
                                           MultiPack,
                                           PackCount, aBuf);
                       End;
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
  tctMonitor       : Begin
                      If Assigned(vOnInternalCommand) Then
                       vOnInternalCommand(InternalCommand, Connection, ID, Command);
                     End;
  tctPeerList      : Begin
                      If Assigned(vOnPeerList) Then
                       vOnPeerList(InternalCommand, Command);
                     End;
 End;
End;

Procedure TAegysClient.OnServiceCommands(InternalCommand : TInternalCommand;
                                         Command         : String);
Var
 ArrayOfPointer   : TArrayOfPointer;
 vaConnection,
 vClientID,
 vClientPassword,
 vAlias           : String;
 Procedure SetPortSocket;
 Var
  aPackClass : TPackClass;
  aBuf       : TAegysBytes;
 Begin
  ArrayOfPointer :=  [@vaConnection, @vClientID, @vClientPassword, @vAlias];
  ParseValues(Command, ArrayOfPointer);
  aPackClass            := TPackClass.Create;
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
//   Sleep(cDelayThread div 2);
  End;
 End;
Begin
 Case InternalCommand Of
  ticLogin,
  ticDataStatus       : Begin
                         If Assigned(vOnServerLogin) Then
                          vOnServerLogin(Self);
                         SetPortSocket;
                         aAegysConnectionList.ClearAll;
                        End;
  ticConnectedPeer    : Begin
                         ArrayOfPointer :=  [@vaConnection, @vClientID, @vClientPassword, @vAlias];
                         ParseValues(Command, ArrayOfPointer);
                         aAegysConnectionList.Add(vaConnection, vClientID, vClientPassword, vAlias);
                         If Assigned(vOnPeerConnected) Then
                          vOnPeerConnected   (vaConnection,  vClientID,  vClientPassword,  vAlias);
                        End;
  ticAccessGranted    : Begin
                         ArrayOfPointer :=  [@vaConnection, @vClientID, @vClientPassword, @vAlias];
                         ParseValues(Command, ArrayOfPointer);
                         If Assigned(vOnAccessGranted) Then
                          vOnAccessGranted   (vaConnection, vClientID,  vClientPassword,  vAlias);
                        End;
  ticAccessDenied     : Begin
                         If Assigned(vOnAccessDenied) Then
                          vOnAccessDenied;
                        End;
  ticDisconnectedPeer : Begin
                         ArrayOfPointer :=  [@vaConnection, @vClientID, @vClientPassword, @vAlias];
                         ParseValues(Command, ArrayOfPointer);
                         aAegysConnectionList.Delete(vaConnection, vClientID, vClientPassword);
                         If Assigned(vOnPeerDisconnected) Then
                          vOnPeerDisconnected(vaConnection,  vClientID,  vClientPassword,  vAlias);
                        End;
  ticKick             : Begin
                         ArrayOfPointer :=  [@vaConnection, @vClientID, @vClientPassword, @vAlias];
                         ParseValues(Command, ArrayOfPointer);
                         aAegysConnectionList.Delete(vaConnection, vClientID, vClientPassword);
                         If Assigned(vOnPeerKick) Then
                          vOnPeerKick(vaConnection,  vClientID,  vClientPassword,  vAlias);
                        End;
  ticIncommingConnect : Begin
                         ArrayOfPointer :=  [@vaConnection, @vClientID, @vClientPassword, @vAlias];
                         ParseValues(Command, ArrayOfPointer);
                         If Assigned(vOnIncommingConnect) Then
                          vOnIncommingConnect (vaConnection,  vClientID,  vClientPassword,  vAlias);
                        End;
  ticIDExistsReqPass  : Begin
                         ArrayOfPointer :=  [@vaConnection, @vClientID, @vAlias];
                         ParseValues(Command, ArrayOfPointer);
                         If Assigned(vOnBeginTransaction) Then
                          vOnBeginTransaction(vaConnection,  vClientID,  vAlias);
                        End;
  ticIDNotFound       : Begin
                         ArrayOfPointer :=  [@vaConnection];
                         ParseValues(Command, ArrayOfPointer);
                         If Assigned(vOnBeginTransactionError) Then
                          vOnBeginTransactionError(vaConnection);
                        End;
  ticPeerOn,
  ticPeerOff,
  ticPeerNewList      : Begin
                         If Assigned(vOnPeerList) Then
                          vOnPeerList(InternalCommand, Command);
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

Procedure TAegysClient.OnBeforeExecuteData (Var abPackList : TPackList);
 Procedure ReceiveStreamClient;
 Var
  aBuf          : TAegysBytes;
  aFirstBufSize : AEInteger;
  aBuffSize,
  aPackSize,
  bPackSize     : AEInt64;
  vActiveTcp    : TIdTCPClient;
 Begin
  SetLength(aBuf, 0);
  vActiveTcp := vTcpReceive;
  If Assigned(vActiveTcp) Then
   Begin
    If (vActiveTcp.Connected) Then
     Begin
      Try
       If Not (vActiveTcp.IOHandler.InputBufferIsEmpty) Then
        Begin
         Try
          aBuffSize     := vActiveTcp.IOHandler.InputBuffer.Size;
          vActiveTcp.IOHandler.ReadBytes(TIdBytes(aBuf), SizeOf(aPackSize));
          Move(aBuf[0], aPackSize, SizeOf(aPackSize));
          aFirstBufSize := Length(aBuf);
          bPackSize     := 0;
          While (aPackSize <> Length(aBuf)) And
                (aPackSize >  Length(aBuf)) And
                (aPackSize > 0)             Do
           Begin
            If bPackSize = 0 Then
             bPackSize := aBuffSize - SizeOf(aPackSize)
            Else
             bPackSize := vActiveTcp.IOHandler.InputBuffer.Size;
            If (Length(aBuf) + bPackSize) > aPackSize Then
             bPackSize := aPackSize - Length(aBuf);
            If (aPackSize > Length(aBuf)) Then
             vActiveTcp.IOHandler.ReadBytes(TIdBytes(aBuf), bPackSize)
            Else
             Break;
           End;
          If (aPackSize = Length(aBuf)) And
             (aPackSize > 0)            Then
           abPackList.Add(aBuf);
         Finally
          Processmessages;
          SetLength(aBuf, 0);
         End;
        End
       Else
        vActiveTcp.CheckForGracefulDisconnect(False);
      Except
       On e : Exception Do
        Raise Exception.Create(e.Message);
      End;
     End
    Else
     Raise Exception.Create(cDisconnectedByServer);
   End;
  Processmessages;
  vActiveTcp := vTcpRequest;
  If Assigned(vActiveTcp) Then
   Begin
    If (vActiveTcp.Connected) Then
     Begin
      Try
       vActiveTcp.IOHandler.CheckForDisconnect(False, False);
       If Not (vActiveTcp.Connected) Then
        Raise Exception.Create(cDisconnectedByServer);
      Except
       On e : Exception Do
        Raise Exception.Create(e.Message);
      End;
     End;
   End;
  Processmessages;
 End;
Begin
 If Assigned(abPackList) Then
  ReceiveStreamClient;
End;

Procedure TAegysClient.OnExecuteData       (Var abPackList : TPackList;
                                            abPackNo       : AeInt64);
Var
 aBuffer : TAegysBytes;
Begin
 If abPackList.Count > 0 Then
  Begin
   aBuffer := abPackList.Items[0].ToBytes; //ReadPack(aPackNo);
   Try
    If Length(aBuffer) > 0 Then
     SendBytes(aBuffer);
   Finally
    abPackList.Delete(0);
    ProcessMessages;
   End;
  End;
End;

Procedure TAegysClient.OnCheckDisconnect(aType           : Char);
Begin
 If (aType = 'S') Then
  Begin
   If (MilliSecondsBetween(Now, aOldTime) >= vConnectTimeOut) And
      (vConnectTimeOut > 0) Then
    Begin
     aOldTime := Now;
     If Assigned(vTcpRequest) Then
      Begin
       vTcpRequest.CheckForGracefulDisconnect(False);
       If Not vTcpRequest.Connected Then
        Raise Exception.Create(cSocketDisconnected);
      End
     Else
      Raise Exception.Create(cSocketDisconnected);
    End;
   ProcessMessages;
  End;
// Else
//  Begin
//   If Assigned(vTcpReceive) Then
//    Begin
//     vTcpReceive.CheckForGracefulDisconnect(False);
//     If Not vTcpReceive.Connected Then
//      Raise Exception.Create(cSocketDisconnected);
//    End
//   Else
//    Raise Exception.Create(cSocketDisconnected);
//  End;
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
    aOldTime := Now;
    If aMoreData = '' Then
     aPackList.Add(vConnection, aID, tdmClientCommand, tdcAsync, CommandType, aBuffer, Format('%s&%s',    [vConnection, vSessionID]))
    Else
     aPackList.Add(vConnection, aID, tdmClientCommand, tdcAsync, CommandType, aBuffer, Format('%s&%s&%s', [vConnection, vSessionID, aMoreData]));
   End;
 Processmessages;
End;

Procedure TAegysClient.SendBytes           (aBuffer       : TAegysBytes;
                                            aDestMyConn   : Boolean;
                                            aMoreData     : String       = '';
                                            CommandType   : TCommandType = tctScreenCapture);
Begin
 If Assigned(vTcpRequest) Then
  If vTcpRequest.Connected Then
   Begin
    aOldTime := Now;
    If aMoreData = '' Then
     aPackList.Add( vConnection, '', tdmClientCommand, tdcAsync, CommandType, aBuffer, Format('%s&%s',    [vConnection, vSessionID]),            aDestMyConn)
    Else
     aPackList.Add( vConnection, '', tdmClientCommand, tdcAsync, CommandType, aBuffer, Format('%s&%s&%s', [vConnection, vSessionID, aMoreData]), aDestMyConn);
   End;
 Processmessages;
End;

Procedure TAegysClient.SendBytes(aBuffer      : TAegysBytes;
                                 aWriteMode   : TRESTDWWriteMode = twmDirect);
Begin
 If Assigned(vTcpRequest) Then
  If vTcpRequest.Connected Then
   Begin
    aOldTime := Now;
    Try
     Processmessages;
     If aWriteMode = twmBuffer Then
      aPackList.Add(aBuffer)
     Else If aWriteMode = twmDirect Then
      vTcpRequest.IOHandler.WriteDirect(TIdBytes(aBuffer));
    Except
     On e : Exception Do
      Begin
       If Assigned(vOnSendPackError) Then
        vOnSendPackError(aBuffer, e.Message);
      End;
    End;
    Processmessages;
   End;
End;

Procedure TAegysClient.SendCommand         (Value         : String);
Begin
 If Assigned(vTcpRequest) Then
  Begin
   aOldTime := Now;
   If vTcpRequest.Connected Then
    aPackList.Add(vConnection, '', tdmServerCommand, tdcAsync, Value)
   Else
    Raise Exception.Create(cCantExecDisconnected);
  End;
 Processmessages;
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
      aOldTime := Now;
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
 Processmessages;
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
   Begin
    aOldTime := Now;
    aPackList.Add(vConnection, '', tdmClientCommand, tdcAsync, CommandType, Format('%s&%s&%s', [vConnection, vSessionID, Value]), aDestMyConn);
   End;
End;

Procedure TAegysClient.SendMessage         (aID,
                                            Value         : String;
                                            CommandType   : TCommandType = tctChat);
Begin
 If Assigned(vTcpRequest) Then
  If vTcpRequest.Connected Then
   Begin
    aOldTime := Now;
    aPackList.Add(vConnection, aID, tdmClientCommand, tdcAsync, CommandType, Value, False);
    Processmessages;
   End;
End;

Procedure TAegysClient.SendMouse  (aID,
                                   Value       : String);
Begin

End;

Procedure TAegysClient.SendMouse  (aDestMyConn : Boolean;
                                   Value       : String);
Begin

End;

Procedure TAegysClient.SendQuality (aID,
                                   Value       : String);
 Procedure SendQualityData;
 Var
  aPack : TPackClass;
 Begin
  aPack := TPackClass.Create;
  Try
   aPack.Owner       := vConnection;
   aPack.Dest        := aID;
   aPack.DataMode    := tdmClientCommand;
   aPack.DataCheck   := tdcAsync;
   aPack.DataType    := tdtString;
   aPack.CommandType := tctMonitor;
   aPack.Command     := cChangeImageQuality + Value;
  Finally
   aPackList.Add(aPack);
  End;
 End;
Begin
 If Assigned(vTcpRequest) Then
  If vTcpRequest.Connected Then
   Begin
    aOldTime := Now;
    SendQualityData;
   End;
 Processmessages;
End;

Procedure TAegysClient.SendMonitor(aID,
                                   Value       : String);
 Procedure SendMonitorData;
 Var
  aPack : TPackClass;
 Begin
  aPack := TPackClass.Create;
  Try
   aPack.Owner       := vConnection;
   aPack.Dest        := aID;
   aPack.DataMode    := tdmClientCommand;
   aPack.DataCheck   := tdcAsync;
   aPack.DataType    := tdtString;
   aPack.CommandType := tctMonitor;
   aPack.Command     := cChangeMonitor + Value;
  Finally
   aPackList.Add(aPack);
  End;
 End;
Begin
 If Assigned(vTcpRequest) Then
  If vTcpRequest.Connected Then
   Begin
    aOldTime := Now;
    SendMonitorData;
   End;
 Processmessages;
End;

Procedure TAegysClient.SendMonitor(aDestMyConn : Boolean;
                                   Value       : String);
Begin
 SendMonitor('', Value);
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
  Processmessages;
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
    Processmessages;
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
   Processmessages;
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
     {$IFDEF FPC}
     vTcpRequest.OnDisconnected := @OnRequestDisconnect;
     vTcpReceive.OnDisconnected := @OnRequestDisconnect;
     {$ELSE}
     vTcpRequest.OnDisconnected := OnRequestDisconnect;
     vTcpReceive.OnDisconnected := OnRequestDisconnect;
     {$ENDIF}
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
        aOldTime             := Now;
        vWelcomeString       := vWelcomeMessage;
        Processmessages;
        {$IFDEF FPC}
        vProcessData         := TAegysThread.Create(aPackList,
                                                    TComponent(Pointer(@Self)^),
                                                    cDelayThread div 2,
                                                    @OnBeforeExecuteData,
                                                    OnReceiveBytes,
                                                    @OnExecuteData,
                                                    @OnAbortData,
                                                    @OnServiceCommands,
                                                    @OnClientCommands,
                                                    @OnThreadRequestError,
                                                    @OnCheckDisconnect,
                                                    @vOnDisconnect);
        {$ELSE}
        vProcessData         := TAegysThread.Create(aPackList,
                                                    TComponent(Pointer(@Self)^),
                                                    cDelayThread,
                                                    OnBeforeExecuteData,
                                                    OnReceiveBytes,
                                                    OnExecuteData,
                                                    OnAbortData,
                                                    OnServiceCommands,
                                                    OnClientCommands,
                                                    OnThreadRequestError,
                                                    OnCheckDisconnect,
                                                    OnDisconnect);
        {$ENDIF}
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
  Begin
   If vTcpRequest.Connected Then
    vTcpRequest.Disconnect;
   If vTcpReceive.Connected Then
    vTcpReceive.Disconnect;
  End;
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

{ TAegysConnectionList }

Function TAegysConnectionList.Add(aConnStr,
                                  aId, aPWD,
                                  aAlias        : String)  : Integer;
Var
 vItem : PAegysConnections;
 Item  : TAegysConnections;
Begin
 New(vItem);
 Item.Connection          := aConnStr;
 Item.ClientID            := aId;
 Item.ClientPassword      := aPWD;
 Item.Alias               := aAlias;
 Item.StartConnectionTime := Now;
 vItem^                   := Item;
 Result                   := Inherited Add(vItem);
End;

Function TAegysConnectionList.Add(Item : TAegysConnections) : Integer;
Var
 vItem: PAegysConnections;
Begin
 New(vItem);
 vItem^        := Item;
 Result        := Inherited Add(vItem);
End;

Procedure TAegysConnectionList.ClearAll;
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

Constructor TAegysConnectionList.Create;
Begin
 Inherited;
End;

procedure TAegysConnectionList.Delete(aConnStr, Id, PWD: String);
Var
 I : Integer;
Begin
 For I := Count -1 Downto 0 Do
  Begin
   If (Items[I].Connection      = aConnStr)  Or
      ((Items[I].ClientID       = Id)        And
       (Items[I].ClientPassword = PWD))      Then
    Begin
     Delete(I);
     Break;
    End;
  End;
End;

procedure TAegysConnectionList.Delete(Index: Integer);
begin
 If (Index > -1)        And
    (Index <= Count -1) Then
  Begin
    Try
     PAegysConnections(TList(Self).Items[Index])^.Connection     := '';
     PAegysConnections(TList(Self).Items[Index])^.ClientID       := '';
     PAegysConnections(TList(Self).Items[Index])^.ClientPassword := '';
     PAegysConnections(TList(Self).Items[Index])^.Alias          := '';
     {$IFDEF FPC}
      Dispose(PAegysConnections(TList(Self).Items[Index]));
     {$ELSE}
      Dispose(TList(Self).Items[Index]);
     {$ENDIF}
    Except
    End;
    TList(Self).Delete(Index);
  End;
End;

Destructor TAegysConnectionList.Destroy;
Begin
 ClearAll;
 Inherited;
End;

Function TAegysConnectionList.GetConnection(aConnStr,
                                            Id,
                                            PWD        : String) : TAegysConnections;
Var
 I : Integer;
Begin
 Result.Connection     := '';
 Result.ClientID       := Result.Connection;
 Result.ClientPassword := Result.ClientID;
 Result.Alias          := Result.ClientPassword;
 Result.Download       := 0;
 Result.Upload         := 0;
 If Not Assigned(Self) Then
  Exit;
 For I := Count -1 DownTo 0 Do
  Begin
   If (Items[I].Connection      = aConnStr) Or
      ((Items[I].ClientID       = Id)       And
       (Items[I].ClientPassword = PWD))     Then
    Begin
     Result := Items[I];
     Break;
    End;
  End;
End;

Function TAegysConnectionList.GetRec(Index: Integer) : TAegysConnections;
Begin
 Result.Connection     := '';
 Result.ClientID       := Result.Connection;
 Result.ClientPassword := Result.ClientID;
 Result.Alias          := Result.ClientPassword;
 Result.Download       := 0;
 Result.Upload         := 0;
 If (Index < Self.Count) And (Index > -1) Then
  Result := TAegysConnections(TList(Self).Items[Index]^);
End;

Procedure TAegysConnectionList.PutRec(Index : Integer;
                                      Item  : TAegysConnections);
Begin
 If (Index < Self.Count) And (Index > -1) Then
  TAegysConnections(TList(Self).Items[Index]^) := Item;
End;

{ TAegysMyConnection }

Constructor TAegysMyConnection.Create;
Begin
 vConnectionActive   := False;
End;

Destructor TAegysMyConnection.Destroy;
Begin
 vConnectionLastShot := '';
 Inherited;
End;

Procedure TAegysMyConnection.FromString(aValue: String);
Begin

End;

Function TAegysMyConnection.ToString : String;
Begin
 Result := EncodeStrings(Format('%d|%d|%s|%s|%s|%s|%s|%s|%s', [Integer(vConnectionActive),
                                                               Integer(vRemoteAssist),
                                                               vConnectionString,
                                                               vConnectionID,
                                                               vConnectionPass,
                                                               vConnectionName,
                                                               vConnectionAlias,
                                                               vConnectionGroup,
                                                               vConnectionLastShot]));
End;

{ TAegysMyConnectionList }

Procedure TAegysMyConnectionList.FromString(aValue      : String;
                                            CommandType : TAegysListCommandType = tlct_NewList);
Var
 aDataValue,
 vDataV,
 vData       : String;
 vConnectionActive,
 vRemoteAssist       : Boolean;
 vConnectionString,
 vConnectionID       : String;
 Item                : TAegysMyConnection;
Begin
 vDataV := aValue;
 If vDataV <> '' Then
  Begin
   If CommandType = tlct_NewList Then
    ClearAll;
   Repeat
    If Pos('<|>', vDataV) > 0 Then
     Begin
      vData := DecodeStrings(Copy(vDataV, 1, Pos('<|>', vDataV) -1));
      System.Delete(vDataV, 1, Pos('<|>', vDataV) + 2);
     End
    Else
     Begin
      vData  := DecodeStrings(vDataV);
      vDataV := '';
     End;
    If CommandType = tlct_NewList Then
     Begin
      Item                   := TAegysMyConnection.Create;
      aDataValue             := Copy(vData, 1, Pos('|', vData) -1);
      System.Delete(vData, 1, Pos('|', vData));
      Item.vConnectionActive := aDataValue = '1';
      aDataValue             := Copy(vData, 1, Pos('|', vData) -1);
      System.Delete(vData, 1, Pos('|', vData));
      Item.vRemoteAssist     := aDataValue = '1';
      aDataValue             := Copy(vData, 1, Pos('|', vData) -1);
      System.Delete(vData, 1, Pos('|', vData));
      Item.vConnectionString := aDataValue;
      aDataValue             := Copy(vData, 1, Pos('|', vData) -1);
      System.Delete(vData, 1, Pos('|', vData));
      Item.vConnectionID     := aDataValue;
      aDataValue             := Copy(vData, 1, Pos('|', vData) -1);
      System.Delete(vData, 1, Pos('|', vData));
      Item.vConnectionPass   := aDataValue;
      aDataValue             := Copy(vData, 1, Pos('|', vData) -1);
      System.Delete(vData, 1, Pos('|', vData));
      Item.vConnectionName   := aDataValue;
      aDataValue             := Copy(vData, 1, Pos('|', vData) -1);
      System.Delete(vData, 1, Pos('|', vData));
      Item.vConnectionAlias  := aDataValue;
      aDataValue             := Copy(vData, 1, Pos('|', vData) -1);
      System.Delete(vData, 1, Pos('|', vData));
      Item.vConnectionGroup  := aDataValue;
      If Pos('|', vData) > 0 Then
       Begin
        aDataValue                := Copy(vData, 1, Pos('|', vData) -1);
        System.Delete(vData, 1, Pos('|', vData));
        Item.vConnectionLastShot  := aDataValue;
       End
      Else
       Begin
        aDataValue                := vData;
        vData                     := '';
        Item.vConnectionLastShot  := aDataValue;
       End;
      Add(Item);
     End
    Else
     Begin
      aDataValue             := Copy(vData, 1, Pos('|', vData) -1);
      System.Delete(vData, 1, Pos('|', vData));
      vConnectionString := aDataValue;
      If Pos('|', vData) > 0 Then
       Begin
        aDataValue             := Copy(vData, 1, Pos('|', vData) -1);
        System.Delete(vData, 1, Pos('|', vData));
       End
      Else
       Begin
        aDataValue := vData;
        vData := '';
       End;
      vConnectionID     := aDataValue;
      Item              := GetConnection(vConnectionString, vConnectionID);
      If Item <> Nil Then
       Item.ConnectionActive := CommandType = tlct_PeerOn;
     End;
   Until vDataV = '';
  End;
End;

Function TAegysMyConnectionList.Add(Item : TAegysMyConnection) : Integer;
Var
 vItem: PAegysMyConnection;
Begin
 New(vItem);
 vItem^        := Item;
 Result        := Inherited Add(vItem);
End;

Procedure TAegysMyConnectionList.ClearAll;
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

Constructor TAegysMyConnectionList.Create;
Begin
 Inherited;
End;

Procedure TAegysMyConnectionList.Delete(aConnStr, Id : String);
Var
 I : Integer;
Begin
 For I := Count -1 Downto 0 Do
  Begin
   If (Items[I].vConnectionString = aConnStr) Or
      (Items[I].vConnectionID     = Id)       Then
    Begin
     Delete(I);
     Break;
    End;
  End;
End;

Procedure TAegysMyConnectionList.Delete(Index : Integer);
Begin
 If (Index > -1)        And
    (Index <= Count -1) Then
  Begin
   Try
    If Assigned(PAegysMyConnection(TList(Self).Items[Index])^) Then
     PAegysMyConnection(TList(Self).Items[Index])^.Free;
    {$IFDEF FPC}
     Dispose(PAegysMyConnection(TList(Self).Items[Index]));
    {$ELSE}
     Dispose(TList(Self).Items[Index]);
    {$ENDIF}
   Except
   End;
   TList(Self).Delete(Index);
  End;
End;

Destructor TAegysMyConnectionList.Destroy;
Begin
 ClearAll;
 Inherited;
End;

Function TAegysMyConnectionList.GetConnection(aConnStr, Id : String) : TAegysMyConnection;
Var
 I : Integer;
Begin
 Result := Nil;
 If Not Assigned(Self) Then
  Exit;
 For I := Count -1 DownTo 0 Do
  Begin
   If ((Items[I].vConnectionString <> '') And
       (Items[I].vConnectionString = aConnStr)) Or
      ((Items[I].vConnectionID    = Id))    Then
    Begin
     If (Items[I].vConnectionString = '')  And
        (aConnStr                   <> '') Then
      Items[I].vConnectionString := aConnStr;
     Result := Items[I];
     Break;
    End;
  End;
End;

Function TAegysMyConnectionList.GetRec (Index : Integer) : TAegysMyConnection;
Begin
 Result := Nil;
 If (Index < Self.Count) And (Index > -1) Then
  Result := TAegysMyConnection(TList(Self).Items[Index]^);
End;

Procedure TAegysMyConnectionList.PutRec(Index : Integer;
                                        Item  : TAegysMyConnection);
Begin
 If (Index < Self.Count) And (Index > -1) Then
  TAegysMyConnection(TList(Self).Items[Index]^) := Item;
End;

Function TAegysMyConnectionList.ToString : String;
Var
 I : Integer;
Begin
 Result := '';
 For I := 0 to Count -1 Do
  Begin
   If I = 0 Then
    Result := Items[I].ToString
   Else
    Result := Result + '<|>' + Items[I].ToString;
  End;
End;

End.
