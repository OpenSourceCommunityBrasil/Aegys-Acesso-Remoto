unit uIpPoolerService;

interface

uses
//ipwCore, ipwIPDaemon, ipwNetCode, ipwIPPort,
  SysUtils, Classes, Graphics, Controls, Forms, IdTCPConnection,
  Vcl.Dialogs, SimpleTimer, IdBuffer, IdComponent, IdCustomTCPServer, IdTCPServer, IdStackConsts, IdIPWatch, IdHTTP,
  IdContext, IdServerIOHandlerSocket, IdUDPBase, IdUDPServer, IdGlobal, IdSocketHandle, IdTCPClient, Code64,
  uDWJSONInterface, Vcl.ExtCtrls, IdHashMessageDigest,
  {$IF CompilerVersion >= 24} //XE3 or higher
  Winapi.Windows, System.Generics.Collections, System.SyncObjs, System.ZLib, System.DateUtils;
  {$ELSE}
  Windows, DateUtils;
  {$IFEND}

Const
 vEOB             = '<$EOLB$>';
 ConnectString    = '<$CONNECTFVSYSTEM$>';
 DisconnectString = '<$DISCONNECTFVSYSTEM$>';
 CheckPortString  = '<$CHECKPORTDISCONNECTFVSYSTEM$>';
 mtFUDPDirReq 	  = 8000;
 mtFUDPDirACK 	  = 9000;
 mtFUDPReq 		    = 8001;
 mtFUDPACK 		    = 9001;
 mtFUDPDataReq 	  = 8002;
 mtFUDPDataACK 	  = 9002;
 udpBuffer        = 512;

Type
 TIpPort = Record
  IP   : String;
  Port : Integer;
End;

//Eventos
Type
 //Eventos Anonimos
 TExecuteFunction = Reference To Function (ConnectionId : Integer;
                                           Wait : Boolean = True;
                                           InitBuffer   : String = '') : String;
 TExecuteProc     = Reference to Procedure;
 //Do Cliente
 TDataInClient            = Procedure Of Object;
 TErrorClient             = Procedure(ErrorCode             : Integer;
                                      Const Description     : String)  Of Object;
 TConnectedClient         = Procedure(StatusCode            : Integer;
                                      Const Description     : String)  Of Object;
 TConnectionStatusClient  = Procedure(Const ConnectionEvent : String;
                                      StatusCode            : Integer;
                                      Const Description     : String)  Of Object;
 TDisconnectedClient      = Procedure(StatusCode            : Integer;
                                      Const Description     : String)  Of Object;
 TReadyToSendClient       = Procedure Of Object;
 //Do Servidor
 TDataInServer            = Procedure(ConnectionId          : Integer) Of Object;
 TErrorServer             = Procedure(ConnectionId,
                                      ErrorCode             : Integer;
                                      Const Description     : String)  Of Object;
 TConnectedServer         = Procedure(ConnectionId,
                                      StatusCode            : Integer;
                                      Const Description     : String)  Of Object;
 TConnectionRequestServer = Procedure (Const Address        : String;
                                       Port                 : Integer;
                                       Var Accept           : Boolean) Of Object;
 TDisconnectedServer      = Procedure(ConnectionId,
                                      StatusCode            : Integer;
                                      Const Description     : String)  Of Object;
 TReadyToSendServer       = Procedure(ConnectionId          : Integer) Of Object;
 TSocketUDPRead           = Procedure (AThread: TIdUDPListenerThread;
                                       Const AData: TIdBytes;
                                       ABinding: TIdSocketHandle) Of Object;
 TTCPServerEXECUTE        = Procedure (AContext : TIdContext)     Of Object;
 TTCPReadEvent            = Procedure (aParent : TObject; Data   : String) Of Object;

Type
 TASInfo = Packed Record
  ip,
  hostname,
  city,
  region,
  country,
  loc,
  org       : String;
End;

Type
 TByteArr    = Array [1..udpBuffer] Of Char;
 TStringArr  = Array of String;
 TValidation = Array of Byte;

Type
 rFUDPDataACK = Packed Record
  PackID      : String[80];
  PackCommand,
  PackParts,
  PackCount,
  TotalSize,
  BufferSize  : WORD;
  PieceBuf    : TByteArr;
End;

Type
 TBufferData = Class
 Private
  PackID      : String[80];
  PackParts,
  TotalSize   : WORD;
  Validation  : TValidation;
  Buffer      : TStringStream;
  Function GetResult : String;
 Public
  Constructor Create(DataPack    : rFUDPDataACK);
  Destructor  Destroy; Override;
  Procedure   AddBuffer(DataPack : rFUDPDataACK);
  Property    Result : String Read GetResult;
End;

Type
 TBufferDownload = Class
 Private
  vBuffer : TThreadList<TBufferData>;
  Function BufferExists(PackID : String; Var BufferID : Integer) : Boolean;
 Public
  Function    ResultBuffer(PackID : String) : String;
  Procedure   AddBuffer(DataPack : rFUDPDataACK);
  Procedure   DeleteBuffer(PackID : String);
  Constructor Create;
  Destructor  Destroy; Override;
End;

Type
 TBroadcastPeer  = Packed Record
  ip,
  localIp : String;
  Port    : Integer;
End;

Type
 TBroadcastPeers = TThreadList<TBroadcastPeer>;
 TObjectP        = ^TComponent;

Type
 TListenThread = Class(TThread)
 Private
  FTCPReadEvent    : TTCPReadEvent;
  RecvData         : String;
  vSender          : TObject;
  vTCPClient       : TIdTCPClient;
  FTerminateEvent  : TEvent;
  vCriticalSection : TCriticalSection;
 Protected
  Procedure Execute; Override;
  Procedure DoOnTCPRead;
 Public
  Constructor Create;
  Destructor  Destroy; Override;
  Property OnTCPRead : TTCPReadEvent Read FTCPReadEvent Write FTCPReadEvent;
  Property TCPObject : TIdTCPClient  Read vTCPClient    Write vTCPClient;
  Property aSender   : TObject       Read vSender       Write vSender;
End;

Type
 TClientRect = Class
  PeerIP,
  LocalIP,
  WelcomeMessage : String;
  UDPPort,
  Handle         : Integer;
  LastAction     : TDatetime;
  Context        : TIdContext;
End;

Type
 PClientDataBuffer = ^TClientDataBuffer;
 TClientDataBuffer = Packed Record
  ConnectionId,
  DataLength   : Integer;
  Data         : String;
  InternalTick : TDateTime;
  Readed,
  EOF          : Boolean;
End;

Type
 TDataBuffer = TThreadList;

Type
 ipClient = Interface
  Procedure Error(Sender                           : TObject;
                  ErrorCode                        : Integer;
                  Const Description                : String); Overload;
  procedure Error(Sender                           : TObject;
                  ConnectionId,
                  ErrorCode                        : Integer;
                  Const Description                : String)  Overload;
  Procedure Connected(Sender                       : TObject;
                      ConnectionId,
                      StatusCode                   : Integer;
                      Const Description            : String); Overload;
  Procedure Connected(Sender                       : TObject;
                      StatusCode                   : Integer;
                      Const Description            : String); Overload;
  Procedure ConnectionRequest(Sender               : TObject;
                              Const Address        : String;
                              Port                 : Integer;
                              Var Accept           : Boolean);
  Procedure ConnectionStatus(Sender                : TObject;
                             Const ConnectionEvent : String;
                             StatusCode            : Integer;
                             Const Description     : String);
  procedure DataInServer(Sender                    : TObject;
                         ConnectionId              : Integer;
                         Text                      : String;
                         EOL                       : Boolean);Overload;
  Procedure DataInClient(Sender                    : TObject;
                         Text                      : String;
                         EOL                       : Boolean);Overload;
  Procedure Disconnected(Sender                    : TObject;
                         ConnectionId,
                         StatusCode                : Integer;
                         Const Description         : String); Overload;
  Procedure Disconnected(Sender                    : TObject;
                         StatusCode                : Integer;
                         Const Description         : String); Overload;
  Procedure ReadyToSendServer(Sender               : TObject;
                              ConnectionId         : Integer);Overload;
  Procedure ReadyToSendClient(Sender               : TObject);Overload;
  Procedure UDPRead          (AThread: TIdUDPListenerThread;
                              Const AData: TIdBytes;
                              ABinding: TIdSocketHandle);
  Procedure Execute          (AContext : TIdContext);
End;

Type
 TipPoolerObject = Class(TObject)//Class(TInterfacedObject, ipClient)
 Private
  aParent          : TObject;
  vUdpListBindings : TDataBuffer;
  Procedure ExecMethod(Execute : TExecuteProc = Nil);
  Procedure Error(Sender                           : TObject;
                  ErrorCode                        : Integer;
                  Const Description                : String); Overload;
  Procedure Error(Sender                           : TObject;
                  ConnectionId,
                  ErrorCode                        : Integer;
                  Const Description                : String)  Overload;
  Procedure Connected(Sender                       : TObject;
                      ConnectionId,
                      StatusCode                   : Integer;
                      Const Description            : String); Overload;
  Procedure Connected(Sender                       : TObject;
                      StatusCode                   : Integer;
                      Const Description            : String); Overload;
  Procedure ConnectionRequest(Sender               : TObject;
                              Const Address        : String;
                              Port                 : Integer;
                              Var Accept           : Boolean);
  Procedure ConnectionStatus(Sender                : TObject;
                             Const ConnectionEvent : String;
                             StatusCode            : Integer;
                             Const Description     : String);
  Procedure AddBuffer(ConnectionId  : Integer;
                      Text          : String;
                      DataEncrypted : Boolean = False);
  procedure DataInServer(Sender                    : TObject;
                         ConnectionId              : Integer;
                         Text                      : String;
                         EOL                       : Boolean);Overload;
  Procedure DataInClient(Sender                    : TObject;
                         Text                      : String;
                         EOL                       : Boolean);Overload;
  Procedure Disconnected(Sender                    : TObject;
                         ConnectionId,
                         StatusCode                : Integer;
                         Const Description         : String); Overload;
  Procedure Disconnected(Sender                    : TObject;
                         StatusCode                : Integer;
                         Const Description         : String); Overload;
  Procedure ReadyToSendServer(Sender               : TObject;
                              ConnectionId         : Integer);Overload;
  Procedure ReadyToSendClient(Sender               : TObject);Overload;
  Function  UDPConnectString   (Value : String; ABinding: TIdSocketHandle) : Boolean;
  Function  UDPDisconnectString(Value : String; ABinding: TIdSocketHandle) : Boolean;
  Procedure UDPRead          (AThread: TIdUDPListenerThread;
                              Const AData: TIdBytes;
                              ABinding: TIdSocketHandle);
  Procedure Execute          (AContext : TIdContext);
  Procedure OnClientStatus(ASender: TObject;
                           Const AStatus: TIdStatus;
                           Const AStatusText: string);
  procedure TCPServerConnect(AContext: TIdContext);
  Procedure TCPServerDisconnect(AContext: TIdContext);
  Function  UDPCheckPortString(Value : String; ABinding: TIdSocketHandle) : Boolean;
  Function  UDPWelcomeMessage(ConnectionId : Integer) : String;
  Procedure UDPUpdateLastAction(ABinding: TIdSocketHandle);
  Procedure TCPRead(Sender : TObject; Data : String);
 Public
  Constructor Create(Sender                        : TObject);
  Destructor  Destroy;Override;
End;

PCliente = ^TipPoolerObject;
Type
 TListClients = TList<PCliente>;

Type
 TipTCPObject = TComponent;

Type
 PipPoolerService = ^TipPoolerService;
 TipPoolerService = Class(TComponent)
 Private
  aSelf                    : TipPoolerService;
  Owner                    : TComponent;        //Proprietario do Componente
  vCommandsReleaseCache    : Boolean;
  vCommandsReleaseCount,
  vConnections             : Integer;
  vDataBuffer              : TDataBuffer;
  vipTCPObject             : TipTCPObject;
  //Eventos do Client
  vDataInClient            : TDataInClient;
  vErrorClient             : TErrorClient;
  vConnectedClient         : TConnectedClient;
  vConnectionRequestServer : TConnectionRequestServer;
  vDisconnectedClient      : TDisconnectedClient;
  //Eventos do Servidor
  vDataInServer            : TDataInServer;
  vErrorServer             : TErrorServer;
  vConnectedServer         : TConnectedServer;
  vConnectionStatusClient  : TConnectionStatusClient;
  vDisconnectedServer      : TDisconnectedServer;
  vReadyToSendClient       : TReadyToSendClient;
  vReadyToSendServer       : TReadyToSendServer;
//  vListenThread            : TTimer;
  vAutoGarbage,
  vInGarbage               : Boolean;
  SimpleTimer              : TSimpletimer;
  SimpleEvent              : TLightWeightEvent;
  vBufferLine,
  vGarbageTime,
  vTimeOut                 : LongInt;
  ClientObject             : TipPoolerObject;
  vGetASInfo,
  vCompression,
  vActive,
  vSingleLineMode,
  vEncryptData             : Boolean;
  vEncryptKey              : Word;
  vLocalPort               : Integer;
  vHost,
  vEOC,
  vLineBreak,
  vWelcomeMessage,
  vInternetIP              : String;
  vCriticalSection         : TCriticalSection;
  SimpleTimerGarbage       : TSimpleTimer;
  vBroadcastPeers          : TBroadcastPeers;
  vASInfo                  : TASInfo;
  vListenThread            : TListenThread;
  BufferDownload           : TBufferDownload;
  Procedure Stop(Var ListenThread : TListenThread);
  Procedure SetSingleLineMode(Value : Boolean);
  Procedure SetTCPObject(Value : TipTCPObject);
  Function  GetTCPObject   : TipTCPObject;
  Procedure SetActive(Value : Boolean);
  Procedure StartActions(Const ShutdownEvent : TLightweightEvent;
                         WaitFor             : LongWord;
                         Value               : TExecuteFunction;
                         ConnectionId        : Integer;
                         Var Result          : String);
  Function  CurrentBuffer(ConnectionId       : Integer;
                          Wait               : Boolean = False;
                          InitBuffer         : String = '') : String;
  //Eventos do Client
  Procedure SetDataInClient(Value            : TDataInClient);
  Function  GetDataInClient                  : TDataInClient;
  Procedure SetErrorClient(Value             : TErrorClient);
  Function  GetErrorClient                   : TErrorClient;
  Procedure SetConnectedClient(Value         : TConnectedClient);
  Function  GetConnectedClient               : TConnectedClient;
  Procedure SetConnectionStatusClient(Value  : TConnectionStatusClient);
  Function  GetConnectionStatusClient        : TConnectionStatusClient;
  Procedure SetDisconnectedClient(Value      : TDisconnectedClient);
  Function  GetDisconnectedClient            : TDisconnectedClient;
  Function  GetReadyToSendClient             : TReadyToSendClient;
  Procedure SetReadyToSendClient(Value       : TReadyToSendClient);
  //Eventos do Servidor
  Procedure SetDataInServer(Value            : TDataInServer);
  Function  GetDataInServer                  : TDataInServer;
  Procedure SetErrorServer(Value             : TErrorServer);
  Function  GetErrorServer                   : TErrorServer;
  Procedure SetConnectedServer(Value         : TConnectedServer);
  Function  GetConnectedServer               : TConnectedServer;
  Procedure SetConnectionRequestServer(Value : TConnectionRequestServer);
  Function  GetConnectionRequestServer       : TConnectionRequestServer;
  Procedure SetDisconnectedServer(Value      : TDisconnectedServer);
  Function  GetDisconnectedServer            : TDisconnectedServer;
  Function  GetReadyToSendServer             : TReadyToSendServer;
  Procedure SetReadyToSendServer(Value       : TReadyToSendServer);
  Procedure SetBufferLine(Value : LongInt);
  Procedure SetAutoGarbage(Value : Boolean);
  Procedure SetGarbageTime(Value : LongInt);
  Procedure CaptureData(Sender: TObject);
  Procedure CaptureExecute(Sender: TObject);
  Function  CountContexts(Contexts : TIdContextThreadList) : Integer;
  Function  SendBuffer(Ip : String; Port : Integer;
                       Var UDPServer : TIdUDPServer;
                       Value : String) : Boolean;
  Procedure SetASInfo;
 Public
  Constructor Create(AOwner                   : TComponent);Override; //Cria o Componente
  Procedure   ReleaseCacheAction(ConnectionId : Integer);
  Destructor  Destroy;Override;
  Function    DataToSend(ConnectionId         : Integer;
                         Data                 : String) : Boolean;Overload;
  Function    DataToSend(Data                 : String) : Boolean;Overload;
  Function    Write(ConnectionId              : Integer;
                    Data                      : String;
                    Const IP                  : String = '';
                    Const Port                : Integer = -1) : Boolean;Overload;
  Function    Write(Data                      : String;
                    Const IP                  : String = '';
                    Const Port                : Integer = -1) : Boolean;Overload;
  Function    Write(ConnectionId              : Integer;
                    Data                      : TMemoryStream;
                    Const IP                  : String = '';
                    Const Port                : Integer = -1) : Boolean;Overload;
  Function    Write(Data                      : TMemoryStream;
                    Const IP                  : String = '';
                    Const Port                : Integer = -1) : Boolean;Overload;
  Function    ReadLn(ConnectionId             : Integer) : String;Overload;
  Function    ReadLn                          : String;Overload;
  Procedure   Read(ConnectionId  : Integer;
                   Var Data      : TMemoryStream);Overload;
  Procedure   Read(Var Data      : TMemoryStream);Overload;
  Function    ReadAll               : String;     Overload;
  Function    ReadAll(ConnectionId  : Integer = -1) : String; Overload;
  Function    CheckConnect    (ConnectionId  : Integer) : Boolean;
  Function    ClientDisconnect(ConnectionId  : Integer) : Boolean;
  Function    HasBuffer(ConnectionId         : Integer = -1) : Integer;
  Function    ReadBuffer(ConnectionId : Integer = -1;
                         BreakPoint   : Boolean = True) : String;Overload;
  Function    ReadBuffer(BreakPoint   : Boolean = True) : String;Overload;
  Procedure   GarbageCollector(ConnectionId : Integer = -2);
  Function    ContextsByHandle(Contexts : TIdContextThreadList;
                               ConnectionId : Integer) : TIdContext;
  Function    HandleExists(Contexts : TIdContextThreadList;
                           ConnectionId : Integer) : Boolean;
  Function    GetIP(ConnectionId : Integer) : TIpPort;
  Function    UDPWelcomeMessage(ConnectionId : Integer) : String;
  Function    UDPClient(ConnectionId : Integer; Var Found : Boolean) : TClientRect;
  Procedure   ClearPeers;
  Procedure   AddPeer(Value : TBroadcastPeer);
  Procedure   DeletePeer(Index : Integer);
  Procedure   SetInternetIP(Value : String);
 Published
  //Propriedades
  Property TCPObject                 : TipTCPObject             Read GetTCPObject               Write SetTCPObject;
  Property TimeOut                   : LongInt                  Read vTimeOut                   Write vTimeOut;
  Property BufferSize                : LongInt                  Read vBufferLine                Write SetBufferLine;
  Property AutoGarbage               : Boolean                  Read vAutoGarbage               Write SetAutoGarbage;
  Property GarbageTime               : LongInt                  Read vGarbageTime               Write SetGarbageTime;
  Property EncryptData               : Boolean                  Read vEncryptData               Write vEncryptData;
  Property EncryptKey                : Word                     Read vEncryptKey                Write vEncryptKey;
  Property Active                    : Boolean                  Read vActive                    Write SetActive;
  Property Compression               : Boolean                  Read vCompression               Write vCompression;
  Property Port                      : Integer                  Read vLocalPort                 Write vLocalPort;
  Property Host                      : String                   Read vHost                      Write vHost;
  Property LineBreak                 : String                   Read vLineBreak                 Write vLineBreak;
  Property CommandBreak              : String                   Read vEOC                       Write vEOC;
  Property SyncMode                  : Boolean                  Read vSingleLineMode            Write SetSingleLineMode;
  Property ActiveConnections         : Integer                  Read vConnections;
  Property ReleaseCache              : Boolean                  Read vCommandsReleaseCache      Write vCommandsReleaseCache;
  Property ReleaseCount              : Integer                  Read vCommandsReleaseCount      Write vCommandsReleaseCount;
  Property WelcomeMessage            :  String                  Read vWelcomeMessage            Write vWelcomeMessage;
  Property BroadcastPeers            : TBroadcastPeers          Read vBroadcastPeers;
  Property ASInfo                    : TASInfo                  Read vASInfo;
  Property InternetIP                : String                   Read vInternetIP                Write SetInternetIP;
  Property GetASInfo                 : Boolean                  Read vGetASInfo                 Write  vGetASInfo;
  //Eventos do Client
  Property OnDataInClient            : TDataInClient            Read GetDataInClient            Write SetDataInClient;
  Property OnErrorClient             : TErrorClient             Read GetErrorClient             Write SetErrorClient;
  Property OnConnectedClient         : TConnectedClient         Read GetConnectedClient         Write SetConnectedClient;
  Property OnConnectionStatusClient  : TConnectionStatusClient  Read GetConnectionStatusClient  Write SetConnectionStatusClient;
  Property OnDisconnectedClient      : TDisconnectedClient      Read GetDisconnectedClient      Write SetDisconnectedClient;
  Property OnReadyToSendClient       : TReadyToSendClient       Read GetReadyToSendClient       Write SetReadyToSendClient;
  //Eventos do Servidor
  Property OnDataInServer            : TDataInServer            Read GetDataInServer            Write SetDataInServer;
  Property OnErrorServer             : TErrorServer             Read GetErrorServer             Write SetErrorServer;
  Property OnConnectedServer         : TConnectedServer         Read GetConnectedServer         Write SetConnectedServer;
  Property OnConnectionRequestServer : TConnectionRequestServer Read GetConnectionRequestServer Write SetConnectionRequestServer;
  Property OnDisconnectedServer      : TDisconnectedServer      Read GetDisconnectedServer      Write SetDisconnectedServer;
  Property OnReadyToSendServer       : TReadyToSendServer       Read GetReadyToSendServer       Write SetReadyToSendServer;
End;

 Function StrToWide(Const strin : String;
                    Var FUDPReq : rFUDPDataACK) : tidBytes;
 Function WideToStr(const strin : tidBytes;
                    Var BufferDownload : TBufferDownload) : String;
 Function GetLocalIP : String;

implementation

Function IsTrue(Value : Array of Byte) : Boolean;
Var
 I : Integer;
begin
 Result := False;
 For I := 0 to Length(Value) -1 do
  Begin
   Result := Value[I] = 1;
   If Not Result Then
    Break;
  End;
end;

Function GenBufferID : Utf8String;
Var
 StartTime : Cardinal;
 Function GeraChave(Tamanho: integer): String;
 Var
  I: Integer;
  Chave: String;
 Const
  str = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
 Begin
  Chave := '';
  For I := 1 To Tamanho Do
   Chave := Chave + str[random(length(str)) + 1];
  Result := Chave;
 End;
 Function MD5(Const Texto: String) : String;
 Var
  idmd5: TIdHashMessageDigest5;
 Begin
  idmd5 := TIdHashMessageDigest5.Create;
  Try
   Result := idmd5.HashStringAsHex(texto);
  Finally
   idmd5.Free;
  End;
 End;
Begin
 StartTime := TThread.GetTickCount;
 Result := MD5(GeraChave(64) + DateTimeToStr(Now) + IntToStr(StartTime));
End;

Function GetMyASInfo : TASInfo;
Var
 json   : TDWJSONObject;
 IdHTTP : TIdHTTP;
Begin
 Result.ip       := '0.0.0.0';
 Result.hostname := '';
 Result.city     := 'unknow';
 Result.region   := Result.city;
 Result.country  := Result.region;
 Result.loc      := Result.country;
 Result.org      := Result.loc;
 IdHTTP          := TIdHTTP.Create(Nil);
 Try
  json := TDWJSONObject.Create(IdHTTP.Get('http://ipinfo.io/json'));
  Result.ip       := json.PairByName['ip'].Value;
  Result.hostname := json.PairByName['hostname'].Value;
  Result.city     := json.PairByName['city'].Value;
  Result.region   := json.PairByName['region'].Value;
  Result.country  := json.PairByName['country'].Value;
  Result.loc      := json.PairByName['loc'].Value;
  Result.org      := json.PairByName['org'].Value;
  json.Free;
 Except
 End;
 IdHTTP.Free;
End;

Function ByteToString(const Value: TByteArr; Size : Integer): String;
Begin
 SetLength(Result, SizeOf(TByteArr));
 SetString(Result, PChar(@Value[1]), Size);
End;

Function GetLocalIP : String;
Var
 IPW : TIdIPWatch;
Begin
 Result := '0.0.0.0';
 IpW := TIdIPWatch.Create(nil);
 Try
  If IpW.LocalIP <> '' Then
   Result := IpW.LocalIP;
 Finally
  IpW.Free;
 End;
End;

Function chars_2_word(Const stringa : String) : word;
Begin
 If length(stringa) >= 2 Then
  Begin
   Result := ord(stringa[2]);
   Result := Result shl 8;
   Result := Result + ord(stringa[1]);
  End
 Else
  Result := 0;
end;

Function TBufferDownload.BufferExists(PackID : String; Var BufferID : Integer) : Boolean;
Var
 I      : Integer;
 vClass : TList<TBufferData>;
Begin
 Result   := False;
 BufferID := -1;
 vClass := vBuffer.LockList;
 Try
  For I := vClass.Count -1 Downto 0 Do
   Begin
    Result := vClass[I].PackID = PackID;
    If Result Then
     Begin
      BufferID := I;
      Break;
     End;
   End;
 Finally
  vBuffer.UnlockList;
 End;
End;

Procedure TBufferDownload.DeleteBuffer(PackID : String);
Var
 I      : Integer;
 vClass : TList<TBufferData>;
Begin
 vClass := vBuffer.LockList;
 Try
  For I := vClass.Count -1 Downto 0 Do
   Begin
    If vClass[I].PackID = PackID Then
     Begin
      vClass[I].Free;
      vClass.Delete(I);
      Break;
     End;
   End;
 Finally
  vBuffer.UnlockList;
 End;
End;

Function  TBufferDownload.ResultBuffer(PackID : String) : String;
Var
 I      : Integer;
 vClass : TList<TBufferData>;
Begin
 Result := '';
 vClass := vBuffer.LockList;
 Try
  For I := vClass.Count -1 Downto 0 Do
   Begin
    If vClass[I].PackID = PackID Then
     Begin
      Result := vClass[I].Result;
      Break;
     End;
   End;
 Finally
  vBuffer.UnlockList;
 End;
End;

Procedure TBufferDownload.AddBuffer(DataPack : rFUDPDataACK);
Var
 BufferID   : Integer;
 BufferData : TBufferData;
 vClass : TList<TBufferData>;
Begin
 If BufferExists(DataPack.PackID, BufferID) Then
  Begin
   Try
    vClass := vBuffer.LockList;
    vClass[BufferID].AddBuffer(DataPack);
   Finally
    vBuffer.UnlockList;
   End;
  End
 Else
  Begin
   Try
    vClass := vBuffer.LockList;
    BufferData := TBufferData.Create(DataPack);
    vClass.Add(BufferData);
   Finally
    vBuffer.UnlockList;
   End;
  End;
End;

Constructor TBufferDownload.Create;
Begin
 vBuffer := TThreadList<TBufferData>.Create;
End;

Destructor  TBufferDownload.Destroy;
Var
 I      : Integer;
 vClass : TList<TBufferData>;
Begin
 vClass := vBuffer.LockList;
 Try
  For I := vClass.Count -1 Downto 0 Do
   Begin
    vClass[I].Free;
    vClass.Delete(I);
   End;
 Finally
  vBuffer.UnlockList;
 End;
 FreeAndNil(vBuffer);
 Inherited;
End;

Constructor TBufferData.Create(DataPack : rFUDPDataACK);
Begin
 Buffer := TStringStream.Create;
 AddBuffer(DataPack);
End;

Destructor  TBufferData.Destroy;
Begin
 Buffer.SetSize(0);
 Buffer.Free;
 Inherited;
End;

Function    TBufferData.GetResult : String;
Begin
 Result := '';
 If IsTrue(Validation) Then
  Result := Buffer.DataString;
End;

Procedure   TBufferData.AddBuffer(DataPack : rFUDPDataACK);
Begin
 PackID          := DataPack.PackID;
 PackParts       := DataPack.PackParts;
 TotalSize       := DataPack.TotalSize;
 If PackParts <> Length(Validation) Then
  SetLength(Validation, PackParts);
 Try
  If (PackParts <> Buffer.Size) Then
   Buffer.SetSize(TotalSize);
  Buffer.Position := (DataPack.PackCount -1) * udpBuffer;
  Buffer.WriteString(ByteToString(DataPack.PieceBuf, DataPack.BufferSize));
  Validation[(DataPack.PackCount -1)] := 1;
 Except
 End;
End;

Function ByteArrayToStr(Var bArray : TByteArray) : String;
Begin
 SetLength(Result, SizeOf(bArray));
 CopyMemory(@Result[1], @bArray[0], SizeOf(bArray));
End;

Function StrToByteArray(Buffer : AnsiString) : TByteArray;
Begin
 SetLength(Result, lstrlen(PChar(Buffer)));
 CopyMemory(@Result[0], @Buffer[1], Length(Buffer));
End;

Function WideToStr(const strin : tidBytes;
                   Var BufferDownload : TBufferDownload) : String;
Var
 FUDPReq : rFUDPDataACK;
Begin
 BytesToRaw(strin, FUDPReq, SizeOf(rFUDPDataACK));
 If FUDPReq.PackParts > 1 Then
  Begin
   BufferDownload.AddBuffer(FUDPReq);
   Result := BufferDownload.ResultBuffer(FUDPReq.PackID);
   If Result <> '' Then
    BufferDownload.DeleteBuffer(FUDPReq.PackID);
  End
 Else
  Result := ByteToString(FUDPReq.PieceBuf, FUDPReq.BufferSize);
end;

Function StrToWide(Const strin : String; Var FUDPReq : rFUDPDataACK) : tidBytes;
 Procedure StrToByte(Const Value: String;Var Dest : TByteArr);
 Var
  I : Integer;
 Begin
  FillChar(Dest[1], length(Dest) * sizeof(char), #0);
  For I := 1 to Length(Value) do
   Dest[I] := Value[I];
 End;
Begin
 StrToByte(strin, FUDPReq.PieceBuf);
 FUDPReq.BufferSize := Length(strin);
 Result             := RawToBytes(FUDPReq, SizeOf(rFUDPDataACK));
end;

Procedure TrimAppMemorySize;
Var
 MainHandle : THandle;
begin
 Try
  MainHandle := OpenProcess(PROCESS_ALL_ACCESS, false, GetCurrentProcessID);
  SetProcessWorkingSetSize(MainHandle, High(SIZE_T), High(SIZE_T)); //$FFFFFFFF, $FFFFFFFF);
  CloseHandle(MainHandle);
 Except
 End;
End;

Function EncryptStr(const S: String; Key: Word): String;
Var
 I: Integer;
Const
 C1 = 53761;
 C2 = 32618;
Begin
 Result := S;
 For I := 1 To Length(S) Do
  Begin
   Result[I] := char(byte(S[I]) xor (Key shr 8));
   Key := (byte(Result[I]) + Key) * C1 + C2;
  End;
End;

Function DecryptStr(const S: String; Key: Word): String;
Var
 I : Integer;
Begin
 Result := '';
 For I := 1 to Length(S) Div 2 Do
  Result:= Result+Char(StrToInt('$'+Copy(S,(I-1)*2+1,2)));
End;

Function StringToHex(S : String) : String;
Begin
 SetLength(Result, Length(S) * 4);
 BinToHex(S[1], PWideChar(Result), Length(S) * SizeOf(Char));
End;

Function HexToString(H : String) : String;
Begin
 SetLength(Result, Length(H) div 4);
 HexToBin(PWideChar(H), Result[1], Length(H) div SizeOf(Char));
End;

Function DeCompressStream(Const SrcStream : TMemoryStream;
                          Var   DestStream : TMemoryStream): Boolean;
Var
 zipFile : TZDecompressionStream;
Begin
 Result := False;
 SrcStream.Position := 0;
 zipFile := TZDecompressionStream.Create(SrcStream);
 Try
  DestStream.Clear;
  DestStream.CopyFrom(zipFile, 0);
  Result := True;
 Except
 End;
 FreeAndNil(zipFile);
 DestStream.Seek(0, soFromBeginning);
End;

Function CompressStream(Const SrcStream : TMemoryStream;
                        DestStream : TMemoryStream): Boolean;
Var
 zipFile : TZCompressionStream;
Begin
 Result := false;
 zipFile := TZCompressionStream.Create(DestStream, zcMax, 15);
 Try
  SrcStream.Position := 0;
  zipFile.CopyFrom(SrcStream, SrcStream.Size);
  Result := True;
 Except
 End;
 FreeAndNil(zipFile);
End;

Function MemoryStreamToString(M : TMemoryStream) : String;
Var
 _MemStr : TStringStream;
Begin
 _MemStr := TStringStream.Create;
 Try
  _MemStr.LoadFromStream(M);
 Finally
  Result := _MemStr.DataString;
  FreeAndNil(_MemStr);
 End;
End;

Procedure StringToMemoryStream(Value : TMemoryStream; Str : String);
Var
 _MemStr : TStringStream;
Begin
 _MemStr := TStringStream.Create(Str);
 Try
  _MemStr.SaveToStream(Value);
 Finally
  FreeAndNil(_MemStr);
 End;
end;

Destructor TListenThread.Destroy;
Begin
 If FTerminateEvent <> Nil Then
  Begin
   RecvData := '';
   FreeAndNil(FTerminateEvent);
  End;
 vCriticalSection.Release;
 vCriticalSection.Leave;
 Inherited;
End;

Procedure TipPoolerService.Stop(Var ListenThread : TListenThread);
Begin
 Try
  vCriticalSection.Enter;
  If ListenThread <> Nil Then
   ListenThread.Terminate;
 Finally
  vCriticalSection.Release;
  vCriticalSection.Leave;
 End;
End;

Constructor TListenThread.Create;
Begin
 Inherited Create(True);
 vCriticalSection := TCriticalSection.Create;
 FTerminateEvent  := TEvent.Create(nil, True, False, 'ListenThreadEvent');
 RecvData         := '';
 Priority         := tpLowest;
// FreeOnTerminate := True;
End;

Procedure TListenThread.Execute;
Var
 vLine : String;
 Procedure ExecMethod(Execute : TExecuteProc = Nil; Sincro : Boolean = False);
 Begin
  TThread.CreateAnonymousThread(Procedure
                                Begin
                                  //Se precisar interagir com a Thread da Interface
                                  TThread.Synchronize(TThread.CurrentThread,
                                                      Procedure
                                                      Begin
                                                       If Assigned(Execute) Then
                                                        Execute;
                                                      End);
                                End).Start;
 End;
Begin
 Try
//  ShutdownEvent := TLightweightEvent.Create;
  While Not (Terminated) Do
   Begin
    Try
     If vTCPClient = Nil Then
      Begin
       FTerminateEvent.WaitFor(10);
       Continue;
      End;
     If Not (vTCPClient.Connected) Then
      Begin
       FTerminateEvent.WaitFor(10);
       Continue;
      End;
     If vTCPClient.IOHandler.InputBufferIsEmpty Then
      Begin
       vTCPClient.IOHandler.CheckForDataOnSource(10);
       vTCPClient.IOHandler.CheckForDisconnect;
       If vTCPClient.IOHandler.InputBufferIsEmpty Then
        Continue;
      End;
     vLine    := vTCPClient.IOHandler.InputBufferAsString;
     RecvData := vLine;
     Synchronize(DoOnTCPRead);
    Except
     Terminate;
     Break;
    End;
   End;
 Finally
 End;
End;

Procedure TListenThread.DoOnTCPRead;
Begin
 Try
  vCriticalSection.Enter;
  If (vTCPClient <> Nil) And
  Not(Terminated)    Then
  If Assigned(FTCPReadEvent) Then
   FTCPReadEvent(vSender, RecvData);
 Finally
  vCriticalSection.Release;
  vCriticalSection.Leave;
 End;
End;

Procedure TipPoolerService.SetActive(Value : Boolean);
Var
 FUDPReq        : rFUDPDataACK;
 Procedure ConnectUDP;
 Begin
  If TIdUDPServer(vipTCPObject).Active Then
   TIdUDPServer(vipTCPObject).Binding.Reset;
//   TIdUDPServer(vipTCPObject).Binding.CloseSocket;
  TIdUDPServer(vipTCPObject).Active      := False;
  Try
   TIdUDPServer(vipTCPObject).BroadcastEnabled := True;
   If (AnsiLowerCase(vHost) <> '127.0.0.1') And
      (AnsiLowerCase(vHost) <> 'localhost') Then
    Begin
     TIdUDPServer(vipTCPObject).Bindings.Clear;
     If (TIdUDPServer(vipTCPObject).DefaultPort = 0) And (vLocalPort > 0) Then
      TIdUDPServer(vipTCPObject).DefaultPort := vLocalPort + 1
     Else
      TIdUDPServer(vipTCPObject).DefaultPort := 0;
    End
   Else
    TIdUDPServer(vipTCPObject).DefaultPort := vLocalPort;
   TIdUDPServer(vipTCPObject).BufferSize  := vBufferLine;
   TIdUDPServer(vipTCPObject).Active      := Value;
  Except
   TIdUDPServer(vipTCPObject).DefaultPort := TIdUDPServer(vipTCPObject).DefaultPort + 1;
   ConnectUDP;
   Exit;
  End;
 End;
Begin
 If (vGetASInfo) And (Value) Then
  SetASInfo;
 If Value Then
  Begin
   If vInternetIP = '' Then
    vInternetIP := vASInfo.ip
   Else
    vASInfo.ip  := vInternetIP;
  End;
  {
 If vipTCPObject is TipwIPPort Then
  Begin
   If Not Value Then
    Begin
     Try
      If TipwIPPort(vipTCPObject).Connected Then
       TipwIPPort(vipTCPObject).Disconnect;
     Except
     End;
     TipwIPPort(vipTCPObject).AcceptData := False;
     TipwIPPort(vipTCPObject).Interrupt;
     TipwIPPort(vipTCPObject).Connected  := TipwIPPort(vipTCPObject).AcceptData;
    End
   Else
    TipwIPPort(vipTCPObject).AcceptData := True;
   Try
    TipwIPPort(vipTCPObject).RemotePort := vLocalPort;
    TipwIPPort(vipTCPObject).RemoteHost := vHost;
    If Not (Value) And (TipwIPPort(vipTCPObject).Connected) Then
     TipwIPPort(vipTCPObject).Connected := Value
    Else If (Value) And Not(TipwIPPort(vipTCPObject).Connected) Then
     TipwIPPort(vipTCPObject).Connected := Value;
   Except
   End;
   Try
    If Value Then
     vActive := TipwIPPort(vipTCPObject).Connected
    Else
     vActive := Value;
   Except
    vActive := False;
   End;
  End
 Else If vipTCPObject is TipwIPDaemon Then
  Begin
   Try
    If Not TipwIPDaemon(vipTCPObject).Listening Then
     TipwIPDaemon(vipTCPObject).LocalPort := vLocalPort;
    If Not (Value) And (TipwIPDaemon(vipTCPObject).Listening) Then
     TipwIPDaemon(vipTCPObject).Shutdown
    Else If (Value) And Not(TipwIPDaemon(vipTCPObject).Listening) Then
     TipwIPDaemon(vipTCPObject).Listening := Value;
   Except
   End;
   Try
    If Value Then
     vActive := TipwIPDaemon(vipTCPObject).Listening
    Else
     vActive := Value;
   Except
    vActive := False;
   End;
  End}
 if (vipTCPObject is TIdUDPServer) Then
  Begin
   Try
    FUDPReq.PackID  := GenBufferID;
    FUDPReq.PackCommand   := mtFUDPReq;
    FUDPReq.PackParts     := 1;
    FUDPReq.PackCount     := 1;
    If (TIdUDPServer(vipTCPObject).Active) And (Not(Value)) Then
     If (AnsiLowerCase(vHost) <> '127.0.0.1') And
        (AnsiLowerCase(vHost) <> 'localhost') Then
      Begin
       FUDPReq.TotalSize     := Length(DisconnectString + vWelcomeMessage);
       TIdUDPServer(vipTCPObject).SendBuffer(vHost, vLocalPort, StrToWide(DisconnectString + vWelcomeMessage, FUDPReq));
      End;
    ConnectUDP;
    If Value Then
     Begin
      vActive := TIdUDPServer(vipTCPObject).Active;
      If vActive then
       If (AnsiLowerCase(vHost) <> '127.0.0.1') And
          (AnsiLowerCase(vHost) <> 'localhost') Then
        Begin
         FUDPReq.TotalSize     := Length(ConnectString + vWelcomeMessage + '<|>' + GetLocalIP + '<|>');
         TIdUDPServer(vipTCPObject).SendBuffer(vHost, vLocalPort, StrToWide(ConnectString + vWelcomeMessage + '<|>' + GetLocalIP + '<|>', FUDPReq));
        End;
     End
    Else
     vActive := Value;
   Except
    vActive := False;
   End;
  End
 Else if (vipTCPObject is TIdTCPServer) Then
  Begin
   Try
    TIdTCPServer(vipTCPObject).Active      := False;
    TIdTCPServer(vipTCPObject).DefaultPort := vLocalPort;
    TIdTCPServer(vipTCPObject).Active      := Value;
    If Value Then
     vActive := TIdTCPServer(vipTCPObject).Active
    Else
     vActive := Value;
   Except
    vActive := False;
   End;
  End
 Else if (vipTCPObject is TIdTCPClient) Then
  Begin
   Try
    Try
     If TIdTCPClient(vipTCPObject).Connected Then
      Begin
       TIdTCPClient(vipTCPObject).Disconnect;
       Application.ProcessMessages;
       Sleep(1);
      End;
    Except
    End;
    If Value Then
     Begin
      Try
       TIdTCPClient(vipTCPObject).Host           := vHost;
       TIdTCPClient(vipTCPObject).Port           := vLocalPort;
       TIdTCPClient(vipTCPObject).ConnectTimeout := 0;
       TIdTCPClient(vipTCPObject).Connect;
      Except
      End;
      vActive := TIdTCPClient(vipTCPObject).Connected;
     End
    Else
     vActive := Value;
   Except
    vActive := False;
   End;
  End
 Else
  vActive := False;
 If Not (vActive) And (vAutoGarbage) Then
  GarbageCollector(-3);
 Try
  If SimpleTimerGarbage <> Nil Then
   SimpleTimerGarbage.Enabled := (vActive) And (vAutoGarbage);
 Except
 End;
End;

Procedure TipPoolerService.GarbageCollector(ConnectionId : Integer = -2);
Var
 I         : Integer;
 vListLock : TList;
Begin
 If Not vInGarbage Then
  vInGarbage := True
 Else
  Exit;
 TrimAppMemorySize;
 Try
  vListLock := vDataBuffer.LockList;
  Try
   I := vListLock.Count -1;
   While I > -1 Do
    Begin
     If ((System.DateUtils.MilliSecondsBetween(Now,
         TClientDataBuffer(vListLock[I]^).InternalTick) > vGarbageTime) Or
         ((ConnectionId > -2)  And
          (TClientDataBuffer(vListLock[I]^).ConnectionId = ConnectionId)) Or
          (ConnectionId = -3)) Or
         (TClientDataBuffer(vListLock[I]^).Readed)  Then
      Begin
       TClientDataBuffer(vListLock[I]^).Data := '';
       FreeMem(vListLock[I]);
       vListLock.Delete(I);
       vListLock.Capacity := vListLock.Count;
       vListLock.Pack;
      End;
     Dec(I);
    End;
  Except
  End;
 Finally
  vDataBuffer.UnlockList;
 End;
 vInGarbage := False;
End;

Destructor  TipPoolerService.Destroy;
 Procedure ClearBuffer;
 Var
  I : Integer;
  vListLock : TList;
 Begin
  vListLock := vDataBuffer.LockList;
  Try
   For I := vListLock.Count -1 Downto 0 do
    Begin
 //    FreeAndNil(vDataBuffer[I]^);
     TClientDataBuffer(vListLock[I]^).Data := '';
     vListLock[I] := Nil;
     vListLock.Delete(I);
     vListLock.Capacity := vListLock.Count;
     vListLock.Pack;
    End;
  Finally
   vDataBuffer.UnlockList;
  End;
  FreeAndNil(vDataBuffer);
 End;
 Procedure ClearPeers;
 Var
  I : Integer;
  vListLock : TList<TBroadcastPeer>;
 Begin
  If vBroadcastPeers <> Nil Then
   Begin
    vListLock := vBroadcastPeers.LockList;
    Try
     For I := vListLock.Count -1 Downto 0 do
      vListLock.Delete(I);
    Finally
     vBroadcastPeers.unlockList;
    End;
    FreeAndNil(vBroadcastPeers);
   End;
 End;
Begin
 GarbageCollector(-3);
 SetTCPObject(Nil);
 Try
  SetActive(False);
  ClearPeers;
 Except
 End;
 If vBroadcastPeers <> Nil Then
  FreeAndNil(vBroadcastPeers);
 If SimpleTimer <> Nil Then
  Begin
//   SimpleTimer.SetEvent;
   SimpleTimer.Enabled := False;
   FreeAndNil(SimpleTimer);
  End;
 If SimpleEvent <> Nil Then
  Begin
//   SimpleTimer.SetEvent;
   FreeAndNil(SimpleEvent);
  End;
 If SimpleTimerGarbage <> Nil Then
  Begin
   SimpleTimerGarbage.Enabled := False;
   FreeAndNil(SimpleTimerGarbage);
  End;
 {
 If (vListenThread <> Nil) And
    (vipTCPObject is TIdTCPClient) Then
  Begin
   Stop(vListenThread);
   Application.ProcessMessages;
   Sleep(2000);
   vListenThread.WaitFor;
   FreeAndNil(vListenThread);
   Application.ProcessMessages;
   Sleep(1000);
   Application.ProcessMessages;
  End;
 }
 If vCriticalSection <> Nil Then
  Begin
   vCriticalSection.Release;
   FreeAndNil(vCriticalSection);
  End;
 If vDataBuffer <> Nil Then
  FreeAndNil(vDataBuffer);
 If ClientObject <> Nil Then
  FreeAndNil(ClientObject);
 If BufferDownload <> Nil Then
  FreeAndNil(BufferDownload);
 Sleep(1000);
 Application.ProcessMessages;
 Sleep(100);
 Inherited;
End;

Procedure TipPoolerService.CaptureData(Sender: TObject);
Var
 vLine : String;
Begin
 SimpleTimer.Enabled := False;
 If vipTCPObject is TIdTCPClient Then
  Begin
   If TIdTCPClient(vipTCPObject).Connected Then
    Begin
     If TIdTCPClient(vipTCPObject).IOHandler.InputBufferIsEmpty Then
      Begin
       TIdTCPClient(vipTCPObject).IOHandler.CheckForDataOnSource(10);
       TIdTCPClient(vipTCPObject).IOHandler.CheckForDisconnect;
       If TIdTCPClient(vipTCPObject).IOHandler.InputBufferIsEmpty Then
        Begin
         SimpleTimer.Enabled := True;
         Exit;
        End;
      End;
     vLine    := TIdTCPClient(vipTCPObject).IOHandler.InputBufferAsString;
     ClientObject.TCPRead(Self, vLine);
    End;
   SimpleTimer.Enabled := True;
  End;
End;

Procedure TipPoolerService.CaptureExecute(Sender: TObject);
Begin
 If SimpleTimerGarbage <> Nil Then
  SimpleTimerGarbage.Enabled := False;
 If (vAutoGarbage) Then
  GarbageCollector;
 If SimpleTimerGarbage <> Nil Then
  SimpleTimerGarbage.Enabled := (SimpleTimerGarbage <> Nil) And (vAutoGarbage);
End;

Procedure TipPoolerService.SetASInfo;
Begin
 vASInfo := GetMyASInfo;
End;

Constructor TipPoolerService.Create(AOwner  : TComponent);
Begin
 Inherited;
 Owner                       := AOwner;
 aSelf                       := Self;
 vTimeOut                    := 5000;
 vGarbageTime                := 5000;
 vBufferLine                 := 8192;
 vConnections                := 0;
 vCompression                := False;
 vLocalPort                  := 777;
 vHost                       := 'localhost';
 vInternetIP                 := '';
 vLineBreak                  := '$<EOL>$';
 vEOC                        := '<$EOC$>';
 vAutoGarbage                := True;
 vGetASInfo                  := False;
 vSingleLineMode             := False;
 vCommandsReleaseCache       := vSingleLineMode;
 vCommandsReleaseCount       := -1;
// SetASInfo;
 vDataBuffer                 := TDataBuffer.Create;
 vCriticalSection            := TCriticalSection.Create;
 vBroadcastPeers             := TBroadcastPeers.Create;
 SimpleEvent                 := TLightWeightEvent.Create;
 SimpleTimer                 := TSimpleTimer.Create;
 SimpleTimer.OnTimer         := CaptureData;
 SimpleTimer.Interval        := 3;
 SimpleTimerGarbage          := TSimpleTimer.Create;
 SimpleTimerGarbage.OnTimer  := CaptureExecute;
 SimpleTimerGarbage.Interval := vGarbageTime;
 vListenThread               := Nil;
 BufferDownload              := TBufferDownload.Create;
End;

Function  TipPoolerService.GetTCPObject   : TipTCPObject;
Begin
 Result := vipTCPObject;
End;

Procedure TipPoolerService.SetSingleLineMode(Value : Boolean);
Begin
 vSingleLineMode := Value;
{
 If (vipTCPObject Is TipwIPPort) Then
  TipwIPPort(vipTCPObject).SingleLineMode := vSingleLineMode;
 }
End;

Procedure TipPoolerService.SetTCPObject(Value : TipTCPObject);
Var
 vStart : Boolean;
Begin
 {
 If (Value is TipwIPDaemon) then
  Begin
   vipTCPObject                                    := Value;
   If ClientObject = Nil Then
    ClientObject                                   := TipPoolerObject.Create(Self);
   TipwIPDaemon(vipTCPObject).DefaultMaxLineLength := vBufferLine;
   TipwIPDaemon(vipTCPObject).OnConnected          := ClientObject.Connected;
   TipwIPDaemon(vipTCPObject).OnConnectionRequest  := ClientObject.ConnectionRequest;
   TipwIPDaemon(vipTCPObject).OnDataIn             := ClientObject.DataInServer;
   TipwIPDaemon(vipTCPObject).OnDisconnected       := ClientObject.Disconnected;
   TipwIPDaemon(vipTCPObject).OnError              := ClientObject.Error;
   TipwIPDaemon(vipTCPObject).OnReadyToSend        := ClientObject.ReadyToSendServer;
  End
 Else
 }
 If (Value is TIdUDPServer) Then
  Begin
   vipTCPObject                                 := Value;
   If ClientObject = Nil Then
    ClientObject                                := TipPoolerObject.Create(Self);
   TIdUDPServer(vipTCPObject).DefaultPort       := 0;
   TIdUDPServer(vipTCPObject).OnUDPRead         := ClientObject.UDPRead;
  End
 Else if (Value is TIdTCPServer) Then
  Begin
   vipTCPObject                                 := Value;
   If ClientObject = Nil Then
    ClientObject                                := TipPoolerObject.Create(Self);
   TIdTCPServer(vipTCPObject).OnExecute         := ClientObject.Execute;
   TIdTCPServer(vipTCPObject).OnConnect         := ClientObject.TCPServerConnect;
   TIdTCPServer(vipTCPObject).OnDisconnect      := ClientObject.TCPServerDisconnect;
  End
 Else if (Value is TIdTCPClient) Then
  Begin
   vipTCPObject                        := Value;
   If ClientObject = Nil Then
    ClientObject                       := TipPoolerObject.Create(Self);
   TIdTCPClient(vipTCPObject).OnStatus := ClientObject.OnClientStatus;
   SimpleTimer.Enabled                 := Value <> Nil;
   {
   vStart                  := False;
   If (vListenThread = Nil) Then
    Begin
     vListenThread           := TListenThread.Create;
     vListenThread.aSender   := Self;
     vListenThread.OnTCPRead := ClientObject.TCPRead;
     vStart                  := True;
    End;
   If vipTCPObject = Nil Then
    vListenThread.TCPObject := Nil
   Else
    vListenThread.TCPObject := TIdTCPClient(vipTCPObject);
   If vStart Then
    vListenThread.Start;
    }
//   TIdTCPClient(vipTCPObject).OnDisconnected    := ClientObject.Execute;
//   TIdTCPClient(vipTCPObject).OnStatus          := ClientObject.Execute;
//   TIdTCPClient(vipTCPObject).OnWork            := ClientObject.Execute;
  End
 {
 Else if (Value is TipwIPPort)  Then
  Begin
   vipTCPObject                                 := Value;
   If ClientObject = Nil Then
    ClientObject                                   := TipPoolerObject.Create(Self);
//   TipwIPPort(vipTCPObject).DefaultMaxLineLength := vBufferLine;
   TipwIPPort(vipTCPObject).OnConnected         := ClientObject.Connected;
   TipwIPPort(vipTCPObject).OnConnectionStatus  := ClientObject.ConnectionStatus;
   TipwIPPort(vipTCPObject).OnDataIn            := ClientObject.DataInClient;
   TipwIPPort(vipTCPObject).OnDisconnected      := ClientObject.Disconnected;
   TipwIPPort(vipTCPObject).OnError             := ClientObject.Error;
   TipwIPPort(vipTCPObject).OnReadyToSend       := ClientObject.ReadyToSendClient;
//   SetSingleLineMode(vSingleLineMode);
  End
}
 Else
  Begin
   SimpleTimer.Enabled := False;
   vipTCPObject := Nil;
  End;
End;

Procedure TipPoolerObject.Error(Sender: TObject;
                                ConnectionId: Integer;
                                ErrorCode: Integer;
                                const Description: String);
Begin
{
 If Sender is TipwIPDaemon Then
  Begin
//   TipwIPDaemon(Sender).DoEvents;
   If Assigned(TipPoolerService(aParent).vErrorServer) Then
    TipPoolerService(aParent).vErrorServer(ConnectionId, ErrorCode, Description);
  End;
}
End;

Procedure TipPoolerObject.Error(Sender: TObject; ErrorCode: integer;
                                Const Description: String);
Begin
{
 If Sender is TipwIPPort Then
  Begin
//   TipwIPDaemon(Sender).DoEvents;
   If Assigned(TipPoolerService(aParent).vErrorClient) Then
    TipPoolerService(aParent).vErrorClient(ErrorCode, Description);
  End;
}
End;

Procedure TipPoolerObject.Connected(Sender: TObject; ConnectionId,
                                    StatusCode: Integer;
                                    Const Description: String);
Begin
{
 If Sender is TipwIPDaemon Then
  Begin
   TipPoolerService(aParent).vConnections := TipwIPDaemon(Sender).ConnectionCount;
//   TipwIPDaemon(Sender).EOL[ConnectionID] := #10;
   If Assigned(TipPoolerService(aParent).vConnectedServer) Then
    TipPoolerService(aParent).vConnectedServer(ConnectionId, StatusCode, Description);
  End;
}
End;

Procedure TipPoolerObject.Connected(Sender: TObject; StatusCode: Integer;
                                    Const Description: String);
Begin
{
 If Sender is TipwIPPort Then
  Begin
   TipPoolerService(aParent).vConnections := 1;
//   TipwIPPort(Sender).DoEvents;
   If Assigned(TipPoolerService(aParent).vConnectedClient) Then
    TipPoolerService(aParent).vConnectedClient(StatusCode, Description);
  End;
}
End;

Procedure TipPoolerObject.ConnectionRequest(Sender: TObject;
                                            Const Address : String;
                                            Port          : Integer;
                                            Var Accept    : Boolean);
Begin
{
 If Sender is TipwIPDaemon Then
  Begin
//   TipwIPDaemon(Sender).DoEvents;
   If Assigned(TipPoolerService(aParent).vConnectionRequestServer) Then
    TipPoolerService(aParent).vConnectionRequestServer(Address, Port, Accept);
  End;
}
End;

Procedure TipPoolerObject.ConnectionStatus(Sender: TObject;
                                           Const ConnectionEvent: String;
                                           StatusCode: Integer;
                                           Const Description : String);
Begin
{
 If Sender is TipwIPPort Then
  Begin
//   TipwIPPort(Sender).DoEvents;
   If Assigned(TipPoolerService(aParent).vConnectionStatusClient) Then
    TipPoolerService(aParent).vConnectionStatusClient(ConnectionEvent, StatusCode, Description);
  End;
}
End;

Procedure TipPoolerObject.ExecMethod(Execute : TExecuteProc = Nil);
Var
 EffectThread : TThread;
Begin
 EffectThread.CreateAnonymousThread(Procedure
                                    Begin
                                     //Se precisar interagir com a Thread da Interface
                                     If Assigned(Execute) Then
                                      TThread.Synchronize (TThread.CurrentThread,
                                                           Procedure
                                                           Begin
                                                            Execute;
                                                            EffectThread.DisposeOf;
                                                           End);
                                    End).Start;
End;

Procedure TipPoolerObject.AddBuffer(ConnectionId  : Integer;
                                    Text          : String;
                                    DataEncrypted : Boolean = False);
Var
 ClientDataBuffer : PClientDataBuffer;
 vTempLine        : String;
 vPosEof          : Integer;
 vListLock        : TList;
Begin
 vPosEof          := Pos(TipPoolerService(aParent).vEOC, Text);
 New(ClientDataBuffer);
 If (Pos(TipPoolerService(aParent).vEOC, Text) = 0) And
    (Text <> '')  Then
  Begin
   Try
//    ClientDataBuffer^              := TClientDataBuffer.Create;
    ClientDataBuffer^.ConnectionId := ConnectionId;
    ClientDataBuffer^.Data         := Text;
    ClientDataBuffer^.DataLength   := Length(Text);
    ClientDataBuffer^.InternalTick := Now;
    ClientDataBuffer^.Readed       := False;
    ClientDataBuffer^.EOF          := False;
    vListLock                      := TipPoolerService(aParent).vDataBuffer.LockList;
    Try
     vListLock.Capacity := vListLock.Count + 1;
     vListLock.Add(ClientDataBuffer);
     vListLock.Capacity := vListLock.Count;
    Finally
     TipPoolerService(aParent).vDataBuffer.UnlockList;
    End;
   Except
   End;
  End
 Else If (Text <> '') Then
  Begin
   vTempLine := Copy(Text, 1, vPosEof + Length(TipPoolerService(aParent).vEOC) -1);
   Delete(Text, 1, vPosEof + Length(TipPoolerService(aParent).vEOC) -1);
//   ClientDataBuffer^              := TClientDataBuffer.Create;
   ClientDataBuffer^.ConnectionId := ConnectionId;
   ClientDataBuffer^.Data         := vTempLine;
   ClientDataBuffer^.DataLength   := Length(vTempLine);
   ClientDataBuffer^.InternalTick := Now;
   ClientDataBuffer^.Readed       := False;
   ClientDataBuffer^.EOF          := (vPosEof > 1);
   vListLock                      := TipPoolerService(aParent).vDataBuffer.LockList;
   Try
    vListLock.Capacity := vListLock.Count + 1;
    vListLock.Add(ClientDataBuffer);
    vListLock.Capacity := vListLock.Count;
   Finally
    TipPoolerService(aParent).vDataBuffer.UnlockList;
   End;
   If (Length(Text) > 0) Then
    Begin
     AddBuffer(ConnectionId, Text, DataEncrypted);
     Exit;
    End;
  End;
End;

Procedure TipPoolerObject.DataInServer(Sender       : TObject;
                                       ConnectionId : Integer;
                                       Text         : String;
                                       EOL          : Boolean);
Var
 vTempString : String;
 vEOF        : Boolean;
Begin
 If Not TipPoolerService(aParent).CheckConnect(ConnectionId) Then
  Exit;
// vTempString := String(TipwIPDaemon(Sender).DataInTextB);
 If EOL Then
  Begin
   //Retirando os EOL's
   If Pos(TipPoolerService(aParent).vLineBreak, vTempString) > 0 Then
    vTempString := StringReplace(vTempString, TipPoolerService(aParent).vLineBreak, '', [rfReplaceAll]);
   //Retirando os EOB's
   If Pos(vEOB, vTempString) > 0 Then
    vTempString := StringReplace(vTempString, vEOB, '', [rfReplaceAll]);
  End;
 If TipPoolerService(aParent).EncryptData Then
  vEOF         := (Pos(TipPoolerService(aParent).vEOC, HexToString(vTempString)) > 0)
 Else
  vEOF         := (Pos(TipPoolerService(aParent).vEOC, vTempString) > 0);
 If vTempString <> '' Then
  Begin
   AddBuffer(ConnectionId, vTempString, False);
   {
   If Sender is TipwIPDaemon Then
    Begin
     Try
      If vEOF Then
       Begin
        If Assigned(TipPoolerService(aParent).vDataInServer) Then
         TipPoolerService(aParent).vDataInServer(ConnectionId);
       End;
     Except
     End;
    End;
    }
  End;
End;

Procedure TipPoolerObject.DataInClient(Sender: TObject; Text: String; EOL: Boolean);
Var
 vTempString  : String;
 vEOF         : Boolean;
Begin
 If Not TipPoolerService(aParent).CheckConnect(-1) Then
  Exit;
// vTempString := String(TipwIPPort(Sender).DataInTextB);
 If EOL Then
  Begin
   //Retirando os EOL's
   If Pos(TipPoolerService(aParent).vLineBreak, vTempString) > 0 Then
    vTempString := StringReplace(vTempString, TipPoolerService(aParent).vLineBreak, '', [rfReplaceAll]);
   //Retirando os EOB's
   If Pos(vEOB, vTempString) > 0 Then
    vTempString := StringReplace(vTempString, vEOB, '', [rfReplaceAll]);
  End;
 If TipPoolerService(aParent).EncryptData Then
  vEOF         := (Pos(TipPoolerService(aParent).vEOC, HexToString(vTempString)) > 0)
 Else
  vEOF         := (Pos(TipPoolerService(aParent).vEOC, vTempString) > 0);
 If vTempString <> '' Then
  Begin
   AddBuffer(-1, vTempString, False);
   {
   If Sender is TipwIPPort Then
    Begin
     Try
      If vEOF Then
       Begin
        If Assigned(TipPoolerService(aParent).vDataInClient) Then
         TipPoolerService(aParent).vDataInClient;
       End;
     Except
     End;
    End;
    }
  End;
End;

Procedure TipPoolerObject.Disconnected(Sender: TObject; StatusCode: Integer;
                                       Const Description: String);
Begin
{
 If Sender is TipwIPPort Then
  Begin
   TipwIPPort(Sender).AcceptData := False;
   TipPoolerService(aParent).vConnections := 0;
   Try
    If Assigned(TipPoolerService(aParent).vDisconnectedClient) Then
     TipPoolerService(aParent).vDisconnectedClient(StatusCode, Description);
   Except
   End;
  End;
}
End;

Procedure TipPoolerObject.OnClientStatus(ASender: TObject;
                                         Const AStatus: TIdStatus;
                                         Const AStatusText: string);
Begin
 Case AStatus Of
  hsResolving     : ;
  hsConnecting    : ;
  hsConnected     :
   Begin
    If Assigned(TipPoolerService(aParent).vReadyToSendClient) Then
     TipPoolerService(aParent).vReadyToSendClient;
   End;
  hsDisconnecting : ;
  hsDisconnected  :
   Begin
    If Assigned(TipPoolerService(aParent).vDisconnectedClient) Then
     TipPoolerService(aParent).vDisconnectedClient(-1, 'Disconnected');
   End;
  hsStatusText    : ;
 End;
End;

Procedure TipPoolerObject.TCPServerDisconnect(AContext: TIdContext);
Begin
 Try
//  If Not Assigned(AContext.Data) or (AContext.Data = Nil) Then Exit;
   TipPoolerService(aParent).vConnections := TipPoolerService(aParent).CountContexts(TIdTCPServer(TipPoolerService(aParent).vipTCPObject).Contexts);
   Try
    If Assigned(TipPoolerService(aParent).vDisconnectedServer) Then
     TipPoolerService(aParent).vDisconnectedServer(TClientRect(AContext.Data).Handle, -1, 'Disconnect');
   Except
   End;
  TClientRect(AContext.Data).Free;
  AContext.Data := Nil;
 Except
 End;
End;

Procedure TipPoolerObject.Disconnected(Sender            : TObject;
                                       ConnectionId,
                                       StatusCode        : Integer;
                                       Const Description : String);
Begin
{
 If Sender is TipwIPDaemon Then
  Begin
   TipPoolerService(aParent).vConnections := TipwIPDaemon(Sender).ConnectionCount;
   Try
    If Assigned(TipPoolerService(aParent).vDisconnectedServer) Then
     TipPoolerService(aParent).vDisconnectedServer(ConnectionId, StatusCode, Description);
   Except
   End;
  End;
}
End;

//Eventos do Client
Procedure TipPoolerService.SetDataInClient(Value : TDataInClient);
Begin
 vDataInClient := Value;
End;

Function  TipPoolerService.GetDataInClient : TDataInClient;
Begin
 Result := vDataInClient;
End;

Procedure TipPoolerService.SetErrorClient(Value : TErrorClient);
Begin
 vErrorClient := Value;
End;

Function  TipPoolerService.GetErrorClient : TErrorClient;
Begin
 Result := vErrorClient;
End;

Procedure TipPoolerService.SetConnectedClient(Value : TConnectedClient);
Begin
 vConnectedClient := Value;
End;

Function  TipPoolerService.GetConnectedClient : TConnectedClient;
Begin
 Result := vConnectedClient;
End;

Procedure TipPoolerService.SetConnectionStatusClient(Value : TConnectionStatusClient);
Begin
 vConnectionStatusClient := Value;
End;

Function  TipPoolerService.GetConnectionStatusClient : TConnectionStatusClient;
Begin
 Result := vConnectionStatusClient;
End;

Procedure TipPoolerService.SetDisconnectedClient(Value : TDisconnectedClient);
Begin
 vDisconnectedClient := Value;
End;

Function  TipPoolerService.GetDisconnectedClient : TDisconnectedClient;
Begin
 Result := vDisconnectedClient;
End;

//Eventos do Servidor
Procedure TipPoolerService.SetDataInServer(Value : TDataInServer);
Begin
 vDataInServer := Value;
End;

Function  TipPoolerService.GetDataInServer : TDataInServer;
Begin
 Result := vDataInServer;
End;

Procedure TipPoolerService.SetErrorServer(Value : TErrorServer);
Begin
 vErrorServer := Value;
End;

Function  TipPoolerService.GetErrorServer : TErrorServer;
Begin
 Result := vErrorServer;
End;

Procedure TipPoolerService.SetConnectedServer(Value : TConnectedServer);
Begin
 vConnectedServer := Value;
End;

Function  TipPoolerService.GetConnectedServer : TConnectedServer;
Begin
 Result := vConnectedServer;
End;

Procedure TipPoolerService.SetConnectionRequestServer(Value : TConnectionRequestServer);
Begin
 vConnectionRequestServer := Value;
End;

Function  TipPoolerService.GetConnectionRequestServer : TConnectionRequestServer;
Begin
 Result := vConnectionRequestServer;
End;

Function  TipPoolerService.GetReadyToSendClient : TReadyToSendClient;
Begin
 Result := vReadyToSendClient;
End;

Procedure TipPoolerService.SetReadyToSendClient(Value : TReadyToSendClient);
Begin
 vReadyToSendClient := Value;
End;

Function  TipPoolerService.GetReadyToSendServer : TReadyToSendServer;
Begin
 Result := vReadyToSendServer;
End;

Function TipPoolerService.ReadBuffer(ConnectionId : Integer = -1;
                                     BreakPoint   : Boolean = True) : String;
Var
 I         : Integer;
 BreakLine : Boolean;
 vTempData : String;
 vListLock : TList;
Begin
 Result := '';
 I      := 0;
 BreakLine := False;
 vListLock := vDataBuffer.LockList;
 Try
  While I <= vListLock.Count -1 Do
   Begin
    If (TClientDataBuffer(vListLock[I]^).ConnectionId = ConnectionId) And
        Not(TClientDataBuffer(vListLock[I]^).Readed) Then
     Begin
      vTempData := vTempData + TClientDataBuffer(vListLock[I]^).Data;
      TClientDataBuffer(vListLock[I]^).Readed := True;
     End;
    BreakLine := (Pos(vEOC, vTempData) > 0);
    If BreakLine Then
     Break;
    Inc(I);
   End;
  Result := vTempData;
  If BreakLine Then
   Begin
    If (Pos(vEOC, vTempData) > 0) Then
     Result := StringReplace(StringReplace(Result, vEOC, '', [rfReplaceAll]), vEOB, '', [rfReplaceAll]);
   End;
  If Result <> '' Then
   If EncryptData Then
    Result := HexToString(Result);
 Finally
  vDataBuffer.UnlockList;
 End;
End;

Function TipPoolerService.ReadBuffer(BreakPoint : Boolean = True) : String;
Begin
 Result := ReadBuffer(-1, BreakPoint);
End;

Function  TipPoolerService.HasBuffer(ConnectionId : Integer = -1) : Integer;
Var
 I   : Integer;
 vTempString : String;
 vListLock   : TList;
Begin
 Result := 0;
 vListLock := vDataBuffer.LockList;
 Try
  I         := vListLock.Count -1;
  Try
   While I > -1 Do
    Begin
     If (TClientDataBuffer(vListLock[I]^).ConnectionId = ConnectionId) And
        Not(TClientDataBuffer(vListLock[I]^).Readed)                   Then
      vTempString := vTempString + TClientDataBuffer(vListLock[I]^).Data;
     If (Pos(vEOC, vTempString) > 0) Then
      Begin
       Inc(Result);
       Delete(vTempString, 1, Pos(vEOC, vTempString) + Length(vEOC) -1);
      End;
     Dec(I);
    End;
  Except

  End;
 Finally
  vDataBuffer.UnlockList;
 End;
End;

Procedure TipPoolerService.SetReadyToSendServer(Value : TReadyToSendServer);
Begin
 vReadyToSendServer := Value;
End;

Procedure TipPoolerService.SetDisconnectedServer(Value : TDisconnectedServer);
Begin
 vDisconnectedServer := Value;
End;

Function  TipPoolerService.GetDisconnectedServer : TDisconnectedServer;
Begin
 Result := vDisconnectedServer;
End;

Procedure TipPoolerService.ReleaseCacheAction(ConnectionId : Integer);
Var
 I : Integer;
 vListLock : TList;
Begin
 I := 0;
 vListLock := vDataBuffer.LockList;
 Try
  While I <= vListLock.Count -1 Do
   Begin
    If (TClientDataBuffer(vListLock[I]^).ConnectionId = ConnectionId) And
       Not(TClientDataBuffer(vListLock[I]^).Readed) Then
     TClientDataBuffer(vListLock[I]^).Readed := True;
    Inc(I);
   End;
 Finally
  vDataBuffer.UnlockList;
 End;
End;

Function TipPoolerService.Write(ConnectionId : Integer;
                                Data         : TMemoryStream;
                                Const IP     : String = '';
                                Const Port   : Integer = -1) : Boolean;
Begin
 Result := Write(ConnectionId, MemoryStreamToString(Data), IP, Port);
End;

Function TipPoolerService.Write(Data         : TMemoryStream;
                                Const IP     : String = '';
                                Const Port   : Integer = -1) : Boolean;
Begin
 Result := Write(-1, MemoryStreamToString(Data), IP, Port);
End;

Function TipPoolerService.UDPWelcomeMessage(ConnectionId : Integer) : String;
Begin
 Result := ClientObject.UDPWelcomeMessage(ConnectionId);
End;

Procedure TipPoolerService.AddPeer(Value : TBroadcastPeer);
Var
 vList  : TList<TBroadcastPeer>;
 ValueT : TBroadcastPeer;
Begin
 vList := vBroadcastPeers.LockList;
 Try
  ValueT.ip      := Value.ip;
  ValueT.localIp := Value.localIp;
  ValueT.Port    := Value.Port;
  If (Value.ip <> '') And (Value.Port <> -1) Then
   vList.Add(ValueT);
 Finally
  vBroadcastPeers.UnlockList;
 End;
End;
Procedure TipPoolerService.SetInternetIP(Value : String);
Begin
 If Trim(Value) <> '' Then
  Begin
   vInternetIP := Trim(Value);
   vASInfo.ip  := vInternetIP;
  End
 Else
  vASInfo := GetMyASInfo;
End;

Procedure TipPoolerService.DeletePeer(Index : Integer);
Var
 vList : TList<TBroadcastPeer>;
Begin
 vList := vBroadcastPeers.LockList;
 Try
  vList.Delete(Index);
 Finally
  vBroadcastPeers.UnlockList;
 End;
End;

Procedure TipPoolerService.ClearPeers;
Var
 vList : TList<TBroadcastPeer>;
 I     : Integer;
Begin
 vList := vBroadcastPeers.LockList;
 Try
  For I := vList.Count -1 Downto 0 Do
   vList.Delete(I);
 Finally
  vBroadcastPeers.UnlockList;
 End;
End;

Function TipPoolerService.UDPClient(ConnectionId : Integer; Var Found : Boolean) : TClientRect;
Var
 vList : TList;
 I     : Integer;
Begin
 Found := False;
 If vipTCPObject is TIdUDPServer Then
  Begin
   vList := ClientObject.vUdpListBindings.LockList;
   Try
    For I := vList.Count -1 Downto 0 Do
     Begin
      Found := TClientRect(vList.Items[I]).Handle = ConnectionId;
      If Found Then
       Begin
        Result := TClientRect(vList.Items[I]);
        Break;
       End;
     End;
   Finally
    ClientObject.vUdpListBindings.UnlockList;
   End;
  End;
End;

Function TipPoolerService.GetIP(ConnectionId : Integer) : TIpPort;
Var
 I, NumClients : Integer;
 ContextsList  : TList;
Begin
 Result.IP := '0.0.0.0';
 Result.Port := 0;
 ContextsList := TIdTCPServer(vipTCPObject).Contexts.LockList;
 With ContextsList Do
  Begin
   Try
    NumClients := Count;
    For I := 0 To NumClients -1 Do
     Begin
      If TIdContext(ContextsList[I]).Binding.Handle = ConnectionId Then
       Begin
        Result.Ip   := TIdContext(ContextsList[I]).Binding.PeerIP;
        Result.Port := TIdContext(ContextsList[I]).Binding.PeerPort;
        Break;
       End;
     End;
   Finally
    TIdTCPServer(vipTCPObject).Contexts.UnlockList;
   End;
  End;
End;

Function TipPoolerService.Write(ConnectionId : Integer;
                                Data         : String;
                                Const IP   : String = '';
                                Const Port : Integer = -1) : Boolean;
Var
 SrcStream,
 DestStream     : TMemoryStream;
 SocketHandle   : TIdContext;
 vDataPack,
 vDataBuff      : AnsiString;
 A, I,
 vBufferSize    : Integer;
 vIp            : String;
 vListData      : TList<TBroadcastPeer>;
 vError         : Boolean;
 FUDPReq        : rFUDPDataACK;
Begin
 Result := False;
 If vCompression Then
  Begin
   SrcStream    := TMemoryStream.Create;
   DestStream   := TMemoryStream.Create;
   Try
    StringToMemoryStream(SrcStream, Data);
    If SrcStream.Size > 0 Then
     Begin
      CompressStream(SrcStream, DestStream);
      Data := MemoryStreamToString(DestStream);
     End;
   Except
    FreeAndNil(SrcStream);
    FreeAndNil(DestStream);
   End;
  End;
 If vEncryptData Then
  Data := StringToHex(Data);
 {
 If vipTCPObject is TipwIPDaemon Then
  Begin
   Try
    If TipwIPDaemon(vipTCPObject).ConnectionCount >= ConnectionId Then
     Begin
      TipwIPDaemon(vipTCPObject).Timeout[ConnectionId]      := vTimeOut;
      If TipwIPDaemon(vipTCPObject).Connected[ConnectionId] Then
       Try
        TipwIPDaemon(vipTCPObject).DataToSendB[ConnectionId] := Data + vEOC + vEOB;
        Result := True;
       Except
       End;
     End;
   Except
   End;
  End
 Else If vipTCPObject is TipwIPPort Then
  Begin
   Try
    If ConnectionId = -1 Then
     Begin
      TipwIPPort(vipTCPObject).Timeout      := vTimeOut;
      If TipwIPPort(vipTCPObject).Connected Then
       Begin
        Try
         TipwIPPort(vipTCPObject).DataToSendB := Data + vEOC + vEOB;
         Result := True;
        Except

        End;
       End;
     End;
   Except
   End;
  End
 Else}
 If vipTCPObject is TIdUDPServer Then
  Begin
   vDataPack           := Data + vEOC + vEOB;
   FUDPReq.PackID      := GenBufferID;
   A                   := 1;
   FUDPReq.PackCommand := mtFUDPReq;
   FUDPReq.TotalSize   := Length(vDataPack);
   If Length(vDataPack) > 0 Then
    FUDPReq.PackParts  := Length(vDataPack) Div udpBuffer
   Else
    FUDPReq.PackParts  := 1;
   FUDPReq.TotalSize   := Length(vDataPack);
   If (Length(vDataPack) Mod udpBuffer) > 0 Then
    Inc(FUDPReq.PackParts);
   While (vDataPack <> '') Do
    Begin
     If (TIdUDPServer(vipTCPObject).Active) Then
      Begin
       If Length(vDataPack) > udpBuffer Then
        vBufferSize := udpBuffer
       Else
        vBufferSize := Length(vDataPack);
       vDataBuff := Copy(vDataPack, 1, vBufferSize);
       Delete(vDataPack, 1, vBufferSize);
//       If (AnsiLowerCase(vHost) <> '127.0.0.1') And
//          (AnsiLowerCase(vHost) <> 'localhost') Then
       If (Trim(IP) <> '') And (Port > -1) Then
        Begin
         FUDPReq.PackCount     := 1;
         If Not TIdUDPServer(vipTCPObject).Active Then
          Begin
           TIdUDPServer(vipTCPObject).Binding.Reset;
           Break;
          End;
         TIdUDPServer(vipTCPObject).SendBuffer(Trim(IP), Port,
                                               StrToWide(vDataBuff, FUDPReq));
         Application.ProcessMessages;
        End
       Else
        Begin
         vError := False;
         Try
          vListData := vBroadcastPeers.LockList;
          For I := 0 To vListData.Count -1 Do
           Begin
            If vASInfo.ip = vListData[I].ip Then
             vIp := vListData[I].localIp
            Else
             vIp := vListData[I].ip;
            Try
             If (TIdUDPServer(vipTCPObject).Active) Then
              Begin
               FUDPReq.PackCount     := A;
               If Not TIdUDPServer(vipTCPObject).Active Then
                Begin
                 TIdUDPServer(vipTCPObject).Binding.Reset;
                 Break;
                End;
               TIdUDPServer(vipTCPObject).SendBuffer(vIp, vListData[I].Port, StrToWide(vDataBuff, FUDPReq));
               Application.ProcessMessages;
              End
             Else
              Begin
               vError := True;
               Break;
              End;
//            TIdUDPServer(vipTCPObject).Broadcast(TIdBytes(vDataBuff), vListData[I].Port,
//                                                 vListData[I].ip); //, IndyTextEncoding_Default);
            Except
             vError := True;
             Break;
            End;
//            TIdUDPServer(vipTCPObject).Broadcast(TIdBytes(vDataBuff), vListData[I].Port,
//                                                 vListData[I].ip); //, IndyTextEncoding_Default);
           End;
         Finally
          Inc(A);
          If Not vError Then
           Result := vListData.Count > 0;
          vBroadcastPeers.UnlockList;
         End;
        End;
      End
     Else Break;
    End;
  End
 Else If vipTCPObject is TIdTCPServer Then
  Begin
   SocketHandle := ContextsByHandle(TIdTCPServer(vipTCPObject).Contexts, ConnectionId);
   If SocketHandle <> Nil Then
    Begin
     Try
      SocketHandle.Connection.IOHandler.Write(Data + vEOC + vEOB);
      Result := True;
     Except
     End;
    End;
  End;
End;

Function IndyTextEncoding_Default : IIdTextEncoding;
var
  LType: IdTextEncodingType;
begin
  LType := encASCII;
  Result := IndyTextEncoding(LType);
end;

Function TipPoolerService.SendBuffer(Ip : String; Port : Integer;
                                     Var UDPServer : TIdUDPServer;
                                     Value : String) : Boolean;
begin
 Result := False;
 Try
  UDPServer.Broadcast(Value, Port, ip); //, IndyTextEncoding_Default);
  Result := True;
 Finally
 End;
end;

Function TipPoolerService.Write(Data         : String;
                                Const IP     : String = '';
                                Const Port   : Integer = -1) : Boolean;
Var
 SrcStream,
 DestStream     : TMemoryStream;
 vDataPack,
 vDataBuff      : AnsiString;
 A, I,
 vBufferSize    : Integer;
 vListData      : TList<TBroadcastPeer>;
 vIp            : String;
 vError         : Boolean;
 FUDPReq        : rFUDPDataACK;
Begin
 Result := False;
 If vCompression Then
  Begin
   SrcStream    := TMemoryStream.Create;
   DestStream   := TMemoryStream.Create;
   Try
    StringToMemoryStream(SrcStream, Data);
    If SrcStream.Size > 0 Then
     Begin
      CompressStream(SrcStream, DestStream);
      Data := MemoryStreamToString(DestStream);
     End;
   Except
   End;
   FreeAndNil(SrcStream);
   FreeAndNil(DestStream);
  End;
 If vEncryptData Then
  Data := StringToHex(Data);
 {
 If vipTCPObject is TipwIPPort Then
  Begin
   TipwIPPort(vipTCPObject).Timeout    := vTimeOut;
   Try
    If TipwIPPort(vipTCPObject).Connected Then
     Begin
      Try
       TipwIPPort(vipTCPObject).DataToSendB := Data + vEOC + vEOB;
       Result := True;
       Exit;
      Except
       Result := False;
      End;
     End;
   Except
   End;
  End
 Else
 }
 If (vipTCPObject is TIdTCPClient) Then
  Begin
   Try
    If TIdTCPClient(vipTCPObject).Connected Then
     Begin
      Try
       TIdTCPClient(vipTCPObject).IOHandler.Write(Data + vEOC + vEOB);
       Result := True;
       Exit;
      Except
       Result := False;
      End;
     End;
   Except
   End;
  End
 Else If vipTCPObject is TIdUDPServer Then
  Begin
   vDataPack           := Data + vEOC + vEOB;
   FUDPReq.PackID      := GenBufferID;
   A                   := 1;
   FUDPReq.PackCommand := mtFUDPReq;
   FUDPReq.TotalSize   := Length(vDataPack);
   If Length(vDataPack) > 0 Then
    FUDPReq.PackParts  := Length(vDataPack) Div udpBuffer
   Else
    FUDPReq.PackParts  := 1;
   FUDPReq.TotalSize   := Length(vDataPack);
   If (Length(vDataPack) Mod udpBuffer) > 0 Then
    Inc(FUDPReq.PackParts);
   While (vDataPack <> '') Do
    Begin
     If (TIdUDPServer(vipTCPObject).Active) Then
      Begin
       If Length(vDataPack) > udpBuffer Then
        vBufferSize := udpBuffer
       Else
        vBufferSize := Length(vDataPack);
       vDataBuff := Copy(vDataPack, 1, vBufferSize);
       Delete(vDataPack, 1, vBufferSize);
//       If (AnsiLowerCase(vHost) <> '127.0.0.1') And
//          (AnsiLowerCase(vHost) <> 'localhost') Then
       If (Trim(IP) <> '') And (Port > -1) Then
        Begin
         FUDPReq.PackCount     := 1;
         If Not TIdUDPServer(vipTCPObject).Active Then
          Begin
           TIdUDPServer(vipTCPObject).Binding.Reset;
           Break;
          End;
         TIdUDPServer(vipTCPObject).SendBuffer(Trim(IP), Port,
                                               StrToWide(vDataBuff, FUDPReq));
//         TIdUDPServer(vipTCPObject).Broadcast(TIdBytes(vDataBuff), Port, Trim(IP));//, IndyTextEncoding_Default);
         Application.ProcessMessages;
        End
       Else
        Begin
         vError := False;
         Try
          vListData := vBroadcastPeers.LockList;
          For I := 0 To vListData.Count -1 Do
           Begin
            If vASInfo.ip = vListData[I].ip Then
             vIp := vListData[I].localIp
            Else
             vIp := vListData[I].ip;
            Try
             If (TIdUDPServer(vipTCPObject).Active) Then
              Begin
               FUDPReq.PackCount     := A;
               If Not TIdUDPServer(vipTCPObject).Active Then
                Begin
                 TIdUDPServer(vipTCPObject).Binding.Reset;
                 Break;
                End;
               TIdUDPServer(vipTCPObject).SendBuffer(vIp, vListData[I].Port, StrToWide(vDataBuff, FUDPReq));
               Application.ProcessMessages;
              End
             Else
              Begin
               vError := True;
               Break;
              End;
//            TIdUDPServer(vipTCPObject).Broadcast(TIdBytes(vDataBuff), vListData[I].Port,
//                                                 vListData[I].ip); //, IndyTextEncoding_Default);
            Except
             vError := True;
             Break;
            End;
//            TIdUDPServer(vipTCPObject).Broadcast(TIdBytes(vDataBuff), vListData[I].Port,
//                                                 vListData[I].ip); //, IndyTextEncoding_Default);
            Application.ProcessMessages;
           End;
         Finally
          Inc(A);
          If Not vError Then
           Result := vListData.Count > 0;
          vBroadcastPeers.UnlockList;
         End;
        End;
      End
     Else Break;
    End;
  End;
End;

Function TipPoolerService.DataToSend(ConnectionId : Integer;
                                     Data : String) : Boolean;
Var
 SrcStream,
 DestStream   : TMemoryStream;
 SocketHandle : TIdContext;
Begin
 Result := False;
 If vCompression Then
  Begin
   SrcStream    := TMemoryStream.Create;
   DestStream   := TMemoryStream.Create;
   Try
    StringToMemoryStream(SrcStream, Data);
    If SrcStream.Size > 0 Then
     Begin
      CompressStream(SrcStream, DestStream);
      Data := MemoryStreamToString(DestStream);
     End;
   Except
    FreeAndNil(SrcStream);
    FreeAndNil(DestStream);
   End;
  End;
 If vEncryptData Then
  Data := StringToHex(Data);
 {
 If vipTCPObject is TipwIPDaemon Then
  Begin
   Try
    If TipwIPDaemon(vipTCPObject).ConnectionCount >= ConnectionId Then
     Begin
      TipwIPDaemon(vipTCPObject).Timeout[ConnectionId]     := vTimeOut;
      If TipwIPDaemon(vipTCPObject).Connected[ConnectionId] Then
       Begin
        Try
         TipwIPDaemon(vipTCPObject).DataToSend[ConnectionId] := Data + vEOC + vLineBreak;
         Result := True;
        Except
         Result := False;
        End;
       End;
     End;
   Except
   End;
  End
 Else}
 If vipTCPObject is TIdTCPServer Then
  Begin
   SocketHandle := ContextsByHandle(TIdTCPServer(vipTCPObject).Contexts, ConnectionId);
   If SocketHandle <> Nil Then
    Begin
     Try
      SocketHandle.Connection.IOHandler.Write(Data + vEOC + vLineBreak);
      Result := True;
     Except
     End;
    End;
  End;
End;

Procedure TipPoolerService.SetGarbageTime(Value : LongInt);
Begin
 If SimpleTimerGarbage <> Nil Then
  Begin
   vGarbageTime                := Value;
   SimpleTimerGarbage.Interval := vGarbageTime;
  End;
End;

Procedure TipPoolerService.SetAutoGarbage(Value : Boolean);
Begin
 vAutoGarbage := Value;
End;

Procedure TipPoolerService.SetBufferLine(Value : LongInt);
Begin
 vBufferLine := Value;
 If (vipTCPObject <> Nil) Then
  Begin
   {
   If (vipTCPObject is TipwIPDaemon) Then
    Begin
     TipwIPDaemon(vipTCPObject).DefaultMaxLineLength := vBufferLine;
     If vBufferLine > -1 Then
      Begin
       TipwIPDaemon(vipTCPObject).Config(Format('InBufferSize=%d',  [vBufferLine]));
       TipwIPDaemon(vipTCPObject).Config(Format('OutBufferSize=%d', [vBufferLine]));
      End;
    End
   Else If (vipTCPObject is TipwIPPort) Then
    Begin
     If vBufferLine > -1 Then
      Begin
       TipwIPPort(vipTCPObject).Config(Format('InBufferSize=%d',  [vBufferLine]));
       TipwIPPort(vipTCPObject).Config(Format('OutBufferSize=%d', [vBufferLine]));
      End;
    End;
   }
  End;
End;

Function TipPoolerService.DataToSend(Data : String) : Boolean;
Var
 vValue      : String;
 SrcStream,
 DestStream   : TMemoryStream;
Begin
{
 If vAutoGarbage Then
  GarbageCollector;
}
 vValue      := Data;
 Result      := False;
 If vCompression Then
  Begin
   SrcStream    := TMemoryStream.Create;
   DestStream   := TMemoryStream.Create;
   Try
    StringToMemoryStream(SrcStream, vValue);
    If SrcStream.Size > 0 Then
     Begin
      CompressStream(SrcStream, DestStream);
      vValue := MemoryStreamToString(DestStream);
     End;
   Finally
    FreeAndNil(SrcStream);
    FreeAndNil(DestStream);
   End;
  End;
 If vEncryptData Then
  vValue := StringToHex(vValue);
 {
 If vipTCPObject is TipwIPPort Then
  Begin
   TipwIPPort(vipTCPObject).Timeout    := vTimeOut;
   Try
    If TipwIPPort(vipTCPObject).Connected Then
     Begin
      Try
       TipwIPPort(vipTCPObject).DataToSend := vValue + vEOC + vLineBreak;
       Result := True;
      Except
       Result := False;
      End;
     End;
   Except
   End;
  End
 Else
 }
 if (vipTCPObject is TIdTCPClient) Then
  Begin
   Try
    If TIdTCPClient(vipTCPObject).Connected Then
     Begin
      Try
       TIdTCPClient(vipTCPObject).IOHandler.Write(vValue + vEOC + vLineBreak);
       Result := True;
      Except
       Result := False;
      End;
     End;
   Except
   End;
  End;
End;

Procedure TipPoolerService.StartActions(Const ShutdownEvent: TLightweightEvent; WaitFor : LongWord;
                                        Value : TExecuteFunction;ConnectionId : Integer;
                                        Var Result : String);
Var
 vActual   : TDateTime;
 vTempWait : LongWord;
Begin
 vActual   := Now;
 vTempWait := System.DateUtils.MilliSecondsBetween(Now, vActual);
 While (WaitFor >= vTempWait) Do
  Begin
   vTempWait := System.DateUtils.MilliSecondsBetween(vActual, Now);
   {
   If vipTCPObject is TipwIPPort Then
    If Not TipwIPPort(vipTCPObject).Connected Then
     Break;
   If vipTCPObject is TipwIPDaemon Then
    If Not TipwIPDaemon(vipTCPObject).Listening Then
     Break;
    }
   ShutdownEvent.WaitFor(5);
   If Assigned(Value) Then
    Begin
     Result := Value(ConnectionId, False, Result);
     if Result <> '' then
      Break;
    End;
  End;
End;

Function  TipPoolerService.CurrentBuffer(ConnectionId : Integer;
                                         Wait         : Boolean = False;
                                         InitBuffer   : String = '') : String;
Var
 I   : Integer;
 vListLock : TList;
Begin
 Result  := '';
 I := 0;
 Result := InitBuffer;
 vListLock := vDataBuffer.LockList;
 Try
  While I <= vListLock.Count -1 Do
   Begin
    If (TClientDataBuffer(vListLock[I]^).ConnectionId = ConnectionId) And
        Not(TClientDataBuffer(vListLock[I]^).Readed) Then
     Begin
      Result := Result + TClientDataBuffer(vListLock[I]^).Data;
      TClientDataBuffer(vListLock[I]^).Readed := True;
      If (Pos(vEOC, Result) > 0) Then
       Begin
        Result := StringReplace(StringReplace(Result, vEOC, '', [rfReplaceAll]), vEOB, '', [rfReplaceAll]);
        Break;
       End;
      Exit;
     End;
    Inc(I);
   End;
  If Wait Then
   Begin
    If ((Result = '') Or (Result = InitBuffer)) And
       (Pos(vEOC, Result) = 0) Then
     Begin
      Try
       StartActions(SimpleEvent, vTimeOut, CurrentBuffer, ConnectionId, Result);
      Finally
       SimpleEvent.SetEvent;
      End;
     End;
   End;
 Finally
  vDataBuffer.UnlockList;
 End;
End;

Function  TipPoolerService.ReadLn(ConnectionId : Integer) : String;
Var
 vValue      : String;
 SrcStream,
 DestStream   : TMemoryStream;
Begin
 vValue      := CurrentBuffer(ConnectionId);
 If vValue <> '' Then
  Begin
   If vEncryptData Then
    vValue  := HexToString(vValue);
   If vCompression Then
    Begin
     SrcStream    := TMemoryStream.Create;
     DestStream   := TMemoryStream.Create;
     Try
      StringToMemoryStream(SrcStream, vValue);
      If SrcStream.Size > 0 Then
       Begin
        DeCompressStream(SrcStream, DestStream);
        vValue := MemoryStreamToString(DestStream);
       End;
     Finally
      FreeAndNil(SrcStream);
      FreeAndNil(DestStream);
     End;
    End;
  End;
 Result := vValue;
End;

Procedure TipPoolerObject.TCPRead(Sender : TObject; Data : String);
Var
 vTempString  : String;
 vEOF         : Boolean;
Begin
 Try
  If aParent = Nil Then
   Exit;
  vTempString := Data;// Text;
  //Retirando os EOB's
  If Pos(vEOB, vTempString) > 0 Then
   vTempString := StringReplace(vTempString, vEOB, '', [rfReplaceAll]);
  If TipPoolerService(Sender).EncryptData Then
   vEOF         := (Pos(TipPoolerService(Sender).vEOC, HexToString(vTempString)) > 0)
  Else
   vEOF         := (Pos(TipPoolerService(Sender).vEOC, vTempString) > 0);
  If vTempString <> '' Then
   Begin
    AddBuffer(-1, vTempString, False);
    Try
     If vEOF Then
      Begin
       If Assigned(TipPoolerService(Sender).vDataInClient) Then
        TipPoolerService(Sender).vDataInClient;
      End;
    Except
    End;
   End;
 Except
 End;
End;

Function  TipPoolerService.CountContexts(Contexts : TIdContextThreadList) : Integer;
Var
 NumClients   : Integer;
 ContextsList : TList;
Begin
 Result := 0;
 ContextsList := TIdTCPServer(vipTCPObject).Contexts.LockList;
 With ContextsList Do
  Begin
   Try
    Result := Count;
   Finally
    TIdTCPServer(vipTCPObject).Contexts.UnlockList;
   End;
  End;
End;

Function  TipPoolerService.HandleExists(Contexts : TIdContextThreadList;
                                        ConnectionId : Integer) : Boolean;
Var
 I, NumClients : Integer;
 ContextsList  : TList;
Begin
 Result := False;
 ContextsList := TIdTCPServer(vipTCPObject).Contexts.LockList;
 With ContextsList Do
  Begin
   Try
    NumClients := Count;
    For I := 0 To NumClients -1 Do
     Begin
      Result := TIdContext(ContextsList[I]).Binding.Handle = ConnectionId;
      If Result Then
       Break;
     End;
   Finally
    TIdTCPServer(vipTCPObject).Contexts.UnlockList;
   End;
  End;
End;

Function  TipPoolerService.ContextsByHandle(Contexts : TIdContextThreadList;
                                            ConnectionId : Integer) : TIdContext;
Var
 I, NumClients : Integer;
 ContextsList  : TList;
Begin
 Result := Nil;
 ContextsList := TIdTCPServer(vipTCPObject).Contexts.LockList;
 With ContextsList Do
  Begin
   Try
    NumClients := Count;
    For I := 0 To NumClients -1 Do
     Begin
      If TIdContext(ContextsList[I]).Binding.Handle = ConnectionId Then
       Begin
        Result := TIdContext(ContextsList[I]);
        Break;
       End;
     End;
   Finally
    TIdTCPServer(vipTCPObject).Contexts.UnlockList;
   End;
  End;
End;

Function  TipPoolerService.CheckConnect(ConnectionId  : Integer) : Boolean;
Begin
 Result := False;
 {
 If (vipTCPObject is TipwIPDaemon) Then
  vConnections := TipwIPDaemon(vipTCPObject).ConnectionCount;
 If vipTCPObject is TipwIPDaemon Then
  Begin
   Try
    If TipwIPDaemon(vipTCPObject).Listening Then
     If (ConnectionId > -1) And (ConnectionId <= vConnections) Then
      Result := TipwIPDaemon(vipTCPObject).Connected[ConnectionId];
   Except
//    If ConnectionId = 1 Then
//     Dec(vConnections);
   End;
  End
 Else If vipTCPObject is TipwIPPort Then
  Result := TipwIPPort(vipTCPObject).Connected
 Else
}
 If vipTCPObject is TIdTCPServer Then
  Begin
   Result := HandleExists(TIdTCPServer(vipTCPObject).Contexts, ConnectionId);
 //  SocketHandle := TIdTCPServer(vipTCPObject).Bindings.BindingByHandle(ConnectionId);
//   Result := SocketHandle <> Nil;
  End
 Else If vipTCPObject is TIdTCPClient Then
  Begin
   Result := TIdTCPClient(vipTCPObject).Connected;
 //  SocketHandle := TIdTCPServer(vipTCPObject).Bindings.BindingByHandle(ConnectionId);
//   Result := SocketHandle <> Nil;
  End;
End;

Function  TipPoolerService.ClientDisconnect(ConnectionId : Integer) : Boolean;
Var
 I, NumClients : Integer;
 ContextsList  : TList;
Begin
 Result := False;
 {
 If vipTCPObject is TipwIPDaemon Then
  Begin
   Try
    If TipwIPDaemon(vipTCPObject).Listening Then
     Begin
      If TipwIPDaemon(vipTCPObject).Connected[ConnectionId] Then
       TipwIPDaemon(vipTCPObject).Disconnect(ConnectionId);
      Result := True;
     End;
   Except

   End;
  End
 Else If vipTCPObject is TipwIPDaemon Then
  Begin
   ContextsList := TIdTCPServer(vipTCPObject).Contexts.LockList;
   With ContextsList Do
    Begin
     Try
      NumClients := Count;
      For I := 0 To NumClients -1 Do
       Begin
        If TIdContext(ContextsList[I]).Binding.Handle = ConnectionId Then
         Begin
          Try
           TIdContext(ContextsList[I]).Binding.CloseSocket;
           Result := True;
          Except

          End;
          Break;
         End;
       End;
     Finally
      TIdTCPServer(vipTCPObject).Contexts.UnlockList;
     End;
    End;
  End;
  }
End;

Function TipPoolerService.ReadAll : String;
Begin
 Result := '';
 While HasBuffer(-1) > 0 Do
  Result := Result + ReadBuffer(-1);
End;

Function TipPoolerService.ReadAll(ConnectionId  : Integer = -1) : String;
Begin
 Result := '';
 While HasBuffer(ConnectionId) > 0 Do
  Result := Result + ReadBuffer(ConnectionId);
End;

Procedure TipPoolerService.Read(ConnectionId : Integer;
                                Var Data : TMemoryStream);
Begin
 StringToMemoryStream(Data, ReadLn(ConnectionId));
End;

Procedure TipPoolerService.Read(Var Data : TMemoryStream);
Begin
 StringToMemoryStream(Data, ReadLn(-1));
End;

Function  TipPoolerService.ReadLn : String;
Begin
 Result := ReadLn(-1);
End;

Destructor  TipPoolerObject.Destroy;
Var
 I     : Integer;
 vList : TList;
Begin
 If vUdpListBindings <> Nil Then
  Begin
   vList := vUdpListBindings.LockList;
   Try
    For I := vList.Count -1 Downto 0 Do
     Begin
      TClientRect(vList.Items[I]).Free;
      vList.Delete(I);
     End;
   Finally
    vUdpListBindings.UnlockList;
    FreeAndNil(vUdpListBindings);
   End;
  End;
 Inherited;
End;

Constructor TipPoolerObject.Create(Sender : TObject);
Begin
 aParent := Sender;
 vUdpListBindings := TDataBuffer.Create;
End;

Procedure TipPoolerObject.Execute(AContext : TIdContext);
Var
 aBuf : TIdBytes;
 vTempString  : String;
 vEOF         : Boolean;
Begin
 Try
  AContext.Connection.IOHandler.CheckForDisconnect;
 Except
  Exit;
 End;
 AContext.Connection.IOHandler.ReadBytes(aBuf, -1);
 vTempString := BytesToString(aBuf);// Text;
 If Pos(vEOB, vTempString) > 0 Then
  vTempString := StringReplace(vTempString, vEOB, '', [rfReplaceAll]);
 If TipPoolerService(aParent).EncryptData Then
  vEOF         := (Pos(TipPoolerService(aParent).vEOC, HexToString(vTempString)) > 0)
 Else
  vEOF         := (Pos(TipPoolerService(aParent).vEOC, vTempString) > 0);
 If vTempString <> '' Then
  Begin
   AddBuffer(AContext.Binding.Handle, vTempString, False);
   Try
    If vEOF Then
     Begin
      If Assigned(TipPoolerService(aParent).vDataInServer) Then
       TipPoolerService(aParent).vDataInServer(AContext.Binding.Handle);
     End;
   Except
   End;
  End;
End;

Function TipPoolerObject.UDPConnectString(Value : String; ABinding: TIdSocketHandle) : Boolean;
Var
 aClient : TClientRect;
Begin
 Result := False;
 If TipPoolerService(aParent).vipTCPObject is TIdUDPServer Then
  Begin
   If Pos(ConnectString, Value) > 0 Then
    Begin
     Result                 := True;
     aClient                := TClientRect.Create;
     aClient.PeerIP         := ABinding.PeerIP;
     aClient.UDPPort        := ABinding.PeerPort;
     aClient.Handle         := ABinding.Handle;
     Value                  := StringReplace(Value, ConnectString, '', [rfReplaceAll]);
     aClient.WelcomeMessage := Copy(Value, 1, Pos('<|>', Value) -1);
     Delete(Value, 1, Pos('<|>', Value) +2);
     aClient.LocalIP        := Copy(Value, 1, Pos('<|>', Value) -1);
     Delete(Value, 1, Pos('<|>', Value) +2);
     aClient.Context        := Nil;
     aClient.LastAction     := Now;
     vUdpListBindings.Add(aClient);
     If Assigned(TipPoolerService(aParent).vReadyToSendServer) Then
      TipPoolerService(aParent).vReadyToSendServer(ABinding.Handle);
    End;
  End;
End;

Function TipPoolerObject.UDPWelcomeMessage(ConnectionId : Integer) : String;
Var
 I     : Integer;
 vList : TList;
Begin
 Result := '';
 vList := vUdpListBindings.LockList;
 Try
  For I := vList.Count -1 Downto 0 Do
   Begin
    If ConnectionId = TClientRect(vList.Items[I]).Handle Then
     Begin
      Result := TClientRect(vList.Items[I]).WelcomeMessage;
      Break;
     End;
   End;
 Finally
  vUdpListBindings.UnlockList;
 End;
End;

Procedure TipPoolerObject.UDPUpdateLastAction(ABinding: TIdSocketHandle);
Var
 I     : Integer;
 vList : TList;
Begin
 vList := vUdpListBindings.LockList;
 Try
  For I := vList.Count -1 Downto 0 Do
   Begin
    If ABinding.Handle = TClientRect(vList.Items[I]).Handle Then
     Begin
      TClientRect(vList.Items[I]).LastAction := Now;
      Break;
     End;
   End;
 Finally
  vUdpListBindings.UnlockList;
 End;
End;

Function TipPoolerObject.UDPCheckPortString(Value : String; ABinding: TIdSocketHandle) : Boolean;
Var
 I     : Integer;
 vList : TList;
Begin
 Result := False;
 If Pos(CheckPortString, Value) > 0 Then
  Begin
   vList := vUdpListBindings.LockList;
   Try
    For I := vList.Count -1 Downto 0 Do
     Begin
      If ABinding.Handle = TClientRect(vList.Items[I]).Handle Then
       Begin
        UDPUpdateLastAction(ABinding);
        Break;
       End;
     End;
   Finally
    vUdpListBindings.UnlockList;
   End;
  End;
End;

Function TipPoolerObject.UDPDisconnectString(Value : String; ABinding: TIdSocketHandle) : Boolean;
Var
 I     : Integer;
 vList : TList;
Begin
 Result := False;
 If Pos(DisconnectString, Value) > 0 Then
  Begin
   vList := vUdpListBindings.LockList;
   Try
    For I := vList.Count -1 Downto 0 Do
     Begin
      If ABinding.Handle = TClientRect(vList.Items[I]).Handle Then
       Begin
        TClientRect(vList.Items[I]).Free;
        vList.Delete(I);
        Result := True;
        Break;
       End;
     End;
   Finally
    vUdpListBindings.UnlockList;
   End;
  End;
End;

Procedure TipPoolerObject.UDPRead(AThread: TIdUDPListenerThread;
                                  Const AData: TIdBytes;
                                  ABinding: TIdSocketHandle);
Var
 vTempString  : String;
 vEOF         : Boolean;
Begin
 vTempString := WideToStr(AData, TipPoolerService(aParent).BufferDownload);// Text;
 If UDPConnectString(vTempString, ABinding)    Or
    UDPDisconnectString(vTempString, ABinding) Or
    UDPCheckPortString(vTempString, ABinding)  Then
  Exit;
 UDPUpdateLastAction(ABinding);
 If Pos(vEOB, vTempString) > 0 Then
  vTempString := StringReplace(vTempString, vEOB, '', [rfReplaceAll]);
 If TipPoolerService(aParent).EncryptData Then
  vEOF         := (Pos(TipPoolerService(aParent).vEOC, HexToString(vTempString)) > 0)
 Else
  vEOF         := (Pos(TipPoolerService(aParent).vEOC, vTempString) > 0);
 If vTempString <> '' Then
  Begin
   If (AnsiLowerCase(TipPoolerService(aParent).vHost) <> '127.0.0.1') And
      (AnsiLowerCase(TipPoolerService(aParent).vHost) <> 'localhost') Then
    AddBuffer(-1, vTempString, False)
   Else
    AddBuffer(ABinding.Handle, vTempString, False);
   Try
    If vEOF Then
     Begin
      If (AnsiLowerCase(TipPoolerService(aParent).vHost) <> '127.0.0.1') And
         (AnsiLowerCase(TipPoolerService(aParent).vHost) <> 'localhost') Then
       Begin
        If Assigned(TipPoolerService(aParent).vDataInClient) Then
         TipPoolerService(aParent).vDataInClient;
       End
      Else
       If Assigned(TipPoolerService(aParent).vDataInServer) Then
        TipPoolerService(aParent).vDataInServer(ABinding.Handle);
     End;
   Except
   End;
  End;
End;

Procedure TipPoolerObject.ReadyToSendClient(Sender: TObject);
Begin
{
 If Sender is TipwIPPort Then
  Begin
   If Assigned(TipPoolerService(aParent).vReadyToSendClient) Then
    TipPoolerService(aParent).vReadyToSendClient;
   TipwIPPort(Sender).EOLB := vEOB;
   if TipPoolerService(aParent).vLineBreak <> #0 then
    TipwIPPort(Sender).EOL := TipPoolerService(aParent).vLineBreak
   Else
    TipwIPPort(Sender).EOL := '';
  End;
}
End;

procedure TipPoolerObject.TCPServerConnect(AContext: TIdContext);
Var
 aClient : TClientRect;
Begin
 If TipPoolerService(aParent).vipTCPObject is TIdTCPServer Then
  Begin
   aClient           := TClientRect.Create;
   aClient.LocalIP   := '';
   aClient.PeerIP    := AContext.Binding.PeerIP;
   aClient.UDPPort   := -1;
   aClient.Handle    := AContext.Binding.Handle;
   aClient.Context   := AContext;
   AContext.Data     := aClient;
   If Assigned(TipPoolerService(aParent).vReadyToSendServer) Then
    TipPoolerService(aParent).vReadyToSendServer(AContext.Binding.Handle);
  End;
End;

Procedure TipPoolerObject.ReadyToSendServer(Sender: TObject;
                                            ConnectionId : Integer);
Begin
 {
 If Sender is TipwIPDaemon Then
  Begin
   TipwIPDaemon(Sender).EOLB[ConnectionId] := vEOB;
   if TipPoolerService(aParent).vLineBreak <> #0  then
    TipwIPDaemon(Sender).EOL[ConnectionId] := TipPoolerService(aParent).vLineBreak
   Else
    TipwIPDaemon(Sender).EOL[ConnectionId] := '';
   If Assigned(TipPoolerService(aParent).vReadyToSendServer) Then
    TipPoolerService(aParent).vReadyToSendServer(ConnectionId);
  End;
  }
End;

end.
