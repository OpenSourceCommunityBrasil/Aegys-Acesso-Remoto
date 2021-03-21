unit uUDPPooler;

interface

uses
  SysUtils,Classes,Graphics,Controls,Forms,Vcl.Dialogs,SimpleTimer,
  IdBuffer,IdComponent,IdStackConsts,
  IdIPWatch,IdHTTP,IdContext,IdServerIOHandlerSocket,IdUDPBase,IdUDPServer,
  IdGlobal,IdSocketHandle,IdUDPClient,
  Code64, System.Generics.Defaults,Vcl.ExtCtrls,IdHashMessageDigest,
  uDWJSONInterface, System.Variants,
 {$IF CompilerVersion >= 24} //XE3 or higher
  Winapi.Windows,System.Generics.Collections,System.SyncObjs,System.ZLib,
  System.DateUtils;
 {$ELSE}
  Windows,DateUtils;
 {$IFEND}

Const
 vEOB             = '<$EOLB$>';
 ConnectString    = '<$CONNECTFVSYSTEM$>';
 DisconnectString = '<$DISCONNECTFVSYSTEM$>';
 CheckPortString  = '<$CHECKPORTDISCONNECTFVSYSTEM$>';
 udpBuffer        = 1024;
 mtDefault        = 1001;

Type
 TPackString = String[32];
 TIpString   = String[40];

Type
 TBroadcastPeer = Packed Record
  ip,
  localIp : TIpString;
  Port    : Integer;
End;

Type
 TClientRect = Class
  PeerIP,
  localIp        : TIpString;
  WelcomeMessage : String;
  ConnectionID   : TPackString;
  UDPPort        : Integer;
  LastAction     : TDatetime;
  Context        : TIdContext;
End;

Type
 TByteArr    = Array [1..udpBuffer] Of Char;
 TValidation = Array of Byte;
 TConnectionStatus = (csWaitConnect,  csConnected,      csDisconnected, csOnTransaction,
                      csError,        csConnectionKick, csDataIn,       csDataOut);
 TOperationMode    = (omServer,       omP2PClient,      omClient);
 TUDPErrors        = (emUDPException, emUDPConnectionException);
 TPackType         = (ptSend = 1001,  ptBack = 1002, ptInData = 1003, ptDownload = 2001, ptUpload = 2002);
 TBroadcastPeers   = TThreadList<TBroadcastPeer>;
 TDataBuffer       = TThreadList;
 TMessageTypes     = Array of Word;

Type
 PClientDataBuffer = ^TClientDataBuffer;
 TClientDataBuffer = Packed Record
  ConnectionID,
  PackID       : TPackString;
  Data         : String;
  MessageType  : Word;
  DataLength   : Integer;
  Readed,
  EOF          : Boolean;
End;

Type
 TTransportPack = Packed Record
  PackSize : Longint;
  buffer   : TIdBytes;
End;

Type
  //Eventos da Minha Classe de UDP
  TDataInClient           = Procedure                                          Of Object;
  TDataInServer           = Procedure(ConnectionID          : TPackString)     Of Object;
  TErrorConnection        = Procedure(ErrorCode             : Integer;
                                      Const Description     : String)          Of Object;
  //TErrorClientConnection  = Procedure(ErrorCode              : Integer;
  //Const Description      : String)          Of Object;
  TTimeoutErrorPack       = Procedure(PackID,
                                      ConnectionId          : TPackString;
                                      ErrorCode             : Integer;
                                      Const Description     : String)          Of Object;
  TTimeoutErrorClientPack = Procedure(PackID                : TPackString;
                                      ErrorCode             : Integer;
                                      Const Description     : String)          Of Object;
  TStatusConnection       = Procedure(StatusConnection      : TConnectionStatus;
                                      ConnectionID          : TPackString;
                                      ErrorCode             : Integer;
                                      Const Description     : String)          Of Object;
  TClientStatusConnection = Procedure(StatusConnection      : TConnectionStatus;
                                      ErrorCode             : Integer;
                                      Const Description     : String)          Of Object;
  TAfterConnected         = Procedure(ConnectionID          : TPackString)     Of Object;
  TAfterClientConnected   = Procedure                                          Of Object;
  //Eventos de Classes Terceiras
  TSocketUDPRead          = Procedure(AThread               : TIdUDPListenerThread;
                                      Const AData           : TIdBytes;
                                      ABinding              : TIdSocketHandle) Of Object;
  TSocketUDPException     = Procedure(AThread               : TIdUDPListenerThread;
                                      ABinding              : TIdSocketHandle;
                                      Const AMessage        : String;
                                      Const AExceptionClass : TClass)          Of Object;
  TExecuteFunction        = Reference To Function(ConnectionID : TPackString;
                                                  Wait         : Boolean = True;
                                                  InitBuffer   : String  = '') :String;

Type
 TAWSInfo = Packed Record
  ip,
  hostname,
  city,
  region,
  country,
  loc,
  org      : String;
End;

Type
 TPackResult   = Packed Record
  MessageType  : Word;
  Data         : String;
End;

Type
 TListPacks = TList<TPackResult>;

Type
 TDataPack = Packed Record
  PackID,
  ConnectionID  : TPackString;
  InternetIP,
  LocalIP       : TIpString;
  LocalPort,
  Handle        : Integer;
  PackType      : TPackType;
  PackCommand,
  PackParts,
  PackCount,
  TotalSize,
  BufferSize    : Word;
  PieceBuf      : TByteArr;
  TimePack      : TDatetime;
End;

Type
 TBufferData = Class
 Private
  PackID,
  ConnectionID : TPackString;
  IpDest       : String;
  PortDest     : Word;
  PackType     : TPackType;
  PackCommand,
  PackParts,
  PackCount,
  BufferSize,
  TotalSize    : Word;
  Validation   : TValidation;
  buffer       : TStringStream;
  TimePack     : TDatetime;
  Tries        : Integer;
  Function GetResult : String;
 Public
  Constructor Create(DataPack : TDataPack; Ip : String; Port : Word);
  Destructor  Destroy;Override;
  Function    GetDataFromBuffer(Position : Integer) : TDataPack;
  Procedure   AddBuffer        (DataPack : TDataPack; Ip : String; Port : Word);
  Function    BufferComplete      : Boolean;
  Function    GetUnsendIndex      : Integer;
  Function    GetBufferPackReader : TDataPack;
  Property    Result              : String Read GetResult;
End;

Type
 TBufferDataThread = TThreadList<TBufferData>;
 TBufferDataList   = TList<TBufferData>;

Type
 PBufferDataset = ^TBufferDataset;
 TBufferDataset = Class
 Private
  vCriticalSection : TCriticalSection;
  vBuffer,
  vBufferReply     : TBufferDataList;
  Function BufferExists    (PackID       : TPackString;
                            Var BufferID : Integer;
                            IdPack : Integer = -1)            : Boolean;
  Function DeCompressStream(Const SrcStream  : TMemoryStream;
                            Var   DestStream : TMemoryStream) : Boolean;
  Function CompressStream  (Const SrcStream  : TMemoryStream;
                            DestStream       : TMemoryStream) : Boolean;
  Procedure Lock;
  Procedure Unlock;
 Public
  Constructor Create;
  Destructor  Destroy;Override;
  Function    ResultBuffer     (PackID        : TPackString) : String;
  Procedure   AddBuffer        (DataPack      : TDataPack;
                                Ip            : String;
                                Port          : Word;
                                BufferParts   : Boolean = False);
  Function    DeleteBuffer     (DataPack      : TDataPack) : Boolean;
  Function    DeleteBufferReply(DataPack : TDataPack) : Boolean;
  Procedure   BufferToStream   (Const ARecord : TDataPack;
                                Var   AStream : TStringStream);
  Procedure   StrToBuffer      (Var   ARecord : TDataPack;
                                Const AStream : String);
  Property    BufferReply : TBufferDataList Read vBufferReply;
End;

Type
 TListenThread = Class(TSimpleTimer)
 Private
  FTerminateEvent         : TEvent;
  InExecute               : Boolean;
  vCriticalSection        : TCriticalSection;
  vComunicationDataCenter : TComponent;
  vConnectionID           : TPackString;
  vHost                   : String;
  vMyHandle,
  vBufferSize,
  vPort                   : Integer;
  vActive                 : Boolean;
  vOperationMode          : TOperationMode;
  vIpVersion              : TIdIPVersion;
  vUDPServer              : TIdUDPServer;
  vUDPClient              : TIdUDPClient;
  vWelcomeMessage         : String;
  Procedure ClearAllBuffers;
  Procedure DataOut;
  Procedure DataIn;
  Procedure Lock;
  Procedure Unlock;
  Procedure StopListen;
  Procedure SetActive(Value  : Boolean);
 Protected
  Procedure Execute  (Sender : TObject);
 Public
  Constructor Create;
  Destructor  Destroy;Override;
  Property    Active                 : Boolean        Read vActive                 Write SetActive;
  Property    OperationMode          : TOperationMode Read vOperationMode          Write vOperationMode;
  Property    ComunicationDataCenter : TComponent     Read vComunicationDataCenter Write vComunicationDataCenter;
  Property    Host                   : String         Read vHost                   Write vHost;
  Property    Port                   : Integer        Read vPort                   Write vPort;
  Property    WelcomeMessage         : String         Read vWelcomeMessage         Write vWelcomeMessage;
End;

Type
 TipPoolerObject = Class(TObject)//Class(TInterfacedObject, ipClient)
 Private
  aParent          : TObject;
  vUdpListBindings : TDataBuffer;
  Procedure ReplyPack(PackID,
                      ConnectionID  : TPackString;
                      Ip            : String;
                      Port,
                      BufferPart    : Integer);
  Procedure AddBuffer(ConnectionID  : TPackString;
                      Text          : String;
                      DataEncrypted : Boolean     = False;
                      MessageType   : Word        = mtDefault;
                      PackID        : TPackString = '');
  {
   Function StrToWide(Const strin : String;
   Var FUDPReq : TDataPack) : tidBytes;
  }
  Function WideToStr(Var FUDPReq     : TDataPack;
                     Buffer          : TBufferDataset;
                     Var PackID      : TPackString;
                     Var MessageType : Word;
                     ConnectionID    : TPackString;
                     Ip              : String;
                     Port            : Integer) : String;
 Public
  Constructor Create(Sender : TObject);
  Destructor  Destroy;Override;
  Function    UDPConnectString    (Value                 : String;
                                   ConnectionID          : TPackString;
                                   ip                    : String;
                                   Port                  : Integer) : Boolean;
  Function    UDPDisconnectString (Value                 : String;
                                   ConnectionID          : TPackString)     : Boolean;
  Procedure   UDPRead             (AThread               : TIdUDPListenerThread;
                                   Const AData           : TIdBytes;
                                   ABinding              : TIdSocketHandle);
  Procedure   UDPException        (AThread               : TIdUDPListenerThread;
                                   ABinding              : TIdSocketHandle;
                                   Const AMessage        : String;
                                   Const AExceptionClass : TClass);
  Function    UDPCheckPortString  (Value                 : String;
                                   ConnectionID          : TPackString)     : Boolean;
  Function    UDPWelcomeMessage   (ConnectionID          : TPackString)     : String;
  Procedure   UDPUpdateLastAction (ConnectionID          : TPackString);
  Function    ClientDetails       (ConnectionID          : TPackString;
                                   Var Found             : Boolean)         : TClientRect;
End;

Type
 PComunicationDataCenter = ^TComunicationDataCenter;
 TComunicationDataCenter = Class(TComponent)
 Private
  vGetAWSInfo,
  vActiveComponent        : Boolean;
  vEOC,
  vInternetIP,
  vLocalIP,
  vWelcomeMessage,
  vHost                   : String;
  vConnectionID           : TPackString;
  vConnections,
  vPort                   : Integer;
  vBufferSize,
  vTimeOut                : Longint;
  vIpVersion              : TIdIPVersion;
  vAWSInfo                : TAWSInfo;
  vDataBuffer             : TDataBuffer;
  vBufferDataset          : TBufferDataset;
  vBufferReplyCopy        : TBufferDataList;
  vBroadcastPeers         : TBroadcastPeers;
  vOperationMode          : TOperationMode;
  vDataInClient           : TDataInClient;
  vDataInServer           : TDataInServer;
  vErrorConnection        : TErrorConnection;
  //vErrorClientConnection  : TErrorClientConnection;
  vTimeoutErrorPack       : TTimeoutErrorPack;
  vTimeoutErrorClientPack : TTimeoutErrorClientPack;
  vStatusConnection       : TStatusConnection;
  vClientStatusConnection : TClientStatusConnection;
  vAfterConnected         : TAfterConnected;
  vAfterClientConnected   : TAfterClientConnected;
  SimpleEvent             : TLightWeightEvent;
  ClientObject            : TipPoolerObject;
  vListenThread           : TListenThread;
  Procedure SetActive    (Value              : Boolean);
  Procedure SetBufferSize(Value              : Longint);
  Procedure SetInternetIP(Value              : String);
  Function  CurrentBuffer(ConnectionID       : TPackString;
                          Wait               : Boolean = False;
                          InitBuffer         : String  = '') : String;
  Procedure StartActions(Const ShutdownEvent : TLightWeightEvent;
                         WaitFor             : LongWord;
                         Value               : TExecuteFunction;
                         ConnectionID        : TPackString;
                         Var Result          : String);
 Public
  Constructor Create          (AOwner        : TComponent);Override;//Cria o Componente
  Destructor  Destroy;Override;
  Procedure ReleaseCacheAction(ConnectionID  : TPackString);
  Procedure ClearPeers;
  Procedure AddPeer           (Value         : TBroadcastPeer);
  Procedure DeletePeer        (Index         : Integer);
  Function  CheckConnect      (ConnectionID  : TPackString   = '-1') : Boolean;//-1 = Client
  Function  ClientDisconnect  (ConnectionID  : TPackString   = '-1') : Boolean;//-1 = Client
  Function  HasBuffer         (ConnectionID  : TPackString   = '-1') : Integer;//-1 = Client
  Function  ReadAll           (ConnectionID  : TPackString   = '-1') : String;
  Function  Read              (ConnectionID  : TPackString   = '-1') : String;
  Function  ReadLn            (ConnectionID  : TPackString   = '-1') : String;Overload;
  Function  ReadLn                                                   : String;Overload;
  Procedure ReadBufferPacks   (Var ListPacks : TListPacks;
                               ConnectionID  : TPackString   = '-1';
                               MessageTypes  : TMessageTypes = []);
  Function  ReadBuffer        (ConnectionID  : TPackString   = '';
                               BreakPoint    : Boolean       = True) : String;
  Function  DataToSend        (ConnectionID  : TPackString;
                               Data          : String)       :Boolean;Overload;
  Function  DataToSend        (Data          : String)       : Boolean;Overload;
  Function  Write             (ConnectionID  : TPackString;
                               Data          : String;
                               MessageType   : Word    = mtDefault;
                               ip            : String  = '';
                               Port          : Integer = -1) : Boolean;Overload;
  Function  Write             (Data          : String;
                               MessageType   : Word    = mtDefault;
                               ip            : String  = '';
                               Port          : Integer = -1) : Boolean;Overload;
  Function  Write             (ConnectionID  : TPackString;
                               Data          : TMemoryStream;
                               MessageType   : Word    = mtDefault;
                               ip            : String  = '';
                               Port          : Integer = -1) : Boolean;Overload;
  Function  Write             (Data          : TMemoryStream;
                               MessageType   : Word    = mtDefault;
                               ip            : String  = '';
                               Port          : Integer = -1) : Boolean;Overload;
  Procedure  SetWelcomeMessage(Value         : String);
  Function   ClientDetails    (ConnectionID  : TPackString;
                               Var Found     : Boolean) : TClientRect;
 Published
  Property   Active                   : Boolean                 Read vActiveComponent        Write SetActive;
  Property   Host                     : String                  Read vHost                   Write vHost;
  Property   Port                     : Integer                 Read vPort                   Write vPort;
  Property   TimeOut                  : Longint                 Read vTimeOut                Write vTimeOut;
  Property   BufferSize               : Longint                 Read vBufferSize             Write SetBufferSize;
  Property   ConnectionsCount         : Integer                 Read vConnections;
  Property   WelcomeMessage           : String                  Read vWelcomeMessage         Write SetWelcomeMessage;
  Property   BroadcastPeers           : TBroadcastPeers         Read vBroadcastPeers;
  Property   OperationMode            : TOperationMode          Read vOperationMode          Write vOperationMode;
  Property   AWSInfo                  : TAWSInfo                Read vAWSInfo;
  Property   InternetIP               : String                  Read vInternetIP             Write SetInternetIP;
  Property   GetAWSInfo               : Boolean                 Read vGetAWSInfo             Write vGetAWSInfo;
  Property   ConnectionID             : TPackString             Read vConnectionID;
  Property   OnClientDataIn           : TDataInClient           Read vDataInClient           Write vDataInClient;
  Property   OnServerDataIn           : TDataInServer           Read vDataInServer           Write vDataInServer;
  //Property OnClientConnectionError  : TErrorClientConnection  Read vErrorClientConnection  Write vErrorClientConnection;
  Property   OnConnectionError        : TErrorConnection        Read vErrorConnection        Write vErrorConnection;
  Property   OnClientStatusConnection : TClientStatusConnection Read vClientStatusConnection Write vClientStatusConnection;
  Property   OnServerStatusConnection : TStatusConnection       Read vStatusConnection       Write vStatusConnection;
  Property   OnServerAfterConnected   : TAfterConnected         Read vAfterConnected         Write vAfterConnected;
  Property   OnAfterClientConnected   : TAfterClientConnected   Read vAfterClientConnected   Write vAfterClientConnected;
End;

Function StrToWide(Const strin : String; Var FUDPReq : TDataPack) : TIdBytes;

Var
 vListenThread : TListenThread;

implementation

Procedure StrToBufferDataPack(Const strin : String;
                              Var FUDPReq : TDataPack);
 Procedure StrToByte(Const Value : String; Var Dest : TByteArr);
 Var
  I : Integer;
 Begin
  FillChar(Dest[1], Length(Dest) * sizeof(Char), #0);
  For I := 1 To Length(Value) Do
   Dest[I] := Value[I];
 End;
Begin
 StrToByte(strin, FUDPReq.PieceBuf);
 FUDPReq.BufferSize := Length(strin);
 FUDPReq.TimePack   := Now;
End;

Function IsTrue(Value : Array of Byte) : Boolean;
Var
 I : Integer;
Begin
 Result := False;
 For I  := 0 To Length(Value) -1 Do
  Begin
   Result := Value[I] = 1;
   If Not Result Then
    Break;
  End;
End;

Function GetUnsendIndex(Value : Array of Byte) : Integer;
Var
 I : Integer;
Begin
 Result := -1;
 For I  := 0 To Length(Value) -1 Do
  Begin
   If Value[I] = 0 Then
    Begin
     Result := I;
     Break;
    End;
  End;
End;

Function GenBufferID : TPackString;
Var
 StartTime : Cardinal;
 Function GeraChave(Tamanho : Integer) : String;
 Var
  I     : Integer;
  Chave : String;
 Const
  str   = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
 Begin
  Chave  := '';
  For I  := 1 To Tamanho Do
   Chave := Chave + str[Random(Length(str)) +1];
  Result := Chave;
 End;
 Function MD5(Const Texto : String) : String;
 Var
  idmd5   : TIdHashMessageDigest5;
 Begin
  idmd5   := TIdHashMessageDigest5.Create;
  Try
   Result := idmd5.HashStringAsHex(Texto);
  Finally
   idmd5.Free;
  End;
 End;
Begin
 StartTime := TThread.GetTickCount;
 Result    := Copy(MD5(GeraChave(64) + DateTimeToStr(Now) + IntToStr(StartTime)), 1, SizeOf(TPackString));
End;

Function GetMyAWSInfo : TAWSInfo;
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

Function ByteToString(const Value : TByteArr;
                      Size        : Integer) : String;
Begin
 SetLength(Result, SizeOf(TByteArr));
 SetString(Result, PChar(@Value[1]), Size);
End;

Function GetLocalIP : String;
Var
 IPW : TIdIPWatch;
Begin
 Result := '0.0.0.0';
 IPW := TIdIPWatch.Create(nil);
 Try
  If IPW.localIp <> '' Then
   Result := IPW.localIp;
 Finally
  IPW.Free;
 End;
End;

Function MemoryStreamToString(M : TMemoryStream):String;
Var
 MemStr : TStringStream;
Begin
 MemStr := TStringStream.Create;
 Try
  MemStr.LoadFromStream(M);
 Finally
  Result := MemStr.DataString;
  FreeAndNil(MemStr);
 End;
End;

Procedure StringToMemoryStream(Value : TMemoryStream; str : String);
Var
 MemStr : TStringStream;
Begin
 MemStr := TStringStream.Create(str);
 Try
  MemStr.SaveToStream(Value);
 Finally
  FreeAndNil(MemStr);
 End;
end;

Constructor TBufferData.Create(DataPack : TDataPack; Ip : String; Port : Word);
Begin
 Inherited Create;
 buffer   := TStringStream.Create;
 Tries    := 0;
 IpDest   := ip;
 PortDest := Port;
 AddBuffer(DataPack, IpDest, PortDest);
End;

Destructor TBufferData.Destroy;
Begin
 buffer.SetSize(0);
 buffer.Free;
 Inherited;
End;

Function TBufferData.GetUnsendIndex : Integer;
Var
 I : Integer;
Begin
 Result := -1;
 For I  := 0 To Length(Validation) -1 do
  Begin
   If Validation[I] = 0 Then
    Begin
     Result := I;
     Break;
    End;
  End;
End;

Function TBufferData.BufferComplete : Boolean;
Var
 I : Integer;
Begin
 Result := False;
 For I := 0 To length(Validation) -1 Do
  Begin
   Result := Validation[I] = 1;
   If Not Result Then
    Break;
  End;
End;

Function TBufferData.GetDataFromBuffer(Position:Integer):TDataPack;
 Procedure StrToByte(Const Value : String; Var Dest: TByteArr);
 Var
  I:Integer;
 Begin
  FillChar(Dest[1], length(Dest) * sizeof(Char), #0);
  For I := 1 to Length(Value) Do
   Dest[I]:=Value[I];
 End;
 Procedure ByteToString(Position : Integer;Var PieceBuf : TByteArr);
 Var
  vTempBuffer : String;
 Begin
  buffer.Position := (Position - 1) * udpBuffer;
  vTempBuffer     := buffer.ReadString(BufferSize);
  StrToByte(vTempBuffer, PieceBuf);
 End;
Begin
 Result.PackID       := PackID;
 Result.PackType     := PackType;
 Result.ConnectionID := ConnectionID;
 Result.PackParts    := PackParts;
 Result.PackCount    := PackCount;
 Result.TotalSize    := TotalSize;
 Result.PackCommand  := PackCommand;
 Result.TimePack     := TimePack;
 Result.BufferSize   := BufferSize;
 If (Result.PackType = ptSend) And
    (Result.PackParts > 1) Then
  ByteToString(1, Result.PieceBuf)
 Else
  ByteToString(PackCount, Result.PieceBuf);
End;

Function TBufferData.GetResult : String;
Begin
 Result := '';
 If IsTrue(Validation) Or
   (PackParts = 1) Then
  Result := buffer.DataString;
End;

Function  TBufferData.GetBufferPackReader : TDataPack;
Begin
 Result.PackID       := PackID;
 Result.PackType     := PackType;
 Result.PackParts    := PackParts;
 Result.TotalSize    := TotalSize;
 Result.ConnectionID := ConnectionID;
 Result.PackCommand  := PackCommand;
 Result.PackCount    := PackCount;
 Result.TimePack     := TimePack;
 Result.BufferSize   := BufferSize;
 Result.InternetIP   := '';
 Result.LocalIP      := '';
 Result.LocalPort    := -1;
 Result.TimePack     := TimePack;
 StrToBufferDataPack(Copy(buffer.DataString, 1, BufferSize), Result);
End;

Procedure TBufferData.AddBuffer(DataPack : TDataPack; Ip : String; Port : Word);
Begin
 PackID       := DataPack.PackID;
 PackType     := DataPack.PackType;
 PackParts    := DataPack.PackParts;
 TotalSize    := DataPack.TotalSize;
 ConnectionID := DataPack.ConnectionID;
 PackCommand  := DataPack.PackCommand;
 PackCount    := DataPack.PackCount;
 TimePack     := DataPack.TimePack;
 BufferSize   := DataPack.BufferSize;
 IpDest       := Ip;
 PortDest     := Port;
 If (DataPack.PackParts = 1) Or
    (DataPack.PackType = ptInData) Then
  Begin
   If PackParts <> Length(Validation) Then
    SetLength(Validation, PackParts);
   Try
    If (DataPack.TotalSize <> buffer.Size) Then
     buffer.SetSize(TotalSize);
    buffer.Position := (DataPack.PackCount -1) * udpBuffer;
    buffer.WriteString(ByteToString(DataPack.PieceBuf, DataPack.BufferSize));
    If (DataPack.PackType = ptInData) Then
     Validation[(DataPack.PackCount -1)] := 1;
   Except
   End;
  End
 Else
  Begin
   SetLength(Validation, 1);
   buffer.SetSize(BufferSize);
   buffer.Position := 0;
   buffer.WriteString(ByteToString(DataPack.PieceBuf, DataPack.BufferSize));
  End;
End;

Function TBufferDataset.BufferExists(PackID : TPackString; Var BufferID : Integer; IdPack : Integer = -1) : Boolean;
Var
 I      : Integer;
 vClass : TList<TBufferData>;
Begin
 Result   := False;
 BufferID := -1;
 vClass   := vBuffer;
 Try
  For I := vClass.Count-1 Downto 0 Do
   Begin
    If IdPack = -1 Then
     Result := (vClass[I].PackID = PackID)
    Else
     Result := (vClass[I].PackID = PackID) And (vClass[I].PackCount = IdPack);
    If Result Then
     Begin
      BufferID := I;
      Break;
     End;
   End;
 Finally
  //vBuffer.UnlockList;
 End;
End;

Function TBufferDataset.DeleteBufferReply(DataPack : TDataPack) : Boolean;
Var
 I,
 vPackNo     : Integer;
 vTempPackNo : String;
 vClass      : TList<TBufferData>;
Begin
 Lock;
 Result := False;
 vPackNo    := -1;
 If ((DataPack.PackParts > 1) And
     (DataPack.PackType <> ptInData)) or
     (DataPack.PackType = ptBack) Then
  Begin
   If DataPack.PackType <> ptBack Then
    vPackNo   := DataPack.PackCount
   Else
    Begin
     vTempPackNo := ByteToString(DataPack.PieceBuf, DataPack.BufferSize);
     vTempPackNo := Copy(vTempPackNo, 1, DataPack.BufferSize);
     vPackNo     := StrToInt(vTempPackNo);
    End;
  End;
 vClass := vBufferReply;
 Try
  For I := vClass.Count -1 Downto 0 Do
   Begin
    If vPackNo = -1 Then
     Begin
      If (vClass[I].PackID = DataPack.PackID) Then
       Begin
        vClass[I].Free;
        vClass.Delete(I);
        Result := True;
        Break;
       End;
     End
    Else If (vClass[I].PackID    = DataPack.PackID) And
            (vClass[I].PackCount = vPackNo)         Then
     Begin
      vClass[I].Free;
      vClass.Delete(I);
      Result := True;
      Break;
     End;
   End;
 Finally
  //vBufferReply.UnlockList;
  Unlock;
 End;
End;

Function  TBufferDataset.DeleteBuffer(DataPack : TDataPack) : Boolean;
Var
 I,
 vPackNo     : Integer;
 vTempPackNo : String;
 vClass      : TList<TBufferData>;
Begin
 Lock;
 vPackNo    := -1;
 Result     := False;
 If ((DataPack.PackParts > 1) And
     (DataPack.PackType <> ptInData)) or
     (DataPack.PackType = ptBack) Then
  Begin
   If DataPack.PackType <> ptBack Then
    vPackNo   := DataPack.PackCount
   Else
    Begin
     vTempPackNo := ByteToString(DataPack.PieceBuf, DataPack.BufferSize);
     vTempPackNo := Copy(vTempPackNo, 1, DataPack.BufferSize);
     vPackNo     := StrToInt(vTempPackNo);
    End;
  End;
 vClass := vBuffer;//.LockList;
 Try
  For I := vClass.Count -1 Downto 0 Do
   Begin
    If vPackNo = -1 Then
     Begin
      If (vClass[I].PackID = DataPack.PackID) Then
       Begin
        vClass[I].Free;
        vClass.Delete(I);
        Result  := True;
        Break;
       End;
     End
    Else If (vClass[I].PackID    = DataPack.PackID) And
            (vClass[I].PackCount = vPackNo)         Then
     Begin
      vClass[I].Free;
      vClass.Delete(I);
      Result    := True;
      Break;
     End;
   End;
 Finally
  //vBuffer.UnlockList;
  Unlock;
 End;
End;

Function TBufferDataset.DeCompressStream(Const SrcStream : TMemoryStream;
                                         Var DestStream  : TMemoryStream) : Boolean;
Var
 zipFile : TZDecompressionStream;
Begin
 Result             := False;
 SrcStream.Position := 0;
 zipFile            := TZDecompressionStream.Create(SrcStream);
 Try
  Try
   DestStream.Clear;
   DestStream.CopyFrom(zipFile,0);
   Result:=True;
  Except
  End;
 Finally
  FreeAndNil(zipFile);
  DestStream.Seek(0,soFromBeginning);
 End;
End;

Procedure TBufferDataset.Lock;
Begin
 If Assigned(vCriticalSection) then
  vCriticalSection.Enter;
End;

Procedure TBufferDataset.Unlock;
Begin
 If Assigned(vCriticalSection) Then
  vCriticalSection.Leave;
End;

Function TBufferDataset.CompressStream(Const SrcStream : TMemoryStream; DestStream : TMemoryStream) : Boolean;
Var
 zipFile : TZCompressionStream;
Begin
 Result  := False;
 zipFile := TZCompressionStream.Create(DestStream, zcMax, 15);
 Try
  Try
   SrcStream.Position := 0;
   zipFile.CopyFrom(SrcStream, SrcStream.Size);
   Result             := True;
  Except
  End;
 Finally
  FreeAndNil(zipFile);
 End;
End;

Procedure TBufferDataset.StrToBuffer(Var ARecord : TDataPack; Const AStream : String);
Var
 vTempStringStream : TStringStream;
 //vTempMemoryStream : TMemoryStream;
Begin
 //vTempMemoryStream := TMemoryStream.Create;
 vTempStringStream := TStringStream.Create(AStream);
 Try
  //vTempStringStream.Position := 0;
  //DeCompressStream(vTempStringStream, vTempMemoryStream);
  vTempStringStream.Position:=0;
  vTempStringStream.Read(ARecord, SizeOf(TDataPack));
 Finally
  //vTempMemoryStream.Clear;
  vTempStringStream.Clear;
  //FreeAndNil(vTempMemoryStream);
  FreeAndNil(vTempStringStream);
 End;
End;

Procedure TBufferDataset.BufferToStream(Const ARecord : TDataPack; Var AStream : TStringStream);
{
 Var
 vTempMemoryStream : TMemoryStream;
}
Begin
  {
   vTempMemoryStream := TMemoryStream.Create;
   Try
   vTempMemoryStream.Write(ARecord, SizeOf(TDataPack));
   CompressStream(vTempMemoryStream, AStream);
   Finally
   vTempMemoryStream.Clear;
   FreeAndNil(vTempMemoryStream);
   End;
  }
 AStream.Write(ARecord, sizeof(TDataPack));
 AStream.Position;
End;

Function TBufferDataset.ResultBuffer(PackID : TPackString) : String;
Var
 I      : Integer;
 vClass : TList<TBufferData>;
Begin
 Result := '';
 vClass := vBuffer;//.LockList;
 Try
  For I := vClass.Count -1 Downto 0 Do
   Begin
    If (vClass[I].PackID = PackID) Then
     Begin
      Result := vClass[I].Result;
      Break;
     End;
   End;
 Finally
    //vBuffer.UnlockList;
 End;
End;

Procedure TipPoolerObject.ReplyPack(PackID,
                                    ConnectionID : TPackString;
                                    Ip           : String;
                                    Port,
                                    BufferPart   : Integer);
Var
 BufferDataRep : TBufferData;
 DataPackB     : TDataPack;
 ClientRect    : TClientRect;
 vFound        : Boolean;
Begin
 DataPackB.PackID       := PackID;
 DataPackB.PackType     := ptBack;
 DataPackB.ConnectionId := ConnectionId;
 DataPackB.InternetIP   := '';
 DataPackB.LocalIP      := '';
 DataPackB.LocalPort    := -1;
 DataPackB.Handle       := 0;
 DataPackB.PackParts    := 1;
 DataPackB.PackCount    := 1;
 DataPackB.TotalSize    := 1;
// DataPackB.BufferSize   := Length(IntToStr(BufferPart));
// DataPackB.TimePack     := Now;
 FillChar(DataPackB.PieceBuf, sizeof(TByteArr), #0);
 StrToBufferDataPack(IntToStr(BufferPart), DataPackB);
 BufferDataRep          := TBufferData.Create(DataPackB, Ip, Port);
 Try
  TComunicationDataCenter(aParent).vBufferDataset.vBufferReply.Add(BufferDataRep);
 Finally
 End;
End;

Procedure TBufferDataset.AddBuffer(DataPack    : TDataPack;
                                   Ip          : String;
                                   Port        : Word;
                                   BufferParts : Boolean = False);
Var
 BufferID    : Integer;
 BufferData  : TBufferData;
 vClass      : TList<TBufferData>;
 vPackNo     : Integer;
 vTempPackNo : String;
Begin
 Lock;
 vPackNo    := -1;
 If ((DataPack.PackParts > 1) And (BufferParts)) or
     (DataPack.PackType = ptBack) Then
  Begin
   If DataPack.PackType <> ptBack Then
    vPackNo   := DataPack.PackCount
   Else
    Begin
     vTempPackNo := ByteToString(DataPack.PieceBuf, DataPack.BufferSize);
     vTempPackNo := Copy(vTempPackNo, 1, DataPack.BufferSize);
     vPackNo     := StrToInt(vTempPackNo);
    End;
  End;
 If BufferExists(DataPack.PackID, BufferID, vPackNo) Then
  Begin
   Try
    vClass := vBuffer;//.LockList;
    If DataPack.PackType <> ptBack Then
     vClass[BufferID].AddBuffer(DataPack, ip, Port)
    Else
     Begin
      If vPackNo = -1 Then
       vClass[BufferID].Validation[(DataPack.PackCount -1)] := 1
      Else
       vClass[BufferID].Validation[0] := 1;
     End;
   Finally
    //vBuffer.UnlockList;
   End;
  End
 Else If DataPack.PackType <> ptBack Then
  Begin
   Try
    vClass     := vBuffer;//.LockList;
    BufferData := TBufferData.Create(DataPack, ip, Port);
    BufferID   := vClass.Add(BufferData);
    If DataPack.PackType = ptInData Then
     vClass[BufferID].Validation[(DataPack.PackCount -1)] := 1;
   Finally
     //vBuffer.UnlockList;
   End;
  End;
 Unlock;
End;

Constructor TListenThread.Create;
Begin
 Inherited;
 vCriticalSection := TCriticalSection.Create;
 FTerminateEvent  := TEvent.Create(Nil, True, False, 'ListenThreadEvent');
 InExecute        := False;
 Interval         := 3;
 OnTimer          := Execute;
 vActive          := False;
 vMyHandle        := -1;
 vUDPServer       := Nil;
 vUDPClient       := Nil;
End;

Destructor TListenThread.Destroy;
Begin
 If vCriticalSection <> Nil Then
  Begin
   vCriticalSection.Release;
   vCriticalSection.Leave;
  End;
 If FTerminateEvent  <> Nil Then
  FreeAndNil(FTerminateEvent);
 FreeAndNil(vCriticalSection);
 StopListen;
 If vUDPServer <> Nil Then
  FreeAndNil(vUDPServer);
 If vUDPClient <> Nil Then
  FreeAndNil(vUDPClient);
 Inherited;
End;

Procedure TListenThread.DataIn;
//Var
// ABuffer:TIdBytes;
Begin
{
 Try
  If TComunicationDataCenter(vComunicationDataCenter).vOperationMode = omP2PClient Then
   If TComunicationDataCenter(vComunicationDataCenter).vUDPServer.Active Then
    If TComunicationDataCenter(vComunicationDataCenter).vUDPServer.ReceiveBuffer(ABuffer) > 0 Then
     TComunicationDataCenter(vComunicationDataCenter).ClientObject.UDPRead(Nil, aBuffer,
                                                                           TComunicationDataCenter(vComunicationDataCenter).vUDPServer.Binding);
 Except
 End;
}
End;

Procedure TListenThread.ClearAllBuffers;
Var
 I         : Integer;
 vListLock : TList<TBufferData>;
Begin
 vListLock := TComunicationDataCenter(vComunicationDataCenter).vBufferDataset.vBuffer;
 For I := vListLock.Count -1 Downto 0 Do
  Begin
   vListLock[I].Free;
   vListLock.Delete(I);
   vListLock.Capacity := vListLock.Count;
   vListLock.Pack;
  End;
 vListLock := TComunicationDataCenter(vComunicationDataCenter).vBufferDataset.vBufferReply;
 For I := vListLock.Count -1 Downto 0 Do
  Begin
   vListLock[I].Free;
   vListLock.Delete(I);
   vListLock.Capacity := vListLock.Count;
   vListLock.Pack;
  End;
End;

Procedure TListenThread.DataOut;
Var
 I, A, X     : Integer;
 vBufferOutT : TList<TBufferData>;
 vPeers      : TList<TBroadcastPeer>;
 FUDPReq     : TDataPack;
 vIp         : String;
 vDeleted    : Boolean;
 vUDPClient  : tIdUDPClient;
Begin
 Try
  If vUDPServer = Nil Then
   Begin
    ClearAllBuffers;
    Exit;
   End;
  If Not (vUDPServer.Active) Then
   ClearAllBuffers;
  vBufferOutT := TComunicationDataCenter(vComunicationDataCenter).vBufferDataset.vBufferReply;
  Try
   vUDPClient  := tIdUDPClient.Create(Nil);
   Try
    vUDPClient.Active           := False;
    vUDPClient.BufferSize       := vBufferSize;
    vUDPClient.ReceiveTimeout   := TComunicationDataCenter(vComunicationDataCenter).vTimeOut;
    vUDPClient.BroadcastEnabled := True;
    vUDPClient.Host             := '0.0.0.0';
    vUDPClient.Port             := TComunicationDataCenter(vComunicationDataCenter).vPort;
    vUDPClient.Active           := True;
   Except

   End;
   I := 0;
   While I <= vBufferOutT.Count -1 Do
    Begin
     If Self = Nil Then
      Break;
     vDeleted := False;
     If Not(vBufferOutT[I].BufferComplete) And
           ((vBufferOutT[I].TimePack < Now) Or
           (vBufferOutT[I].Tries     = 0)) Then
      Begin
       vBufferOutT[I].TimePack := Now;
       Inc(vBufferOutT[I].Tries);
       A := vBufferOutT[I].GetUnsendIndex;
       If A >- 1 Then
        Begin
         FUDPReq            := vBufferOutT[I].GetDataFromBuffer(A);
         FUDPReq.InternetIP := TComunicationDataCenter(vComunicationDataCenter).vInternetIP;
         FUDPReq.LocalIP    := TComunicationDataCenter(vComunicationDataCenter).vLocalIP;
         FUDPReq.LocalPort  := vUDPServer.Binding.Port;
         If TComunicationDataCenter(vComunicationDataCenter).vOperationMode = omP2PClient Then
          Begin
           vPeers := TComunicationDataCenter(vComunicationDataCenter).vBroadcastPeers.LockList;
           Try
            If (vPeers.Count > 0) Then
             Begin
              If (Trim(vBufferOutT[I].IpDest) = '') Then
               Begin
                For X := 0 To vPeers.Count -1 Do
                 Begin
                  If TComunicationDataCenter(vComunicationDataCenter).vAWSInfo.ip = vPeers[X].ip Then
                   vIp := vPeers[X].localIp
                  Else
                   vIp := vPeers[X].ip;
                  vUDPClient.Broadcast(RawToBytes(FUDPReq, SizeOf(TDataPack)), vPeers[X].Port, vIp);
//                  vUDPClient.SendBuffer(vIp, vPeers[X].Port, RawToBytes(FUDPReq, SizeOf(TDataPack)));
                  If Assigned(TComunicationDataCenter(vComunicationDataCenter).vClientStatusConnection) Then
                   TComunicationDataCenter(vComunicationDataCenter).vClientStatusConnection(csDataOut,0,'');
                 End;
               End
              Else
               Begin
                vUDPClient.Broadcast(RawToBytes(FUDPReq, SizeOf(TDataPack)), vBufferOutT[I].PortDest, vBufferOutT[I].IpDest);
{
                vUDPClient.SendBuffer(vBufferOutT[I].IpDest,
                                      vBufferOutT[I].PortDest,
                                      RawToBytes(FUDPReq, SizeOf(TDataPack)));
}
                If Assigned(TComunicationDataCenter(vComunicationDataCenter).vClientStatusConnection) Then
                 TComunicationDataCenter(vComunicationDataCenter).vClientStatusConnection(csDataOut,0,'');
               End;
             End
            Else
             Begin
              If Trim(vBufferOutT[I].IpDest) = '' Then
               Begin
                vUDPClient.Broadcast(RawToBytes(FUDPReq, SizeOf(TDataPack)), vPort, vHost);
//                vUDPClient.SendBuffer(vHost, vPort, RawToBytes(FUDPReq, SizeOf(TDataPack)));
                If Assigned(TComunicationDataCenter(vComunicationDataCenter).vClientStatusConnection) Then
                 TComunicationDataCenter(vComunicationDataCenter).vClientStatusConnection(csDataOut, 0, '');
               End
              Else
               Begin
                vUDPClient.Broadcast(RawToBytes(FUDPReq, SizeOf(TDataPack)), vBufferOutT[I].PortDest, vBufferOutT[I].IpDest);
//                vUDPClient.SendBuffer(vBufferOutT[I].IpDest, vBufferOutT[I].PortDest, RawToBytes(FUDPReq, SizeOf(TDataPack)));
                If Assigned(TComunicationDataCenter(vComunicationDataCenter).vClientStatusConnection) Then
                 TComunicationDataCenter(vComunicationDataCenter).vClientStatusConnection(csDataOut, 0, '');
               End;
             End;
           Finally
            TComunicationDataCenter(vComunicationDataCenter).vBroadcastPeers.UnlockList;
           End;
          End
         Else If TComunicationDataCenter(vComunicationDataCenter).vOperationMode = omServer Then
          Begin
           Try
            vUDPServer.Bindings.BeginUpdate;
            If vBufferOutT[I].IpDest = '' Then
             Begin
              For X := vUDPServer.Bindings.Count -1 DownTo 0 Do
               Begin
                If TComunicationDataCenter(vComunicationDataCenter).vAWSInfo.ip =
                   vUDPServer.Bindings[X].PeerIP Then
                 vIp := vBufferOutT[I].IpDest
                Else
                 vIp := vUDPServer.Bindings[X].PeerIP;
                vUDPClient.Broadcast(RawToBytes(FUDPReq, SizeOf(TDataPack)), vUDPServer.Bindings[X].PeerPort, vIp);
                {
                vUDPClient.SendBuffer(vIp,
                                      vUDPServer.Bindings[X].PeerPort,
                                      RawToBytes(FUDPReq, SizeOf(TDataPack)));
                 }
               End;
              If TComunicationDataCenter(vComunicationDataCenter).vBufferDataset.DeleteBufferReply(vBufferOutT[I].GetBufferPackReader) Then
               Begin
                vDeleted := True;
                Continue;
               End
              Else
               Begin
                vBufferOutT[I].Free;
                vBufferOutT.Delete(I);
                vDeleted := True;
                Continue;
               End;
             End
            Else
             Begin
              If TComunicationDataCenter(vComunicationDataCenter).vAWSInfo.ip =
                 vBufferOutT[I].IpDest Then
               vIp := FUDPReq.LocalIP
              Else
               vIp := vBufferOutT[I].IpDest;
              vUDPClient.Broadcast(RawToBytes(FUDPReq, SizeOf(TDataPack)), vBufferOutT[I].PortDest, vIp);
              {
              vUDPClient.SendBuffer(vIp,
                                    vBufferOutT[I].PortDest,
                                    RawToBytes(FUDPReq, SizeOf(TDataPack)));
              }
             End;
           Finally
            vUDPServer.Bindings.EndUpdate;
           End;
          End;
         If FUDPReq.PackType = ptBack Then
          Begin
           If Not vDeleted Then
            Begin
             If TComunicationDataCenter(vComunicationDataCenter).vBufferDataset.DeleteBufferReply(vBufferOutT[I].GetBufferPackReader) Then
              Continue
             Else
              Begin
               vBufferOutT[I].Free;
               vBufferOutT.Delete(I);
               Continue;
              End;
            End;
          End;
        End;
      End
     Else
      Begin
       If Not vDeleted Then
        Begin
         If TComunicationDataCenter(vComunicationDataCenter).vBufferDataset.DeleteBufferReply(vBufferOutT[I].GetBufferPackReader) Then
          Continue
         Else
          Begin
           vBufferOutT[I].Free;
           vBufferOutT.Delete(I);
           Continue;
          End;
        End;
      End;
     Inc(I);
    End;
   Finally
    vUDPClient.Active := False;
    FreeAndNil(vUDPClient);
   End;
 Finally
  //TComunicationDataCenter(vComunicationDataCenter).vBufferDataset.vBufferReply.UnlockList;
 End;
 Try
  vBufferOutT := TComunicationDataCenter(vComunicationDataCenter).vBufferDataset.vBuffer;
  //TComunicationDataCenter(vComunicationDataCenter).vBufferDataset.vBuffer.LockList;
  Try
   vUDPClient  := tIdUDPClient.Create(Nil);
   Try
    vUDPClient.Active           := False;
    vUDPClient.BroadcastEnabled := True;
    vUDPClient.Host             := '0.0.0.0';
    vUDPClient.Port             := TComunicationDataCenter(vComunicationDataCenter).vPort;
    vUDPClient.Active           := True;
   Except

   End;
   I := 0;
   While I <= vBufferOutT.Count -1 Do
    Begin
     If Self = Nil Then
      Break;
     If (vBufferOutT[I].PackType = ptBack)   Or
        (vBufferOutT[I].PackType = ptInData) Then
      Begin
       If (vBufferOutT[I].PackType = ptBack) Then
        Begin
         vBufferOutT[I].Free;
         vBufferOutT.Delete(I);
//         TComunicationDataCenter(vComunicationDataCenter).vBufferDataset.DeleteBuffer(vBufferOutT[I].GetBufferPackReader);
        End
       Else
        Inc(I);
       Continue;
      End;
     If Not(vBufferOutT[I].BufferComplete) And
           ((vBufferOutT[I].TimePack  < Now) or
           (vBufferOutT[I].Tries = 0))     Then
      Begin
       vBufferOutT[I].TimePack := Now;
       Inc(vBufferOutT[I].Tries);
       A                       := vBufferOutT[I].GetUnsendIndex;
       If A > -1 Then
        Begin
         FUDPReq            := vBufferOutT[I].GetDataFromBuffer(A);
         FUDPReq.InternetIP := TComunicationDataCenter(vComunicationDataCenter).vInternetIP;
         FUDPReq.LocalIP    := TComunicationDataCenter(vComunicationDataCenter).vLocalIP;
         FUDPReq.LocalPort  := vUDPServer.Binding.Port;
         If TComunicationDataCenter(vComunicationDataCenter).vOperationMode = omP2PClient Then
          Begin
           vPeers := TComunicationDataCenter(vComunicationDataCenter).vBroadcastPeers.LockList;
           Try
            Try
             If vPeers.Count > 0 Then
              Begin
               If (Trim(vBufferOutT[I].IpDest) = '') Then
                Begin
                 For X := 0 To vPeers.Count -1 Do
                  Begin
                   If TComunicationDataCenter(vComunicationDataCenter).vAWSInfo.ip = vPeers[X].ip Then
                    vIp := vPeers[X].localIp
                   Else
                    vIp := vPeers[X].ip;
                   vUDPClient.Broadcast(RawToBytes(FUDPReq, SizeOf(TDataPack)), vPeers[X].Port, vIp);
                   //vUDPClient.SendBuffer(vIp, vPeers[X].Port, RawToBytes(FUDPReq, SizeOf(TDataPack)));
                   If Assigned(TComunicationDataCenter(vComunicationDataCenter).vClientStatusConnection) Then
                    TComunicationDataCenter(vComunicationDataCenter).vClientStatusConnection(csDataOut, 0, '');
                  End;
                End
               Else
                Begin
                 vUDPClient.Broadcast(RawToBytes(FUDPReq, SizeOf(TDataPack)),
                                      vBufferOutT[I].PortDest,
                                      vBufferOutT[I].IpDest);
{
                 vUDPClient.SendBuffer(vBufferOutT[I].IpDest,
                                       vBufferOutT[I].PortDest,
                                       RawToBytes(FUDPReq, SizeOf(TDataPack)));
}
                 If Assigned(TComunicationDataCenter(vComunicationDataCenter).vClientStatusConnection) Then
                  TComunicationDataCenter(vComunicationDataCenter).vClientStatusConnection(csDataOut,0,'');
                End;
              End
             Else
              Begin
               If Trim(vBufferOutT[I].IpDest) = '' Then
                Begin
                 vUDPClient.Broadcast(RawToBytes(FUDPReq, SizeOf(TDataPack)), vPort, vHost);
//                 vUDPClient.SendBuffer(vHost, vPort, RawToBytes(FUDPReq, SizeOf(TDataPack)));
                 If Assigned(TComunicationDataCenter(vComunicationDataCenter).vClientStatusConnection) Then
                   TComunicationDataCenter(vComunicationDataCenter).vClientStatusConnection(csDataOut, 0, '');
                End
               Else
                Begin
                 vUDPClient.Broadcast(RawToBytes(FUDPReq, SizeOf(TDataPack)), vBufferOutT[I].PortDest, vBufferOutT[I].IpDest);
//                 vUDPClient.SendBuffer(vBufferOutT[I].IpDest, vBufferOutT[I].PortDest, RawToBytes(FUDPReq, SizeOf(TDataPack)));
                 If Assigned(TComunicationDataCenter(vComunicationDataCenter).vClientStatusConnection) Then
                   TComunicationDataCenter(vComunicationDataCenter).vClientStatusConnection(csDataOut, 0, '');
                End;
              End;
            Finally
             //Strm.Clear;
             //FreeAndNil(Strm);
            End;
           Finally
            TComunicationDataCenter(vComunicationDataCenter).vBroadcastPeers.UnlockList;
           End;
          End
         Else If TComunicationDataCenter(vComunicationDataCenter).vOperationMode = omServer Then
          Begin
           vUDPServer.Bindings.BeginUpdate;
           Try
            If vBufferOutT[I].IpDest = '' Then
             Begin
              For X := vUDPServer.Bindings.Count -1 DownTo 0 Do
               Begin
                If TComunicationDataCenter(vComunicationDataCenter).vAWSInfo.ip =
                   vUDPServer.Bindings[X].PeerIP Then
                 vIp := vBufferOutT[I].IpDest
                Else
                 vIp := vUDPServer.Bindings[X].PeerIP;
                vUDPClient.Broadcast(RawToBytes(FUDPReq, SizeOf(TDataPack)),
                                     vUDPServer.Bindings[X].PeerPort, vIp);
{
                vUDPClient.SendBuffer(vIp,
                                      vUDPServer.Bindings[X].PeerPort,
                                      RawToBytes(FUDPReq, SizeOf(TDataPack)));
}
               End;
              If TComunicationDataCenter(vComunicationDataCenter).vBufferDataset.DeleteBufferReply(vBufferOutT[I].GetBufferPackReader) Then
               Begin
                vDeleted := True;
                Continue;
               End
              Else
               Begin
                vBufferOutT[I].Free;
                vBufferOutT.Delete(I);
                vDeleted := True;
                Continue;
               End;
             End
            Else
             Begin
              If TComunicationDataCenter(vComunicationDataCenter).vAWSInfo.ip =
                 vBufferOutT[I].IpDest Then
               vIp := FUDPReq.LocalIP
              Else
               vIp := vBufferOutT[I].IpDest;
              vUDPClient.Broadcast(RawToBytes(FUDPReq, SizeOf(TDataPack)),
                                   vBufferOutT[I].PortDest, vIp);
             {
              vUDPClient.SendBuffer(vIp,
                                    vBufferOutT[I].PortDest,
                                    RawToBytes(FUDPReq, SizeOf(TDataPack)));
             }
             End;
           Finally
            vUDPServer.Bindings.EndUpdate;
           End;
          End;
        End;
      End;
     Inc(I);
    End;
  Finally
   vUDPClient.Active := False;
   FreeAndNil(vUDPClient);
  End;
 Finally
   //TComunicationDataCenter(vComunicationDataCenter).vBufferDataset.vBuffer.UnlockList;
 End;
End;

Procedure TListenThread.SetActive(Value : Boolean);
Var
 FUDPReq : TDataPack;
 Procedure ConnectUDP;
 Begin
  Try
   StopListen;
   Case vOperationMode Of
    omServer,
    omP2PClient         : Begin
                           If vUDPServer     = Nil Then
                            Begin
                             vUDPServer                  := TIdUDPServer.Create(Application);
                             vUDPServer.OnUDPException   := TComunicationDataCenter(vComunicationDataCenter).ClientObject.UDPException;
                             vUDPServer.OnUDPRead        := TComunicationDataCenter(vComunicationDataCenter).ClientObject.UDPRead;
                             vUDPServer.BroadcastEnabled := True;
                             vUDPServer.ThreadedEvent    := False;
                            End;
                           vUDPServer.IPVersion          := vIpVersion;
                           vUDPServer.ReceiveTimeout     := TComunicationDataCenter(vComunicationDataCenter).vTimeOut;
                           If vOperationMode = omP2PClient Then
                            Begin
                             vUDPServer.Bindings.Clear;
                             vUDPServer.DefaultPort := 0;
                            End
                           Else
                            vUDPServer.DefaultPort   := vPort;
                           vUDPServer.ReceiveTimeout := TComunicationDataCenter(vComunicationDataCenter).vTimeOut;
                           If Value Then
                            Begin
                             vUDPServer.BufferSize  := vBufferSize;
                             FUDPReq.PackID         := GenBufferID;
                             FUDPReq.PackType       := ptSend;
                             FUDPReq.PackCommand    := mtDefault;
                             FUDPReq.PackParts      := 1;
                             FUDPReq.PackCount      := 1;
                            End;
                           vUDPServer.Active        := Value;
                           If Value Then
                            Begin
                             vActive                := vUDPServer.Active;
                             If vActive then
                              Begin
                               If vMyHandle = -1 Then
                                vMyHandle           := vUDPServer.Binding.Handle;
                               vConnectionID        := GenBufferID;
                               FUDPReq.ConnectionId := vConnectionID;
                               FUDPReq.InternetIP   := '';
                               FUDPReq.LocalIP      := '';
                               FUDPReq.LocalPort    := -1;
                               TComunicationDataCenter(vComunicationDataCenter).vConnectionID := vConnectionID;
                               If vOperationMode = omP2PClient Then
                                Begin
                                 FUDPReq.TotalSize  := Length(ConnectString + vWelcomeMessage    + '<|>' + GetLocalIP + '<|>');
                                 FUDPReq.BufferSize := FUDPReq.TotalSize;
                                 StrToWide(ConnectString + vWelcomeMessage  + '<|>' + GetLocalIP + '<|>', FUDPReq);
                                 FUDPReq.TimePack   := Now;
                                 TComunicationDataCenter(vComunicationDataCenter).vBufferDataset.AddBuffer(FUDPReq, vHost, vPort);
                                  //vUDPServer.SendBuffer(vHost, vPort, StrToWide(ConnectString + vWelcomeMessage + '<|>' + GetLocalIP + '<|>', FUDPReq));
                                End;
                               If Not Enabled Then
                                Enabled := True;
                              End;
                            End
                           Else
                            vActive := Value;
                          End;
    omClient            :
                          Begin

                          End;
   End;
  Except
   On E : Exception Do
    Begin
     If vOperationMode in [omServer, omP2PClient] Then
      vUDPServer.DefaultPort := vUDPServer.DefaultPort + 1;
     If Assigned(TComunicationDataCenter(vComunicationDataCenter).vErrorConnection) Then
      TComunicationDataCenter(vComunicationDataCenter).vErrorConnection(5001, E.Message);
     Exit;
    End;
  End;
 End;
Begin
  Case vOperationMode Of
    omServer,
    omP2PClient : Begin
                   StopListen;
                   If Value Then
                    ConnectUDP;
                   If vUDPServer <> Nil Then
                    vActive := vUDPServer.Active
                   Else
                    vActive := False;
                   Enabled  := vActive;
                  End;
    omClient    : Begin
                   StopListen;
                   If Value Then
                    Begin
                     If vUDPClient = Nil Then
                      Begin
                       vUDPClient          := TIdUDPClient.Create(Nil);
                        //vListenThread.ConnectionObject := vUDPClient;
                      End;
                     vUDPClient.BufferSize := vBufferSize;
                    End;
                   vActive                 := vUDPClient.Active;
                  End;
  End;
End;

Procedure TListenThread.Lock;
Begin
 If Assigned(vCriticalSection) then
  vCriticalSection.Enter;
End;

Procedure TListenThread.Unlock;
Begin
 If Assigned(vCriticalSection) Then
  vCriticalSection.Leave;
End;

Procedure TListenThread.Execute(Sender : TObject);
 Function ActiveControls : Boolean;
 Begin
  Result := (vUDPServer <> Nil) Or
            (vUDPClient <> Nil);
  If Result Then
   Begin
    If (vUDPServer <> Nil)      Then
     Result := vUDPServer.Active
    Else If (vUDPClient <> Nil) Then
     Result := vUDPClient.Active;
   End;
 End;
Begin
 Enabled := False;
 If Not InExecute Then
  Begin
   InExecute := True;
   Try
    Lock;
    Try
     If ActiveControls Then
      Begin
       DataIn;
       DataOut;
       Application.ProcessMessages;
      End;
    Except
    End;
   Finally
    Unlock;
   End;
   InExecute := False;
  End;
 Enabled := ActiveControls;
End;

Function TComunicationDataCenter.ReadBuffer(ConnectionID : TPackString = '';
                                            BreakPoint   : Boolean = True) : String;
Var
 I         : Integer;
 BreakLine : Boolean;
 vTempData : String;
 vListLock : TList;
Begin
 Result    := '';
 I         := 0;
 BreakLine := False;
 If ConnectionID = '-1' then
  ConnectionID := vConnectionID;
 vListLock := vDataBuffer.LockList;
 Try
  While I <= vListLock.Count -1 Do
   Begin
    If (TClientDataBuffer(vListLock[I]^).ConnectionId = ConnectionId) And
        Not(TClientDataBuffer(vListLock[I]^).Readed)                  Then
     Begin
      vTempData                               := vTempData+TClientDataBuffer(vListLock[I]^).Data;
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
 Finally
  vDataBuffer.UnlockList;
 End;
End;

Procedure TComunicationDataCenter.SetInternetIP(Value : String);
Begin
 vLocalIP := GetLocalIP;
 If Trim(Value) <> '' Then
  Begin
   vInternetIP  := Trim(Value);
   vAWSInfo.ip  := vInternetIP;
  End
 Else
  vAWSInfo      := GetMyAWSInfo;
 vInternetIP := vAWSInfo.ip;
End;

Procedure TComunicationDataCenter.SetBufferSize(Value : Longint);
Begin
 vBufferSize               := Value;
 vListenThread.vBufferSize := vBufferSize;
End;

Procedure TListenThread.StopListen;
Var
 FUDPReq : TDataPack;
Begin
 If (Self       = Nil) Then
  Exit;
 If (vUDPServer = Nil) Then
  Exit;
  Case vOperationMode Of
    omServer,
    omP2PClient : Begin
                   Enabled := False;
                   If vUDPServer  <> Nil Then
                    If vUDPServer.Active Then
                     Begin
                      vUDPServer.OnUDPRead   := Nil;
                      If vMyHandle = -1 Then
                       vMyHandle             := vUDPServer.Binding.Handle;
                      vConnectionID          := TComunicationDataCenter(vComunicationDataCenter).vConnectionID;
                      vUDPServer.BufferSize  := TComunicationDataCenter(vComunicationDataCenter).vBufferSize;
                      FUDPReq.PackID         := GenBufferID;
                      FUDPReq.PackType       := ptSend;
                      FUDPReq.PackCommand    := mtDefault;
                      FUDPReq.PackParts      := 1;
                      FUDPReq.PackCount      := 1;
                      FUDPReq.ConnectionId   := vConnectionID;
                      If (vUDPServer.Active) Then
                       If vOperationMode = omP2PClient Then
                        Begin
                         FUDPReq.TotalSize   := Length(DisconnectString + vWelcomeMessage);
                         FUDPReq.BufferSize  := FUDPReq.TotalSize;
                         StrToWide(DisconnectString + vWelcomeMessage, FUDPReq);
                         FUDPReq.TimePack    := Now;
                         TComunicationDataCenter(vComunicationDataCenter).vBufferDataset.AddBuffer(FUDPReq, vHost, vPort);
                           //vUDPServer.SendBuffer(vHost, vPort, StrToWide(DisconnectString + vWelcomeMessage, FUDPReq));
                        End;
                     End;
                   If vUDPClient <> Nil Then
                    Begin
                     vUDPClient.Active := False;
                     FreeAndNil(vUDPClient);
                    End;
                   If vUDPServer <> Nil Then
                    Begin
                     Try
                      vUDPServer.Bindings.BeginUpdate;
                      Try
                       vUDPServer.Bindings.Clear;
                      Finally
                       vUDPServer.Bindings.EndUpdate;
                      End;
                      vUDPServer.Active    := False;
                      vMyHandle            := -1;
                     Except
                     End;
                     FreeAndNil(vUDPServer);
                    End;
                  End;
    omClient    :
                  Begin
                   If vUDPClient <> Nil Then
                    Begin
                     vUDPClient.Active := False;
                     FreeAndNil(vUDPClient);
                    End;
                  End;
  End;
End;

Function TipPoolerObject.WideToStr(Var FUDPReq     : TDataPack;
                                   Buffer          : TBufferDataset;
                                   Var PackID      : TPackString;
                                   Var MessageType : Word;
                                   ConnectionID    : TPackString;
                                   Ip              : String;
                                   Port            : Integer) : String;
Var
 vConnectionID : TPackString;
 PackType      : TPackType;
Begin
 vConnectionID := FUDPReq.ConnectionID;
 If TComunicationDataCenter(aParent).vOperationMode = omP2PClient Then
  FUDPReq.ConnectionID := TComunicationDataCenter(aParent).vConnectionID;
 PackType              := FUDPReq.PackType;
 If (PackType <> ptBack) Then
  FUDPReq.PackType     := ptInData;
 Buffer.AddBuffer(FUDPReq, Ip, Port);
 FUDPReq.PackType      := PackType;
 If (FUDPReq.PackType <> ptBack) Then
  Begin
   Result := Buffer.ResultBuffer(FUDPReq.PackID);
   If Result <> '' Then
    Buffer.DeleteBuffer(FUDPReq);
  End
 Else
  Buffer.DeleteBuffer(FUDPReq);
 If (FUDPReq.PackType <> ptBack) Then
  Begin
   MessageType := FUDPReq.PackCommand;
   PackID      := FUDPReq.PackID;
   ReplyPack(FUDPReq.PackID,
             vConnectionID, Ip, Port,
             FUDPReq.PackCount);
  End;
End;

Procedure TipPoolerObject.AddBuffer(ConnectionId  : TPackString;
                                    Text          : String;
                                    DataEncrypted : Boolean     = False;
                                    MessageType   : Word        = mtDefault;
                                    PackID        : TPackString = '');
Var
 ClientDataBuffer : PClientDataBuffer;
 vTempLine        : String;
 vPosEof          : Integer;
 vListLock        : TList;
Begin
 vPosEof := Pos(TComunicationDataCenter(aParent).vEOC, Text);
 New(ClientDataBuffer);
 If (Pos(TComunicationDataCenter(aParent).vEOC, Text) = 0) And (Text <> '') Then
  Begin
   Try
     //ClientDataBuffer^              := TClientDataBuffer.Create;
    ClientDataBuffer^.PackID       := PackID;
    ClientDataBuffer^.ConnectionId := ConnectionId;
    ClientDataBuffer^.MessageType  := MessageType;
    ClientDataBuffer^.Data         := Text;
    ClientDataBuffer^.DataLength   := Length(Text);
    ClientDataBuffer^.Readed       := False;
    ClientDataBuffer^.EOF          := False;
    vListLock:=TComunicationDataCenter(aParent).vDataBuffer.LockList;
    Try
     vListLock.Capacity:=vListLock.Count+1;
     vListLock.Add(ClientDataBuffer);
     vListLock.Capacity:=vListLock.Count;
    Finally
     TComunicationDataCenter(aParent).vDataBuffer.UnlockList;
    End;
   Except
   End;
  End
 Else If (Text <> '') Then
  Begin
   vTempLine := Copy(Text,1, vPosEof + Length(TComunicationDataCenter(aParent).vEOC) -1);
   Delete(Text, 1, vPosEof + Length(TComunicationDataCenter(aParent).vEOC) -1);
   ClientDataBuffer^.PackID       := PackID;
   ClientDataBuffer^.ConnectionId := ConnectionId;
   ClientDataBuffer^.MessageType  := MessageType;
   ClientDataBuffer^.Data         := vTempLine;
   ClientDataBuffer^.DataLength   := Length(vTempLine);
   ClientDataBuffer^.Readed       := False;
   ClientDataBuffer^.EOF          := (vPosEof > 1);
   vListLock:=TComunicationDataCenter(aParent).vDataBuffer.LockList;
   Try
    vListLock.Capacity   := vListLock.Count+1;
    vListLock.Add(ClientDataBuffer);
    vListLock.Capacity   := vListLock.Count;
   Finally
    TComunicationDataCenter(aParent).vDataBuffer.UnlockList;
   End;
   If (Length(Text) > 0) Then
    Begin
     AddBuffer(ConnectionId, Text, DataEncrypted, MessageType, PackID);
     Exit;
    End;
  End;
End;

Procedure TipPoolerObject.UDPUpdateLastAction(ConnectionID : TPackString);
Var
 I     : Integer;
 vList : TList;
Begin
 vList  := vUdpListBindings.LockList;
 Try
  For I := vList.Count -1 Downto 0 Do
   Begin
    If ConnectionID = TClientRect(vList.Items[I]).ConnectionID Then
     Begin
      TClientRect(vList.Items[I]).LastAction := Now;
      Break;
     End;
   End;
 Finally
  vUdpListBindings.UnlockList;
 End;
End;

Function TipPoolerObject.UDPWelcomeMessage(ConnectionID : TPackString) : String;
Var
 I     : Integer;
 vList : TList;
Begin
 Result := '';
 vList  := vUdpListBindings.LockList;
 Try
  For I := vList.Count -1 Downto 0 Do
   Begin
    If ConnectionID = TClientRect(vList.Items[I]).ConnectionID Then
     Begin
      Result := TClientRect(vList.Items[I]).WelcomeMessage;
      Break;
     End;
   End;
 Finally
  vUdpListBindings.UnlockList;
 End;
End;

Function TipPoolerObject.UDPCheckPortString(Value        : String;
                                            ConnectionID : TPackString) : Boolean;
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
      If ConnectionID = TClientRect(vList.Items[I]).ConnectionID Then
       Begin
        UDPUpdateLastAction(ConnectionID);
        Break;
       End;
     End;
   Finally
    vUdpListBindings.UnlockList;
   End;
  End;
End;

Procedure TipPoolerObject.UDPException(AThread               : TIdUDPListenerThread;
                                       ABinding              : TIdSocketHandle;
                                       Const AMessage        : String;
                                       Const AExceptionClass : TClass);
Begin
 If Assigned(TComunicationDataCenter(aParent).vErrorConnection) Then
  TComunicationDataCenter(aParent).vErrorConnection(Integer(emUDPException), AMessage);
 TComunicationDataCenter(aParent).Active := False;
  //TComunicationDataCenter(aParent).Active := True;
End;

Procedure TipPoolerObject.UDPRead(AThread     : TIdUDPListenerThread;
                                  Const AData : TIdBytes;
                                  ABinding    : TIdSocketHandle);
Var
 vPackID      : TPackString;
 vTempString  : String;
 vMessageType : Word;
 vEOF         : Boolean;
 FUDPReq      : TDataPack;
Begin
 BytesToRaw(AData, FUDPReq, SizeOf(TDataPack));
 FUDPReq.InternetIP := ABinding.PeerIP;
// FUDPReq.ConnectionId := ABinding.Handle;
 vTempString := WideToStr(FUDPReq, TComunicationDataCenter(aParent).vBufferDataset,
                          vPackID, vMessageType, FUDPReq.ConnectionID,
                          FUDPReq.InternetIP, FUDPReq.LocalPort);
 If UDPConnectString   (vTempString, FUDPReq.ConnectionID, FUDPReq.InternetIP, FUDPReq.LocalPort) Or
    UDPDisconnectString(vTempString, FUDPReq.ConnectionID) Or
    UDPCheckPortString (vTempString, FUDPReq.ConnectionID) Then
  Exit;
 UDPUpdateLastAction(FUDPReq.ConnectionID);
 If vTempString = '' Then
  Begin
   TComunicationDataCenter(aParent).vListenThread.OnTimer(TComunicationDataCenter(aParent).vListenThread);
   Exit;
  End;
 If Pos(vEOB,vTempString) > 0 Then
  vTempString := StringReplace(vTempString, vEOB, '', [rfReplaceAll]);
 vEOF := (Pos(TComunicationDataCenter(aParent).vEOC, vTempString) > 0);
 If vTempString <> '' Then
  Begin
   AddBuffer(FUDPReq.ConnectionID, vTempString, False, vMessageType, vPackID);
   If vEOF Then
    Begin
     Try
      If TComunicationDataCenter(aParent).vOperationMode in [omServer] Then
       Begin
        If Assigned(TComunicationDataCenter(aParent).vDataInServer) Then
         TComunicationDataCenter(aParent).vDataInServer(FUDPReq.ConnectionID); //ABinding.Handle);
       End
      Else If TComunicationDataCenter(aParent).vOperationMode in [omP2PClient] Then
       Begin
        If Assigned(TComunicationDataCenter(aParent).vDataInClient) Then
         TComunicationDataCenter(aParent).vDataInClient;
       End;
     Except
     End;
    End;
  End;
End;

Function TComunicationDataCenter.ClientDetails(ConnectionID : TPackString; Var Found : Boolean) : TClientRect;
Begin
 If ConnectionID = '-1' then
  ConnectionID := vConnectionID;
 Result := ClientObject.ClientDetails(ConnectionID, Found);
End;

Function TipPoolerObject.ClientDetails(ConnectionID : TPackString; Var Found : Boolean) : TClientRect;
Var
 I     : Integer;
 vList : TList;
Begin
 Found  := False;
 Result := Nil;
 If vUdpListBindings <> Nil Then
  Begin
   vList  := vUdpListBindings.LockList;
   Try
    For I := vList.Count -1 Downto 0 Do
     Begin
      If ConnectionID = TClientRect(vList.Items[I]).ConnectionID Then
       Begin
        Result := TClientRect(vList.Items[I]);
        Found  := True;
        Break;
       End;
     End;
   Finally
    vUdpListBindings.UnlockList;
   End;
  End;
End;

Function TipPoolerObject.UDPDisconnectString(Value : String; ConnectionID : TPackString) : Boolean;
Var
 I     : Integer;
 vList : TList;
Begin
 Result := False;
 If Pos(DisconnectString,Value) > 0 Then
  Begin
   vList := vUdpListBindings.LockList;
   Try
    For I := vList.Count -1 Downto 0 Do
     Begin
      If ConnectionID = TClientRect(vList.Items[I]).ConnectionID Then
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

Function TipPoolerObject.UDPConnectString(Value        : String;
                                          ConnectionID : TPackString;
                                          ip           : String;
                                          Port         : Integer) : Boolean;
Var
 aClient : TClientRect;
 Function PeerExist(ID : TPackString) : Boolean;
 Var
  I     : Integer;
  vList : TList;
 Begin
  vList  := vUdpListBindings.LockList;
  Result := False;
  Try
   For I := vList.Count -1 DownTo 0 Do
    Begin
     Result := TClientRect(vList.Items[I]).ConnectionID = id;
     If Result Then
      Break;
    End;
  Finally
   vUdpListBindings.UnlockList;
  End;
 End;
Begin
 Result := Pos(ConnectString, Value) > 0;
 If (Result) And (Not PeerExist(ConnectionID)) Then
  Begin
   Result                 := True;
   aClient                := TClientRect.Create;
   aClient.PeerIP         := IP;
   aClient.UDPPort        := Port;
   aClient.ConnectionID   := ConnectionID;
   Value                  := StringReplace(Value, ConnectString, '', [rfReplaceAll]);
   aClient.WelcomeMessage := Copy(Value, 1, Pos('<|>', Value) -1);
   Delete(Value, 1, Pos('<|>', Value) +2);
   aClient.LocalIP        := Copy(Value, 1, Pos('<|>', Value) -1);
   Delete(Value, 1, Pos('<|>', Value) +2);
   aClient.Context        := Nil;
   aClient.LastAction     := Now;
   vUdpListBindings.Add(aClient);
   If TComunicationDataCenter(aParent).vOperationMode = omServer Then
    Begin
     If Assigned(TComunicationDataCenter(aParent).vStatusConnection) Then
      TComunicationDataCenter(aParent).vStatusConnection(csConnected, ConnectionID, -1, '');
     If Assigned(TComunicationDataCenter(aParent).vAfterConnected) Then
      TComunicationDataCenter(aParent).vAfterConnected(ConnectionID); //ABinding.Handle);
    End
   Else If TComunicationDataCenter(aParent).vOperationMode = omP2PClient Then
    Begin
     If Assigned(TComunicationDataCenter(aParent).vClientStatusConnection) Then
      TComunicationDataCenter(aParent).vClientStatusConnection(csConnected, -1, '');
     If Assigned(TComunicationDataCenter(aParent).vAfterClientConnected) Then
      TComunicationDataCenter(aParent).vAfterClientConnected;
    End;
  End;
End;

Constructor TipPoolerObject.Create(Sender : TObject);
Begin
 aParent          := Sender;
 vUdpListBindings := TDataBuffer.Create;
End;

Destructor TipPoolerObject.Destroy;
Var
 I     : Integer;
 vList : TList;
Begin
 If vUdpListBindings <> Nil Then
  Begin
   vList  := vUdpListBindings.LockList;
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

Function StrToWide(Const strin : String; Var FUDPReq : TDataPack) : TIdBytes;
 Procedure StrToByte(Const Value : String; Var Dest : TByteArr);
 Var
  I : Integer;
 Begin
  FillChar(Dest[1], length(Dest) * sizeof(Char), #0);
  For I := 1 To Length(Value) do
   Dest[I] := Value[I];
 End;
Begin
 StrToByte(strin, FUDPReq.PieceBuf);
 FUDPReq.BufferSize := Length(strin);
 Result             := RawToBytes(FUDPReq, sizeof(TDataPack));
End;

Procedure TComunicationDataCenter.SetActive(Value:Boolean);
Begin
 vListenThread.ComunicationDataCenter := Self;
 vListenThread.vBufferSize            := vBufferSize;
 vListenThread.Host                   := vHost;
 vListenThread.Port                   := vPort;
 vListenThread.vOperationMode         := vOperationMode;
 If Value Then
  SetInternetIP('');
 vListenThread.Active                 := Value;
 vActiveComponent                     := vListenThread.Active;
End;

Procedure TComunicationDataCenter.ReleaseCacheAction(ConnectionID : TPackString);
Var
 I         : Integer;
 vListLock : TList;
Begin
 I := 0;
 vListLock := vDataBuffer.LockList;
 Try
  While I <= vListLock.Count -1 Do
   Begin
    If (TClientDataBuffer(vListLock[I]^).ConnectionID = ConnectionID) And
        Not(TClientDataBuffer(vListLock[I]^).Readed)                  Then
     TClientDataBuffer(vListLock[I]^).Readed := True;
    Inc(I);
   End;
 Finally
  vDataBuffer.UnlockList;
 End;
End;

Procedure TComunicationDataCenter.ClearPeers;
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

Procedure TComunicationDataCenter.AddPeer(Value : TBroadcastPeer);
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

Function TComunicationDataCenter.CheckConnect(ConnectionId : TPackString = '-1') : Boolean;
Begin
 Result := False;
 If ConnectionID = '-1' then
  ConnectionID := vConnectionID;
End;

Procedure TComunicationDataCenter.ReadBufferPacks(Var ListPacks : TListPacks;
                                                  ConnectionId  : TPackString   = '-1';
                                                  MessageTypes  : TMessageTypes = []);
Var
 I          : Integer;
 vListLock  : TList;
 PackResult : TPackResult;
 Function ValidType(MessageType : Word; MessageTypes : TMessageTypes) : Boolean;
 Var
  I : Integer;
 Begin
  Result := False;
  For I  := 0 To Length(MessageTypes) -1 Do
   Begin
    Result := MessageType = MessageTypes[I];
    If Result Then
     Break;
   End;
 End;
Begin
 If ListPacks = Nil Then
  ListPacks := TListPacks.Create;
 I := 0;
 If ConnectionID = '-1' then
  ConnectionID := vConnectionID;
 vListLock := vDataBuffer.LockList;
 Try
  Try
   While I <= vListLock.Count -1 Do
    Begin
     If (TClientDataBuffer(vListLock[I]^).ConnectionID = ConnectionID)        And
        Not(TClientDataBuffer(vListLock[I]^).Readed)                          And
        ValidType(TClientDataBuffer(vListLock[I]^).MessageType, MessageTypes) Then
      Begin
       TClientDataBuffer(vListLock[I]^).Readed := True;
       PackResult.MessageType := TClientDataBuffer(vListLock[I]^).MessageType;
       PackResult.Data        := StringReplace(StringReplace(TClientDataBuffer(vListLock[I]^).Data,
                                               vEOC, '', [rfReplaceAll]), vEOB, '', [rfReplaceAll]);
       ListPacks.Add(PackResult);
      End;
     Inc(I);
    End;
  Finally
   vDataBuffer.UnlockList;
  End;
 Except
  ListPacks.Clear;
 End;
End;

Function TComunicationDataCenter.ReadAll(ConnectionID : TPackString = '-1') : String;
Begin
 Result := '';
 If ConnectionID = '-1' then
  ConnectionID := vConnectionID;
 While HasBuffer(ConnectionID) > 0 Do
  Result := Result + ReadBuffer(ConnectionID);
End;

Procedure TComunicationDataCenter.StartActions(Const ShutdownEvent : TLightWeightEvent;
                                               WaitFor             : LongWord;
                                               Value               : TExecuteFunction;
                                               ConnectionID        : TPackString;
                                               Var Result          : String);
Var
 vActual   : TDatetime;
 vTempWait : LongWord;
Begin
 vActual   := Now;
 vTempWait := System.DateUtils.MilliSecondsBetween(Now, vActual);
 While (WaitFor >= vTempWait) Do
  Begin
   vTempWait := System.DateUtils.MilliSecondsBetween(vActual, Now);
   ShutdownEvent.WaitFor(5);
   If Assigned(Value) Then
    Begin
     Result := Value(ConnectionID, False, Result);
     If Result <> '' Then
      Break;
    End;
  End;
End;

Function TComunicationDataCenter.CurrentBuffer(ConnectionId : TPackString;
                                               Wait         : Boolean = False;
                                               InitBuffer   : String  = '') : String;
Var
 I         : Integer;
 vListLock : TList;
Begin
 Result    := '';
 I         := 0;
 Result    := InitBuffer;
 vListLock := vDataBuffer.LockList;
 Try
  While I <= vListLock.Count -1 Do
   Begin
    If (TClientDataBuffer(vListLock[I]^).ConnectionID = ConnectionID) And
       Not(TClientDataBuffer(vListLock[I]^).Readed)                   Then
    Begin
     Result := Result+TClientDataBuffer(vListLock[I]^).Data;
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
    If ((Result = '') Or (Result = InitBuffer)) And (Pos(vEOC, Result) = 0) Then
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

Function TComunicationDataCenter.ReadLn(ConnectionID : TPackString) : String;
Begin
 If ConnectionID = '-1' then
  ConnectionID := vConnectionID;
 Result := CurrentBuffer(ConnectionId);
End;

Function TComunicationDataCenter.ReadLn : String;
Begin
 Result := ReadLn(vConnectionID);
End;

Function TComunicationDataCenter.Read(ConnectionID : TPackString = '-1') : String;
Begin
 If ConnectionID = '-1' then
  ConnectionID := vConnectionID;
 Result := ReadBuffer(ConnectionID);
End;

Function TComunicationDataCenter.DataToSend(Data : String) : Boolean;
Begin
 Result := False;

End;

Function TComunicationDataCenter.DataToSend(ConnectionID : TPackString;
                                            Data         : String) : Boolean;
Begin
 Result := False;
 If ConnectionID = '-1' then
  ConnectionID := vConnectionID;

End;

Function TComunicationDataCenter.Write(Data        : String;
                                       MessageType : Word    = mtDefault;
                                       ip          : String  = '';
                                       Port        : Integer = -1) : Boolean;
Begin
 Result := Write(vConnectionID, Data, MessageType, ip, Port);
End;

Function TComunicationDataCenter.Write(ConnectionID : TPackString;
                                       Data         : String;
                                       MessageType  : Word    = mtDefault;
                                       ip           : String  = '';
                                       Port         : Integer = -1) : Boolean;
Var
 vDataPack,
 vDataBuff   : String;
 A,
 vBufferSize : Integer;
 vActive     : Boolean;
 FUDPReq     : TDataPack;
 ClientRect  : TClientRect;
 vFound      : Boolean;
Begin
 Result               := False;
 vDataPack            := Data + vEOC + vEOB;
 If ConnectionID = '-1' Then
  ConnectionID := vConnectionID;
 FUDPReq.ConnectionID := ConnectionID;
 FUDPReq.PackID       := GenBufferID;
 A                    := 1;
 FUDPReq.PackCommand  := MessageType;
 FUDPReq.PackType     := ptSend;
 FUDPReq.TotalSize    := Length(vDataPack);
 If Length(vDataPack) > 0 Then
  FUDPReq.PackParts   := Length(vDataPack) Div udpBuffer
 Else
  FUDPReq.PackParts   := 1;
 FUDPReq.TotalSize    := Length(vDataPack);
 If (Length(vDataPack) Mod udpBuffer) > 0 Then
  Inc(FUDPReq.PackParts);
 While (vDataPack <> '') Do
  Begin
   vActive := vListenThread.Active;
   If vActive Then
    Begin
     If length(vDataPack) > udpBuffer Then
      vBufferSize := udpBuffer
     Else
      vBufferSize := Length(vDataPack);
     vDataBuff := Copy(vDataPack, 1, vBufferSize);
     FUDPReq.PackCount  := A;
     FUDPReq.BufferSize := Length(vDataBuff);
     StrToBufferDataPack(vDataBuff, FUDPReq);
     FUDPReq.TimePack   := Now;
     If Trim(ip) = '' Then
      Begin
       ClientRect         := ClientDetails(ConnectionId, vFound);
       If vFound Then
        vBufferDataset.AddBuffer(FUDPReq, ClientRect.PeerIP, ClientRect.UDPPort, FUDPReq.PackParts > 1)
       Else
        vBufferDataset.AddBuffer(FUDPReq, '', 0, FUDPReq.PackParts > 1);
      End
     Else
      vBufferDataset.AddBuffer(FUDPReq, Ip, Port, FUDPReq.PackParts > 1);
     Delete(vDataPack, 1, vBufferSize);
     Inc(A);
    End
   Else
    Break;
  End;
End;

Function TComunicationDataCenter.Write(ConnectionID : TPackString;
                                       Data         : TMemoryStream;
                                       MessageType  : Word    = mtDefault;
                                       ip           : String  = '';
                                       Port         : Integer = -1) : Boolean;
Begin
 If ConnectionID = '-1' then
  ConnectionID := vConnectionID;
 Result := Write(ConnectionID, MemoryStreamToString(Data), MessageType, ip, Port);
End;

Function TComunicationDataCenter.Write(Data        : TMemoryStream;
                                       MessageType : Word   = mtDefault;
                                       ip          : String = '';
                                       Port        : Integer = -1) : Boolean;
Begin
 Result := Write(vConnectionID, MemoryStreamToString(Data), MessageType, ip, Port);
End;

Procedure TComunicationDataCenter.SetWelcomeMessage(Value : String);
Begin
 vWelcomeMessage              := Value;
 vListenThread.WelcomeMessage := vWelcomeMessage;
End;

Function TComunicationDataCenter.HasBuffer(ConnectionId : TPackString = '-1') : Integer;
Var
 I           : Integer;
 vTempString : String;
 vListLock   : TList;
Begin
 Result := 0;
 If ConnectionID = '-1' then
  ConnectionID := vConnectionID;
 vListLock := vDataBuffer.LockList;
 Try
  I := vListLock.Count -1;
  Try
   While I > -1 Do
    Begin
     If (TClientDataBuffer(vListLock[I]^).ConnectionID = ConnectionID) And
        Not(TClientDataBuffer(vListLock[I]^).Readed) Then
      vTempString := vTempString + TClientDataBuffer(vListLock[I]^).Data;
     If (Pos(vEOC, vTempString) > 0) Then
      Begin
       Inc(Result);
       Delete(vTempString, 1, Pos(vEOC,vTempString) + Length(vEOC) -1);
      End;
     Dec(I);
    End;
  Except

  End;
 Finally
  vDataBuffer.UnlockList;
 End;
End;

Function TComunicationDataCenter.ClientDisconnect(ConnectionID : TPackString = '-1') : Boolean;
Begin
 Result := False;
 If ConnectionID = '-1' then
  ConnectionID := vConnectionID;
End;

Procedure TComunicationDataCenter.DeletePeer(Index : Integer);
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

Destructor TComunicationDataCenter.Destroy;
 Procedure ClearBuffer;
 Var
  I         : Integer;
  vListLock : TList;
 Begin
  vListLock := vDataBuffer.LockList;
  Try
   For I := vListLock.Count -1 Downto 0 Do
    Begin
     TClientDataBuffer(vListLock[I]^).Data := '';
     vListLock[I]       := Nil;
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
  I         : Integer;
  vListLock : TList<TBroadcastPeer>;
 Begin
  If vBroadcastPeers <> Nil Then
   Begin
    vListLock := vBroadcastPeers.LockList;
    Try
     For I := vListLock.Count -1 Downto 0 Do
      vListLock.Delete(I);
    Finally
     vBroadcastPeers.UnlockList;
    End;
    FreeAndNil(vBroadcastPeers);
   End;
 End;
 Procedure FreeBufferList(Var vBuffer : TBufferDataList);
 Var
  I:Integer;
 Begin
  For I:=vBuffer.Count-1 Downto 0 Do
   Begin
    vBuffer[I].Free;
    vBuffer.Delete(I);
   End;
  FreeAndNil(vBuffer);
 End;
Begin
 SetActive(False);
 ClearPeers;
 ClearBuffer;
 FreeAndNil(SimpleEvent);
 FreeAndNil(vBufferDataset);
 FreeBufferList(vBufferReplyCopy);
 FreeAndNil(vListenThread);
 Inherited;
End;

Constructor TComunicationDataCenter.Create(AOwner:TComponent);
Begin
 Inherited;
 vBroadcastPeers         := TBroadcastPeers.Create;
 SimpleEvent             := TLightWeightEvent.Create;
 ClientObject            := TipPoolerObject.Create(Self);
 vListenThread           := TListenThread.Create;
 //vListenThread.aSender   := Self;
 vBufferDataset          := TBufferDataset.Create;
 vBufferReplyCopy        := TBufferDataList.Create;
 vDataBuffer             := TDataBuffer.Create;
 vOperationMode          := omServer;
 vConnectionID           := GenBufferID;
 vEOC                    := '<$EOC$>';
 vHost                   := '0.0.0.0';
 vPort                   := 0;
 vConnections            := 0;
                                 //2kb      4Kb    8k     16k   Buffer
 vBufferSize             := 4096;//2048;  //4096   8192;  16384;
 vTimeOut                := 3000;
 vActiveComponent        := False;
 vIpVersion              := Id_IPv4;
 vDataInClient           := Nil;
 vDataInServer           := Nil;
 vErrorConnection        := Nil;
 vTimeoutErrorPack       := Nil;
 vTimeoutErrorClientPack := Nil;
 vStatusConnection       := Nil;
 vClientStatusConnection := Nil;
 vAfterConnected         := Nil;
 vAfterClientConnected   := Nil;
End;

Constructor TBufferDataset.Create;
Begin
 vBuffer          := TBufferDataList.Create;//TBufferDataThread.Create;
 vBufferReply     := TBufferDataList.Create;//TBufferDataThread.Create;
 vCriticalSection := TCriticalSection.Create;
End;

Destructor TBufferDataset.Destroy;
 Procedure FreeBuffer(Var vBuffer : TBufferDataList);//TBufferDataThread);
 Var
  I      : Integer;
  vClass : TList<TBufferData>;
 Begin
  If vBuffer <> Nil Then
   Begin
    vClass := vBuffer;//.LockList;
    Try
     For I := vClass.Count -1 Downto 0 Do
      Begin
       vClass[I].Free;
       vClass.Delete(I);
      End;
    Finally
     //vBuffer.UnlockList;
    End;
    FreeAndNil(vBuffer);
   End;
 End;
Begin
 If (vCriticalSection <> Nil) Then
  Begin
   vCriticalSection.Release;
   vCriticalSection.Leave;
   FreeAndNil(vCriticalSection);
  End;
 FreeBuffer(vBuffer);
 FreeBuffer(vBufferReply);
 Inherited;
End;

end.
