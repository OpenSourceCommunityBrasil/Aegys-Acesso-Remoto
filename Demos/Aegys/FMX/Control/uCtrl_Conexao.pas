unit uCtrl_Conexao;

{
  Project Aegys Remote Support.

  Created by Gilberto Rocha da Silva in 04/05/2017 based on project Allakore, has by objective to promote remote access
  and other resources freely to all those who need it, today maintained by a beautiful community. Listing below our
  higly esteemed collaborators:

  Gilberto Rocha da Silva (XyberX) (Creator of Aegys Project/Main Developer/Admin)
  Wendel Rodrigues Fassarella (wendelfassarella) (Creator of Aegys FMX/CORE Developer)
  Rai Duarte Jales (Raí Duarte) (Aegys Server Developer)
  Roniery Santos Cardoso (Aegys Developer)
  Alexandre Carlos Silva Abade (Aegys Developer)
  Mobius One (Aegys Developer)
}

interface

uses
  System.Classes, System.Threading, uCtrl_Threads, System.Win.ScktComp,
  uConstants, uLocaleFunctions, Winapi.Windows;

type
  TConexao = class
  private
    procedure SocketAreaRemotaConnect(Sender: TObject;
      Socket: TCustomWinSocket);
    procedure SocketAreaRemotaDisconnect(Sender: TObject;
      Socket: TCustomWinSocket);
    procedure SocketAreaRemotaError(Sender: TObject; Socket: TCustomWinSocket;
      ErrorEvent: TErrorEvent; var ErrorCode: Integer);
    procedure SocketArquivosConnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure SocketArquivosDisconnect(Sender: TObject;
      Socket: TCustomWinSocket);
    procedure SocketArquivosError(Sender: TObject; Socket: TCustomWinSocket;
      ErrorEvent: TErrorEvent; var ErrorCode: Integer);
    procedure SocketPrincipalConnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure SocketPrincipalConnecting(Sender: TObject;
      Socket: TCustomWinSocket);
    procedure SocketPrincipalDisconnect(Sender: TObject;
      Socket: TCustomWinSocket);
    procedure SocketPrincipalError(Sender: TObject; Socket: TCustomWinSocket;
      ErrorEvent: TErrorEvent; var ErrorCode: Integer);
    procedure SocketTecladoConnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure SocketTecladoDisconnect(Sender: TObject;
      Socket: TCustomWinSocket);
    procedure SocketTecladoError(Sender: TObject; Socket: TCustomWinSocket;
      ErrorEvent: TErrorEvent; var ErrorCode: Integer);
  private
    Locale: TLocale;
    FAcessando: Boolean;
    FID: string;
    FIntervalo: Integer;
    FLatencia: Integer;
    FMostrarMouse: Boolean;
    FOldClipboardText: string;
    FResolucaoAltura: Integer;
    FResolucaoLargura: Integer;
    FSenha: string;
    FSocketAreaRemota: TClientSocket;
    FSocketArquivos: TClientSocket;
    FSocketPrincipal: TClientSocket;
    FSocketTeclado: TClientSocket;
    FThreadAreaRemota: TThreadConexaoAreaRemota;
    FThreadArquivos: TThreadConexaoArquivos;
    FThreadPrincipal: TThreadConexaoPrincipal;
    FThreadTeclado: TThreadConexaoTeclado;
    FVisualizador: Boolean;
    procedure SetAcessando(const Value: Boolean);
    procedure SetID(const Value: string);
    procedure SetIntervalo(const Value: Integer);
    procedure SetLatencia(const Value: Integer);
    procedure SetMostrarMouse(const Value: Boolean);
    procedure SetOldClipboardText(const Value: string);
    procedure SetResolucaoAltura(const Value: Integer);
    procedure SetResolucaoLargura(const Value: Integer);
    procedure SetSenha(const Value: string);
    procedure SetSocketAreaRemota(const Value: TClientSocket);
    procedure SetSocketArquivos(const Value: TClientSocket);
    procedure SetSocketPrincipal(const Value: TClientSocket);
    procedure SetSocketTeclado(const Value: TClientSocket);
    procedure SetThreadAreaRemota(const Value: TThreadConexaoAreaRemota);
    procedure SetThreadArquivos(const Value: TThreadConexaoArquivos);
    procedure SetThreadPrincipal(const Value: TThreadConexaoPrincipal);
    procedure SetThreadTeclado(const Value: TThreadConexaoTeclado);
    procedure SetVisualizador(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure CriarThread(AThread: IDThreadType; ASocket: TCustomWinSocket);
    procedure FecharSockets;
    procedure LimparThread(AThread: IDThreadType);
    procedure ReconectarSocket;
    procedure ReconectarSocketsSecundarios;
    property Acessando: Boolean read FAcessando write SetAcessando;
    property ID: string read FID write SetID;
    property Intervalo: Integer read FIntervalo write SetIntervalo;
    property Latencia: Integer read FLatencia write SetLatencia;
    property MostrarMouse: Boolean read FMostrarMouse write SetMostrarMouse;
    property OldClipboardText: string read FOldClipboardText
      write SetOldClipboardText;
    property ResolucaoAltura: Integer read FResolucaoAltura
      write SetResolucaoAltura;
    property ResolucaoLargura: Integer read FResolucaoLargura
      write SetResolucaoLargura;
    property Senha: string read FSenha write SetSenha;
    property SocketAreaRemota: TClientSocket read FSocketAreaRemota
      write SetSocketAreaRemota;
    property SocketArquivos: TClientSocket read FSocketArquivos
      write SetSocketArquivos;
    property SocketPrincipal: TClientSocket read FSocketPrincipal
      write SetSocketPrincipal;
    property SocketTeclado: TClientSocket read FSocketTeclado
      write SetSocketTeclado;
    property ThreadAreaRemota: TThreadConexaoAreaRemota read FThreadAreaRemota
      write SetThreadAreaRemota;
    property ThreadArquivos: TThreadConexaoArquivos read FThreadArquivos
      write SetThreadArquivos;
    property ThreadPrincipal: TThreadConexaoPrincipal read FThreadPrincipal
      write SetThreadPrincipal;
    property ThreadTeclado: TThreadConexaoTeclado read FThreadTeclado
      write SetThreadTeclado;
    property Visualizador: Boolean read FVisualizador write SetVisualizador;
  end;

implementation

{ TConexao }

uses uFormConexao, System.SysUtils, uFormTelaRemota, System.StrUtils;

constructor TConexao.Create;
var
  xHost: AnsiString;
  iPort: Integer;
begin
  Locale := TLocale.Create;
  if (ParamStr(1) <> '') then
    xHost := ParamStr(1)
  else
    xHost := SERVIDOR;

  if (ParamStr(2) <> '') then
    iPort := StrToIntDef(ParamStr(2), PORTA)
  else
    iPort := PORTA;

  FMostrarMouse := False;

  SocketPrincipal := TClientSocket.Create(nil);
  SocketPrincipal.Active := False;
  SocketPrincipal.ClientType := ctNonBlocking;
  SocketPrincipal.OnConnecting := SocketPrincipalConnecting;
  SocketPrincipal.OnConnect := SocketPrincipalConnect;
  SocketPrincipal.OnDisconnect := SocketPrincipalDisconnect;
  SocketPrincipal.OnError := SocketPrincipalError;
  SocketPrincipal.HOST := xHost;
  SocketPrincipal.Port := iPort;

  SocketAreaRemota := TClientSocket.Create(nil);
  SocketAreaRemota.Active := False;
  SocketAreaRemota.ClientType := ctNonBlocking;
  SocketAreaRemota.OnConnect := SocketAreaRemotaConnect;
  SocketAreaRemota.OnError := SocketAreaRemotaError;
  SocketAreaRemota.OnDisconnect := SocketAreaRemotaDisconnect;
  SocketAreaRemota.HOST := xHost;
  SocketAreaRemota.Port := iPort;

  SocketTeclado := TClientSocket.Create(nil);
  SocketTeclado.Active := False;
  SocketTeclado.ClientType := ctNonBlocking;
  SocketTeclado.OnConnect := SocketTecladoConnect;
  SocketTeclado.OnError := SocketTecladoError;
  SocketTeclado.OnDisconnect := SocketTecladoDisconnect;
  SocketTeclado.HOST := xHost;
  SocketTeclado.Port := iPort;

  SocketArquivos := TClientSocket.Create(nil);
  SocketArquivos.Active := False;
  SocketArquivos.ClientType := ctNonBlocking;
  SocketArquivos.OnConnect := SocketArquivosConnect;
  SocketArquivos.OnError := SocketArquivosError;
  SocketArquivos.OnDisconnect := SocketArquivosDisconnect;
  SocketArquivos.HOST := xHost;
  SocketArquivos.Port := iPort;

  ResolucaoLargura := 986;
  ResolucaoAltura := 600;

  Latencia := 256;
end;

procedure TConexao.CriarThread(AThread: IDThreadType;
  ASocket: TCustomWinSocket);
begin
  case AThread of
    ttPrincipal:
      begin
        LimparThread(ttPrincipal);
        FThreadPrincipal := TThreadConexaoPrincipal.Create(ASocket);
      end;
    ttAreaRemota:
      begin
        LimparThread(ttAreaRemota);
        FThreadAreaRemota := TThreadConexaoAreaRemota.Create(ASocket);
      end;
    ttTeclado:
      begin
        LimparThread(ttTeclado);
        FThreadTeclado := TThreadConexaoTeclado.Create(ASocket);
      end;
    ttArquivos:
      begin
        LimparThread(ttArquivos);
        FThreadArquivos := TThreadConexaoArquivos.Create(ASocket);
      end;
  end;
end;

destructor TConexao.Destroy;
begin
  if Assigned(FSocketPrincipal) then
    FreeAndNil(FSocketPrincipal);
  if Assigned(FSocketAreaRemota) then
    FreeAndNil(FSocketAreaRemota);
  if Assigned(FSocketTeclado) then
    FreeAndNil(FSocketTeclado);
  if Assigned(FSocketArquivos) then
    FreeAndNil(FSocketArquivos);
  if Assigned(FThreadPrincipal) then
    LimparThread(ttPrincipal);
  if Assigned(FThreadAreaRemota) then
    LimparThread(ttAreaRemota);
  if Assigned(FThreadTeclado) then
    LimparThread(ttTeclado);
  if Assigned(FThreadArquivos) then
    LimparThread(ttArquivos);
  Locale.DisposeOf;
  inherited;
end;

procedure TConexao.FecharSockets;
begin
  SocketPrincipal.Close;
  SocketAreaRemota.Close;
  SocketTeclado.Close;
  SocketArquivos.Close;

  Visualizador := False;

  if Acessando then
    Acessando := False;

  if not FormConexao.Visible then
    FormConexao.Show;

  FormConexao.LimparConexao;
end;

procedure TConexao.LimparThread(AThread: IDThreadType);
begin
  case AThread of
    ttPrincipal:
      begin
        if Assigned(FThreadPrincipal) then
        begin
          if not FThreadPrincipal.Finished then
            FThreadPrincipal.Terminate;
          FThreadPrincipal := nil;
        end;
      end;
    ttAreaRemota:
      begin
        if Assigned(FThreadAreaRemota) then
        begin
          if not FThreadAreaRemota.Finished then
            FThreadAreaRemota.Terminate;
          FThreadAreaRemota := nil;
        end;
      end;
    ttTeclado:
      begin
        if Assigned(FThreadTeclado) then
        begin
          if not FThreadTeclado.Finished then
            FThreadTeclado.Terminate;
          FThreadTeclado := nil;
        end;
      end;
    ttArquivos:
      begin
        if Assigned(FThreadArquivos) then
        begin
          if not FThreadArquivos.Finished then
            FThreadArquivos.Terminate;
          FThreadArquivos := nil;
        end;
      end;
  end;
end;

procedure TConexao.ReconectarSocket;
begin
  if not SocketPrincipal.Active then
    SocketPrincipal.Active := True;
end;

procedure TConexao.ReconectarSocketsSecundarios;
begin
  Visualizador := False;
  SocketAreaRemota.Close;
  SocketTeclado.Close;
  SocketArquivos.Close;
  Sleep(1000);
  SocketAreaRemota.Active := True;
  SocketTeclado.Active := True;
  SocketArquivos.Active := True;
end;

procedure TConexao.SetAcessando(const Value: Boolean);
begin
  FAcessando := Value;
end;

procedure TConexao.SetID(const Value: string);
begin
  FID := Value;
end;

procedure TConexao.SetLatencia(const Value: Integer);
begin
  FLatencia := Value;
end;

procedure TConexao.SetMostrarMouse(const Value: Boolean);
var
  xSend: string;
begin
  FMostrarMouse := Value;
  if Conexao.Visualizador then
  begin
    xSend := IfThen(Value, '<|SHOWMOUSE|>', '<|HIDEMOUSE|>');
    SocketPrincipal.Socket.SendText('<|REDIRECT|>' + xSend);
  end;
end;

procedure TConexao.SetOldClipboardText(const Value: string);
begin
  FOldClipboardText := Value;
end;

procedure TConexao.SetResolucaoAltura(const Value: Integer);
begin
  FResolucaoAltura := Value;
end;

procedure TConexao.SetResolucaoLargura(const Value: Integer);
begin
  FResolucaoLargura := Value;
end;

procedure TConexao.SetSenha(const Value: string);
begin
  FSenha := Value;
end;

procedure TConexao.SetSocketAreaRemota(const Value: TClientSocket);
begin
  FSocketAreaRemota := Value;
end;

procedure TConexao.SetSocketArquivos(const Value: TClientSocket);
begin
  FSocketArquivos := Value;
end;

procedure TConexao.SetSocketPrincipal(const Value: TClientSocket);
begin
  FSocketPrincipal := Value;
end;

procedure TConexao.SetSocketTeclado(const Value: TClientSocket);
begin
  FSocketTeclado := Value;
end;

procedure TConexao.SetThreadAreaRemota(const Value: TThreadConexaoAreaRemota);
begin
  FThreadAreaRemota := Value;
end;

procedure TConexao.SetThreadArquivos(const Value: TThreadConexaoArquivos);
begin
  FThreadArquivos := Value;
end;

procedure TConexao.SetThreadPrincipal(const Value: TThreadConexaoPrincipal);
begin
  FThreadPrincipal := Value;
end;

procedure TConexao.SetThreadTeclado(const Value: TThreadConexaoTeclado);
begin
  FThreadTeclado := Value;
end;

procedure TConexao.SetIntervalo(const Value: Integer);
begin
  FIntervalo := Value;
end;

procedure TConexao.SetVisualizador(const Value: Boolean);
begin
  FVisualizador := Value;
end;

procedure TConexao.SocketAreaRemotaConnect(Sender: TObject;
  Socket: TCustomWinSocket);
begin
  Socket.SendText('<|DESKTOPSOCKET|>' + ID + '<|END|>');
  CriarThread(ttAreaRemota, Socket);
end;

procedure TConexao.SocketAreaRemotaDisconnect(Sender: TObject;
  Socket: TCustomWinSocket);
begin
  LimparThread(ttAreaRemota);
end;

procedure TConexao.SocketAreaRemotaError(Sender: TObject;
  Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
  ErrorCode := 0;
end;

procedure TConexao.SocketArquivosConnect(Sender: TObject;
  Socket: TCustomWinSocket);
begin
  Socket.SendText('<|FILESSOCKET|>' + ID + '<|END|>');
  CriarThread(ttArquivos, Socket);
end;

procedure TConexao.SocketArquivosDisconnect(Sender: TObject;
  Socket: TCustomWinSocket);
begin
  LimparThread(ttArquivos);
end;

procedure TConexao.SocketArquivosError(Sender: TObject;
  Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
  ErrorCode := 0;
end;

Function MacAddress: string;
Var
 Lib    : Cardinal;
 Func   : Function(GUID: PGUID): longint; Stdcall;
 GUID1,
 GUID2  : TGUID;
Begin
 Result := '';
 Lib := LoadLibrary('rpcrt4.dll');
 If Lib <> 0 Then
  Begin
   @Func := GetProcAddress(Lib, 'UuidCreateSequential');
   If assigned(Func) Then
    Begin
     If (Func(@GUID1) = 0) And
        (Func(@GUID2) = 0) And
        (GUID1.D4[2] = GUID2.D4[2]) And
        (GUID1.D4[3] = GUID2.D4[3]) And
        (GUID1.D4[4] = GUID2.D4[4]) And
        (GUID1.D4[5] = GUID2.D4[5]) And
        (GUID1.D4[6] = GUID2.D4[6]) And
        (GUID1.D4[7] = GUID2.D4[7]) Then
      Result := IntToHex(GUID1.D4[2], 2) + '-' +
                IntToHex(GUID1.D4[3], 2) + '-' +
                IntToHex(GUID1.D4[4], 2) + '-' +
                IntToHex(GUID1.D4[5], 2) + '-' +
                IntToHex(GUID1.D4[6], 2) + '-' +
                IntToHex(GUID1.D4[7], 2);
    End;
  End;
End;

Function SystemDrive : String;
Var
 DirWin,
 SystemDriv : String;
Begin
 SetLength(DirWin, MAX_PATH);
 GetSystemDirectory(PChar(DirWin), MAX_PATH);
 SystemDriv := Copy(DirWin, 1, 3);
 Result := SystemDriv
End;

Function SerialNumHardDisk(FDrive : String) : String;
Var
 Serial,
 DirLen,
 Flags   : DWORD;
 DLabel  : Array[0..11] Of Char;
Begin
 Try
  GetVolumeInformation(PChar(Copy(FDrive, 1, 1) + ':\'), DLabel, 12, @Serial, DirLen, Flags, nil, 0);
  Result := IntToHex(Serial, 8);
 Except
  Result := '';
 End;
End;

procedure TConexao.SocketPrincipalConnect(Sender: TObject;
  Socket: TCustomWinSocket);
Var
 vHD,
 vMAC : String;
begin
  FormConexao.MudarStatusConexao(3, Locale.GetLocale(MSGS, 'Connected'));
  Intervalo := 0;
  FormConexao.tmrIntervalo.Enabled := True;
  vMAC := MacAddress;
  vHD  := SerialNumHardDisk(SystemDrive);
  Socket.SendText('<|MAINSOCKET|><|MAC|>' + vMAC + '<|>' + '<|HD|>' + vHD + '<|>');
  CriarThread(ttPrincipal, Socket);
end;

procedure TConexao.SocketPrincipalConnecting(Sender: TObject;
  Socket: TCustomWinSocket);
begin
  FormConexao.MudarStatusConexao(1, Locale.GetLocale(MSGS, 'Connecting'));
end;

procedure TConexao.SocketPrincipalDisconnect(Sender: TObject;
  Socket: TCustomWinSocket);
begin
  LimparThread(ttPrincipal);
  if (FormTelaRemota.Visible) then
    FormTelaRemota.Close;
  FormConexao.SetOffline;
  FormConexao.MudarStatusConexao(2, Locale.GetLocale(MSGS, 'ConnectionError'));
  FecharSockets;
end;

procedure TConexao.SocketPrincipalError(Sender: TObject;
  Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
  ErrorCode := 0;
  if (FormTelaRemota.Visible) then
    FormTelaRemota.Close;
  FormConexao.SetOffline;
  FormConexao.MudarStatusConexao(2, Locale.GetLocale(MSGS, 'ConnectionError'));
  FecharSockets;
end;

procedure TConexao.SocketTecladoConnect(Sender: TObject;
  Socket: TCustomWinSocket);
begin
  Socket.SendText('<|KEYBOARDSOCKET|>' + ID + '<|END|>');
  CriarThread(ttTeclado, Socket);
end;

procedure TConexao.SocketTecladoDisconnect(Sender: TObject;
  Socket: TCustomWinSocket);
begin
  LimparThread(ttTeclado);
end;

procedure TConexao.SocketTecladoError(Sender: TObject; Socket: TCustomWinSocket;
  ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
  ErrorCode := 0;
end;

end.
