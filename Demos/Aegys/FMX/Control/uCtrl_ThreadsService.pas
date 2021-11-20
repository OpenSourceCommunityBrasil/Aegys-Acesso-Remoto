unit uCtrl_ThreadsService;

interface

uses
  System.Classes, System.Win.ScktComp, uConstants;

type
  TThreadBase = class(TThread)
  private
    FTipo: IDThreadType;
    arrAcessos: array of TCustomWinSocket;
    FProtocolo: string;
  public
    scClient: TCustomWinSocket;
    constructor Create(ASocket: TCustomWinSocket; AProtocolo: string); overload; virtual;
    function GetAcesso(AHandle: Integer): TCustomWinSocket;
    function LengthAcessos: Integer;
    procedure Execute; override;
    procedure RemoverAcesso(AHandle: Integer);
    procedure SetAcesso(ASocket: TCustomWinSocket);
    procedure ThreadTerminate(ASender: TObject); virtual;
  end;

  TThreadConexaoDefinidor = class(TThread)
  private
    scClient: TCustomWinSocket;
  public
    constructor Create(ASocket: TCustomWinSocket); overload;
    procedure Execute; override;
  end;

  TThreadConexaoPrincipal = class(TThreadBase)
  public
    constructor Create(ASocket: TCustomWinSocket; AProtocolo: string); override;
    procedure Execute; override;
    procedure LimparAcessos;
    procedure ThreadTerminate(ASender: TObject); override;
  end;

  TThreadConexaoAreaRemota = class(TThreadBase)
  public
    constructor Create(ASocket: TCustomWinSocket; AProtocolo: string); override;
  end;

  TThreadConexaoTeclado = class(TThreadBase)
  public
    constructor Create(ASocket: TCustomWinSocket; AProtocolo: string); override;
  end;

  TThreadConexaoArquivos = class(TThreadBase)
  public
    constructor Create(ASocket: TCustomWinSocket; AProtocolo: string); override;
  end;

implementation

{ TThreadConexaoDefinidor }

uses System.SysUtils, uCtrl_Conexoes, uDMServer, Vcl.Dialogs;

{ TThreadConexaoDefinidor }

constructor TThreadConexaoDefinidor.Create(ASocket: TCustomWinSocket);
begin
  inherited Create(True);
  scClient := ASocket;
  FreeOnTerminate := True;
  Resume;
end;

procedure TThreadConexaoDefinidor.Execute;
var
  xBuffer, xBufferTemp, xID: string;
  iPosition: Integer;
begin
  inherited;

  while True do
  begin
    Sleep(FOLGAPROCESSAMENTO);

    if (scClient = nil)
      or not(scClient.Connected)
      or not(Assigned(DMServer)) then
      Break;

    if scClient.ReceiveLength < 1 then
      Continue;

    xBuffer := scClient.ReceiveText;

    iPosition := Pos('<|MAINSOCKET|>', xBuffer);
    if iPosition > 0 then
    begin
      DMServer.Conexoes.AdicionarConexao(IntToStr(scClient.Handle));
      DMServer.Conexoes.RetornaItemPorConexao(IntToStr(scClient.Handle)).CriarThread(ttPrincipal, scClient);
      Break;
    end;

    iPosition := Pos('<|DESKTOPSOCKET|>', xBuffer);
    if iPosition > 0 then
    begin
      xBufferTemp := xBuffer;
      Delete(xBufferTemp, 1, iPosition + 16);
      xID := Copy(xBufferTemp, 1, Pos('<|END|>', xBufferTemp) - 1);
      DMServer.Conexoes.RetornaItemPorID(xID).CriarThread(ttAreaRemota, scClient);
      Break;
    end;

    iPosition := Pos('<|KEYBOARDSOCKET|>', xBuffer);
    if iPosition > 0 then
    begin
      xBufferTemp := xBuffer;
      Delete(xBufferTemp, 1, iPosition + 17);
      xID := Copy(xBufferTemp, 1, Pos('<|END|>', xBufferTemp) - 1);
      DMServer.Conexoes.RetornaItemPorID(xID).CriarThread(ttTeclado, scClient);
      Break;
    end;

    iPosition := Pos('<|FILESSOCKET|>', xBuffer);
    if iPosition > 0 then
    begin
      xBufferTemp := xBuffer;
      Delete(xBufferTemp, 1, Pos('<|FILESSOCKET|>', xBuffer) + 14);
      xID := Copy(xBufferTemp, 1, Pos('<|END|>', xBufferTemp) - 1);
      DMServer.Conexoes.RetornaItemPorID(xID).CriarThread(ttArquivos, scClient);
      Break;
    end;
  end;
end;

{ TThreadBase }

function TThreadBase.LengthAcessos: Integer;
begin
  Result := Length(arrAcessos);
end;

procedure TThreadBase.RemoverAcesso(AHandle: Integer);
var
  i: Integer;
begin
  for i := Low(arrAcessos) to High(arrAcessos) do
  begin
    if (Assigned(arrAcessos[i])) and (AHandle = arrAcessos[i].Handle) then
    begin
      if LengthAcessos = 1 then
        scClient.SendText('<|DISCONNECTED|>');
      arrAcessos[i] := nil;
    end;
  end;
end;

constructor TThreadBase.Create(ASocket: TCustomWinSocket; AProtocolo: string);
begin
  inherited Create(True);
  FProtocolo := AProtocolo;
  scClient := ASocket;
  FreeOnTerminate := True;
  OnTerminate := ThreadTerminate;
  Resume;
end;

procedure TThreadBase.Execute;
var
  xBuffer: string;
  i: Integer;
begin
  inherited;

  while True do
  begin
    Sleep(FOLGAPROCESSAMENTO);

    if (scClient = nil)
      or not(scClient.Connected)
      or (Terminated)
      or not(Assigned(DMServer)) then
      Break;

    if scClient.ReceiveLength < 1 then
      Continue;

    xBuffer := scClient.ReceiveText;

    for i := Low(arrAcessos) to High(arrAcessos) do
    begin
      if (Assigned(arrAcessos[i])) and (arrAcessos[i].Connected) then
      begin
        while arrAcessos[i].SendText(xBuffer) < 0 do
          Sleep(FOLGAPROCESSAMENTO);
      end;
    end;
  end;
end;

function TThreadBase.GetAcesso(AHandle: Integer): TCustomWinSocket;
var
  i: Integer;
begin
  Result := nil;
  for i := Low(arrAcessos) to High(arrAcessos) do
  begin
    if (Assigned(arrAcessos[i])) and (AHandle = arrAcessos[i].Handle) then
    begin
      Result := arrAcessos[i];
      Break;
    end;
  end;
end;

procedure TThreadBase.SetAcesso(ASocket: TCustomWinSocket);
var
  i: Integer;
  bAchou: Boolean;
begin
  bAchou := False;

  for i := Low(arrAcessos) to High(arrAcessos) do
  begin
    if (Assigned(arrAcessos[i])) and (arrAcessos[i].Handle = ASocket.Handle) then
      bAchou := True;
  end;

  if not bAchou then
  begin
    i := Length(arrAcessos) + 1;
    SetLength(arrAcessos, i);
    arrAcessos[High(arrAcessos)] := ASocket;
  end;
end;

procedure TThreadBase.ThreadTerminate(ASender: TObject);
begin
  if (Assigned(DMServer)) and (not Terminated) then
    DMServer.Conexoes.RetornaItemPorConexao(FProtocolo).LimparThread(FTipo);
end;

{ TThreadConexaoPrincipal }

constructor TThreadConexaoPrincipal.Create(ASocket: TCustomWinSocket; AProtocolo: string);
begin
  FTipo := ttPrincipal;
  inherited;
end;

procedure TThreadConexaoPrincipal.Execute;
var
  xBuffer, xBufferTemp, xID, xIDAcesso, xSenhaAcesso: string;
  iPosition: Integer;
  FConexao, FConexaoAcesso: TConexao;
  i: Integer;
begin
  FConexao := DMServer.Conexoes.RetornaItemPorConexao(FProtocolo);

  while scClient.SendText('<|ID|>' + FConexao.ID + '<|>' + FConexao.Senha + '<|END|>') < 0 do
    Sleep(FOLGAPROCESSAMENTO);

  while True do
  begin
    Sleep(FOLGAPROCESSAMENTO);

    if (scClient = nil)
      or not(scClient.Connected)
      or (Terminated)
      or not(Assigned(DMServer)) then
      Break;

    if scClient.ReceiveLength < 1 then
      Continue;

    xBuffer := scClient.ReceiveText;

    iPosition := Pos('<|FINDID|>', xBuffer);
    if iPosition > 0 then
    begin
      xBufferTemp := xBuffer;
      Delete(xBufferTemp, 1, iPosition + 9);
      xIDAcesso := Copy(xBufferTemp, 1, Pos('<|END|>', xBufferTemp) - 1);

      if DMServer.Conexoes.VerificaID(xIDAcesso) then
      begin
        while scClient.SendText('<|IDEXISTS!REQUESTPASSWORD|>') < 0 do
          Sleep(FOLGAPROCESSAMENTO);
      end
      else
      begin
        while scClient.SendText('<|IDNOTEXISTS|>') < 0 do
          Sleep(FOLGAPROCESSAMENTO);
      end;
    end;

    if xBuffer.Contains('<|PONG|>') then
      FConexao.PingFinal := GetTickCount - FConexao.PingInicial;

    iPosition := Pos('<|CHECKIDPASSWORD|>', xBuffer);
    if iPosition > 0 then
    begin
      xIDAcesso := '';
      xSenhaAcesso := '';

      xBufferTemp := xBuffer;
      Delete(xBufferTemp, 1, iPosition + 18);
      iPosition := Pos('<|>', xBufferTemp);
      xIDAcesso := Copy(xBufferTemp, 1, iPosition - 1);

      Delete(xBufferTemp, 1, iPosition + 2);
      xSenhaAcesso := Copy(xBufferTemp, 1, Pos('<|END|>', xBufferTemp) - 1);

      if (DMServer.Conexoes.VerificaIDSenha(xIDAcesso, xSenhaAcesso)) then
      begin
        while scClient.SendText('<|ACCESSGRANTED|>') < 0 do
          Sleep(FOLGAPROCESSAMENTO);
      end
      else
      begin
        while scClient.SendText('<|ACCESSDENIED|>') < 0 do
          Sleep(FOLGAPROCESSAMENTO);
      end;
    end;

    iPosition := Pos('<|RELATION|>', xBuffer);
    if iPosition > 0 then
    begin
      xID := '';
      xIDAcesso := '';

      xBufferTemp := xBuffer;
      Delete(xBufferTemp, 1, iPosition + 11);
      iPosition := Pos('<|>', xBufferTemp);
      xID := Copy(xBufferTemp, 1, iPosition - 1);

      Delete(xBufferTemp, 1, iPosition + 2);
      xIDAcesso := Copy(xBufferTemp, 1, Pos('<|END|>', xBufferTemp) - 1);

      FConexao := nil;
      FConexaoAcesso := nil;
      FConexao := DMServer.Conexoes.RetornaItemPorID(xID);
      FConexaoAcesso := DMServer.Conexoes.RetornaItemPorID(xIDAcesso);

      // RECONNECT SOCKET CLIENT

      FConexao.ThreadPrincipal.SetAcesso(FConexaoAcesso.ThreadPrincipal.scClient);
      FConexaoAcesso.ThreadPrincipal.SetAcesso(FConexao.ThreadPrincipal.scClient);

      FConexao.ThreadAreaRemota.SetAcesso(FConexaoAcesso.ThreadAreaRemota.scClient);
      FConexaoAcesso.ThreadAreaRemota.SetAcesso(FConexao.ThreadAreaRemota.scClient);

      FConexao.ThreadTeclado.SetAcesso(FConexaoAcesso.ThreadTeclado.scClient);

      FConexao.ThreadArquivos.SetAcesso(FConexaoAcesso.ThreadArquivos.scClient);
      FConexaoAcesso.ThreadArquivos.SetAcesso(FConexao.ThreadArquivos.scClient);

      FConexaoAcesso.ThreadPrincipal.scClient.SendText('<|ACCESSING|>');

      FConexaoAcesso.ThreadAreaRemota.scClient.SendText('<|GETFULLSCREENSHOT|>');
    end;

    // Stop relations
    if xBuffer.Contains('<|STOPACCESS|>') then
    begin
      LimparAcessos;
      //erro aqui, quando eu envio o disconnect para quem fechou o acesso, quem ainda está acessando perde as imagens e não volta mais
//      scClient.SendText('<|DISCONNECTED|>');
    end;

    // Redirect commands
    iPosition := Pos('<|REDIRECT|>', xBuffer);
    if iPosition > 0 then
    begin
      xBufferTemp := xBuffer;
      Delete(xBufferTemp, 1, iPosition + 11);

      if (Pos('<|FOLDERLIST|>', xBufferTemp) > 0) then
      begin
        while (scClient.Connected) do
        begin
          Sleep(FOLGAPROCESSAMENTO); // Avoids using 100% CPU

          if (Pos('<|ENDFOLDERLIST|>', xBufferTemp) > 0) then
            Break;

          xBufferTemp := xBufferTemp + scClient.ReceiveText;
        end;
      end;

      if (Pos('<|FILESLIST|>', xBufferTemp) > 0) then
      begin
        while (scClient.Connected) do
        begin
          Sleep(FOLGAPROCESSAMENTO); // Avoids using 100% CPU

          if (Pos('<|ENDFILESLIST|>', xBufferTemp) > 0) then
            Break;

          xBufferTemp := xBufferTemp + scClient.ReceiveText;
        end;
      end;

      for i := Low(arrAcessos) to High(arrAcessos) do
      begin
        if (Assigned(arrAcessos[i])) and (arrAcessos[i].Connected) then
        begin
          while arrAcessos[i].SendText(xBuffer) < 0 do
            Sleep(FOLGAPROCESSAMENTO);
        end;
      end;
    end;
  end;
end;

procedure TThreadConexaoPrincipal.LimparAcessos;
var
  i: Integer;
  Conexao: TConexao;
begin
  for i := Low(arrAcessos) to High(arrAcessos) do
  begin
    if Assigned(arrAcessos[i]) then
    begin
      Conexao := DMServer.Conexoes.RetornaItemPorHandle(arrAcessos[i].Handle);
      if Assigned(Conexao) then
      begin
        Conexao.ThreadPrincipal.RemoverAcesso(scClient.Handle);
        arrAcessos[i] := nil;
      end;
    end;
  end;
  SetLength(arrAcessos, 0);
end;

procedure TThreadConexaoPrincipal.ThreadTerminate(ASender: TObject);
var
  Conexao: TConexao;
  i: Integer;
begin
  if Assigned(DMServer) then
  begin
    LimparAcessos;

    Conexao := DMServer.Conexoes.RetornaItemPorConexao(FProtocolo);

    if not Terminated then
      Conexao.LimparThread(ttPrincipal);

    DMServer.Conexoes.RemoverConexao(Conexao.Protocolo);
  end;
end;

{ TThreadConexaoAreaRemota }

constructor TThreadConexaoAreaRemota.Create(ASocket: TCustomWinSocket; AProtocolo: string);
begin
  FTipo := ttAreaRemota;
  inherited;
end;

{ TThreadConexaoTeclado }

constructor TThreadConexaoTeclado.Create(ASocket: TCustomWinSocket; AProtocolo: string);
begin
  FTipo := ttTeclado;
  inherited;
end;

{ TThreadConexaoArquivos }

constructor TThreadConexaoArquivos.Create(ASocket: TCustomWinSocket; AProtocolo: string);
begin
  FTipo := ttArquivos;
  inherited;
end;

end.
