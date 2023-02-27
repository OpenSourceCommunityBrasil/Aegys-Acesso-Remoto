unit uCtrl_ThreadsServer;

interface

uses
  System.Classes, System.Win.ScktComp, uFrameConexao;

type
  TThreadConexaoDefinidor = class(TThread)
  private
    scDefinidor: TCustomWinSocket;
  public
    constructor Create(ASocket: TCustomWinSocket); overload;
    procedure Execute; override;
  end;

  TThreadConexaoPrincipal = class(TThread)
  private
    scAcessoPrincipal: TCustomWinSocket;
    xID, xSenha, xIDAcesso, xSenhaAcesso: string;
  public
    iPingInicial, iPingFinal: Int64;
    scPrincipal: TCustomWinSocket;
    constructor Create(ASocket: TCustomWinSocket); overload;
    procedure Execute; override;
  end;

  TThreadConexaoAreaRemota = class(TThread)
  private
    scAreaRemota: TCustomWinSocket;
    scAcessoAreaRemota: TCustomWinSocket;
    xMeuID: string;
  public
    constructor Create(ASocket: TCustomWinSocket; AID: string); overload;
    procedure Execute; override;
  end;

  TThreadConexaoTeclado = class(TThread)
  private
    scTeclado: TCustomWinSocket;
    scAcessoTeclado: TCustomWinSocket;
    xMeuID: string;
  public
    constructor Create(ASocket: TCustomWinSocket; AID: string); overload;
    procedure Execute; override;
  end;

  TThreadConexaoArquivos = class(TThread)
  private
    scArquivos: TCustomWinSocket;
    scAcessoArquivos: TCustomWinSocket;
    xMeuID: string;
  public
    constructor Create(ASocket: TCustomWinSocket; AID: string); overload;
    procedure Execute; override;
  end;

const
  PORT = 3898;
  PROCESSINGSLACK = 2;

implementation

{ TThreadConexaoDefinidor }

uses uFormServidor, System.SysUtils, FMX.Types;

constructor TThreadConexaoDefinidor.Create(ASocket: TCustomWinSocket);
begin
  inherited Create(False);
  scDefinidor := ASocket;
  FreeOnTerminate := True;
end;

procedure TThreadConexaoDefinidor.Execute;
var
  xBuffer, xBufferTemp, xID: string;
  iPosition: Integer;
  ThreadMain: TThreadConexaoPrincipal;
  ThreadDesktop: TThreadConexaoAreaRemota;
  ThreadKeyboard: TThreadConexaoTeclado;
  ThreadFiles: TThreadConexaoArquivos;
begin
  inherited;
  while True do
  begin
    Sleep(PROCESSINGSLACK);

    if (scDefinidor = nil) or not(scDefinidor.Connected) then
      Break;

    if scDefinidor.ReceiveLength < 1 then
      Continue;

    xBuffer := scDefinidor.ReceiveText;

    iPosition := Pos('<|MAINSOCKET|>', xBuffer);
    if iPosition > 0 then
    begin
      ThreadMain := TThreadConexaoPrincipal.Create(scDefinidor);
      Break;
    end;

    iPosition := Pos('<|DESKTOPSOCKET|>', xBuffer);
    if iPosition > 0 then
    begin
      xBufferTemp := xBuffer;
      Delete(xBufferTemp, 1, iPosition + 16);
      xID := Copy(xBufferTemp, 1, Pos('<|END|>', xBufferTemp) - 1);
      ThreadDesktop := TThreadConexaoAreaRemota.Create(scDefinidor, xID);
      Break;
    end;

    iPosition := Pos('<|KEYBOARDSOCKET|>', xBuffer);
    if iPosition > 0 then
    begin
      xBufferTemp := xBuffer;
      Delete(xBufferTemp, 1, iPosition + 17);
      xID := Copy(xBufferTemp, 1, Pos('<|END|>', xBufferTemp) - 1);
      ThreadKeyboard := TThreadConexaoTeclado.Create(scDefinidor, xID);
      Break;
    end;

    iPosition := Pos('<|FILESSOCKET|>', xBuffer);
    if iPosition > 0 then
    begin
      xBufferTemp := xBuffer;
      Delete(xBufferTemp, 1, Pos('<|FILESSOCKET|>', xBuffer) + 14);
      xID := Copy(xBufferTemp, 1, Pos('<|END|>', xBufferTemp) - 1);
      ThreadFiles := TThreadConexaoArquivos.Create(scDefinidor, xID);
      Break;
    end;
  end;
end;

{ TThreadConexaoPrincipal }

constructor TThreadConexaoPrincipal.Create(ASocket: TCustomWinSocket);
begin
  inherited Create(False);
  scPrincipal := ASocket;
  iPingInicial := 0;
  iPingFinal := 256;
  FreeOnTerminate := True;
end;

procedure TThreadConexaoPrincipal.Execute;
var
  xBuffer, xBufferTemp: string;
  iPosition: Integer;
  fFrame, fFrame2: TFrameConexao;
  recConexao: TConexaoRec;
begin
  inherited;

  Synchronize(
    procedure
    begin
      xID := FormServidor.GerarID;
      xSenha := FormServidor.GerarSenha;

      FormServidor.AdicionarItens(
        IntToStr(scPrincipal.Handle),
        scPrincipal.RemoteAddress,
        xID,
        xSenha);
    end);

  fFrame := FormServidor.RetornaItemPorConexao(IntToStr(scPrincipal.Handle));
  if fFrame <> nil then
  begin
    recConexao := fFrame.Conexao;
    recConexao.ThreadPrincipal := TObject(Self);
    fFrame.Conexao := recConexao;
  end;

  while scPrincipal.SendText('<|ID|>' + xID + '<|>' + xSenha + '<|END|>') < 0 do
    Sleep(PROCESSINGSLACK);

  while True do
  begin
    Sleep(PROCESSINGSLACK);

    if (scPrincipal = nil) or not(scPrincipal.Connected) then
      Break;

    if scPrincipal.ReceiveLength < 1 then
      Continue;

    xBuffer := scPrincipal.ReceiveText;

    iPosition := Pos('<|FINDID|>', xBuffer);
    if iPosition > 0 then
    begin
      xBufferTemp := xBuffer;
      Delete(xBufferTemp, 1, iPosition + 9);
      xIDAcesso := Copy(xBufferTemp, 1, Pos('<|END|>', xBufferTemp) - 1);

      if (FormServidor.VerificaID(xIDAcesso)) then
      begin
        if (FormServidor.RetornaItemPorID(xIDAcesso).Conexao.IDParceiro = '') then
        begin
          while scPrincipal.SendText('<|IDEXISTS!REQUESTPASSWORD|>') < 0 do
            Sleep(PROCESSINGSLACK);
        end
        else
        begin
          while scPrincipal.SendText('<|ACCESSBUSY|>') < 0 do
            Sleep(PROCESSINGSLACK);
        end
      end
      else
      begin
        while scPrincipal.SendText('<|IDNOTEXISTS|>') < 0 do
          Sleep(PROCESSINGSLACK);
      end;
    end;

    if xBuffer.Contains('<|PONG|>') then
    begin
      iPingFinal := GetTickCount - iPingInicial;
      Synchronize(
        procedure
        begin
          FormServidor.InserirLatencia(IntToStr(scPrincipal.Handle), iPingFinal);
        end);
    end;

    iPosition := Pos(
      '<|CHECKIDPASSWORD|>', xBuffer);
    if iPosition > 0 then
    begin
      xBufferTemp := xBuffer;
      Delete(xBufferTemp, 1, iPosition + 18);
      iPosition := Pos('<|>', xBufferTemp);
      xIDAcesso := Copy(xBufferTemp, 1, iPosition - 1);
      Delete(xBufferTemp, 1, iPosition + 2);
      xSenhaAcesso := Copy(xBufferTemp, 1, Pos('<|END|>', xBufferTemp) - 1);

      if (FormServidor.VerificaIDSenha(xIDAcesso, xSenhaAcesso)) then
      begin
        while scPrincipal.SendText('<|ACCESSGRANTED|>') < 0 do
          Sleep(PROCESSINGSLACK);
      end
      else
      begin
        while scPrincipal.SendText('<|ACCESSDENIED|>') < 0 do
          Sleep(PROCESSINGSLACK);
      end;
    end;

    iPosition := Pos('<|RELATION|>', xBuffer);
    if iPosition > 0 then
    begin
      xBufferTemp := xBuffer;
      Delete(xBufferTemp, 1, iPosition + 11);
      iPosition := Pos('<|>', xBufferTemp);
      xID := Copy(xBufferTemp, 1, iPosition - 1);
      Delete(xBufferTemp, 1, iPosition + 2);
      xIDAcesso := Copy(xBufferTemp, 1, Pos('<|END|>', xBufferTemp) - 1);
      fFrame := FormServidor.RetornaItemPorID(xID);
      fFrame2 := FormServidor.RetornaItemPorID(xIDAcesso);
      Synchronize(
        procedure
        begin
          FormServidor.InserirIDAcesso(IntToStr(scPrincipal.Handle), xIDAcesso);
        end);

      // Relates the main Sockets
      TThreadConexaoPrincipal(fFrame.Conexao.ThreadPrincipal).scAcessoPrincipal := TThreadConexaoPrincipal(fFrame2.Conexao.ThreadPrincipal).scPrincipal;
      TThreadConexaoPrincipal(fFrame2.Conexao.ThreadPrincipal).scAcessoPrincipal := TThreadConexaoPrincipal(fFrame.Conexao.ThreadPrincipal).scPrincipal;
      // Relates the Remote Desktop
      TThreadConexaoAreaRemota(fFrame.Conexao.ThreadAreaRemota).scAcessoAreaRemota := TThreadConexaoAreaRemota(fFrame2.Conexao.ThreadAreaRemota).scAreaRemota;
      TThreadConexaoAreaRemota(fFrame2.Conexao.ThreadAreaRemota).scAcessoAreaRemota := TThreadConexaoAreaRemota(fFrame.Conexao.ThreadAreaRemota).scAreaRemota;
      // Relates the Keyboard Socket
      TThreadConexaoTeclado(fFrame.Conexao.ThreadTeclado).scAcessoTeclado := TThreadConexaoTeclado(fFrame2.Conexao.ThreadTeclado).scTeclado;
      // Relates the Share Files
      TThreadConexaoArquivos(fFrame.Conexao.ThreadArquivos).scAcessoArquivos := TThreadConexaoArquivos(fFrame2.Conexao.ThreadArquivos).scArquivos;
      TThreadConexaoArquivos(fFrame2.Conexao.ThreadArquivos).scAcessoArquivos := TThreadConexaoArquivos(fFrame.Conexao.ThreadArquivos).scArquivos;
      // Warns Access
      TThreadConexaoPrincipal(fFrame.Conexao.ThreadPrincipal).scAcessoPrincipal.SendText('<|ACCESSING|>');
      // Get first screenshot
      TThreadConexaoAreaRemota(fFrame.Conexao.ThreadAreaRemota).scAcessoAreaRemota.SendText('<|GETFULLSCREENSHOT|>');
    end;

    // Stop relations
    if xBuffer.Contains('<|STOPACCESS|>') then
    begin
      scPrincipal.SendText('<|DISCONNECTED|>');
      scAcessoPrincipal.SendText('<|DISCONNECTED|>');
      scAcessoPrincipal := nil;
      TThreadConexaoPrincipal(fFrame2.Conexao.ThreadPrincipal).scAcessoPrincipal := nil;
      Synchronize(
        procedure
        begin
          recConexao := fFrame.Conexao;
          recConexao.IDParceiro := '';
          fFrame.Conexao := recConexao;

          recConexao := fFrame2.Conexao;
          recConexao.IDParceiro := '';
          fFrame2.Conexao := recConexao;
        end);
    end;

    // Redirect commands
    iPosition := Pos('<|REDIRECT|>', xBuffer);
    if iPosition > 0 then
    begin
      xBufferTemp := xBuffer;
      Delete(xBufferTemp, 1, iPosition + 11);

      if (Pos('<|FOLDERLIST|>', xBufferTemp) > 0) then
      begin
        while (scPrincipal.Connected) do
        begin
          Sleep(PROCESSINGSLACK); // Avoids using 100% CPU

          if (Pos('<|ENDFOLDERLIST|>', xBufferTemp) > 0) then
            Break;

          xBufferTemp := xBufferTemp + scPrincipal.ReceiveText;
        end;
      end;

      if (Pos('<|FILESLIST|>', xBufferTemp) > 0) then
      begin
        while (scPrincipal.Connected) do
        begin
          Sleep(PROCESSINGSLACK); // Avoids using 100% CPU

          if (Pos('<|ENDFILESLIST|>', xBufferTemp) > 0) then
            Break;

          xBufferTemp := xBufferTemp + scPrincipal.ReceiveText;
        end;
      end;

      if (scAcessoPrincipal <> nil) and (scAcessoPrincipal.Connected) then
      begin
        while scAcessoPrincipal.SendText(xBufferTemp) < 0 do
          Sleep(PROCESSINGSLACK);
      end;
    end;
  end;

  if (scAcessoPrincipal <> nil) and (scAcessoPrincipal.Connected) then
  begin
    while scAcessoPrincipal.SendText('<|DISCONNECTED|>') < 0 do
      Sleep(PROCESSINGSLACK);
  end;

  Synchronize(
    procedure
    begin
      fFrame2 := FormServidor.RetornaItemPorID(fFrame.Conexao.IDParceiro);

      if fFrame2 <> nil then
      begin
        recConexao := fFrame2.Conexao;
        recConexao.IDParceiro := '';
        fFrame2.Conexao := recConexao;
      end;

      FormServidor.RemoveItemPorConexao(fFrame.Conexao.Conexao);
    end);
end;

{ TThreadConexaoAreaRemota }

constructor TThreadConexaoAreaRemota.Create(ASocket: TCustomWinSocket;
AID:
  string);
begin
  inherited Create(False);
  scAreaRemota := ASocket;
  xMeuID := AID;
  FreeOnTerminate := True;
end;

procedure TThreadConexaoAreaRemota.Execute;
var
  xBuffer: string;
  fFrame: TFrameConexao;
  recConexao: TConexaoRec;
begin
  inherited;

  fFrame := FormServidor.RetornaItemPorID(xMeuID);
  if fFrame <> nil then
  begin
    recConexao := fFrame.Conexao;
    recConexao.ThreadAreaRemota := TObject(Self);
    fFrame.Conexao := recConexao;
  end;

  while True do
  begin
    Sleep(PROCESSINGSLACK);

    if (scAreaRemota = nil) or not(scAreaRemota.Connected) then
      Break;

    if scAreaRemota.ReceiveLength < 1 then
      Continue;

    xBuffer := scAreaRemota.ReceiveText;

    if (scAcessoAreaRemota <> nil) and (scAcessoAreaRemota.Connected) then
    begin
      while scAcessoAreaRemota.SendText(xBuffer) < 0 do
        Sleep(PROCESSINGSLACK);
    end;
  end;
end;

{ TThreadConexaoTeclado }

constructor TThreadConexaoTeclado.Create(ASocket: TCustomWinSocket;
AID:
  string);
begin
  inherited Create(False);
  scTeclado := ASocket;
  xMeuID := AID;
  FreeOnTerminate := True;
end;

procedure TThreadConexaoTeclado.Execute;
var
  xBuffer: string;
  fFrame: TFrameConexao;
  recConexao: TConexaoRec;
begin
  inherited;

  fFrame := FormServidor.RetornaItemPorID(xMeuID);
  if fFrame <> nil then
  begin
    recConexao := fFrame.Conexao;
    recConexao.ThreadTeclado := TObject(Self);
    fFrame.Conexao := recConexao;
  end;

  while True do
  begin
    Sleep(PROCESSINGSLACK);

    if (scTeclado = nil) or not(scTeclado.Connected) then
      Break;

    if scTeclado.ReceiveLength < 1 then
      Continue;

    xBuffer := scTeclado.ReceiveText;

    if (scAcessoTeclado <> nil) and (scAcessoTeclado.Connected) then
    begin
      while scAcessoTeclado.SendText(xBuffer) < 0 do
        Sleep(PROCESSINGSLACK);
    end;
  end;
end;

{ TThreadConexaoArquivos }

constructor TThreadConexaoArquivos.Create(ASocket: TCustomWinSocket;
AID:
  string);
begin
  inherited Create(False);
  scArquivos := ASocket;
  xMeuID := AID;
  FreeOnTerminate := True;
end;

procedure TThreadConexaoArquivos.Execute;
var
  xBuffer: string;
  fFrame: TFrameConexao;
  recConexao: TConexaoRec;
begin
  inherited;

  fFrame := FormServidor.RetornaItemPorID(xMeuID);
  if fFrame <> nil then
  begin
    recConexao := fFrame.Conexao;
    recConexao.ThreadArquivos := TObject(Self);
    fFrame.Conexao := recConexao;
  end;

  while True do
  begin
    Sleep(PROCESSINGSLACK);

    if (scArquivos = nil) or not(scArquivos.Connected) then
      Break;

    if scArquivos.ReceiveLength < 1 then
      Continue;

    xBuffer := scArquivos.ReceiveText;

    if (scAcessoArquivos <> nil) and (scAcessoArquivos.Connected) then
    begin
      while scAcessoArquivos.SendText(xBuffer) < 0 do
        Sleep(PROCESSINGSLACK);
    end;
  end;
end;

end.
