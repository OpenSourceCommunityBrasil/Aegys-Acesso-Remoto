unit uCtrl_Threads;

interface

uses
  System.Classes, System.Win.ScktComp;

type
  TThreadConexaoPrincipal = class(TThread)
    Socket: TCustomWinSocket;
    constructor Create(ASocket: TCustomWinSocket); overload;
    procedure Execute; override;
    procedure ThreadTerminate(ASender: TObject);
  end;

  TThreadConexaoAreaRemota = class(TThread)
    Socket: TCustomWinSocket;
    constructor Create(ASocket: TCustomWinSocket); overload;
    procedure Execute; override;
    procedure ThreadTerminate(ASender: TObject);
  end;

  TThreadConexaoTeclado = class(TThread)
    Socket: TCustomWinSocket;
    constructor Create(ASocket: TCustomWinSocket); overload;
    procedure Execute; override;
    procedure ThreadTerminate(ASender: TObject);
  end;

  TThreadConexaoArquivos = class(TThread)
    Socket: TCustomWinSocket;
    constructor Create(ASocket: TCustomWinSocket); overload;
    procedure Execute; override;
    procedure ThreadTerminate(ASender: TObject);
  end;

implementation

{ TConexaoPrincipal }

uses uFormArquivos, Winapi.Windows, uFormChat, uFormConexao, uFormTelaRemota, FMX.Forms,
  FMX.ListView.Types, System.SysUtils, uLibClass, uFormSenha, FMX.Platform.Win, uSendKeyClass,
  System.Rtti, FMX.Platform, FMX.Surfaces, StreamManager, FMX.Graphics,
  uConstants;

constructor TThreadConexaoPrincipal.Create(ASocket: TCustomWinSocket);
begin
  inherited Create(True);
  Socket := ASocket;
  FreeOnTerminate := True;
  OnTerminate := ThreadTerminate;
  Resume;
end;

procedure TThreadConexaoPrincipal.Execute;
var
  Buffer: string;
  BufferTemp: string;
  Extension: string;
  i: Integer;
  Position: Integer;
  MousePosX: Integer;
  MousePosY: Integer;
  FoldersAndFiles: TStringList;
  L: TListItem;
  FileToUpload: TFileStream;
  hDesktop: HDESK;
  Svc: IFMXClipboardService;
begin
  inherited;

  FoldersAndFiles := nil;
  FileToUpload := nil;

  while (Socket.Connected) and (not Terminated) do
  begin
    Sleep(FOLGAPROCESSAMENTO); // Avoids using 100% CPU

    if (Socket = nil) or not(Socket.Connected) or (Terminated) then
      Break;

    if Socket.ReceiveLength < 1 then
      Continue;

    Buffer := Socket.ReceiveText;

    // Received data, then resets the timeout
    Conexao.Intervalo := 0;

    // EUREKA: This is the responsable to interact with UAC. But we need run
    // the software on SYSTEM account to work.
    hDesktop := OpenInputDesktop(0, True, MAXIMUM_ALLOWED);
    if hDesktop <> 0 then
    begin
      SetThreadDesktop(hDesktop);
      CloseHandle(hDesktop);
    end;

    // If receive ID, are Online
    Position := Pos('<|ID|>', Buffer);
    if Position > 0 then
    begin
      BufferTemp := Buffer;
      Delete(BufferTemp, 1, Position + 5);
      Position := Pos('<|>', BufferTemp);
      Conexao.ID := Copy(BufferTemp, 1, Position - 1);
      Delete(BufferTemp, 1, Position + 2);
      Conexao.Senha := Copy(BufferTemp, 1, Pos('<|END|>', BufferTemp) - 1);
      Synchronize(FormConexao.SetOnline);

      // If this Socket are connected, then connect the Desktop Socket, Keyboard Socket, File Download Socket and File Upload Socket
      Synchronize(
        procedure
        begin
          Conexao.SocketAreaRemota.Active := True;
          Conexao.SocketTeclado.Active := True;
          Conexao.SocketArquivos.Active := True;
        end);
    end;

    // Ping
    if Buffer.Contains('<|PING|>') then
      Socket.SendText('<|PONG|>');

    Position := Pos('<|SETPING|>', Buffer);
    if Position > 0 then
    begin
      BufferTemp := Buffer;
      Delete(BufferTemp, 1, Position + 10);
      BufferTemp := Copy(BufferTemp, 1, Pos('<|END|>', BufferTemp) - 1);
      Conexao.Latencia := StrToInt(BufferTemp);
    end;

    // Warns access and remove Wallpaper
    if Buffer.Contains('<|ACCESSING|>') then
    begin
      Synchronize(
        procedure
        begin
          FormConexao.btnConectar.Enabled := False;
          FormConexao.MudarStatusConexao(3, 'Suporte remoto conectado!');
        end);
      Conexao.Acessando := True;
    end;

    if Buffer.Contains('<|IDEXISTS!REQUESTPASSWORD|>') then
    begin
      Synchronize(
        procedure
        begin
          FormConexao.MudarStatusConexao(0, 'Aguardando autenticação...');
          FormSenha.ShowModal;
        end);
    end;

    if Buffer.Contains('<|IDNOTEXISTS|>') then
    begin
      Synchronize(
        procedure
        begin
          FormConexao.MudarStatusConexao(2, 'ID não existe.');
          FormConexao.btnConectar.Enabled := True;
        end);
    end;

    if Buffer.Contains('<|ACCESSDENIED|>') then
    begin
      Synchronize(
        procedure
        begin
          FormConexao.MudarStatusConexao(2, 'Senha errada!');
          FormConexao.btnConectar.Enabled := True;
        end);
    end;

    if Buffer.Contains('<|ACCESSBUSY|>') then
    begin
      Synchronize(
        procedure
        begin
          FormConexao.MudarStatusConexao(2, 'Computador ocupado!');
          FormConexao.btnConectar.Enabled := True;
        end);
    end;

    if Buffer.Contains('<|ACCESSGRANTED|>') then
    begin
      Synchronize(
        procedure
        begin
          FormConexao.MudarStatusConexao(3, 'Acesso garantido!');
          Conexao.Visualizador := True;
          FormConexao.tmrClipboard.Enabled := True;
          FormConexao.LimparConexao;
          FormTelaRemota.Show;
          FormConexao.Hide;
          Socket.SendText('<|RELATION|>' + Conexao.ID + '<|>' + FormConexao.txtIDParceiro.Text + '<|END|>');
        end);
    end;

    if Buffer.Contains('<|DISCONNECTED|>') then
    begin
      Synchronize(
        procedure
        begin
          FormTelaRemota.Hide;
          FormArquivos.Hide;
          FormChat.Hide;
          Conexao.ReconectarSocketsSecundarios;
          FormConexao.Show;
          FormConexao.SetOnline;
          FormConexao.MudarStatusConexao(2, 'Conexão perdida!');
          FlashWindow(FmxHandleToHWND(FormConexao.Handle), True);
        end);
    end;

    { Redirected commands }

    if Buffer.Contains('<|SHOWMOUSE|>') then
      Conexao.MostrarMouse := True;

    if Buffer.Contains('<|HIDEMOUSE|>') then
      Conexao.MostrarMouse := False;

    // Desktop Remote
    Position := Pos('<|RESOLUTION|>', Buffer);
    if Position > 0 then
    begin
      BufferTemp := Buffer;
      Delete(BufferTemp, 1, Position + 13);
      Position := Pos('<|>', BufferTemp);
      Conexao.ResolucaoLargura := StrToInt(Copy(BufferTemp, 1, Position - 1));
      Delete(BufferTemp, 1, Position + 2);
      Conexao.ResolucaoAltura := StrToInt(Copy(BufferTemp, 1, Pos('<|END|>', BufferTemp) - 1));
      FormTelaRemota.PROC_REDIMENSIONARExecute(nil);
    end;

    Position := Pos('<|SETMOUSEPOS|>', Buffer);
    if Position > 0 then
    begin
      BufferTemp := Buffer;
      Delete(BufferTemp, 1, Position + 14);
      Position := Pos('<|>', BufferTemp);
      MousePosX := StrToInt(Copy(BufferTemp, 1, Position - 1));
      Delete(BufferTemp, 1, Position + 2);
      MousePosY := StrToInt(Copy(BufferTemp, 1, Pos('<|END|>', BufferTemp) - 1));
      SetCursorPos(MousePosX, MousePosY);
    end;

    Position := Pos('<|SETMOUSELEFTCLICKDOWN|>', Buffer);
    if Position > 0 then
    begin
      BufferTemp := Buffer;
      Delete(BufferTemp, 1, Position + 24);
      Position := Pos('<|>', BufferTemp);
      MousePosX := StrToInt(Copy(BufferTemp, 1, Position - 1));
      Delete(BufferTemp, 1, Position + 2);
      MousePosY := StrToInt(Copy(BufferTemp, 1, Pos('<|END|>', BufferTemp) - 1));
      SetCursorPos(MousePosX, MousePosY);
      Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
    end;

    Position := Pos('<|SETMOUSELEFTCLICKUP|>', Buffer);
    if Position > 0 then
    begin
      BufferTemp := Buffer;
      Delete(BufferTemp, 1, Position + 22);
      Position := Pos('<|>', BufferTemp);
      MousePosX := StrToInt(Copy(BufferTemp, 1, Position - 1));
      Delete(BufferTemp, 1, Position + 2);
      MousePosY := StrToInt(Copy(BufferTemp, 1, Pos('<|END|>', BufferTemp) - 1));
      SetCursorPos(MousePosX, MousePosY);
      Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
    end;

    Position := Pos('<|SETMOUSERIGHTCLICKDOWN|>', Buffer);
    if Position > 0 then
    begin
      BufferTemp := Buffer;
      Delete(BufferTemp, 1, Position + 25);
      Position := Pos('<|>', BufferTemp);
      MousePosX := StrToInt(Copy(BufferTemp, 1, Position - 1));
      Delete(BufferTemp, 1, Position + 2);
      MousePosY := StrToInt(Copy(BufferTemp, 1, Pos('<|END|>', BufferTemp) - 1));
      SetCursorPos(MousePosX, MousePosY);
      Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_RIGHTDOWN, 0, 0, 0, 0);
    end;

    Position := Pos('<|SETMOUSERIGHTCLICKUP|>', Buffer);
    if Position > 0 then
    begin
      BufferTemp := Buffer;
      Delete(BufferTemp, 1, Position + 23);
      Position := Pos('<|>', BufferTemp);
      MousePosX := StrToInt(Copy(BufferTemp, 1, Position - 1));
      Delete(BufferTemp, 1, Position + 2);
      MousePosY := StrToInt(Copy(BufferTemp, 1, Pos('<|END|>', BufferTemp) - 1));
      SetCursorPos(MousePosX, MousePosY);
      Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_RIGHTUP, 0, 0, 0, 0);
    end;

    Position := Pos('<|SETMOUSEMIDDLEDOWN|>', Buffer);
    if Position > 0 then
    begin
      BufferTemp := Buffer;
      Delete(BufferTemp, 1, Position + 21);
      Position := Pos('<|>', BufferTemp);
      MousePosX := StrToInt(Copy(BufferTemp, 1, Position - 1));
      Delete(BufferTemp, 1, Position + 2);
      MousePosY := StrToInt(Copy(BufferTemp, 1, Pos('<|END|>', BufferTemp) - 1));
      SetCursorPos(MousePosX, MousePosY);
      Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MIDDLEDOWN, 0, 0, 0, 0);
    end;

    Position := Pos('<|SETMOUSEMIDDLEUP|>', Buffer);
    if Position > 0 then
    begin
      BufferTemp := Buffer;
      Delete(BufferTemp, 1, Position + 19);
      Position := Pos('<|>', BufferTemp);
      MousePosX := StrToInt(Copy(BufferTemp, 1, Position - 1));
      Delete(BufferTemp, 1, Position + 2);
      MousePosY := StrToInt(Copy(BufferTemp, 1, Pos('<|END|>', BufferTemp) - 1));
      SetCursorPos(MousePosX, MousePosY);
      Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MIDDLEUP, 0, 0, 0, 0);
    end;

    Position := Pos('<|WHEELMOUSE|>', Buffer);
    if Position > 0 then
    begin
      BufferTemp := Buffer;
      Delete(BufferTemp, 1, Position + 13);
      BufferTemp := Copy(BufferTemp, 1, Pos('<|END|>', BufferTemp) - 1);
      Mouse_Event(MOUSEEVENTF_WHEEL, 0, 0, DWORD(StrToInt(BufferTemp)), 0);
    end;

    // Clipboard Remote
    Position := Pos('<|CLIPBOARD|>', Buffer);
    if Position > 0 then
    begin
      BufferTemp := Buffer;
      Delete(BufferTemp, 1, Position + 12);
      BufferTemp := Copy(BufferTemp, 1, Pos('<|END|>', BufferTemp) - 1);
      if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Svc) then
        Svc.SetClipboard(BufferTemp);
    end;

    // Chat
    Position := Pos('<|CHAT|>', Buffer);
    if Position > 0 then
    begin
      BufferTemp := Buffer;
      Delete(BufferTemp, 1, Position + 7);
      BufferTemp := Copy(BufferTemp, 1, Pos('<|END|>', BufferTemp) - 1);

      Synchronize(
        procedure
        begin
          FormChat.Mensagem(BufferTemp);

          if not(FormChat.Visible) then
          begin
            Beep;
            FormChat.Show;
          end;

          if not(FormChat.Active) then
          begin
            Beep;
            FlashWindow(FmxHandleToHWND(FormConexao.Handle), True);
            FlashWindow(FmxHandleToHWND(FormChat.Handle), True);
          end;
        end);
    end;

    // Share Files
    // Request Folder List
    Position := Pos('<|GETFOLDERS|>', Buffer);
    if Position > 0 then
    begin
      BufferTemp := Buffer;
      Delete(BufferTemp, 1, Position + 13);
      BufferTemp := Copy(BufferTemp, 1, Pos('<|END|>', BufferTemp) - 1);
      Socket.SendText('<|REDIRECT|><|FOLDERLIST|>' + TRDLib.ListFolders(BufferTemp) + '<|ENDFOLDERLIST|>');
    end;

    // Request Files List
    Position := Pos('<|GETFILES|>', Buffer);
    if Position > 0 then
    begin
      BufferTemp := Buffer;
      Delete(BufferTemp, 1, Position + 11);
      BufferTemp := Copy(BufferTemp, 1, Pos('<|END|>', BufferTemp) - 1);
      Socket.SendText('<|REDIRECT|><|FILESLIST|>' + TRDLib.ListFiles(BufferTemp, '*.*') + '<|ENDFILESLIST|>');
    end;

    // Receive Folder List
    Position := Pos('<|FOLDERLIST|>', Buffer);
    if Position > 0 then
    begin
      while Socket.Connected do
      begin
        if Buffer.Contains('<|ENDFOLDERLIST|>') then
          Break;

        if Socket.ReceiveLength > 0 then
          Buffer := Buffer + Socket.ReceiveText;

        Sleep(FOLGAPROCESSAMENTO);
      end;

      BufferTemp := Buffer;
      Delete(BufferTemp, 1, Position + 13);
      FoldersAndFiles := TStringList.Create;
      FoldersAndFiles.Text := Copy(BufferTemp, 1, Pos('<|ENDFOLDERLIST|>', BufferTemp) - 1);
      FoldersAndFiles.Sort;

      Synchronize(
        procedure
        begin
          FormArquivos.CarregarListaPastas(FoldersAndFiles.Text);
          FormArquivos.Caption := 'Compartilhamento de arquivos - ' + IntToStr(FormArquivos.lstArquivos.Items.Count) + ' Itens encontrados';
        end);

      FreeAndNil(FoldersAndFiles);
      Socket.SendText('<|REDIRECT|><|GETFILES|>' + FormArquivos.txtPasta.Text + '<|END|>');
    end;

    // Receive Files List
    Position := Pos('<|FILESLIST|>', Buffer);
    if Position > 0 then
    begin
      while Socket.Connected do
      begin
        if Buffer.Contains('<|ENDFILESLIST|>') then
          Break;

        if Socket.ReceiveLength > 0 then
          Buffer := Buffer + Socket.ReceiveText;

        Sleep(FOLGAPROCESSAMENTO);
      end;

      BufferTemp := Buffer;
      Delete(BufferTemp, 1, Position + 12);
      FoldersAndFiles := TStringList.Create;
      FoldersAndFiles.Text := Copy(BufferTemp, 1, Pos('<|ENDFILESLIST|>', BufferTemp) - 1);
      FoldersAndFiles.Sort;

      Synchronize(
        procedure
        begin
          FormArquivos.CarregarListaArquivos(FoldersAndFiles.Text);
          FormArquivos.txtPasta.Enabled := True;
          FormArquivos.Caption := 'Compartilhamento de arquivos - ' + IntToStr(FormArquivos.lstArquivos.Items.Count) + ' Itens encontrados';
        end);

      FreeAndNil(FoldersAndFiles);
    end;

    Position := Pos('<|UPLOADPROGRESS|>', Buffer);
    if Position > 0 then
    begin
      BufferTemp := Buffer;
      Delete(BufferTemp, 1, Position + 17);
      BufferTemp := Copy(BufferTemp, 1, Pos('<|END|>', BufferTemp) - 1);

      Synchronize(
        procedure
        begin
          FormArquivos.pgbUpload.Value := StrToInt(BufferTemp);
          FormArquivos.lblTamanhoUpload.Text := 'Tamanho: ' + TRDLib.GetSize(FormArquivos.pgbUpload.Value) + ' / ' + TRDLib.GetSize(FormArquivos.pgbUpload.Max);
        end);
    end;

    if Buffer.Contains('<|UPLOADCOMPLETE|>') then
    begin
      Synchronize(
        procedure
        begin
          FormArquivos.pgbUpload.Value := 0;
          FormArquivos.btnUpload.Enabled := True;
          FormArquivos.txtPasta.Enabled := False;
          FormArquivos.lblTamanhoUpload.Text := 'Tamanho: 0 B / 0 B';
        end);

      Conexao.SocketPrincipal.Socket.SendText('<|REDIRECT|><|GETFOLDERS|>' + FormArquivos.txtPasta.Text + '<|END|>');

      Synchronize(
        procedure
        begin
          MessageBox(0, 'Arquivo enviado com sucesso.', 'Suporte Remoto - Arquivos', MB_ICONASTERISK + MB_TOPMOST);
        end);
    end;

    Position := Pos('<|DOWNLOADFILE|>', Buffer);
    if Position > 0 then
    begin
      BufferTemp := Buffer;
      Delete(BufferTemp, 1, Position + 15);
      BufferTemp := Copy(BufferTemp, 1, Pos('<|END|>', BufferTemp) - 1);
      FileToUpload := TFileStream.Create(BufferTemp, fmOpenRead);
      Conexao.SocketArquivos.Socket.SendText('<|SIZE|>' + IntToStr(FileToUpload.Size) + '<|END|>');
      Conexao.SocketArquivos.Socket.SendStream(FileToUpload);
    end;
  end;
end;

procedure TThreadConexaoPrincipal.ThreadTerminate(ASender: TObject);
begin
  if (Assigned(Conexao)) and (not Terminated) then
    Conexao.LimparThread(ttPrincipal);
end;

{ TConexaoAreaRemota }

constructor TThreadConexaoAreaRemota.Create(ASocket: TCustomWinSocket);
begin
  inherited Create(True);
  Socket := ASocket;
  FreeOnTerminate := True;
  OnTerminate := ThreadTerminate;
  Resume;
end;

procedure TThreadConexaoAreaRemota.Execute;
var
  Position: Integer;
  Buffer: string;
  TempBuffer: string;
  MyFirstBmp: TMemoryStream;
  PackStream: TMemoryStream;
  MyTempStream: TMemoryStream;
  UnPackStream: TMemoryStream;
  MyCompareBmp: TMemoryStream;
  MySecondBmp: TMemoryStream;
  hDesktop: HDESK;
begin
  inherited;

  try
    MyFirstBmp := TMemoryStream.Create;
    UnPackStream := TMemoryStream.Create;
    MyTempStream := TMemoryStream.Create;
    MySecondBmp := TMemoryStream.Create;
    MyCompareBmp := TMemoryStream.Create;
    PackStream := TMemoryStream.Create;

    while True do
    begin
      Sleep(FOLGAPROCESSAMENTO); // Avoids using 100% CPU

      if (Socket = nil) or not(Socket.Connected) or (Terminated) then
        Break;

      if Socket.ReceiveLength < 1 then
        Continue;

      Buffer := Buffer + Socket.ReceiveText; // Accommodates in memory all images that are being received and not processed. This helps in smoothing and mapping so that changes in the wrong places do not occur.

      Position := Pos('<|GETFULLSCREENSHOT|>', Buffer);
      if Position > 0 then
      begin
        Delete(Buffer, 1, Position + 20);
        Conexao.SocketPrincipal.Socket.SendText('<|REDIRECT|><|RESOLUTION|>' + IntToStr(Trunc(Screen.Width)) + '<|>' + IntToStr(Trunc(Screen.Height)) + '<|END|>');

        MyFirstBmp.Clear;
        UnPackStream.Clear;
        MyTempStream.Clear;
        MySecondBmp.Clear;
        MyCompareBmp.Clear;
        PackStream.Clear;

        Synchronize(
          procedure
          begin
            GetScreenToMemoryStream(Conexao.MostrarMouse, MyFirstBmp);
          end);

        MyFirstBmp.Position := 0;
        PackStream.LoadFromStream(MyFirstBmp);
        TRDLib.CompressStreamWithZLib(PackStream);
        PackStream.Position := 0;
        Socket.SendText('<|IMAGE|>' + TRDLib.MemoryStreamToString(PackStream) + '<|END|>');

        while True do
        begin
          Sleep(16);

          if (Socket = nil) or not(Socket.Connected) or (Terminated) then
            Break;

          // EUREKA: This is the responsable to interact with UAC. But we need run
          // the software on SYSTEM account to work.
          hDesktop := OpenInputDesktop(0, True, MAXIMUM_ALLOWED);
          if hDesktop <> 0 then
          begin
            SetThreadDesktop(hDesktop);
            CloseHandle(hDesktop);
          end;

          // Workaround to run on change from secure desktop to default.
          try
            GetScreenToMemoryStream(Conexao.MostrarMouse, MySecondBmp);
            MySecondBmp.Position := 0;
          except
            Synchronize(
              procedure
              begin
                GetScreenToMemoryStream(Conexao.MostrarMouse, MySecondBmp);
                MySecondBmp.Position := 0;
              end);
          end;

          // Check if the resolution has been changed
          if MyFirstBmp.Size <> MySecondBmp.Size then
            Conexao.SocketPrincipal.Socket.SendText('<|REDIRECT|><|RESOLUTION|>' + IntToStr(Trunc(Screen.Width)) + '<|>' + IntToStr(Trunc(Screen.Height)) + '<|END|>');

          CompareStream(MyFirstBmp, MySecondBmp, MyCompareBmp);
          MyCompareBmp.Position := 0;
          PackStream.LoadFromStream(MyCompareBmp);
          TRDLib.CompressStreamWithZLib(PackStream);
          PackStream.Position := 0;

          if (Socket <> nil) and (Socket.Connected) then
          begin
            while Socket.SendText('<|IMAGE|>' + TRDLib.MemoryStreamToString(PackStream) + '<|END|>') < 0 do
              Sleep(FOLGAPROCESSAMENTO);
          end;
        end;
      end;

      // Processes all Buffer that is in memory.
      while Buffer.Contains('<|END|>') do
      begin
        Delete(Buffer, 1, Pos('<|IMAGE|>', Buffer) + 8);
        Position := Pos('<|END|>', Buffer);
        TempBuffer := Copy(Buffer, 1, Position - 1);
        MyTempStream.Write(AnsiString(TempBuffer)[1], Length(TempBuffer));
        Delete(Buffer, 1, Position + 6); // Clears the memory of the image that was processed.
        MyTempStream.Position := 0;
        UnPackStream.LoadFromStream(MyTempStream);
        TRDLib.DeCompressStreamWithZLib(UnPackStream);

        if (MyFirstBmp.Size = 0) then
        begin
          MyFirstBmp.LoadFromStream(UnPackStream);
          MyFirstBmp.Position := 0;

          Synchronize(
            procedure
            begin
              FormTelaRemota.imgTelaRemota.Bitmap.LoadFromStream(MyFirstBmp);
              FormTelaRemota.Caption := 'Suporte Remoto (Latência: ' + IntToStr(Conexao.Latencia) + ' ms)';
            end);
        end
        else
        begin
          MyCompareBmp.Clear;
          MySecondBmp.Clear;
          MyCompareBmp.LoadFromStream(UnPackStream);
          ResumeStream(MyFirstBmp, MySecondBmp, MyCompareBmp);

          Synchronize(
            procedure
            begin
              FormTelaRemota.imgTelaRemota.Bitmap.LoadFromStream(MySecondBmp);
              FormTelaRemota.Caption := 'Suporte Remoto (Latência: ' + IntToStr(Conexao.Latencia) + ' ms)';
            end);
        end;

        UnPackStream.Clear;
        MyTempStream.Clear;
        MySecondBmp.Clear;
        MyCompareBmp.Clear;
        PackStream.Clear;
      end;
    end;
  finally
    FreeAndNil(MyFirstBmp);
    FreeAndNil(UnPackStream);
    FreeAndNil(MyTempStream);
    FreeAndNil(MySecondBmp);
    FreeAndNil(MyCompareBmp);
    FreeAndNil(PackStream);
  end;
end;

procedure TThreadConexaoAreaRemota.ThreadTerminate(ASender: TObject);
begin
  if (Assigned(Conexao)) and (not Terminated) then
    Conexao.LimparThread(ttAreaRemota);
end;

{ TConexaoTeclado }

constructor TThreadConexaoTeclado.Create(ASocket: TCustomWinSocket);
begin
  inherited Create(True);
  Socket := ASocket;
  FreeOnTerminate := True;
  OnTerminate := ThreadTerminate;
  Resume;
end;

procedure TThreadConexaoTeclado.Execute;
var
  Buffer: string;
  hDesktop: HDESK;
begin
  while True do
  begin
    Sleep(FOLGAPROCESSAMENTO); // Avoids using 100% CPU

    if (Socket = nil) or not(Socket.Connected) or (Terminated) then
      Break;

    if Socket.ReceiveLength < 1 then
      Continue;

    Buffer := Socket.ReceiveText;

    // EUREKA: This is the responsable to interact with UAC. But we need run
    // the software on SYSTEM account to work.
    hDesktop := OpenInputDesktop(0, True, MAXIMUM_ALLOWED);
    if hDesktop <> 0 then
    begin
      SetThreadDesktop(hDesktop);
      CloseHandle(hDesktop);
    end;

    // Combo Keys
    if Buffer.Contains('<|ALTDOWN|>') then
    begin
      Buffer := StringReplace(Buffer, '<|ALTDOWN|>', '', [rfReplaceAll]);
      keybd_event(18, 0, 0, 0);
    end;

    if Buffer.Contains('<|ALTUP|>') then
    begin
      Buffer := StringReplace(Buffer, '<|ALTUP|>', '', [rfReplaceAll]);
      keybd_event(18, 0, KEYEVENTF_KEYUP, 0);
    end;

    if Buffer.Contains('<|CTRLDOWN|>') then
    begin
      Buffer := StringReplace(Buffer, '<|CTRLDOWN|>', '', [rfReplaceAll]);
      keybd_event(17, 0, 0, 0);
    end;

    if Buffer.Contains('<|CTRLUP|>') then
    begin
      Buffer := StringReplace(Buffer, '<|CTRLUP|>', '', [rfReplaceAll]);
      keybd_event(17, 0, KEYEVENTF_KEYUP, 0);
    end;

    if Buffer.Contains('<|SHIFTDOWN|>') then
    begin
      Buffer := StringReplace(Buffer, '<|SHIFTDOWN|>', '', [rfReplaceAll]);
      keybd_event(16, 0, 0, 0);
    end;

    if Buffer.Contains('<|SHIFTUP|>') then
    begin
      Buffer := StringReplace(Buffer, '<|SHIFTUP|>', '', [rfReplaceAll]);
      keybd_event(16, 0, KEYEVENTF_KEYUP, 0);
    end;

    if Buffer.Contains('?') then
    begin
      if GetKeyState(VK_SHIFT) < 0 then
      begin
        keybd_event(16, 0, KEYEVENTF_KEYUP, 0);
        SendKeys(PWideChar(Buffer), False);
        keybd_event(16, 0, 0, 0);
      end;
    end
    else
      SendKeys(PWideChar(Buffer), False);
  end;
end;

procedure TThreadConexaoTeclado.ThreadTerminate(ASender: TObject);
begin
  if (Assigned(Conexao)) and (not Terminated) then
    Conexao.LimparThread(ttTeclado);
end;

{ TConexaoArquivos }

constructor TThreadConexaoArquivos.Create(ASocket: TCustomWinSocket);
begin
  inherited Create(True);
  Socket := ASocket;
  FreeOnTerminate := True;
  OnTerminate := ThreadTerminate;
  Resume;
end;

procedure TThreadConexaoArquivos.Execute;
var
  Position: Integer;
  FileSize: Int64;
  ReceivingFile: Boolean;
  Buffer: string;
  BufferTemp: string;
  FileStream: TFileStream;
begin
  inherited;

  ReceivingFile := False;
  FileStream := nil;

  while True do
  begin
    Sleep(FOLGAPROCESSAMENTO); // Avoids using 100% CPU

    if (Socket = nil) or not(Socket.Connected) or (Terminated) then
      Break;

    if Socket.ReceiveLength < 1 then
      Continue;

    Buffer := Socket.ReceiveText;

    if not(ReceivingFile) then
    begin
      Position := Pos('<|DIRECTORYTOSAVE|>', Buffer);
      if Position > 0 then
      begin
        BufferTemp := Buffer;
        Delete(BufferTemp, 1, Position + 18);
        Position := Pos('<|>', BufferTemp);
        BufferTemp := Copy(BufferTemp, 1, Position - 1);
        FormArquivos.DirectoryToSaveFile := BufferTemp;
      end;

      Position := Pos('<|SIZE|>', Buffer);
      if Position > 0 then
      begin
        BufferTemp := Buffer;
        Delete(BufferTemp, 1, Position + 7);
        BufferTemp := Copy(BufferTemp, 1, Pos('<|END|>', BufferTemp) - 1);
        FileSize := StrToInt(BufferTemp);
        FileStream := TFileStream.Create(FormArquivos.DirectoryToSaveFile + '.tmp', fmCreate or fmOpenReadWrite);

        if (Conexao.Visualizador) then
        begin
          Synchronize(
            procedure
            begin
              FormArquivos.pgbDownload.Max := FileSize;
              FormArquivos.pgbDownload.Value := 0;
              FormArquivos.lblTamanhoDownload.Text := 'Tamanho: ' + TRDLib.GetSize(FileStream.Size) + ' / ' + TRDLib.GetSize(FileSize);
            end);
        end;

        Delete(Buffer, 1, Pos('<|END|>', Buffer) + 6);
        ReceivingFile := True;
      end;
    end;

    if (Length(Buffer) > 0) and (ReceivingFile) then
    begin
      FileStream.Write(AnsiString(Buffer)[1], Length(Buffer));

      if (Conexao.Visualizador) then
      begin
        Synchronize(
          procedure
          begin
            FormArquivos.pgbDownload.Value := FileStream.Size;
            FormArquivos.lblTamanhoDownload.Text := 'Tamanho: ' + TRDLib.GetSize(FileStream.Size) + ' / ' + TRDLib.GetSize(FileSize);
          end);
      end
      else
      begin
        while Conexao.SocketPrincipal.Socket.SendText('<|REDIRECT|><|UPLOADPROGRESS|>' + IntToStr(FileStream.Size) + '<|END|>') < 0 do
          Sleep(FOLGAPROCESSAMENTO);
      end;

      if (FileStream.Size = FileSize) then
      begin
        FreeAndNil(FileStream);

        if (FileExists(FormArquivos.DirectoryToSaveFile)) then
          DeleteFile(FormArquivos.DirectoryToSaveFile);

        RenameFile(FormArquivos.DirectoryToSaveFile + '.tmp', FormArquivos.DirectoryToSaveFile);

        if not(Conexao.Visualizador) then
          Conexao.SocketPrincipal.Socket.SendText('<|REDIRECT|><|UPLOADCOMPLETE|>')
        else
        begin
          Synchronize(
            procedure
            begin
              FormArquivos.pgbDownload.Value := 0;
              FormArquivos.btnDownload.Enabled := True;
              FormArquivos.lblTamanhoDownload.Text := 'Tamanho: 0 B / 0 B';
              MessageBox(0, 'Arquivo baixado com sucesso.', 'Suporte Remoto - Arquivos', MB_ICONASTERISK + MB_TOPMOST);
            end);
        end;

        ReceivingFile := False;
      end;
    end;
  end;
end;

procedure TThreadConexaoArquivos.ThreadTerminate(ASender: TObject);
begin
  if (Assigned(Conexao)) and (not Terminated) then
    Conexao.LimparThread(ttArquivos);
end;

end.
