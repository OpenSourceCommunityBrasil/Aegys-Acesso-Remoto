unit uCtrl_Threads;

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
  System.Classes,

  IdBaseComponent,
  IdComponent, IdUDPBase, IdUDPClient, IdCustomTCPServer, IdSocksServer
  , VCL.Forms
{$IF DEFINED (ANDROID) || (IOS)}
    , Fmx.Graphics
{$ENDIF}
{$IF DEFINED (MSWINDOWS)}
    , System.Win.ScktComp, Vcl.Graphics
{$ENDIF}
    , uFunctions, Fmx.Types;

Type
 TThreadConexaoPrincipal = class(TThread)
 Private
  {$IF DEFINED (ANDROID) || (IOS)}
   Socket: IdUDPClient;
  {$ENDIF}
  {$IF DEFINED (MSWINDOWS)}
   Socket: TCustomWinSocket;
  {$ENDIF}
  Function GetMonitorCount : Integer;
 Public
  {$IF DEFINED (ANDROID) || (IOS)}
   Constructor Create(ASocket : IdUDPClient);    Overload;
  {$ENDIF}
  {$IF DEFINED (MSWINDOWS)}
   Constructor Create(ASocket : TCustomWinSocket);Overload;
  {$ENDIF}
  Procedure Execute; Override;
  Procedure ThreadTerminate(ASender : TObject);
 End;

  TThreadConexaoAreaRemota = class(TThread)
  Private
   vBreak   : Boolean;
   vInitbuffer,
   vMonitor : String;
   {$IF DEFINED (ANDROID) || (IOS)}
    Socket: IdUDPClient;
   {$ENDIF}
   {$IF DEFINED (MSWINDOWS)}
    Socket: TCustomWinSocket;
   {$ENDIF}
   Procedure SetMonitor(Value : String);
  Public
   {$IF DEFINED (ANDROID) || (IOS)}
    Constructor Create(ASocket: IdUDPClient); overload;
   {$ENDIF}
   {$IF DEFINED (MSWINDOWS)}
    Constructor Create(ASocket: TCustomWinSocket); overload;
   {$ENDIF}
    Property  Capture    : Boolean Read vBreak      Write vBreak;
    Property  Monitor    : String  Read vMonitor    Write SetMonitor;
    Property  Initbuffer : String  Read vInitbuffer Write vInitbuffer;
    procedure Execute; override;
    procedure ThreadTerminate(ASender: TObject);
  end;

  TThreadConexaoTeclado = class(TThread)
{$IF DEFINED (ANDROID) || (IOS)}
    Socket: IdUDPClient;
    constructor Create(ASocket: IdUDPClient); overload;
{$ENDIF}
{$IF DEFINED (MSWINDOWS)}
    Socket: TCustomWinSocket;
    constructor Create(ASocket: TCustomWinSocket); overload;
{$ENDIF}
    procedure Execute; override;
    procedure ThreadTerminate(ASender: TObject);
  end;

  TThreadConexaoArquivos = class(TThread)
{$IF DEFINED (ANDROID) || (IOS)}
    Socket: IdUDPClient;
    constructor Create(ASocket: IdUDPClient); overload;
{$ENDIF}
{$IF DEFINED (MSWINDOWS)}
    Socket: TCustomWinSocket;
    constructor Create(ASocket: TCustomWinSocket); overload;
{$ENDIF}
    procedure Execute; override;
    procedure ThreadTerminate(ASender: TObject);
  end;

  Procedure Delay(msecs : Cardinal);
  Procedure ListFoldersB(Directory      : String;
                         Var ReturnData : TStringList);

  Var
   ActualDownloadFileName : String;

implementation

{ TConexaoPrincipal }

uses uFormChat, uFormConexao, uFormTelaRemota,
  Fmx.Forms,
  Fmx.ListView.Types, System.SysUtils, uLibClass, uFormSenha,
  uSendKeyClass,
  System.Rtti, Fmx.Platform, Fmx.Surfaces, StreamManager,
  uConstants

{$IF DEFINED (ANDROID) || (IOS)}
{$ENDIF}
{$IF DEFINED (MSWINDOWS)}
    , Winapi.Windows,
  Fmx.Platform.Win
{$ENDIF}
    , CCR.Clipboard, uFilesFoldersOP, uFileTransfer;


Procedure Delay(msecs : Cardinal);
Var
 FirstTickCount: Cardinal;
Begin
 FirstTickCount := GetTickCount;
 Repeat
  Application.ProcessMessages;
 Until ((GetTickCount - FirstTickCount) >= msecs);
End;


Procedure ListFoldersB(Directory      : String;
                       Var ReturnData : TStringList);
Var
 FileName,
 Filelist   : String;
 Searchrec  : TWin32FindData;
 FindHandle : THandle;
Begin
 ReturnData.Clear;
 If Directory <> '' Then
  Begin
   Try
    FindHandle := FindFirstFile(PChar(Directory + '*.*'), Searchrec);
    If FindHandle <> INVALID_HANDLE_VALUE Then
     Begin
      Repeat
       If (Pos('ecycle.bin', LowerCase(Searchrec.cFileName)) = 0) Then
        Begin
         FileName := Searchrec.cFileName;
         If (FileName = '.') Then
          Continue;
         If ((Searchrec.dwFileAttributes And FILE_ATTRIBUTE_DIRECTORY) <> 0) Then
          ReturnData.Add(FileName)
         Else
          Filelist := Filelist + (FileName + #13);
        End;
      Until Not(FindNextFile(FindHandle, Searchrec));
     End;
   Finally
    Winapi.Windows.FindClose(FindHandle);
   End;
  End;
// ReturnStr := (Dirlist);
// Result := ReturnStr;
End;

Function TThreadConexaoPrincipal.GetMonitorCount : Integer;
Begin
 Result := VCL.Forms.Screen.MonitorCount;
End;

{$IF DEFINED (ANDROID) || (IOS)}

constructor TThreadConexaoPrincipal.Create(ASocket: IdUDPClient);
{$ENDIF}
{$IF DEFINED (MSWINDOWS)}
  constructor TThreadConexaoPrincipal.Create(ASocket: TCustomWinSocket);
{$ENDIF}
  begin
    inherited Create(True);
    Socket := ASocket;
    FreeOnTerminate := True;
    OnTerminate := ThreadTerminate;
    Resume;
  end;

  procedure TThreadConexaoPrincipal.Execute;
  var
    Buffer,
    BufferTemp,
    aTempID,
    Extension   : string;
    i: Integer;
    Position: Integer;
    MousePosX: Integer;
    MousePosY: Integer;
    vDataSendReceive,
    FoldersAndFiles: TStringList;
    L: TListItem;
    FileToUpload: TFileStream;

{$IF DEFINED (ANDROID) || (IOS)}
{$ENDIF}
{$IF DEFINED (MSWINDOWS)}
    hDesktop: HDESK;
{$ENDIF}
    Svc: IFMXClipboardService;
    Locale: TLocale;
    BInputsBlock:Boolean;
    Procedure ListDrivers(Var ReturnData : TStringList);
    Var
     ShellProps : TShellProps;
     I          : Integer;
    Begin
     ShellProps := TShellProps.Create;
     ReturnData.Clear;
     ReturnData.Add(ShellProps.LocalStation);
     Try
      For I := 0 To ShellProps.Drivers.Count -1 Do
       ReturnData.Add(ShellProps.Drivers[I]);
     Except
     End;
     FreeAndNil(ShellProps);
    End;
    Procedure ListFilesB(FileName   : String;
                         Var Return : TStringList);
    Var
     ShellProps  : TShellProps;
     I           : Integer;
     vLinha      : AnsiString;
     Function GetSize(Bytes : Int64) : String;
     Const
      K = Int64(1024);
      M = K * K;
      G = K * M;
      T = K * G;
     Begin
           If Bytes < K Then Result := Format('%d B',  [Bytes])
      Else If Bytes < M Then Result := Format('%f KB', [Bytes / K])
      Else If Bytes < G Then Result := Format('%f MB', [Bytes / M])
      Else If Bytes < T Then Result := Format('%f GB', [Bytes / G])
      Else                   Result := Format('%f TB', [Bytes / T]);
     End;
    Begin
     ShellProps  := TShellProps.Create;
     Return.Clear;
     Try
      ShellProps.Folder := FileName;
      For I := 0 To ShellProps.FilesCount -1 do
       Begin
        If ShellProps.Files[I].FileType <> fpDir Then
         Begin
          vLinha := Format('%s|%s|%s|%s', [ShellProps.Files[I].FileName,
                                           GetSize(ShellProps.Files[I].FileSize),
                                           ShellProps.Files[I].FileTypeDesc,
                                           FormatDateTime('dd/mm/yyyy hh:mm:ss', ShellProps.Files[I].LastWrite)]);
          Return.Add(vLinha);
         End;
       End;
     Except

     End;
    eND;
    Function MontaLinhaEnvio(Value : TStringList) : AnsiString;
    Var
     I : Integer;
    Begin
     Result := '';
     For I := 0 To Value.Count -1 do
      Result := Result + Value[I] + #$D#$A;
    End;
  begin
    inherited;
    Locale := TLocale.Create;

    FoldersAndFiles := nil;
    FileToUpload := nil;
{$IF DEFINED (ANDROID) || (IOS)}
    while (Socket.Connected) and (not Terminated) do
{$ENDIF}
{$IF DEFINED (MSWINDOWS)}
      while (Socket.Connected) and (not Terminated) do
{$ENDIF}
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
          Position := Pos('<|>', BufferTemp);
          Conexao.Senha := Copy(BufferTemp, 1, Position - 1);
          Delete(BufferTemp, 1, Position + 2);
          Conexao.SenhaGerada := Copy(BufferTemp, 1, Pos('<|END|>', BufferTemp) - 1);
          Synchronize(FormConexao.SetOnline);
//          // If this Socket are connected, then connect the Desktop Socket, Keyboard Socket, File Download Socket and File Upload Socket
          Conexao.ReconectarSocketsSecundarios;
        end;

        // Ping
        if Buffer.Contains('<|PING|>') then
         Begin
          Socket.SendText('<|PONG|>');
          StringReplace(Buffer, '<|PING|>', '', [rfReplaceAll]);
          Application.ProcessMessages;
          Sleep(FOLGAPROCESSAMENTO); // Avoids using 100% CPU
          If Socket.ReceiveLength < 1 then
           Continue;
          Buffer := Socket.ReceiveText;
         End;

        Position := Pos('<|SETPING|>', Buffer);
        if Position > 0 then
        begin
//          BufferTemp := Buffer;
          Delete(Buffer, 1, Position + 10);
          BufferTemp := Copy(Buffer, 1, Pos('<|END|>', Buffer) - 1);
          Delete(Buffer, 1, Pos('<|END|>', Buffer) + 6);
          Conexao.Latencia := StrToInt(BufferTemp);
          If Trim(Buffer) = '' then
           Continue;
        end;

        // Warns access and remove Wallpaper
        if Buffer.Contains('<|ACCESSING|>') then
        begin
          Synchronize(
            procedure
            begin
              FormConexao.btnConectar.Enabled := False;
              FormConexao.LbtnConectar.Enabled :=
                FormConexao.btnConectar.Enabled;
              FormConexao.MudarStatusConexao(3, Locale.GetLocale(MSGS,
                'RemoteConnected'));
            end);
          Conexao.Acessando := True;
        end;

        if Buffer.Contains('<|IDEXISTS!REQUESTPASSWORD|>') then
        begin
          Synchronize(
            procedure
            begin
              FormConexao.MudarStatusConexao(0, Locale.GetLocale(MSGS,
                'AwaitingAuth'));
              FormSenha.ShowModal;
            end);
        end;

        if Buffer.Contains('<|IDNOTEXISTS|>') then
        begin
          Synchronize(
            procedure
            begin
              FormConexao.MudarStatusConexao(2, Locale.GetLocale(MSGS,
                'IDNoExist'));
              FormConexao.btnConectar.Enabled := True;
              FormConexao.LbtnConectar.Enabled :=
                FormConexao.btnConectar.Enabled;
            end);
        end;

        if Buffer.Contains('<|ACCESSDENIED|>') then
        begin
          Synchronize(
            procedure
            begin
              FormConexao.MudarStatusConexao(2, Locale.GetLocale(MSGS,
                'WrongPassword'));

              FormConexao.btnConectar.Enabled := True;
              FormConexao.LbtnConectar.Enabled :=
                FormConexao.btnConectar.Enabled;
            end);
        end;

        if Buffer.Contains('<|ACCESSBUSY|>') then
        begin
          Synchronize(
            procedure
            begin
              FormConexao.MudarStatusConexao(2, Locale.GetLocale(MSGS,
                'BusyGuest'));
              FormConexao.btnConectar.Enabled := True;
              FormConexao.LbtnConectar.Enabled :=
                FormConexao.btnConectar.Enabled;
            end);
        end;

        if Buffer.Contains('<|ACCESSGRANTED|>') then
        begin
         Synchronize(Procedure
                     Begin
//                      Conexao.ReconectarSocketsSecundarios;
                      FormConexao.MudarStatusConexao(3, Locale.GetLocale(MSGS, 'Granted'));
                     End);
         Socket.SendText('<|GETMONITORCOUNT|>' + Conexao.ID + '<|>' +
                         FormConexao.EGuestID.Text + '<|END|>');
        end;
        if Buffer.Contains('<|GETMONITORCOUNT|>') then
        begin
         Synchronize(Procedure
                     Begin
//                      Conexao.ReconectarSocketsSecundarios;
                     End);
          BufferTemp := Buffer;
          Delete(BufferTemp, 1, Position + Length('<|GETMONITORCOUNT|>'));
          Position := Pos('<|END|>', BufferTemp);
          aTempID  := Copy(BufferTemp, 1, Position -1);
          Socket.SendText('<|MONITORS|>' + aTempID + '<|>' + IntToStr(GetMonitorCount) + '<|END|>');
        end;
        if Buffer.Contains('<|CHANGEMONITOR|>') then
        begin
          BufferTemp := Buffer;
          Delete(BufferTemp, 1, Position + Length('<|CHANGEMONITOR|>'));
          Position := Pos('<|END|>', BufferTemp);
          aTempID  := Copy(BufferTemp, 1, Position -1);
          Conexao.ThreadAreaRemota.Capture := False;
          If Not Conexao.ThreadAreaRemota.Finished Then
           Conexao.ThreadAreaRemota.Terminate;
          WaitForSingleObject(Conexao.ThreadAreaRemota.Handle, INFINITE);
          Conexao.ThreadAreaRemota := Nil;
          Conexao.ThreadAreaRemota := TThreadConexaoAreaRemota.Create(Conexao.SocketAreaRemota.Socket);
          Conexao.ThreadAreaRemota.Monitor := aTempID;
          Conexao.ThreadAreaRemota.Initbuffer := '<|GETFULLSCREENSHOT|>';
          Conexao.ThreadAreaRemota.Resume;
        end;
        if Buffer.Contains('<|MONITORS|>') then
        begin
          BufferTemp := Buffer;
          Delete(BufferTemp, 1, Position + Length('<|MONITORS|>'));
          Position := Pos('<|END|>', BufferTemp);
          aTempID  := Copy(BufferTemp, 1, Position -1);
          Synchronize(
            procedure
            begin
              FormConexao.MudarStatusConexao(3, Locale.GetLocale(MSGS,
                'Granted'));
              Conexao.Visualizador := True;
              FormConexao.tmrClipboard.Enabled := True;
              FormConexao.LimparConexao;
              FormTelaRemota.ActualScreen      := '0';
              FormTelaRemota.ActualIDConnected := FormConexao.EGuestID.Text;
              If aTempID = '' Then
               FormTelaRemota.AddItems(0)
              Else
               FormTelaRemota.AddItems(StrToInt(aTempID));
              FormTelaRemota.Show;
              FormConexao.Hide;
            end);
          Socket.SendText('<|RELATION|>' + Conexao.ID + '<|>' +
                          FormConexao.EGuestID.Text + '<|>' + '<|BESTQ|>' +
                          IntToStr(FormConexao.cbQuality.ItemIndex) + '<|END|>');
        end;

        if Buffer.Contains('<|DISCONNECTED|>') then
        begin
          Synchronize(
            procedure
            begin
              FormTelaRemota.Hide;
              If Assigned(fFileTransfer) Then
               fFileTransfer.Hide;
              FormChat.Hide;
              Conexao.ReconectarSocketsSecundarios;
              FormConexao.Show;
              FormConexao.SetOnline;
              FormConexao.MudarStatusConexao(2, Locale.GetLocale(MSGS,
                'LostConnection'));
              FlashWindow(FmxHandleToHWND(FormConexao.Handle), True);
            end);
        end;

        { Redirected commands }

        if Buffer.Contains('<|SHOWMOUSE|>') then
          Conexao.MostrarMouse := True;

        if Buffer.Contains('<|HIDEMOUSE|>') then
          Conexao.MostrarMouse := False;
        if Buffer.Contains('<|BLOCKINPUT|>') then
        Synchronize(
            procedure
            begin
               BInputsBlock :=true;
               Blockinput(true);
            end);


        if Buffer.Contains('<|UNBLOCKINPUT|>') then
        Synchronize(
            procedure
            begin
               BInputsBlock :=false;
               Blockinput(false);
            end);
        // Desktop Remote
        Position := Pos('<|RESOLUTION|>', Buffer);
        if Position > 0 then
        begin
          BufferTemp := Buffer;
          Delete(BufferTemp, 1, Position + 13);
          Position := Pos('<|>', BufferTemp);
          Conexao.ResolucaoLargura :=
            StrToInt(Copy(BufferTemp, 1, Position - 1));
          Delete(BufferTemp, 1, Position + 2);
          Conexao.ResolucaoAltura :=
            StrToInt(Copy(BufferTemp, 1, Pos('<|END|>', BufferTemp) - 1));
          FormTelaRemota.PROC_REDIMENSIONARExecute(nil);
        end;

        Position := Pos('<|SETMOUSEPOS|>', Buffer);
        if Position > 0 then
        begin
          BufferTemp := Buffer;
          Delete(BufferTemp, 1, Position + 14);
          Position := Pos('<|>', BufferTemp);
          aTempID   := Conexao.ThreadAreaRemota.Monitor;
          If Trim(aTempID) = '' Then
           aTempID := '0';
          MousePosX := Vcl.Forms.Screen.Monitors[StrToInt(aTempID)].Left + StrToInt(Copy(BufferTemp, 1, Position - 1));
          Delete(BufferTemp, 1, Position + 2);
          MousePosY := Vcl.Forms.Screen.Monitors[StrToInt(aTempID)].Top + StrToInt(Copy(BufferTemp, 1, Pos('<|END|>', BufferTemp) - 1));
          if Buffer.Contains('<|BLOCKINPUT|>') then
          begin
            Synchronize(
            procedure
            begin
              BlockInput(false);
              SetCursorPos(MousePosX, MousePosY);
              application.ProcessMessages;
              BlockInput(true);
            end);
          end else
          SetCursorPos(MousePosX, MousePosY);
        end;

        Position := Pos('<|SETMOUSELEFTCLICKDOWN|>', Buffer);
        if Position > 0 then
        begin
          aTempID   := Conexao.ThreadAreaRemota.Monitor;
          If Trim(aTempID) = '' Then
           aTempID := '0';
          BufferTemp := Buffer;
          Delete(BufferTemp, 1, Position + 24);
          Position := Pos('<|>', BufferTemp);
          MousePosX := Vcl.Forms.Screen.Monitors[StrToInt(aTempID)].Left + StrToInt(Copy(BufferTemp, 1, Position - 1));
          Delete(BufferTemp, 1, Position + 2);
          MousePosY := Vcl.Forms.Screen.Monitors[StrToInt(aTempID)].Top + StrToInt(Copy(BufferTemp, 1,
            Pos('<|END|>', BufferTemp) - 1));
          if Buffer.Contains('<|BLOCKINPUT|>') then
          begin
            Synchronize(
            procedure
            begin
              BlockInput(false);
              SetCursorPos(MousePosX, MousePosY);
              Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
              application.ProcessMessages;
              BlockInput(true);
            end);
          end else
          begin
            SetCursorPos(MousePosX, MousePosY);
            Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
          end;
        end;

        Position := Pos('<|SETMOUSELEFTCLICKUP|>', Buffer);
        if Position > 0 then
        begin
          aTempID   := Conexao.ThreadAreaRemota.Monitor;
          If Trim(aTempID) = '' Then
           aTempID := '0';
          BufferTemp := Buffer;
          Delete(BufferTemp, 1, Position + 22);
          Position := Pos('<|>', BufferTemp);
          MousePosX := Vcl.Forms.Screen.Monitors[StrToInt(aTempID)].Left + StrToInt(Copy(BufferTemp, 1, Position - 1));
          Delete(BufferTemp, 1, Position + 2);
          MousePosY := Vcl.Forms.Screen.Monitors[StrToInt(aTempID)].Top + StrToInt(Copy(BufferTemp, 1,
            Pos('<|END|>', BufferTemp) - 1));
          if Buffer.Contains('<|BLOCKINPUT|>') then
          begin
            Synchronize(
            procedure
            begin
              BlockInput(false);
              SetCursorPos(MousePosX, MousePosY);
              Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
              application.ProcessMessages;
              blockinput(true);
            end);
          end else
          begin
            SetCursorPos(MousePosX, MousePosY);
            Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
          end;

        end;

        Position := Pos('<|SETMOUSERIGHTCLICKDOWN|>', Buffer);
        if Position > 0 then
        begin
          aTempID   := Conexao.ThreadAreaRemota.Monitor;
          If Trim(aTempID) = '' Then
           aTempID := '0';
          BufferTemp := Buffer;
          Delete(BufferTemp, 1, Position + 25);
          Position := Pos('<|>', BufferTemp);
          MousePosX := Vcl.Forms.Screen.Monitors[StrToInt(aTempID)].Left + StrToInt(Copy(BufferTemp, 1, Position - 1));
          Delete(BufferTemp, 1, Position + 2);
          MousePosY := Vcl.Forms.Screen.Monitors[StrToInt(aTempID)].Top + StrToInt(Copy(BufferTemp, 1,
            Pos('<|END|>', BufferTemp) - 1));
          if Buffer.Contains('<|BLOCKINPUT|>') then
          begin
            Synchronize(
            procedure
            begin
              BlockInput(false);
              SetCursorPos(MousePosX, MousePosY);
              Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_RIGHTDOWN, 0, 0, 0, 0);
              application.ProcessMessages;
              Blockinput(true);
            end);
          end else
          begin
            SetCursorPos(MousePosX, MousePosY);
            Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_RIGHTDOWN, 0, 0, 0, 0);
          end;

        end;

        Position := Pos('<|SETMOUSERIGHTCLICKUP|>', Buffer);
        if Position > 0 then
        begin
          aTempID   := Conexao.ThreadAreaRemota.Monitor;
          If Trim(aTempID) = '' Then
           aTempID := '0';
          BufferTemp := Buffer;
          Delete(BufferTemp, 1, Position + 23);
          Position := Pos('<|>', BufferTemp);
          MousePosX := Vcl.Forms.Screen.Monitors[StrToInt(aTempID)].Left + StrToInt(Copy(BufferTemp, 1, Position - 1));
          Delete(BufferTemp, 1, Position + 2);
          MousePosY := Vcl.Forms.Screen.Monitors[StrToInt(aTempID)].Top + StrToInt(Copy(BufferTemp, 1,
            Pos('<|END|>', BufferTemp) - 1));
          if Buffer.Contains('<|BLOCKINPUT|>') then
          begin
            Synchronize(
            procedure
            begin
              BlockInput(false);
              SetCursorPos(MousePosX, MousePosY);
              Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_RIGHTUP, 0, 0, 0, 0);
              application.ProcessMessages;
              BlockInput(true);
            end);
          end else
          begin
            SetCursorPos(MousePosX, MousePosY);
            Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_RIGHTUP, 0, 0, 0, 0);
          end;

        end;

        Position := Pos('<|SETMOUSEMIDDLEDOWN|>', Buffer);
        if Position > 0 then
        begin
          aTempID   := Conexao.ThreadAreaRemota.Monitor;
          If Trim(aTempID) = '' Then
           aTempID := '0';
          BufferTemp := Buffer;
          Delete(BufferTemp, 1, Position + 21);
          Position := Pos('<|>', BufferTemp);
          MousePosX := Vcl.Forms.Screen.Monitors[StrToInt(aTempID)].Left + StrToInt(Copy(BufferTemp, 1, Position - 1));
          Delete(BufferTemp, 1, Position + 2);
          MousePosY := Vcl.Forms.Screen.Monitors[StrToInt(aTempID)].Top + StrToInt(Copy(BufferTemp, 1,
            Pos('<|END|>', BufferTemp) - 1));
          if Buffer.Contains('<|BLOCKINPUT|>') then
          begin
            Synchronize(
            procedure
            begin
              BlockInput(false);
              SetCursorPos(MousePosX, MousePosY);
              Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MIDDLEDOWN, 0, 0, 0, 0);
              application.ProcessMessages;
              BlockInput(true);
            end);
          end else
          begin
            SetCursorPos(MousePosX, MousePosY);
            Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MIDDLEDOWN, 0, 0, 0, 0);
          end;

        end;

        Position := Pos('<|SETMOUSEMIDDLEUP|>', Buffer);
        if Position > 0 then
        begin
          aTempID   := Conexao.ThreadAreaRemota.Monitor;
          If Trim(aTempID) = '' Then
           aTempID := '0';
          BufferTemp := Buffer;
          Delete(BufferTemp, 1, Position + 19);
          Position := Pos('<|>', BufferTemp);
          MousePosX := Vcl.Forms.Screen.Monitors[StrToInt(aTempID)].Left + StrToInt(Copy(BufferTemp, 1, Position - 1));
          Delete(BufferTemp, 1, Position + 2);
          MousePosY := Vcl.Forms.Screen.Monitors[StrToInt(aTempID)].Top + StrToInt(Copy(BufferTemp, 1,
            Pos('<|END|>', BufferTemp) - 1));
          if Buffer.Contains('<|BLOCKINPUT|>') then
          begin
            Synchronize(
            procedure
            begin
              BlockInput(false);
              SetCursorPos(MousePosX, MousePosY);
              Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MIDDLEUP, 0, 0, 0, 0);
              application.ProcessMessages;
              BlockInput(true);
            end);
          end else
          begin
            SetCursorPos(MousePosX, MousePosY);
            Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MIDDLEUP, 0, 0, 0, 0);
          end;

        end;

        Position := Pos('<|WHEELMOUSE|>', Buffer);
        if Position > 0 then
        begin
          BufferTemp := Buffer;
          Delete(BufferTemp, 1, Position + 13);
          BufferTemp := Copy(BufferTemp, 1, Pos('<|END|>', BufferTemp) - 1);
          if Buffer.Contains('<|BLOCKINPUT|>') then
          begin
            Synchronize(
            procedure
            begin
              BlockInput(false);
              Mouse_Event(MOUSEEVENTF_WHEEL, 0, 0, DWORD(StrToInt(BufferTemp)), 0);
              application.ProcessMessages;
              BlockInput(true);
            end);
          end else
          begin
            Mouse_Event(MOUSEEVENTF_WHEEL, 0, 0, DWORD(StrToInt(BufferTemp)), 0);
          end;


        end;


        // Clipboard Remote
        Position := Pos('<|CLIPBOARD|>', Buffer);
        if Position > 0 then
        begin
          BufferTemp := Buffer;
          Delete(BufferTemp, 1, Position + 12);
          BufferTemp := Copy(BufferTemp, 1, Pos('<|END|>', BufferTemp) - 1);

          if TPlatformServices.Current.SupportsPlatformService
            (IFMXClipboardService, Svc) then
          begin
            if Buffer.Contains('<|BLOCKINPUT|>') then
            begin
              Synchronize(
              procedure
              begin
                BlockInput(false);
              end);
            end;
            Svc.SetClipboard(BufferTemp);
            if Buffer.Contains('<|BLOCKINPUT|>') then
            begin
              Synchronize(
              procedure
              begin
                BlockInput(true);
              end);
            end;
          end;
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

                FormChat.Show;
              end;

              if not(FormChat.Active) then
              begin
                FlashWindow(FmxHandleToHWND(FormConexao.Handle), True);
                FlashWindow(FmxHandleToHWND(FormChat.Handle), True);
              end;
            end);
        end;

        // Share Files
        // Request Folder List
        Position := Pos('<|GETFOLDERS|>', Buffer);
        if Position > 0 then
         Begin
          BufferTemp := Buffer;
          Application.ProcessMessages;
          Delete(BufferTemp, 1, Position + 13);
          BufferTemp := Copy(BufferTemp, 1, Pos('<|END_GETFOLDERS|>', BufferTemp) - 1);
          vDataSendReceive := TStringList.Create;
          ListFoldersB(BufferTemp, vDataSendReceive);
          Socket.SendText('<|REDIRECT|><|FOLDERLIST|>' + vDataSendReceive.Text + '<|END_FOLDERLIST|>');
          Application.ProcessMessages;
          vDataSendReceive.Free;
         End;
       Position := Pos('<|GETDRIVERS|>', Buffer);
       if Position > 0 then
        Begin
         BufferTemp := Buffer;
         Application.ProcessMessages;
         vDataSendReceive := TStringList.Create;
         ListDrivers(vDataSendReceive);
         Try
          Socket.SendText('<|REDIRECT|><|DRIVERLIST|>' + vDataSendReceive.Text + '<|END_DRIVERLIST|>');
          Application.ProcessMessages;
         Finally
          vDataSendReceive.Free;
         End;
        End;
       Position := Pos('<|DRIVERLIST|>', Buffer);
       if Position > 0 then
        Begin
         BufferTemp := Buffer;
         Application.ProcessMessages;
         Delete(BufferTemp, 1, Position + 13);
         BufferTemp := Copy(BufferTemp, 1, Pos('<|END_DRIVERLIST|>', BufferTemp) - 1);
         FoldersAndFiles := TStringList.Create;
         FoldersAndFiles.Text := BufferTemp;
         Synchronize(Procedure
                     Var
                      I : Integer;
                     Begin
                      If Assigned(fFileTransfer) Then
                       Begin
                        fFileTransfer.ceRemotePath.Items.Clear;
                        fFileTransfer.lPCRemoto.Text := FoldersAndFiles[0];
                        For I := 1 To FoldersAndFiles.count - 1 Do
                         fFileTransfer.ceRemotePath.Items.Add(' ' + FoldersAndFiles[I]);
                        If fFileTransfer.ceRemotePath.Items.Count > 0 Then
                         Begin
                          fFileTransfer.ceRemotePath.ItemIndex := 0;
                          fFileTransfer.ceRemotePath.OnChange(fFileTransfer.ceRemotePath);
                         End;
                       End;
                     End);
         FreeAndNil(FoldersAndFiles);
//         fFileTransfer.tLoadAction.Enabled := True;
        End;
        // Request Files List
       Position := Pos('<|GETFILES|>', Buffer);
       if Position > 0 then
        Begin
         BufferTemp := Buffer;
         Application.ProcessMessages;
         Delete(BufferTemp, 1, Position + 11);
         BufferTemp := Copy(BufferTemp, 1, Pos('<|END_GETFILES|>', BufferTemp) - 1);
         vDataSendReceive := TStringList.Create;
         Try
          ListFilesB(BufferTemp, vDataSendReceive);
          If vDataSendReceive.Count > 0 Then
           Begin
            BufferTemp := '<|REDIRECT|><|FILESLIST|>' + MontaLinhaEnvio(vDataSendReceive) + '<|END_FILESLIST|>';
            Socket.SendText(BufferTemp);
            Application.ProcessMessages;
           End;
         Except
         End;
         FreeAndNil(vDataSendReceive);
        End;

        // Receive Folder List
        Position := Pos('<|FOLDERLIST|>', Buffer);
        if Position > 0 then
        begin
          while (Socket.Connected) And (Not Terminated) do
          begin
            if Buffer.Contains('<|END_FOLDERLIST|>') then
              Break;

            if Socket.ReceiveLength > 0 then
              Buffer := Buffer + Socket.ReceiveText;

            Sleep(FOLGAPROCESSAMENTO);
          end;

          BufferTemp := Buffer;
          Application.ProcessMessages;
          Delete(BufferTemp, 1, Position + 13);
          FoldersAndFiles := TStringList.Create;
          FoldersAndFiles.Text := Copy(BufferTemp, 1, Pos('<|END_FOLDERLIST|>', BufferTemp) - 1);
          FoldersAndFiles.Sort;
          If Copy(FoldersAndFiles.Text, 1, 2) <> '..' Then
           FoldersAndFiles.Text := '..' + #13 + FoldersAndFiles.Text;
          Synchronize(
            procedure
            begin
             If Assigned(fFileTransfer) Then
              Begin
               fFileTransfer.CarregarListaPastas(FoldersAndFiles.Text);
               fFileTransfer.Caption := Format(Locale.GetLocale(FRMS, 'FileTitle'), [fFileTransfer.DestCount]);
              End;
            end);

          FreeAndNil(FoldersAndFiles);
          If Assigned(fFileTransfer) Then
           Begin
            BufferTemp := '<|REDIRECT|><|GETFILES|>' + fFileTransfer.ActiveFolder + '<|END_GETFILES|>';
            Socket.SendText(BufferTemp);
           End;
          Application.ProcessMessages;
        end;

        // Receive Files List
        Position := Pos('<|FILESLIST|>', Buffer);
        if Position > 0 then
        begin
          while (Socket.Connected) And (Not Terminated) do
          begin
            if Buffer.Contains('<|END_FILESLIST|>') then
              Break;

            if Socket.ReceiveLength > 0 then
              Buffer := Buffer + Socket.ReceiveText;

            Sleep(FOLGAPROCESSAMENTO);
          end;

          BufferTemp := Buffer;
          Application.ProcessMessages;
          Delete(BufferTemp, 1, Position + 12);
          FoldersAndFiles := TStringList.Create;
          FoldersAndFiles.Text := Copy(BufferTemp, 1, Pos('<|END_FILESLIST|>', BufferTemp) - 1);
          FoldersAndFiles.Sort;
          Synchronize(
            procedure
            begin
             If Assigned(fFileTransfer) Then
              Begin
               fFileTransfer.CarregarListaArquivos(FoldersAndFiles.Text);
               fFileTransfer.Caption := Format(Locale.GetLocale(FRMS,
                'FileTitle'), [fFileTransfer.DestCount]);
              End;
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
             If Assigned(fFileTransfer) Then
              Begin
               fFileTransfer.pgbUpload.Value := StrToInt(BufferTemp);
               fFileTransfer.LUploadSize.Text := Format(Locale.GetLocale(MAIN,
                 'Size'), [TRDLib.GetSize(fFileTransfer.pgbUpload.Value),
                 TRDLib.GetSize(fFileTransfer.pgbUpload.Max)])
              End;
            end);
        end;

        if Buffer.Contains('<|UPLOADCOMPLETE|>') then
        begin
          Synchronize(
            procedure
            begin
             If Assigned(fFileTransfer) Then
              Begin
               fFileTransfer.pgbUpload.Value := 0;
//              fFileTransfer.btnUpload.Enabled := True;
//              fFileTransfer.EFolder.Enabled := False;
               fFileTransfer.LUploadSize.Text := Format(Locale.GetLocale(MAIN,
                 'Size'), ['0 B', '0 B']);
              End;
            end);
         If Assigned(fFileTransfer) Then
          Conexao.SocketPrincipal.Socket.SendText('<|REDIRECT|><|GETFOLDERS|>' +
                                                   fFileTransfer.Directory_Local + '<|END|>');
          Application.ProcessMessages;
          Synchronize(
            procedure
            begin
              MessageBox(0, Locale.GetLocaleDlg(MSGS, 'SendSuccess'),
                Locale.GetLocaleDlg(FRMS, 'FileSubTitle'),
                MB_ICONASTERISK + MB_TOPMOST);
            end);
        end;

        Position := Pos('<|DOWNLOADFILE|>', Buffer);
        if Position > 0 then
        begin
          BufferTemp := Buffer;
          Delete(BufferTemp, 1, Position + 15);
          BufferTemp := Copy(BufferTemp, 1, Pos('<|END|>', BufferTemp) - 1);
          FileToUpload := TFileStream.Create(BufferTemp, fmOpenRead);
          Conexao.SocketArquivos.Socket.SendText
            ('<|SIZE|>' + IntToStr(FileToUpload.Size) + '<|END|>');
          Conexao.SocketArquivos.Socket.SendStream(FileToUpload);
        end;
       application.ProcessMessages;
      end;
    Locale.DisposeOf;
  end;

  procedure TThreadConexaoPrincipal.ThreadTerminate(ASender: TObject);
  begin
    if (Assigned(Conexao)) and (not Terminated) then
     Conexao.LimparThread(ttPrincipal);
  end;

{ TConexaoAreaRemota }
{$IF DEFINED (ANDROID) || (IOS)}
  constructor TThreadConexaoAreaRemota.Create(ASocket: IdUDPClient);
{$ENDIF}
{$IF DEFINED (MSWINDOWS)}
    constructor TThreadConexaoAreaRemota.Create(ASocket: TCustomWinSocket);
{$ENDIF}
    begin
      inherited Create(True);
      Socket := ASocket;
      FreeOnTerminate := True;
      OnTerminate := ThreadTerminate;
      vBreak      := True;
      vInitbuffer := '';
      vMonitor    := '0';
    end;

    procedure TThreadConexaoAreaRemota.Execute;
    var
      {$IF DEFINED (ANDROID) || (IOS)}
      PixelFormat: TPixelFormat;
      {$ENDIF}
      {$IF DEFINED (MSWINDOWS)}
      PixelFormat: Vcl.Graphics.TPixelFormat;
      {$ENDIF}
      Position: Integer;
      vTempText,
      xValue, Buffer, TempBuffer: String;
      PackStream, MyTempStream, UnPackStream: TMemoryStream;
      MyFirstBmp, MyCompareBmp, MySecondBmp: TStream;
      hDesktop: HDESK;
      bFirst,
      bFirstMon : Boolean;
      Locale: TLocale;
      vBitmap: TBitmap;
    begin
      inherited;
      Locale := TLocale.Create;
      bFirstMon := False;
      try
        MyFirstBmp := TMemoryStream.Create;
        UnPackStream := TMemoryStream.Create;
        MyTempStream := TMemoryStream.Create;
        MySecondBmp := TMemoryStream.Create;
        MyCompareBmp := TMemoryStream.Create;
        PackStream := TMemoryStream.Create;
        bFirst := True;

        while (vBreak) And (not Terminated) do
        begin
          Application.ProcessMessages;
          Sleep(FOLGAPROCESSAMENTO); // Avoids using 100% CPU
          Application.ProcessMessages;
          If vInitBuffer = '' Then
           Begin
            if (Socket = nil) or not(Socket.Connected) or (Terminated) then
             Break;
            if (Socket.ReceiveLength < 1) and
              (Pos('<|GETFULLSCREENSHOT|>', Buffer) <= 0) then
             Continue;
           End;
          if vInitbuffer <> '' then
           Begin
            Buffer := vInitBuffer;
            vInitBuffer := '';
           End
          Else If (Socket.ReceiveLength > 1) Then
           Buffer := Buffer + Socket.ReceiveText;
          // Accommodates in memory all images that are being received and not processed. This helps in smoothing and mapping so that changes in the wrong places do not occur.

          Position := Pos('<|GETFULLSCREENSHOT|>', Buffer);
          if Position > 0 then
          begin
            bFirstMon := True;
{$IF DEFINED (ANDROID) || (IOS)}
            PixelFormat := TPixelFormat(0);
{$ENDIF}
{$IF DEFINED (MSWINDOWS)}
            PixelFormat := Vcl.Graphics.TPixelFormat(0);
{$ENDIF}
            xValue := Buffer;
            If Pos('<|BESTQ|>', xValue) > 0 Then
            Begin
              xValue := Copy(xValue, Pos('<|BESTQ|>', xValue) + 9, 1);
              Case StrToInt(xValue) Of
{$IF DEFINED (ANDROID) || (IOS)}
                0:
                  PixelFormat := TPixelFormat.RGB;
                1:
                  PixelFormat := TPixelFormat.RGBA16;
                2:
                  PixelFormat := TPixelFormat.RGBA32F;
                3:
                  PixelFormat := TPixelFormat.None;
{$ENDIF}
{$IF DEFINED (MSWINDOWS)}
                0:
                  PixelFormat := pf4bit;
                1:
                  PixelFormat := pf8bit;
                2:
                  PixelFormat := pf15bit;
                3:
                  PixelFormat := pfDevice;
{$ENDIF}
              End;
            End;
            Delete(Buffer, 1, Position + 20);
//            If Not bFirstMon Then
            Conexao.SocketPrincipal.Socket.SendText('<|REDIRECT|><|RESOLUTION|>' + IntToStr(Trunc(Screen.Width)) + '<|>' + IntToStr(Trunc(Screen.Height)) + '<|END|>');
            TMemoryStream(MyFirstBmp).Clear;
            UnPackStream.Clear;
            MyTempStream.Clear;
            TMemoryStream(MySecondBmp).Clear;
            TMemoryStream(MyCompareBmp).Clear;
            PackStream.Clear;
            {$IF DEFINED (ANDROID) || (IOS)}
            {$ENDIF}
            {$IF DEFINED (MSWINDOWS)}
            GetScreenToMemoryStream(Conexao.MostrarMouse,
                                    TMemoryStream(MyFirstBmp), PixelFormat, vMonitor);
            {$ENDIF}
            MyFirstBmp.Position := 0;
            PackStream.LoadFromStream(MyFirstBmp);
            PackStream.Position := 0;
            TRDLib.CompressStreamWithZLib(PackStream);
            PackStream.Position := 0;
            vTempText := Socket.ReceiveText;
            Socket.SendText('<|FIRSTIMAGE|>' + TRDLib.MemoryStreamToString(PackStream) + '<|END|>');
            bFirstMon := False;
            while (vBreak) And (not Terminated)  do
            begin
              Sleep(FOLGAPROCESSAMENTO);

              if (Socket = nil) or not(Socket.Connected) or (Terminated) then
                Break;

              if Socket.ReceiveLength >= 1 then
              begin
                Buffer := Buffer + Socket.ReceiveText;

                Position := Pos('<|GETFULLSCREENSHOT|>', Buffer);

                if Position > 0 then
                  Break;
              end;

              // EUREKA: This is the responsable to interact with UAC. But we need run
              // the software on SYSTEM account to work.
              Try
               hDesktop := OpenInputDesktop(0, True, MAXIMUM_ALLOWED);
               If hDesktop <> 0 then
                Begin
                 SetThreadDesktop(hDesktop);
                 CloseHandle(hDesktop);
                End;
              Except
              End;
              // Workaround to run on change from secure desktop to default.
              try
                GetScreenToMemoryStream(Conexao.MostrarMouse,
                  TMemoryStream(MySecondBmp), PixelFormat, vMonitor);
                MySecondBmp.Position := 0;
              except
                Synchronize(
                  procedure
                  begin
                    GetScreenToMemoryStream(Conexao.MostrarMouse,
                      TMemoryStream(MySecondBmp), PixelFormat, vMonitor);
                    MySecondBmp.Position := 0;
                  end);
              end;

              // Check if the resolution has been changed
              if MyFirstBmp.Size <> MySecondBmp.Size then
                Conexao.SocketPrincipal.Socket.SendText
                  ('<|REDIRECT|><|RESOLUTION|>' + IntToStr(Trunc(Screen.Width))
                  + '<|>' + IntToStr(Trunc(Screen.Height)) + '<|END|>');
              If CompareStream(TMemoryStream(MyFirstBmp),
                TMemoryStream(MySecondBmp), TMemoryStream(MyCompareBmp)) Then
              Begin
                MyCompareBmp.Position := 0;
                If MyCompareBmp.Size > 0 Then
                Begin
                  PackStream.LoadFromStream(MyCompareBmp);
                  TRDLib.CompressStreamWithZLib(PackStream);
                  PackStream.Position := 0;
                  If (Socket <> nil) and (Socket.Connected) then
                  Begin
                    while Socket.SendText
                      ('<|IMAGE|>' + TRDLib.MemoryStreamToString(PackStream) +
                      '<|END|>') < 0 do
                      Sleep(FOLGAPROCESSAMENTO);
                  End;
                End;
              End;
            end;
          end;

          // Processes all Buffer that is in memory.
          while Buffer.Contains('<|END|>') do
          begin
            Position := Pos('<|FIRSTIMAGE|>', Buffer);
            if Position > 0 then
            begin
              TMemoryStream(MyFirstBmp).Clear;
              TMemoryStream(MyCompareBmp).Clear;
              TMemoryStream(MySecondBmp).Clear;
              Delete(Buffer, 1, Pos('<|FIRSTIMAGE|>', Buffer) + Length('<|FIRSTIMAGE|>') -1);
              bFirst := True;
            end
            else
            begin
              Delete(Buffer, 1, Pos('<|IMAGE|>', Buffer) + 8);
              bFirst := False;
            end;
            Position := Pos('<|END|>', Buffer);
            If Position > 0 Then
             TempBuffer := Copy(Buffer, 1, Position - 1)
            Else
             TempBuffer := Buffer;
            MyTempStream.Write(AnsiString(TempBuffer)[1], Length(TempBuffer));
            Delete(Buffer, 1, Position + 6);
            // Clears the memory of the image that was processed.
            MyTempStream.Position := 0;
            UnPackStream.LoadFromStream(MyTempStream);
            UnPackStream.Position := 0;
            TRDLib.DeCompressStreamWithZLib(UnPackStream);

            if (bFirst) and (MyFirstBmp.Size = 0) then
            begin
              TMemoryStream(MyFirstBmp).LoadFromStream(UnPackStream);
              MyFirstBmp.Position := 0;
              bFirst := False;

              Synchronize(
                procedure
                begin
                  try
                    // vBitmap := TBitmap.Create;
                    // vBitmap.LoadFromStream(MyFirstBmp);
                    // FormTelaRemota.imgTelaRemota.Fill.Kind := TbrushKind.Bitmap;
                    // FormTelaRemota.imgTelaRemota.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
                    // FormTelaRemota.imgTelaRemota.Fill.Bitmap.Bitmap.PixelFormat := PixelFormat;
                    FormTelaRemota.imgTelaRemota.Fill.Bitmap.Bitmap.
                      LoadFromStream(MyFirstBmp); // := vBitmap;
                    FormTelaRemota.Caption := FormTelaRemota.ActualIDConnected + ' - ' + Format(Locale.GetLocaleDlg(FRMS,
                      'RemoteTitle'), [IntToStr(Conexao.Latencia)]);
                  except
                    on e: exception do
                    begin

                    end;
                  end;
                end);
            end
            else
            begin
              TMemoryStream(MyCompareBmp).Clear;
              TMemoryStream(MySecondBmp).Clear;
              TMemoryStream(MyCompareBmp).LoadFromStream(UnPackStream);
              ResumeStream(TMemoryStream(MyFirstBmp),
                TMemoryStream(MySecondBmp), TMemoryStream(MyCompareBmp));

              Synchronize(
                procedure
                begin
                  Try
                    // vBitmap := TBitmap.Create;
                    // vBitmap.LoadFromStream(MySecondBmp);
                    // FormTelaRemota.imgTelaRemota.Fill.Kind := TbrushKind.Bitmap;
                    // FormTelaRemota.imgTelaRemota.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
                    // FormTelaRemota.imgTelaRemota.Fill.Bitmap.Bitmap.PixelFormat := PixelFormat;
                    FormTelaRemota.imgTelaRemota.Fill.Bitmap.Bitmap.
                      LoadFromStream(MySecondBmp); // := vBitmap;
                    FormTelaRemota.Caption := FormTelaRemota.ActualIDConnected + ' - ' + Format(Locale.GetLocaleDlg(FRMS,
                      'RemoteTitle'), [IntToStr(Conexao.Latencia)]);
                  Except
                    on e: exception do
                    begin

                    end;
                  End;
                end);
            end;

            UnPackStream.Clear;
            MyTempStream.Clear;
            TMemoryStream(MySecondBmp).Clear;
            TMemoryStream(MyCompareBmp).Clear;
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
        Locale.DisposeOf;
      end;
    end;

Procedure TThreadConexaoAreaRemota.SetMonitor(Value : String);
Begin
 vMonitor := Value;
End;

procedure TThreadConexaoAreaRemota.ThreadTerminate(ASender: TObject);
    begin
      if (Assigned(Conexao)) and (not Terminated) then
        Conexao.LimparThread(ttAreaRemota);
    end;

  { TConexaoTeclado }
{$IF DEFINED (ANDROID) || (IOS)}
    constructor TThreadConexaoTeclado.Create(ASocket: IdUDPClient);
{$ENDIF}
{$IF DEFINED (MSWINDOWS)}
      constructor TThreadConexaoTeclado.Create(ASocket: TCustomWinSocket);
{$ENDIF}
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
        Bblockinpunt:boolean;
        InicioPalavra,tamanhopalavra:integer;
      begin
        while (not Terminated) do
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
           //BLOCKEDINPUTS
          Bblockinpunt := Buffer.Contains('<|BLOCKINPUT|>');
          if Bblockinpunt then
          begin
            InicioPalavra := pos('<|BLOCKINPUT|>',Buffer);
            TamanhoPalavra := length(Buffer);
            if InicioPalavra > 0 then
            Delete(Buffer,InicioPalavra,TamanhoPalavra);
            Synchronize(
            procedure
            begin
              BlockInput(false);
            end);
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
            Buffer := StringReplace(Buffer, '<|SHIFTDOWN|>', '',
              [rfReplaceAll]);
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

          //before writer keys if blocked them
          Synchronize(
          procedure
          begin
            BlockInput(Bblockinpunt);
          end
          );
        end;
      end;

      procedure TThreadConexaoTeclado.ThreadTerminate(ASender: TObject);
      begin
        if (Assigned(Conexao)) and (not Terminated) then
          Conexao.LimparThread(ttTeclado);
      end;

    { TConexaoArquivos }

{$IF DEFINED (ANDROID) || (IOS)}
      constructor TThreadConexaoArquivos.Create(ASocket: IdUDPClient);
{$ENDIF}
{$IF DEFINED (MSWINDOWS)}
        constructor TThreadConexaoArquivos.Create(ASocket: TCustomWinSocket);
{$ENDIF}
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
          BufferTemp,
          FileName: string;
          FileStream: TMemoryStream;
          Locale: TLocale;
        begin
          inherited;
          Locale := TLocale.Create;
          ReceivingFile := False;
          FileStream := nil;

          while (not Terminated) do
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
                If Position > 0 Then
                 Begin
                  BufferTemp := Copy(BufferTemp, 1, Position - 1);
                  FileName := BufferTemp;
                 End
                Else
                 FileName := ActualDownloadFileName;
              end;

              Position := Pos('<|SIZE|>', Buffer);
              if Position > 0 then
              begin
                BufferTemp := Buffer;
                Delete(BufferTemp, 1, Position + 7);
                BufferTemp := Copy(BufferTemp, 1,
                  Pos('<|END|>', BufferTemp) - 1);
                FileSize := StrToInt(BufferTemp);
                FileStream := TMemoryStream.Create;
                if (Conexao.Visualizador) then
                begin
                  Synchronize(
                    procedure
                    begin
                     If Assigned(fFileTransfer) Then
                      Begin
                       fFileTransfer.pgbDownload.Max := FileSize;
                       fFileTransfer.pgbDownload.Value := 0;
                       fFileTransfer.LDownloadSize.Text :=
                         Format(Locale.GetLocaleDlg(MAIN, 'Size'),
                         [TRDLib.GetSize(FileStream.Size),
                         TRDLib.GetSize(FileSize)]);
                      End;
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
                   If Assigned(fFileTransfer) Then
                    Begin
                     fFileTransfer.pgbDownload.Value := FileStream.Size;
                     fFileTransfer.LDownloadSize.Text :=
                       Format(Locale.GetLocaleDlg(MAIN, 'Size'),
                       [TRDLib.GetSize(FileStream.Size),
                       TRDLib.GetSize(FileSize)]);
                    End;
                  end);
              end
              else
              begin
                while Conexao.SocketPrincipal.Socket.SendText
                  ('<|REDIRECT|><|UPLOADPROGRESS|>' + IntToStr(FileStream.Size)
                  + '<|END|>') < 0 do
                  Sleep(FOLGAPROCESSAMENTO);
              end;

              if (FileStream.Size = FileSize) then
              begin
                FileStream.Position := 0;
                If Trim(FileName) <> '' Then
                 Begin
                  If FileExists(FileName) Then
                   DeleteFile(PChar(FileName));
                  FileStream.SaveToFile(FileName);
                  FileName := '';
                 End
                Else
                 Begin
                  If FileExists(ActualDownloadFileName) Then
                   DeleteFile(PChar(ActualDownloadFileName));
                  FileStream.SaveToFile(ActualDownloadFileName);
                  ActualDownloadFileName := '';
                 End;
                FreeAndNil(FileStream);
                Synchronize(
                procedure
                begin
                  try
                  Clipboard.Open;
                  Clipboard.AssignFile(pwchar(GetEnvironmentVariable('TEMP')+'\'+FileName));
                  except
                  end;
                  Clipboard.close;
                end
                );

                //DeleteFile(pwchar(GetEnvironmentVariable('TEMP')+'\'+FileName));
                if not(Conexao.Visualizador) then
                  Conexao.SocketPrincipal.Socket.SendText
                    ('<|REDIRECT|><|UPLOADCOMPLETE|>')
                else
                begin
                  Synchronize(
                    procedure
                    begin
                     If Assigned(fFileTransfer) Then
                      Begin
                       fFileTransfer.pgbDownload.Value := 0;
//                      fFileTransfer.btnDownload.Enabled := True;
                       fFileTransfer.LDownloadSize.Text :=
                         Format(Locale.GetLocaleDlg(MAIN, 'Size'),
                         ['0 B', '0 B']);
                       MessageBox(0, Locale.GetLocaleDlg(MSGS,
                         'DownloadSuccess'), Locale.GetLocaleDlg(FRMS,
                         'FileSubTitle'), MB_ICONASTERISK + MB_TOPMOST);
                      End;
                    end);
                end;

                ReceivingFile := False;
              end;
            end;
          end;
          Locale.DisposeOf;
        end;

        procedure TThreadConexaoArquivos.ThreadTerminate(ASender: TObject);
        begin
          if (Assigned(Conexao)) and (not Terminated) then
            Conexao.LimparThread(ttArquivos);
        end;

end.
