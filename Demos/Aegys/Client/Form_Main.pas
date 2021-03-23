unit Form_Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.pngimage,
  Vcl.ExtCtrls,
  Vcl.StdCtrls, Vcl.Buttons, System.Win.ScktComp, StreamManager,
  System.SyncObjs,
  sndkey32, IdBaseComponent, Vcl.AppEvnts, Vcl.ComCtrls, Winapi.MMSystem,
  Registry, Vcl.Menus, Vcl.Mask, Clipbrd, uProxy, IdComponent, IdTCPConnection,
  IdTCPClient, Shellapi, IdHTTP, IdGlobal, IdBuffer, ZLibEX, System.Zip,
  Vcl.Imaging.jpeg, uIpPoolerService, System.Generics.Collections, IdException,
  IdExceptionCore,
  IdAssignedNumbers, IdHeaderList, IdHTTPHeaderInfo, IdReplyRFC, IdSSL, IdURI,
  IdCookie,
  IdCookieManager, IdAuthentication, IdAuthenticationManager,
  IdMultipartFormData, TThreadTimer,
  IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdIOHandlerStream,
  System.NetEncoding, IdIntercept, IdCompressionIntercept,
  MaskUtils, SimpleTimer, uScanlineComparer,
  System.DateUtils, uComboChoose, uCaptureDeviceMode,
  IdSocketHandle, IdStack, IdUDPClient, IdUDPBase, IdUDPServer, uFilesFoldersOP,
  Tlhelp32, uUDPPooler, uUDPSuperComponents, ComObj, ActiveX,
  JDRMGraphics, ImageCapture, System.ImageList, Vcl.ImgList;

Type
  TConnectionInfo = Packed Record
    Desktop, FilesClient, CommandsClient: TPeerconnected;
  End;

Type
  TExecuteProc = Reference to Procedure;
  TMessageType = (mt_Keyboard = 3050,
    // Mouse Click Down/Up
    mt_MouseLCD = 3051, mt_MouseLCU = 3052, mt_MouseMCD = 4051,
    mt_MouseMCU = 4052, mt_MouseRCD = 5051, mt_MouseRCU = 5052,
    // Mouse Double Click
    mt_MouseDBC = 6051,
    // Mouse Wheel
    mt_MouseWheel = 7050,
    // Mouse Move
    mt_MouseMove = 8050, mt_Clipborad = 9050);

Const // 2kb  //4Kb  //8kb  //16kb
  MaxBuffer = 1024 * 8; // 2048 //4096 //8192 //16384
  MaxBufferProxy = 1024 * 8; // 2048 //4096 //8192 //16384
  TMaxBufferTCP = 1024 * 1;
  MaxNewFrame = 40;
  FrameTimeOut = 2000;
  MaxSendFrames = 0;
  SendForThread = False;
  SendSync = False;
  NewDeskCapture = False;
  OnlyFullBuffer = False;
  DllCapture = False;
  ShowFrameRate = False;
  Cols = 2;
  Rows = 4;
  HightSpeed = False;
  MouseCaptureC = False;
  inTimerCollect = True;
  HabLogs = False;
  CollectorTimerC = 4000;
  TimeOutC = 4000;
  TimeOutF = 500;
  TSendMouseNone = 1000;
  TProxyTimeOutNAT = 10000;
  TProxyTimeOutProxy = 15000;
  FrameRate = 30;
  CaptureW = 0;
  CaptureH = 0;
  CompareLength = 8; // 8 = 8 Bits, 16 = 16 Bits e por aí vai
  vEOB = '<$EOLB$>'; // Meu IP NAT
  mascara = '99-999-999;0;_';
  tInitCom = '<|#INITC#|>';
  tDataSep = '<|#|>';
  tFinalCom = '<|#FINALC#|>';
  TNoneData = '#NONE#';
  TNewFrameData = '<#|NEWFRAME|#>';
  TSendEvents = 50;
  TLineSend = '<|$initstream$|><|>IMAGETYPE=FULL<|>SIZE=%d<|>%s<$COMMANDEND$>';
  ByPass = False;
  ZCompressionLevel = zcDefault; // zcMax; //zcFastest; //zcDefault;
  DefaultAction = stNAT;//stProxy;

Type
  TListitems = TList<TMemoryStream>;

type
  Tfrm_Main = class(TForm)
    Reconnect_Timer: TTimer;
    Timeout_Timer: TTimer;
    Clipboard_Timer: TTimer;
    TicServer: TTrayIcon;
    Time_Update: TTimer;
    TimeoutAction: TTimer;
    tScreenshot: TTimer;
    pnl_topo: TPanel;
    MainMenu1: TMainMenu;
    Conexo1: TMenuItem;
    Opes1: TMenuItem;
    Panel2: TPanel;
    Image5: TImage;
    Status_Label: TLabel;
    lbl_mypc: TLabel;
    Status_Image: TImage;
    Image1: TImage;
    Image3: TImage;
    Image2: TImage;
    lStatusCon: TLabel;
    Panel1: TPanel;
    Panel3: TPanel;
    mmNova: TMemo;
    mmAtual: TMemo;
    Label4: TLabel;
    Label3: TLabel;
    Label1: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    YourPassword_Edit: TEdit;
    Connect_BitBtn: TButton;
    YourID_Edit: TEdit;
    Panel4: TPanel;
    Panel5: TPanel;
    TargetID_MaskEdit: TComboBox;
    cbQualidade: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure Reconnect_TimerTimer(Sender: TObject);
    procedure Timeout_TimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Clipboard_TimerTimer(Sender: TObject);
    procedure mniConfigClick(Sender: TObject);
    procedure mniShowClick(Sender: TObject);
    procedure mniMinimiserClick(Sender: TObject);
    procedure mniCloseClick(Sender: TObject);
    procedure TicServerDblClick(Sender: TObject);
    procedure Time_UpdateTimer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure TargetID_MaskEditxChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure tScreenShotTimer(Sender: TObject);
    procedure TimeoutActionTimer(Sender: TObject);
    procedure Opes1Click(Sender: TObject);
    procedure lblstatusconexaoClick(Sender: TObject);
    procedure Connect_BitBtnClick(Sender: TObject);
    procedure TargetID_MaskEditKeyPress(Sender: TObject; var Key: Char);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    vCloseConnection, vMouseCapture, vNewFrameClient, vInitCapture,
      vInitSection: Boolean;
    vLinhaKeys, vCommandEnd: String;
    ippMain_Socket: TIdTCPClient;
    vSendData, vReceiveData: TProcessData;
    vSendType: TSendType;
    // ippDesktopClient,
    // ippPSFilesClient  : TIdUDPServer;
    // SimpleTimer       : TSimpleTimer;
    vComboList: TComboList;
    vLogsList: TStringList;
    vInitImageCapture: TImageCapture;
    ConnectionInfo: TConnectionInfo;
    Procedure OpenUPNPPorts;
    Procedure CaptureExecute;
    Procedure CreateComponents;
    Procedure DestroyComponents;
    Procedure DeactiveComponents;
    Procedure checksUpdates;
    Procedure LoadConfigs;
    Function GetDirectoryToSaveFile: String;
    Procedure SetDirectoryToSaveFile(Value: String);
    Procedure Connected(StatusCode: Integer; Const Description: String);
    Procedure Disconnected(StatusCode: Integer; Const Description: String);
    Procedure Error(ErrorCode: Integer; Const Description: String);
    Procedure DataIn(Value: String);
    Procedure ReadyToSendClient;
    Procedure Main_SocketDisconnected(StatusCode: Integer;
      Const Description: String);
    Procedure Main_SocketError(ErrorCode: Integer; Const Description: String);
    Procedure Main_SocketDataIn;
    Procedure Main_SocketReadyToSendClient;
    Procedure OnPeerConnected(PeerConnected: TPeerconnected);
    Procedure OnPeerConnectedFilesClient(PeerConnected: TPeerconnected);
    Procedure OnPeerConnectedCommandsClient(PeerConnected: TPeerconnected);
    Procedure OnPeerConTimeOut(PeerIP, LocalIP: String; Port: Word);
    Procedure DataInFiles(Value: String);
    Procedure ConnectAll; Overload;
    Procedure ConnectAll(SendType: TSendType; Active: Boolean = False);
      Overload;
    Procedure LoadComboOptions;
    Procedure TargetAdd(Value: String);
    Procedure SetLinhaKeys(Value: String);
    Procedure ProcessKeys(Value: String);
    Function GetStopSendFile: Boolean;
    Procedure SetStopSendFile(Value: Boolean);
    Function GetCancelOPSendFile: Boolean;
    Procedure SetCancelOPSendFile(Value: Boolean);
    Procedure ProcessCommands(Var ListPacks: TListPacks);
    Procedure OnGetLongString(Value: String);
    Procedure OnBinaryIn(Value: TIdBytes);
    procedure AdicionaFirewall;
    Procedure OnProgress(Sender: TObject);
  public
    { Public declarations }
    MyID, URL, MyPassword, LastPassWord, LastPassWordClient: String;
    ResolutionTargetWidth, ResolutionTargetHeight: Integer;
    CritialSection: TCriticalSection;
    ipPSMain_Socket: TipPoolerService;
    ipPSDeskTopClient, ipCommandsClient, ipPSFilesClient: TUDPSuperClient;
    SendingFile, Viewer: Boolean;
    FileSize: Int64;
    Function GetSize(Bytes: Int64): String;
    Procedure AddLog(Value: String);
    Procedure ExecMethod(Execute: TExecuteProc = Nil; Sincro: Boolean = False);
    procedure ClearConnection;
    procedure SetOffline;
    procedure SetOnline;
    procedure Reconnect;
    procedure CloseSockets;
    Property DirectoryToSaveFile: String Read GetDirectoryToSaveFile
      Write SetDirectoryToSaveFile;
    Property CommandEnd: String Read vCommandEnd;
    Property CloseConnection: Boolean Read vCloseConnection;
    Property InitSection: Boolean Read vInitSection;
    Property MouseCapture: Boolean Read vMouseCapture Write vMouseCapture;
    Property InitCapture: Boolean Read vInitCapture Write vInitCapture;
    Property LinhaKeys: String Read vLinhaKeys Write SetLinhaKeys;
    Property StopSendFile: Boolean Read GetStopSendFile Write SetStopSendFile;
    Property CancelOPSendFile: Boolean Read GetCancelOPSendFile
      Write SetCancelOPSendFile;
    Property GetFrame: Boolean Read vNewFrameClient Write vNewFrameClient;
    Property SendData: TProcessData Read vSendData Write vSendData;
    Property ReceiveData: TProcessData Read vReceiveData Write vReceiveData;
    Property SendType: TSendType Read vSendType;

    procedure Botao_conectar_parceiro;
  end;

Procedure SendStreamM(OutputClient: TipPoolerService; Picture: TMemoryStream;
  Buffered: Boolean = True); Overload;
Function SendStreamQueue(OutputClient: TUDPSuperClient; Var NewFrame: Boolean;
  Buffered: Boolean = True; ImageViewQ: TImageViewQ = tiv_Medium)
  : Boolean; Overload;
Procedure SendStreamF(OutputClient: TUDPSuperClient; FileStream: TFileStream;
  Buffered: Boolean = False); Overload;
Procedure SendStreamS(InputValue: String);
Procedure GetStreamM(InputClient: TUDPSuperClient;
  Var StreamValue: TMemoryStream); Overload;
Procedure GetStreamM(InputClient: TUDPSuperClient; InputValue: String);
  Overload;
Procedure GetStreamS(InputValue: String);
Procedure GetStreamF(InputClient: TIdTCPClient; Var StreamValue: TFileStream;
  FileSize: LongInt = 0); Overload;
Procedure GetStreamF(InputClient: TipPoolerService;
  Var StreamValue: TFileStream); Overload;
// Novas Procedures
Procedure LogoffRules(Rule: Cardinal);
Procedure BlockInput(ABlockInput: Boolean); Stdcall; External 'USER32.DLL';

Var
  frm_Main: Tfrm_Main;
  MousePosX, MousePosY, NewFrame, ResolutionWidth, ResolutionHeight,
    Timeout: Integer;
  TimeoutConnectionT: Word;
  indexbmp: Integer = 0;
  vTries: Integer = 0;
  vLinhaSend, vDecompressString, vInitString, vDirectoryToSaveFile,
    OldWallpaper, OldClipboardText, vMachine, vGroup, vLineSendOld,
    vLineSendD: String;
  ImageViewQ: TImageViewQ = tiv_Medium;
  InCreate: Boolean = True;
  vNotClose: Boolean = True;
  Diferencial: Boolean = True;
  Comparer: Boolean = True;
  vPeerNotify: Boolean = False;
  vIConnect: Boolean = False;
  InClose: Boolean = False;
  vIdentify: Boolean = False;
  SendResolution: Boolean = False;
  OnGetScreenShot: Boolean = False;
  OnSend: Boolean = False;
  OnSendComands: Boolean = False;
  ReceivingFile: Boolean = False;
  vCloseSockets: Boolean = False;
  vEnterInMainSocket: Boolean = False;
  vSendMyData: Boolean = False;
  vCaptureSideClient: Boolean = False;
  vStopSendFile: Boolean = False;
  vCancelOPSendFile: Boolean = False;
  WaitDisconnect: Boolean = False;
  CtrlPressed: Boolean = False;
  ShiftPressed: Boolean = False;
  AltPressed: Boolean = False;

  vExecuteData: Boolean = False;

  vWhereNew: Boolean = False;

  Accessed, LostConnection: Boolean;
  vCaptureScreens: TListitems;
  vOldStream: TMemoryStream = Nil;
  vOldS: String = '';
  vFrameRate, LastFrameTime: TDateTime;
  CompressionEncoding, CompressionDecoding: TEncoding;
  ASMSize, muASM: Integer;
  pdst: Pointer;

implementation

{$R *.dfm}

uses
  Form_Password, Form_RemoteScreen, Form_Chat, Form_ShareFiles, Form_Config,
  uUteis, uCaptureScreen, uConnectar;

Function Tfrm_Main.GetCancelOPSendFile: Boolean;
Begin
  Result := vCancelOPSendFile;
End;

Procedure Tfrm_Main.SetCancelOPSendFile(Value: Boolean);
Begin
  vCancelOPSendFile := Value;
End;

Function Tfrm_Main.GetStopSendFile: Boolean;
Begin
  Result := vStopSendFile;
End;

procedure Tfrm_Main.Connect_BitBtnClick(Sender: TObject);
begin
  Botao_conectar_parceiro;
end;

procedure Tfrm_Main.AdicionaFirewall;
Var
  Parametros, Parametros2: String;
  VersionInfo: TOSVersionInfo;
Begin
  Parametros := '';
  Parametros2 := Parametros;
  VersionInfo.dwOSVersionInfoSize := SizeOf(VersionInfo);
  GetVersionEx(VersionInfo);
  With VersionInfo Do
  Begin
    Case dwPlatformId Of
      1:
        Begin
          Case dwMinorVersion of
            0, 10, 90:
              Parametros := 'add allowedprogram "' + Application.ExeName +
                '" "Aegys" enable all';
          End;
        End;
      2:
        Begin
          Case dwMajorVersion Of
            3, 4, 5:
              Parametros := 'add allowedprogram "' + Application.ExeName +
                '" "Aegys" enable all';
          Else
            Begin
              Parametros := 'advfirewall firewall add rule name="Aegys" ' +
                'dir=in action=allow profile=any protocol=UDP ' +
                'description="Aegys Acesso Remoto" ' + 'program="' +
                Application.ExeName + '"';
              Parametros2 := 'advfirewall firewall add rule name="Aegys" ' +
                'dir=in action=allow profile=any protocol=TCP ' +
                'description="Aegys Acesso Remoto" ' + 'program="' +
                Application.ExeName + '"';
            End;
          End;
        End;
    End;
  End;
  ShellExecute(Handle, 'open', 'netsh', PChar(Parametros), '', SW_HIDE);
  If Parametros2 <> '' Then
    ShellExecute(Handle, 'open', 'netsh', PChar(Parametros2), '', SW_HIDE);
end;

procedure Tfrm_Main.Botao_conectar_parceiro;
Var
  formato: String;
  Function LimpaLixo(Value: String): String;
  Begin
    Result := Trim(StringReplace(Value, '-', '', [rfReplaceAll]));
  End;

begin
  If (LimpaLixo(TargetID_MaskEdit.Text) <> '') And
    (Length(LimpaLixo(TargetID_MaskEdit.Text)) = 8) Then
  Begin
    If (TargetID_MaskEdit.Text = MyID) Then
      Application.MessageBox('Você não pode conectar a si mesmo!', 'Aegys', 16)
    Else
    Begin
      formato := StringReplace(TargetID_MaskEdit.Text, '-', '',
        [rfReplaceAll, rfIgnoreCase]);
      formato := Trim(formato);
      formato := MaskDoFormatText(mascara, formato, #0);
      ipPSMain_Socket.Write('<|FINDID|>' + formato + '<|>' + '<|LASTPASSWORD|>'
        + LastPassWord + vCommandEnd);
      TargetID_MaskEdit.Enabled := False;
      Connect_BitBtn.Enabled := False;
      Status_Image.Picture.Assign(Image1.Picture);
      vWhereNew := DefaultAction = stProxy;
      ConnectAll(DefaultAction);
      Status_Label.Caption := 'Procurando o ID...';
    End;
  End
  Else
    Application.MessageBox('O ID Digitado é inválido!', 'Aegys', 16);
end;

procedure Tfrm_Main.Button1Click(Sender: TObject);
begin
  Botao_conectar_parceiro;
end;

procedure Tfrm_Main.tScreenShotTimer(Sender: TObject);
begin
  If vIConnect Then
  Begin
    If frm_RemoteScreen <> Nil Then
      frm_RemoteScreen.SendEventsTimer;
  End
  Else
  Begin
    If Not vStopSendFile Then
      CaptureExecute
    Else
      tScreenshot.Enabled := False;
    If Not tScreenshot.Enabled Then
      vSendData.Active := False;
  End;
  If (vIConnect) Then
  Begin
{$IFDEF MSWINDOWS}
{$IFNDEF FMX}Application.Processmessages;
{$ELSE}FMX.Forms.TApplication.Processmessages; {$ENDIF}
{$ENDIF}
  End;
end;

Procedure Tfrm_Main.SetStopSendFile(Value: Boolean);
Begin
  vStopSendFile := Value;
  {
    if Not (vStopSendFile) Then
    Begin
    vFileToUpload
    End
  }
End;

Procedure LogoffRules(Rule: Cardinal);
Var
  SimbPriv: TTokenPrivileges;
  X: DWORD;
  HSimb: THandle;
Begin
  // Desligamento forçado      : EWX_FORCE
  // Dxecutar logoff           : EWX_LOGOFF
  // Executa o desligamento    : EWX_POWEROFF
  // Executa a reinicialização : EWX_REBOOT
  // Executa o encerramento    : EWX_SHUTDOWN
  If Win32Platform = VER_PLATFORM_WIN32_NT Then
  Begin
    OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES, HSimb);
    LookUpPrivilegeValue(Nil, 'SeShutdownPrivilege',
      SimbPriv.Privileges[0].Luid);
    SimbPriv.PrivilegeCount := 1;
    SimbPriv.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
    X := 0;
    AdjustTokenPrivileges(HSimb, False, SimbPriv, 0, PTokenPrivileges(nil)^, X);
    CloseHandle(HSimb);
  End;
  ExitWindowsEx(Rule, 0);
End;

Function Tfrm_Main.GetDirectoryToSaveFile: String;
Begin
  Result := vDirectoryToSaveFile;
End;

Procedure Tfrm_Main.SetDirectoryToSaveFile(Value: String);
Begin
  vDirectoryToSaveFile := Value;
End;

// Get current Version
Function GetAppVersionStr: String;
Var
  Exe: String;
  Size, Handle: DWORD;
  Buffer: TBytes;
  FixedPtr: PVSFixedFileInfo;
Begin
  Exe := ParamStr(0);
  Size := GetFileVersionInfoSize(PChar(Exe), Handle);
  If Size = 0 Then
    RaiseLastOSError;
  SetLength(Buffer, Size);
  If Not GetFileVersionInfo(PChar(Exe), Handle, Size, Buffer) Then
    RaiseLastOSError;
  If Not VerQueryValue(Buffer, '\', Pointer(FixedPtr), Size) Then
    RaiseLastOSError;
  Result := Format('%d.%d.%d.%d', [LongRec(FixedPtr.dwFileVersionMS).Hi,
    // major
    LongRec(FixedPtr.dwFileVersionMS).Lo, // minor
    LongRec(FixedPtr.dwFileVersionLS).Hi, // release
    LongRec(FixedPtr.dwFileVersionLS).Lo]) // build
End;

Function GetWallpaperDirectory: String;
Var
  Reg: TRegistry;
Begin
  Reg := nil;
  Try
    Reg := TRegistry.Create;
    Reg.RootKey := HKEY_CURRENT_USER;
    Reg.OpenKey('Control Panel\Desktop', False);
    Result := Reg.ReadString('Wallpaper');
  Finally
    Reg.DisposeOf;
  End;
End;

Procedure ChangeWallpaper(Directory: String);
Var
  Reg: TRegistry;
Begin
  Reg := Nil;
  Try
    Reg := TRegistry.Create;
    Reg.RootKey := HKEY_CURRENT_USER;
    Reg.OpenKey('Control Panel\Desktop', False);
    Reg.WriteString('Wallpaper', Directory);
    SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, Nil, SPIF_SENDWININICHANGE);
  Finally
    Reg.DisposeOf;
  End;
End;

Procedure Tfrm_Main.checksUpdates;
Var
  arquivo, caminho: String;
  MyFile: TFileStream;
  IdHTTP: TIdHTTP;
Begin
  caminho := URL; // local onde estará o arquivo versaoatual.txt
  arquivo := 'versaoatual.txt';
  // nome do arquivo a ser baixado (versaoatual.txt)
  MyFile := TFileStream.Create(arquivo, fmCreate); // cria o versaoatual.txt
  Try
   Try
    IdHTTP := TIdHTTP.Create(Nil);
    IdHTTP.Get(caminho + arquivo, MyFile); // baixando versaoatual.txt
    { CARREGA VERSAOATUAL E VERSAO.TXT NOS MEMOS }
    If FileExists(arquivo) Then
     mmAtual.Lines.LoadFromFile(arquivo); // carregar o versaoatual.txt no memo2
    If FileExists('versao.txt') Then
     mmNova.Lines.LoadFromFile('versao.txt'); // carrega o versao.txt no memo3
    { verifica a versao }
    If (mmAtual.Lines.Count > 0) And (mmNova.Lines.Count > 0) Then
    If mmAtual.Lines[0] <> mmNova.Lines[0] Then
     Begin
      If Application.MessageBox('Há uma nova versão do Aegys,' + #13#10 +
        'Deseja atualizar o seu Aegys?', 'Update!!',
        MB_YESNO + MB_ICONINFORMATION) = IDYES Then
        ShellExecute(Handle, 'open', PChar('Update.exe'), '', '',
          SW_SHOWNORMAL);
     End;
   Finally
    MyFile.Free;
    IdHTTP.Free;
   End;
  Except
  End;
End;

procedure Tfrm_Main.ClearConnection;
begin
  If frm_Main = Nil Then
    Exit;
  frm_Main.ResolutionTargetWidth := 0;
  frm_Main.ResolutionTargetHeight := 0;
  If frm_RemoteScreen <> Nil Then
  Begin
    With frm_RemoteScreen Do
    Begin
      // MouseIcon_Image.Picture.Assign(MouseIcon_unchecked_Image.Picture);
      // KeyboardIcon_Image.Picture.Assign(KeyboardIcon_unchecked_Image.Picture);
      // ResizeIcon_Image.Picture.Assign(ResizeIcon_checked_Image.Picture);
      MouseIcon_Image.Down := False;
      KeyboardIcon_Image.Down := MouseIcon_Image.Down;
      // ResizeIcon_Image.Down := true;
      // Screen_Image.Picture.Assign(ScreenStart_Image.Picture);
      Width := 986;
      Height := 646;
    End;
  End;
  If frm_ShareFiles <> Nil Then
  Begin
    With frm_ShareFiles do
    Begin
      Download_BitBtn.Enabled := True;
      Upload_BitBtn.Enabled := Download_BitBtn.Enabled;
      Download_ProgressBar.Position := 0;
      Upload_ProgressBar.Position := 0;
      SizeDownload_Label.Caption := 'Tamanho: 0 B / 0 B';
      SizeUpload_Label.Caption := 'Tamanho: 0 B / 0 B';
      Directory_Edit := 'C:\';
      ShareFiles_ListView.Items.Clear;
      If (Visible) Then
        Close;
    End;
  End;
  If frm_RemoteScreen <> Nil then
  Begin
    frm_RemoteScreen.rzpDownload.Visible := False;
    If frm_ShareFiles <> Nil Then
      frm_ShareFiles.FreeForClose := Not(frm_RemoteScreen.rzpDownload.Visible);
    frm_RemoteScreen.pbDados.Position := 0;
  End;
  If frm_Chat <> Nil Then
  Begin
    With frm_Chat do
    Begin
      Width := 230;
      Height := 340;
      Left := Screen.WorkAreaWidth - Width;
      Top := Screen.WorkAreaHeight - Height;
      Chat_RichEdit.Clear;
      YourText_Edit.Clear;
      Chat_RichEdit.SelStart := Chat_RichEdit.GetTextLen;
      Chat_RichEdit.SelAttributes.Style := [fsBold];
      Chat_RichEdit.SelAttributes.Color := clWhite;
      Chat_RichEdit.SelText := 'Aegys - Chat' + #13 + #13;
      FirstMessage := True;
      LastMessageAreYou := False;
      If (Visible) Then
        Close;
    End;
  End;
end;

procedure Tfrm_Main.Clipboard_TimerTimer(Sender: TObject);
Begin
  If Viewer Then
  Begin
    Try
      Clipboard.Open;
      If (Clipboard.HasFormat(CF_TEXT)) then
      Begin
        If Not(OldClipboardText = Clipboard.AsText) Then
        Begin
          OldClipboardText := Clipboard.AsText;
          vLinhaKeys := '<|CLIPBOARD|>' + OldClipboardText + vCommandEnd +
            vLinhaKeys;
        End;
      End;
    Finally
      Clipboard.Close;
    End;
  End;
End;

// Return size (B, KB, MB or GB)
Function Tfrm_Main.GetSize(Bytes: Int64): String;
Const
  K = Int64(1024);
  M = K * K;
  G = K * M;
  T = K * G;
Begin
  If Bytes < K Then
    Result := Format('%d B', [Bytes])
  Else If Bytes < M Then
    Result := Format('%f KB', [Bytes / K])
  Else If Bytes < G Then
    Result := Format('%f MB', [Bytes / M])
  Else If Bytes < T Then
    Result := Format('%f GB', [Bytes / G])
  Else
    Result := Format('%f TB', [Bytes / T]);
End;

Procedure ListDrivers(Var ReturnData: TStringList);
Var
  ShellProps: TShellProps;
  I: Integer;
Begin
  ShellProps := TShellProps.Create;
  ReturnData.Clear;
  ReturnData.Add(ShellProps.LocalStation);
  For I := 0 To ShellProps.Drivers.Count - 1 Do
    ReturnData.Add(ShellProps.Drivers[I]);
  FreeAndNil(ShellProps);
End;

Procedure ListFoldersB(Directory: String; Var ReturnData: TStringList);
Var
  FileName, Filelist: String;
  Searchrec: TWin32FindData;
  FindHandle: THandle;
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
            If ((Searchrec.dwFileAttributes And FILE_ATTRIBUTE_DIRECTORY)
              <> 0) Then
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

// Function to List Folders
Function ListFolders(Directory: String): String;
Var
  FileName, Filelist, Dirlist, ReturnStr: String;
  Searchrec: TWin32FindData;
  FindHandle: THandle;
Begin
  ReturnStr := '';
  If Directory <> '' Then
  Begin
    Try
      FindHandle := FindFirstFile(PChar(Directory + '*.*'), Searchrec);
      If FindHandle <> INVALID_HANDLE_VALUE Then
      Begin
        Repeat
          FileName := Searchrec.cFileName;
          If (FileName = '.') Then
            Continue;
          If ((Searchrec.dwFileAttributes And FILE_ATTRIBUTE_DIRECTORY)
            <> 0) Then
            Dirlist := Dirlist + (FileName + #13)
          Else
            Filelist := Filelist + (FileName + #13);
        Until Not(FindNextFile(FindHandle, Searchrec));
      End;
    Finally
      Winapi.Windows.FindClose(FindHandle);
    End;
  End;
  ReturnStr := (Dirlist);
  Result := ReturnStr;
End;

Procedure ListFilesB(FileName: String; Var Return: TStringList);
Var
  ShellProps: TShellProps;
  I: Integer;
  vLinha: AnsiString;
Begin
  ShellProps := TShellProps.Create;
  ShellProps.Folder := FileName;
  Return.Clear;
  Try
    For I := 0 To ShellProps.FilesCount - 1 do
    Begin
      If ShellProps.Files[I].FileType <> fpDir Then
      Begin
        vLinha := Format('%s|%s|%s|%s', [ShellProps.Files[I].FileName,
          frm_Main.GetSize(ShellProps.Files[I].FileSize),
          ShellProps.Files[I].FileTypeDesc,
          FormatDateTime('dd/mm/yyyy hh:mm:ss',
          ShellProps.Files[I].LastWrite)]);
        Return.Add(vLinha);
      End;
    End;
  Finally
  End;
End;

// Function to List Files
Function ListFiles(FileName, Ext: string): string;
var
  SearchFile: TSearchRec;
  FindResult: Integer;
  Arc: TStrings;
begin
  Arc := TStringList.Create;
  FindResult := FindFirst(FileName + Ext, faArchive, SearchFile);
  try
    while FindResult = 0 do
    begin
      // Application.ProcessMessages;
      Arc.Add(SearchFile.Name);
      FindResult := FindNext(SearchFile);
    end;
  finally
    FindClose(SearchFile)
  end;
  Result := Arc.Text;
end;

procedure Tfrm_Main.Reconnect;
Begin
  Try
   If ipPSMain_Socket <> Nil Then
    Begin
     If Not(ipPSMain_Socket.Active) then
      Begin
       Try
        ipPSMain_Socket.Active := False;
        ipPSMain_Socket.Active := True;
       Except
       End;
       If Not ipPSMain_Socket.Active Then
        Begin
         if frm_Main <> Nil then
          Begin
           Status_Image.Picture.Assign(Image2.Picture);
           Status_Label.Caption := 'Offline, Aguardando conexão com o servidor...';
          End;
        End
       Else
        Begin
         If ipPSDeskTopClient.SendType = stNAT Then
          lStatusCon.Caption := 'Status : Aguardando Comandos...'
         Else
          lStatusCon.Caption := 'Status : Aguardando Comandos...';
        End;
      End;
    End;
  Except
   If ipPSMain_Socket <> Nil Then
    Begin
     If Not ipPSMain_Socket.Active Then
      Begin
       If frm_Main <> Nil Then
        Begin
         Status_Image.Picture.Assign(Image2.Picture);
         Status_Label.Caption := 'Offline, Aguardando conexão com o servidor...';
        End;
      End;
    End;
  End;
End;

Procedure Tfrm_Main.CloseSockets;
Begin
 Sleep(500);
 vWhereNew := False;
 vPeerNotify := False;
 vExecuteData := False;
 vCancelOPSendFile := False;
 vInitCapture := False;
 vStopSendFile := True;
 vIConnect := False;
 Self.Visible := True;
 vCaptureSideClient := False;
 vSendMyData := False;
 vInitSection := False;
 If vReceiveData <> Nil Then
  vReceiveData.Active := False;
 If dmCaptureScreen <> Nil Then
  dmCaptureScreen.RenewCommand;
 If Not vCloseConnection Then
  vCloseConnection := True;
 If NewDeskCapture Then
  Begin
   If dmCaptureScreen.FastDesktop <> Nil Then
    dmCaptureScreen.FastDesktop.Active := False;
  End
 Else
  Begin
   If tScreenshot <> Nil Then
    Begin
     tScreenshot.Enabled := False;
     vSendData.Active := False;
    End;
  End;
 If vCloseSockets Then
  Exit
 Else
  vCloseSockets := True;
 Reconnect_Timer.Enabled := False;
 // TerminateAllThreads;
 ClearConnection;
 dmCaptureScreen.StopCapture;
 If Not InClose Then
  Begin
   // DeactiveComponents;
   DestroyComponents;
   CreateComponents;
   vEnterInMainSocket := True;
   WaitDisconnect := False;
  End
 Else
  DestroyComponents;
 Viewer := False;
 // Restore Wallpaper
 If (Accessed) Then
  Begin
   Accessed := False;
   ChangeWallpaper(OldWallpaper);
  End;
 // Show main form and repaint
 If (Self <> Nil) Then
  ShowApplication;
 If frm_RemoteScreen <> Nil Then
  frm_RemoteScreen.Close;
 // Application.Restore;
 If (Not(InClose)) Then
  Reconnect_Timer.Enabled := True;
 vCloseSockets := False;
 If ipPSDeskTopClient <> Nil Then
  Begin
   If ipPSDeskTopClient.SendType = stNAT Then
    lStatusCon.Caption := 'Status : Desconectado'
   Else
    lStatusCon.Caption := 'Status : Desconectado';
  End;
End;

Procedure SetConnected;
Begin
 With frm_Main Do
  Begin
   YourID_Edit.Text := 'Recebendo...';
   YourID_Edit.Enabled := False;
   YourPassword_Edit.Text := 'Recebendo...';
   YourPassword_Edit.Enabled := False;
   Connect_BitBtn.Enabled := False;
  End;
End;

procedure Tfrm_Main.SetOffline;
Var
 I : Integer;
Begin
 If frm_Main <> Nil Then
  Begin
   vWhereNew := False;
   Timeout_Timer.Enabled := False;
   Clipboard_Timer.Enabled := False;
   YourID_Edit.Text := 'Offline';
   YourID_Edit.Enabled := False;
   If TryStrToInt(YourPassword_Edit.Text, I) Then
    LastPassWord := YourPassword_Edit.Text;
   YourPassword_Edit.Text := 'Offline';
   YourPassword_Edit.Enabled := False;
   Connect_BitBtn.Enabled := False;
  End;
End;

Procedure Tfrm_Main.SetOnline;
Var
 formato : String;
begin
 formato := StringReplace(frm_Main.TargetID_MaskEdit.Text, '-', '', [rfReplaceAll, rfIgnoreCase]);
 formato := Trim(formato);
 formato := MaskDoFormatText(mascara, formato, #0);
 YourID_Edit.Text := MyID;
 YourID_Edit.Enabled := True;
 If LastPassWord = '' Then
  LastPassWord := MyPassword;
 YourPassword_Edit.Text := LastPassWord;
 YourPassword_Edit.Enabled := True;
 // TargetID_MaskEdit.Clear;
 TargetID_MaskEdit.Enabled := True;
 Connect_BitBtn.Enabled := True;
 If (vParID <> '') And (vParSenha <> '') Then
  Begin
   TargetID_MaskEdit.Text := vParID;
   ipPSMain_Socket.Write('<|CHECKIDPASSWORD|>' + formato + '<|>' + vParSenha + vCommandEnd);
  End;
End;

Function FileToMemoryStream(OpenFile        : AnsiString;
                            Const SrcStream : TMemoryStream): Boolean;
Var
 fs : TFileStream;
Begin
 Result := False;
 fs := TFileStream.Create(OpenFile, fmOpenRead or fmShareDenyNone);
 Try
  If fs.Size > 0 Then
   Begin
    fs.Seek(0, soFromBeginning);
    SrcStream.Clear;
    SrcStream.SetSize(0);
    SrcStream.CopyFrom(fs, fs.Size);
    Result := True;
   End;
 Finally
  fs.Free;
 End;
End;

Function MemoryStreamToFile(Const SrcStream : TMemoryStream;
                            DestFile        : AnsiString) : Boolean;
Var
 fs : TFileStream;
Begin
 Result := False;
 fs := TFileStream.Create(DestFile, fmCreate or fmShareExclusive);
 Try
  DeleteFile(DestFile);
  If SrcStream.Size > 0 Then
   Begin
    SrcStream.Seek(0, soFromBeginning);
    fs.CopyFrom(SrcStream, SrcStream.Size);
    Result := True;
   End;
 Finally
  fs.Free;
 End;
End;

Function HexToString(H : String) : String;
Begin
 SetLength(Result, Length(H) div 4);
 HexToBin(PWideChar(H), Result[1], Length(H) div SizeOf(Char));
End;

Procedure Tfrm_Main.OnProgress(Sender: TObject);
Begin
 {$IFDEF MSWINDOWS}
 {$IFNDEF FMX}Application.Processmessages;
 {$ELSE}FMX.Forms.TApplication.Processmessages; {$ENDIF}
 {$ENDIF}
End;

Function ZDecompressStr(const S   : String;
                        Var Value : String) : Boolean;
Var
 zipFile   : TZDecompressionStream;
 strInput,
 strOutput : TStringStream;
Begin
 Result := False;
 Value := '';
 If S <> '' Then
  Begin
   Try
    strInput  := TStringStream.Create(S,  CompressionDecoding);
    strOutput := TStringStream.Create('', CompressionEncoding);
    Try
     strInput.Position  := 0;
     zipFile            := TZDecompressionStream.Create(strInput);
     zipFile.OnProgress := frm_Main.OnProgress;
     zipFile.Position   := 0;
     strOutput.CopyFrom(zipFile, zipFile.Size);
     strOutput.Position := 0;
     Value              := strOutput.DataString;
     Result             := True;
    Except
    End;
    zipFile.Free;
   Finally
    strInput.Free;
    strOutput.Free;
   End;
  End;
End;

Procedure ZDecompressStream(const S   : TMemoryStream;
                            Var Value : TMemoryStream);
Var
 zipFile : TZDecompressionStream;
Begin
 If S.Size > 0 Then
  Begin
   Value := TMemoryStream.Create;
   Try
    S.Position := 0;
    zipFile := TZDecompressionStream.Create(S);
    zipFile.OnProgress := frm_Main.OnProgress;
    zipFile.Position := 0;
    Value.CopyFrom(zipFile, zipFile.Size);
    Value.Position := 0;
   Except
   End;
   zipFile.Free;
  End;
End;

Function StringToHex(S : String): String;
Begin
 SetLength(Result, Length(S) * 4);
 BinToHex(S[1], PWideChar(Result), Length(S) * SizeOf(Char));
End;

Function ZCompressStr(const S : String;
                      level   : TZCompressionLevel = zcMax) : TIdBytes;
Var
 Compress   : TzCompressionStream;
 SrcStream,
 OutPut     : TStringStream;
Begin
 OutPut    := TStringStream.Create('', CompressionDecoding);
 SrcStream := TStringStream.Create(S, CompressionEncoding);
 OutPut.Position := 0;
 Try
  Compress := TzCompressionStream.Create(OutPut, ZCompressionLevel);
  Compress.OnProgress := dmCaptureScreen.OnProgress;
  Compress.CopyFrom(SrcStream, 0);
  FreeAndNil(Compress);
  OutPut.Position := 0;
  If OutPut.Size > 0 Then
   ReadTIdBytesFromStream(OutPut, Result, OutPut.Size);
  // Result          := OutPut.DataString;
  FreeAndNil(OutPut);
  FreeAndNil(SrcStream);
 Except
  SetLength(Result, 0);
 End;
End;

Function CompressStreamS(Const SrcS : String;
                         Var DestS  : String): Boolean;
Var
 zipFile   : TzCompressionStream;
 strInput,
 strOutput : TStringStream;
Begin
 Result := False;
 Try
  strInput  := TStringStream.Create(SrcS);
  strOutput := TStringStream.Create;
  Try
   zipFile := TzCompressionStream.Create(strOutput, zcDefault);
   zipFile.CopyFrom(strInput, strInput.Size);
  Finally
   zipFile.Free;
  End;
  DestS := strOutput.DataString;
  Result := True;
 Finally
  strInput.Free;
  strOutput.Free;
 End;
End;

// Compress Stream with zLib
Function CompressStream(Const SrcStream : TMemoryStream;
                        DestStream      : TMemoryStream) : Boolean;
Var
 zipFile : TzCompressionStream;
Begin
 Result := False;
 Try
  zipFile := TzCompressionStream.Create(DestStream, zcDefault);
  SrcStream.Position := 0;
  zipFile.CopyFrom(SrcStream, SrcStream.Size);
  Result := True;
 Except
 End;
 zipFile.Free;
End;

// Decompress Stream with zLib
Function DeCompressStream(Const SrcStream : TMemoryStream;
                          Var DestStream  : TMemoryStream) : Boolean;
Var
 zipFile : TZDecompressionStream;
Begin
 Result := False;
 SrcStream.Position := 0;
 zipFile := TZDecompressionStream.Create(SrcStream);
 Try
  zipFile.Position := 0;
  DestStream.Clear;
  DestStream.SetSize(0);
  DestStream.CopyFrom(zipFile, zipFile.Size);
  Result := True;
 Except
 End;
 zipFile.Free;
End;

// Decompress Stream with zLib
Function DeCompressStreamS(Const SrcS : String;
                           Var DestS  : String) : Boolean;
Var
 zipFile   : TZDecompressionStream;
 strInput,
 strOutput : TStringStream;
Begin
 Result := False;
 If SrcS <> '' Then
  Begin
   Try
    strInput := TStringStream.Create(SrcS);
    strOutput := TStringStream.Create;
    Try
     zipFile := TZDecompressionStream.Create(strInput);
     zipFile.Position := 0;
     strOutput.CopyFrom(zipFile, zipFile.Size);
     strOutput.Position := 0;
     DestS := strOutput.DataString;
     Result := True;
    Except
    End;
    zipFile.Free;
   Finally
    strInput.Free;
    strOutput.Free;
   End;
  End;
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

Procedure StringToMemoryStream(Value : TMemoryStream;
                               Str   : String);
Var
 _MemStr: TStringStream;
Begin
 _MemStr := TStringStream.Create(Str);
 Try
  _MemStr.SaveToStream(Value);
 Finally
  FreeAndNil(_MemStr);
 End;
End;

Procedure Tfrm_Main.ExecMethod(Execute : TExecuteProc = Nil;
                               Sincro  : Boolean      = False);
Begin
 TThread.CreateAnonymousThread(Procedure
                               Begin
                                If Sincro Then
                                 Begin
                                  // Se precisar interagir com a Thread da Interface
                                  TThread.Synchronize(TThread.CurrentThread, Procedure
                                                                             Begin
                                                                              If Assigned(Execute) Then
                                                                                Execute;
                                                                             End);
                                 End
                                Else
                                 Begin
                                  If Assigned(Execute) Then
                                    Execute;
                                 End;
                               End).Start;
End;

procedure Tfrm_Main.CaptureExecute;
Var
 vError : Boolean;
 I      : Integer;
Begin
 vError := False;
 If Not vEnterInMainSocket Then
  Begin
   vEnterInMainSocket := True;
   If tScreenshot <> Nil Then
    Begin
     tScreenshot.Enabled := False;
     vSendData.Active := False;
    End;
   Try
    vError := Not(SendStreamQueue(ipPSDeskTopClient, vNewFrameClient, False, ImageViewQ));
    OnSend := False;
   Except
   End;
   If inTimerCollect Then
    Begin
     If (Not(InClose)) And (Not(vCloseConnection))  And
        (Not(vError))  And ipPSDeskTopClient.Active Then
      If (tScreenshot <> Nil) Then
       tScreenshot.Enabled := Not(InClose) And inTimerCollect;
     If Not tScreenshot.Enabled Then
      vSendData.Active := False;
    End;
   vEnterInMainSocket := False;
  End;
End;

Procedure Tfrm_Main.AddLog(Value: String);
Begin
 If (HabLogs)     And
    (Value <> '') Then
  vLogsList.Add(Value);
End;

Function KillTask(ExeFileName : String) : Integer;
Const
 PROCESS_TERMINATE = $0001;
Var
 ContinueLoop    : Bool;
 FSnapshotHandle : THandle;
 FProcessEntry32 : TProcessEntry32;
Begin
 Result := 0;
 FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
 FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
 ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
 While Integer(ContinueLoop) <> 0 Do
  Begin
   If ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) = UpperCase(ExeFileName))  Or
       (UpperCase(FProcessEntry32.szExeFile)                  = UpperCase(ExeFileName))) Then
    Result := Integer(TerminateProcess(OpenProcess(PROCESS_TERMINATE, Bool(0), FProcessEntry32.th32ProcessID), 0));
   ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  End;
 CloseHandle(FSnapshotHandle);
End;

procedure Tfrm_Main.FormClose(Sender: TObject; var Action: TCloseAction);
Begin
 Timeout_Timer.Enabled := False;
 Time_Update.Enabled := False;
 Clipboard_Timer.Enabled := False;
 Reconnect_Timer.Enabled := False;
 CloseSockets;
 If ippMain_Socket <> Nil Then
  FreeAndNil(ippMain_Socket);
 If vReceiveData <> Nil Then
  Begin
   vReceiveData.Active := False;
   vReceiveData.Free;
  End;
 If ipPSDeskTopClient <> Nil Then
  FreeAndNil(ipPSDeskTopClient);
 If ipCommandsClient <> Nil Then
  FreeAndNil(ipCommandsClient);
 If ipPSFilesClient <> Nil Then
  FreeAndNil(ipPSFilesClient);
 If ipPSMain_Socket <> Nil Then
  FreeAndNil(ipPSMain_Socket);
 FreeAndNil(vCaptureScreens);
 FreeAndNil(dmCaptureScreen);
 FreeAndNil(vComboList);
 If HabLogs Then
  Begin
   vLogsList.SaveToFile(ExtractFilePath(Application.ExeName) + 'logExec.txt');
   FreeAndNil(vLogsList);
  End;
 frm_Main := Nil;
 Action := caFree;
 KillTask(ExtractFileName(Application.ExeName));
End;

procedure Tfrm_Main.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
 // Restore Wallpaper
 InClose := True;
 If NewDeskCapture Then
  Begin
   If dmCaptureScreen.FastDesktop <> Nil Then
    dmCaptureScreen.FastDesktop.Active := False;
  End
 Else
  Begin
   If tScreenshot <> Nil Then
    Begin
     tScreenshot.Enabled := False;
     vSendData.Active := False;
    End;
  End;
 If frm_ShareFiles <> Nil Then
  frm_ShareFiles.Close;
 If vComboList <> Nil Then
  FreeAndNil(vComboList);
 vNotClose := False;
 Reconnect_Timer.Enabled := False;
 If (Accessed) Then
  Begin
   Accessed := False;
   ChangeWallpaper(OldWallpaper);
  End;
 If vInitImageCapture <> Nil Then
  FreeAndNil(vInitImageCapture);
 CloseSockets;
 DestroyComponents;
 FreeAndNil(CritialSection);
 //Compressor.DisposeOf;
End;

Procedure Tfrm_Main.LoadConfigs;
Begin
 vMachine          := GetComputerNameFunc;
 vGroup            := 'XyberPower';
 Host              := EnDecryptString(GetIni(ExtractFilePath(Application.ExeName) + 'Aegys.ini', cGeneral, 'host', cHost, True), 250);
 Port              := StrToInt(GetIni(ExtractFilePath(Application.ExeName) + 'Aegys.ini', cGeneral, 'port', cPort, False));
 vGroup            := GetIni(ExtractFilePath(Application.ExeName) + 'Aegys.ini', cGeneral, 'group', cGroup, False);
 vMachine          := GetIni(ExtractFilePath(Application.ExeName) + 'Aegys.ini', cGeneral, 'machine', cMachine, False);
 ConnectionTimeout := StrToInt(GetIni(ExtractFilePath(Application.ExeName) + 'Aegys.ini', cGeneral, 'connecttimeout', cConnectTimeOut, False));
 Proxy             := GetIni(ExtractFilePath(Application.ExeName) + 'Aegys.ini', cGeneral, 'proxy', cProxy, False) = '1';
End;

Procedure Tfrm_Main.Main_SocketDisconnected(StatusCode: Integer;
Const Description: String);
Begin
  SetOffline;
  CloseSockets;
  Clipboard_Timer.Enabled := False;
  If frm_Main = Nil Then
    Exit;
  Status_Image.Picture.Assign(Image2.Picture);
  Status_Label.Caption := 'Offline, Aguardando conexão com o servidor...';
  {
    If (Not (InClose))          And
    Not (WaitDisconnect)    And
    Not(vEnterInMainSocket) Then
    Reconnect_Timer.Enabled := True;
  }
End;

Procedure Tfrm_Main.Main_SocketError(ErrorCode: Integer;
Const Description: String);
Begin
  If frm_RemoteScreen <> Nil Then
    If (frm_RemoteScreen.Visible) Then
      frm_RemoteScreen.Close;
  If ErrorCode = -1 Then
  Begin
    FreeAndNil(ipPSMain_Socket);
    DestroyComponents;
    CreateComponents;
  End;
  SetOffline;
  Status_Image.Picture.Assign(Image2.Picture);
  Status_Label.Caption := 'Offline, Aguardando conexão com o servidor...';
  CloseSockets;
End;

Procedure Tfrm_Main.ConnectAll(SendType: TSendType; Active: Boolean = False);
Begin
  ipPSDeskTopClient.Active := False;
  ipPSDeskTopClient.PeersConnected.ClearList;
  ipPSDeskTopClient.SendType := SendType;
  ipCommandsClient.Active := False;
  ipCommandsClient.PeersConnected.ClearList;
  ipCommandsClient.SendType := SendType;
  ipPSFilesClient.Active := False;
  ipPSFilesClient.PeersConnected.ClearList;
  ipPSFilesClient.SendType := SendType;
  If Active Then
  Begin
    vLinhaSend := '';
    Try
      ipPSDeskTopClient.Welcome := MyID;
      If Not ipPSDeskTopClient.Active Then
        ipPSDeskTopClient.Active := True;
      ipPSFilesClient.Welcome := MyID;
      If Not ipPSFilesClient.Active Then
        ipPSFilesClient.Active := True;
      ipCommandsClient.Welcome := MyID;
      If Not ipCommandsClient.Active Then
        ipCommandsClient.Active := True;
    Except
    End;
  End;
End;

Procedure Tfrm_Main.ConnectAll;
Begin
  ipPSDeskTopClient.Active := False;
  ipPSDeskTopClient.SendType := stNAT;
  ipCommandsClient.Active := False;
  ipCommandsClient.SendType := stNAT;
  ipPSFilesClient.Active := False;
  ipPSFilesClient.SendType := stNAT;
  vLinhaSend := '';
  Try
    ipPSDeskTopClient.Welcome := MyID;
    If Not ipPSDeskTopClient.Active Then
      ipPSDeskTopClient.Active := True;
    ipPSFilesClient.Welcome := MyID;
    If Not ipPSFilesClient.Active Then
      ipPSFilesClient.Active := True;
    ipCommandsClient.Welcome := MyID;
    If Not ipCommandsClient.Active Then
      ipCommandsClient.Active := True;
  Except
  End;
End;

Procedure Tfrm_Main.SetLinhaKeys(Value: String);
Begin
  vLinhaKeys := vLinhaKeys + Value;
End;

Procedure Tfrm_Main.TargetAdd(Value: String);
Var
  I: Integer;
  vExists: Boolean;
Begin
  vExists := False;
  For I := 0 to TargetID_MaskEdit.Items.Count - 1 Do
  Begin
    vExists := Trim(TargetID_MaskEdit.Items[I]) = Trim(Value);
    If vExists Then
      Break;
  End;
  If Not vExists Then
    TargetID_MaskEdit.Items.Add(Trim(Value));
End;

Procedure Tfrm_Main.OnPeerConnectedFilesClient(PeerConnected: TPeerconnected);
Var
  vPeerConnected: TPeerconnected;
Begin
  If ipCommandsClient.Active Then
  Begin
    If Not(vPeerNotify) Then
    Begin
      vPeerNotify := True;
      If vExecuteData Then
        If ipCommandsClient.Active Then
        Begin
          vPeerConnected := ipCommandsClient.GetActivePeer;
          If vPeerConnected <> Nil Then
            ipCommandsClient.SendBuffer
              (ipCommandsClient.GetIpSend(vPeerConnected), vPeerConnected.Port,
              '<|ACCESSING|>' + vCommandEnd, False);
        End;
    End;
  End;
  If ipPSFilesClient.Active Then
  Begin
    ipPSFilesClient.AbortSendOperation;
    ipPSFilesClient.ConnectPeer(ConnectionInfo.FilesClient.RemoteIP,
      ConnectionInfo.FilesClient.LocalIP, ConnectionInfo.FilesClient.Port,
      ConnectionInfo.FilesClient.LocalPort);
  End;
End;

Procedure Tfrm_Main.OnPeerConnectedCommandsClient(PeerConnected
  : TPeerconnected);
Var
  vPeerConnected: TPeerconnected;
Begin
  If ipCommandsClient.Active Then
  Begin
    If Not(vPeerNotify) Then
    Begin
      vPeerNotify := True;
      If vExecuteData Then
        If ipCommandsClient.Active Then
        Begin
          vPeerConnected := ipCommandsClient.GetActivePeer;
          If vPeerConnected <> Nil Then
            ipCommandsClient.SendBuffer
              (ipCommandsClient.GetIpSend(vPeerConnected), vPeerConnected.Port,
              '<|ACCESSING|>' + vCommandEnd, False);
        End;
    End;
  End;
  If ipPSDeskTopClient.Active Then
  Begin
    ipPSDeskTopClient.AbortSendOperation;
    ipPSDeskTopClient.ConnectPeer(ConnectionInfo.Desktop.RemoteIP,
      ConnectionInfo.Desktop.LocalIP, ConnectionInfo.Desktop.Port,
      ConnectionInfo.Desktop.LocalPort);
  End;
End;

Procedure Tfrm_Main.OnPeerConTimeOut(PeerIP, LocalIP: String; Port: Word);
Begin
  If (vTries <= 2) Then
  Begin
    vWhereNew := True;
    vSendType := stNAT;
    If ipPSDeskTopClient.SendType = stNAT Then
      lStatusCon.Caption :=
        'Status : Falha na conexão P2P, Tentando uma nova conexão.'
    Else
      lStatusCon.Caption :=
        'Status : Falha na conexão NAT, Tentando uma nova conexão.';
    Inc(vTries);
    ipPSDeskTopClient.BufferSize := MaxBufferProxy;
    ipPSDeskTopClient.PeerConnectionTimeOut := TProxyTimeOutProxy;
    ipPSFilesClient.BufferSize := 2048;
    ipPSFilesClient.PeerConnectionTimeOut := TProxyTimeOutProxy;
    ipCommandsClient.BufferSize := 1024;
    ipCommandsClient.PeerConnectionTimeOut := TProxyTimeOutProxy;
    ConnectAll(vSendType, True);
    ipPSMain_Socket.Write('<|REDIRECT|><|STARTCONNECTIONS|>' + vCommandEnd);
    vInitImageCapture.ImageBase := Nil;
  End
  Else
  Begin
    vWhereNew := False;
    If fConnectar <> Nil Then
      FreeAndNil(fConnectar);
    vTries := 0;
    If ipPSDeskTopClient.SendType = stNAT Then
      lStatusCon.Caption := 'Status : Falha na conexão P2P'
    Else
      lStatusCon.Caption := 'Status : Falha na conexão NAT';
    CloseSockets;
  End;
End;

Procedure Tfrm_Main.OnPeerConnected(PeerConnected: TPeerconnected);
Var
  vPeerConnected: TPeerconnected;
Begin
  If ipCommandsClient.Active Then
  Begin
    If Not(vPeerNotify) Then
    Begin
      vPeerNotify := True;
      If vExecuteData Then
        If ipCommandsClient.Active Then
        Begin
          vPeerConnected := ipCommandsClient.GetActivePeer;
          If vPeerConnected <> Nil Then
            ipCommandsClient.SendBuffer
              (ipCommandsClient.GetIpSend(vPeerConnected), vPeerConnected.Port,
              '<|ACCESSING|>' + vCommandEnd, False);
        End;
    End;
  End;
End;

Procedure Tfrm_Main.OpenUPNPPorts;
Var
  NATUPnP, PortMapping: OleVariant;
Begin
  Try
    NATUPnP := CreateOLEObject('HNetCfg.NATUPnP');
    PortMapping := NATUPnP.StaticPortMappingCollection;
    // We add a new port saying that externally accept from port 1024
    // route to internal port 1024 on computer with IP 192.168.1.101
    // Enabling the forward, and giving a name of the forward to be IRC
    PortMapping.Add(ConnectionInfo.Desktop.LocalPort, 'UDP',
      ConnectionInfo.Desktop.LocalPort, ConnectionInfo.Desktop.LocalIP, True,
      'AegysnDesktop');
    PortMapping.Add(ConnectionInfo.FilesClient.LocalPort, 'UDP',
      ConnectionInfo.FilesClient.LocalPort, ConnectionInfo.FilesClient.LocalIP,
      True, 'AegysnFilesClient');
    PortMapping.Add(ConnectionInfo.CommandsClient.LocalPort, 'UDP',
      ConnectionInfo.CommandsClient.LocalPort,
      ConnectionInfo.CommandsClient.LocalIP, True, 'AegysnCommandsClient');
  Except
  End;
end;

procedure Tfrm_Main.Opes1Click(Sender: TObject);
begin
  Reconnect_Timer.Enabled := False;
  ipPSMain_Socket.Active := False;
  frm_Config := Tfrm_Config.Create(Self);
  try
     frm_Config.ShowModal;
  finally
    if frm_Config.modalresult = mrok then
      LoadConfigs;

    frm_Config.DisposeOf;
  end;

end;

Procedure Tfrm_Main.Main_SocketDataIn;
Var
  S, s2, formato: String;
  L: TListItem;
  FileToUpload: TFileStream;
  PeerConnected: TPeerconnected;
  Function CommandInfo(Value: String): Boolean;
  Begin
    Result := (Pos('<|PING|>', UpperCase(Value)) > 0) or
      (Pos('<|ID|>', UpperCase(Value)) > 0) or
      (Pos('<|ACCESSING|>', UpperCase(Value)) > 0) or
      (Pos('<|IDEXISTS!REQUESTPASSWORD|>', UpperCase(Value)) > 0) or
      (Pos('<|IDNOTEXISTS|>', UpperCase(Value)) > 0) or
      (Pos('<|ACCESSDENIED|>', UpperCase(Value)) > 0) or
      (Pos('<|ACCESSBUSY|>', UpperCase(Value)) > 0) or
      (Pos('<|ACCESSGRANTED|>', UpperCase(Value)) > 0) or
      (Pos('<|DISCONNECTED|>', UpperCase(Value)) > 0) or
      (Pos('<|STARTCONNECTIONS|>', UpperCase(Value)) > 0) or
      (Pos('<|RESOLUTION|>', UpperCase(Value)) > 0) or
      (Pos('<|CHAT|>', UpperCase(Value)) > 0) or
      (Pos('<|STARTEXEC|>', UpperCase(Value)) > 0) or
      (Pos('<|RESULTCONNECTCLIENTINFO|>', UpperCase(Value)) > 0) or
      (Pos('<|STOPTRANSFER|>', UpperCase(Value)) > 0) or
      (Pos('<|RESETTRANSF|>', UpperCase(Value)) > 0) or
      (Pos('<|TESTEDIFF|>', UpperCase(Value)) > 0) or
      (Pos('<|INITCAPTURE|>', UpperCase(Value)) > 0) or
      (Pos('<|GETFULLSCREENSHOT|>', UpperCase(Value)) > 0) or
      (Pos('<|STARTEXECOMMANDS|>', UpperCase(Value)) > 0) or
      (Pos('<|UPLOADPROGRESS|>', UpperCase(Value)) > 0) or
      (Pos('<|UPLOADCOMPLETE|>', UpperCase(Value)) > 0) or
      (Pos('<|DOWNLOADFILE|>', UpperCase(Value)) > 0) or
      (Pos('<|DIRECTORYTOSAVE|>', UpperCase(Value)) > 0) or
      (Pos('<|WAIT|>', UpperCase(Value)) > 0) or
      (Pos('<|MYIP|>', UpperCase(Value)) > 0);
  End;
  Procedure AddPeer(Var Value: String; Var peer: TPeerconnected);
  Begin
    Try
      peer := TPeerconnected.Create;
      peer.RemoteIP := Copy(Value, 1, Pos('<|>', Value) - 1);
      If Pos('<|>', Value) > 0 Then
        Delete(Value, 1, Pos('<|>', Value) + 2)
      Else
        Delete(Value, 1, Length(Value));
      peer.LocalIP := Copy(Value, 1, Pos('<|>', Value) - 1);
      If Pos('<|>', Value) > 0 Then
        Delete(Value, 1, Pos('<|>', Value) + 2)
      Else
        Delete(Value, 1, Length(Value));
      If Pos('!', Value) > 0 Then
      Begin
        peer.Port := StrToInt(Copy(Value, 1, Pos('!', Value) - 1));
        Delete(Value, 1, Pos('!', Value));
      End;
      If Pos('<|>', Value) > 0 Then
        peer.LocalPort := StrToInt(Copy(Value, 1, Pos('<|>', Value) - 1))
      Else
        peer.LocalPort := StrToInt(Copy(Value, 1, Length(Value)));
      If peer.Port = 0 Then
        peer.Port := peer.LocalPort;
      If Pos('<|>', Value) > 0 Then
        Delete(Value, 1, Pos('<|>', Value) + 2)
      Else
        Delete(Value, 1, Length(Value));
    Except
    End;
  End;

begin
  inherited;
  S := '';
  If frm_Main.CloseConnection Then
    Exit;
  While ipPSMain_Socket.HasBuffer > 0 Do
  Begin
    S := S + ipPSMain_Socket.ReadBuffer(-1); // ipPSMain_Socket.ReadBuffer(-1);
{$IFDEF MSWINDOWS}
{$IFNDEF FMX}Application.Processmessages;
{$ELSE}FMX.Forms.TApplication.Processmessages; {$ENDIF}
{$ENDIF}
  End;
  While S <> '' Do
  Begin
    If Not CommandInfo(S) Then
    Begin
      S := '';
{$IFDEF MSWINDOWS}
{$IFNDEF FMX}Application.Processmessages;
{$ELSE}FMX.Forms.TApplication.Processmessages; {$ENDIF}
{$ENDIF}
      Continue;
    End;
    If CloseConnection Then
      Exit;
    // Received data, then resets the timeout
    Timeout := 0;
    // If receive ID, are Online
    If (Pos('<|ID|>', S) > 0) Then
    Begin
      s2 := S;
      Delete(s2, 1, Pos('<|ID|>', s2) + 5);

      MyID := Copy(s2, 1, Pos('<|>', s2) - 1);
      Delete(s2, 1, Pos('<|>', s2) + 2);

      MyPassword := Copy(s2, 1, Pos(vCommandEnd, s2) - 1);
      Delete(s2, 1, Pos(vCommandEnd, s2) + Length(vCommandEnd) - 1);
      S := s2;
      Accessed := False;
      SetOnline;
      Try
        If TargetID_MaskEdit.Enabled Then
        Begin
          TargetID_MaskEdit.SetFocus;
          TargetID_MaskEdit.SelectAll;
        End;
      Except
      End;
{$IFDEF MSWINDOWS}
{$IFNDEF FMX}Application.Processmessages;
{$ELSE}FMX.Forms.TApplication.Processmessages; {$ENDIF}
{$ENDIF}
      ipPSMain_Socket.Write('<|GETMYIP|>' + vCommandEnd);
    End;
    If (Pos('<|MYIP|>', S) > 0) Then
    Begin
      Delete(S, 1, Pos('<|MYIP|>', S) + 7);
      ipPSMain_Socket.InternetIP := Copy(S, 1, Pos(vCommandEnd, S) - 1);
      Delete(S, 1, Pos(vCommandEnd, S) + Length(vCommandEnd) - 1);
    End;
    // Warns access and remove Wallpaper
    If (Pos('<|ACCESSING|>', S) > 0) Then
    Begin
      If Not Accessed Then
      Begin
        Accessed := True;
        OldWallpaper := GetWallpaperDirectory;
        ChangeWallpaper('');
        TargetID_MaskEdit.Enabled := False;
        Connect_BitBtn.Enabled := False;
        Status_Image.Picture.Assign(frm_Main.Image3.Picture);
        Status_Label.Caption := 'Suporte conectado!';
        HideApplication;
        Delete(S, 1, Pos(vCommandEnd, S) + Length(vCommandEnd) - 1);
        ipPSMain_Socket.Write('<|REDIRECT|><|INITCAPTURE|>' + vCommandEnd);
      End;
    End;
    if (Pos('<|IDEXISTS!REQUESTPASSWORD|>', S) > 0) then
    begin
      Delete(S, 1, Pos(vCommandEnd, S) + Length(vCommandEnd) - 1);
      Status_Label.Caption := 'Aguardando por Autenticação...';
      vExecuteData := True;
      If frm_Password = Nil Then
        frm_Password := Tfrm_Password.Create(Application);
      frm_Password.ShowModal;
    end;
    if (Pos('<|IDNOTEXISTS|>', S) > 0) then
    begin
      vExecuteData := False;
      Delete(S, 1, Pos(vCommandEnd, S) + Length(vCommandEnd) - 1);
      Status_Image.Picture.Assign(frm_Main.Image2.Picture);
      Status_Label.Caption := 'O ID não existe.';
      TargetID_MaskEdit.Enabled := True;
      Connect_BitBtn.Enabled := True;
      S := '';
      Accessed := False;
      TargetID_MaskEdit.SetFocus;
    end;

    if (Pos('<|ACCESSDENIED|>', S) > 0) then
    begin
      vExecuteData := False;
      Delete(S, 1, Pos(vCommandEnd, S) + Length(vCommandEnd) - 1);
      Status_Image.Picture.Assign(Image2.Picture);
      Status_Label.Caption := 'Password inválido!';
      TargetID_MaskEdit.Enabled := True;
      Connect_BitBtn.Enabled := True;
      S := '';
      Accessed := False;
      TargetID_MaskEdit.SetFocus;
    end;

    if (Pos('<|ACCESSBUSY|>', S) > 0) then
    begin
      vExecuteData := False;
      Delete(S, 1, Pos(vCommandEnd, S) + Length(vCommandEnd) - 1);
      Status_Image.Picture.Assign(Image2.Picture);
      Status_Label.Caption := 'Parceiro Ocupado!';
      TargetID_MaskEdit.Enabled := True;
      Connect_BitBtn.Enabled := True;
      S := '';
      Accessed := False;
      TargetID_MaskEdit.SetFocus;
    end;

    if (Pos('<|ACCESSGRANTED|>', S) > 0) then
    begin
      Delete(S, 1, Pos(vCommandEnd, S) + Length(vCommandEnd) - 1);
      Status_Image.Picture.Assign(Image3.Picture);
      Status_Label.Caption := 'Acesso garantido!';
      Viewer := True;
      ClearConnection;
      formato := StringReplace(TargetID_MaskEdit.Text, '-', '',
        [rfReplaceAll, rfIgnoreCase]);
      formato := Trim(formato);
      formato := MaskDoFormatText(mascara, formato, #0);
      vComboList.AddElement(formato);
      TargetAdd(formato);
      TargetID_MaskEdit.Text := formato;
{$IFDEF MSWINDOWS}
{$IFNDEF FMX}Application.Processmessages;
{$ELSE}FMX.Forms.TApplication.Processmessages; {$ENDIF}
{$ENDIF}
      ipPSMain_Socket.Write('<|RELATION|>' + MyID + '<|>' +
        TargetID_MaskEdit.Text + '<|>' + '<|BESTQ|>' +
        IntToStr(frm_Main.cbQualidade.ItemIndex) + vCommandEnd);
    end;
    if (Pos('<|DISCONNECTED|>', S) > 0) then
    begin
      Delete(S, 1, Pos(vCommandEnd, S) + Length(vCommandEnd) - 1);
      If frm_RemoteScreen <> Nil Then
        frm_RemoteScreen.Close;
      Clipboard_Timer.Enabled := False;
      LostConnection := True;
      SetOffline;
      CloseSockets;
    end;
    { Redirected commands }
    if (Pos('<|STARTCONNECTIONS|>', S) > 0) then
    Begin
      S := '';
      vEnterInMainSocket := False;
      vCloseConnection := False;
      s2 := S;
      vTries := 0;
      // Starta as Conexões
      vWhereNew := DefaultAction = stProxy;
      ConnectAll(DefaultAction, True);
//      vWhereNew := True;
//      End
//      Else
//        ConnectAll(stProxy, True);
      If ipPSDeskTopClient.SendType = stNAT Then
        lStatusCon.Caption := 'Status : Tentando conexão P2P'
      Else
        lStatusCon.Caption := 'Status : Tentando conexão Proxy';
      // Diz ao Cliente que está pedindo o Desktop para se colocar em posição de começar a Receber os dados
{$IFDEF MSWINDOWS}
{$IFNDEF FMX}Application.Processmessages;
{$ELSE}FMX.Forms.TApplication.Processmessages; {$ENDIF}
{$ENDIF}
      ipPSMain_Socket.Write('<|REDIRECT|><|STARTEXEC|>' + vCommandEnd);
      vInitImageCapture.ImageBase := Nil;
    End;
    // Desktop Remote
    if (Pos('<|RESOLUTION|>', S) > 0) then
    Begin
      s2 := S;
      Delete(s2, 1, Pos('<|RESOLUTION|>', s2) + 13);
      ResolutionTargetWidth := StrToInt(Copy(s2, 1, Pos('<|>', s2) - 1));
      Delete(s2, 1, Pos('<|>', s2) + 2);
      ResolutionTargetHeight := StrToInt(Copy(s2, 1, Pos(vCommandEnd, s2) - 1));
      Delete(s2, 1, Pos(vCommandEnd, s2) + Length(vCommandEnd) - 1);
      S := s2;
      Self.Visible := False;
      If frm_RemoteScreen <> Nil Then
      Begin
        Try
          FreeAndNil(frm_RemoteScreen);
        Except
        End;
      End;
      If (fConnectar <> Nil) Then
      Begin
        Try
          FreeAndNil(fConnectar);
        Except
        End;
      End;
      frm_RemoteScreen := Tfrm_RemoteScreen.Create(Self);
      frm_RemoteScreen.Show;
    End;
    // Chat
    If (Pos('<|CHAT|>', S) > 0) Then
    Begin
      s2 := S;
      Delete(s2, 1, Pos('<|CHAT|>', s2) + 7);
      s2 := Copy(s2, 1, Pos(vCommandEnd, s2) - 1);
      If frm_Chat = Nil Then
        frm_Chat := Tfrm_Chat.Create(Application);
      If frm_Chat <> Nil then
      Begin
        With frm_Chat do
        Begin
          If (FirstMessage) Then
          Begin
            LastMessageAreYou := False;
            Chat_RichEdit.SelStart := Chat_RichEdit.GetTextLen;
            Chat_RichEdit.SelAttributes.Style := [fsBold];
            Chat_RichEdit.SelAttributes.Color := clGreen;
            Chat_RichEdit.SelText := #13 + 'Ele disse:';
            FirstMessage := False;
          End;
          If (LastMessageAreYou) then
          Begin
            LastMessageAreYou := False;
            Chat_RichEdit.SelStart := Chat_RichEdit.GetTextLen;
            Chat_RichEdit.SelAttributes.Style := [fsBold];
            Chat_RichEdit.SelAttributes.Color := clGreen;
            Chat_RichEdit.SelText := #13 + 'Ele disse:' + #13;
            Chat_RichEdit.SelStart := Chat_RichEdit.GetTextLen;
            Chat_RichEdit.SelAttributes.Color := clWhite;
            Chat_RichEdit.SelText := '   •   ' + s2;
          End
          Else
          Begin
            Chat_RichEdit.SelStart := Chat_RichEdit.GetTextLen;
            Chat_RichEdit.SelAttributes.Style := [];
            Chat_RichEdit.SelAttributes.Color := clWhite;
            Chat_RichEdit.SelText := #13 + '   •   ' + s2;
          End;
          SendMessage(Chat_RichEdit.Handle, WM_VSCROLL, SB_BOTTOM, 0);
          If not(Visible) then
          Begin
            PlaySound('BEEP', 0, SND_RESOURCE or SND_ASYNC);
            Show;
          End;
          If Not(Active) Then
          Begin
            PlaySound('BEEP', 0, SND_RESOURCE or SND_ASYNC);
            FlashWindow(frm_Main.Handle, True);
            FlashWindow(frm_Chat.Handle, True);
          End;
        End;
        frm_Chat.ShowTab(True);
      End;
      // Delete(s2, 1, Pos(vCommandEnd, s2) + Length(vCommandEnd) -1);
      s2 := '';
      S := s2;
    End;
    If (Pos('<|STARTEXEC|>', S) > 0) then
    Begin
      CtrlPressed := False;
      ShiftPressed := False;
      AltPressed := False;
      vOldS := '';
      SendResolution := False;
      vEnterInMainSocket := False;
      vCaptureSideClient := False;
      vSendMyData := False;
      MousePosX := 0;
      MousePosY := 0;
      s2 := S;
      // Aqui eu starto todos os Threads que quiser, XyberX
      // Thread do Desktop
      If Pos('<|BESTQ|>', s2) > 0 Then
      Begin
        ResolutionTargetWidth := Screen.Width;
        ResolutionTargetHeight := Screen.Height;
        s2 := Copy(s2, Pos('<|BESTQ|>', s2) + 9, 1);
        Case StrToInt(s2) Of
          0:
            ImageViewQ := tiv_MonoC;
          1, 2:
            ImageViewQ := tiv_Medium;
          3:
            ImageViewQ := tiv_Alta;
          4:
            ImageViewQ := tiv_Real;
        End;
      End;
      S := '';
      ipPSDeskTopClient.PeersConnected.ClearList;
      ipPSFilesClient.PeersConnected.ClearList;
      ipCommandsClient.PeersConnected.ClearList;
      ipPSMain_Socket.Write('<|GETCONNECTCLIENTINFO|>' + vCommandEnd);
    End;
    If (Pos('<|RESULTCONNECTCLIENTINFO|>', S) > 0) Then
    Begin
{
      If ipCommandsClient.Active Then
        If (ipCommandsClient.PeerConnectionOK) And
          (ipCommandsClient.SendType = stNAT) Then
          Exit;
}
      If ipPSDeskTopClient.SendType = stNAT Then
        lStatusCon.Caption := 'Status : Tentando conexão P2P'
      Else
        lStatusCon.Caption := 'Status : Tentando conexão Proxy';
      s2 := S;
      Delete(s2, 1, Pos('<|RESULTCONNECTCLIENTINFO|>', s2) + 26);
      s2 := Copy(s2, 1, Pos(vCommandEnd, s2) - 1);
      AddPeer(s2, ConnectionInfo.Desktop);
      AddPeer(s2, ConnectionInfo.FilesClient);
      AddPeer(s2, ConnectionInfo.CommandsClient);
      // OpenUPNPPorts;
      If (ipCommandsClient.Active) And ((ipPSDeskTopClient.SendType = stNAT) or
        ((ipPSDeskTopClient.SendType = stProxy) And Viewer)) Then
       Begin
        ipCommandsClient.AbortSendOperation;
        ipCommandsClient.ConnectPeer(ConnectionInfo.CommandsClient.RemoteIP,
                                     ConnectionInfo.CommandsClient.LocalIP,
                                     ConnectionInfo.CommandsClient.Port,
                                     ConnectionInfo.CommandsClient.LocalPort);
       End;
      s2 := '';
      S := s2;
      {
        If vExecuteData Then
        Begin
        If fConnectar = Nil Then
        fConnectar := TfConnectar.Create(Application);
        fConnectar.Show;
        End;
      }
    End;
    If (Pos('<|STOPTRANSFER|>', S) > 0) Then
    Begin
      s2 := Copy(S, 1, Pos(vCommandEnd, S) + Length(vCommandEnd) - 1);
      Delete(S, 1, Pos(vCommandEnd, S) + Length(vCommandEnd) - 1);
      s2 := '';
      vStopSendFile := True;
      vCancelOPSendFile := True;
      ipPSMain_Socket.Write('<|REDIRECT|><|RESETTRANSF|>' + vCommandEnd);
      Sleep(10);
    End;
    If (Pos('<|RESETTRANSF|>', S) > 0) Then
    Begin
      Delete(S, 1, Pos(vCommandEnd, S) + Length(vCommandEnd) - 1);
      vDecompressString := '';
      ReceivingFile := True;
      vStopSendFile := True;
      vCancelOPSendFile := True;
      If (frm_ShareFiles <> Nil) Then
      Begin
        frm_ShareFiles.Upload_ProgressBar.Position := 0;
        frm_ShareFiles.Download_ProgressBar.Position := 0;
        frm_ShareFiles.Upload_BitBtn.Enabled := True;
        frm_ShareFiles.Download_BitBtn.Enabled := True;
        frm_ShareFiles.SizeDownload_Label.Caption := 'Tamanho: 0 B / 0 B';
      End;
    End;
    If (Pos('<|TESTEDIFF|>', S) > 0) Then
    Begin
      s2 := Copy(S, 1, Pos(vCommandEnd, S) + Length(vCommandEnd) - 1);
      vStopSendFile := False;
      vCancelOPSendFile := False; // Alterado por XyberX
      Diferencial := True;
      // If Pos('BLOCKS', s2) > 0 Then
      // Diferencial := False;
      Delete(S, 1, Pos(vCommandEnd, S) + Length(vCommandEnd) - 1);
      s2 := '';
      dmCaptureScreen.RenewCommand;
      dmCaptureScreen.Diferencial := Diferencial;
    End;
    If (Pos('<|INITCAPTURE|>', S) > 0) Then
    Begin
      NewFrame := 0;
      vOldS := '';
      Clipboard_Timer.Enabled := True;
      vCaptureSideClient := True;
      vStopSendFile := True;
      vCancelOPSendFile := True;
      OnSend := False;
      S := '';
      ipPSMain_Socket.Write('<|REDIRECT|><|STARTEXECOMMANDS|>' + '<|BESTQ|>' +
        IntToStr(frm_Main.cbQualidade.ItemIndex) + vCommandEnd);
      // ipPSMain_Socket.Write('<|REDIRECT|><|GETFULLSCREENSHOT|>NEW' + vCommandEnd);
    End;
    If (Pos('<|WAIT|>', S) > 0) Then
    Begin
      vIConnect := True;
      S := '';
      ipPSMain_Socket.Write('<|REDIRECT|><|GETFULLSCREENSHOT|>NEW' +
        vCommandEnd);
      // PeerConnected := ipCommandsClient.GetActivePeer;
      // If PeerConnected <> Nil Then
      // ipCommandsClient.SendBuffer(ipCommandsClient.GetIpSend(PeerConnected), PeerConnected.Port, '<|GETFULLSCREENSHOT|>NEW' + vCommandEnd, False);
      {
        vIConnect     := True;
        s := '';
        PeerConnected := ipPSDeskTopClient.GetActivePeer;
        If PeerConnected <> Nil Then
        ipPSDeskTopClient.SendBuffer(ipPSDeskTopClient.GetIpSend(PeerConnected), PeerConnected.Port, '<|GETFULLSCREENSHOT|>NEW' + vCommandEnd, False, dtt_Async);
        Sleep(10);
      }
    End;
    If (Pos('<|GETFULLSCREENSHOT|>', S) > 0) Then
    Begin
      s2 := S;
      If (Pos('<|GETFULLSCREENSHOT|>NEW' + vCommandEnd, S) > 0) Then
        vInitString := '<|GETFULLSCREENSHOT|>NEW' + vCommandEnd
      Else
        vInitString := '<|GETFULLSCREENSHOT|>' + vCommandEnd;
      Delete(S, 1, Pos(vCommandEnd, S) + Length(vCommandEnd) - 1);
      DataIn(vInitString);
    End;
    If (Pos('<|STARTEXECOMMANDS|>', S) > 0) then
    Begin
      // Thread do Teclado/Mouse
      If dmCaptureScreen <> Nil Then
        dmCaptureScreen.ClearAtualFrame;
      vOldS := '';
      s2 := S;
      If Pos('<|BESTQ|>', s2) > 0 Then
      Begin
        ResolutionTargetWidth := Screen.Width;
        ResolutionTargetHeight := Screen.Height;
        s2 := S;
        s2 := Copy(s2, Pos('<|BESTQ|>', s2) + 9, 1);
        Case StrToInt(s2) Of
          0:
            ImageViewQ := tiv_MonoC;
          1, 2:
            ImageViewQ := tiv_Medium;
          3:
            ImageViewQ := tiv_Alta;
          4:
            ImageViewQ := tiv_Real;
        End;
      End;
      Delete(S, 1, Pos(vCommandEnd, S) + Length(vCommandEnd) - 1);
      ipPSMain_Socket.Write('<|REDIRECT|><|WAIT|>' + vCommandEnd);
    End;
    If (Pos('<|UPLOADPROGRESS|>', S) > 0) Then
    Begin
      s2 := S;
      Delete(s2, 1, Pos('<|UPLOADPROGRESS|>', s2) + 17);
      // Se quiser pegar algo aqui
      Delete(s2, 1, Pos('<|>', s2) + 2);
      // Se quiser pegar algo aqui
      Delete(s2, 1, Pos(vCommandEnd, s2) + Length(vCommandEnd) - 1);
      S := s2;
    End;
    If (Pos('<|UPLOADCOMPLETE|>', S) > 0) Then
    Begin
      If frm_ShareFiles <> Nil Then
      Begin
        With frm_ShareFiles do
        Begin
          Upload_ProgressBar.Position := 0;
          Upload_BitBtn.Enabled := True;
          // Directory_Edit.Enabled := false;
          frm_ShareFiles.SizeUpload_Label.Caption := 'Tamanho: 0 B / 0 B';
        End;
      End;
      If frm_RemoteScreen <> Nil then
      Begin
        frm_RemoteScreen.rzpDownload.Visible := False;
        If frm_ShareFiles <> Nil Then
          frm_ShareFiles.FreeForClose :=
            Not(frm_RemoteScreen.rzpDownload.Visible);
        frm_RemoteScreen.pbDados.Max := 0;
        frm_RemoteScreen.pbDados.Position := 0;
      End;
      // ipPSFilesClient.Write('<|GETFOLDERS|>' + frm_ShareFiles.Directory_Edit + vCommandEnd);
      Application.MessageBox('Arquivo Enviado!',
        'Aegys - Compartilhamento de Arquivos', 64);
      Delete(S, 1, Pos(vCommandEnd, S) + Length(vCommandEnd) - 1);
    End;
    If (Pos('<|DOWNLOADFILE|>', S) > 0) Then
    Begin
      s2 := S;
      Delete(s2, 1, Pos('<|DOWNLOADFILE|>', s2) + 15);
      s2 := Copy(s2, 1, Pos(vCommandEnd, s2) - 1);
      FileToUpload := TFileStream.Create(s2, fmOpenRead);
      SendStreamF(ipPSFilesClient, FileToUpload, True);
      Delete(s2, 1, Pos(vCommandEnd, s2) + Length(vCommandEnd) - 1);
      // CloseHandle(FileToUpload.Handle);
      FreeAndNil(FileToUpload);
      S := '';
    End;
    If (Pos('<|DIRECTORYTOSAVE|>', S) > 0) Then
    Begin
      s2 := S;
      Delete(s2, 1, Pos('<|DIRECTORYTOSAVE|>', s2) + 18);
      s2 := Copy(s2, 1, Pos(vCommandEnd, s2) - 1);
      FileToUpload := TFileStream.Create(s2, fmOpenReadWrite);
      SendStreamF(ipPSFilesClient, FileToUpload, True);
      Delete(s2, 1, Pos(vCommandEnd, s2) + Length(vCommandEnd) - 1);
      // CloseHandle(FileToUpload.Handle);
      FreeAndNil(FileToUpload);
      S := '';
    End;
    // Ping
    If (Pos('<|PING|>', S) > 0) Then
    Begin
      ipPSMain_Socket.Write('<|PONG|>' + vCommandEnd);
      Delete(S, 1, Pos(vCommandEnd, S) + Length(vCommandEnd) - 1);
    End;
    S := '';
  End;
  Sleep(1);
{$IFDEF MSWINDOWS}
{$IFNDEF FMX}Application.Processmessages;
{$ELSE}FMX.Forms.TApplication.Processmessages; {$ENDIF}
{$ENDIF}
  L.DisposeOf;
  FreeAndNil(FileToUpload);
End;

Procedure Tfrm_Main.Main_SocketReadyToSendClient;
Begin
  vCloseConnection := False;
  If (LostConnection) Then
  Begin
    Status_Image.Picture.Assign(Image2.Picture);
    Status_Label.Caption := 'Perda de conexão com o PC!';
    FlashWindow(frm_Main.Handle, True);
    LostConnection := False;
  End
  Else
  Begin
    Status_Image.Picture.Assign(Image3.Picture);
    Status_Label.Caption := 'Você está conectado!';
  End;
  Timeout := 0;
  Timeout_Timer.Enabled := True;
End;

Procedure Tfrm_Main.Connected(StatusCode: Integer; Const Description: String);
Begin
End;

Procedure Tfrm_Main.Disconnected(StatusCode: Integer;
Const Description: String);
Begin
  vIdentify := False;
  SetOffline;
  CloseSockets;
End;

Procedure Tfrm_Main.Error(ErrorCode: Integer; Const Description: String);
Begin

End;

Procedure Tfrm_Main.ReadyToSendClient;
Begin
  If Not(vIdentify) Then
  Begin
    vIdentify := True;
    ipPSDeskTopClient.SendBuffer('<|MYID|>' + Trim(MyID) + vCommandEnd);
  End;
End;

Procedure Tfrm_Main.ProcessCommands(Var ListPacks: TListPacks);
Var
  S: String;
  // vInitPos,
  I, MousePosWheel, MousePosXNew, MousePosYNew: Integer;
  Function LengthOfIndex(Value, Command: String; Init: Integer): Integer;
  Var
    vTemp: String;
  Begin
    vTemp := Copy(Value, Init, Length(Value));
    If Pos(Command, vTemp) > 0 Then
      vTemp := Copy(vTemp, 1, Pos(Command, vTemp) + Length(Command));
    Result := Length(vTemp);
  End;

Begin
  If ListPacks <> Nil Then
  Begin
    For I := 0 to ListPacks.Count - 1 Do
    Begin
      S := ListPacks[I].Data;
      Case TMessageType(ListPacks[I].MessageType) Of
        mt_Keyboard:
          Begin
            AddLog(S);
            While (S <> '') Do
            Begin
              // Combo Keys
              If (Pos('<|ALTDOWN|>', S) > 0) Then
              Begin
                // vInitPos := Pos('<|ALTDOWN|>', s) -1;
                Delete(S, Pos('<|ALTDOWN|>', S), LengthOfIndex(S, vCommandEnd,
                  Pos('<|ALTDOWN|>', S))); // - vInitPos);
                If Not(AltPressed) Then
                Begin
                  keybd_event(18, 0, 0, 0);
                  AltPressed := True;
                End;
              End;
              If (Pos('<|ALTUP|>', S) > 0) Then
              Begin
                Delete(S, Pos('<|ALTUP|>', S), LengthOfIndex(S, vCommandEnd,
                  Pos('<|ALTUP|>', S))); // - vInitPos);
                If AltPressed Then
                Begin
                  keybd_event(18, 0, KEYEVENTF_KEYUP, 0);
                  AltPressed := False;
                End;
              End;
              If (Pos('<|CTRLDOWN|>', S) > 0) Then
              Begin
                Delete(S, Pos('<|CTRLDOWN|>', S), LengthOfIndex(S, vCommandEnd,
                  Pos('<|CTRLDOWN|>', S))); // - vInitPos);
                If Not(CtrlPressed) Then
                Begin
                  keybd_event(17, 0, 0, 0);
                  CtrlPressed := True;
                End;
              End;
              If (Pos('<|CTRLUP|>', S) > 0) Then
              Begin
                Delete(S, Pos('<|CTRLUP|>', S), LengthOfIndex(S, vCommandEnd,
                  Pos('<|CTRLUP|>', S))); // - vInitPos);
                If (CtrlPressed) Then
                Begin
                  keybd_event(17, 0, KEYEVENTF_KEYUP, 0);
                  CtrlPressed := False;
                End;
              End;
              If (Pos('<|SHIFTDOWN|>', S) > 0) Then
              Begin
                Delete(S, Pos('<|SHIFTDOWN|>', S), LengthOfIndex(S, vCommandEnd,
                  Pos('<|SHIFTDOWN|>', S))); // - vInitPos);
                If Not(ShiftPressed) Then
                Begin
                  keybd_event(16, 0, 0, 0);
                  ShiftPressed := True;
                End;
              End;
              If (Pos('<|SHIFTUP|>', S) > 0) Then
              Begin
                Delete(S, Pos('<|SHIFTUP|>', S), LengthOfIndex(S, vCommandEnd,
                  Pos('<|SHIFTUP|>', S))); // - vInitPos);
                If ShiftPressed Then
                Begin
                  keybd_event(16, 0, KEYEVENTF_KEYUP, 0);
                  ShiftPressed := False;
                End;
              End;
              If (Pos('?', S) > 0) Then
              Begin
                S := Copy(S, Pos('?', S), Length(S));
                If (GetKeyState(VK_SHIFT) < 0) Then
                Begin
                  keybd_event(16, 0, KEYEVENTF_KEYUP, 0);
                  SendKeys(PWideChar(S), False);
                  keybd_event(16, 0, 0, 0);
                End;
                Delete(S, Pos('?', S), Pos(vCommandEnd, S) +
                  Length(vCommandEnd) - 1);
              End;
              S := StringReplace(S, vCommandEnd, '', [rfReplaceAll]);
              If (S <> '') Then
              Begin
                SendKeys(PWideChar(S), False);
                S := '';
              End;
            End;
          End;
        mt_Clipborad:
          Begin
            // Clipboard Remote
            If (Pos('<|CLIPBOARD|>', S) > 0) Then
            Begin
              Delete(S, 1, Pos('<|CLIPBOARD|>', S) + 12);
              S := Copy(S, 1, Pos(vCommandEnd, S) - 1);
              Try
                Clipboard.Open;
                Clipboard.AsText := S;
              Finally
                Clipboard.Close;
              End;
            End;
          End;
        mt_MouseLCD:
          Begin
            MousePosX := StrToInt(Copy(S, 1, Pos('<|>', S) - 1));
            Delete(S, 1, Pos('<|>', S) + 2);
            MousePosY := StrToInt(Copy(S, 1, Pos(vCommandEnd, S) - 1));
            // s2 := Copy(s2, Pos(vCommandEnd, s2) + Length(vCommandEnd), Length(s2));
            Delete(S, 1, Pos(vCommandEnd, S) + Length(vCommandEnd) - 1);
            SetCursorPos(MousePosX, MousePosY);
            Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTDOWN, MousePosX,
              MousePosY, 0, 0);
            Continue;
          End;
        mt_MouseLCU:
          Begin
            MousePosX := StrToInt(Copy(S, 1, Pos('<|>', S) - 1));
            Delete(S, 1, Pos('<|>', S) + 2);
            MousePosY := StrToInt(Copy(S, 1, Pos(vCommandEnd, S) - 1));
            Delete(S, 1, Pos(vCommandEnd, S) + Length(vCommandEnd) - 1);
            SetCursorPos(MousePosX, MousePosY);
            Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTUP, MousePosX,
              MousePosY, 0, 0);
            Continue;
          End;
        mt_MouseMCD:
          Begin
            MousePosX := StrToInt(Copy(S, 1, Pos('<|>', S) - 1));
            Delete(S, 1, Pos('<|>', S) + 2);
            MousePosY := StrToInt(Copy(S, 1, Pos(vCommandEnd, S) - 1));
            // s2 := Copy(s2, Pos(vCommandEnd, s2) + Length(vCommandEnd), Length(s2));
            Delete(S, 1, Pos(vCommandEnd, S) + Length(vCommandEnd) - 1);
            SetCursorPos(MousePosX, MousePosY);
            Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MIDDLEDOWN,
              MousePosX, MousePosY, 0, 0);
            Continue;
          End;
        mt_MouseMCU:
          Begin
            MousePosX := StrToInt(Copy(S, 1, Pos('<|>', S) - 1));
            Delete(S, 1, Pos('<|>', S) + 2);
            MousePosY := StrToInt(Copy(S, 1, Pos(vCommandEnd, S) - 1));
            Delete(S, 1, Pos(vCommandEnd, S) + Length(vCommandEnd) - 1);
            SetCursorPos(MousePosX, MousePosY);
            Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MIDDLEUP, MousePosX,
              MousePosY, 0, 0);
            Continue;
          End;
        mt_MouseRCD:
          Begin
            MousePosX := StrToInt(Copy(S, 1, Pos('<|>', S) - 1));
            Delete(S, 1, Pos('<|>', S) + 2);
            MousePosY := StrToInt(Copy(S, 1, Pos(vCommandEnd, S) - 1));
            Delete(S, 1, Pos(vCommandEnd, S) + Length(vCommandEnd) - 1);
            SetCursorPos(MousePosX, MousePosY);
            Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_RIGHTDOWN,
              MousePosX, MousePosY, 0, 0);
            Continue;
          End;
        mt_MouseRCU:
          Begin
            MousePosX := StrToInt(Copy(S, 1, Pos('<|>', S) - 1));
            Delete(S, 1, Pos('<|>', S) + 2);
            MousePosY := StrToInt(Copy(S, 1, Pos(vCommandEnd, S) - 1));
            // s2 := Copy(s2, Pos(vCommandEnd, s2) + Length(vCommandEnd), Length(s2));
            Delete(S, 1, Pos(vCommandEnd, S) + Length(vCommandEnd) - 1);
            SetCursorPos(MousePosX, MousePosY);
            Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_RIGHTUP, MousePosX,
              MousePosY, 0, 0);
            Continue;
          End;
        mt_MouseDBC:
          Begin
            MousePosX := StrToInt(Copy(S, 1, Pos('<|>', S) - 1));
            Delete(S, 1, Pos('<|>', S) + 2);
            MousePosY := StrToInt(Copy(S, 1, Pos(vCommandEnd, S) - 1));
            Delete(S, 1, Pos(vCommandEnd, S) + Length(vCommandEnd) - 1);
            Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTDOWN, MousePosX,
              MousePosY, 0, 0);
            Sleep(10);
            Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTUP, MousePosX,
              MousePosY, 0, 0);
            Sleep(10);
            Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTDOWN, MousePosX,
              MousePosY, 0, 0);
            Sleep(10);
            Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTUP, MousePosX,
              MousePosY, 0, 0);
            Continue;
          End;
        mt_MouseWheel:
          Begin
            MousePosWheel := DWORD(StrToInt(Copy(S, 1, Pos('<|>', S) - 1)));
            Delete(S, 1, Pos('<|>', S) + 2);
            MousePosX := StrToInt(Copy(S, 1, Pos('<|>', S) - 1));
            Delete(S, 1, Pos('<|>', S) + 2);
            MousePosY := StrToInt(Copy(S, 1, Pos(vCommandEnd, S) - 1));
            Delete(S, 1, Pos(vCommandEnd, S) + Length(vCommandEnd) - 1);
            Mouse_Event(MOUSEEVENTF_WHEEL, MousePosXNew, MousePosYNew,
              DWORD(MousePosWheel), 0);
            Continue;
          End;
        mt_MouseMove:
          Begin
            Try
              MousePosXNew := StrToInt(Copy(S, 1, Pos('<|>', S) - 1));
              Delete(S, 1, Pos('<|>', S) + 2);
              MousePosYNew := StrToInt(Copy(S, 1, Pos(vCommandEnd, S) - 1));
              Delete(S, 1, Pos(vCommandEnd, S) + Length(vCommandEnd) - 1);
              If Not((MousePosX = MousePosXNew) And
                (MousePosY = MousePosYNew)) Then
              Begin
                MousePosX := MousePosXNew;
                MousePosY := MousePosYNew;
                Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MOVE,
                  MousePosX * (65535 div Screen.Width),
                  MousePosY * (65535 div Screen.Height), 0, 0);
                Continue;
              End;
            Except
            End;
          End;
      End;
    End;
    ListPacks.Clear;
    ListPacks.Free;
  End;
End;

procedure Tfrm_Main.OnBinaryIn(Value: TIdBytes);
  Procedure WriteBytesToString(Const S: TIdBytes; Var Result: String);
  Var
    n: LongInt;
    Stream: TMemoryStream;
    StreamB: TStringStream;
  Begin
    n := Length(S);
    Stream := TMemoryStream.Create;
    StreamB := TStringStream.Create('', CompressionEncoding);
    Try
      If n > 0 Then
      Begin
        WriteTIdBytesToStream(Stream, S, n, 0);
        // Stream.Write(s[0], n);
        Stream.Position := 0;
        StreamB.LoadFromStream(Stream);
        StreamB.Position := 0;
      End;
      Result := StreamB.DataString;
    Finally
      FreeAndNil(Stream);
      FreeAndNil(StreamB);
    End;
  End;
  Procedure WriteBytesToStream(Const S: TIdBytes; Var Result: TMemoryStream);
  Var
    n: LongInt;
    Stream: TMemoryStream;
  Begin
    n := Length(S);
    Stream := TMemoryStream.Create;
    Result := TMemoryStream.Create;
    Try
      If n > 0 Then
      Begin
        WriteTIdBytesToStream(Stream, S, n, 0);
        // Stream.Write(s[0], n);
        Stream.Position := 0;
        Result.CopyFrom(Stream, Stream.Size);
        Result.Position := 0;
      End;
    Finally
      FreeAndNil(Stream);
    End;
  End;

Var
  PeerConnected: TPeerconnected;
  TempValue: TIdBytes;
  Stream: TMemoryStream;
Begin
  If Not(frm_Main.CloseConnection) Then
  Begin
    If NewDeskCapture Then
    Begin
      If dmCaptureScreen.FastDesktop <> Nil Then
      Begin
        dmCaptureScreen.FastDesktop.Delay := 5;
        dmCaptureScreen.FastDesktop.PixelFormat := TPixelFormat(ImageViewQ);
        dmCaptureScreen.FastDesktop.Active := True;
      End;
    End
    Else
    Begin
      If Not tScreenshot.Enabled Then
      Begin
        // tScreenShot.SyncEvent := True;
        tScreenshot.Enabled := True;
      End;
    End;
    TempValue := Value;
    WriteBytesToStream(TempValue, Stream);
    GetStreamM(ipPSDeskTopClient, Stream);
  End;
End;

Procedure Tfrm_Main.OnGetLongString(Value: String);
Var
  PeerConnected: TPeerconnected;
Begin
  If Not(frm_Main.CloseConnection) Then
  Begin
    If Not tScreenshot.Enabled Then
    Begin
      // tScreenShot.SyncEvent := True;
      tScreenshot.Enabled := True;
    End;
    If Not(inTimerCollect) Then
    Begin
      If Not(HightSpeed) Then
      Begin
        AddLog(vLinhaKeys);
        PeerConnected := ipPSDeskTopClient.GetActivePeer;
        If PeerConnected <> Nil Then
          ipPSDeskTopClient.SendBuffer
            (ipPSDeskTopClient.GetIpSend(PeerConnected), PeerConnected.Port,
            '<|GETFULLSCREENSHOT|>' + vCommandEnd, False, dtt_Async);
        If Not vReceiveData.Active Then
          vReceiveData.Active := True;
        vReceiveData.AddPack(Value);
        // GetStreamM(ipPSDeskTopClient, Value);
      End
      Else
      Begin
        AddLog(vLinhaKeys);
        PeerConnected := ipPSDeskTopClient.GetActivePeer;
        If PeerConnected <> Nil Then
          ipPSDeskTopClient.SendBuffer
            (ipPSDeskTopClient.GetIpSend(PeerConnected), PeerConnected.Port,
            vLinhaKeys + '<|GETFULLSCREENSHOT|>' + vCommandEnd, False,
            dtt_Async);
        frm_Main.vLinhaKeys := '';
        If Not vReceiveData.Active Then
          vReceiveData.Active := True;
        vReceiveData.AddPack(Value);
        // GetStreamM(ipPSDeskTopClient, Value);
      End;
    End
    Else
    Begin
      AddLog(vLinhaKeys);
      frm_Main.vLinhaKeys := '';
      If Not vReceiveData.Active Then
        vReceiveData.Active := True;
      vReceiveData.AddPack(Value);
      // GetStreamM(ipPSDeskTopClient, Value);
    End;
  End;
End;

Procedure Tfrm_Main.DataIn(Value: String);
Var
  SendType: Boolean;
  S: String;
  PeerConnected: TPeerconnected;
  Function ReadString(const Stream: TStream): AnsiString;
  Var
    n: LongInt;
  Begin
    Stream.Seek(0, soFromBeginning);
    n := Stream.Size;
    If n > 0 Then
    Begin
      SetLength(Result, n);
      Stream.Read(Result[1], n);
    End;
  End;
  Procedure DeskTopCommands(Var Value: String);
  Var
    ListPacks: TListPacks;
    I: Integer;
    vTempString: String;
    Procedure LoadPacks(Var ListPacks: TListPacks; Var Value: String);
    Var
      PackResult: TPackResult;
    Begin
      While (Pos(tInitCom, Value) > 0) Do
      Begin
        PackResult.Data := Copy(Value, Pos(tInitCom, Value) + Length(tInitCom),
          Pos(tDataSep, Value) - Length(tInitCom) - 1);
        Delete(Value, 1, Pos(tDataSep, Value) + Length(tDataSep) - 1);
        PackResult.MessageType :=
          StrToInt(Copy(Value, 1, Pos(tFinalCom, Value) - 1));
        Delete(Value, 1, Pos(tFinalCom, Value) + Length(tFinalCom) - 1);
        ListPacks.Add(PackResult);
      End;
    End;

  Begin
    ListPacks := TListPacks.Create;
    Try
      vTempString := Value; // DecompressString(Value);
      If vTempString <> '' Then
      Begin
        Value := vTempString;
        If (Value <> '') And (Pos(tInitCom, Value) > 0) Then
        Begin
          LoadPacks(ListPacks, Value);
          If ListPacks.Count > 0 Then
            ProcessCommands(ListPacks);
        End;
      End;
    Finally
    End;
  End;
  Function Conexao(Value: String): Boolean;
  Var
    aPeerConnected: TPeerconnected;
  Begin
    Result := False;
    If (Pos('<|ACCESSING|>', Value) > 0) Then
    Begin
      Result := True;
      If Not Accessed Then
      Begin
        Accessed := True;
        OldWallpaper := GetWallpaperDirectory;
        ChangeWallpaper('');
        TargetID_MaskEdit.Enabled := False;
        Connect_BitBtn.Enabled := False;
        Status_Image.Picture.Assign(frm_Main.Image3.Picture);
        Status_Label.Caption := 'Suporte conectado!';
        HideApplication;
        Delete(S, 1, Pos(vCommandEnd, S) + Length(vCommandEnd) - 1);
        aPeerConnected := ipCommandsClient.GetActivePeer;
        If aPeerConnected <> Nil Then
          ipCommandsClient.SendBuffer
            (ipCommandsClient.GetIpSend(aPeerConnected), aPeerConnected.Port,
            '<|INITCAPTURE|>' + vCommandEnd, False);
      End;
    End
    Else If (Pos('<|INITCAPTURE|>', Value) > 0) Then
    Begin
      Result := True;
      NewFrame := 0;
      vOldS := '';
      Clipboard_Timer.Enabled := True;
      vCaptureSideClient := True;
      vStopSendFile := True;
      vCancelOPSendFile := True;
      OnSend := False;
      S := '';
      aPeerConnected := ipCommandsClient.GetActivePeer;
      If aPeerConnected <> Nil Then
        ipCommandsClient.SendBuffer(ipCommandsClient.GetIpSend(aPeerConnected),
          aPeerConnected.Port, '<|STARTEXECOMMANDS|>' + '<|BESTQ|>' +
          IntToStr(frm_Main.cbQualidade.ItemIndex) + vCommandEnd, False);
    End
    Else If (Pos('<|STARTEXECOMMANDS|>', Value) > 0) then
    Begin
      Result := True;
      // Thread do Teclado/Mouse
      If dmCaptureScreen <> Nil Then
        dmCaptureScreen.ClearAtualFrame;
      If Pos('<|BESTQ|>', Value) > 0 Then
      Begin
        ResolutionTargetWidth := Screen.Width;
        ResolutionTargetHeight := Screen.Height;
        Value := Copy(Value, Pos('<|BESTQ|>', Value) + 9, 1);
        Case StrToInt(Value) Of
          0:
            ImageViewQ := tiv_MonoC;
          1, 2:
            ImageViewQ := tiv_Medium;
          3:
            ImageViewQ := tiv_Alta;
          4:
            ImageViewQ := tiv_Real;
        End;
      End;
      Delete(S, 1, Pos(vCommandEnd, S) + Length(vCommandEnd) - 1);
      aPeerConnected := ipCommandsClient.GetActivePeer;
      If aPeerConnected <> Nil Then
        ipCommandsClient.SendBuffer(ipCommandsClient.GetIpSend(aPeerConnected),
          aPeerConnected.Port, '<|WAIT|>' + vCommandEnd, False);
    End
    Else If (Pos('<|WAIT|>', Value) > 0) Then
    Begin
      Result := True;
      vIConnect := True;
      aPeerConnected := ipCommandsClient.GetActivePeer;
      If aPeerConnected <> Nil Then
        ipCommandsClient.SendBuffer(ipCommandsClient.GetIpSend(aPeerConnected),
          aPeerConnected.Port, '<|GETFULLSCREENSHOT|>NEW' + vCommandEnd, False);
      // Sleep(10);
    End
    Else If (Pos('<|RESOLUTION|>', Value) > 0) then
    Begin
      Result := True;
      Delete(Value, 1, Pos('<|RESOLUTION|>', Value) + 13);
      ResolutionTargetWidth := StrToInt(Copy(Value, 1, Pos('<|>', Value) - 1));
      Delete(Value, 1, Pos('<|>', Value) + 2);
      ResolutionTargetHeight :=
        StrToInt(Copy(Value, 1, Pos(vCommandEnd, Value) - 1));
      Delete(Value, 1, Pos(vCommandEnd, Value) + Length(vCommandEnd) - 1);
      Self.Visible := False;
      If frm_RemoteScreen <> Nil Then
      Begin
        Try
          FreeAndNil(frm_RemoteScreen);
        Except
        End;
      End;
      frm_RemoteScreen := Tfrm_RemoteScreen.Create(Self);
      frm_RemoteScreen.Show;
    End;
  End;

Begin
  Try
    If frm_Main.CloseConnection Then
      Exit;
    SendType := SendResolution;
    vInitString := Value;
    If Conexao(vInitString) Then
      Exit;
    If Pos(TNoneData, vInitString) > 0 Then
      Exit;
    DeskTopCommands(vInitString);
    If ipPSDeskTopClient.Active Then
      While (vInitString <> '') Do
      Begin
        Try
          S := vInitString;
          vInitString := '';
          If (Pos(TNewFrameData, S) > 0) Then
          Begin
            vNewFrameClient := True;
            Exit;
          End;
          If (Pos('<|GETFULLSCREENSHOT|>', S) > 0) then
          Begin
            If frm_Chat = Nil Then
            Begin
              frm_Chat := Tfrm_Chat.Create(Application);
              frm_Chat.Show;
            End;
            SendType := True;
            vNewFrameClient := True;
            If (Pos('<|STOPTRANSFER|>', S) > 0) Then
            Begin
              Delete(S, Pos('<|STOPTRANSFER|>', S), Pos(vCommandEnd, S) +
                Length(vCommandEnd) - 1);
              vStopSendFile := True;
              vCancelOPSendFile := True;
              ipPSMain_Socket.
                Write('<|REDIRECT|><|RESETTRANSF|>' + vCommandEnd);
            End;
            If Trim(S) = '' Then
              Exit;
            If Pos('<|GETFULLSCREENSHOT|>', S) > 1 Then
              vInitString := Copy(S, 1, Pos('<|GETFULLSCREENSHOT|>', S) - 1);
            If Not(HightSpeed) Then
            Begin
              If Not OnSend Then
                OnSend := True;
              If (vInitString <> '') Then
              Begin
                AddLog(vInitString);
                ProcessKeys(vInitString);
                vInitString := '';
              End;
              If (inTimerCollect) And (tScreenshot.Enabled) Then
                Exit;
              // ExecMethod(Procedure
              // Begin
              LastFrameTime := Now;
              If SendType then
              Begin
                ResolutionWidth := Screen.Width;
                ResolutionHeight := Screen.Height;
                If Not SendResolution Then
                Begin
                  PeerConnected := ipPSDeskTopClient.GetActivePeer;
                  If PeerConnected <> Nil Then
                    ipPSDeskTopClient.SendBuffer
                      (ipPSDeskTopClient.GetIpSend(PeerConnected),
                      PeerConnected.Port, '<|RESOLUTION|>' +
                      IntToStr(Screen.Width) + '<|>' + IntToStr(Screen.Height) +
                      vCommandEnd, False);
                  SendResolution := True;
                End;
                Try
                  dmCaptureScreen.InitCapture;
                  If Not(inTimerCollect) Then
                  Begin
                    CaptureExecute;
                    OnSend := False;
                  End
                  Else
                  Begin
                    If NewDeskCapture Then
                    Begin
                      If dmCaptureScreen.FastDesktop <> Nil Then
                      Begin
                        dmCaptureScreen.FastDesktop.Delay := 5;
                        dmCaptureScreen.FastDesktop.Active := True;
                      End;
                    End
                    Else
                    Begin
                      If tScreenshot <> Nil Then
                      Begin
                        // tScreenShot.SyncEvent := False;
                        tScreenshot.Enabled := True;
                      End;
                    End;
                  End;
                Except
                End;
                S := '';
              End;
              // End);
            End
            Else
            Begin
              If (vInitString <> '') Then
              Begin
                AddLog(vInitString);
                ProcessKeys(vInitString);
                vInitString := '';
              End;
              LastFrameTime := Now;
              If SendType then
              Begin
                ResolutionWidth := Screen.Width;
                ResolutionHeight := Screen.Height;
                If Not SendResolution Then
                Begin
                  frm_Main.ipPSMain_Socket.
                    Write('<|REDIRECT|><|RESOLUTION|>' + IntToStr(Screen.Width)
                    + '<|>' + IntToStr(Screen.Height) + vCommandEnd);
                  SendResolution := True;
                End;
                Try
                  If Not(inTimerCollect) Then
                  Begin
                    CaptureExecute;
                    OnSend := False;
                  End
                  Else
                  Begin
                    If NewDeskCapture Then
                    Begin
                      If dmCaptureScreen.FastDesktop <> Nil Then
                      Begin
                        dmCaptureScreen.FastDesktop.Delay := 5;
                        dmCaptureScreen.FastDesktop.Active := True;
                      End;
                    End
                    Else
                    Begin
                      If tScreenshot <> Nil Then
                      Begin
                        // tScreenShot.SyncEvent := False;
                        tScreenshot.Enabled := True;
                      End;
                    End;
                  End;
                Except
                End;
                S := '';
              End;
            End;
          End;
          S := '';
        Except
        End;
      End;
  Finally
  End;
End;

Procedure Tfrm_Main.DataInFiles(Value: String);
Var
  Extension, vString, vLine, S, s2: String;
  vDataSendReceive, FoldersAndFiles: TStringList;
  I, WindowHandleMsg: Integer;
  L: TListItem;
  StreamValue: TStringStream;
  FileStream: TFileStream;
  PeerConnected: TPeerconnected;
  Function GetValue(Var Value: String): String;
  Begin
    If Pos('|', Value) > 0 Then
    Begin
      Result := Copy(Value, 1, Pos('|', Value) - 1);
      Delete(Value, 1, Pos('|', Value));
    End
    Else
    Begin
      Result := Copy(Value, 1, Length(Value));
      Delete(Value, 1, Length(Value));
    End;
  End;
  Function MontaLinhaEnvio(Value: TStringList): AnsiString;
  Var
    I: Integer;
  Begin
    Result := '';
    For I := 0 To Value.Count - 1 do
      Result := Result + Value[I] + #$D#$A;
  End;

Begin
  FileStream := Nil;
  // Application.ProcessMessages;
  If ipPSFilesClient.Active Then
  Begin
    Try
      S := Value;
      If (Pos('<|DIRECTORYTOSAVE|>', S) > 0) Then
      Begin
        s2 := S;
        Delete(s2, 1, Pos('<|DIRECTORYTOSAVE|>', s2) + 18);
        frm_Main.DirectoryToSaveFile := Copy(s2, 1, Pos('<|>', s2) - 1);
        Delete(s2, 1, Pos('<|>', s2) + 2);
        s2 := Copy(s2, 1, Pos(vCommandEnd, s2) - 1);
        s2 := StringReplace(s2, '<|SIZE|>', '', [rfReplaceAll, rfIgnoreCase]);
        s2 := StringReplace(s2, vCommandEnd, '', [rfReplaceAll, rfIgnoreCase]);
        FileSize := StrToInt(s2);
        S := '';
        vString := '';
        vDecompressString := '';
        vStopSendFile := False;
        vCancelOPSendFile := vStopSendFile;
        ReceivingFile := True;
      End;
      If (Pos('<|$initstream$|>', S) > 0) Then
      Begin
        vStopSendFile := False;
        vCancelOPSendFile := vStopSendFile;
        vDecompressString := '';
        s2 := S;
        Delete(s2, 1, Pos('<|$initstream$|>', s2) + 15);
        s2 := Copy(s2, 1, Pos(vCommandEnd, s2) - 1);
        FileSize := StrToInt(s2);
        If Not(Viewer) And (frm_ShareFiles <> Nil) Then
        Begin
          frm_ShareFiles.Download_ProgressBar.Max := FileSize;
          frm_ShareFiles.Download_ProgressBar.Position := 0;
          frm_ShareFiles.SizeDownload_Label.Caption := 'Tamanho: ' + GetSize(0)
            + ' / ' + GetSize(FileSize);
          If frm_RemoteScreen <> Nil Then
          Begin
            If Not(vCancelOPSendFile) Then
            Begin
              frm_RemoteScreen.pbDados.Max := FileSize;
              frm_RemoteScreen.pbDados.Position := 0;
              frm_RemoteScreen.rzpDownload.Visible := True;
              If frm_ShareFiles <> Nil Then
                frm_ShareFiles.FreeForClose :=
                  Not(frm_RemoteScreen.rzpDownload.Visible);
            End;
          End;
        End;
        S := '';
        vString := '';
        vDecompressString := '';
        ReceivingFile := True;
      End
      Else If (Pos('<|>SENDBUFFER<|>', S) > 0) Then
      Begin
        vString := Copy(S, Pos('<|>SENDBUFFER<|>', S) + 16,
          Pos(vCommandEnd, S) - 1);
        Delete(S, 1, Pos(vCommandEnd, S) + Length(vCommandEnd) - 1);
        vString := StringReplace(vString, vCommandEnd, '', [rfReplaceAll]);
        If (vString <> '') And (vString <> vCommandEnd) Then
        Begin
          vDecompressString := vDecompressString + vString;
          If (frm_ShareFiles <> Nil) Then
          Begin
            If SendingFile Then
            Begin
              frm_ShareFiles.Upload_ProgressBar.Max := FileSize;
              frm_ShareFiles.Upload_ProgressBar.Position :=
                Length(vDecompressString);
              frm_ShareFiles.SizeUpload_Label.Caption := 'Tamanho: ' +
                GetSize(Length(vDecompressString)) + ' / ' + GetSize(FileSize);
            End
            Else
            Begin
              frm_ShareFiles.Download_ProgressBar.Max := FileSize;
              frm_ShareFiles.Download_ProgressBar.Position :=
                Length(vDecompressString);
              frm_ShareFiles.SizeDownload_Label.Caption := 'Tamanho: ' +
                GetSize(Length(vDecompressString)) + ' / ' + GetSize(FileSize);
            End;
          End;
          If (frm_RemoteScreen <> Nil) Then
          Begin
            If Not(vCancelOPSendFile) Then
            Begin
              frm_RemoteScreen.pbDados.Max := FileSize;
              frm_RemoteScreen.pbDados.Position := Length(vDecompressString);
              frm_RemoteScreen.rzpDownload.Visible := True;
              If frm_ShareFiles <> Nil Then
                frm_ShareFiles.FreeForClose :=
                  Not(frm_RemoteScreen.rzpDownload.Visible);
            End;
          End;
        End;
      End;
      If (Pos('<|>BUFFEREND<|>', S) > 0) Then
      Begin
        Try
          vString := vDecompressString;
          If frm_ShareFiles <> Nil Then
            WindowHandleMsg := frm_ShareFiles.Handle
          Else
            WindowHandleMsg := Self.Handle;
          If vString = '' Then
          Begin
            ReceivingFile := False;
            If SendingFile Then
              MessageBox(WindowHandleMsg, 'Upload finalizado com erro !',
                'Aegys - Compartilhamento de Arquivos', 64)
            Else
              MessageBox(WindowHandleMsg, 'Download finalizado com erro !',
                'Aegys - Compartilhamento de Arquivos', 64);
            Exit;
          End;
        Except
          ReceivingFile := False;
          Exit;
        End;
        FileStream := TFileStream.Create(frm_Main.DirectoryToSaveFile + '.tmp',
          fmCreate or fmOpenReadWrite);
        FileStreamFromBase64(vString, FileStream);
        FreeAndNil(FileStream);
        vString := '';
        If (FileExists(frm_Main.DirectoryToSaveFile)) Then
          DeleteFile(frm_Main.DirectoryToSaveFile);
        RenameFile(frm_Main.DirectoryToSaveFile + '.tmp',
          frm_Main.DirectoryToSaveFile);
        frm_Main.DirectoryToSaveFile := '';
        If (frm_ShareFiles <> Nil) Then
        Begin
          If SendingFile Then
          Begin
            frm_ShareFiles.Upload_ProgressBar.Position := 0;
            frm_ShareFiles.Upload_BitBtn.Enabled := True;
            frm_ShareFiles.SizeUpload_Label.Caption := 'Tamanho: 0 B / 0 B';
          End
          Else
          Begin
            frm_ShareFiles.Download_ProgressBar.Position := 0;
            frm_ShareFiles.Download_BitBtn.Enabled := True;
            frm_ShareFiles.SizeDownload_Label.Caption := 'Tamanho: 0 B / 0 B';
          End;
          If frm_RemoteScreen <> Nil Then
          Begin
            frm_RemoteScreen.pbDados.Max := FileSize;
            frm_RemoteScreen.pbDados.Position := 0;
            frm_RemoteScreen.rzpDownload.Visible := False;
            If frm_ShareFiles <> Nil Then
              frm_ShareFiles.FreeForClose :=
                Not(frm_RemoteScreen.rzpDownload.Visible);
          End;
          If frm_ShareFiles <> Nil Then
            WindowHandleMsg := frm_ShareFiles.Handle
          Else
            WindowHandleMsg := Self.Handle;
          If SendingFile Then
            MessageBox(WindowHandleMsg, 'Upload completo!',
              'Aegys - Compartilhamento de Arquivos', 64)
          Else
            MessageBox(WindowHandleMsg, 'Download completo!',
              'Aegys - Compartilhamento de Arquivos', 64);
          If frm_ShareFiles <> Nil Then
            frm_ShareFiles.RenewDir;
        End;
        ReceivingFile := False;
        if SendingFile then
        Begin
          SendingFile := False;
          PeerConnected := ipPSFilesClient.GetActivePeer;
          If PeerConnected <> Nil Then
            ipPSFilesClient.SendBuffer(ipPSFilesClient.GetIpSend(PeerConnected),
              PeerConnected.Port, '<|GETFOLDERS|>' +
              frm_ShareFiles.Directory_Edit + vCommandEnd);
        End;
        s2 := '';
        S := '';
      End;
      // Share Files
      // Request Folder List
      If (Pos('<|GETDRIVERS|>', S) > 0) Then
      Begin
        s2 := S;
        vDataSendReceive := TStringList.Create;
        ListDrivers(vDataSendReceive);
        Try
          PeerConnected := ipPSFilesClient.GetActivePeer;
          If PeerConnected <> Nil Then
            ipPSFilesClient.SendBuffer(ipPSFilesClient.GetIpSend(PeerConnected),
              PeerConnected.Port, '<|DRIVERLIST|>' + vDataSendReceive.Text +
              vCommandEnd);
        Finally
          vDataSendReceive.Free;
          S := '';
        End;
      End;
      If (Pos('<|GETFOLDERS|>', S) > 0) Then
      Begin
        s2 := S;
        vDataSendReceive := TStringList.Create;
        Delete(s2, 1, Pos('<|GETFOLDERS|>', s2) + 13);
        s2 := Copy(s2, 1, Pos(vCommandEnd, s2) - 1);
        ListFoldersB(s2, vDataSendReceive);
        Try
          PeerConnected := ipPSFilesClient.GetActivePeer;
          If PeerConnected <> Nil Then
            ipPSFilesClient.SendBuffer(ipPSFilesClient.GetIpSend(PeerConnected),
              PeerConnected.Port, '<|FOLDERLIST|>' + vDataSendReceive.Text +
              vCommandEnd);
        Finally
          vDataSendReceive.Free;
          S := '';
        End;
      End;
      // Request Files List
      If (Pos('<|GETFILES|>', S) > 0) Then
      Begin
        s2 := S;
        Delete(s2, 1, Pos('<|GETFILES|>', s2) + 11);
        s2 := Copy(s2, 1, Pos(vCommandEnd, s2) - 1);
        vDataSendReceive := TStringList.Create;
        ListFilesB(s2, vDataSendReceive);
        Try
          PeerConnected := ipPSFilesClient.GetActivePeer;
          If PeerConnected <> Nil Then
            ipPSFilesClient.SendBuffer(ipPSFilesClient.GetIpSend(PeerConnected),
              PeerConnected.Port, '<|FILESLIST|>' +
              MontaLinhaEnvio(vDataSendReceive) + vCommandEnd);
        Finally
          vDataSendReceive.Free;
          S := '';
        End;
      End;
      // Receive Files List
      If (Pos('<|FILESLIST|>', S) > 0) then
      Begin
        s2 := S;
        s2 := Copy(s2, Pos('<|FILESLIST|>', s2) + 13, (Pos(vCommandEnd, s2) - 1)
          - (Pos('<|FILESLIST|>', s2) + 13));
        FoldersAndFiles := TStringList.Create;
        FoldersAndFiles.Text := s2;
        Delete(s2, 1, Pos(vCommandEnd, s2) + Length(vCommandEnd) - 1);
        If FoldersAndFiles.Count > 0 Then
          FoldersAndFiles.Sort;
        For I := 0 to FoldersAndFiles.Count - 1 do
        Begin
          L := frm_ShareFiles.ShareFiles_ListView.Items.Add;
          vLine := FoldersAndFiles.Strings[I];
          L.Caption := GetValue(vLine);
          L.SubItems.Add(GetValue(vLine));
          L.SubItems.Add(GetValue(vLine));
          L.SubItems.Add(GetValue(vLine));
          L.ImageIndex := frm_ShareFiles.GetIcon(L.Caption);
        End;
        FreeAndNil(FoldersAndFiles);
        // frm_ShareFiles.Directory_Edit.Enabled := True;
        frm_ShareFiles.Caption := 'Compartilhamento de Arquivos - ' +
          IntToStr(frm_ShareFiles.ShareFiles_ListView.Items.Count) +
          ' Itens Encontrados';
        S := '';
      End;
      // Receive Folder List
      If (Pos('<|DRIVERLIST|>', S) > 0) Then
      Begin
        s2 := S;
        Delete(s2, 1, Pos('<|DRIVERLIST|>', s2) + 13);
        s2 := Copy(s2, 1, Pos(vCommandEnd, s2) - 1);
        FoldersAndFiles := TStringList.Create;
        FoldersAndFiles.Text := s2;
        frm_ShareFiles.cbRemoteDrivers.Items.Clear;
        frm_ShareFiles.lNomeComputadorRemoto.Caption := FoldersAndFiles[0];
        For I := 1 To FoldersAndFiles.Count - 1 Do
          frm_ShareFiles.cbRemoteDrivers.Items.Add(' ' + FoldersAndFiles[I]);
        FreeAndNil(FoldersAndFiles);
        If frm_ShareFiles.cbRemoteDrivers.Items.Count > 0 Then
        Begin
          frm_ShareFiles.cbRemoteDrivers.ItemIndex := 0;
          frm_ShareFiles.cbRemoteDrivers.OnChange
            (frm_ShareFiles.cbRemoteDrivers);
        End;
        S := '';
      End;
      If (Pos('<|FOLDERLIST|>', S) > 0) Then
      Begin
        s2 := S;
        Delete(s2, 1, Pos('<|FOLDERLIST|>', s2) + 13);
        s2 := Copy(s2, 1, Pos(vCommandEnd, s2) - 1);
        Delete(s2, 1, Pos(vCommandEnd, s2) + Length(vCommandEnd) - 1);
        FoldersAndFiles := TStringList.Create;
        FoldersAndFiles.Text := s2;
        FoldersAndFiles.Sort;
        s2 := '..' + #13 + FoldersAndFiles.Text;
        FoldersAndFiles.Text := s2;
        frm_ShareFiles.ShareFiles_ListView.Clear;
        For I := 0 To FoldersAndFiles.Count - 1 Do
        Begin
          If (FoldersAndFiles.Strings[I] = '.') Or
            (FoldersAndFiles.Strings[I] = '') Then
            Continue;
          L := frm_ShareFiles.ShareFiles_ListView.Items.Add;
          If (FoldersAndFiles.Strings[I] = '..') Then
          Begin
            L.Caption := '..';
            L.ImageIndex := 0;
          End
          Else
          Begin
            L.Caption := FoldersAndFiles.Strings[I];
            L.ImageIndex := 1;
          End;
          frm_ShareFiles.Caption := 'Compartilhamento de Arquivos - ' +
            IntToStr(frm_ShareFiles.ShareFiles_ListView.Items.Count) +
            ' Items found';
        End;
        FreeAndNil(FoldersAndFiles);
        PeerConnected := ipPSFilesClient.GetActivePeer;
        If PeerConnected <> Nil Then
          ipPSFilesClient.SendBuffer(ipPSFilesClient.GetIpSend(PeerConnected),
            PeerConnected.Port, '<|GETFILES|>' + frm_ShareFiles.Directory_Edit +
            vCommandEnd);
        S := '';
      End;
    Except
    End;
  End;
  // Application.ProcessMessages;
End;

Procedure Tfrm_Main.ProcessKeys(Value: String);
Var
  S, s2, s3: String;
  MousePosWheel, MousePosXNew, MousePosYNew: Integer;
  Function ExistCommands(Value: String): Boolean;
  Begin
    Result := (Pos('<|SETMOUSELEFTCLICKDOWN|>', Value) > 0) Or
      (Pos('<|SETMOUSELEFTCLICKUP|>', Value) > 0) Or
      (Pos('<|SETMOUSERIGHTCLICKDOWN|>', Value) > 0) Or
      (Pos('<|SETMOUSERIGHTCLICKUP|>', Value) > 0) Or
      (Pos('<|SETMOUSEMIDDLEDOWN|>', Value) > 0) Or
      (Pos('<|SETMOUSEMIDDLEUP|>', Value) > 0) Or
      (Pos('<|SETMOUSEDOUBLECLICK|>', Value) > 0) Or
      (Pos('<|WHEELMOUSE|>', Value) > 0) Or (Pos('<|SETMOUSEPOS|>', Value) > 0)
      Or (Pos('<|CLIPBOARD|>', Value) > 0) Or (Pos('<|ALTDOWN|>', Value) > 0) Or
      (Pos('<|ALTUP|>', Value) > 0) Or (Pos('<|CTRLDOWN|>', Value) > 0) Or
      (Pos('<|CTRLUP|>', Value) > 0) Or (Pos('<|SHIFTDOWN|>', Value) > 0) Or
      (Pos('<|SHIFTUP|>', Value) > 0) Or (Pos('?', Value) > 0);
  End;
  Function PartialCommands(Value: String): Boolean;
  Begin
    Result := ((Pos('<|', Value) > 0) Or (Pos('|>', Value) > 0)) And
      (Pos(vCommandEnd, Value) + 1 <> Pos('<|', Value));
  End;
  Procedure ProcessCommands(source: String; Var OutPut: String);
  Begin
    if Pos('|>', source) > 0 then
      OutPut := Copy(source, 1, Pos('|>', source) + 1)
    Else
      OutPut := Copy(source, 1, Length(source));
  End;

Begin
  Try
    S := Value;
    If S <> '' Then
    Begin
      s2 := S;
      s3 := '';
      While ((s2 <> '') And (ExistCommands(s2))) Do
      Begin
        ProcessCommands(s2, s3);
        If (Pos('<|SETMOUSELEFTCLICKDOWN|>', s3) > 0) then
        Begin
          Delete(s2, 1, Pos('<|SETMOUSELEFTCLICKDOWN|>', s2) + 24);
          MousePosX := StrToInt(Copy(s2, 1, Pos('<|>', s2) - 1));
          Delete(s2, 1, Pos('<|>', s2) + 2);
          MousePosY := StrToInt(Copy(s2, 1, Pos(vCommandEnd, s2) - 1));
          // s2 := Copy(s2, Pos(vCommandEnd, s2) + Length(vCommandEnd), Length(s2));
          Delete(s2, 1, Pos(vCommandEnd, s2) + Length(vCommandEnd) - 1);
          SetCursorPos(MousePosX, MousePosY);
          Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTDOWN, MousePosX,
            MousePosY, 0, 0);
          Continue;
        End;
        If (Pos('<|SETMOUSELEFTCLICKUP|>', s3) > 0) then
        Begin
          Delete(s2, 1, Pos('<|SETMOUSELEFTCLICKUP|>', s2) + 22);
          MousePosX := StrToInt(Copy(s2, 1, Pos('<|>', s2) - 1));
          Delete(s2, 1, Pos('<|>', s2) + 2);
          MousePosY := StrToInt(Copy(s2, 1, Pos(vCommandEnd, s2) - 1));
          // s2 := Copy(s2, Pos(vCommandEnd, s2) + Length(vCommandEnd), Length(s2));
          Delete(s2, 1, Pos(vCommandEnd, s2) + Length(vCommandEnd) - 1);
          SetCursorPos(MousePosX, MousePosY);
          Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTUP, MousePosX,
            MousePosY, 0, 0);
          Continue;
        End;
        If (Pos('<|SETMOUSERIGHTCLICKDOWN|>', s3) > 0) Then
        Begin
          Delete(s2, 1, Pos('<|SETMOUSERIGHTCLICKDOWN|>', s2) + 25);
          MousePosX := StrToInt(Copy(s2, 1, Pos('<|>', s2) - 1));
          Delete(s2, 1, Pos('<|>', s2) + 2);
          MousePosY := StrToInt(Copy(s2, 1, Pos(vCommandEnd, s2) - 1));
          // s2 := Copy(s2, Pos(vCommandEnd, s2) + Length(vCommandEnd), Length(s2));
          Delete(s2, 1, Pos(vCommandEnd, s2) + Length(vCommandEnd) - 1);
          SetCursorPos(MousePosX, MousePosY);
          Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_RIGHTDOWN, MousePosX,
            MousePosY, 0, 0);
          Continue;
        End;
        If (Pos('<|SETMOUSERIGHTCLICKUP|>', s3) > 0) Then
        Begin
          Delete(s2, 1, Pos('<|SETMOUSERIGHTCLICKUP|>', s2) + 23);
          MousePosX := StrToInt(Copy(s2, 1, Pos('<|>', s2) - 1));
          Delete(s2, 1, Pos('<|>', s2) + 2);
          MousePosY := StrToInt(Copy(s2, 1, Pos(vCommandEnd, s2) - 1));
          // s2 := Copy(s2, Pos(vCommandEnd, s2) + Length(vCommandEnd), Length(s2));
          Delete(s2, 1, Pos(vCommandEnd, s2) + Length(vCommandEnd) - 1);
          SetCursorPos(MousePosX, MousePosY);
          Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_RIGHTUP, MousePosX,
            MousePosY, 0, 0);
          Continue;
        End;
        If (Pos('<|SETMOUSEMIDDLEDOWN|>', s3) > 0) Then
        Begin
          Delete(s2, 1, Pos('<|SETMOUSEMIDDLEDOWN|>', s2) + 21);
          MousePosX := StrToInt(Copy(s2, 1, Pos('<|>', s2) - 1));
          Delete(s2, 1, Pos('<|>', s2) + 2);
          MousePosY := StrToInt(Copy(s2, 1, Pos(vCommandEnd, s2) - 1));
          // s2 := Copy(s2, Pos(vCommandEnd, s2) + Length(vCommandEnd), Length(s2));
          Delete(s2, 1, Pos(vCommandEnd, s2) + Length(vCommandEnd) - 1);
          SetCursorPos(MousePosX, MousePosY);
          Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MIDDLEDOWN, MousePosX,
            MousePosY, 0, 0);
          Continue;
        End;
        If (Pos('<|SETMOUSEMIDDLEUP|>', s3) > 0) Then
        Begin
          Delete(s2, 1, Pos('<|SETMOUSEMIDDLEUP|>', s2) + 19);
          MousePosX := StrToInt(Copy(s2, 1, Pos('<|>', s2) - 1));
          Delete(s2, 1, Pos('<|>', s2) + 2);
          MousePosY := StrToInt(Copy(s2, 1, Pos(vCommandEnd, s2) - 1));
          // s2 := Copy(s2, Pos(vCommandEnd, s2) + Length(vCommandEnd), Length(s2));
          Delete(s2, 1, Pos(vCommandEnd, s2) + Length(vCommandEnd) - 1);
          SetCursorPos(MousePosX, MousePosY);
          Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MIDDLEUP, MousePosX,
            MousePosY, 0, 0);
          Continue;
        End;
        If (Pos('<|SETMOUSEDOUBLECLICK|>', s3) > 0) Then
        Begin
          Delete(s2, 1, Pos('<|SETMOUSEDOUBLECLICK|>', s2) + 22);
          MousePosX := StrToInt(Copy(s2, 1, Pos('<|>', s2) - 1));
          Delete(s2, 1, Pos('<|>', s2) + 2);
          MousePosY := StrToInt(Copy(s2, 1, Pos(vCommandEnd, s2) - 1));
          Delete(s2, 1, Pos(vCommandEnd, s2) + Length(vCommandEnd) - 1);
          Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTDOWN, MousePosX,
            MousePosY, 0, 0);
          Sleep(10);
          Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTUP, MousePosX,
            MousePosY, 0, 0);
          Sleep(10);
          Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTDOWN, MousePosX,
            MousePosY, 0, 0);
          Sleep(10);
          Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTUP, MousePosX,
            MousePosY, 0, 0);
          Continue;
        End;
        If (Pos('<|WHEELMOUSE|>', s3) > 0) Then
        Begin
          Delete(s2, 1, Pos('<|WHEELMOUSE|>', s2) + 13);
          MousePosWheel := DWORD(StrToInt(Copy(s2, 1, Pos('<|>', s2) - 1)));
          Delete(s2, 1, Pos('<|>', s2) + 2);
          MousePosX := StrToInt(Copy(s2, 1, Pos('<|>', s2) - 1));
          Delete(s2, 1, Pos('<|>', s2) + 2);
          MousePosY := StrToInt(Copy(s2, 1, Pos(vCommandEnd, s2) - 1));
          Delete(s2, 1, Pos(vCommandEnd, s2) + Length(vCommandEnd) - 1);
          Mouse_Event(MOUSEEVENTF_WHEEL, MousePosXNew, MousePosYNew,
            DWORD(MousePosWheel), 0);
          Continue;
        End;
        If (Pos('<|SETMOUSEPOS|>', s3) > 0) Then
        Begin
          Delete(s2, 1, Pos('<|SETMOUSEPOS|>', s2) + 14);
          Try
            MousePosXNew := StrToInt(Copy(s2, 1, Pos('<|>', s2) - 1));
            Delete(s2, 1, Pos('<|>', s2) + 2);
            MousePosYNew := StrToInt(Copy(s2, 1, Pos(vCommandEnd, s2) - 1));
            // s2           := Copy(s2, Pos(vCommandEnd, s2) + Length(vCommandEnd), Length(s2));
            Delete(s2, 1, Pos(vCommandEnd, s2) + Length(vCommandEnd) - 1);
            If Not((MousePosX = MousePosXNew) And
              (MousePosY = MousePosYNew)) Then
            Begin
              MousePosX := MousePosXNew;
              MousePosY := MousePosYNew;
              Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MOVE,
                MousePosX * (65535 div Screen.Width),
                MousePosY * (65535 div Screen.Height), 0, 0);
              Continue;
            End;
          Except
          End;
        End;
        // Combo Keys
        If (Pos('<|ALTDOWN|>', s2) > 0) Then
        Begin
          Delete(s2, 1, Pos('<|ALTDOWN|>', s2) + 14);
          Delete(s2, 1, Pos(vCommandEnd, s2) + Length(vCommandEnd) - 1);
          If Not(AltPressed) Then
          Begin
            keybd_event(18, 0, 0, 0);
            AltPressed := True;
          End;
        End;
        If (Pos('<|ALTUP|>', s2) > 0) Then
        Begin
          Delete(s2, 1, Pos('<|ALTUP|>', s2) + 14);
          Delete(s2, 1, Pos(vCommandEnd, s2) + Length(vCommandEnd) - 1);
          If AltPressed Then
          Begin
            keybd_event(18, 0, KEYEVENTF_KEYUP, 0);
            AltPressed := False;
          End;
        End;
        If (Pos('<|CTRLDOWN|>', s2) > 0) Then
        Begin
          Delete(s2, 1, Pos('<|CTRLDOWN|>', s2) + 14);
          Delete(s2, 1, Pos(vCommandEnd, s2) + Length(vCommandEnd) - 1);
          If Not(CtrlPressed) Then
          Begin
            keybd_event(17, 0, 0, 0);
            CtrlPressed := True;
          End;
        End;
        If (Pos('<|CTRLUP|>', s2) > 0) Then
        Begin
          Delete(s2, 1, Pos('<|CTRLUP|>', s2) + 14);
          Delete(s2, 1, Pos(vCommandEnd, s2) + Length(vCommandEnd) - 1);
          If (CtrlPressed) Then
          Begin
            keybd_event(17, 0, KEYEVENTF_KEYUP, 0);
            CtrlPressed := False;
          End;
        End;
        If (Pos('<|SHIFTDOWN|>', s2) > 0) Then
        Begin
          Delete(s2, 1, Pos('<|SHIFTDOWN|>', s2) + 14);
          Delete(s2, 1, Pos(vCommandEnd, s2) + Length(vCommandEnd) - 1);
          If Not(ShiftPressed) Then
          Begin
            keybd_event(16, 0, 0, 0);
            ShiftPressed := True;
          End;
        End;
        If (Pos('<|SHIFTUP|>', s2) > 0) Then
        Begin
          Delete(s2, 1, Pos('<|SHIFTUP|>', s2) + 14);
          Delete(s2, 1, Pos(vCommandEnd, s2) + Length(vCommandEnd) - 1);
          If ShiftPressed Then
          Begin
            keybd_event(16, 0, KEYEVENTF_KEYUP, 0);
            ShiftPressed := False;
          End;
        End;
        // Clipboard Remote
        If (Pos('<|CLIPBOARD|>', s3) > 0) Then
        Begin
          Delete(s2, 1, Pos('<|CLIPBOARD|>', s2) + 12);
          s2 := Copy(s2, 1, Pos(vCommandEnd, s2) - 1);
          Try
            Clipboard.Open;
            Clipboard.AsText := s2;
          Finally
            Clipboard.Close;
          End;
          s2 := '';
          S := s2;
        End;
        If (Pos('?', s2) > 0) Then
        Begin
          s2 := Copy(s2, Pos('?', s2), Length(s2));
          If (GetKeyState(VK_SHIFT) < 0) Then
          Begin
            keybd_event(16, 0, KEYEVENTF_KEYUP, 0);
            SendKeys(PWideChar(s2), False);
            keybd_event(16, 0, 0, 0);
          End;
          Delete(s2, Pos('?', s2), Pos(vCommandEnd, s2) +
            Length(vCommandEnd) - 1);
        End;
      End;
      If Not(PartialCommands(s2)) Then
      Begin
        // s2 := Copy(s2, 1, Pos(vCommandEnd, s2) -1);
        s2 := StringReplace(s2, vCommandEnd, '', [rfReplaceAll]);
        If (s2 <> '') Then
        Begin
          SendKeys(PWideChar(s2), False);
          // Delete(s2, 1, Pos(vCommandEnd, s2) + Length(vCommandEnd) -1);
          s2 := '';
        End;
      End;
    End;
  Except
  End;
End;

Procedure Tfrm_Main.DeactiveComponents;
Begin
  If vInitImageCapture <> Nil Then
    vInitImageCapture.ImageBase := Nil;
  If ipPSDeskTopClient <> Nil Then
  Begin
    Try
      ipPSDeskTopClient.Active := False;
    Except
    End;
  End;
  If ipCommandsClient <> Nil Then
  Begin
    Try
      ipCommandsClient.Active := False;
    Except
    End;
  End;
  If ipPSFilesClient <> Nil Then
  Begin
    Try
      ipPSFilesClient.Active := False;
    Except
    End;
  End;
  If ipPSMain_Socket <> Nil Then
  Begin
    Try
      ipPSMain_Socket.Active := False;
    Except
    End;
  End;
End;

Procedure Tfrm_Main.DestroyComponents;
Begin
  vInitString := '';
  SendResolution := False;
  vNewFrameClient := False;
  If vInitImageCapture <> Nil Then
    vInitImageCapture.ImageBase := Nil;
  If ipPSDeskTopClient <> Nil Then
  Begin
    Try
      ipPSDeskTopClient.Active := False;
      // ipPSDeskTopClient.TCPObject := Nil;
      FreeAndNil(ipPSDeskTopClient);
    Except
    End;
  End;
  If ipCommandsClient <> Nil Then
  Begin
    Try
      ipCommandsClient.Active := False;
      // ipPSFilesClient.TCPObject := Nil;
      FreeAndNil(ipCommandsClient);
    Except
    End;
  End;
  If ipPSFilesClient <> Nil Then
  Begin
    Try
      ipPSFilesClient.Active := False;
      // ipPSFilesClient.TCPObject := Nil;
      FreeAndNil(ipPSFilesClient);
    Except
    End;
  End;
  If ipPSMain_Socket <> Nil Then
  Begin
    Try
      ipPSMain_Socket.Active := False;
      // ipPSMain_Socket.TCPObject := Nil;
      // FreeAndNil(ippMain_Socket);
    Except
    End;
  End;
End;

Procedure Tfrm_Main.CreateComponents;
Begin
  vNewFrameClient := False;
  Try
    If ipPSDeskTopClient = Nil Then
      ipPSDeskTopClient := TUDPSuperClient.Create(Self);
    If ipPSFilesClient = Nil Then
      ipPSFilesClient := TUDPSuperClient.Create(Self);
    If ipCommandsClient = Nil Then
      ipCommandsClient := TUDPSuperClient.Create(Self);
    vSendType := DefaultAction;
    vWhereNew := DefaultAction = stProxy;
    If ipPSFilesClient <> Nil Then
    Begin
      ipPSFilesClient.BufferSize := 2048;
      ipPSFilesClient.OnGetData := DataInFiles;
      ipPSFilesClient.OnPeerConnected := OnPeerConnected;
      // Adicionado para Teste de Proxy via UDP
      ipPSFilesClient.SendType := vSendType;
      // ipPSFilesClient.OnPeerConTimeOut       := OnPeerConTimeOut;
      ipPSFilesClient.PeerConnectionTimeOut := TProxyTimeOutNAT;
    End;
    If ipPSDeskTopClient <> Nil Then
    Begin
      ipPSDeskTopClient.BufferSize := MaxBuffer;
      ipPSDeskTopClient.OnGetData := DataIn;
      ipPSDeskTopClient.OnGetLongString := OnGetLongString;
      ipPSDeskTopClient.OnBinaryIn := OnBinaryIn;
      ipPSDeskTopClient.OnPeerConnected := OnPeerConnectedFilesClient;
      // Adicionado para Teste de Proxy via UDP
      ipPSDeskTopClient.SendType := vSendType;
      // ipPSDeskTopClient.OnPeerConTimeOut      := OnPeerConTimeOut;
      ipPSDeskTopClient.PeerConnectionTimeOut := TProxyTimeOutNAT;
    End;
    If ipCommandsClient <> Nil Then
    Begin
      ipCommandsClient.BufferSize := 1024;
      ipCommandsClient.OnGetData := DataIn;
      ipCommandsClient.OnPeerConnected := OnPeerConnectedCommandsClient;
      // Adicionado para Teste de Proxy via UDP
      ipCommandsClient.SendType := vSendType;
      ipCommandsClient.PeerConnectionTimeOut := TProxyTimeOutNAT;
      ipCommandsClient.OnPeerConTimeOut := OnPeerConTimeOut;
    End;
    If ipPSMain_Socket = Nil Then
    Begin
      ipPSMain_Socket := TipPoolerService.Create(Self);
      If ippMain_Socket = Nil Then
        ippMain_Socket := TIdTCPClient.Create(Nil);
      ipPSMain_Socket.TCPObject := ippMain_Socket;
      ipPSMain_Socket.OnDisconnectedClient := Main_SocketDisconnected;
      ipPSMain_Socket.OnErrorClient := Main_SocketError;
      ipPSMain_Socket.OnDataInClient := Main_SocketDataIn;
      ipPSMain_Socket.OnReadyToSendClient := Main_SocketReadyToSendClient;
      ipPSMain_Socket.Timeout := TimeOutC;
      ipPSMain_Socket.BufferSize := TMaxBufferTCP;
      ipPSMain_Socket.LineBreak := vEOB;
    End;
    SetHostPortGroupMach;
  Except
  End;
End;

procedure Tfrm_Main.lblstatusconexaoClick(Sender: TObject);
begin
  Botao_conectar_parceiro;
end;

Procedure Tfrm_Main.LoadComboOptions;
Var
  I: Integer;
Begin
  For I := 0 to vComboList.Elements.Count - 1 Do
    TargetID_MaskEdit.Items.Add(vComboList.Elements[I]);
  TargetID_MaskEdit.Text := vComboList.GetLastAccess;
End;

procedure Tfrm_Main.FormCreate(Sender: TObject);
begin
  // SimpleTimer          := TSimpleTimer.Create;
  AdicionaFirewall;
  vInitCapture := False;
  vInitImageCapture := TImageCapture.Create(Self);
  vInitImageCapture.Colums := Cols;
  vInitImageCapture.Rows := Rows;
  // SimpleTimer.OnTimer  := CaptureExecute;
  // SimpleTimer.Interval := 1;
  CritialSection := TCriticalSection.Create;
  vCaptureScreens := TListitems.Create;

  // carrega as configuações do arquivo ini
  LoadConfigs;

  vCloseConnection := True;
  vCommandEnd := '<$COMMANDEND$>';
  vMouseCapture := MouseCaptureC;
  vComboList := TComboList.Create(Self);
  LoadComboOptions;
  NewFrame := 0;
  FirstExecute := True;
  // Insert version on Caption of the Form
  Caption := Caption + ' - ' + GetAppVersionStr;

  // salva a versão do aplicativo no ini
  SaveIni('version', Caption, ExtractFilePath(Application.ExeName) + 'Aegys.ini',
    cGeneral, False);

  If (ParamCount > 0) Then
  Begin
    vParID := ParamStr(1);
    vParSenha := ParamStr(2);
  End;

  ippMain_Socket := Nil;
  CreateComponents;
  Reconnect_Timer.Enabled := False;


  vMAC := MacAddress;
  vHD := SerialNumHardDisk(SystemDrive);
  LastPassWord := ''; // inicializa ultima senha em branco
  LastPassWordClient := '';
  Proxy := False;
  If Proxy Then
  Begin
    HostProxy := GetIni(ExtractFilePath(Application.ExeName) + 'Aegys.ini', cGeneral, 'hostproxy', cHostProxy, True);
    PortProxy := StrToInt(GetIni(ExtractFilePath(Application.ExeName) + 'Aegys.ini', cGeneral, 'portproxy', cPortProxy, False));
  End
  Else
  Begin
    HostProxy := '';
    PortProxy := 0;
  End;
  SetLanguage;
  SetHostPortGroupMach;
  ResolutionTargetWidth := 0;
  ResolutionTargetHeight := 0;
  SetOffline;
  mmAtual.Visible := False;
  mmNova.Visible := False;
  If HabLogs Then
    vLogsList := TStringList.Create;
  vReceiveData := TProcessData.Create(Self);
  vReceiveData.OwnerForm := frm_RemoteScreen;
  vReceiveData.ExecFunction := GetStreamS;
  vReceiveData.Processmessages := True;
  vSendData := TProcessData.Create(Self);
  vSendData.OwnerForm := frm_Main;
  vSendData.ExecFunction := SendStreamS;
  vSendData.Processmessages := False;
end;

procedure Tfrm_Main.FormKeyDown(Sender: TObject; var Key: Word;
Shift: TShiftState);
begin
  If (Key = VK_F7) Then
  Begin
    Reconnect_Timer.Enabled := False;
    ipPSMain_Socket.Active := False;
    frm_Config := Tfrm_Config.Create(Self);
    frm_Config.ShowModal;
    frm_Config.DisposeOf;
  End;
end;

procedure Tfrm_Main.FormShow(Sender: TObject);
var
  Computer: PChar;
  function GetComputerNameFunc: string;
  var
    ipbuffer: string;
    nsize: DWORD;
  begin
    nsize := 255;
    SetLength(ipbuffer, nsize);
    if GetComputerName(PChar(ipbuffer), nsize) then
      Result := ipbuffer;
  end;

begin
  If InCreate Then
  Begin
    If dmCaptureScreen = Nil Then
      dmCaptureScreen := TdmCaptureScreen.Create(Self);
    If Not(Diferencial) Then
      dmCaptureScreen.MaxNewFrame := MaxNewFrame * 100
    Else
      dmCaptureScreen.MaxNewFrame := MaxNewFrame;
    Reconnect_Timer.Enabled := InCreate;
    InCreate := False;
  End;

  lbl_mypc.Caption := GetComputerNameFunc;
  cbQualidade.ItemIndex := 3;
end;

procedure Tfrm_Main.mniCloseClick(Sender: TObject);
begin
  CloseAplication;
end;

procedure Tfrm_Main.mniConfigClick(Sender: TObject);
begin
  Reconnect_Timer.Enabled := False;
  frm_Config := Tfrm_Config.Create(Self);
  frm_Config.ShowModal;
  frm_Config.DisposeOf;
  Reconnect_Timer.Enabled := True;
end;

procedure Tfrm_Main.mniMinimiserClick(Sender: TObject);
begin
  HideApplication;
end;

procedure Tfrm_Main.mniShowClick(Sender: TObject);
begin
  ShowApplication;
end;

procedure Tfrm_Main.Reconnect_TimerTimer(Sender: TObject);
Begin
  Reconnect_Timer.Enabled := False;
  // Reconnect Sockets
  Reconnect;
  If (Not(InClose)) Then
    Reconnect_Timer.Enabled := True;
End;

procedure Tfrm_Main.TargetID_MaskEditKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Botao_conectar_parceiro;
  end;
end;

procedure Tfrm_Main.TargetID_MaskEditxChange(Sender: TObject);
{
  Var
  formato : String;
}
begin
  {
    TargetID_MaskEdit.OnChange := Nil;
    Try
    If TargetID_MaskEdit.Text = '' Then
    TargetID_MaskEdit.Text := '  -   -   '
    Else
    Begin
    formato                    := StringReplace(TargetID_MaskEdit.Text, '-', '', [rfReplaceAll, rfIgnoreCase]);
    formato                    := Trim(formato);
    TargetID_MaskEdit.Text     := MaskDoFormatText(mascara, formato, #0);
    If TargetID_MaskEdit.Text[Length(TargetID_MaskEdit.Text)] = '-' Then
    TargetID_MaskEdit.Text    := Copy(TargetID_MaskEdit.Text, 1, Length(TargetID_MaskEdit.Text) - 1);
    TargetID_MaskEdit.SelStart := Length(TargetID_MaskEdit.Text);
    End;
    Finally
    TargetID_MaskEdit.OnChange  := TargetID_MaskEditChange;
    TargetID_MaskEdit.ItemIndex := -1;
    End;
  }
end;

procedure Tfrm_Main.TicServerDblClick(Sender: TObject);
begin
  ShowApplication;
end;

procedure Tfrm_Main.TimeoutActionTimer(Sender: TObject);
begin
  TimeoutAction.Enabled := False;
  OnPeerConTimeOut('', '', 0);
end;

procedure Tfrm_Main.Timeout_TimerTimer(Sender: TObject);
Var
  PeerConnected: TPeerconnected;
begin
  Timeout_Timer.Enabled := False;
  If (Not(InClose)) Then
  Begin
    If (Timeout > ConnectionTimeout) then
    Begin
      If frm_RemoteScreen <> Nil Then
      Begin
        If (frm_RemoteScreen.Visible) then
          frm_RemoteScreen.Close
        Else
        Begin
          frm_Main.SetOffline;
          frm_Main.CloseSockets;
          frm_Main.Reconnect;
        End;
      End;
    End
    Else
      Inc(Timeout);
    If Not(inTimerCollect) Then
    Begin
      If vCaptureSideClient Then
      Begin
        If (System.DateUtils.MilliSecondsBetween(Now, LastFrameTime) >
          TimeOutF) Then
        Begin
          LastFrameTime := Now;
          PeerConnected := ipPSDeskTopClient.GetActivePeer;
          If PeerConnected <> Nil Then
            ipPSDeskTopClient.SendRawString
              (ipPSDeskTopClient.GetIpSend(PeerConnected), PeerConnected.Port,
              '<|GETFULLSCREENSHOT|>NEW' + vCommandEnd, False, 5);
          // SendBuffer(ipPSDeskTopClient.GetIpSend(PeerConnected), PeerConnected.Port, '<|GETFULLSCREENSHOT|>NEW' + vCommandEnd, False, dtt_Async);
          vLinhaKeys := '';
        End;
      End;
    End;
    If (Not(InClose)) Then
    Begin
      If ipPSMain_Socket.Active Then
      Begin
        If ((YourID_Edit.Text <> MyID) And (MyID <> '') And (Not(vSendMyData)))
          Or ((MyID = '') And (Not(vSendMyData))) Then
        Begin
          vSendMyData := True;
          ipPSMain_Socket.Write('<|MAINSOCKET|>' + '<|GROUP|>' + vGroup + '<|>'
            + '<|MACHINE|>' + vMachine + '<|>' + '<|MAC|>' + vMAC + '<|>' +
            '<|HD|>' + vHD + '<|>' + '<|LASTPASSWORD|>' + LastPassWord +
            vCommandEnd);
        End;
      End;
    End;
  End;
{$IFDEF MSWINDOWS}
{$IFNDEF FMX}Application.Processmessages;
{$ELSE}FMX.Forms.TApplication.Processmessages; {$ENDIF}
{$ENDIF}
  If (Not(InClose)) Then
    Timeout_Timer.Enabled := True;
end;

procedure Tfrm_Main.Time_UpdateTimer(Sender: TObject);
begin
  Time_Update.Enabled := False;
  URL := GetIni(ExtractFilePath(Application.ExeName) + 'Aegys.ini', cGeneral, 'urlupdates', cUrlUpdates, False);
  If ((URL <> '0') And (URL <> '')) Then
    checksUpdates;
end;

Procedure GetStreamF(InputClient: TipPoolerService;
Var StreamValue: TFileStream);
Var
  sStream, DestStream: TMemoryStream;
  aFileSize, aPosition, aCount: LongInt;
  vLineString: String;
  Function ReadString(const Stream: TStream): AnsiString;
  Var
    n: LongInt;
  Begin
    Stream.Seek(0, soFromBeginning);
    n := Stream.Size;
    If n > 0 Then
    Begin
      SetLength(Result, n);
      Stream.Read(Result[1], n);
    End;
  End;
  Procedure WriteString(const Stream: TStream; const S: AnsiString);
  Var
    n: LongInt;
  Begin
    n := Length(S);
    If n > 0 Then
      Stream.Write(S[1], n);
  End;

Begin
  aCount := StrToInt(InputClient.ReadLn);
  StreamValue.Position := 0;
  aPosition := 0;
  aFileSize := aCount;
  While (aCount > 0) Do
  Begin
    Try
      sStream := TMemoryStream.Create;
      DestStream := TMemoryStream.Create;
      InputClient.Read(sStream);
      DeCompressStream(sStream, DestStream);
      aPosition := aPosition + DestStream.Size;
      vLineString := vLineString + ReadString(DestStream);
      aCount := StrToInt(InputClient.ReadLn);
    Finally
      sStream.DisposeOf;
      DestStream.DisposeOf;
    End;
    If frm_ShareFiles <> Nil Then
    Begin
      If Not(vCancelOPSendFile) Then
      Begin
        frm_ShareFiles.Download_ProgressBar.Max := aFileSize;
        frm_ShareFiles.Download_ProgressBar.Position := aPosition;
        frm_ShareFiles.SizeDownload_Label.Caption := 'Tamanho: ' +
          frm_Main.GetSize(aPosition) + ' / ' + frm_Main.GetSize(aFileSize);
        // Application.ProcessMessages;
      End;
    End;
    If frm_RemoteScreen <> Nil Then
    Begin
      If Not(vCancelOPSendFile) Then
      Begin
        frm_RemoteScreen.pbDados.Max := aFileSize;
        frm_RemoteScreen.pbDados.Position := aPosition;
        frm_RemoteScreen.rzpDownload.Visible := True;
        If frm_ShareFiles <> Nil Then
          frm_ShareFiles.FreeForClose :=
            Not(frm_RemoteScreen.rzpDownload.Visible);
      End;
    End;
  End;
  FileStreamFromBase64(vLineString, StreamValue);
End;

Procedure GetStreamF(InputClient: TIdTCPClient; Var StreamValue: TFileStream;
FileSize: LongInt = 0);
Var
  sStream, DestStream: TMemoryStream;
  aFileSize, aPosition, aCount: LongInt;
  vLineString: String;
  Function ReadString(const Stream: TStream): AnsiString;
  Var
    n: LongInt;
  Begin
    Stream.Seek(0, soFromBeginning);
    n := Stream.Size;
    If n > 0 Then
    Begin
      SetLength(Result, n);
      Stream.Read(Result[1], n);
    End;
  End;
  Procedure WriteString(const Stream: TStream; const S: AnsiString);
  Var
    n: LongInt;
  Begin
    n := Length(S);
    If n > 0 Then
      Stream.Write(S[1], n);
  End;

Begin
  aCount := StrToInt(InputClient.IOHandler.ReadLn);
  StreamValue.Position := 0;
  aPosition := 0;
  aFileSize := FileSize;
  If aFileSize = 0 Then
    aFileSize := aCount;
  While (aCount > 0) Do
  Begin
    sStream := TMemoryStream.Create;
    DestStream := TMemoryStream.Create;
    Try
      InputClient.IOHandler.ReadStream(sStream, aCount, False);
      DeCompressStream(sStream, DestStream);
      aPosition := aPosition + DestStream.Size;
      vLineString := vLineString + ReadString(DestStream);
      aCount := StrToInt(InputClient.IOHandler.ReadLn);
    Finally
      sStream.DisposeOf;
      DestStream.DisposeOf;
    End;
    If frm_ShareFiles <> Nil Then
    Begin
      If Not(vCancelOPSendFile) Then
      Begin
        frm_ShareFiles.Download_ProgressBar.Max := aFileSize;
        frm_ShareFiles.Download_ProgressBar.Position := aPosition;
        frm_ShareFiles.SizeDownload_Label.Caption := 'Tamanho: ' +
          frm_Main.GetSize(aPosition) + ' / ' + frm_Main.GetSize(aFileSize);
        // Application.ProcessMessages;
      End;
    End;
    If frm_RemoteScreen <> Nil Then
    Begin
      If Not(vCancelOPSendFile) Then
      Begin
        frm_RemoteScreen.pbDados.Max := aFileSize;
        frm_RemoteScreen.pbDados.Position := aPosition;
        frm_RemoteScreen.rzpDownload.Visible := True;
        If frm_ShareFiles <> Nil Then
          frm_ShareFiles.FreeForClose :=
            Not(frm_RemoteScreen.rzpDownload.Visible);
      End;
    End;
  End;
  FileStreamFromBase64(vLineString, StreamValue);
End;

Procedure SendStreamF(OutputClient: TUDPSuperClient; FileStream: TFileStream;
Buffered: Boolean = False);
Var
  vSendBuffer, vString: String;
  iBufferLength, aPosition, iPos, iCount, aCount: LongInt;
  PeerConnected: TPeerconnected;
  B: Array of Byte absolute vString;
  Procedure WriteString(const Stream: TStream; const S: AnsiString);
  Var
    n: LongInt;
  Begin
    n := Length(S);
    If n > 0 Then
      Stream.Write(S[1], n);
  End;

Begin
  vString := Base64FromFileStream(FileStream);
  // CompressStreamS(vString, vCompressStream);
  // vString    := vCompressStream;
  aCount := Length(vString);
  iCount := 0;
  iPos := 1;
  aPosition := 0;
  vCancelOPSendFile := False;
  vStopSendFile := False;
  If (aCount > 0) And (iCount < aCount) Then
  Begin
    PeerConnected := OutputClient.GetActivePeer;
    If PeerConnected <> Nil Then
      OutputClient.SendBuffer(OutputClient.GetIpSend(PeerConnected),
        PeerConnected.Port, '<|$initstream$|>' + IntToStr(aCount) +
        frm_Main.vCommandEnd);
    While (aCount > 0) And (iCount < aCount) Do
    Begin
      Try
        If aCount = iCount Then
          Break;
        if aCount < (iCount + OutputClient.BufferSize) Then
          iBufferLength := (aCount - iCount)
        Else
          iBufferLength := OutputClient.BufferSize;
        If Buffered Then
        Begin
          iCount := iCount + iBufferLength;
          vSendBuffer := Copy(vString, iPos, iBufferLength);
        End
        Else
        Begin
          iCount := aCount;
          vSendBuffer := vString;
        End;
        iPos := iPos + iBufferLength;
        If vStopSendFile Then
        Begin
          vCancelOPSendFile := True;
          frm_Main.ipPSMain_Socket.
            Write('<|REDIRECT|><|RESETTRANSF|>' + frm_Main.CommandEnd);
          Exit;
        End;
        If vStopSendFile Then
        Begin
          vCancelOPSendFile := True;
          frm_Main.ipPSMain_Socket.
            Write('<|REDIRECT|><|RESETTRANSF|>' + frm_Main.CommandEnd);
          Exit;
        End;
        OutputClient.SendBuffer(OutputClient.GetIpSend(PeerConnected),
          PeerConnected.Port, '<|>SENDBUFFER<|>' + vSendBuffer +
          frm_Main.vCommandEnd);
        Application.Processmessages;
        aPosition := aPosition + Length(vSendBuffer);
        If (frm_ShareFiles <> Nil) And (frm_Main.SendingFile) Then
        Begin
          If Not(vCancelOPSendFile) Then
          Begin
            frm_ShareFiles.Upload_ProgressBar.Max := aCount;
            frm_ShareFiles.Upload_ProgressBar.Position := aPosition;
            frm_ShareFiles.SizeUpload_Label.Caption := 'Tamanho: ' +
              frm_Main.GetSize(aPosition) + ' / ' + frm_Main.GetSize(aCount);
          End;
        End;
        If frm_RemoteScreen <> Nil then
        Begin
          If Not(vCancelOPSendFile) Then
          Begin
            frm_RemoteScreen.rzpDownload.Visible := True;
            If frm_ShareFiles <> Nil Then
              frm_ShareFiles.FreeForClose :=
                Not(frm_RemoteScreen.rzpDownload.Visible);
            frm_RemoteScreen.pbDados.Max := aCount;
            frm_RemoteScreen.pbDados.Position := aPosition;
          End;
        End;
      Except
        Break;
      End;
    End;
    If vStopSendFile Then
    Begin
      vCancelOPSendFile := True;
      frm_Main.ipPSMain_Socket.
        Write('<|REDIRECT|><|RESETTRANSF|>' + frm_Main.CommandEnd);
      Exit;
    End;
    OutputClient.SendBuffer(OutputClient.GetIpSend(PeerConnected),
      PeerConnected.Port, '<|>BUFFEREND<|>' + frm_Main.vCommandEnd);
    If (frm_ShareFiles <> Nil) Then
    Begin
      If frm_Main.SendingFile then
      Begin
        frm_ShareFiles.Upload_ProgressBar.Max := 0;
        frm_ShareFiles.Upload_ProgressBar.Position := 0;
        frm_ShareFiles.SizeUpload_Label.Caption := 'Tam: ' + frm_Main.GetSize(0)
          + ' / ' + frm_Main.GetSize(0);
      End;
      If frm_RemoteScreen <> Nil then
      Begin
        frm_RemoteScreen.rzpDownload.Visible := False;
        If frm_ShareFiles <> Nil Then
          frm_ShareFiles.FreeForClose :=
            Not(frm_RemoteScreen.rzpDownload.Visible);
        frm_RemoteScreen.pbDados.Max := 0;
        frm_RemoteScreen.pbDados.Position := 0;
      End;
      frm_Main.SendingFile := False;
      // frm_Main.ipPSFilesClient.Write('<|GETFOLDERS|>' + frm_ShareFiles.Directory_Edit + frm_Main.vCommandEnd);
      frm_ShareFiles.Upload_BitBtn.Enabled := True;
      frm_ShareFiles.Download_BitBtn.Enabled := True;
      Application.Processmessages;
    End;
  End;
  vCancelOPSendFile := False;
  vStopSendFile := vCancelOPSendFile;
End;

Function ResumeStreamASM(Const S, d: Pointer; Var c: Pointer): Integer;
  Assembler;
Var
  src: ^Char;
  dest: ^Char;
  n1, n2: Cardinal;
Begin
  Asm
    mov muASM, 0
    mov pdst, ECX              // Move resolutado pra PDST
    mov src, EAX               // Move S pra src
    mov dest, EDX              // Move D pra dest
    call System.@LStrLen       // Tamanho de string S
    mov n1, EAX                // Move tamanho do S para n1
    mov EAX, dest              // Move dest para EAX
    call System.@LStrLen       // Tamanho do dst/D
    mov n2, EAX                // Move Tamanho D para n2
    mov EDX, EAX               // Move tamanho D para EDX segundo parametro setlenght
    mov EAX, pdst              // Move Result/pdst para EAX primeiro parametro strlenght
    call System.@LStrSetLength // Seta parametro pdst para tamanho n2
    mov ECX, ASMSize           // Mov n2 para ECX para controlar loopings
    test ECX, ECX              // Testa ECX
    jz @@end                   // Se EXX = 0 Termina
    push ESI                   // Guarda ESI na pilha
    push EDI
    mov EAX, pdst              // EAX := pdst; //Endereço da string de resultado
    mov ESI, src               // ESI := src; //String de origem
    mov EDI, dest
    mov EDX, [EAX]             // EDX := pdst^; //String de resultado
  @@cycle:
    mov AL, [EDI]             // Move um caracter do primeiro stream para AL
    cmp AL, '0'               // Copara se o caracter é 0 no segundo stream
    jne @@diferente           // Se for Diferente pula para igual
    mov AL, [ESI]             // Se defente copia Caracter do Segund stream para AL
    mov [EDX], AL             // Coloca caracter no terceiro stream
    mov muASM, 1
    cmp AL, AL                // Apenas para gerra um Je
    je @@incremento           // Incrementa caracter
  @@diferente:
    mov AL, [EDI]             // Se for <> Coloca '0' em AL
    mov [EDX], AL             // Move o caracter correto para terceiro Stream
  @@incremento:
    inc ESI
    inc EDI
    inc EDX
    dec ECX
    cmp ECX, 0
    ja @@cycle
    pop EDI
    pop ESI                   // Recupera ESI na pilha
  @@end:
  End;
  Result := muASM;
End;

Procedure ResumeStreamS(FirstStream: TMemoryStream;
Var SecondStream: TMemoryStream; CompareStream: TMemoryStream); Overload;
Var
  MyFirstStream, MySecondStream, MyCompareStream: TStringStream;
  P1, P2, P3: Pointer;
  vSource, vDest, vCompared: TBitmap;
Begin
  If CompareFromDelphi Then
  Begin
    MyFirstStream := TStringStream.Create;
    CompareStream.Position := 0;
    MyFirstStream.CopyFrom(CompareStream, CompareStream.Size);
    MySecondStream := TStringStream.Create;
    FirstStream.Position := 0;
    MySecondStream.CopyFrom(FirstStream, FirstStream.Size);
    MyCompareStream := TStringStream.Create;
    SecondStream := TMemoryStream.Create;
    vSource := TBitmap.Create;
    vDest := TBitmap.Create;
    vCompared := TBitmap.Create;
    Try
      vSource.LoadFromStream(MyFirstStream);
      vDest.LoadFromStream(MySecondStream);
      RecoveryComparer(vSource, vDest, vCompared);
      // , TPixelFormat(ImageViewQ));
      vCompared.SaveToStream(MyCompareStream);
      MyCompareStream.Position := 0;
      SecondStream.CopyFrom(MyCompareStream, MyCompareStream.Size);
    Finally
      MyFirstStream.Free;
      MySecondStream.Free;
      MyCompareStream.Free;
      vSource.Free;
      vDest.Free;
      vCompared.Free;
    End;
  End
  Else
  Begin
    MyFirstStream := TStringStream.Create;
    FirstStream.Position := 0;
    MyFirstStream.CopyFrom(FirstStream, FirstStream.Size);
    MySecondStream := TStringStream.Create('');
    MyCompareStream := TStringStream.Create;
    SecondStream := TMemoryStream.Create;
    CompareStream.Position := 0;
    MyCompareStream.CopyFrom(CompareStream, CompareStream.Size);
    Try
      MyFirstStream.Position := 0;
      MySecondStream.Clear;
      If MyFirstStream.Size <> MyCompareStream.Size Then
        MyFirstStream.SetSize(MyCompareStream.Size);
      MyFirstStream.Position := 0;
      P1 := MyFirstStream.Memory;
      P2 := MyCompareStream.Memory;
      P3 := MySecondStream.Memory;
      muASM := 0;
      ASMSize := MyCompareStream.Size;
      If ResumeStreamASM(P1, P2, P3) <> 0 Then
      Begin
        MySecondStream.Clear;
        MySecondStream.Write(P3^, MyCompareStream.Size);
        MySecondStream.Position := 0;
        SecondStream.CopyFrom(MySecondStream, MySecondStream.Size);
        MyFirstStream.Clear;
        MyFirstStream.CopyFrom(MyCompareStream, 0);
      End;
    Finally
      MyFirstStream.Free;
      MySecondStream.Free;
      MyCompareStream.Free;
    End;
    Asm
      mov EDX, 0                 // Move tamanho D para EDX segundo parametro setlenght
      mov EAX, pdst              // Move Result/pdst para EAX primeiro para metro strlenght
      call System.@LStrSetLength // Seta parametro pdst para tamanho n2
    End;
  End;
End;

Procedure ResumeStreamS(FirstStream: String; Var SecondStream: String;
CompareStream: String); Overload;
Var
  MyFirstStream, MySecondStream, MyCompareStream: TStringStream;
  P1, P2, P3: Pointer;
  vSource, vDest, vCompared: TBitmap;
Begin
  If CompareFromDelphi Then
  Begin
    MyFirstStream := TStringStream.Create(CompareStream);
    MySecondStream := TStringStream.Create(FirstStream);
    MyCompareStream := TStringStream.Create;
    vSource := TBitmap.Create;
    vDest := TBitmap.Create;
    vCompared := TBitmap.Create;
    Try
      vSource.LoadFromStream(MyFirstStream);
      vDest.LoadFromStream(MySecondStream);
      RecoveryComparer(vSource, vDest, vCompared);
      // , TPixelFormat(ImageViewQ));
      vCompared.SaveToStream(MyCompareStream);
      MyCompareStream.Position := 0;
      SecondStream := MyCompareStream.DataString;
    Finally
      MyFirstStream.Free;
      MySecondStream.Free;
      MyCompareStream.Free;
      vSource.Free;
      vDest.Free;
      vCompared.Free;
    End;
  End
  Else
  Begin
    MyFirstStream := TStringStream.Create(FirstStream);
    MySecondStream := TStringStream.Create('');
    MyCompareStream := TStringStream.Create(CompareStream);
    Try
      MyFirstStream.Position := 0;
      MySecondStream.Clear;
      If MyFirstStream.Size <> MyCompareStream.Size Then
        MyFirstStream.SetSize(MyCompareStream.Size);
      MyFirstStream.Position := 0;
      P1 := MyFirstStream.Memory;
      P2 := MyCompareStream.Memory;
      P3 := MySecondStream.Memory;
      muASM := 0;
      ASMSize := MyCompareStream.Size;
      If ResumeStreamASM(P1, P2, P3) <> 0 Then
      Begin
        MySecondStream.Clear;
        MySecondStream.Write(P3^, MyCompareStream.Size);
        MySecondStream.Position := 0;
        SecondStream := MySecondStream.DataString;
        MyFirstStream.Clear;
        MyFirstStream.CopyFrom(MyCompareStream, 0);
      End;
    Finally
      MyFirstStream.Free;
      MySecondStream.Free;
      MyCompareStream.Free;
    End;
    Asm
      mov EDX, 0                 // Move tamanho D para EDX segundo parametro setlenght
      mov EAX, pdst              // Move Result/pdst para EAX primeiro para metro strlenght
      call System.@LStrSetLength // Seta parametro pdst para tamanho n2
    End;
  End;
End;

Procedure SendStreamS(InputValue: String);
Var
  PeerConnected: TPeerconnected;
  vTempData: TIdBytes;
  vTempsString: String;
Begin
  vTempData := ZCompressStr(InputValue, ZCompressionLevel);
  If (Length(vTempData) > 0) Then
  Begin
    vTempsString := CompressionEncoding.GetString(TBytes(vTempData));
    // If ByPass Then
    // vTempData    := ZCompressStr(vTempData, ZCompressionLevel);
    PeerConnected := frm_Main.ipPSDeskTopClient.GetActivePeer;
    If PeerConnected <> Nil Then
      frm_Main.ipPSDeskTopClient.SendLongBuffer
        (frm_Main.ipPSDeskTopClient.GetIpSend(PeerConnected),
        PeerConnected.Port, Format(TLineSend, [Length(vTempsString),
        vTempsString]), False, dtt_Async);
  End;
End;

Procedure GetStreamS(InputValue: String);
Var
  vDecompressS, vString, vLineString, vStringSize, vImageState, vOldA,
    vResult: String;
  vDecompressOK, vNewFrame, vNoDiff: Boolean;
  vScreenWidth, vScreenHeight, vLeft, vTop, vWidth, vHeight, vFRResult: Integer;
  PeerConnected: TPeerconnected;
  Procedure StringToStream(Const S: String; Imagem: TImage;
                           NoDiff: Boolean; ImageState: String);
  Var
    vDados: TStringStream;
    Bitmap2: TBitmap;
    vCol, vRow: Integer;
    Block: TJDRMImageBlock;
  Begin
    vDados := TStringStream.Create(S, CompressionDecoding);
    if vDados.Size = 0 then
    Begin
      vDados.Free;
      Exit;
    End;
    Bitmap2 := Nil;
    frm_Main.InitCapture := True;
    Try
      vDados.Position := 0;
      If Not NewDeskCapture Then
      Begin
        If Not(NoDiff) Then
        Begin
          Bitmap2 := TBitmap.Create;
          Try
            Bitmap2.LoadFromStream(vDados);
            Imagem.Picture.Assign(Bitmap2);
          Except

          End;
           Bitmap2.FreeImage;
           Bitmap2.Free;
        End
        Else
        Begin
          Bitmap2 := TBitmap.Create;
          Bitmap2.LoadFromStream(vDados);
          Delete(ImageState, 1, Pos('=', ImageState));
          If (Pos('FULL', ImageState) > 0) Then
          Begin
            frm_Main.vInitImageCapture.ImageBase := Bitmap2;
            Imagem.Picture.Assign(Bitmap2);
          End
          Else
          Begin
            If Pos('SIZE=0', vStringSize) = 0 Then
            Begin
              vCol := StrToInt(Copy(ImageState, Pos('|', ImageState) + 1,
                (Pos(':', ImageState) - 1) - Pos('|', ImageState)));
              vRow := StrToInt(Copy(ImageState, Pos(':', ImageState) + 1,
                Length(ImageState)));
              // frm_Main.vInitImageCapture.GetAltImage(Bitmap, Bitmap2, vCol, vRow, True);
              Imagem.Picture.Assign(Bitmap2);
              frm_Main.vInitImageCapture.ImageBase := Bitmap2;
            End;
          End;
        End;
      End
      Else
      Begin
        Block := TJDRMImageBlock.Create(Nil);
        Try
          Block.Compression := 100;
          Block.ScreenWidth := vScreenWidth;
          Block.ScreenHeight := vScreenHeight;
          Block.Left := vLeft;
          Block.Top := vTop;
          Block.Width := vWidth;
          Block.Height := vHeight;
          Block.AsBitmap.LoadFromStream(vDados);
          If (frm_Main <> Nil) Then
          Begin
            frm_RemoteScreen.DesktopViewCapture.DrawBlock(Block);
            frm_RemoteScreen.DesktopViewCapture.UpdateImage;
          End;
        Finally
          Block.Free;
        End;
      End;
    Finally
      If vDados <> Nil Then
      Begin
        vDados.SetSize(0);
        FreeAndNil(vDados);
      End;
      If Not NewDeskCapture Then
      Begin
        If (Bitmap2 <> Nil) Then
        Begin
          // Bitmap2.FreeImage;
          FreeAndNil(Bitmap2);
        End;
      End;
    End;
  End;

Begin
  vFRResult := 0;
  Try
    If ShowFrameRate Then
      If frm_RemoteScreen <> Nil Then
      Begin
        vFRResult := MilliSecondsBetween(Now, vFrameRate);
        If vFRResult > 0 Then
        Begin
          vFRResult := 1000 div vFRResult;
          frm_RemoteScreen.Caption :=
            Format('Aegys - Computador Remoto, FPS : %d', [vFRResult]);
        End;
        If NewDeskCapture Then
          frm_RemoteScreen.DesktopViewCapture.Visible := True;
      End;
  Except

  End;
  While (InputValue <> '') Do
  Begin
    {
      PeerConnected := frm_Main.ipPSDeskTopClient.GetActivePeer;
      If PeerConnected <> Nil Then
      frm_Main.ipPSDeskTopClient.SendBuffer(frm_Main.ipPSDeskTopClient.GetIpSend(PeerConnected), PeerConnected.Port,
      TNewFrameData + frm_Main.CommandEnd);
    }
    If frm_Main.CloseConnection Then
      Exit;
    If Pos('<|$initstream$|>', InputValue) > 0 Then
    Begin
      vLineString := InputValue;
      InputValue := '';
      vNewFrame := Pos(TNewFrameData, vLineString) > 0;
      If vNewFrame Then
        vLineString := StringReplace(vLineString, TNewFrameData, '',
          [rfReplaceAll]);
      vString := Copy(vLineString, Pos('<|$initstream$|>', vLineString) + 19,
        Pos(frm_Main.vCommandEnd, vLineString) - 1);
      vNoDiff := False;
      If (Pos('IMAGETYPE=', vString) > 0) Then
      Begin
        vImageState := Copy(vString, 1, Pos('<|>', vString) - 1);
        Delete(vString, 1, Pos('<|>', vString) + 2);
        vNoDiff := True;
      End
      Else If (Pos('BLOCKDEFS=', vString) > 0) Then
      Begin
        vImageState := Copy(vString, 1, Pos('<|>', vString) - 1);
        Delete(vString, 1, Pos('<|>', vString) + 2);
        vImageState := StringReplace(vImageState, 'BLOCKDEFS=', '',
          [rfReplaceAll]);
        vLeft := StrToInt(Copy(vImageState, 2, Pos('|', vImageState) - 2));
        Delete(vImageState, 1, Pos('|', vImageState));
        vTop := StrToInt(Copy(vImageState, 2, Pos('|', vImageState) - 2));
        Delete(vImageState, 1, Pos('|', vImageState));
        vWidth := StrToInt(Copy(vImageState, 2, Pos('|', vImageState) - 2));
        Delete(vImageState, 1, Pos('|', vImageState));
        vHeight := StrToInt(Copy(vImageState, 2, Pos('|', vImageState) - 2));
        Delete(vImageState, 1, Pos('|', vImageState));
        vScreenWidth :=
          StrToInt(Copy(vImageState, 3, Pos('|', vImageState) - 3));
        Delete(vImageState, 1, Pos('|', vImageState));
        vScreenHeight := StrToInt(Copy(vImageState, 3, Length(vImageState)));
        Delete(vImageState, 1, Length(vImageState));
      End;
      vStringSize := Copy(vString, 0, Pos('<|>', vString) - 1);
      If (Trim(vStringSize) = '') Or (Trim(vStringSize) = 'SIZE=0') Then
        Exit;
      vString := Copy(vString, Pos('<|>', vString) + 3, Length(vString));
      If Pos(frm_Main.vCommandEnd, vString) > 0 Then
        vString := Copy(vString, 1, Pos(frm_Main.vCommandEnd, vString) - 1);
    End
    Else
    Begin
      vString := InputValue;
      InputValue := '';
    End;
    If (vString <> '') And (vString <> frm_Main.vCommandEnd) Then
    Begin
      Try
        If ByPass Then
        Begin
          vDecompressOK := ZDecompressStr(vString, vDecompressS);
          vDecompressOK := ZDecompressStr(vDecompressS, vString);
          vDecompressS := vString;
        End
        Else
          vDecompressOK := ZDecompressStr(vString, vDecompressS);
        If vDecompressOK Then
        Begin
          If Not NewDeskCapture Then
          Begin
            If (Trim(vOldS) <> '') And (vDecompressS <> '') And
              (Not vNewFrame) Then
            Begin
              vOldA := vOldS;
              ResumeStreamS(vOldS, vResult, vDecompressS);
              vDecompressS := vResult;
              vResult := '';
            End;
            If (vDecompressS <> '') Then
              If (vDecompressS[1] <> '0') And (vOldA = vOldS) Then
                vOldS := vDecompressS
              Else If (vDecompressS[1] = '0') Then
                vDecompressS := '';
            If vDecompressS = '' Then
              Exit;
          End;
        End
        Else
          Exit;
      Except
        vDecompressS := '';
        // Exit;
      End;
      If Pos('SIZE=0', UpperCase(vDecompressS)) > 0 Then
        Exit;
      Try
        If frm_Main.CloseConnection Then
          Exit;
        If Length(vDecompressS) > 0 Then
        Begin
          If frm_RemoteScreen <> Nil Then
          Begin
{$IFDEF MSWINDOWS}
{$IFNDEF FMX}Application.Processmessages;
{$ELSE}FMX.Forms.TApplication.Processmessages; {$ENDIF}
{$ENDIF}
            If frm_RemoteScreen <> Nil Then
              StringToStream(vDecompressS, frm_RemoteScreen.iDesktopCapture,
                vNoDiff, vImageState)
            Else
              Exit;
            vDecompressS := '';
            If (Not(frm_Main.vInitSection)) And (frm_RemoteScreen.Active) Then
            Begin
              frm_Main.vInitSection := True;
              frm_RemoteScreen.MouseIcon_Image.Down := frm_Main.vInitSection;
              frm_RemoteScreen.KeyboardIcon_Image.Down := frm_Main.vInitSection;
              If Assigned(frm_RemoteScreen.MouseIcon_Image.OnClick) Then
                frm_RemoteScreen.MouseIcon_Image.OnClick
                  (frm_RemoteScreen.MouseIcon_Image);
              If Assigned(frm_RemoteScreen.btn_aclose.OnClick) Then
                frm_RemoteScreen.btn_aclose.OnClick
                  (frm_RemoteScreen.btn_aclose);
            End;
          End;
        End;
      Except
      End;
    End;
  End;
  vFrameRate := Now;
End;

Procedure GetStreamM(InputClient: TUDPSuperClient; InputValue: String);
Var
  vDecompressS, // : AnsiString;
  vString, vLineString, vStringSize, vImageState, vResult: String;
  vNewFrame, vNoDiff: Boolean;
  vScreenWidth, vScreenHeight, vLeft, vTop, vWidth, vHeight, vFRResult: Integer;
  PeerConnected: TPeerconnected;
  Procedure StringToStream(Const S: String; Imagem: TImage;
  NoDiff: Boolean; ImageState: String);
  Var
    vDados: TStringStream;
    Bitmap, Bitmap2: TBitmap;
    vCol, vRow: Integer;
    Block: TJDRMImageBlock;
  Begin
    vDados := TStringStream.Create(S);
    Bitmap := Nil;
    Bitmap2 := Nil;
    Try
      vDados.Position := 0;
      If Not NewDeskCapture Then
      Begin
        If Not(NoDiff) Then
         Begin
          Bitmap := TBitmap.Create;
          Bitmap.LoadFromStream(vDados);
          Imagem.Picture.Assign(Bitmap); // .LoadFromStream(vDados);
          Bitmap.FreeImage;
          FreeAndNil(Bitmap);
         End
        Else
        Begin
          Bitmap := TBitmap.Create;
          Bitmap.LoadFromStream(vDados);
          Delete(ImageState, 1, Pos('=', ImageState));
          If (Pos('FULL', ImageState) > 0) Then
          Begin
            frm_Main.vInitImageCapture.ImageBase := Bitmap;
            Imagem.Picture.Assign(Bitmap);
          End
          Else
          Begin
            If Pos('SIZE=0', vStringSize) = 0 Then
            Begin
              vCol := StrToInt(Copy(ImageState, Pos('|', ImageState) + 1,
                (Pos(':', ImageState) - 1) - Pos('|', ImageState)));
              vRow := StrToInt(Copy(ImageState, Pos(':', ImageState) + 1,
                Length(ImageState)));
              frm_Main.vInitImageCapture.GetAltImage(Bitmap, Bitmap2, vCol,
                vRow, True);
              Imagem.Picture.Assign(Bitmap2);
              frm_Main.vInitImageCapture.ImageBase := Bitmap2;
            End;
          End;
        End;
      End
      Else
      Begin
        Block := TJDRMImageBlock.Create(Nil);
        Try
          Block.Compression := 100;
          Block.ScreenWidth := vScreenWidth;
          Block.ScreenHeight := vScreenHeight;
          Block.Left := vLeft;
          Block.Top := vTop;
          Block.Width := vWidth;
          Block.Height := vHeight;
          Block.AsBitmap.LoadFromStream(vDados);
          If (frm_Main <> Nil) Then
          Begin
            frm_RemoteScreen.DesktopViewCapture.DrawBlock(Block);
            frm_RemoteScreen.DesktopViewCapture.UpdateImage;
          End;
        Finally
          Block.Free;
        End;
      End;
    Finally
      vDados.SetSize(0);
      FreeAndNil(vDados);
      If Not NewDeskCapture Then
      Begin
        If (Bitmap <> Nil) Then
        Begin
          // Bitmap.FreeImage;
          FreeAndNil(Bitmap);
        End;
        If (Bitmap2 <> Nil) Then
        Begin
          // Bitmap2.FreeImage;
          FreeAndNil(Bitmap2);
        End;
      End;
    End;
  End;

Begin
  vFRResult := 0;
  If ShowFrameRate Then
    If frm_RemoteScreen <> Nil Then
    Begin
      vFRResult := MilliSecondsBetween(Now, vFrameRate);
      If vFRResult > 0 Then
      Begin
        vFRResult := 1000 div vFRResult;
        frm_RemoteScreen.Caption :=
          Format('Aegys - Computador Remoto, FPS : %d', [vFRResult]);
      End;
      If NewDeskCapture Then
        frm_RemoteScreen.DesktopViewCapture.Visible := True;
    End;
  While (InputValue <> '') Do
  Begin
    {
      PeerConnected := InputClient.GetActivePeer;
      If PeerConnected <> Nil Then
      InputClient.SendBuffer(InputClient.GetIpSend(PeerConnected), PeerConnected.Port,
      TNewFrameData + frm_Main.CommandEnd, False, dtt_Async);
    }
    If frm_Main.CloseConnection Then
      Exit;
    vLineString := InputValue;
    InputValue := '';
    vNewFrame := Pos(TNewFrameData, vLineString) > 0;
    If vNewFrame Then
      vLineString := StringReplace(vLineString, TNewFrameData, '',
        [rfReplaceAll]);
    vString := Copy(vLineString, Pos('<|$initstream$|>', vLineString) + 19,
      Pos(frm_Main.vCommandEnd, vLineString) - 1);
    vNoDiff := False;
    If (Pos('IMAGETYPE=', vString) > 0) Then
    Begin
      vImageState := Copy(vString, 1, Pos('<|>', vString) - 1);
      Delete(vString, 1, Pos('<|>', vString) + 2);
      vNoDiff := True;
    End
    Else If (Pos('BLOCKDEFS=', vString) > 0) Then
    Begin
      vImageState := Copy(vString, 1, Pos('<|>', vString) - 1);
      Delete(vString, 1, Pos('<|>', vString) + 2);
      vImageState := StringReplace(vImageState, 'BLOCKDEFS=', '',
        [rfReplaceAll]);
      vLeft := StrToInt(Copy(vImageState, 2, Pos('|', vImageState) - 2));
      Delete(vImageState, 1, Pos('|', vImageState));
      vTop := StrToInt(Copy(vImageState, 2, Pos('|', vImageState) - 2));
      Delete(vImageState, 1, Pos('|', vImageState));
      vWidth := StrToInt(Copy(vImageState, 2, Pos('|', vImageState) - 2));
      Delete(vImageState, 1, Pos('|', vImageState));
      vHeight := StrToInt(Copy(vImageState, 2, Pos('|', vImageState) - 2));
      Delete(vImageState, 1, Pos('|', vImageState));
      vScreenWidth := StrToInt(Copy(vImageState, 3, Pos('|', vImageState) - 3));
      Delete(vImageState, 1, Pos('|', vImageState));
      vScreenHeight := StrToInt(Copy(vImageState, 3, Length(vImageState)));
      Delete(vImageState, 1, Length(vImageState));
    End;
    vStringSize := Copy(vString, 0, Pos('<|>', vString) - 1);
    If (Trim(vStringSize) = '') Or (Trim(vStringSize) = 'SIZE=0') Then
      Exit;
    vString := Copy(vString, Pos('<|>', vString) + 3, Length(vString));
    If Pos(frm_Main.vCommandEnd, vString) > 0 Then
      vString := Copy(vString, 1, Pos(frm_Main.vCommandEnd, vString) - 1);
    If (vString <> '') And (vString <> frm_Main.vCommandEnd) Then
    Begin
      Try
        ZDecompressStr(vString, vDecompressS);
        If Not NewDeskCapture Then
        Begin
          If (Diferencial) Then
          Begin
            If (Trim(vOldS) <> '') And (vDecompressS <> '') And
              (Not vNewFrame) Then
            Begin
              ResumeStreamS(vOldS, vResult, vDecompressS);
              vDecompressS := vResult;
              vResult := '';
            End;
            If vDecompressS <> '' Then
              vOldS := vDecompressS;
          End;
          If vDecompressS = '' Then
            Exit;
        End;
      Except
        vDecompressS := '';
        // Exit;
      End;
      Try
        If frm_Main.CloseConnection Then
          Exit;
        If Length(vDecompressS) > 0 Then
        Begin
          If frm_RemoteScreen <> Nil Then
          Begin
            StringToStream(vDecompressS, frm_RemoteScreen.iDesktopCapture,
              vNoDiff, vImageState);
            vDecompressS := '';
            If (Not(frm_Main.vInitSection)) And (frm_RemoteScreen.Active) Then
            Begin
              frm_Main.vInitSection := True;
              frm_RemoteScreen.MouseIcon_Image.Down := frm_Main.vInitSection;
              frm_RemoteScreen.KeyboardIcon_Image.Down := frm_Main.vInitSection;
              If Assigned(frm_RemoteScreen.MouseIcon_Image.OnClick) Then
                frm_RemoteScreen.MouseIcon_Image.OnClick
                  (frm_RemoteScreen.MouseIcon_Image);
              If Assigned(frm_RemoteScreen.btn_aclose.OnClick) Then
                frm_RemoteScreen.btn_aclose.OnClick
                  (frm_RemoteScreen.btn_aclose);
            End;
          End;
        End;
      Except
      End;
    End;
  End;
  vFrameRate := Now;
End;

Procedure GetStreamM(InputClient: TUDPSuperClient;
Var StreamValue: TMemoryStream);
Var
  vResult, vDecompressS: TMemoryStream;
  vNewFrame, vNoDiff: Boolean;
  PeerConnected: TPeerconnected;
  vFRResult: Integer;
  Procedure StringToStream(Const S: TMemoryStream; Imagem: TImage);
  Var
    Bitmap: TBitmap;
  Begin
    Bitmap := Nil;
    Try
      S.Position := 0;
      Bitmap := TBitmap.Create;
      Bitmap.LoadFromStream(S);
      Imagem.Picture.Assign(Bitmap);
    Finally
      If (Bitmap <> Nil) Then
        FreeAndNil(Bitmap);
    End;
  End;

Begin
  vFRResult := 0;
  If ShowFrameRate Then
    If frm_RemoteScreen <> Nil Then
    Begin
      vFRResult := MilliSecondsBetween(Now, vFrameRate);
      If vFRResult > 0 Then
      Begin
        vFRResult := 1000 div vFRResult;
        frm_RemoteScreen.Caption :=
          Format('Aegys - Computador Remoto, FPS : %d', [vFRResult]);
      End;
      If NewDeskCapture Then
        frm_RemoteScreen.DesktopViewCapture.Visible := True;
    End;
  If StreamValue.Size > 0 Then
  Begin
    PeerConnected := InputClient.GetActivePeer;
    If PeerConnected <> Nil Then
      InputClient.SendBuffer(InputClient.GetIpSend(PeerConnected),
        PeerConnected.Port, TNewFrameData + frm_Main.CommandEnd, False,
        dtt_Async);
    If frm_Main.CloseConnection Then
      Exit;
    Try
      ZDecompressStream(StreamValue, vDecompressS);
      StreamValue.Free;
      If vOldStream = Nil Then
        vOldStream := TMemoryStream.Create;
      If Not NewDeskCapture Then
      Begin
        If (Diferencial) Then
        Begin
          If (vOldStream.Size > 0) And (vDecompressS.Size > 0) And
            (Not vNewFrame) Then
          Begin
            ResumeStreamS(vOldStream, vResult, vDecompressS);
            vDecompressS.Free;
            vDecompressS := TMemoryStream.Create;
            vResult.Position := 0;
            vDecompressS.CopyFrom(vResult, vResult.Size);
            vDecompressS.Position := 0;
            vResult.Free;
          End;
          If vDecompressS.Size > 0 Then
          Begin
            vOldStream.Free;
            vOldStream := TMemoryStream.Create;
            vDecompressS.Position := 0;
            vOldStream.CopyFrom(vDecompressS, vDecompressS.Size);
          End;
        End;
        If vDecompressS.Size = 0 Then
        Begin
          vDecompressS.Free;
          Exit;
        End;
      End;
    Except
      vDecompressS.Free;
      // Exit;
    End;
    Try
      If frm_Main.CloseConnection Then
        Exit;
      If vDecompressS.Size > 0 Then
      Begin
        If frm_RemoteScreen <> Nil Then
        Begin
          StringToStream(vDecompressS, frm_RemoteScreen.iDesktopCapture);
          vDecompressS.Free;
          If (Not(frm_Main.vInitSection)) And (frm_RemoteScreen.Active) Then
          Begin
            frm_Main.vInitSection := True;
            frm_RemoteScreen.MouseIcon_Image.Down := frm_Main.vInitSection;
            frm_RemoteScreen.KeyboardIcon_Image.Down := frm_Main.vInitSection;
            If Assigned(frm_RemoteScreen.MouseIcon_Image.OnClick) Then
              frm_RemoteScreen.MouseIcon_Image.OnClick
                (frm_RemoteScreen.MouseIcon_Image);
            If Assigned(frm_RemoteScreen.btn_aclose.OnClick) Then
              frm_RemoteScreen.btn_aclose.OnClick(frm_RemoteScreen.btn_aclose);
          End;
        End;
      End;
    Except
    End;
  End;
  vFrameRate := Now;
End;

Function SendStreamQueue(OutputClient: TUDPSuperClient; Var NewFrame: Boolean;
Buffered: Boolean = True; ImageViewQ: TImageViewQ = tiv_Medium)
  : Boolean; Overload;
Var
  vString: String;
  PeerConnected: TPeerconnected;
  I: Integer;
  ValueSend: TIdBytes;
  Procedure WriteString(Const S: String; Var Result: TIdBytes);
  Var
    n: LongInt;
    Stream: TStringStream;
  Begin
    n := Length(S);
    Stream := TStringStream.Create(S, CompressionEncoding);
    Try
      Stream.Position := 0;
      ReadTIdBytesFromStream(Stream, Result, Stream.Size, 0);
    Finally
      Stream.Free;
    End;
  End;

Begin
  Result := False;
  Try
    frm_Main.GetFrame := False;
    dmCaptureScreen.ImageViewQ := ImageViewQ;
    dmCaptureScreen.CaptureWidth := CaptureW;
    dmCaptureScreen.CaptureHeight := CaptureH;
    dmCaptureScreen.Diferencial := Diferencial;
    dmCaptureScreen.Cols := Cols;
    dmCaptureScreen.Rows := Rows;
    dmCaptureScreen.MaxNewFrame := MaxNewFrame;
    Try
      vLinhaSend := dmCaptureScreen.AviStreamOfBmpstreams(10, 30);
{$IFDEF MSWINDOWS}
{$IFNDEF FMX}Application.Processmessages;
{$ELSE}FMX.Forms.TApplication.Processmessages; {$ENDIF}
{$ENDIF}
      If frm_Main.CloseConnection Then
        Exit;
      Result := True;
    Except
      vLinhaSend := '';
    End;
    LastFrameTime := Now;
    If (Not(frm_Main.SendData.Active)) And (SendForThread) Then
      frm_Main.SendData.Active := SendForThread;
    If (vLinhaSend <> '') Then
    Begin
      LastFrameTime := Now;
      If vLinhaSend <> '' Then
      Begin
        If Not SendForThread Then
          SendStreamS(vLinhaSend)
        Else
          frm_Main.SendData.AddPack(vLinhaSend);
      End
      Else
        frm_Main.GetFrame := True;
    End;
    Result := True;
  Except
    Result := False;
  End;
End;

Procedure SendStreamM(OutputClient: TipPoolerService; Picture: TMemoryStream;
Buffered: Boolean = True);
Var
  sStream, copyStream: TMemoryStream;
  vString: String;
  iBufferLength, iPos, iCount, aCount: LongInt;
  vFirstData: Boolean;
  B: Array of Byte Absolute vString;
  Procedure WriteString(const Stream: TStream; const S: AnsiString);
  Var
    n: LongInt;
  Begin
    n := Length(S);
    If n > 0 Then
      Stream.Write(S[1], n);
  End;

Begin
  vFirstData := False;
  copyStream := TMemoryStream.Create;
  CompressStream(Picture, copyStream);
  copyStream.Position := 0;
  aCount := copyStream.Size;
  iCount := 0;
  iPos := 0;
  If (aCount > 0) And (iCount < aCount) Then
  Begin
    While (aCount > 0) And (iCount < aCount) Do
    Begin
      Try
        If aCount = iCount Then
          Break;
        if aCount < (iCount + OutputClient.BufferSize) Then
          iBufferLength := (aCount - iCount)
        Else
          iBufferLength := OutputClient.BufferSize;
        sStream := TMemoryStream.Create;
        If Buffered Then
        Begin
          iCount := iCount + iBufferLength;
          copyStream.Position := iPos + 1;
          sStream.CopyFrom(copyStream, iBufferLength);
          iPos := iPos + iBufferLength;
        End
        Else
        Begin
          iCount := aCount;
          copyStream.Position := 0;
          sStream.CopyFrom(copyStream, copyStream.Size);
        End;
        sStream.Position := 0;
        If Buffered Then
        Begin
          if Not(vFirstData) then
          Begin
            vFirstData := True;
            OutputClient.
              Write('<|$initstream$|>' + MemoryStreamToString(sStream));
          End
          Else
            OutputClient.DataToSend(MemoryStreamToString(sStream));
          If (iCount >= aCount) then
          Begin
            OnGetScreenShot := False;
            OutputClient.Write(MemoryStreamToString(sStream) +
              frm_Main.vCommandEnd);
          End;
        End
        Else
        Begin
          OnGetScreenShot := False;
          OutputClient.Write('<|$initstream$|>' + MemoryStreamToString(sStream)
            + frm_Main.vCommandEnd);
        End;
        FreeAndNil(sStream);
      Except
        Break;
      End;
      If Not OutputClient.Active Then
        Break;
    End;
  End;
  FreeAndNil(copyStream);
End;

Initialization

CompressionEncoding := TEncoding.ANSI;
CompressionDecoding := TEncoding.ANSI;

end.
