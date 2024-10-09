unit uFormConexao;

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

interface

uses
  System.SysUtils,  System.Types,       System.UITypes,            System.Classes,
  System.Variants,  System.Actions,     System.Win.ScktComp,       System.Messaging,
  FMX.Types,        FMX.Controls,       Vcl.Forms,                 FMX.Dialogs,
  FMX.Edit,         FMX.Objects,        FMX.Controls.Presentation, FMX.Layouts,
  FMX.Ani,          FMX.TabControl,     FMX.ListBox, FMX.Menus,    FMX.StdCtrls,
  uAegysBase,       uAegysDataTypes,    uFunctions, CCR.Clipboard, windows, shellapi,
  Messages,         uAegysConsts,       uAegysClientMotor,         FMX.Forms,
  FMX.ActnList,     uAegysBufferPack,   Vcl.Graphics,              Vcl.Imaging.jpeg,
  FMX.Graphics;

Const
 aFatorTela    = 3;
 cMaxSpriteCap = 10;

type
  TFormConexao = class(TForm)
    lyHeader: TLayout;
    PhLogo: TPath;
    LSubTitle: TLabel;
    Layout2: TLayout;
    LTitle: TLabel;
    Layout3: TLayout;
    lyMachineID: TLayout;
    RMachineID: TRectangle;
    LlyMachineIDCaption: TLabel;
    LMachineID: TLabel;
    Layout6: TLayout;
    PhMachineIDCopy: TPath;
    sbMachineIDCopy: TSpeedButton;
    lyGuestID: TLayout;
    RGuestID: TRectangle;
    LlyGuestIDCaption: TLabel;
    Layout10: TLayout;
    EGuestID: TEdit;
    lyConnect: TLayout;
    btnConectar: TRoundRect;
    LbtnConectar: TLabel;
    aniBtnLogin: TFloatAnimation;
    ActionList1: TActionList;
    actConnect: TAction;
    lyStatus: TLayout;
    PhStatus: TPath;
    LStatus: TLabel;
    tmrIntervalo: TTimer;
    tmrClipboard: TTimer;
    actCopyID: TAction;
    actCopyPassword: TAction;
    Layout4: TLayout;
    sbPasteID: TSpeedButton;
    PhPasteID: TPath;
    actPasteID: TAction;
    LVersion: TLabel;
    phOptions: TPath;
    sbOptions: TSpeedButton;
    lyQuality: TLayout;
    rQuality: TRectangle;
    LlyResolutionCaption: TLabel;
    cbQuality: TComboBox;
    lyPassword: TLayout;
    RPassword: TRectangle;
    LlyPasswordCaption: TLabel;
    Layout8: TLayout;
    LPassword: TLabel;
    sbPasswordCopy: TSpeedButton;
    PhPasswordCopy: TPath;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure actPasteIDExecute(Sender: TObject);
    procedure actConnectExecute(Sender: TObject);
    procedure actCopyIDExecute(Sender: TObject);
    procedure actCopyPasswordExecute(Sender: TObject);
    procedure tmrClipboardTimer(Sender: TObject);
    procedure tmrIntervaloTimer(Sender: TObject);
    procedure EGuestIDTyping(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sbOptionsClick(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure FormShow(Sender: TObject);
  private
    Position,
    vFPS,
    MousePosX,
    MousePosY         : Integer;
    vInitTime,
    vFinalTime        : TDateTime;
    aPackList         : TPackList;
    aConnection,
    vClientID,
    vOldClipboardFile,
    vOldClipboardText : String;
    TrayWnd           : HWND;
    TrayIconData      : TNotifyIconData;
    aFirstCapture,
    BInputsBlock,
    TrayIconAdded,
    vVisualizador,
    isvisible         : Boolean;
    SendDataThread    : TAegysMotorThread;//Envio de Desktop
//    SendCommandEvents : TAegysMotorThread;//Envio de Comandos
    Function  MascaraID              (AText,
                                      AMascara          : String) : String;
    procedure Translate;
    procedure SetColors;
    function  ClipboardGetAsFile                        : String;
    procedure TrayWndProc            (Var Message       : TMessage);
    procedure ShowAppOnTaskbar;
    procedure HideApponTaskbar;
    procedure SetTrayIcon;
    procedure OnBeforeConnect        (Sender            : TObject;
                                      Var WelcomeString : String);
    procedure OnConnect              (Sender            : TObject);
    procedure OnServerLogin          (Sender            : TObject);
    procedure OnDisconnect           (Sender            : TObject);
    procedure OnBeginTransactionError(Connection        : String);
    procedure OnBeginTransaction     (Connection        : String;
                                      Var ClientID,
                                      Alias             : String);
    procedure OnIncommingConnect     (Connection        : String;
                                      Var ClientID,
                                      ClientPassword,
                                      Alias             : String);
    procedure OnAccessGranted        (Connection        : String;
                                      Var ClientID,
                                      ClientPassword,
                                      SpecialData       : String);
    procedure OnPeerConnected        (Connection        : String;
                                      Var ClientID,
                                      ClientPassword,
                                      Alias             : String);
    procedure OnPeerKick             (Connection        : String;
                                      Var ClientID,
                                      ClientPassword,
                                      Alias             : String);
    procedure OnPeerDisconnected     (Connection        : String;
                                      Var ClientID,
                                      ClientPassword,
                                      Alias             : String);
    Function  OnPulseData            (aPack             : TAegysBytes;
                                      CommandType       : TCommandType = tctScreenCapture): Boolean;
    procedure OnProcessData          (aPackList         : TPackList;
                                      aFullFrame        : Boolean);
    procedure OnScreenCapture        (Connection,
                                      ID, Command       : String;
                                      MultiPack         : Boolean;
                                      PackCount         : AeInteger;
                                      aBuf              : TAegysBytes);
    procedure OnKeyboardCapture      (Command           : String);
    procedure OnMouseCapture         (Command           : String);
    procedure KillThreads;
    procedure ExecuteCommand         (aLine             : String);
    procedure AccessDenied;
    Procedure OnInternalCommand      (InternalCommand   : TInternalCommand;
                                      Connection,
                                      ID, Command       : String);
    procedure OnPeerList(InternalCommand: TInternalCommand; Command: String);
    procedure ReposicionarFav;
    procedure OnFileTransfer(Connection,
                             ID,
                             CommandMessage : String;
                             Command        : TInternalCommand;
                             MultiPack      : Boolean;
                             PackCount      : AeInteger;
                             aBuf           : TAegysBytes);
  public
   aMyAlias,
   aMyFixPass,
   aMyGroup      : String;
   aUnAssistConn : Boolean;
   Procedure SendConfigs;
   Procedure Kick;
   Procedure SetPeerDisconnected;
   Procedure LimparConexao;
   Procedure MudarStatusConexao     (AStatus           : Integer;
                                      AMensagem         : String);
   Procedure SetOffline;
   Procedure SetOnline;
   Procedure SetSockets;
  end;

  Function RFB_Data : Boolean;

var
  FormConexao        : TFormConexao;
  Conexao            : TAegysClient;
  aLastPass,
  aMonitor,
  aMonitorCount      : String;
  vDrawCursor,
  Bblockinput        : Boolean;
  vResolucaoLargura,
  vOldResolucaoLargura,
  vResolucaoAltura,
  vOldResolucaoAltura,
  aIncSprite,
  CF_FILE            : Integer;
  mx, my             : Single;
  WndProcHook        : THandle;
  vActualImage       : TMemoryStream;

const
  WM_ICONTRAY = WM_USER + 1;

implementation

{$R *.fmx}

uses uFormTelaRemota,  uFileTransfer,    uFormChat,        FMX.Clipboard,
     System.IOUtils,   System.Rtti,      uLibClass,        uConstants,
     System.DateUtils, FMX.Platform.Win, uFormConfig,
     StreamManager,    uFormSenha,       uSendKeyClass,    uAegysTools,
     uAegysZlib,       NB30, uFavoritos, uFilesFoldersOP,  uASMTools;


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
 Try
  ShellProps.Free;
 Except
 End;
End;

Procedure ListFiles(FolderName : String;
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
  ShellProps.Folder := FolderName;
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
End;

Procedure ListFolders(Directory      : String;
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
    Windows.FindClose(FindHandle);
   End;
  End;
// ReturnStr := (Dirlist);
// Result := ReturnStr;
End;

Function WndProc(Code: integer; WParam, LParam: LongInt): LRESULT; stdcall;
var
  msg: TCWPRetStruct;
begin;
  if (Code >= HC_ACTION) and (LParam > 0) then begin
    msg := PCWPRetStruct(LParam)^;
    if (msg.Message = WM_EXITSIZEMOVE) Then Begin//and (msg.WParam = SIZE_MINIMIZED) then begin
     if Assigned(FormConexao) then
      if FormConexao.Active then
       FormConexao.ReposicionarFav;
      // Application has been minimized
      // Check msg.wnd = WindowHandleToPlatform(Form1.Handle).wnd if necessary
    end;
  end;
  result := CallNextHookEx(WndProcHook, Code, WParam, LParam)
end;

Function RFB_Data : Boolean;
var
 ver            : tosversioninfo;
 majorversion,
 minorversion,
 buildnumber    : Integer;
 name,
 csdversion     : String;
Begin
 majorversion := 0;
 minorversion := 0;
 Ver.dwOSVersionInfoSize := SizeOf( TOSVersionInfo );
 If Windows.GetVersionEx(Ver) Then
  Begin
   With Ver Do
    Begin
     Case dwPlatformId Of
      VER_PLATFORM_WIN32s        : Name := 'Win32s';
      VER_PLATFORM_WIN32_WINDOWS : Name := 'Windows 95';
      VER_PLATFORM_WIN32_NT      : Name := 'Windows NT';
     End;
     majorversion := dwMajorVersion;
     minorversion := dwminorversion;
     buildnumber  := dwBuildNumber;
     csdversion   := szCSDVersion;
//            label1.caption:=name;
//            label10.caption:=minorversion;
//            label2.caption:=majorversion;
//            label3.caption:=buildnumber;
//            label4.caption:=csdversion;
    End;
  End;
 Result := (majorversion < 7);
End;


Procedure TFormConexao.OnFileTransfer(Connection,
                                      ID,
                                      CommandMessage    : String;
                                      Command           : TInternalCommand;
                                      MultiPack         : Boolean;
                                      PackCount         : AeInteger;
                                      aBuf              : TAegysBytes);
Var
 I                : Integer;
 aPackClass       : TPackClass;
 vBuf             : TAegysBytes;
 vDataSendReceive : TStringList;
 Procedure NewPack;
 Begin
  aPackClass             := TPackClass.Create;
  aPackClass.DataMode    := tdmClientCommand;
  aPackClass.Owner       := Conexao.Connection;
  aPackClass.Dest        := Connection;
  aPackClass.DataCheck   := tdcAsync;
  aPackClass.CommandType := tctFileTransfer;
 End;
 Procedure FreePack;
 Begin
  SetLength(vBuf, 0);
  FreeAndNil(aPackClass);
 End;
 Function MontaLinhaEnvio(Value : TStringList) : AnsiString;
 Var
  I : Integer;
 Begin
  Result := '';
  For I := 0 To Value.Count -1 do
   Result := Result + Value[I] + sLineBreak;
 End;
Begin
 Case Command Of
  ticGetFolders : Begin
                   vDataSendReceive := TStringList.Create;
                   Try
                    ListFolders(CommandMessage, vDataSendReceive);
                   Finally
                    Try
                     If vDataSendReceive.Text <> '' Then
                      Begin
                       NewPack;
                       Try
                        aPackClass.Command     := Format('%s%s', [cSetFolders, vDataSendReceive.Text]);
                        vBuf                   := aPackClass.ToBytes;
                        Conexao.SendBytes(vBuf, twmBuffer);
                       Finally
                        FreePack;
                       End;
                      End;
                    Finally
                     FreeAndNil(vDataSendReceive);
                    End;
                   End;
                  End;
  ticSetFolders : Begin
                   vDataSendReceive := TStringList.Create;
                   Try
                    vDataSendReceive.Text := CommandMessage;
                    If Copy(vDataSendReceive.Text, 1, 2) <> '..' Then
                     vDataSendReceive.Text := '..' + #13 + vDataSendReceive.Text;
                    If Assigned(fFileTransfer) Then
                     Begin
                      fFileTransfer.CarregarListaPastas(vDataSendReceive.Text);
                      fFileTransfer.Caption := Format('Transferência de Arquivo %d', [fFileTransfer.DestCount]);
                     End;
                   Finally
                    FreeAndNil(vDataSendReceive);
                    NewPack;
                    Try
                     aPackClass.Command     := Format('%s%s', [cGetFiles, fFileTransfer.ActiveFolder]);
                     vBuf                   := aPackClass.ToBytes;
                     Conexao.SendBytes(vBuf, twmBuffer);
                    Finally
                     FreePack;
                    End;
                   End;
                  End;
  ticGetFiles   : Begin
                   vDataSendReceive := TStringList.Create;
                   Try
                    ListFiles(CommandMessage, vDataSendReceive);
                   Finally
                    Try
                     If vDataSendReceive.Text <> '' Then
                      Begin
                       NewPack;
                       Try
                        aPackClass.Command     := Format('%s%s', [cSetFiles, MontaLinhaEnvio(vDataSendReceive)]);
                        vBuf                   := aPackClass.ToBytes;
                        Conexao.SendBytes(vBuf, twmBuffer);
                       Finally
                        FreePack;
                       End;
                      End;
                    Finally
                     FreeAndNil(vDataSendReceive);
                    End;
                   End;
                  End;
  ticGetDrivers : Begin
                   vDataSendReceive := TStringList.Create;
                   Try
                    ListDrivers(vDataSendReceive);
                   Finally
                    Try
                     If vDataSendReceive.Text <> '' Then
                      Begin
                       NewPack;
                       Try
                        aPackClass.Command     := Format('%s%s', [cSetDrivers, vDataSendReceive.Text]);
                        vBuf                   := aPackClass.ToBytes;
                        Conexao.SendBytes(vBuf, twmBuffer);
                       Finally
                        FreePack;
                       End;
                      End;
                    Finally
                     FreeAndNil(vDataSendReceive);
                    End;
                   End;
                  End;
  ticSetFiles   : Begin
                   If Assigned(fFileTransfer) Then
                    Begin
                     fFileTransfer.CarregarListaArquivos(CommandMessage);
                     fFileTransfer.Caption := Format('Transferência de Arquivo %d', [fFileTransfer.DestCount]);
                    End;
                  End;
  ticSetDrivers : Begin
                   vDataSendReceive := TStringList.Create;
                   Try
                    vDataSendReceive.Text := CommandMessage;
                    If Assigned(fFileTransfer) Then
                     Begin
                      fFileTransfer.ceRemotePath.Items.Clear;
                      fFileTransfer.lPCRemoto.Text := vDataSendReceive[0];
                      For I := 1 To vDataSendReceive.count - 1 Do
                       fFileTransfer.ceRemotePath.Items.Add(' ' + vDataSendReceive[I]);
                      If fFileTransfer.ceRemotePath.Items.Count > 0 Then
                       Begin
                        fFileTransfer.ceRemotePath.ItemIndex := 0;
                        fFileTransfer.ceRemotePath.OnChange(fFileTransfer.ceRemotePath);
                       End;
                     End;
                   Finally
                    FreeAndNil(vDataSendReceive);
                   End;
                  End;
 End;
End;

Procedure TFormConexao.ExecuteCommand(aLine : String);
Var
 aTempID        : String;
 InicioPalavra,
 TamanhoPalavra : Integer;
 vOnMouseShow   : Boolean;
Begin
 vOnMouseShow   := False;
 If aLine.Contains(cShowMouse)    Then
  Begin
   vOnMouseShow  := True;
   vMostrarMouse := vOnMouseShow;
  End;
 If aLine.Contains(cHideMouse)    Then
  Begin
   vOnMouseShow  := True;
   vMostrarMouse := False;
  End;
 If aLine.Contains(cBlockInput)   Then
  Begin
   BInputsBlock := True;
   Blockinput(BInputsBlock);
  End;
 If aLine.Contains(cUnBlockInput) Then
  Begin
   BInputsBlock := False;
   Blockinput(BInputsBlock);
  End;
 Position := Pos(cMousePos, aLine);
 While Position > 0 Do
  Begin
   Delete(aLine, InitStrPos, Position + Length(cMousePos) -1);
   Position := Pos(cSeparatorTag, aLine);
   aTempID  := aMonitor;
   If Trim(aTempID) = '' Then
    aTempID := '0';
   MousePosX := Vcl.Forms.Screen.Monitors[StrToInt(aTempID)].Left + StrToInt(Copy(aLine, InitStrPos, Position - 1));
   Delete(aLine, InitStrPos, Position + 2);
   MousePosY := Vcl.Forms.Screen.Monitors[StrToInt(aTempID)].Top  + StrToInt(Copy(aLine, InitStrPos, Pos(cEndTag, aLine) - 1));
   Delete(aLine, InitStrPos, Pos(cEndTag, aLine) + Length(cEndTag) -1);
   If aLine.Contains(cBlockInput) then
    Begin
     BlockInput(False);
     SetCursorPos(MousePosX, MousePosY);
     Application.ProcessMessages;
     BlockInput(True);
    End
   Else
    SetCursorPos(MousePosX, MousePosY);
   Application.Processmessages;
   Position := Pos(cMousePos, aLine);
  End;
 Position := Pos(cMouseClickLeftDown, aLine);
 If Position > 0 then
  Begin
   aTempID   := aMonitor;
   If Trim(aTempID) = '' Then
    aTempID := '0';
   Delete(aLine, InitStrPos, Position + Length(cMouseClickLeftDown) -1);
   Position   := Pos(cSeparatorTag, aLine);
   MousePosX  := Vcl.Forms.Screen.Monitors[StrToInt(aTempID)].Left + StrToInt(Copy(aLine, InitStrPos, Position - 1));
   Delete(aLine, 1, Position + 2);
   MousePosY := Vcl.Forms.Screen.Monitors[StrToInt(aTempID)].Top + StrToInt(Copy(aLine, InitStrPos, Pos(cEndTag, aLine) - 1));
   Delete(aLine, InitStrPos, Pos(cEndTag, aLine) + Length(cEndTag) -1);
   If aLine.Contains(cBlockInput) Then
    Begin
     BlockInput(false);
     SetCursorPos(MousePosX, MousePosY);
     Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
     Application.ProcessMessages;
     BlockInput(true);
    End
   Else
    Begin
     SetCursorPos(MousePosX, MousePosY);
     Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
    End;
  End;
 Position := Pos(cMouseClickLeftUp, aLine);
 If Position > 0 then
  Begin
   aTempID   := aMonitor;
   If Trim(aTempID) = '' Then
    aTempID := '0';
   Delete(aLine, InitStrPos, Position + Length(cMouseClickLeftUp) -1);
   Position := Pos(cSeparatorTag, aLine);
   MousePosX := Vcl.Forms.Screen.Monitors[StrToInt(aTempID)].Left + StrToInt(Copy(aLine, InitStrPos, Position - 1));
   Delete(aLine, 1, Position + 2);
   MousePosY := Vcl.Forms.Screen.Monitors[StrToInt(aTempID)].Top + StrToInt(Copy(aLine, InitStrPos, Pos(cEndTag, aLine) - 1));
   Delete(aLine, InitStrPos, Pos(cEndTag, aLine) + Length(cEndTag) -1);
   If aLine.Contains(cBlockInput) then
    Begin
     BlockInput(false);
     SetCursorPos(MousePosX, MousePosY);
     Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
     Application.ProcessMessages;
     blockinput(true);
    End
   Else
    Begin
     SetCursorPos(MousePosX, MousePosY);
     Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
    End;
  End;
 Position := Pos(cMouseClickRightDown, aLine);
 If Position > 0 Then
  Begin
   aTempID   := aMonitor;
   If Trim(aTempID) = '' Then
    aTempID := '0';
   Delete(aLine, InitStrPos, Position + Length(cMouseClickRightDown) -1);
   Position := Pos(cSeparatorTag, aLine);
   MousePosX := Vcl.Forms.Screen.Monitors[StrToInt(aTempID)].Left + StrToInt(Copy(aLine, InitStrPos, Position - 1));
   Delete(aLine, InitStrPos, Position + 2);
   MousePosY := Vcl.Forms.Screen.Monitors[StrToInt(aTempID)].Top + StrToInt(Copy(aLine, InitStrPos, Pos(cEndTag, aLine) - 1));
   Delete(aLine, InitStrPos, Pos(cEndTag, aLine) + Length(cEndTag) -1);
   If aLine.Contains(cBlockInput) Then
    Begin
     BlockInput(false);
     SetCursorPos(MousePosX, MousePosY);
     Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_RIGHTDOWN, 0, 0, 0, 0);
     Application.ProcessMessages;
     Blockinput(true);
    End
   Else
    Begin
     SetCursorPos(MousePosX, MousePosY);
     Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_RIGHTDOWN, 0, 0, 0, 0);
    End;
  End;
 Position := Pos(cMouseClickRightUp, aLine);
 If Position > 0 Then
  Begin
   aTempID   := aMonitor;
   If Trim(aTempID) = '' Then
    aTempID := '0';
   Delete(aLine, InitStrPos, Position + Length(cMouseClickRightUp) -1);
   Position := Pos(cSeparatorTag, aLine);
   MousePosX := Vcl.Forms.Screen.Monitors[StrToInt(aTempID)].Left + StrToInt(Copy(aLine, InitStrPos, Position - 1));
   Delete(aLine, InitStrPos, Position + 2);
   MousePosY := Vcl.Forms.Screen.Monitors[StrToInt(aTempID)].Top + StrToInt(Copy(aLine, InitStrPos, Pos(cEndTag, aLine) - 1));
   Delete(aLine, InitStrPos, Pos(cEndTag, aLine) + Length(cEndTag) -1);
   If aLine.Contains(cBlockInput) then
    Begin
     BlockInput(false);
     SetCursorPos(MousePosX, MousePosY);
     Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_RIGHTUP, 0, 0, 0, 0);
     Application.ProcessMessages;
     BlockInput(true);
    End
   Else
    Begin
     SetCursorPos(MousePosX, MousePosY);
     Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_RIGHTUP, 0, 0, 0, 0);
    End;
  End;
 Position := Pos(cMouseClickMiddleDown, aLine);
 If Position > 0 Then
  Begin
   aTempID   := aMonitor;
   If Trim(aTempID) = '' Then
    aTempID := '0';
   Delete(aLine, InitStrPos, Position + Length(cMouseClickMiddleDown) -1);
   Position := Pos(cSeparatorTag, aLine);
   MousePosX := Vcl.Forms.Screen.Monitors[StrToInt(aTempID)].Left + StrToInt(Copy(aLine, InitStrPos, Position - 1));
   Delete(aLine, InitStrPos, Position + 2);
   MousePosY := Vcl.Forms.Screen.Monitors[StrToInt(aTempID)].Top + StrToInt(Copy(aLine, InitStrPos,  Pos(cEndTag, aLine) - 1));
   Delete(aLine, InitStrPos, Pos(cEndTag, aLine) + Length(cEndTag) -1);
   If aLine.Contains(cBlockInput) Then
    Begin
     BlockInput(false);
     SetCursorPos(MousePosX, MousePosY);
     Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MIDDLEDOWN, 0, 0, 0, 0);
     Application.ProcessMessages;
     BlockInput(true);
    End
   Else
    Begin
     SetCursorPos(MousePosX, MousePosY);
     Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MIDDLEDOWN, 0, 0, 0, 0);
    End;
  End;
 Position := Pos(cMouseClickMiddleUp, aLine);
 If Position > 0 Then
  Begin
   aTempID   := aMonitor;
   If Trim(aTempID) = '' Then
    aTempID := '0';
   Delete(aLine, InitStrPos, Position + Length(cMouseClickMiddleUp) -1);
   Position := Pos(cSeparatorTag, aLine);
   MousePosX := Vcl.Forms.Screen.Monitors[StrToInt(aTempID)].Left + StrToInt(Copy(aLine, InitStrPos, Position - 1));
   Delete(aLine, InitStrPos, Position + 2);
   MousePosY := Vcl.Forms.Screen.Monitors[StrToInt(aTempID)].Top  + StrToInt(Copy(aLine, InitStrPos, Pos(cEndTag, aLine) - 1));
   Delete(aLine, InitStrPos, Pos(cEndTag, aLine) + Length(cEndTag) -1);
   If aLine.Contains(cBlockInput) Then
    Begin
     BlockInput(false);
     SetCursorPos(MousePosX, MousePosY);
     Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MIDDLEUP, 0, 0, 0, 0);
     Application.ProcessMessages;
     BlockInput(true);
    End
   Else
    Begin
     SetCursorPos(MousePosX, MousePosY);
     Mouse_Event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MIDDLEUP, 0, 0, 0, 0);
    End;
  End;
 Position := Pos(cWheelMouse, aLine);
 If Position > 0 then
  Begin
   Delete(aLine, InitStrPos, Position + Length(cWheelMouse) -1);
   Position := StrToInt(Copy(aLine, InitStrPos, Pos(cEndTag, aLine) - 1));
   Delete(aLine, InitStrPos, Pos(cEndTag, aLine) + Length(cEndTag) -1);
   If aLine.Contains(cBlockInput) Then
    Begin
     BlockInput(false);
     Mouse_Event(MOUSEEVENTF_WHEEL, 0, 0, DWORD(StrToInt(aLine)), 0);
     Application.ProcessMessages;
     BlockInput(true);
    End
   Else
    Mouse_Event(MOUSEEVENTF_WHEEL, 0, 0, DWORD(Position), 0);
  End;
 If vOnMouseShow then
  vDrawCursor := vMostrarMouse;
 Bblockinput := aLine.Contains(cBlockInput);
 If Bblockinput Then
  Begin
   InicioPalavra  := pos(cBlockInput, aLine);
   TamanhoPalavra := Length(cBlockInput);
   If InicioPalavra > 0          Then
    Delete(aLine, InicioPalavra, TamanhoPalavra);
   BlockInput(false);
  End;
 If aLine.Contains(cAltDown)     Then
  Begin
   aLine := StringReplace(aLine, cAltDown, '', [rfReplaceAll]);
   keybd_event(18, 0, 0, 0);
  End;
 If aLine.Contains(cAltUp)       Then
  Begin
   aLine := StringReplace(aLine, cAltUp, '', [rfReplaceAll]);
   keybd_event(18, 0, KEYEVENTF_KEYUP, 0);
  End;
 If aLine.Contains(cCtrlDown)    Then
  Begin
   aLine := StringReplace(aLine, cCtrlDown, '', [rfReplaceAll]);
   keybd_event(17, 0, 0, 0);
  End;
 If aLine.Contains(cCtrlUp) Then
  Begin
   aLine := StringReplace(aLine, cCtrlUp, '', [rfReplaceAll]);
   keybd_event(17, 0, KEYEVENTF_KEYUP, 0);
  End;
 If aLine.Contains(cShiftDown) then
  Begin
   aLine := StringReplace(aLine, cShiftDown, '', [rfReplaceAll]);
   keybd_event(16, 0, 0, 0);
  End;
 If aLine.Contains(cShiftUp) then
  Begin
   aLine := StringReplace(aLine, cShiftUp, '', [rfReplaceAll]);
   keybd_event(16, 0, KEYEVENTF_KEYUP, 0);
  End;
 If aLine.Contains('?') Then
  Begin
   If GetKeyState(VK_SHIFT) < 0 Then
    Begin
     keybd_event(16, 0, KEYEVENTF_KEYUP, 0);
     SendKeys(PWideChar(aLine), False);
     keybd_event(16, 0, 0, 0);
    End;
  End
 Else If aLine <> '' Then
  SendKeys(PWideChar(aLine), False);
// Processmessages;
// BlockInput(Bblockinput);
End;

Procedure TFormConexao.LimparConexao;
Begin
  vResolucaoLargura := 986;
  vResolucaoAltura  := 600;
  vVisualizador     := False;
  vOldClipboardText := '';
  If Assigned(FormTelaRemota) Then
   FreeAndNil(FormTelaRemota);
  If Assigned(fFileTransfer) Then
   FreeAndNil(fFileTransfer);
  If Assigned(FormChat) Then
   FreeAndNil(FormChat);
End;

Procedure TFormConexao.tmrClipboardTimer(Sender: TObject);
Var

  FileStream: TFileStream;
  s, FileName: string;
  ClipFormat: TClipboardFormat;
  aFileStream : TAegysBytes;
Begin
  tmrClipboard.Enabled := vVisualizador;
  try

    if Clipboard.HasText then
    begin
      if (vOldClipboardText <> Clipboard.AsText) Then
      Begin
        vOldClipboardText := Clipboard.AsText;
        Conexao.SendMessage(EGuestID.Text, '<|CLIPBOARD|>' + vOldClipboardText + '<|END|>', tctClipBoard);
      End;
    end
    else
    begin
      for s in Clipboard.GetFileNames do
      begin
        if s <> '' then
          if (vOldClipboardFile <> s) And (FileExists(s)) Then
          begin
            FileStream := TFileStream.Create(s, fmOpenRead);
            Try
             FileName := ExtractFileName(s);
             vOldClipboardFile := s;
             Conexao.SendMessage(EGuestID.Text, '<|DIRECTORYTOSAVE|>' + FileName + '<|><|SIZE|>' + intToStr(FileStream.Size) + '<|END|>', tctFileTransfer);
             FileStream.Position := 0;
             FileStream.Read(aFileStream, FileStream.Size);
             Conexao.SendBytes(EGuestID.Text, aFileStream);
             Application.Processmessages;
            Finally
             SetLength(aFileStream, 0);
             FreeAndNil(FileStream);
            End;
          end;
      end;
    end;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;

end;

procedure TFormConexao.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 TThread.Synchronize(Nil, Procedure
                     Begin
                      If Assigned(Conexao) then
                       FreeAndNil(Conexao);
                      If Assigned(fFavoritos) then
                       fFavoritos.Close;
                      FormConexao := Nil;
                     End);
 FormConexao.DisposeOf;
end;

procedure TFormConexao.SendConfigs;
Var
 aPackClass : TPackClass;
 aBuf       : TAegysBytes;
Begin
 aPackClass            := TPackClass.Create;
 Try
  aPackClass.DataMode  := tdmServerCommand;
  aPackClass.DataCheck := tdcAsync;
  aPackClass.Command   := cMyConfigs + EncodeStrings(Format('%s|%d|%s|%s', [aMyAlias,
                                                                            Integer(aUnAssistConn),
                                                                            aMyFixPass,
                                                                            aMyGroup]));
  aBuf                 := aPackClass.ToBytes;
  Conexao.SendBytes(aBuf);
 Finally
  SetLength(aBuf, 0);
  FreeAndNil(aPackClass);
 End;

End;

procedure TFormConexao.FormCreate(Sender: TObject);
begin
 CF_FILE := RegisterClipboardFormat('FileName');
 // inicializando os objetos
 Conexao := TAegysClient.Create(Self);
 // --------------------------
 SetColors;
 SetOffline;
// Translate;
 SetSockets;
 isvisible := True;
 // load confg
// CFG := TSQLiteConfig.Create;
// Try
//  lyGuestID.Visible := Not StrToIntDef(CFG.getValue(QUICK_SUPPORT), 0).ToBoolean;
//  lyConnect.Visible := Not StrToIntDef(CFG.getValue(QUICK_SUPPORT), 0).ToBoolean;
//  lyQuality.Visible := Not StrToIntDef(CFG.getValue(QUICK_SUPPORT), 0).ToBoolean;
// Finally
//  FreeAndNil(CFG);
// End;
 aPackList         := TPackList.Create;
 SendDataThread    := Nil;
// SendCommandEvents := Nil;
 Position          := 0;
 MousePosX         := 0;
 MousePosY         := 0;
end;

procedure TFormConexao.FormDestroy(Sender: TObject);
begin
 FreeAndNil(aPackList);
end;

procedure TFormConexao.FormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Single);
begin
  mx := X;
  my := Y;
end;

Procedure TFormConexao.ReposicionarFav;
Begin
 If Not Assigned(fFavoritos) Then
  fFavoritos := TfFavoritos.Create(Self);
 fFavoritos.Show;
 fFavoritos.Left := (Self.Left + self.Width);
 fFavoritos.Top  := Self.Top;
End;

procedure TFormConexao.FormShow(Sender: TObject);
Begin
 ReposicionarFav;
End;

Procedure TFormConexao.HideApponTaskbar;
var
  hAppWnd: HWND;
  ExStyle: LONG_PTR;
begin
  hAppWnd := FMX.Platform.Win.ApplicationHWND();
  ShowWindow(hAppWnd, SW_HIDE);
  ExStyle := GetWindowLongPtr(hAppWnd, GWL_EXSTYLE);
  SetWindowLongPtr(hAppWnd, GWL_EXSTYLE, (ExStyle and WS_EX_APPWINDOW) or
    WS_EX_TOOLWINDOW);
  // ShowWindow(hAppWnd, SW_SHOW);
end;

function TFormConexao.MascaraID(AText, AMascara: string): string;
var
  i: Integer;
begin
  for i := 1 to Length(AText) do
  begin
    if (AMascara[i] = '9') and not(AText[i] in ['0' .. '9']) and
      (Length(AText) = Length(AMascara) + 1) then
      Delete(AText, i, 1);
    if (AMascara[i] <> '9') and (AText[i] in ['0' .. '9']) then
      Insert(AMascara[i], AText, i);
  end;
  Result := AText;
end;

procedure TFormConexao.MudarStatusConexao(AStatus: Integer; AMensagem: string);
var
  cColor: TAlphaColor;
begin
 cColor := TAlphaColorRec.Yellow;
  case AStatus of
    1:
      cColor := TAlphaColorRec.Yellow;
    2:
      cColor := $FFED3237;
    3:
      cColor := TAlphaColorRec.Mediumseagreen;
  end;
  PhStatus.Fill.Color := cColor;
  PhStatus.Tag := AStatus;
  LStatus.Text := AMensagem;
//  SetOnline;
end;

procedure TFormConexao.sbOptionsClick(Sender: TObject);
begin
  Application.CreateForm(TfConfig, fConfig);
  fConfig.show;
//  fConfig.CallBackConfig := Translate;
end;

procedure TFormConexao.actPasteIDExecute(Sender: TObject);
begin
  EGuestID.Text := MascaraID(TRDLib.ColarTexto, '99-999-999');
  EGuestID.GoToTextEnd;
end;

function TFormConexao.ClipboardGetAsFile: string;
var
  Data: THandle;
begin
  Clipboard.Open;
  Data := GetClipboardData(CF_FILE);
  try
    if Data <> 0 then
      Result := PChar(GlobalLock(Data))
    else
      Result := '';
  finally
    if Data <> 0 then
      GlobalUnlock(Data);
    Clipboard.Close;
  end;
end;

procedure TFormConexao.actConnectExecute(Sender: TObject);
begin
 If LbtnConectar.Enabled Then
  Begin
   vOldResolucaoAltura   := -1;
   vOldResolucaoLargura  := -1;
   If Assigned(FormTelaRemota) Then
    FreeAndNil(FormTelaRemota);
   vDrawCursor := False;
   If not(LlyGuestIDCaption.Text = '   -   -   ') then
    begin
      if (LlyGuestIDCaption.Text = Conexao.SessionID) then
        MessageBox(0, 'Erro Connectando...',
          'RemoteSupport',
          MB_ICONASTERISK + MB_TOPMOST)
      else
      begin
        LbtnConectar.Enabled := False;
        Conexao.SendCommand(cFindID + EGuestID.Text);
        btnConectar.Enabled := False;
        MudarStatusConexao(1, 'Procurando ID');
      end;
    end;
  End;
end;

procedure TFormConexao.actCopyIDExecute(Sender: TObject);
begin
  TRDLib.CopiarTexto(LMachineID.Text);
end;

procedure TFormConexao.actCopyPasswordExecute(Sender: TObject);
begin
  TRDLib.CopiarTexto(LPassword.Text)
end;

procedure TFormConexao.Translate;
begin
  self.Caption := 'Suporte Remoto';
  LSubTitle.Text := 'Aegys';
  LVersion.Text := Format('Versão %s', [TRDLib.GetAppVersionStr]);
  LlyMachineIDCaption.Text := 'Meu ID';
  LlyPasswordCaption.Text := 'Password';
  LlyGuestIDCaption.Text := 'ID do Acesso Remoto';
  LlyResolutionCaption.Text := 'Qualidade do Vídeo';
  LbtnConectar.Text := 'Connectar';
  SetSockets;

end;

procedure TFormConexao.TrayWndProc(var Message: TMessage);
begin
  if Message.MSG = WM_ICONTRAY then
  begin
    case Message.LParam of
      WM_LBUTTONDOWN:
        begin
          if not isvisible then
            self.show; // If u use some frmMain.hide
          SetForegroundWindow(FmxHandleToHWND(FormConexao.Handle));
          if TrayIconAdded then
          begin
            // Shell_NotifyIcon(NIM_DELETE, @TrayIconData);
            TrayIconAdded := False;
            ShowAppOnTaskbar;
          end;
        end;
      // WM_RBUTTONDOWN: ShowMessage('RolePlay , but can be a PopUpMenu');
    end;
  end
  else
    Message.Result := DefWindowProc(TrayWnd, Message.MSG, Message.WParam,
      Message.LParam);
end;

procedure TFormConexao.SetColors;
begin
  RGuestID.Fill.Color := SECONDARY_COLOR;
  RMachineID.Fill.Color := SECONDARY_COLOR;
  rQuality.Fill.Color := SECONDARY_COLOR;
  RPassword.Fill.Color := SECONDARY_COLOR;
  PhLogo.Fill.Color := PRIMARY_COLOR;
  phOptions.Fill.Color := PRIMARY_COLOR;
  btnConectar.Fill.Color := PRIMARY_COLOR;
end;

procedure TFormConexao.SetOffline;
begin
  LMachineID.Text      := 'Offline';
  LPassword.Text       := LMachineID.Text;
  btnConectar.Enabled  := False;
  LbtnConectar.Enabled := btnConectar.Enabled;
  tmrIntervalo.Enabled := False;
  tmrClipboard.Enabled := False;
  MudarStatusConexao(1, 'Offline');
end;


Procedure TFormConexao.Kick;
Begin
 Conexao.DisconnectAllPeers;
End;

Procedure TFormConexao.KillThreads;
Begin
 If Assigned(SendDataThread) Then //Thread da Area de Trabalho
  Begin
   Try
    SendDataThread.Kill;
   Except
   End;
   {$IFDEF FPC}
    WaitForThreadTerminate(SendDataThread.Handle, 100);
   {$ELSE}
    {$IF Not Defined(HAS_FMX)}
     WaitForSingleObject  (SendDataThread.Handle, 100);
    {$IFEND}
   {$ENDIF}
   FreeAndNil(SendDataThread);
  End;
End;

procedure TFormConexao.SetPeerDisconnected;
begin
  Kick;
//  KillThreads;
  btnConectar.Enabled  := True;
  LbtnConectar.Enabled := btnConectar.Enabled;
  tmrIntervalo.Enabled := False;
  tmrClipboard.Enabled := False;
  MudarStatusConexao(1, 'Peer Disconnected');
  If Conexao.ConnectionList.Count = 0 Then
   Windows.ShowWindow(FormToHWND(Application.MainForm), SW_RESTORE);
end;

procedure TFormConexao.SetOnline;
begin
  LMachineID.Text      := Conexao.SessionID;
  LPassword.Text       := Conexao.SessionPWD;
  btnConectar.Enabled  := True;
  LbtnConectar.Enabled := btnConectar.Enabled;
  MudarStatusConexao(3, 'Online');
end;

Function SerialNumHardDisk(FDrive : String): String;
Var
 Serial,
 DirLen,
 Flags   : DWORD;
 DLabel  : Array [0 .. 11] of Char;
Begin
 Try
  GetVolumeInformation(PChar(Copy(FDrive, 1, 1) + ':\'), DLabel, 12, @Serial, DirLen, Flags, nil, 0);
  Result := IntToHex(Serial, 8);
 Except
  Result := '';
 End;
End;

Function MacAddress : String;
var
 AdapterList: TLanaEnum;
 Adapter: TAdapterStatus;
 NCB1, NCB2: TNCB;
 Lana: AnsiChar;
begin
  FillChar(NCB1, SizeOf(NCB1), 0);
  NCB1.ncb_command := Char(NCBENUM);
  NCB1.ncb_buffer := @AdapterList;
  NCB1.ncb_length := SizeOf(AdapterList);
  Netbios(@NCB1);
  if Byte(AdapterList.length) > 0 then
  begin
    //AdapterList.lana[] contiene i vari dispositivi hardware
    Lana := AdapterList.lana[0];
    FillChar(NCB2, SizeOf(NCB2), 0);
    NCB2.ncb_command := Char(NCBRESET);
    NCB2.ncb_lana_num := Lana;
    if Netbios(@NCB2) <> Char(NRC_GOODRET) then begin Result := 'mac non trovato'; Exit; end;
    FillChar(NCB2, SizeOf(NCB2), 0);
    NCB2.ncb_command := Char(NCBASTAT);
    NCB2.ncb_lana_num := Lana;
    NCB2.ncb_callname := '*';
    FillChar(Adapter, SizeOf(Adapter), 0);
    NCB2.ncb_buffer := @Adapter;
    NCB2.ncb_length := SizeOf(Adapter);
    if Netbios(@NCB2) <> Char(NRC_GOODRET) then begin Result := 'mac non trovato'; Exit; end;
    Result := IntToHex(Byte(Adapter.adapter_address[0]), 2) + '-' +
              IntToHex(Byte(Adapter.adapter_address[1]), 2) + '-' +
              IntToHex(Byte(Adapter.adapter_address[2]), 2) + '-' +
              IntToHex(Byte(Adapter.adapter_address[3]), 2) + '-' +
              IntToHex(Byte(Adapter.adapter_address[4]), 2) + '-' +
              IntToHex(Byte(Adapter.adapter_address[5]), 2);
  end
  else Result := 'mac non trovato';
end;

Procedure TFormConexao.OnBeforeConnect(Sender            : TObject;
                                       Var WelcomeString : String);
Var
 vDrive : String;
Begin
 vDrive        := Uppercase(Copy(ParamStr(0), 1, 1));
 Welcomestring := MacAddress + '|' + SerialNumHardDisk(vDrive);
End;

procedure TFormConexao.OnServerLogin(Sender: TObject);
begin
 TThread.Synchronize(Nil, Procedure
                          Begin
                           SetOnline;
                           aConnection := Conexao.Connection;
                          End);
end;

procedure TFormConexao.OnConnect(Sender: TObject);
begin
 //SetOnline;
 Application.ProcessMessages;
end;

procedure TFormConexao.OnBeginTransaction(Connection        : String;
                                          Var ClientID,
                                          Alias             : String);
Begin
 aFirstCapture := True;
 aIncSprite    := 0;
 If Not Assigned(FormSenha) Then
  FormSenha := TFormSenha.Create(Self);
 If aLastPass <> '' Then
  Begin
   FormSenha.EPassword.Text := aLastPass;
   aLastPass := '';
   FormSenha.LOkButton.OnClick(Nil);
  End
 Else
  FormSenha.Showmodal;
End;

procedure TFormConexao.AccessDenied;
Begin
 btnConectar.Enabled  := True;
 LbtnConectar.Enabled := btnConectar.Enabled;
 MudarStatusConexao(3, Format('Id "%s" denied...', [EGuestID.Text]));
End;

procedure TFormConexao.OnBeginTransactionError(Connection : String);
Begin
 SetOnline;
 MudarStatusConexao(2, Format('Id "%s" not found...', [Connection]));
End;

procedure TFormConexao.OnDisconnect(Sender: TObject);
begin
 TThread.Synchronize(Nil, Procedure
                          Begin
                           SetOffline;
                          End);
end;

Procedure TFormConexao.OnKeyboardCapture (Command           : String);
Begin
 OnMouseCapture(Command);
End;

Procedure TFormConexao.OnMouseCapture    (Command           : String);
Begin
 ExecuteCommand(Command);
End;

Procedure TFormConexao.OnIncommingConnect(Connection        : String;
                                          Var ClientID,
                                          ClientPassword,
                                          Alias             : String);
Begin
 MudarStatusConexao(1, Format('IncommingConnect "%s"...', [Connection]));
End;

Procedure TFormConexao.OnInternalCommand(InternalCommand   : TInternalCommand;
                                         Connection,
                                         ID,
                                         Command           : String);
 Function MonitorCountPack : TAegysBytes;
 Var
  aPack : TPackClass;
 Begin
  SetLength(Result, 0);
  aPack := TPackClass.Create;
  Try
   aPack.Dest        := Connection;
   aPack.DataCheck   := tdcAsync;
   aPack.CommandType := tctMonitor;
   aPack.Command     := cGetMonitorCount + IntToStr(Screen.DisplayCount);
  Finally
   Result := aPack.ToBytes;
   FreeAndNil(aPack);
  End;
 End;
Begin
 Case InternalCommand Of
  ticGetMonitorCount : Begin
                        If (Command = '') Then
                         Begin
                          If Assigned(Conexao) Then
                           Conexao.SendCommand(Connection, MonitorCountPack);
                         End
                        Else
                         Begin
                          aMonitorCount := Command;
                          If Assigned(FormTelaRemota) Then
                           FormTelaRemota.AddItems(StrToInt(aMonitorCount));
                         End;
                       End;
  ticChangeMonitor   : Begin
                        aMonitor := Command;
                       End;
 End;
End;

Function TFormConexao.OnPulseData(aPack       : TAegysBytes;
                                  CommandType : TCommandType = tctScreenCapture) : Boolean;
Begin
 If Assigned(Conexao) Then
  Begin
   Result := Conexao.Active;
   If Result Then
    Begin
     If CommandType = tctScreenCapture Then
      Conexao.SendBytes(aPack)
     Else
      Conexao.SendCommand(aConnection, aPack);
//     Processmessages;
    End;
  End;
End;

Procedure TFormConexao.OnProcessData(aPackList  : TPackList;
                                     aFullFrame : Boolean);
Var
 aPackClass  : TPackClass;
 Procedure aCapture;
 Var
  vMonitor : String;
 Begin
  Try
   vMonitor := aMonitor;
   If vMonitor = '' Then
    vMonitor := '0';
   GetScreenToMemoryStream(aPackClass, vDrawCursor, pf16bit, vMonitor, aFullFrame);
  Finally
   Processmessages;
  End;
 End;
Begin
 Try
  aPackClass              := Nil;
  aCapture;
  If Assigned(aPackClass)  Then
   Begin
    TThread.Synchronize(Nil, Procedure
                             Begin
                              If Not Assigned(Conexao) Then
                               Exit;
                              If Assigned(aPackList)   Then
                               aPackList.Add(aPackClass)
                              Else
                               Exit;
                              Processmessages;
                             End);
   End;
 Except
 End;
End;

Procedure TFormConexao.OnAccessGranted(Connection        : String;
                                       Var ClientID,
                                       ClientPassword,
                                       SpecialData       : String); //tela remota
 Function MonitorCountPack : TAegysBytes;
 Var
  aPack : TPackClass;
 Begin
  SetLength(Result, 0);
  aPack := TPackClass.Create;
  Try
   aPack.Dest        := Connection;
   aPack.DataCheck   := tdcAsync;
   aPack.CommandType := tctMonitor;
   aPack.Command     := cGetMonitorCount;
  Finally
   Result := aPack.ToBytes;
   FreeAndNil(aPack);
  End;
 End;
Begin
 Try
  //ResolucaoLargura
  vFPS := 0;
  If Not Assigned(FormTelaRemota) Then
   FormTelaRemota                 := TFormTelaRemota.Create(Self);
  vClientID                       := ClientID;
  FormTelaRemota.Caption          := Format(cCaptureTitle, [vClientID, vFPS]);
  FormTelaRemota.Connection       := Connection;
  FormTelaRemota.vActualIDConnected := vClientID + '<|>' + ClientPassword;
  aConnection                     := FormTelaRemota.Connection;
  Conexao.SendCommand(aConnection, MonitorCountPack);
  FormTelaRemota.Show;
 Finally

 End;
End;

Procedure TFormConexao.OnPeerConnected(Connection        : String;
                                       Var ClientID,
                                       ClientPassword,
                                       Alias             : String); //Captura de tela
Begin
 If (Not Assigned(SendDataThread))     Or
    (Conexao.ConnectionList.Count = 1) Then
  Begin
   If Not Assigned(SendDataThread) Then
    Begin
     SendDataThread                := TAegysMotorThread.Create;
     aConnection                   := Connection;
     Try
      Windows.ShowWindow(FormToHWND(Self), SW_Minimize);
      SendDataThread.OnProcessData := OnProcessData;
      SendDataThread.OnPulseData   := OnPulseData;
      SendDataThread.Resume;
     Finally

     End;
    End
   Else
    SendDataThread.GetFullFrame;
  End
 Else
  SendDataThread.GetFullFrame;
End;

Procedure TFormConexao.OnPeerDisconnected(Connection        : String;
                                          Var ClientID,
                                          ClientPassword,
                                          Alias             : String); //Captura de tela
Begin
 SetPeerDisconnected;
End;

Procedure TFormConexao.OnPeerKick(Connection      : String;
                                  Var ClientID,
                                  ClientPassword,
                                  Alias           : String);
Begin
 If Assigned(FormTelaRemota) Then
  FormTelaRemota.Close;
 If Conexao.ConnectionList.Count = 0 Then
  KillThreads;  //TODO XyberX, Pois isso daqui para a Captura geral...
 btnConectar.Enabled  := True;
 LbtnConectar.Enabled := btnConectar.Enabled;
 tmrIntervalo.Enabled := False;
 tmrClipboard.Enabled := False;
 MudarStatusConexao(1, 'Peer Disconnected');
 If Conexao.ConnectionList.Count = 0 Then
  Windows.ShowWindow(FormToHWND(Self), SW_RESTORE);
End;

Procedure TFormConexao.OnScreenCapture(Connection,
                                       ID,
                                       Command         : String;
                                       MultiPack       : Boolean;
                                       PackCount       : AeInteger;
                                       aBuf            : TAegysBytes);
Var
 ArrayOfPointer : TArrayOfPointer;
 vStream        : TStream;
 vResultMemoryStream : TMemoryStream;
 vImageFile,
 vAltura,
 vLargura       : String;
 vGetFPS,
 aPackCountData,
 I, pRectTop,
 pRectLeft,
 pRectBottom,
 pRectRight,
 aBuffPosition  : AeInteger;
 aSizeData      : AeInt64;
 bBuf           : TAegysBytes;
 aOldbmpPart,
 MybmpPart      : Vcl.Graphics.TBitmap;
 vStreamBitmap  : TMemoryStream;
 MyConnection   : TAegysMyConnection;
 Function GetFps : Integer;
 Begin
  Result     := 0;
  vFinalTime := Now;
  If MilliSecondsBetween(vInitTime, vFinalTime) >= 1000 Then
   Begin
    Result    := vFPS;
    vFPS      := 0;
    vInitTime := Now;
   End
  Else
   Inc(vFPS);
 End;
 Procedure ResizeScreen(Altura, Largura : Integer);
 Var
  vScreenSizeFact,
  vFatorA : Integer;
  Function MDC(a, b : Integer) : Integer;
  Var
   resto : Integer;
  Begin
   While b <> 0 Do
    Begin
     resto := a mod b;
     a     := b;
     b     := resto;
    End;
   Result := a;
  End;
  Function ProporcaoTela(Direita, Topo : Integer) : Integer;
  Begin
   Result := Round(Direita / MDC(Direita, Topo))
  End;
 Begin
  If ((vResolucaoAltura  >  0)  And
      (vResolucaoLargura >  0)) And
     ((vResolucaoAltura  <> vOldResolucaoAltura)   Or
      (vResolucaoLargura <> vOldResolucaoLargura)) Then
   Begin
    vOldResolucaoAltura   := vResolucaoAltura;
    vOldResolucaoLargura  := vResolucaoLargura;
    If vOldResolucaoAltura  > vOldResolucaoLargura Then
     Begin
      vFatorA          := Round((vOldResolucaoLargura  / vOldResolucaoAltura)  * 100);
      FormTelaRemota.Width  := Round((Screen.Height    / 100) * vFatorA);
      If vOldResolucaoAltura > Screen.Height Then
       vFatorA          := Round((Screen.Height        / vOldResolucaoAltura)  * 100)
      Else
       vFatorA          := Round((vOldResolucaoAltura  / Screen.Height)        * 100);
      vScreenSizeFact  := Round((Screen.Height / 100)  * vFatorA);
      FormTelaRemota.Height := vScreenSizeFact + Round((Screen.Height - vScreenSizeFact) / 2);
      FormTelaRemota.Top    := Round((Screen.Height / 2) - (FormTelaRemota.Height / 2));
      FormTelaRemota.Left   := Round(Screen.Width - FormTelaRemota.Width);
     End
    Else
     Begin
      If vOldResolucaoAltura  > Screen.Height Then
       vFatorA              := Round((Screen.Height         / vOldResolucaoAltura)   * 100)
      Else If vOldResolucaoAltura = Screen.Height Then
       vFatorA              := Round(((vOldResolucaoAltura  - 100) / Screen.Height)  * 100)
      Else
       vFatorA              := Round((vOldResolucaoAltura   / Screen.Height)         * 100);
      FormTelaRemota.Height := Round((Screen.Height         / 100) * vFatorA);
      If vOldResolucaoLargura  > Screen.Width Then
       vFatorA              := Round((Screen.Width          / vOldResolucaoLargura)  * 100)
      Else If vOldResolucaoLargura  = Screen.Width Then
       vFatorA              := Round(((vOldResolucaoLargura - 50) / Screen.Width)    * 100)
      Else
       vFatorA              := Round((vOldResolucaoLargura   / Screen.Width)         * 100);
      vScreenSizeFact       := Round((Screen.Width          / 100) * vFatorA);
      FormTelaRemota.Width  := vScreenSizeFact;
      FormTelaRemota.Top    := Round((Screen.Height / 2) - (FormTelaRemota.Height / 2));
      FormTelaRemota.Left   := Round((Screen.Width  / 2) - (FormTelaRemota.Width  / 2));
     End;
   End;
 End;
 Function ResizeImage(Height, Width : Integer) : TStream;
 Var
  bmpA, bmpB: TBitmap;
  src, trg: TRectF;
 Begin
  bmpA := Nil;
  bmpB := Nil;
  Result := TMemoryStream.Create;
  Try
   vStream.Position := 0;
   bmpA := TBitmap.Create;
   bmpA.LoadFromStream(vStream);
   vStream.Position := 0;
   bmpB:= TBitmap.Create;
   bmpB.SetSize(Width, Height);
   src := RectF(0, 0, bmpA.Width, bmpA.Height);
   trg := RectF(0, 0, Width, Height);
   bmpB.Canvas.BeginScene;
   bmpB.Canvas.DrawBitmap(bmpA, src, trg, 1);
   bmpB.Canvas.EndScene;
   bmpB.SaveToStream(Result);
   Result.Position := 0;
  Finally
   FreeAndNil(bmpA);
   FreeAndNil(bmpB);
  End;
 End;
Begin
 If Assigned(FormTelaRemota) Then
  Begin
   vStream := TMemoryStream.Create;
   Try
    If Command <> '' Then
     Begin
      ArrayOfPointer := [@vAltura, @vLargura];
      ParseValues(Command, ArrayOfPointer);
      If vAltura <> '' Then
       vResolucaoAltura  := Round(StrToFloat(vAltura));
      If vLargura <> '' Then
       vResolucaoLargura := Round(StrToFloat(vLargura));
     End;
    Processmessages;
    If cCompressionData Then
     ZDecompressBytesStream(aBuf, vStream)
    Else
     Begin
      vStream.Write(aBuf[0], Length(aBuf));
      vStream.Position := 0;
     End;
    Processmessages;
    Try
//     If (vActualImage.Size = 0) Then
//      Begin
     vStream.Position := 0;
//       vActualImage.Clear;
//       vActualImage.CopyFrom(vStream, vStream.Size);
//       vStream.Position := 0;
//      End
//     Else
//      Begin
//       vActualImage.Position := 0;
//       vResultMemoryStream := TMemoryStream.Create;
//       ResumeStreamB(vActualImage, vResultMemoryStream, TMemoryStream(vStream));
//       vResultMemoryStream.Position := 0;
//       vActualImage.Clear;
//       vActualImage.CopyFrom(vResultMemoryStream, vResultMemoryStream.Size);
//       vResultMemoryStream.Position := 0;
//       TMemoryStream(vStream).Clear;
//       vStream.CopyFrom(vResultMemoryStream, vResultMemoryStream.Size);
//       vStream.Position  := 0;
//       FreeAndnil(vResultMemoryStream);
//      End;
     ResizeScreen(vResolucaoAltura, vResolucaoLargura);
     If (aFirstCapture) And
        (aIncSprite = cMaxSpriteCap) Then
      Begin
       aIncSprite := 0;
       vStreamBitmap := TMemoryStream(ResizeImage(80, 80));
       Try
        vImageFile                      := EncodeStream(vStreamBitmap);
        MyConnection                    := Conexao.FavConnectionList.GetConnection(Connection, ID);
        If Assigned(MyConnection) Then
         Begin
          MyConnection.ConnectionLastShot := vImageFile;
          If Assigned(fFavoritos) Then
           fFavoritos.SetImage(MyConnection, MyConnection.ConnectionLastShot);
         End;
        aFirstCapture := False;
       Finally
        FreeAndNil(vStreamBitmap);
        Processmessages;
       End;
      End
     Else If (aFirstCapture) Then
      Inc(aIncSprite);
     FormTelaRemota.imgTelaRemota.Fill.Bitmap.Bitmap.LoadFromStream(vStream); //.Bitmap.LoadFromStream(vStream);
     vGetFPS := GetFps;
     If vGetFPS > 0 Then
      FormTelaRemota.Caption := Format(cCaptureTitle, [vClientID, vGetFPS]);
    Finally
     FreeAndNil(vStream);
     Processmessages;
    End;
   Finally
    Processmessages;
   End;
  End;
End;

Procedure TFormConexao.OnPeerList(InternalCommand      : TInternalCommand;
                                  Command              : String);
Begin
  TThread.Synchronize(Nil, Procedure
                           Var
                            vCommand,
                            vMyConfigs : String;
                            Procedure DecodeConfigs(aMyConfigs : String);
                            Var
                             vTempData : String;
                            Begin
                             vTempData      := DecodeStrings(aMyConfigs);
                             aMyAlias       := Copy(vTempData, 1, Pos('|', vTempData) -1);
                             Delete(vTempData, 1, Pos('|', vTempData));
                             aUnAssistConn  := Copy(vTempData, 1, Pos('|', vTempData) -1) = '1';
                             Delete(vTempData, 1, Pos('|', vTempData));
                             aMyFixPass     := Copy(vTempData, 1, Pos('|', vTempData) -1);
                             Delete(vTempData, 1, Pos('|', vTempData));
                             aMyGroup       := vTempData;
                            End;
                           Begin
                            vMyConfigs := '';
                            vCommand   := Command;
                            Case InternalCommand Of
                             ticPeerOn      : Begin
                                               Conexao.FavConnectionList.FromString(vCommand, tlct_PeerOn);
                                               fFavoritos.CreatePanels(Conexao.FavConnectionList);
                                              End;
                             ticPeerOff     : Begin
                                               Conexao.FavConnectionList.FromString(vCommand, tlct_PeerOff);
                                               fFavoritos.CreatePanels(Conexao.FavConnectionList);
                                              End;
                             ticPeerNewList : Begin
                                               If Pos(cMyConfigs, vCommand) > 0 Then
                                                Begin
                                                 vMyConfigs := Copy(vCommand, Pos(cMyConfigs, vCommand) + Length(cMyConfigs), Length(vCommand));
                                                 Delete(vCommand, Pos(cMyConfigs, vCommand), Length(vCommand));
                                                End;
                                               DecodeConfigs(vMyConfigs);
                                               Conexao.FavConnectionList.FromString(vCommand, tlct_NewList);
                                               fFavoritos.CreatePanels(Conexao.FavConnectionList);
                                              End;
                            End
                           End);
End;

Procedure TFormConexao.SetSockets;
Var
// CFG : TSQLiteConfig;
 host : string;
Begin
// CFG := TSQLiteConfig.Create;
 Try
  If SERVIDOR <> '' Then
   host := SERVIDOR;
//  Else
//   host := iif(CFG.getValue(SERVER) = '', '0.0.0.0', CFG.getValue(SERVER));
  Conexao.Disconnect;
  Conexao.OnBeforeConnect         := OnBeforeConnect;
  Conexao.OnConnect               := OnConnect;
  Conexao.OnDisconnect            := OnDisconnect;
  Conexao.OnServerLogin           := OnServerLogin;
  Conexao.OnBeginTransactionError := OnBeginTransactionError;
  Conexao.OnBeginTransaction      := OnBeginTransaction;
  Conexao.OnIncommingConnect      := OnIncommingConnect;
  Conexao.OnAccessGranted         := OnAccessGranted;
  Conexao.OnPeerConnected         := OnPeerConnected;
  Conexao.OnPeerDisconnected      := OnPeerDisconnected;
  Conexao.OnPeerList              := OnPeerList;
  Conexao.OnScreenCapture         := OnScreenCapture;
  Conexao.OnKeyboardCapture       := OnKeyboardCapture;
  Conexao.OnMouseCapture          := OnMouseCapture;
  Conexao.OnPeerKick              := OnPeerKick;
  Conexao.OnClientInternalCommand := OnInternalCommand;
  Conexao.OnAccessDenied          := AccessDenied;
  Conexao.OnFileTransfer          := OnFileTransfer;
  Conexao.Host                    := Host;
  Conexao.Port                    := PORTA;
  Sleep(FOLGAPROCESSAMENTO);
  Conexao.Connect;
 Finally
//  FreeAndNil(CFG);
 End;
End;

procedure TFormConexao.SetTrayIcon;
begin
  HideApponTaskbar;
  TrayWnd := AllocateHWnd(TrayWndProc); // Alocate the wndProc
  with TrayIconData do
  begin // Instaciate
    cbSize := SizeOf;
    Wnd := TrayWnd;
    uID := 1;
    uFlags := NIF_MESSAGE + NIF_ICON + NIF_TIP;
    uCallbackMessage := WM_ICONTRAY;
    hIcon := GetClassLong(FmxHandleToHWND(self.Handle), GCL_HICONSM);
    StrPCopy(szTip, 'Aegys Remote Acess');
  end;
  // creating the icon
  if not TrayIconAdded then
    TrayIconAdded := Shell_NotifyIcon(NIM_ADD, @TrayIconData);
  if self.Visible then
  begin
    self.Hide;
    isvisible := False;
  end;
end;

procedure TFormConexao.ShowAppOnTaskbar;
var
  hAppWnd: HWND;
  ExStyle: LONG_PTR;
begin
  hAppWnd := FMX.Platform.Win.ApplicationHWND();
  ShowWindow(hAppWnd, SW_HIDE);
  ExStyle := GetWindowLongPtr(hAppWnd, GWL_EXSTYLE);
  SetWindowLongPtr(hAppWnd, GWL_EXSTYLE, (ExStyle and WS_EX_TOOLWINDOW) or
    WS_EX_APPWINDOW);
  ShowWindow(hAppWnd, SW_SHOW);
end;

procedure TFormConexao.EGuestIDTyping(Sender: TObject);
begin
  TEdit(Sender).Text := MascaraID(TEdit(Sender).Text, '99-999-999');
  TEdit(Sender).GoToTextEnd;
end;

Procedure TFormConexao.tmrIntervaloTimer(Sender: TObject);
Begin
 If (Conexao.SessionTime > INTERVALOCONEXAO) Then
  Begin
   Processmessages;
   If (FormTelaRemota.Visible) Then
    FormTelaRemota.Close
   Else
    Begin
     SetOffline;
     Conexao.Disconnect;
     SetSockets;
    End;
  End;
 Conexao.SessionTime := Conexao.SessionTime + 1;
End;

Initialization
 WndProcHook  := SetWindowsHookEx(WH_CALLWNDPROCRET, @WndProc, 0, GetCurrentThreadId);
 vActualImage := TMemoryStream.Create;

Finalization
 UnhookWindowsHookEx(WndProcHook);
 FreeAndNil(vActualImage);

End.
