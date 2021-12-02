unit uFormConexao;

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
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Actions,
  System.Win.ScktComp, System.Messaging,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Objects, FMX.Controls.Presentation, FMX.Layouts, FMX.ActnList,
  FMX.Ani, FMX.TabControl, FMX.ListBox, FMX.Menus,
  uCtrl_Threads, uCtrl_Conexao, uFunctions, CCR.Clipboard, windows, shellapi,
  Messages, uSQLiteConfig;

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
    tmrReconnect: TTimer;
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
    Pm_systemtray: TPopupMenu;
    MenuItem1: TMenuItem;
    TmrSystemTray: TTimer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure actPasteIDExecute(Sender: TObject);
    procedure actConnectExecute(Sender: TObject);
    procedure actCopyIDExecute(Sender: TObject);
    procedure actCopyPasswordExecute(Sender: TObject);
    procedure tmrClipboardTimer(Sender: TObject);
    procedure tmrReconnectTimer(Sender: TObject);
    procedure tmrIntervaloTimer(Sender: TObject);
    procedure EGuestIDTyping(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sbOptionsClick(Sender: TObject);
    procedure TmrSystemTrayTimer(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure FormShow(Sender: TObject);
  private
    Locale: TLocale;
    TrayWnd: HWND;
    TrayIconData: TNotifyIconData;
    TrayIconAdded: Boolean;
    isvisible: Boolean;
    function MascaraID(AText, AMascara: string): string;
    procedure Translate;
    procedure SetColors;
    function ClipboardGetAsFile: string;
    procedure TrayWndProc(var Message: TMessage);
    procedure ShowAppOnTaskbar;
    procedure HideApponTaskbar;
    procedure SetTrayIcon;
  public
    procedure LimparConexao;
    procedure MudarStatusConexao(AStatus: Integer; AMensagem: string);
    procedure SetOffline;
    procedure SetOnline;
  end;

var
  FormConexao: TFormConexao;
  Conexao: TConexao;
  CF_FILE: Integer;
  mx, my: Single;

const
  WM_ICONTRAY = WM_USER + 1;

implementation

{$R *.fmx}

uses uFormTelaRemota, uFormArquivos, uFormChat, FMX.Clipboard,
  System.IOUtils, System.Rtti, uLibClass,
  uConstants, BCrypt, System.DateUtils, uHttpClass,
  System.Threading, FMX.Platform.Win, uFormConfig;

Procedure TFormConexao.LimparConexao;
Begin
  Conexao.ResolucaoLargura := 986;
  Conexao.ResolucaoAltura := 600;
  FormTelaRemota.tCapturarComandos.Enabled := True;
  FormTelaRemota.tCapturarComandos.Enabled := False;
  // FormTelaRemota.imgTelaRemota.Fill.Kind            := TbrushKind.Bitmap;
  // FormTelaRemota.imgTelaRemota.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
  FormTelaRemota.imgTelaRemota.Fill.Bitmap.Bitmap.Assign
    (FormTelaRemota.imgTelaInicial.Bitmap);
  FormArquivos.btnDownload.Enabled := True;
  FormArquivos.btnUpload.Enabled := True;
  FormArquivos.pgbDownload.Value := 0;
  FormArquivos.pgbUpload.Value := 0;
  FormArquivos.LDownloadSize.Text := Locale.GetLocale(MAIN, 'Size');
  FormArquivos.LUploadSize.Text := Locale.GetLocale(MAIN, 'Size');
  FormArquivos.EFolder.Text := 'C:\';
  FormArquivos.lstArquivos.Items.Clear;
  If (FormArquivos.Visible) Then
    FormArquivos.Close;
  FormChat.Width := 230;
  FormChat.Height := 340;
  FormChat.Left := Trunc(Screen.WorkAreaWidth - FormChat.Width);
  FormChat.Top := Trunc(Screen.WorkAreaHeight - FormChat.Height);
  FormChat.lstMensagens.Clear;
  If (FormChat.Visible) Then
    FormChat.Close;
End;

Procedure TFormConexao.tmrClipboardTimer(Sender: TObject);
Var

  FileStream: TFileStream;
  s, FileName: string;
  ClipFormat: TClipboardFormat;
Begin
  tmrClipboard.Enabled := Conexao.Visualizador;
  try

    if Clipboard.HasText then
    begin
      if (Conexao.OldClipboardText <> Clipboard.AsText) Then
      Begin
        Conexao.OldClipboardText := Clipboard.AsText;
        Conexao.SocketPrincipal.Socket.SendText('<|REDIRECT|><|CLIPBOARD|>' +
          Conexao.OldClipboardText + '<|END|>');
      End;
    end
    else
    begin
      for s in Clipboard.GetFileNames do
      begin
        if s <> '' then
          if (Conexao.OldClipboardFile <> s) And (FileExists(s)) Then
          begin
            FileStream := TFileStream.Create(s, fmOpenRead);
            FileName := ExtractFileName(s);
            Conexao.OldClipboardFile := s;
            Conexao.SocketArquivos.Socket.SendText('<|DIRECTORYTOSAVE|>' +
              FileName + '<|><|SIZE|>' + intToStr(FileStream.Size) + '<|END|>');
            FileStream.Position := 0;
            Conexao.SocketArquivos.Socket.SendStream(FileStream);
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
  if Assigned(Conexao) then
    FreeAndNil(Conexao);
  Application.Terminate;
end;

procedure TFormConexao.FormCreate(Sender: TObject);
var
  CFG: TSQLiteConfig;
begin
  CF_FILE := RegisterClipboardFormat('FileName');
  // inicializando os objetos
  Locale := TLocale.Create;
  Conexao := TConexao.Create;
  // --------------------------
  SetColors;
  SetOffline;
  Translate;

  isvisible := True;

  // load confg
  CFG := TSQLiteConfig.Create;
  try
    lyGuestID.Visible := not StrToIntDef(CFG.getValue(QUICK_SUPPORT), 0)
      .ToBoolean;
    lyConnect.Visible := not StrToIntDef(CFG.getValue(QUICK_SUPPORT), 0)
      .ToBoolean;
    lyQuality.Visible := not StrToIntDef(CFG.getValue(QUICK_SUPPORT), 0)
      .ToBoolean;
  finally
    CFG.DisposeOf;
  end;
end;

procedure TFormConexao.FormDestroy(Sender: TObject);
begin
  Locale.DisposeOf;
end;

procedure TFormConexao.FormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Single);
begin
  mx := X;
  my := Y;
end;

procedure TFormConexao.FormShow(Sender: TObject);
var
  CFG: TSQLiteConfig;
begin
  CFG := TSQLiteConfig.Create;
  try
    if StrToIntDef(CFG.getValue(ENABLE_SYSTRAY), 0).ToBoolean then
      if isvisible then
        SetTrayIcon;
  finally
    CFG.DisposeOf;
  end;
end;

procedure TFormConexao.HideApponTaskbar;
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
end;

procedure TFormConexao.sbOptionsClick(Sender: TObject);
begin
  Application.CreateForm(TfConfig, fConfig);
  fConfig.show;
  fConfig.CallBackConfig := Translate;
end;

procedure TFormConexao.actPasteIDExecute(Sender: TObject);
begin
  EGuestID.Text := MascaraID(TRDLib.ColarTexto, '999-999-999');
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
    If not(LlyGuestIDCaption.Text = '   -   -   ') then
    begin
      if (LlyGuestIDCaption.Text = Conexao.ID) then
        MessageBox(0, Locale.GetLocaleDlg(DLGS, 'ErrorSelfConnect'),
          Locale.GetLocaleDlg(DLGS, 'RemoteSupport'),
          MB_ICONASTERISK + MB_TOPMOST)
      else
      begin
        LbtnConectar.Enabled := False;
        Conexao.SocketPrincipal.Socket.SendText('<|FINDID|>' + EGuestID.Text +
          '<|END|>');
        btnConectar.Enabled := False;
        MudarStatusConexao(1, Locale.GetLocale(MSGS, 'SearchingID'));
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

procedure TFormConexao.tmrReconnectTimer(Sender: TObject);
begin
  Conexao.ReconectarSocket;
end;

procedure TFormConexao.Translate;
begin
  self.Caption := Locale.GetLocale(FRMS, 'MainTitle');
  LSubTitle.Text := Locale.GetLocale(FRMS, 'MainSubTitle');
  LVersion.Text := Format(Locale.GetLocale(MAIN, 'Version'),
    [TRDLib.GetAppVersionStr]);
  LlyMachineIDCaption.Text := Locale.GetLocale(FRMS, 'MainMachineID');
  LlyPasswordCaption.Text := Locale.GetLocale(FRMS, 'MainPassword');
  LlyGuestIDCaption.Text := Locale.GetLocale(FRMS, 'MainGuestID');
  LlyResolutionCaption.Text := Locale.GetLocale(FRMS, 'MainResolution');
  LbtnConectar.Text := Locale.GetLocale(FRMS, 'MainConnectButton');
  Locale.GetLocale(cbQuality, tcbQuality);
  Conexao.ReconectarSocket(True);
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
  LMachineID.Text := Locale.GetLocale(MSGS, 'Disconnected');
  LPassword.Text := Locale.GetLocale(MSGS, 'Disconnected');
  btnConectar.Enabled := False;
  LbtnConectar.Enabled := btnConectar.Enabled;
  tmrIntervalo.Enabled := False;
  tmrClipboard.Enabled := False;
end;

procedure TFormConexao.SetOnline;
begin
  LMachineID.Text := Conexao.ID;
  LPassword.Text := Conexao.SenhaGerada;
  btnConectar.Enabled := True;
  LbtnConectar.Enabled := btnConectar.Enabled;
end;

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

procedure TFormConexao.TmrSystemTrayTimer(Sender: TObject);
begin
  // TmrSystemTray.Enabled := False;
  // if systemtray then
  // self.Hide;
end;

procedure TFormConexao.EGuestIDTyping(Sender: TObject);
begin
  TEdit(Sender).Text := MascaraID(TEdit(Sender).Text, '999-999-999');
  TEdit(Sender).GoToTextEnd;
end;

procedure TFormConexao.tmrIntervaloTimer(Sender: TObject);
begin
  if (Conexao.Intervalo > INTERVALOCONEXAO) then
  begin
    if (FormTelaRemota.Visible) then
      FormTelaRemota.Close
    else
    begin
      SetOffline;
      Conexao.FecharSockets;
      Conexao.ReconectarSocket;
    end;
  end;
  Conexao.Intervalo := Conexao.Intervalo + 1;
end;

end.
