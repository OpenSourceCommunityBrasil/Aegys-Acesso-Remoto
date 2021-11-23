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
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Objects, FMX.Controls.Presentation, FMX.Layouts, System.Actions,
  FMX.ActnList, FMX.Ani, uCtrl_Threads, FireDAC.Stan.Intf, System.Win.ScktComp,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, uDWAbout, uRESTDWPoolerDB, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, uDWConstsData, FMX.TabControl,
  uCtrl_Conexao, uRESTDWServerEvents, uLocaleFunctions;

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
    lyPassword: TLayout;
    RPassword: TRectangle;
    LlyPasswordCaption: TLabel;
    Layout8: TLayout;
    LPassword: TLabel;
    sbPasswordCopy: TSpeedButton;
    PhPasswordCopy: TPath;
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
    Divider: TLine;
    lyStatus: TLayout;
    PhStatus: TPath;
    LStatus: TLabel;
    tmrReconnect: TTimer;
    tmrIntervalo: TTimer;
    tmrClipboard: TTimer;
    actCopyID: TAction;
    actCopyPassword: TAction;
    tcPrincipal: TTabControl;
    tabAcesso: TTabItem;
    RMainBackground: TRectangle;
    Layout4: TLayout;
    RDQuery: TRESTDWClientSQL;
    RESTDWDataBase1: TRESTDWDataBase;
    actTabChange: TChangeTabAction;
    sbPasteID: TSpeedButton;
    PhPasteID: TPath;
    actPasteID: TAction;
    LVersion: TLabel;
    recDownload: TRectangle;
    Layout16: TLayout;
    aniDownload: TAniIndicator;
    phOptions: TPath;
    sbOptions: TSpeedButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure actPasteIDExecute(Sender: TObject);
    procedure actConnectExecute(Sender: TObject);
    procedure actCopyIDExecute(Sender: TObject);
    procedure actCopyPasswordExecute(Sender: TObject);
    procedure tmrClipboardTimer(Sender: TObject);
    procedure tmrReconnectTimer(Sender: TObject);
    procedure tmrIntervaloTimer(Sender: TObject);
    procedure EGuestIDKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure EGuestIDTyping(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sbOptionsClick(Sender: TObject);
  private
    FHash: string;
    Locale: TLocale;
    function MascaraID(AText, AMascara: string): string;
    procedure MudarTab(TabItem: TTabItem);
    procedure VerificarAtualizacao;
    procedure Translate;
    procedure SetColors;
  public
    procedure LimparConexao;
    procedure MudarStatusConexao(AStatus: Integer; AMensagem: string);
    procedure SetOffline;
    procedure SetOnline;
  end;

var
  FormConexao: TFormConexao;
  Conexao: TConexao;

implementation

{$R *.fmx}

uses uFormTelaRemota, uFormArquivos, uFormChat, FMX.Clipboard,
  System.IOUtils, FMX.Platform, System.Rtti, uLibClass,
  Winapi.Windows, uConstants, BCrypt, System.DateUtils, uHttpClass,
  System.Threading, Winapi.ShellAPI, FMX.Platform.Win, uFormConfig;

procedure TFormConexao.LimparConexao;
begin
  Conexao.ResolucaoLargura := 986;
  Conexao.ResolucaoAltura := 600;

  FormTelaRemota.tCapturarComandos.Enabled := True;
  FormTelaRemota.tCapturarComandos.Enabled := False;
  FormTelaRemota.imgTelaRemota.WrapMode := TImageWrapMode.Fit;
  FormTelaRemota.imgTelaRemota.MultiResBitmap.Assign
    (FormTelaRemota.imgTelaRemota.MultiResBitmap);

  FormArquivos.btnDownload.Enabled := True;
  FormArquivos.btnUpload.Enabled := True;
  FormArquivos.pgbDownload.Value := 0;
  FormArquivos.pgbUpload.Value := 0;
  FormArquivos.LDownloadSize.Text := Locale.GetLocale(APP, 'Size');
  FormArquivos.LUploadSize.Text := Locale.GetLocale(APP, 'Size');

  FormArquivos.EFolder.Text := 'C:\';
  FormArquivos.lstArquivos.Items.Clear;

  if (FormArquivos.Visible) then
    FormArquivos.Close;

  FormChat.Width := 230;
  FormChat.Height := 340;

  FormChat.Left := Trunc(Screen.WorkAreaWidth - FormChat.Width);
  FormChat.Top := Trunc(Screen.WorkAreaHeight - FormChat.Height);

  FormChat.lstMensagens.Clear;

  if (FormChat.Visible) then
    FormChat.Close;
end;

procedure TFormConexao.tmrClipboardTimer(Sender: TObject);
var
  Svc: IFMXClipboardService;
  vlClip: TValue;
begin
  try
    tmrClipboard.Enabled := Conexao.Visualizador;
    if TPlatformServices.Current.SupportsPlatformService
      (IFMXClipboardService, Svc) then
    begin
      vlClip := Svc.GetClipboard;
      if not(vlClip.IsEmpty) and (vlClip.IsType<string>) and
        (Conexao.OldClipboardText <> vlClip.ToString) then
      begin
        Conexao.OldClipboardText := vlClip.ToString;
        Conexao.SocketPrincipal.Socket.SendText('<|REDIRECT|><|CLIPBOARD|>' +
          Conexao.OldClipboardText + '<|END|>');
      end;
    end;
  except
  end;
end;

procedure TFormConexao.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(Conexao) then
    FreeAndNil(Conexao);
  Application.Terminate;
end;

procedure TFormConexao.FormCreate(Sender: TObject);
begin
  // inicializando os objetos
  Locale := TLocale.Create;
  Conexao := TConexao.Create;
  // --------------------------
  tcPrincipal.TabPosition := TTabPosition.None;
  tcPrincipal.ActiveTab := tabAcesso;
  SetOffline;
  Translate;
end;

procedure TFormConexao.FormDestroy(Sender: TObject);
begin
  Locale.DisposeOf;
end;

procedure TFormConexao.FormShow(Sender: TObject);
begin
  VerificarAtualizacao;
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

procedure TFormConexao.MudarTab(TabItem: TTabItem);
begin
  actTabChange.Tab := TabItem;
  actTabChange.ExecuteTarget(self);
end;

procedure TFormConexao.sbOptionsClick(Sender: TObject);
begin
  Application.CreateForm(TfConfig, fConfig);
  fConfig.show;
  fConfig.CallBackLang := Translate;
end;

procedure TFormConexao.actPasteIDExecute(Sender: TObject);
begin
  EGuestID.Text := MascaraID(TRDLib.ColarTexto, '999-999-999');
  EGuestID.GoToTextEnd;
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
  TRDLib.CopiarTexto(LPassword.Text);
end;

procedure TFormConexao.tmrReconnectTimer(Sender: TObject);
begin
  Conexao.ReconectarSocket;
end;

procedure TFormConexao.Translate;
begin
  self.Caption := Locale.GetLocale(FRMS, 'MainTitle');
  LSubTitle.Text := Locale.GetLocale(FRMS, 'MainSubTitle');
  LVersion.Text := Format(Locale.GetLocale(APP, 'Version'),
    [TRDLib.GetAppVersionStr]);
  LlyMachineIDCaption.Text := Locale.GetLocale(FRMS, 'MainMachineID');
  LlyPasswordCaption.Text := Locale.GetLocale(FRMS, 'MainPassword');
  LlyGuestIDCaption.Text := Locale.GetLocale(FRMS, 'MainGuestID');
  LbtnConectar.Text := Locale.GetLocale(FRMS, 'MainConnectButton');
  Conexao.ReconectarSocket(True);
end;

procedure TFormConexao.SetColors;
begin
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
  LPassword.Text := Conexao.Senha;
  btnConectar.Enabled := True;
  LbtnConectar.Enabled := btnConectar.Enabled;
end;

procedure TFormConexao.EGuestIDKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  // Dica: propriedade ShortCut da Action
  // if Key = vkReturn then
  // begin
  // actConnectExecute(Sender);
  // Key := vkNone;
  // end;

  // Dica: propriedade FilterChar do TEdit no FMX já faz esse controle
  // if not CharInSet(KeyChar, ['0' .. '9']) then
  // KeyChar := #0;
end;

procedure TFormConexao.EGuestIDTyping(Sender: TObject);
begin
  TEdit(Sender).Text := MascaraID(TEdit(Sender).Text, '999-999-999');
  TEdit(Sender).GoToTextEnd;
  // TEdit(Sender).SelStart := Length(TEdit(Sender).Text);
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

procedure TFormConexao.VerificarAtualizacao;
var
  xMessage, xLink, xFile: string;
  sl: TStringList;
begin
  // auto atualizador
  // bloqueei para não ficar rodando essa função sem necessidade no Aegys
  // xMessage := 'Foi encontrada uma versão mais atual disponível para download.' + sLineBreak +
  // 'Deseja atualizar agora?';
  //
  // xLink := ARQUIVO_SITE;
  // xFile := 'AegysSuporteCliente.exe';
  //
  // try
  // if (DateOf(TRDHttp.DataArquivo(xLink)) > DateOf(TFile.GetLastWriteTime(ParamStr(0))))
  // and (MessageDlg(xMessage, TMsgDlgType.mtInformation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes) then
  // begin
  // aniDownload.Enabled := True;
  // recDownload.Visible := True;
  // TTask.Run(
  // procedure
  // begin
  // RenameFile(ParamStr(0), TPath.Combine(ExtractFilePath(ParamStr(0)), 'OLD_' + xFile));
  // if TRDHttp.Download(TPath.Combine(ExtractFilePath(ParamStr(0)), xFile), xLink, nil) then
  // begin
  // ShellExecute(
  // WindowHandleToPlatform(Handle).Wnd,
  // 'open',
  // PChar(TPath.Combine(ExtractFilePath(ParamStr(0)), xFile)),
  // '',
  // '',
  // SW_SHOWNORMAL);
  // Application.Terminate;
  // end;
  // end);
  // end;
  // except
  // ShowMessage('Não foi possível atualizar o Executável.');
  // end;
end;

end.
