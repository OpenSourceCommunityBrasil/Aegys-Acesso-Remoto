unit uFormConexao;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Objects, FMX.Controls.Presentation, FMX.Layouts, System.Actions,
  FMX.ActnList, FMX.Ani, uCtrl_Threads, FireDAC.Stan.Intf, System.Win.ScktComp,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, uDWAbout, uRESTDWPoolerDB, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, uDWConstsData, FMX.TabControl,
  uCtrl_Conexao, uRESTDWServerEvents;

type
  TFormConexao = class(TForm)
    layLogo: TLayout;
    Path1: TPath;
    Label1: TLabel;
    Layout2: TLayout;
    Label2: TLabel;
    Layout3: TLayout;
    layID: TLayout;
    Rectangle1: TRectangle;
    Label3: TLabel;
    txtIDPC: TLabel;
    Layout6: TLayout;
    Path2: TPath;
    SpeedButton1: TSpeedButton;
    laySenha: TLayout;
    Rectangle3: TRectangle;
    Label5: TLabel;
    Layout8: TLayout;
    txtSenhaPC: TLabel;
    SpeedButton2: TSpeedButton;
    Path3: TPath;
    layAcesso: TLayout;
    Rectangle4: TRectangle;
    Label7: TLabel;
    Layout10: TLayout;
    txtIDParceiro: TEdit;
    layConectar: TLayout;
    btnConectar: TRoundRect;
    Label23: TLabel;
    aniBtnLogin: TFloatAnimation;
    ActionList1: TActionList;
    PROC_CONECTAR: TAction;
    lnDivisao: TLine;
    layStatus: TLayout;
    imgStatus: TPath;
    lblStatus: TLabel;
    tmrReconnect: TTimer;
    tmrIntervalo: TTimer;
    tmrClipboard: TTimer;
    PROC_COPIAR_ID: TAction;
    PROC_COPIAR_SENHA: TAction;
    tcPrincipal: TTabControl;
    tabAcesso: TTabItem;
    Rectangle5: TRectangle;
    Layout4: TLayout;
    RDQuery: TRESTDWClientSQL;
    RESTDWDataBase1: TRESTDWDataBase;
    actMudarTab: TChangeTabAction;
    SpeedButton3: TSpeedButton;
    Path5: TPath;
    PROC_COLAR_ID: TAction;
    lblVersao: TLabel;
    recDownload: TRectangle;
    Layout16: TLayout;
    aniDownload: TAniIndicator;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PROC_COLAR_IDExecute(Sender: TObject);
    procedure PROC_CONECTARExecute(Sender: TObject);
    procedure PROC_COPIAR_IDExecute(Sender: TObject);
    procedure PROC_COPIAR_SENHAExecute(Sender: TObject);
    procedure tmrClipboardTimer(Sender: TObject);
    procedure tmrReconnectTimer(Sender: TObject);
    procedure tmrIntervaloTimer(Sender: TObject);
    procedure txtIDParceiroKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure txtIDParceiroTyping(Sender: TObject);
  private
    FHash: string;
    function MascaraID(AText, AMascara: string): string;
    procedure MudarTab(TabItem: TTabItem);
    procedure VerificarAtualizacao;
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
  System.Threading, Winapi.ShellAPI, FMX.Platform.Win;

procedure TFormConexao.LimparConexao;
begin
  Conexao.ResolucaoLargura := 986;
  Conexao.ResolucaoAltura := 600;

  FormTelaRemota.tCapturarComandos.Enabled := True;
  FormTelaRemota.tCapturarComandos.Enabled := False;
  FormTelaRemota.imgTelaRemota.WrapMode := TImageWrapMode.Fit;
  FormTelaRemota.imgTelaRemota.MultiResBitmap.Assign(FormTelaRemota.imgTelaRemota.MultiResBitmap);

  FormArquivos.btnDownload.Enabled := True;
  FormArquivos.btnUpload.Enabled := True;
  FormArquivos.pgbDownload.Value := 0;
  FormArquivos.pgbUpload.Value := 0;
  FormArquivos.lblTamanhoDownload.Text := 'Tamanho: 0 B / 0 B';
  FormArquivos.lblTamanhoUpload.Text := 'Tamanho: 0 B / 0 B';

  FormArquivos.txtPasta.Text := 'C:\';
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
    if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Svc) then
    begin
      vlClip := Svc.GetClipboard;
      if not(vlClip.IsEmpty)
        and (vlClip.IsType<string>)
        and (Conexao.OldClipboardText <> vlClip.ToString) then
      begin
        Conexao.OldClipboardText := vlClip.ToString;
        Conexao.SocketPrincipal.Socket.SendText('<|REDIRECT|><|CLIPBOARD|>' + Conexao.OldClipboardText + '<|END|>');
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
  tcPrincipal.TabPosition := TTabPosition.None;
  tcPrincipal.ActiveTab := tabAcesso;
  lblVersao.Text := 'Versão: ' + TRDLib.GetAppVersionStr;

  Conexao := TConexao.Create;

  SetOffline;
  Conexao.ReconectarSocket;
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
    if (AMascara[i] = '9') and not(AText[i] in ['0' .. '9']) and (Length(AText) = Length(AMascara) + 1) then
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
  imgStatus.Fill.Color := cColor;
  lblStatus.Text := AMensagem;
end;

procedure TFormConexao.MudarTab(TabItem: TTabItem);
begin
  actMudarTab.Tab := TabItem;
  actMudarTab.ExecuteTarget(self);
end;

procedure TFormConexao.PROC_COLAR_IDExecute(Sender: TObject);
begin
  txtIDParceiro.Text := MascaraID(TRDLib.ColarTexto, '999-999-999');
  txtIDParceiro.SelStart := Length(txtIDParceiro.Text);
end;

procedure TFormConexao.PROC_CONECTARExecute(Sender: TObject);
begin
  if not(txtIDParceiro.Text = '   -   -   ') then
  begin
    if (txtIDParceiro.Text = Conexao.ID) then
      MessageBox(0, 'Não é possível conetar com o próprio ID!', 'Suporte Remoto', MB_ICONASTERISK + MB_TOPMOST)
    else
    begin
      Conexao.SocketPrincipal.Socket.SendText('<|FINDID|>' + txtIDParceiro.Text + '<|END|>');
      btnConectar.Enabled := False;
      MudarStatusConexao(1, 'Procurando ID...');
    end;
  end;
end;

procedure TFormConexao.PROC_COPIAR_IDExecute(Sender: TObject);
begin
  TRDLib.CopiarTexto(txtIDPC.Text);
end;

procedure TFormConexao.PROC_COPIAR_SENHAExecute(Sender: TObject);
begin
  TRDLib.CopiarTexto(txtSenhaPC.Text);
end;

procedure TFormConexao.tmrReconnectTimer(Sender: TObject);
begin
  Conexao.ReconectarSocket;
end;

procedure TFormConexao.SetOffline;
begin
  txtIDPC.Text := 'Desconectado';
  txtSenhaPC.Text := 'Desconectado';
  btnConectar.Enabled := False;
  tmrIntervalo.Enabled := False;
  tmrClipboard.Enabled := False;
end;

procedure TFormConexao.SetOnline;
begin
  txtIDPC.Text := Conexao.ID;
  txtSenhaPC.Text := Conexao.Senha;
  btnConectar.Enabled := True;
end;

procedure TFormConexao.txtIDParceiroKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkReturn then
  begin
    PROC_CONECTARExecute(Sender);
    Key := vkNone;
  end;
  if not CharInSet(KeyChar, ['0' .. '9']) then
    KeyChar := #0;
end;

procedure TFormConexao.txtIDParceiroTyping(Sender: TObject);
begin
  TEdit(Sender).Text := MascaraID(TEdit(Sender).Text, '999-999-999');
  TEdit(Sender).SelStart := Length(TEdit(Sender).Text);
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
  //auto atualizador
  //bloqueei para não ficar rodando essa função sem necessidade no Aegys
//  xMessage := 'Foi encontrada uma versão mais atual disponível para download.' + sLineBreak +
//    'Deseja atualizar agora?';
//
//  xLink := ARQUIVO_SITE;
//  xFile := 'AegysSuporteCliente.exe';
//
//  try
//    if (DateOf(TRDHttp.DataArquivo(xLink)) > DateOf(TFile.GetLastWriteTime(ParamStr(0))))
//      and (MessageDlg(xMessage, TMsgDlgType.mtInformation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes) then
//    begin
//      aniDownload.Enabled := True;
//      recDownload.Visible := True;
//      TTask.Run(
//        procedure
//        begin
//          RenameFile(ParamStr(0), TPath.Combine(ExtractFilePath(ParamStr(0)), 'OLD_' + xFile));
//          if TRDHttp.Download(TPath.Combine(ExtractFilePath(ParamStr(0)), xFile), xLink, nil) then
//          begin
//            ShellExecute(
//              WindowHandleToPlatform(Handle).Wnd,
//              'open',
//              PChar(TPath.Combine(ExtractFilePath(ParamStr(0)), xFile)),
//              '',
//              '',
//              SW_SHOWNORMAL);
//            Application.Terminate;
//          end;
//        end);
//    end;
//  except
//    ShowMessage('Não foi possível atualizar o Executável.');
//  end;
end;

end.
