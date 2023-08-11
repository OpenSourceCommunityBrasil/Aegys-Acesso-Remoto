unit UPrincipal;

interface

uses
  // SYSTEM
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.ImageList,
  System.Rtti,
  System.Bindings.Outputs,
  System.Messaging,
  System.Generics.Collections,
  System.StrUtils,
  // FMX
  FMX.TabControl,
  FMX.ComboEdit,
  FMX.ListView.Types,
  FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base,
  FMX.ListView,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.Bind.Editors,
  FMX.Bind.DBEngExt,
  FMX.Objects,
  FMX.Platform,
  FMX.ImgList,
  FMX.Layouts,
  FMX.Edit,
  // FIREDAC
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Param,
  FireDAC.Stan.Error,
  FireDAC.DatS,
  FireDAC.Phys.Intf,
  FireDAC.DApt.Intf,
  FireDAC.Comp.DataSet,
  FireDAC.Comp.Client,
  // DATA
  Data.DB,
  Data.Bind.EngExt,
  Data.Bind.Components,
  Data.Bind.DBScope,
  // Android
  Androidapi.JNI.Provider,
  Androidapi.JNI.Net,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Media,
  Androidapi.JNIBridge,
  Androidapi.JNI.Hardware,
  // Aegys
  uAegysBase,
  uAegysDataTypes,
  uAegysConsts;

type
  TfrmPrincipal = class(TForm)
    tmGetScreenInfo: TTimer;
    StyleBook1: TStyleBook;
    ltTitulo: TLayout;
    imgTituloLetra: TImage;
    ltTituloStatus: TLayout;
    tmConnect: TTimer;
    tcPrincipal: TTabControl;
    tbiConfig: TTabItem;
    tbiRemoteAccess: TTabItem;
    imgTituloLogo: TImage;
    tbiConnections: TTabItem;
    GridPanelLayout1: TGridPanelLayout;
    Rectangle1: TRectangle;
    GridPanelLayout2: TGridPanelLayout;
    Label1: TLabel;
    edDeviceAegysID: TEdit;
    Label2: TLabel;
    edDeviceAegysPassword: TEdit;
    Layout5: TLayout;
    Label4: TLabel;
    Rectangle2: TRectangle;
    GridPanelLayout3: TGridPanelLayout;
    Label3: TLabel;
    edRemoteAegysID: TEdit;
    btRemoteConnect: TButton;
    Layout6: TLayout;
    Label5: TLabel;
    GridPanelLayout4: TGridPanelLayout;
    Label6: TLabel;
    cbConfigServer: TComboEdit;
    Label7: TLabel;
    edConfigPort: TEdit;
    Label8: TLabel;
    GridPanelLayout5: TGridPanelLayout;
    lConfigFrameRate: TLabel;
    tbConfigFrameRate: TTrackBar;
    Rectangle3: TRectangle;
    GridPanelLayout6: TGridPanelLayout;
    lImgT: TLabel;
    lImgQ: TLabel;
    Label9: TLabel;
    lSendCaptureStatus: TLabel;
    swSendCaptureStatus: TSwitch;
    lSendCaptureSelect: TLabel;
    swSendCaptureSelect: TSwitch;
    Rectangle4: TRectangle;
    GridPanelLayout7: TGridPanelLayout;
    Label10: TLabel;
    lvConnectedClients: TListView;
    mtConnectedClients: TFDMemTable;
    mtConnectedClientsPeerID: TStringField;
    mtConnectedClientsConnection: TStringField;
    mtConnectedClientsPass: TStringField;
    mtConnectedClientsAlias: TStringField;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkListControlToField1: TLinkListControlToField;
    lImgB: TLabel;
    rtAlerta: TRectangle;
    rtAlertaConteudo: TRectangle;
    lAlertaTitulo: TLabel;
    lAlertaConteudo: TLabel;
    ltAlertaBotoes: TLayout;
    btAlertaOK: TButton;
    ImageList1: TImageList;
    Label11: TLabel;
    Edit1: TEdit;
    lSendAccess: TLabel;
    swAccessibility: TSwitch;
    procedure FormCreate(Sender: TObject);
    procedure tmGetScreenInfoTimer(Sender: TObject);
    procedure tmConnectTimer(Sender: TObject);
    procedure tbConfigFrameRateTracking(Sender: TObject);
    procedure swSendCaptureStatusSwitch(Sender: TObject);
    procedure swSendCaptureSelectSwitch(Sender: TObject);
    procedure lvConnectedClientsUpdateObjects(const Sender: TObject;
      const AItem: TListViewItem);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btAlertaOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvConnectedClientsItemClickEx(const Sender: TObject; ItemIndex: Integer;
      const LocalClickPos: TPointF; const ItemObject: TListItemDrawable);
    procedure swAccessibilitySwitch(Sender: TObject);
  private
    vAegysClient: TAegysClient;
    auxImgBand: Int64;
    auxImgTime: Integer;
    auxAppActive: Boolean;
    procedure OnBeforeConnect(Sender: TObject; var WelcomeString: String);
    procedure OnConnect(Sender: TObject);
    procedure OnDisconnect(Sender: TObject);
    procedure OnServerLogin(Sender: TObject);
    procedure OnPeerConnected(Connection: String; var ClientID, ClientPassword,
      Alias: String);
    procedure OnPeerDisconnected(Connection: String;
      var ClientID, ClientPassword, Alias: String);
    procedure MostraAlerta(Tipo, Titulo, Conteudo: String);
    function CheckBatteryOptimization: Boolean;
    function AppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
    procedure OnMouseCapture(Command: string);
    function SplitAegysCommandString(Command, CommandType: string): TStringDynArray;
    procedure OnThreadErro(ErroCode: Integer; ErrorMessage: String);
  public
  end;

var
  frmPrincipal: TfrmPrincipal;

implementation

{$R *.fmx}

Uses
  Androidapi.JNI.JavaTypes, Androidapi.Helpers, Androidapi.JNI.os,
  Androidapi.JNI.App, FMX.Helpers.Android, FMX.Surfaces, Androidapi.JNI.util;

procedure TfrmPrincipal.swAccessibilitySwitch(Sender: TObject);
begin
  if swAccessibility.IsChecked then
  begin
    lSendAccess.Text := 'INPUT ACCESS';
  end
  else
  begin
    lSendAccess.Text := 'NO INPUT ACCESS';
  end;
end;

procedure TfrmPrincipal.swSendCaptureSelectSwitch(Sender: TObject);
var
  x: Integer;
  ScreenShare: TListItemImage;
begin
  if swSendCaptureSelect.IsChecked then
  begin
    lSendCaptureSelect.Text := 'TO ALL';

    for x := 0 to lvConnectedClients.Items.Count - 1 do
    begin
      ScreenShare := TListItemImage(TListViewItem(lvConnectedClients.Items[x])
        .Objects.FindDrawable('lviScreenShare'));
      ScreenShare.TagString := '1';
      ScreenShare.Bitmap := ImageList1.Bitmap(TSizeF.Create(128, 128), 1);
    end;
  end
  else
  begin
    lSendCaptureSelect.Text := 'TO SELECTED';

    for x := 0 to lvConnectedClients.Items.Count - 1 do
    begin
      ScreenShare := TListItemImage(TListViewItem(lvConnectedClients.Items[x])
        .Objects.FindDrawable('lviScreenShare'));
      ScreenShare.TagString := '0';
      ScreenShare.Bitmap := ImageList1.Bitmap(TSizeF.Create(128, 128), 2);
    end;
  end;
end;

procedure TfrmPrincipal.swSendCaptureStatusSwitch(Sender: TObject);
begin
  if swSendCaptureStatus.IsChecked then
  begin
    begin
      auxImgBand := 0;
      auxImgTime := 0;

      lSendCaptureStatus.Text := 'SENDING';

      tmGetScreenInfo.Enabled := true;
    end
  end
  else
  begin
    tmGetScreenInfo.Enabled := false;

    lSendCaptureStatus.Text := 'NOT SENDING';

    lImgT.Text := '-';
    lImgQ.Text := '-';
    lImgB.Text := '-';

    swSendCaptureStatus.Enabled := true;
  end;
end;

procedure TfrmPrincipal.tbConfigFrameRateTracking(Sender: TObject);
begin
  if tbConfigFrameRate.Value = 0 then
  begin
    lConfigFrameRate.Text := '5 FPS';

    tmGetScreenInfo.Interval := trunc(1000 / 5);
  end
  else if tbConfigFrameRate.Value = 1 then
  begin
    lConfigFrameRate.Text := '10 FPS';

    tmGetScreenInfo.Interval := trunc(1000 / 10);
  end
  else if tbConfigFrameRate.Value = 2 then
  begin
    lConfigFrameRate.Text := '20 FPS';

    tmGetScreenInfo.Interval := trunc(1000 / 20);
  end
  else if tbConfigFrameRate.Value = 3 then
  begin
    lConfigFrameRate.Text := '30 FPS';

    tmGetScreenInfo.Interval := trunc(1000 / 30);
  end;
end;

procedure TfrmPrincipal.tmConnectTimer(Sender: TObject);
begin
  try
    tmConnect.Enabled := false;

    if not(vAegysClient.Active) then
    begin
      vAegysClient.Host := cbConfigServer.Text;
      vAegysClient.Port := StrToInt(edConfigPort.Text);
      vAegysClient.SessionFixedPWD := '';
      vAegysClient.AcceptUnAssist := false;
      vAegysClient.ConnectTimeOut := 5000;
      vAegysClient.RequestTimeOut := 30000;

      if not(vAegysClient.Active) then
      begin
        TThread.CreateAnonymousThread(
          procedure
          begin
            try
              vAegysClient.Connect;
            except
              // TThread.Synchronize(nil,
              // procedure
              // begin
              // frmPrincipal.OnDisconnect(vAegysClient);
              // end);
            end;
          end).Start;
      end;
    end;
  except
    vAegysClient.Disconnect;
  end;
end;

procedure TfrmPrincipal.tmGetScreenInfoTimer(Sender: TObject);
var
  vBitmapSurface: TBitmapSurface;
  aBuf: TAegysBytes;
  vStream: TStream;
  vSaveParams: TBitmapCodecSaveParams;
begin
  Try
    tmGetScreenInfo.Enabled := false;

    vBitmapSurface := nil;

    vStream := nil;

    vSaveParams.Quality := 10;

    if (mtConnectedClients.RecordCount > 0) then
    begin
      vBitmapSurface := TBitmapSurface.Create;

      vBitmapSurface.Assign(frmPrincipal.tcPrincipal.MakeScreenshot);

      vStream := TMemoryStream.Create;

      TBitmapCodecManager.SaveToStream(vStream, vBitmapSurface, '.jpg', @vSaveParams);

      vStream.Position := 0;

      if auxImgTime >= 1000 then
      begin
        if auxAppActive then
          lImgB.Text := 'BW.    : ' + FormatFloat('0.000', (auxImgBand / 1024 / 1024))
            + ' MB/s';

        auxImgTime := 0;

        auxImgBand := 0;
      end;

      if auxAppActive then
      begin
        lSendCaptureStatus.Text := 'SENDING';

        lImgQ.Text := 'Quality: ' + vSaveParams.Quality.ToString + ' %';

        lImgT.Text := 'Size   : ' + FormatFloat('0.000',
          (vStream.Size / 1024 / 1024)) + ' MB';

      end;
      auxImgTime := auxImgTime + tmGetScreenInfo.Interval;
      auxImgBand := auxImgBand + vStream.Size;

      SetLength(aBuf, vStream.Size);

      vStream.Read(aBuf[0], Length(aBuf));

      vAegysClient.SendBytes(aBuf, swSendCaptureSelect.IsChecked);
    end
    else
    begin
      lSendCaptureStatus.Text := 'SENDING (NONE)';

      lImgT.Text := '-';
      lImgQ.Text := '-';
      lImgB.Text := '-';
    end;
  Finally
    SetLength(aBuf, 0);

    if Assigned(vStream) then
      FreeAndNil(vStream);

    if Assigned(vBitmapSurface) then
      FreeAndNil(vBitmapSurface);

    tmGetScreenInfo.Enabled := true;
  End;
end;

procedure TfrmPrincipal.lvConnectedClientsItemClickEx(const Sender: TObject;
ItemIndex: Integer; const LocalClickPos: TPointF; const ItemObject: TListItemDrawable);
begin
  if ((ItemObject is TListItemImage) and (TListItemImage(ItemObject).Name = 'lviKickUser'))
  then
  begin
    vAegysClient.DisconnectPeer(mtConnectedClients.FieldByName('PeerID').AsString,
      mtConnectedClients.FieldByName('Pass').AsString,
      mtConnectedClients.FieldByName('Connection').AsString);

    MostraAlerta('', 'ALERTA', 'Usuário ' + mtConnectedClients.FieldByName('PeerID')
      .AsString + ' kickado com sucesso!');
  end
  else if ((ItemObject is TListItemImage) and
    (TListItemImage(ItemObject).Name = 'lviScreenShare') and
    not(swSendCaptureSelect.IsChecked)) then
  begin
    if (TListItemImage(ItemObject)).TagString = '0' then
    begin
      (TListItemImage(ItemObject)).TagString := '1';
      (TListItemImage(ItemObject)).Bitmap :=
        ImageList1.Bitmap(TSizeF.Create(128, 128), 1);
    end
    else if (TListItemImage(ItemObject)).TagString = '1' then
    begin
      (TListItemImage(ItemObject)).TagString := '0';
      (TListItemImage(ItemObject)).Bitmap :=
        ImageList1.Bitmap(TSizeF.Create(128, 128), 2);
    end;
  end;
end;

procedure TfrmPrincipal.lvConnectedClientsUpdateObjects(const Sender: TObject;
const AItem: TListViewItem);
var
  TituloPeerID, TituloAlias, Alias: TListItemText;
  KickUser, ScreenShare: TListItemImage;
begin

  if AItem.Purpose = TListItemPurpose.None then
  begin
    TituloPeerID := TListItemText(AItem.Objects.FindDrawable('lvtTituloPeerID'));
    TituloPeerID.Text := 'Peer ID';

    TituloAlias := TListItemText(AItem.Objects.FindDrawable('lvtTituloPeerAlias'));
    TituloAlias.Text := 'Peer Alias';

    Alias := TListItemText(AItem.Objects.FindDrawable('lvtPeerAlias'));
    if String.IsNullOrEmpty(Alias.Text) then
      Alias.Text := 'N/A';

    KickUser := TListItemImage(AItem.Objects.FindDrawable('lviKickUser'));
    KickUser.PlaceOffset.x := (Sender as TListView).Width - KickUser.Width - 40;
    KickUser.Bitmap := ImageList1.Bitmap(TSizeF.Create(128, 128), 0);

    ScreenShare := TListItemImage(AItem.Objects.FindDrawable('lviScreenShare'));
    ScreenShare.PlaceOffset.x := 20;

    if ((String.IsNullOrEmpty(ScreenShare.TagString)) or (ScreenShare.TagString = '0'))
    then
    begin
      ScreenShare.TagString := '0';
      ScreenShare.Bitmap := ImageList1.Bitmap(TSizeF.Create(128, 128), 2);
    end
    else if (not(String.IsNullOrEmpty(ScreenShare.TagString)) and
      (ScreenShare.TagString = '1')) then
    begin
      ScreenShare.TagString := '1';
      ScreenShare.Bitmap := ImageList1.Bitmap(TSizeF.Create(128, 128), 1);
    end;
  end;

end;

Procedure TfrmPrincipal.OnBeforeConnect(Sender: TObject; Var WelcomeString: String);
Var
  vDrive: String;
Begin
  WelcomeString := '12-34-56'
End;

Procedure TfrmPrincipal.OnConnect(Sender: TObject);
Begin
  swSendCaptureStatus.Enabled := true;

  mtConnectedClients.Open;

  edDeviceAegysID.Text       := vAegysClient.SessionID;
  edDeviceAegysPassword.Text := vAegysClient.SessionPWD;
End;

Procedure TfrmPrincipal.OnServerLogin(Sender: TObject);
Begin
  edDeviceAegysID.Text       := StringReplace(vAegysClient.SessionID, '-', ' - ', [rfReplaceAll]);
  edDeviceAegysPassword.Text := vAegysClient.SessionPWD;
End;

Procedure TfrmPrincipal.OnDisconnect(Sender: TObject);
Begin
  swSendCaptureStatus.IsChecked := false;
  swSendCaptureStatus.Enabled := false;
  mtConnectedClients.Close;

  edDeviceAegysID.Text := '';
  edDeviceAegysPassword.Text := '';

  tmConnect.Enabled := true;
End;

procedure TfrmPrincipal.OnPeerConnected(Connection: String;
Var ClientID, ClientPassword, Alias: String);
begin
  if not(mtConnectedClients.Locate('PeerID', ClientID)) then
    mtConnectedClients.InsertRecord([ClientID, Connection, ClientPassword, Alias]);
end;

procedure TfrmPrincipal.OnPeerDisconnected(Connection: String;
Var ClientID, ClientPassword, Alias: String);
begin
  if mtConnectedClients.Locate('PeerID', ClientID) then
  begin
    mtConnectedClients.Locate('PeerID', ClientID);
    mtConnectedClients.Delete;
  end;
end;

procedure TfrmPrincipal.btAlertaOKClick(Sender: TObject);
var
  vIntent: JIntent;
begin
  rtAlerta.Visible := false;

  if rtAlerta.TagString = 'BATTERY' then
  begin
    if CheckBatteryOptimization then
    begin
      vIntent := TJIntent.Create;

      vIntent.setAction(TJSettings.JavaClass.ACTION_IGNORE_BATTERY_OPTIMIZATION_SETTINGS);

      TAndroidHelper.Context.startActivity(vIntent);
    end;
  end;

end;

procedure TfrmPrincipal.MostraAlerta(Tipo, Titulo, Conteudo: String);
begin
  lAlertaTitulo.Text := UpperCase(Titulo);

  lAlertaConteudo.Text := Conteudo;

  rtAlerta.TagString := Tipo;

  rtAlerta.Visible := true;
end;

procedure TfrmPrincipal.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  vAegysClient.Disconnect;
end;

procedure TfrmPrincipal.FormCreate(Sender: TObject);
var
  AppEventSvc: IFMXApplicationEventService;
begin
  auxAppActive := true;

  if TPlatformServices.Current.SupportsPlatformService(IFMXApplicationEventService,
    IInterface(AppEventSvc)) then
  begin
    AppEventSvc.SetApplicationEventHandler(AppEvent);
  end;

  edDeviceAegysID.Caret.Visible := false;
  edDeviceAegysPassword.Caret.Visible := false;

  vAegysClient := TAegysClient.Create(Self);
  vAegysClient.OnBeforeConnect := OnBeforeConnect;
  vAegysClient.OnConnect := OnConnect;
  vAegysClient.OnDisconnect := OnDisconnect;
  vAegysClient.OnServerLogin := OnServerLogin;
  vAegysClient.OnPeerConnected := OnPeerConnected;
  vAegysClient.OnPeerDisconnected := OnPeerDisconnected;
  vAegysClient.OnMouseCapture := OnMouseCapture;
//  vAegysClient.OnThreadtError := OnThreadErro;

  tmConnect.Enabled := true;
end;

procedure TfrmPrincipal.OnThreadErro(ErroCode: Integer; ErrorMessage: String);
begin
  ShowMessage(ErrorMessage);
end;

procedure TfrmPrincipal.OnMouseCapture(Command: string);
var
  vAuxArray: TStringDynArray;
begin
  if swAccessibility.IsChecked then
  begin
    if Pos(cMouseClickLeftDown, Command) > 0 then
    begin
      vAuxArray := SplitAegysCommandString(Command, cMousePos);

      ShowMessage(cMouseClickLeftDown + ' x:' + vAuxArray[0] + ' y:' + vAuxArray[1]);
    end
    else if Pos(cMouseClickLeftUp, Command) > 0 then
    begin
      vAuxArray := SplitAegysCommandString(Command, cMousePos);

      ShowMessage(cMouseClickLeftUp + ' x:' + vAuxArray[0] + ' y:' + vAuxArray[1]);
    end;
  end;
end;

function TfrmPrincipal.SplitAegysCommandString(Command, CommandType: string)
  : TStringDynArray;
var
  vAuxResult: String;
begin
  vAuxResult := StringReplace(Command, CommandType, '', []);

  vAuxResult := Copy(vAuxResult, 1, Pos(cEndTag, vAuxResult) - 1);

  vAuxResult := StringReplace(vAuxResult, cSeparatorTag, '|', [rfReplaceAll]);

  Result := SplitString(vAuxResult, '|');
end;

procedure TfrmPrincipal.FormShow(Sender: TObject);
begin
  CheckBatteryOptimization;
end;

function TfrmPrincipal.AppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
begin
  if AAppEvent = TApplicationEvent.BecameActive then
  begin
    auxAppActive := true;
  end
  else if AAppEvent = TApplicationEvent.EnteredBackground then
  begin
    auxAppActive := false;
  end;

  Result := true;
end;

function TfrmPrincipal.CheckBatteryOptimization: Boolean;
var
  vPowerManager: JPowerManager;
begin
  vPowerManager := TJPowerManager.Wrap
    ((SharedActivityContext.getSystemService(TJContext.JavaClass.POWER_SERVICE)
    as ILocalObject).GetObjectID);

  if not(vPowerManager.isIgnoringBatteryOptimizations
    (SharedActivityContext.getPackageName)) then
  begin
    Result := true;

    MostraAlerta('BATTERY', 'Otimizador de Bateria',
      'Para que o Aegys funcione corretamente você deve desligar o otimizador de bateria para o APP.');
  end
  else
    Result := false;
end;

end.
