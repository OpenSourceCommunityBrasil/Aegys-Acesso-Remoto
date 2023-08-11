unit uPrincipal;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.Edit, FMX.StdCtrls, FMX.Layouts, FMX.ListBox,
  System.ImageList, FMX.ImgList, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  uAegysBase, uAegysDataTypes, FMX.Platform, AndroidAPI.JNI.Telephony,
  AndroidAPI.Helpers, AndroidAPI.JNI.OS, Androidapi.JNI.Provider; //,System.Permissions, ;

Const
 cDadosRecebidos = 'Dados recebidos...';

type
  TForm4 = class(TForm)
    eHost: TEdit;
    ePort: TEdit;
    eSessionID: TEdit;
    eSessionPWD: TEdit;
    eFixedPWD: TEdit;
    eConnectTimeOut: TEdit;
    ERequestTimeOut: TEdit;
    cbAcceptUnAssist: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    lbPeersConnected: TListBox;
    Label8: TLabel;
    bConnect: TButton;
    eIDConn: TEdit;
    eLoginPass: TEdit;
    Label9: TLabel;
    Label10: TLabel;
    cbToAll: TCheckBox;
    sbIDConn: TSpeedButton;
    ImageList1: TImageList;
    eMessage: TEdit;
    lMSG: TLabel;
    sbSendMSG: TSpeedButton;
    mReply: TMemo;
    iImgSend: TImageControl;
    sbSendIMG: TSpeedButton;
    procedure bConnectClick(Sender: TObject);
    procedure sbIDConnClick(Sender: TObject);
    procedure sbSendMSGClick(Sender: TObject);
    procedure sbSendIMGClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
   Function  CapturaTela : TBitmap;
   Procedure Connect;
   Procedure OnBeforeConnect   (Sender            : TObject;
                                Var WelcomeString : String);
   Procedure OnReceiveBytes    (aBuffer           : TAegysBytes);
   Procedure OnChatReceive     (Connection,
                                ID,
                                Command           : String);
   Procedure OnScreenCapture   (Connection,
                                ID,
                                Command           : String;
                                aBuf              : TAegysBytes);
   Procedure OnReceiveCommand  (InternalCommand   : TInternalCommand;
                                Command           : String);
   Procedure OnConnect         (Sender            : TObject);
   Procedure OnDisconnect      (Sender            : TObject);
   Procedure SetControls       (Value             : Boolean);
   Procedure OnServerLogin     (Sender            : TObject);
   Procedure OnPeerConnected   (Connection        : String;
                                Var ClientID,
                                ClientPassword,
                                Alias             : String);
   Procedure OnPeerDisconnected(Connection        : String;
                                Var ClientID,
                                ClientPassword,
                                Alias             : String);
   Function GetDeviceID : String;
  public
    { Public declarations }
   vAegysClient : TAegysClient;
  end;

var
  Form4: TForm4;

implementation

Uses uAegysBufferPack;
{$R *.fmx}

{ TForm4 }

procedure TForm4.bConnectClick(Sender: TObject);
begin
 Connect;
end;

Function TForm4.GetDeviceID : String;
Begin
 Try
  Result := JStringToString(TJSettings_SECURE.JavaClass.getString(TAndroidHelper.Activity.getContentResolver, TJSettings_SECURE.JavaClass.ANDROID_ID));
 Except
  Result := '';
 End;
End;

Procedure TForm4.Connect;
Begin
 vAegysClient.Host            := eHost.Text;
 vAegysClient.Port            := StrToInt(ePort.Text);
 vAegysClient.SessionFixedPWD := eFixedPWD.Text;
 vAegysClient.AcceptUnAssist  := cbAcceptUnAssist.IsChecked;
 vAegysClient.ConnectTimeOut  := StrToInt(eConnectTimeOut.Text);
 vAegysClient.RequestTimeOut  := StrToInt(eRequestTimeOut.Text);
 Try
  If Not vAegysClient.Active Then
   vAegysClient.Connect
  Else
   vAegysClient.Disconnect;
 Except

 End;
End;

procedure TForm4.FormCreate(Sender: TObject);
//Var
// lPermissionTelState : String;
begin
// lPermissionTelState := JStringtoString(TJManifest_permission.Javaclass.READ_PHONE_STATE);
// PermissionsService.RequestPermissions([lPermissionTelState], nil, nil);
 vAegysClient                     := TAegysClient.Create(Self);
 vAegysClient.OnBeforeConnect     := OnBeforeConnect;
 vAegysClient.OnReceiveBytes      := OnReceiveBytes;
 vAegysClient.OnReceiveCommand    := OnReceiveCommand;
 vAegysClient.OnChatReceive       := OnChatReceive;
 vAegysClient.OnConnect           := OnConnect;
 vAegysClient.OnDisconnect        := OnDisconnect;
 vAegysClient.OnServerLogin       := OnServerLogin;
 vAegysClient.OnPeerConnected     := OnPeerConnected;
 vAegysClient.OnPeerDisconnected  := OnPeerDisconnected;
 vAegysClient.OnScreenCapture     := OnScreenCapture;
 SetControls(False);
end;

procedure TForm4.FormDestroy(Sender: TObject);
begin
 FreeAndNil(vAegysClient);
end;

procedure TForm4.OnBeforeConnect(Sender: TObject; var WelcomeString: String);
begin
 Welcomestring := Copy(GetDeviceID, InitStrPos, 7) + '|' + Copy(GetDeviceID, 6, 7);
end;

procedure TForm4.OnChatReceive(Connection, ID, Command: String);
Begin
 mReply.Lines.Add(Format('%s - %s >%s', [Connection, ID, Command]));
 if Command <> cDadosRecebidos then
  vAegysClient.SendMessage(ID, cDadosRecebidos);
End;

procedure TForm4.OnConnect(Sender: TObject);
begin
 SetControls(True);
 bConnect.Text := 'Disconnect';
end;

procedure TForm4.OnDisconnect(Sender: TObject);
begin
 SetControls(False);
 bConnect.Text := 'Connect';
 eSessionID.Text  := '';
 eSessionPWD.Text := '';
 mReply.Lines.Clear;
 lbPeersConnected.Items.Clear;
end;

procedure TForm4.OnPeerConnected(Connection: String; var ClientID,
  ClientPassword, Alias: String);
begin
 lbPeersConnected.Items.Add(Connection + ' - ' + ClientID);
end;

procedure TForm4.OnPeerDisconnected(Connection: String; var ClientID,
  ClientPassword, Alias: String);
begin
 If lbPeersConnected.Items.IndexOf(Connection + ' - ' + ClientID) > -1 Then
  lbPeersConnected.Items.Delete(lbPeersConnected.Items.IndexOf(Connection + ' - ' + ClientID));
end;

procedure TForm4.OnReceiveBytes(aBuffer: TAegysBytes);
Var
 aPackClass : TPackClass;
 vCommand   : String;
Begin
 aPackClass := TPackClass.Create;
 Try
  aPackClass.FromBytes(aBuffer);
  If aPackClass.DataType = tdtString Then
   Begin
    vCommand := aPackClass.Command;
    mReply.Lines.Add(vCommand);
   End;
 Finally
  FreeAndNil(aPackClass);
 End;
End;

procedure TForm4.OnReceiveCommand(InternalCommand: TInternalCommand;
  Command: String);
begin
 mReply.Lines.Add(Command);
end;

procedure TForm4.OnScreenCapture(Connection, ID, Command: String;
  aBuf: TAegysBytes);
Var
 vStream : TStream;
 vBitmap : TBitmap;
Begin
 vStream := TMemoryStream.Create;
 vBitmap := TBitmap.Create;
 Try
  vStream.Write(aBuf[0], Length(aBuf));
  vStream.Position := 0;
  vBitmap.LoadFromStream(vStream);
  iImgSend.Bitmap.Assign(vBitmap);
 Finally
  FreeAndNil(vStream);
  FreeAndNil(vBitmap);
 End;
End;

procedure TForm4.OnServerLogin(Sender: TObject);
begin
 eSessionID.Text  := vAegysClient.SessionID;
 eSessionPWD.Text := vAegysClient.SessionPWD;
end;

procedure TForm4.sbIDConnClick(Sender: TObject);
begin
 vAegysClient.Join(eIDConn.Text, eLoginPass.Text, '');
end;

function MakeScaleScreenshot(Sender : TControl): TBitmap;
Var
 fScreenScale: Single;
 Function GetScreenScale: Single;
 var
  ScreenService: IFMXScreenService;
 begin
  Result := 1;
  if TPlatformServices.Current.SupportsPlatformService (IFMXScreenService, IInterface(ScreenService)) then
   Result := ScreenService.GetScreenScale;
 end;
begin
 fScreenScale := GetScreenScale;
 Result := TBitmap.Create(Round(Sender.Width*fScreenScale), Round(Sender.Height*fScreenScale));
 Result.Clear(0);
 if Result.Canvas.BeginScene then
  try
   Sender.PaintTo(Result.Canvas, RectF(0,0,Result.Width,Result.Height));
  finally
   Result.Canvas.EndScene;
  end;
end;

Function TForm4.CapturaTela : TBitmap;
Begin
 result := MakeScaleScreenshot(TControl(Self));
End;

procedure TForm4.sbSendIMGClick(Sender: TObject);
Var
 aBuf    : TAegysBytes;
 vStream : TStream;
 vBitmap : TBitmap;
begin
 Try
  vBitMap := CapturaTela;
  vStream := TMemoryStream.Create;
  vBitMap.SaveToStream(vStream);
  vStream.Position := 0;
  SetLength(aBuf, vStream.Size);
  vStream.Read(aBuf[0], Length(aBuf));
  If cbToAll.IsChecked Then
   vAegysClient.SendBytes(aBuf, cbToAll.IsChecked)
  Else
   vAegysClient.SendBytes(eIDConn.Text, aBuf);
 Finally
  Application.ProcessMessages;
  SetLength(aBuf, 0);
  FreeAndNil(vStream);
  FreeAndNil(vBitMap);
 End;
end;

procedure TForm4.sbSendMSGClick(Sender: TObject);
begin
 If cbToAll.IsChecked Then
  vAegysClient.SendMessage(eMessage.Text, cbToAll.IsChecked)
 Else
  vAegysClient.SendMessage(eIDConn.Text, eMessage.Text);
end;

procedure TForm4.SetControls(Value: Boolean);
begin
 eIDConn.Enabled    := Value;
 cbToAll.Enabled    := eIDConn.Enabled;
 eLoginPass.Enabled := eIDConn.Enabled;
 sbIDConn.Enabled   := eIDConn.Enabled;
 sbSendMSG.Enabled  := sbIDConn.Enabled;
 sbSendIMG.Enabled  := sbSendMSG.Enabled;
 eMessage.Enabled   := sbSendIMG.Enabled;
 mReply.Enabled     := eMessage.Enabled;
 If Not mReply.Enabled Then
  Begin
   mReply.Lines.Clear;
   iImgSend.Bitmap := Nil;
  End;
end;

end.
