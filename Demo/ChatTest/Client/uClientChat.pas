unit uClientChat;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,    Vcl.Controls,    Vcl.Forms,  Vcl.Dialogs,
  Vcl.StdCtrls,   Vcl.ExtCtrls,    Vcl.Buttons,     uAegysBase, uAegysDataTypes,
  vcl.Imaging.jpeg;

Const
 cDadosRecebidos = 'Dados recebidos...';

type
  TForm2 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    eHost: TEdit;
    ePort: TEdit;
    Label3: TLabel;
    eSessionID: TEdit;
    Label4: TLabel;
    eSessionPWD: TEdit;
    Label5: TLabel;
    eFixedPWD: TEdit;
    cbAcceptUnAssist: TCheckBox;
    Label6: TLabel;
    eConnectTimeOut: TEdit;
    Label7: TLabel;
    ERequestTimeOut: TEdit;
    bConnect: TButton;
    Label8: TLabel;
    lbPeersConnected: TListBox;
    lMSG: TLabel;
    eMessage: TEdit;
    mReply: TMemo;
    Label10: TLabel;
    iImgSend: TImage;
    sbSendMSG: TSpeedButton;
    sbSendIMG: TSpeedButton;
    eIDConn: TEdit;
    Label11: TLabel;
    sbIDConn: TSpeedButton;
    Label12: TLabel;
    eLoginPass: TEdit;
    cbToAll: TCheckBox;
    tAutoCap: TTimer;
    sbAutoCap: TSpeedButton;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bConnectClick(Sender: TObject);
    procedure sbIDConnClick(Sender: TObject);
    procedure sbSendMSGClick(Sender: TObject);
    procedure sbSendIMGClick(Sender: TObject);
    procedure tAutoCapTimer(Sender: TObject);
    procedure sbAutoCapClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  protected
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd);
      message WM_ERASEBKGND;
  private
    { Private declarations }
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
                                MultiPack         : Boolean;
                                PackCount         : AeInteger;
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
   Procedure OnPeerKick        (Connection        : String;
                                Var ClientID,
                                ClientPassword,
                                Alias             : String);
  public
    { Public declarations }
   vAegysClient : TAegysClient;
   vImgProcessing: boolean;
  end;

var
  Form2: TForm2;

implementation

Uses uAegysBufferPack;

{$R *.dfm}

Function MacAddress : String;
Var
 Lib    : Cardinal;
 Func   : Function(GUID: PGUID): longint; Stdcall;
 GUID1,
 GUID2  : TGUID;
Begin
 Result := '';
 Lib := LoadLibrary('rpcrt4.dll');
 If Lib <> 0 Then
  Begin
   @Func := GetProcAddress(Lib, 'UuidCreateSequential');
   If assigned(Func) Then
    Begin
     If (Func(@GUID1) = 0) And
        (Func(@GUID2) = 0) And
        (GUID1.D4[2] = GUID2.D4[2]) And
        (GUID1.D4[3] = GUID2.D4[3]) And
        (GUID1.D4[4] = GUID2.D4[4]) And
        (GUID1.D4[5] = GUID2.D4[5]) And
        (GUID1.D4[6] = GUID2.D4[6]) And
        (GUID1.D4[7] = GUID2.D4[7]) Then
      Result := IntToHex(GUID1.D4[2], 2) + '-' +
                IntToHex(GUID1.D4[3], 2) + '-' +
                IntToHex(GUID1.D4[4], 2) + '-' +
                IntToHex(GUID1.D4[5], 2) + '-' +
                IntToHex(GUID1.D4[6], 2) + '-' +
                IntToHex(GUID1.D4[7], 2);
    End;
  End;
End;

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

Function CapturaTela : TBitmap;
Var
 dc : hdc;
 cv : TCanvas;
Begin
 result := TBitmap.Create;
 result.Width := Screen.Width;
 result.Height := Screen.Height;
 dc := GetDc(0);
 cv := TCanvas.Create;
 Try
  cv.Handle := DC;
  result.Canvas.CopyRect(Rect(0, 0, Screen.Width, Screen.Height),
                         cv, Rect(0,0,Screen.Width, Screen.Height));
 Finally
  cv.Free;
  ReleaseDC(0, DC);
 End;
End;

Procedure TForm2.OnServerLogin(Sender             : TObject);
Begin
 eSessionID.Text  := vAegysClient.SessionID;
 eSessionPWD.Text := vAegysClient.SessionPWD;
End;

Procedure TForm2.OnReceiveCommand(InternalCommand : TInternalCommand;
                                  Command         : String);
Begin
 mReply.Lines.Add(Command);
End;

procedure TForm2.sbIDConnClick(Sender: TObject);
begin
 vAegysClient.Join(eIDConn.Text, eLoginPass.Text, '');
end;

procedure TForm2.sbSendIMGClick(Sender: TObject);
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
  If cbToAll.Checked Then
   vAegysClient.SendBytes(aBuf, cbToAll.Checked)
  Else
   vAegysClient.SendBytes(eIDConn.Text, aBuf);
 Finally
  Application.ProcessMessages;
  SetLength(aBuf, 0);
  FreeAndNil(vStream);
  FreeAndNil(vBitMap);
 End;
end;

procedure TForm2.sbSendMSGClick(Sender: TObject);
begin
 If cbToAll.Checked Then
  vAegysClient.SendMessage(eMessage.Text, cbToAll.Checked)
 Else
  vAegysClient.SendMessage(eIDConn.Text, eMessage.Text);
end;

Procedure TForm2.OnScreenCapture(Connection,
                            ID,
                            Command           : String;
                            MultiPack         : Boolean;
                            PackCount         : AeInteger;
                            aBuf              : TAegysBytes);
begin
  if not(vImgProcessing) then
  begin
    vImgProcessing := true;

    TThread.CreateAnonymousThread(
      procedure
      Var
        vStream: TStream;
        vBitmap: TBitmap;
        vJpg: TJPEGImage;
      Begin
        Try
          vStream := nil;
          vBitmap := nil;
          vJpg := nil;

          vStream := TMemoryStream.Create;
          vBitmap := TBitmap.Create;
          vJpg := TJPEGImage.Create;

          vStream.Write(aBuf[0], Length(aBuf));
          vStream.Position := 0;
          Try
            vJpg.LoadFromStream(vStream);
            TThread.Synchronize(nil,
              procedure
              begin
                iImgSend.Picture.Assign(vJpg);
              end);
          Except
            vBitmap.LoadFromStream(vStream);
            TThread.Synchronize(nil,
              procedure
              begin
                iImgSend.Picture.Assign(vBitmap);
              end);
          End;
        Finally
          if assigned(vStream) then
            FreeAndNil(vStream);
          if assigned(vBitmap) then
            FreeAndNil(vBitmap);
          if assigned(vJpg) then
            FreeAndNil(vJpg);

          vImgProcessing := false;
        End;
      end).Start;
  end;

  Application.ProcessMessages;
End;

Procedure TForm2.OnChatReceive (Connection,
                                ID,
                                Command           : String);
Begin
 mReply.Lines.Add(Format('%s - %s >%s', [Connection, ID, Command]));
 if Command <> cDadosRecebidos then
  vAegysClient.SendMessage(ID, cDadosRecebidos);
End;

Procedure TForm2.OnReceiveBytes(aBuffer : TAegysBytes);
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

procedure TForm2.bConnectClick(Sender: TObject);
Begin
 Connect;
End;

procedure TForm2.Connect;
Begin
 vAegysClient.Host            := eHost.Text;
 vAegysClient.Port            := StrToInt(ePort.Text);
 vAegysClient.SessionFixedPWD := eFixedPWD.Text;
 vAegysClient.AcceptUnAssist  := cbAcceptUnAssist.Checked;
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

procedure TForm2.FormCreate(Sender: TObject);
begin
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
 vAegysClient.OnPeerKick          := OnPeerKick;
 SetControls(False);

 vImgProcessing := false;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
 FreeAndNil(vAegysClient);
 Release;
end;

Procedure TForm2.OnBeforeConnect(Sender            : TObject;
                                 Var WelcomeString : String);
Var
 vDrive : String;
Begin
 vDrive        := Uppercase(Copy(ParamStr(0), 1, 1));
 Welcomestring := MacAddress + '|' + SerialNumHardDisk(vDrive);
End;

procedure TForm2.SetControls(Value : Boolean);
Begin
 eIDConn.Enabled    := Value;
 cbToAll.Checked    := eIDConn.Enabled;
 eLoginPass.Enabled := eIDConn.Enabled;
 sbIDConn.Enabled   := eIDConn.Enabled;
 sbSendMSG.Enabled  := sbIDConn.Enabled;
 sbSendIMG.Enabled  := sbSendMSG.Enabled;
 eMessage.Enabled   := sbSendIMG.Enabled;
 mReply.Enabled     := eMessage.Enabled;
 sbAutoCap.Enabled  := mReply.Enabled;
 tAutoCap.Enabled   := False;
 If Not mReply.Enabled Then
  Begin
   mReply.Lines.Clear;
   iImgSend.Picture := Nil;
  End;
End;

procedure TForm2.SpeedButton1Click(Sender: TObject);
begin
 Try
  If cbToAll.Checked Then
   vAegysClient.DisconnectAllPeers
  Else
   Begin
    If lbPeersConnected.ItemIndex > -1 Then
     vAegysClient.DisconnectPeer('', '', lbPeersConnected.Items[lbPeersConnected.ItemIndex]);
   End;
 Finally
 End;
end;
procedure TForm2.sbAutoCapClick(Sender: TObject);
begin
 tAutoCap.Enabled := Not tAutoCap.Enabled;
end;

procedure TForm2.tAutoCapTimer(Sender: TObject);
begin
 tAutoCap.Enabled := False;
 Try
  sbSendIMG.Click;
 Finally
  tAutoCap.Enabled := True;
 End;
end;

procedure TForm2.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 0;
end;

procedure TForm2.OnConnect(Sender: TObject);
begin
 SetControls(True);
 bConnect.Caption := 'Disconnect';
end;

procedure TForm2.OnDisconnect(Sender: TObject);
begin
 SetControls(False);
 bConnect.Caption := 'Connect';
 eSessionID.Text  := '';
 eSessionPWD.Text := '';
 mReply.Lines.Clear;
 lbPeersConnected.Items.Clear;
end;

Procedure TForm2.OnPeerConnected(Connection      : String;
                                 Var ClientID,
                                 ClientPassword,
                                 Alias           : String);
Begin
 lbPeersConnected.AddItem(Connection + ' - ' + ClientID, Nil);
End;

Procedure TForm2.OnPeerDisconnected(Connection      : String;
                                    Var ClientID,
                                    ClientPassword,
                                    Alias           : String);
begin
 If lbPeersConnected.Items.IndexOf(Connection + ' - ' + ClientID) > -1 Then
  lbPeersConnected.Items.Delete(lbPeersConnected.Items.IndexOf(Connection + ' - ' + ClientID));
end;

Procedure TForm2.OnPeerKick(Connection      : String;
                            Var ClientID,
                            ClientPassword,
                            Alias           : String);
Begin
 If lbPeersConnected.Items.IndexOf(Connection + ' - ' + ClientID) > -1 Then
  lbPeersConnected.Items.Delete(lbPeersConnected.Items.IndexOf(Connection + ' - ' + ClientID));
End;

end.
