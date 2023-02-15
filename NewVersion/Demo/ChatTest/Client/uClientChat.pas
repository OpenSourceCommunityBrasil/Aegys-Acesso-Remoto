unit uClientChat;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,    Vcl.Controls,    Vcl.Forms,  Vcl.Dialogs,
  Vcl.StdCtrls,   Vcl.ExtCtrls,    Vcl.Buttons,     uAegysBase, uAegysDataTypes;

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
    Label9: TLabel;
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bConnectClick(Sender: TObject);
    procedure sbIDConnClick(Sender: TObject);
  private
    { Private declarations }
   Procedure Connect;
   Procedure OnBeforeConnect(Sender            : TObject;
                             Var WelcomeString : String);
   Procedure OnReceiveBytes (aBuffer: TAegysBytes);
   Procedure OnReceiveCommand(InternalCommand : TInternalCommand;
                              Command         : String);
   Procedure OnConnect       (Sender          : TObject);
   Procedure OnDisconnect    (Sender          : TObject);
   Procedure SetControls(Value: Boolean);
  public
    { Public declarations }
   vAegysClient : TAegysClient;
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

Procedure TForm2.OnReceiveCommand(InternalCommand : TInternalCommand;
                                  Command         : String);
Begin
 eSessionID.Text  := vAegysClient.SessionID;
 eSessionPWD.Text := vAegysClient.SessionPWD;
End;

procedure TForm2.sbIDConnClick(Sender: TObject);
begin
 vAegysClient.Join(eIDConn.Text, eLoginPass.Text, '');
end;

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
 vAegysClient                  := TAegysClient.Create(Self);
 vAegysClient.OnBeforeConnect  := OnBeforeConnect;
 vAegysClient.OnReceiveBytes   := OnReceiveBytes;
 vAegysClient.OnReceiveCommand := OnReceiveCommand;
 vAegysClient.OnConnect        := OnConnect;
 vAegysClient.OnDisconnect     := OnDisconnect;
 SetControls(False);
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
 eLoginPass.Enabled := eIDConn.Enabled;
 sbIDConn.Enabled   := eIDConn.Enabled;
 sbSendMSG.Enabled  := sbIDConn.Enabled;
 sbSendIMG.Enabled  := sbSendMSG.Enabled;
 eMessage.Enabled   := sbSendIMG.Enabled;
 mReply.Enabled     := eMessage.Enabled;
 If Not mReply.Enabled Then
  Begin
   mReply.Lines.Clear;
   iImgSend.Picture := Nil;
  End;
End;

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
end;

end.
