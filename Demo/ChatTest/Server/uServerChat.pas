unit uServerChat;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics,   Vcl.Controls,    Vcl.Forms,       Vcl.Dialogs,     Vcl.StdCtrls,
  uAegysBase,     IdContext,       Data.DB,         Vcl.ExtCtrls,    Vcl.Grids,
  Vcl.DBGrids, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error,
  FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait, FireDAC.Comp.Client,
  FireDAC.DApt, FireDAC.Phys.FBDef, FireDAC.Phys.IBBase, FireDAC.Phys.FB;

Const
 cServidor_BD = 'localhost';
 cPorta_BD    = '3050';
 cUsuario_BD  = 'SYSDBA';
 cSenha_BD    = 'masterkey';

type
  TForm3 = class(TForm)
    bActive: TButton;
    lClientsConnect: TLabel;
    Label1: TLabel;
    FDPhysFBDriverLink1: TFDPhysFBDriverLink;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bActiveClick(Sender: TObject);
  private
    { Private declarations }
   vClientsConnected : Integer;
   vAction           : Boolean;
   vAegysService     : TAegysService;
   Procedure Connect;
   Procedure ConnectClient   (Const Sender   : TAegysSession);
   Procedure DisconnectClient(Const Sender   : TAegysSession);
   Procedure OnNewSession(Var vAegysSession    : TAegysSession;
                          Connection,
                          ID                   : String;
                          Var MyConnectionList : TAegysMyConnectionList);
   Procedure OnPeerService(Var vAegysSession   : TAegysSession;
                           Connection,
                           ID,
                           aServiceCommand     : String);
   Procedure OnUpdateMyConfigs(Var vAegysSession   : TAegysSession;
                               Connection,
                               ID,
                               aServiceCommand     : String);
  public
    { Public declarations }
   Function  GeneratePassword(LastPassword   : String) : String;
   Procedure GetNewID        (ContextList    : TSessionList;
                              Value          : String;
                              Var ClientID,
                              ClientPassword : String;
                              Var Accept    : Boolean);
  end;

  Function CreateDB               : TFDConnection;
  Function CreateQuery(Connection : TFDConnection) : TFDQuery;

var
  Form3: TForm3;

implementation

{$R *.dfm}

Uses uConsts, uAegysTools, uAegysDataTypes;

Function GenerateIDUnique(mac, hd : String) : String;
 Function LetToNum(Str : String) : String;
 Const
  Cad1: String = 'ABCDEF';
  Cad2: String = '123456';
 var
  x, y: integer;
 Begin
  Result := '';
  For y := 1 To Length(Str) Do
   Begin
    x := Pos(Str[y], Cad1);
    If x > 0 Then
     Result := Result + Copy(Cad2,x,1)
    Else
     Result := Result + Copy(str,y,1);
   End;
 End;
 Function RemoveChrInvalidos(Str : String) : String;
 Var
  x   : Integer;
  ret : String;
 Begin
  ret := '';
  For x := 1 To Length(Str) Do
   Begin
    If (Str[x] <> '-') And
       (Str[x] <> '.') And
       (Str[x] <> ',') And
       (Str[x] <> '/') Then
     ret := ret + Str[x];
   End;
  RemoveChrInvalidos := Trim(TrimRight(ret));
 End;
Var
 AMac,
 AHD, S,
 sID1,
 sID2,
 sID3 : String;
Begin
 AMac := RemoveChrInvalidos(mac);
 AHD  := RemoveChrInvalidos(hd);
 S    := LetToNum(AMac + AHD); // Trocando as letras pelos numeros;
 sID1 := Copy(s,StrToIntDef(Copy(s,1,1),1),2);
 sID2 := Copy(s,StrToIntDef(Copy(s,10,1),2),3);
 sID3 := Copy(s,StrToIntDef(Copy(s,length(s)-3,1),3),3);
 Result := sID1 + '-'+ sID2  +'-'+ sID3;
End;

Function TForm3.GeneratePassword(LastPassword : String) : String;
Begin
 Randomize;
 If (LastPassword <> '') Then
  Result := LastPassword
 Else
  Result := IntToStr(Random(9)) + IntToStr(Random(9)) + IntToStr(Random(9)) + IntToStr(Random(9));
End;


Procedure TForm3.GetNewID(ContextList      : TSessionList;
                          Value            : String;
                          Var ClientID,
                          ClientPassword   : String;
                          Var Accept       : Boolean);
Var
 I             : Integer;
 strMAC,
 strHD, ID,
 vLastPassword : String;
 Exists        : Boolean;
 Procedure ParseSerial(aValue        : String;
                       Var aMAC,
                       aHD,
                       aLastPassword : String);
 Begin
  aLastPassword := '';
  strMAC := Copy(aValue, 1, Pos('|', aValue) -1);
  Delete(aValue, 1, Pos('|', aValue));
  If Pos('|', aValue) > 0 Then
   Begin
    aHD  := Copy(aValue, 1, Pos('|', aValue) -1);
    Delete(aValue, 1, Pos('|', aValue));
    aLastPassword := aValue;
   End
  Else
   aHD   := aValue;
 End;
Begin
 Randomize;
 ParseSerial(Value, strMAC, strHD, vLastPassword);
 ID := GenerateIDUnique(strMAC, strHD);
 Exists := False;
 Try
  For I := ContextList.Count - 1 DownTo 0 do
   Begin
    Exists := (ContextList.Items[i].SessionID = ID);
    If Exists Then
     Break;
   End;
 Finally
  ClientID       := ID;
  ClientPassword := GeneratePassword(vLastPassword);
 End;
End;


procedure TForm3.bActiveClick(Sender: TObject);
begin
 Connect;
 If vAegysService.Active Then
  bActive.Caption := 'Deactive Server'
 Else
  bActive.Caption := 'Active Server';
end;

Procedure TForm3.Connect;
Begin
 vClientsConnected     := 0;
 vAegysService.ServicePort := PORTA;
 Try
  vAegysService.Active := Not vAegysService.Active;
 Except

 End;
 If vAegysService.Active Then
  Caption := cTitle + Format(' Port:%d - Connect', [PORTA])
 Else
  Caption := cTitle + Format(' Port:%d - Disconnect', [PORTA]);
End;

Procedure TForm3.ConnectClient(Const Sender : TAegysSession);
Begin
 Inc(vClientsConnected);
 lClientsConnect.Caption := FormatFloat('0000', vClientsConnected);
End;

Procedure TForm3.DisconnectClient(Const Sender : TAegysSession);
Begin
 Dec(vClientsConnected);
 lClientsConnect.Caption := FormatFloat('0000', vClientsConnected);
End;

Function CreateDB               : TFDConnection;
Begin
 Result := TFDConnection.Create(Nil);
 Result.DriverName := 'FB';
 Result.Params.Add('DriverID=FB');
 Result.Params.Add('Server=' + cServidor_BD);
 Result.Params.Add('Port=' + cPorta_BD);
 Result.Params.Add('Database=' + IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'AEGYSSERVICE.FDB');
 Result.Params.Add('User_Name=' + cUsuario_BD);
 Result.Params.Add('Password=' + cSenha_BD);
 Result.Params.Add('Protocol=TCPIP');
 Result.Params.Add('Charset=ISO8859_2');
 Result.Connected := True;
End;

Function CreateQuery(Connection : TFDConnection) : TFDQuery;
Begin
 Result            := TFDQuery.Create(Nil);
 Result.Connection := Connection;
End;

Procedure TForm3.OnNewSession(Var vAegysSession    : TAegysSession;
                              Connection,
                              ID                   : String;
                              Var MyConnectionList : TAegysMyConnectionList);
Var
 vFDConnection : TFDConnection;
 vFDQuery      : TFDQuery;
 aConnection   : TAegysMyConnection;
 aStreamImage  : TStream;
Begin
 vFDConnection := CreateDB;
 vFDQuery      := CreateQuery(vFDConnection);
 Try
  vFDQuery.SQL.Clear;
  vFDQuery.SQL.Add('SELECT * FROM MYCONFIGS WHERE AE_ID = :ID');
  vFDQuery.ParamByName('ID').AsString := ID;
  vFDQuery.Open;
  If vFDQuery.Eof Then
   Begin
    vFDQuery.Insert;
    vFDQuery.FindField('ID').Required    := False;
    vFDQuery.FindField('AE_ID').AsString := ID;
    vFDQuery.FindField('ACCEPTNONASSIST').AsInteger := 0;
   End
  Else
   vFDQuery.Edit;
  vFDQuery.FindField('LAST_ACCESS').AsDateTime := Now;
  vFDQuery.Post;
  vAegysSession.AcceptUnAssist   := vFDQuery.FindField('ACCEPTNONASSIST').AsInteger = 1;
  If vAegysSession.AcceptUnAssist Then
   vAegysSession.SessionFixedPWD := vFDQuery.FindField('FIXED_PASSWORD').AsString
  Else
   vAegysSession.SessionFixedPWD := '';
  vAegysSession.SessionAlias     := vFDQuery.FindField('ALIAS').AsString;
  vAegysSession.SessionGroup     := vFDQuery.FindField('GROUP').AsString;
  vFDQuery.Close;
  vFDQuery.SQL.Clear;
  vFDQuery.SQL.Add('SELECT * FROM MYPEERLIST WHERE OWNER_AE_ID = :ID');
  vFDQuery.ParamByName('ID').AsString := ID;
  vFDQuery.Open;
  While Not vFDQuery.Eof Do
   Begin
    aConnection                    := TAegysMyConnection.Create;
    aConnection.ConnectionName     := vFDQuery.FindField('NAME_CONNECTION').AsString;
    aConnection.ConnectionID       := vFDQuery.FindField('AE_ID').AsString;
    aConnection.ConnectionPass     := vFDQuery.FindField('FIXED_PASSWORD').AsString;
    aConnection.ConnectionAlias    := vFDQuery.FindField('ALIAS').AsString;
    aConnection.ConnectionGroup    := vFDQuery.FindField('GROUP').AsString;
    aConnection.RemoteAssist       := (vFDQuery.FindField('REMOTEASSIST').AsInteger = 1);
    aStreamImage                   := TMemoryStream.Create;
    Try
     TBlobField(vFDQuery.FindField('LASTSHOT')).SaveToStream(aStreamImage);
     aStreamImage.Position := 0;
     aConnection.ConnectionLastShot := EncodeStream(aStreamImage);
    Finally
     FreeAndNil(aStreamImage);
    End;
    Try
     MyConnectionList.Add(aConnection);
    Finally
     vFDQuery.Next;
    End;
   End;
  vFDQuery.Close;
 Finally
  FreeAndNil(vFDQuery);
  FreeAndNil(vFDConnection);
 End;
End;

Procedure TForm3.OnUpdateMyConfigs(Var vAegysSession : TAegysSession;
                                   Connection, ID,
                                   aServiceCommand   : String);
Var
 vFDConnection    : TFDConnection;
 vFDQuery         : TFDQuery;
Begin
 vFDConnection := CreateDB;
 vFDQuery      := CreateQuery(vFDConnection);
 Try
  vFDQuery.SQL.Clear;
  vFDQuery.SQL.Add('SELECT * FROM MYCONFIGS WHERE AE_ID = :ID');
  vFDQuery.ParamByName('ID').AsString := ID;
  vFDQuery.Open;
  If Not vFDQuery.Eof Then
   Begin
    vFDQuery.Edit;
    Try
     vFDQuery.FindField('ALIAS').AsString             := Copy(aServiceCommand, 1, Pos('|', aServiceCommand) -1);
     Delete(aServiceCommand, 1, Pos('|', aServiceCommand));
     vFDQuery.FindField('ACCEPTNONASSIST').AsInteger  := StrToInt(Copy(aServiceCommand, 1, Pos('|', aServiceCommand) -1));
     Delete(aServiceCommand, 1, Pos('|', aServiceCommand));
     vFDQuery.FindField('FIXED_PASSWORD').AsString    := Copy(aServiceCommand, 1, Pos('|', aServiceCommand) -1);
     Delete(aServiceCommand, 1, Pos('|', aServiceCommand));
     vFDQuery.FindField('GROUP').AsString             := aServiceCommand;
     vAegysSession.AcceptUnAssist  := vFDQuery.FindField('ACCEPTNONASSIST').AsInteger = 1;
     vAegysSession.SessionAlias    := vFDQuery.FindField('ALIAS').AsString;
     vAegysSession.SessionGroup    := vFDQuery.FindField('GROUP').AsString;
     vAegysSession.SessionFixedPWD := vFDQuery.FindField('FIXED_PASSWORD').AsString;
    Finally
     vFDQuery.Post;
    End;
   End;
 Finally
  FreeAndNil(vFDConnection);
  FreeAndNil(vFDQuery);
 End;
End;

Procedure TForm3.OnPeerService(Var vAegysSession : TAegysSession;
                               Connection, ID,
                               aServiceCommand   : String);
Var
 vFDConnection    : TFDConnection;
 vFDQuery         : TFDQuery;
 aConnection      : TAegysMyConnection;
 InternalCommand  : TInternalCommand;
 vCommandValue,
 vEndID,
 vConnectionID,
 vConnectionName,
 vConnectionAlias,
 vConnectionGroup,
 vConnectionPass  : String;
 vRemoteAssist    : Boolean;
 aStreamImage     : TMemoryStream;
Begin
 vFDConnection := CreateDB;
 vFDQuery      := CreateQuery(vFDConnection);
 Try
  ParseCommand(aServiceCommand, InternalCommand);
  Case InternalCommand Of
   ticPeerSendImage : Begin
                       vEndID := Copy(aServiceCommand, 1, Pos('&', aServiceCommand) -1);
                       Delete(aServiceCommand, 1, Pos('&', aServiceCommand));
                       vCommandValue := aServiceCommand;
                       vFDQuery.SQL.Clear;
                       vFDQuery.SQL.Add('SELECT * FROM MYPEERLIST WHERE OWNER_AE_ID = :OWNER_ID AND AE_ID = :END_ID');
                       vFDQuery.ParamByName('OWNER_ID').AsString := ID;
                       vFDQuery.ParamByName('END_ID').AsString   := vEndID;
                       vFDQuery.Open;
                       If vFDQuery.Eof Then
                        Begin
                         vFDQuery.Insert;
                         vFDQuery.FindField('OWNER_AE_ID').AsString := ID;
                         vFDQuery.FindField('AE_ID').AsString       := vEndID;
                        End
                       Else
                        vFDQuery.Edit;
                       Try
                        aStreamImage                   := DecodeStream(vCommandValue);
                        If Assigned(aStreamImage) Then
                         TBlobField(vFDQuery.FindField('LASTSHOT')).LoadFromStream(aStreamImage);
                       Finally
                        FreeAndNIl(aStreamImage);
                       End;
                       vFDQuery.Post;
                      End;
   ticNewPeerData   : Begin
                       vEndID := Copy(aServiceCommand, 1, Pos('&', aServiceCommand) -1);
                       Delete(aServiceCommand, 1, Pos('&', aServiceCommand));
                       vConnectionID   := DecodeStrings(Copy(aServiceCommand, 1, Pos('&', aServiceCommand) -1));
                       Delete(aServiceCommand, 1, Pos('&', aServiceCommand));
                       vConnectionName  := DecodeStrings(Copy(aServiceCommand, 1, Pos('&', aServiceCommand) -1));
                       Delete(aServiceCommand, 1, Pos('&', aServiceCommand));
                       vConnectionAlias := DecodeStrings(Copy(aServiceCommand, 1, Pos('&', aServiceCommand) -1));
                       Delete(aServiceCommand, 1, Pos('&', aServiceCommand));
                       vConnectionGroup := DecodeStrings(Copy(aServiceCommand, 1, Pos('&', aServiceCommand) -1));
                       Delete(aServiceCommand, 1, Pos('&', aServiceCommand));
                       vRemoteAssist    := Copy(aServiceCommand, 1, Pos('&', aServiceCommand) -1) = '1';
                       Delete(aServiceCommand, 1, Pos('&', aServiceCommand));
                       vConnectionPass  := DecodeStrings(aServiceCommand);
                       aServiceCommand  := '';
                       vFDQuery.SQL.Clear;
                       vFDQuery.SQL.Add('SELECT * FROM MYPEERLIST WHERE OWNER_AE_ID = :OWNER_ID AND AE_ID = :END_ID');
                       vFDQuery.ParamByName('OWNER_ID').AsString := ID;
                       vFDQuery.ParamByName('END_ID').AsString   := vEndID;
                       vFDQuery.Open;
                       If vFDQuery.Eof Then
                        Begin
                         vFDQuery.Insert;
                         Try
                          vFDQuery.FindField('ID').Required              := False;
                          vFDQuery.FindField('OWNER_AE_ID').AsString     := ID;
                          vFDQuery.FindField('AE_ID').AsString           := vConnectionID;
                          vFDQuery.FindField('NAME_CONNECTION').AsString := vConnectionName;
                          vFDQuery.FindField('ALIAS').AsString           := vConnectionAlias;
                          vFDQuery.FindField('GROUP').AsString           := vConnectionGroup;
                          vFDQuery.FindField('REMOTEASSIST').AsInteger   := Integer(vRemoteAssist);
                          vFDQuery.FindField('FIXED_PASSWORD').AsString  := vConnectionPass;
                          aConnection                                    := TAegysMyConnection.Create;
                          aConnection.ConnectionName                     := vFDQuery.FindField('NAME_CONNECTION').AsString;
                          aConnection.ConnectionID                       := vFDQuery.FindField('AE_ID').AsString;
                          aConnection.ConnectionPass                     := vFDQuery.FindField('FIXED_PASSWORD').AsString;
                          aConnection.ConnectionAlias                    := vFDQuery.FindField('ALIAS').AsString;
                          aConnection.ConnectionGroup                    := vFDQuery.FindField('GROUP').AsString;
                          aConnection.RemoteAssist                       := (vFDQuery.FindField('REMOTEASSIST').AsInteger = 1);
                          vAegysSession.MyConnections.Add(aConnection);
                         Finally
                          vFDQuery.Post;
                         End;
                        End;
                      End;
   ticEditPeerData  : Begin
                       vEndID := Copy(aServiceCommand, 1, Pos('&', aServiceCommand) -1);
                       Delete(aServiceCommand, 1, Pos('&', aServiceCommand));
                       vConnectionID   := DecodeStrings(Copy(aServiceCommand, 1, Pos('&', aServiceCommand) -1));
                       Delete(aServiceCommand, 1, Pos('&', aServiceCommand));
                       vConnectionName  := DecodeStrings(Copy(aServiceCommand, 1, Pos('&', aServiceCommand) -1));
                       Delete(aServiceCommand, 1, Pos('&', aServiceCommand));
                       vConnectionAlias := DecodeStrings(Copy(aServiceCommand, 1, Pos('&', aServiceCommand) -1));
                       Delete(aServiceCommand, 1, Pos('&', aServiceCommand));
                       vConnectionGroup := DecodeStrings(Copy(aServiceCommand, 1, Pos('&', aServiceCommand) -1));
                       Delete(aServiceCommand, 1, Pos('&', aServiceCommand));
                       vRemoteAssist    := Copy(aServiceCommand, 1, Pos('&', aServiceCommand) -1) = '1';
                       Delete(aServiceCommand, 1, Pos('&', aServiceCommand));
                       vConnectionPass  := DecodeStrings(aServiceCommand);
                       aServiceCommand  := '';
                       vFDQuery.SQL.Clear;
                       vFDQuery.SQL.Add('SELECT * FROM MYPEERLIST WHERE OWNER_AE_ID = :OWNER_ID AND AE_ID = :END_ID');
                       vFDQuery.ParamByName('OWNER_ID').AsString := ID;
                       vFDQuery.ParamByName('END_ID').AsString   := vEndID;
                       vFDQuery.Open;
                       If Not vFDQuery.Eof Then
                        Begin
                         vFDQuery.Edit;
                         Try
                          vFDQuery.FindField('AE_ID').AsString           := vConnectionID;
                          vFDQuery.FindField('NAME_CONNECTION').AsString := vConnectionName;
                          vFDQuery.FindField('ALIAS').AsString           := vConnectionAlias;
                          vFDQuery.FindField('GROUP').AsString           := vConnectionGroup;
                          vFDQuery.FindField('REMOTEASSIST').AsInteger   := Integer(vRemoteAssist);
                          vFDQuery.FindField('FIXED_PASSWORD').AsString  := vConnectionPass;
                         Finally
                          vFDQuery.Post;
                         End;
                        End;
                      End;
   ticDeletePeerData: Begin
                       vEndID := aServiceCommand;
                       vFDQuery.SQL.Clear;
                       vFDQuery.SQL.Add('DELETE FROM MYPEERLIST WHERE OWNER_AE_ID = :OWNER_ID AND AE_ID = :END_ID');
                       vFDQuery.ParamByName('OWNER_ID').AsString := ID;
                       vFDQuery.ParamByName('END_ID').AsString   := vEndID;
                       vFDQuery.ExecSQL;
                      End;
  End;
  vFDQuery.Close;
 Finally
  FreeAndNil(vFDQuery);
  FreeAndNil(vFDConnection);
 End;
End;

procedure TForm3.FormCreate(Sender: TObject);
begin
 vAegysService                    := TAegysService.Create(Self);
 vAegysService.OnGetClientDetails := GetNewID;
 vAegysService.OnConnect          := ConnectClient;
 vAegysService.OnDisconnect       := DisconnectClient;
 vAegysService.OnNewSession       := OnNewSession;
 vAegysService.OnPeerService      := OnPeerService;
 vAegysService.OnUpdateMyConfigs  := OnUpdateMyConfigs;
 vAegysService.RequestTimeout     := 5000;
 vClientsConnected                := 0;
// Connect;
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
 vAction                          := False;
 FreeAndNil(vAegysService);
 Release;
end;

end.
