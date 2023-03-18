unit uServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, BufDataset, Forms, Controls, Graphics, Dialogs,
  uAegysBase, ExtCtrls, DBGrids;

type

  { TfServer }

  TfServer = class(TForm)
    DBGrid1: TDBGrid;
    QryConexoes: TBufDataset;
    DataSource1: TDataSource;
    tReload: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tReloadTimer(Sender: TObject);
  private
    vAction       : Boolean;
    vAegysService : TAegysService;
    Procedure Connect;
  public
    Function  GeneratePassword(LastPassword   : String) : String;
    Procedure GetNewID        (ContextList    : TSessionList;
                               Value          : String;
                               Var ClientID,
                               ClientPassword : String;
                               Var Accept    : Boolean);
  end;

var
  fServer: TfServer;

implementation

Uses uConstants;

{$R *.lfm}

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

Function TfServer.GeneratePassword(LastPassword : String) : String;
Begin
 Randomize;
 If (LastPassword <> '') Then
  Result := LastPassword
 Else
  Result := IntToStr(Random(9)) + IntToStr(Random(9)) + IntToStr(Random(9)) + IntToStr(Random(9));
End;


Procedure TfServer.GetNewID(ContextList      : TSessionList;
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

procedure TfServer.FormCreate(Sender: TObject);
begin
 vAegysService                    := TAegysService.Create(Self);
 {$IFDEF FPC}
 vAegysService.OnGetClientDetails := @GetNewID;
 {$ELSE}
 vAegysService.OnGetClientDetails := GetNewID;
 {$ENDIF}
 Connect;
 //tReload.Enabled                  := True;
end;

procedure TfServer.FormDestroy(Sender: TObject);
begin
 vAction                          := False;
 tReload.Enabled                  := vAction;
 FreeAndNil(vAegysService);
 Release;
end;

procedure TfServer.tReloadTimer(Sender: TObject);
Var
 I            : Integer;
 Conexao      : TAegysSession;
 vSessionList : TSessionList;
Begin
 vAction         := tReload.Enabled;
 vSessionList    := vAegysService.SessionList;
 tReload.Enabled := False;
 Try
  QryConexoes.DisableControls;
  QryConexoes.Close;
  QryConexoes.CreateDataset;
  For I := vSessionList.Count -1 DownTo 0 Do
   Begin
    If Not vAction Then
     Exit;
    Try
     Conexao                    := vSessionList[I];
     QryConexoes.Insert;
     QryConexoes.FindField('PROTOCOLO').AsString := Conexao.Connection;
     QryConexoes.FindField('ID').AsString        := Conexao.SessionID;
     QryConexoes.FindField('SENHA').AsString     := Conexao.SessionFixedPWD;
     QryConexoes.FindField('SENHA2').AsString    := Conexao.SessionPWD;
     QryConexoes.FindField('LATENCIA').AsString  := IntToStr(Conexao.Latency);
     QryConexoes.Post;
    Except
     Continue;
    End;
   End;
 Finally
  QryConexoes.EnableControls;
  tReload.Enabled := vAction;
 End;
End;

Procedure TfServer.Connect;
Begin
 vAegysService.ServicePort := PORTA;
 Try
  vAegysService.Active     := Not vAegysService.Active;
 Except

 End;
 If vAegysService.Active Then
  Caption := 'AegysServer' + Format(' Port:%d - Connect', [PORTA])
 Else
  Caption := 'AegysServer' + Format(' Port:%d - Disconnect', [PORTA]);
End;

end.

