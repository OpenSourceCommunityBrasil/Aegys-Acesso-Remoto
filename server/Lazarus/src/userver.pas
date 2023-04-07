unit uServerChat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, BufDataset, Forms, Controls, Graphics, Dialogs,
  uAegysBase, ExtCtrls, DBGrids, StdCtrls;

type

  { TForm3 }

  TForm3 = class(TForm)
    bActive: TButton;
    Label1: TLabel;
    lClientsConnect: TLabel;
    procedure bActiveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    vClientsConnected : Integer;
    vAction           : Boolean;
    vAegysService     : TAegysService;
    Procedure Connect;
    Procedure ConnectClient   (Const Sender   : TAegysSession);
    Procedure DisconnectClient(Const Sender   : TAegysSession);
    procedure UpdateCaption;
    procedure UpdateServerStatus;
  public
    Function  GeneratePassword(LastPassword   : String) : String;
    Procedure GetNewID        (ContextList    : TSessionList;
                               Value          : String;
                               Var ClientID,
                               ClientPassword : String;
                               Var Accept    : Boolean);
  end;

var
  Form3: TForm3;

implementation

Uses uConsts;

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

Procedure TForm3.UpdateCaption;
Begin
 lClientsConnect.Caption := FormatFloat('0000', vClientsConnected);
End;

Procedure TForm3.UpdateServerStatus;
Begin
 If vAegysService.Active Then
  Begin
   Caption         := cTitle + Format(' Port:%d - Connect', [PORTA]);
   bActive.Caption := 'Deactive Server';
  End
 Else
  Begin
   Caption         := cTitle + Format(' Port:%d - Disconnect', [PORTA]);
   bActive.Caption := 'Active Server';
  End;
End;

Procedure TForm3.ConnectClient(Const Sender : TAegysSession);
Begin
 Inc(vClientsConnected);
 TThread.Synchronize(Nil, @UpdateCaption);
End;

Procedure TForm3.DisconnectClient(Const Sender : TAegysSession);
Begin
 Dec(vClientsConnected);
 TThread.Synchronize(Nil, @UpdateCaption);
End;

procedure TForm3.FormCreate(Sender: TObject);
begin
 vAegysService                    := TAegysService.Create(Self);
 {$IFDEF FPC}
 vAegysService.OnGetClientDetails := @GetNewID;
 vAegysService.OnConnect          := @ConnectClient;
 vAegysService.OnDisconnect       := @DisconnectClient;
 {$ELSE}
 vAegysService.OnGetClientDetails := GetNewID;
 vAegysService.OnConnect          := ConnectClient;
 vAegysService.OnDisconnect       := DisconnectClient;
 {$ENDIF}
 vClientsConnected                := 0;
end;

procedure TForm3.bActiveClick(Sender: TObject);
begin
 Connect;
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
 vAction                          := False;
 FreeAndNil(vAegysService);
 Release;
end;

Procedure TForm3.Connect;
Begin
 vClientsConnected     := 0;
 vAegysService.ServicePort := PORTA;
 Try
  vAegysService.Active     := Not vAegysService.Active;
 Except

 End;
 UpdateServerStatus;
End;

end.

