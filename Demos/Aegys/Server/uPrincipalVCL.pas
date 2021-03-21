unit uPrincipalVCL;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, IdContext, uUtilServer,
  IdServerIOHandler, System.Generics.Collections,
  IdBaseComponent, IdComponent, IdCustomTCPServer, IdTCPServer, Vcl.Menus,
  Vcl.ExtCtrls, IdBuffer, System.SyncObjs, System.ZLib,
  IdServerIOHandlerSocket, IdServerIOHandlerStack, IdIntercept,
  IdCompressionIntercept, uIpPoolerService, IdUDPBase,
  IdUDPServer, IdGlobal, IdSocketHandle, IdAntiFreezeBase, Vcl.IdAntiFreeze,
  IdScheduler, IdSchedulerOfThread, IdSchedulerOfThreadDefault, uUDPPooler,
  uUDPSuperComponents;

Const                      //2kb    4Kb    8kb    16kb
 MaxBuffer    = 1024 * 30; //2048 //4096 //8192 //16384
 vEOB         = '<$EOLB$>';
 TimeOutC     = 3000;
 GarbageTimeC = 5000;
 TPingTimer   = 1000;
 vCommandEnd  = '<$COMMANDEND$>';
 PingTest     = True;

Type
 TThreadConnection = Packed Record
  Main,
  Target : TIdContext;
End;

Type
 TIpConnection = Packed Record
  Main,
  Target  : TPackString;
  Reply   : Boolean;
  Host,
  LocalIP : String;
  UpdPort,
  UdpLocalPort : Integer;
End;

Type
 PInteger = ^Integer;

Type
 TClientSetings = Packed Record //Class
  ID,
  Group,
  Machine,
  MAC, HD,
  LastPassword,
  IP,
  Maq,
  TargetID,
  Password,
  TargetPassword,
  InsertTargetID  : String;
  ThreadID        : Cardinal;
  StartPing,
  EndPing         : Integer;
  lItem,
  lItem2          : TListItem;
  ipMain,
  ipDesktop,
  ipFiles,
  ipOthers        : TIpConnection;
//  Constructor Create;
End;
PCliente = ^TClientSetings;
Type
 TListClients = TList<PCliente>;

type
  TfServerControl = class(TForm)
    MainMenu1: TMainMenu;
    sbAcesso: TStatusBar;
    Arquivo1: TMenuItem;
    ConfigurarServidor1: TMenuItem;
    N1: TMenuItem;
    LigarServidor1: TMenuItem;
    DesligarServidor1: TMenuItem;
    N2: TMenuItem;
    Sair1: TMenuItem;
    idTCPMain: TIdTCPServer;
    IdSTDTCPMain: TIdSchedulerOfThreadDefault;
    idAFTCPMain: TIdAntiFreeze;
    ipPSDeskTop: TUDPSuperServer;
    ipPSFiles: TUDPSuperServer;
    ipCommandsServer: TUDPSuperServer;
    Connections_ListView: TListView;
    Ping_Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Sair1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LigarServidor1Click(Sender: TObject);
    procedure DesligarServidor1Click(Sender: TObject);
    procedure Ping_TimerTimer(Sender: TObject);
  private
    { Private declarations }
   FConexoes : TListClients; //Lista de Peers Conectados
   Function  GenerateID(strMAC, strHD : String)        : String;
   Function  GeneratePassword(LastPassword   : String) : String;
   Function  StartServer(Value : Boolean = False) : Boolean;
   Procedure CloseConnections;
   Procedure ReadyToSendServer(ConnectionId : Integer);
   Procedure ReadyToSendFiles(ABinding: TIdSocketHandle);
   Procedure ReadyToSendOther(ConnectionId : Integer);
   Procedure TCPMainConnected(ConnectionId,
                              StatusCode        : Integer;
                              Const Description : String);
   Procedure TCPMainDisconnected(ConnectionId,
                                 StatusCode: Integer;
                                 Const Description: String);
   Procedure TCPMainDataIn(ConnectionId: Integer);
   Procedure TCPMainReadyToSendServer(ConnectionId : Integer);
   Procedure DeskTopReadyToSend(ABinding: TIdSocketHandle);
   procedure CommandsReadyToSend(ABinding: TIdSocketHandle);
   procedure DisconnectClient(Value : String);
   Procedure UpdateDesktopUDPPort(ClientRect : TPeerConnected);
   Procedure UpdateFilesUDPPort(ClientRect : TPeerConnected);
   Procedure UpdateCommandsUDPPort(ClientRect : TPeerConnected);
   Procedure AddItems(Value : TClientSetings);
   Procedure UpdateItem(Value: TClientSetings);
  public
    { Public declarations }
   ipPSTCPMain     : TipPoolerService;
   CriticalSection : TCriticalSection;
   Procedure ClearItem    (Value : TClientSetings);
   Function FindListItemID(ID    : String): TListItem;
   Function FindItemID    (ID    : String): PCliente;
  end;

var
  fServerControl: TfServerControl;

implementation

{$R *.dfm}

{

Constructor TIpConnection.Create;
Begin
 Main   := -1;
 Target := Main;
 vReply := False;
End;

Constructor TClientSetings.Create;
Begin
 Inherited;
 lItem     := Nil;
 lItem2    := Nil;
 ipDesktop := Nil;
 ipOthers  := Nil;
 ipFiles   := Nil;
End;
}

Procedure TfServerControl.CloseConnections;
Var
 I : Integer;
 vConexoes : TList<PCliente>;
Begin
 vConexoes := FConexoes;
 Try
  System.TMonitor.Enter(vConexoes);
  If vConexoes.Count > 0 Then
   Begin
    For I := vConexoes.Count -1 Downto 0 Do
     Begin
      Try
       If vConexoes[I].ipMain.Main <> '-1' Then
        Begin
         ipPSTCPMain.ReleaseCacheAction(StrToInt(vConexoes[I].ipMain.Main));
         ipPSTCPMain.ClientDisconnect(StrToInt(vConexoes[I].ipMain.Main));
        End;
 //      FreeAndNil(TClientSetings(FConexoes[I]^));
       Dispose(vConexoes[I]);
       Connections_ListView.Items.Delete(FindListItemID(vConexoes[I].ID).Index);
       vConexoes[I] := Nil;
       vConexoes.Delete(I);
      Except

      End;
     End;
   End;
 Finally
  System.TMonitor.PulseAll(vConexoes);
  System.TMonitor.Exit(vConexoes);
 End;
End;

Function TfServerControl.StartServer(Value : Boolean = False) : Boolean;
Begin
 Result                   := False;
 Try
  Ping_Timer.Enabled      := False;
  CloseConnections;
  ipPSTCPMain.Active      := False;
  ipPSDeskTop.Active      := False;
  ipCommandsServer.Active := False;
  ipPSFiles.Active        := False;
 Except
 End;
 If Value Then
  Begin
   Try
    ipPSTCPMain.Active      := Value;
    ipPSDeskTop.Active      := Value;
    ipCommandsServer.Active := Value;
    ipPSFiles.Active        := Value;
    Result                  := True;
    Ping_Timer.Enabled      := (Result) And (PingTest);
    sbAcesso.SimpleText     := 'Servidor Ligado...';
   Except
    sbAcesso.SimpleText     := 'Erro ao ligar o Serviço...';
   End;
  End
 Else
  sbAcesso.SimpleText      := 'Servidor Desligado...';
 LigarServidor1.Enabled    := Not ipPSTCPMain.Active;
 DesligarServidor1.Enabled := Not LigarServidor1.Enabled;
End;

procedure TfServerControl.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 StartServer;
 FreeAndNil(CriticalSection);
 ipPSTCPMain.Active      := False;
 ipPSDeskTop.Active      := False;
 ipCommandsServer.Active := False;
 ipPSFiles.Active        := False;
 FreeAndNil(FConexoes);
 fServerControl := Nil;
 Release;
End;

Function MemoryStreamToString(M : TMemoryStream) : String;
Var
 _MemStr : TStringStream;
Begin
 _MemStr := TStringStream.Create;
 Try
  _MemStr.LoadFromStream(M);
 Finally
  Result := _MemStr.DataString;
  FreeAndNil(_MemStr);
 End;
End;

Procedure StringToMemoryStream(Value : TMemoryStream; Str : String);
Var
 _MemStr : TStringStream;
Begin
 _MemStr := TStringStream.Create(Str);
 Try
  _MemStr.SaveToStream(Value);
 Finally
  FreeAndNil(_MemStr);
 End;
end;

// Decompress Stream with zLib
Function DeCompressStream(Const SrcStream : TMemoryStream;
                          Var   DestStream : TMemoryStream): Boolean;
Var
 zipFile : TZDecompressionStream;
Begin
  Result := False;
  Try
   SrcStream.Position := 0;
   zipFile := TZDecompressionStream.Create(SrcStream);
   DestStream.Clear;
   DestStream.CopyFrom(zipFile, 0);
   Result := True;
  Finally
   zipFile.DisposeOf;
  End;
end;

Procedure TfServerControl.ReadyToSendFiles(ABinding: TIdSocketHandle);
Var
 ClientRect : TPeerConnected;
begin
 ipPSFiles.PeersConnected.GetPeerInfo(ABinding.PeerIP, ABinding.PeerPort, ClientRect);
 If ClientRect <> Nil Then
  UpdateFilesUDPPort(ClientRect);
End;

Procedure TfServerControl.ReadyToSendOther(ConnectionId : Integer);
Begin

End;

procedure TfServerControl.ReadyToSendServer(ConnectionId : Integer);
Begin
End;

Procedure TfServerControl.TCPMainConnected(ConnectionId,
                                           StatusCode        : Integer;
                                           Const Description : String);
Begin
End;

Procedure TfServerControl.TCPMainDisconnected(ConnectionId,
                                              StatusCode: Integer;
                                              Const Description: String);
 Procedure DeskTopTargetOff(Id : TPackString);
 Var
  I : Integer;
  vConexoes : TList<PCliente>;
 Begin
  Try
   vConexoes := FConexoes;
   I := vConexoes.Count -1;
   System.TMonitor.Enter(vConexoes);
   While I > -1 Do
    Begin
     If vConexoes[I].ipDesktop.Main = Id Then
      Begin
       vConexoes[I].ipDesktop.Target := '-1';
       Exit;
      End;
     Dec(I);
    End;
  Finally
   System.TMonitor.PulseAll(vConexoes);
   System.TMonitor.Exit(vConexoes);
  End;
 End;
 Procedure CommandsTargetOff(Id : TPackString);
 Var
  I : Integer;
  vConexoes : TList<PCliente>;
 Begin
  Try
   vConexoes := FConexoes;
   I := vConexoes.Count -1;
   System.TMonitor.Enter(vConexoes);
   While I > -1 Do
    Begin
     If vConexoes[I].ipOthers.Main = Id Then
      Begin
       vConexoes[I].ipOthers.Target := '-1';
       Exit;
      End;
     Dec(I);
    End;
  Finally
   System.TMonitor.PulseAll(vConexoes);
   System.TMonitor.Exit(vConexoes);
  End;
 End;
 Procedure FilesTargetOff(Id : TPackString);
 Var
  I : Integer;
  vConexoes : TList<PCliente>;
 Begin
  Try
   vConexoes := FConexoes;
   I := vConexoes.Count -1;
   System.TMonitor.Enter(vConexoes);
   While I > -1 Do
    Begin
     If vConexoes[I].ipFiles.Main = Id Then
      Begin
       vConexoes[I].ipFiles.Target := '-1';
       Exit;
      End;
     Dec(I);
    End;
  Finally
   System.TMonitor.PulseAll(vConexoes);
   System.TMonitor.Exit(vConexoes);
  End;
 End;
 Procedure MainTargetOff(Id : TPackString);
 Var
  I : Integer;
  vConexoes : TList<PCliente>;
 Begin
  Try
   vConexoes := FConexoes;
   I := vConexoes.Count -1;
   System.TMonitor.Enter(vConexoes);
   While I > -1 Do
    Begin
     If vConexoes[I].ipMain.Main = Id Then
      Begin
       vConexoes[I].ipMain.Target := '-1';
       Exit;
      End;
     Dec(I);
    End;
  Finally
   System.TMonitor.PulseAll(vConexoes);
   System.TMonitor.Exit(vConexoes);
  End;
 End;
 Function CheckIDExists(ID : String)  : Boolean;
 Var
  I : Integer;
  vConexoes : TList<PCliente>;
 Begin
  vConexoes := FConexoes;
  Result := False;
  Try
  For I := vConexoes.Count -1 Downto 0 do
   Begin
    Result := Trim(vConexoes[I]^.ID) = Trim(ID);
    If Result Then
     Break;
   End;
  Finally

  End;
 End;
Var
 I,
 vTarget   : Integer;
 vConexoes : TList<PCliente>;
Begin
 Try
  vTarget := -1;
  vConexoes := FConexoes;
  I := vConexoes.Count -1;
  System.TMonitor.PulseAll(vConexoes);
  While I > -1 Do
   Begin
    If StrToInt(vConexoes[I]^.ipMain.Main) = ConnectionId Then
     Begin
      ClearItem(vConexoes[I]^);
      If vConexoes[I]^.ipMain.Target <> '-1' Then
       Begin
        Try
         vTarget := StrToInt(vConexoes[I]^.ipMain.Target);
         vConexoes[I]^.ipMain.Target     := '-1';
         vConexoes[I]^.ipDesktop.Target  := '-1';
         vConexoes[I]^.ipFiles.Target  := '-1';
         ipPSTCPMain.Write(vTarget, '<|DISCONNECTED|>' + vCommandEnd);
        Except
        End;
       End;
      Connections_ListView.Items.Delete(FindListItemID(vConexoes[I].ID).Index);
      ipPSTCPMain.ReleaseCacheAction(ConnectionId);
      Dispose(vConexoes[I]);
      vConexoes.Delete(I);
      If vTarget > -1 Then
       Begin
        MainTargetOff(IntToStr(vTarget));
        ipPSTCPMain.ReleaseCacheAction(vTarget);
        ipPSTCPMain.ClientDisconnect(vTarget);
       End;
      Break;
     End;
    Dec(I);
   End;
 Except
 End;
 {$IFDEF MSWINDOWS}
 {$IFNDEF FMX}Application.Processmessages;
       {$ELSE}FMX.Forms.TApplication.ProcessMessages;{$ENDIF}
 {$ENDIF}
 Sleep(1);
 System.TMonitor.PulseAll(vConexoes);
// System.TMonitor.Exit(vConexoes);
End;

procedure TfServerControl.DisconnectClient(Value : String);
Var
 I : Integer;
 vConexoes : TList<PCliente>;
Begin
 vConexoes := FConexoes;
 Try
  I := vConexoes.Count -1;
  While I > -1 Do
   Begin
    If Trim(vConexoes[I].ID) = Trim(Value) Then
     Begin
      If vConexoes[I].ipMain.Main <> '-1' Then
       Begin
        ClearItem(vConexoes[I]^);
        ipPSTCPMain.ClientDisconnect(StrToInt(vConexoes[I].ipMain.Main));
       End;
      Break;
     End;
    Dec(I);
   End;
 Except
 End;
// FConexoes.unlockList;
End;

Function TfServerControl.FindItemID(ID : String) : PCliente;
Var
 I : Integer;
 vConexoes : TList<PCliente>;
Begin
 vConexoes := FConexoes;
 Result    := Nil;
 Try
 For I := vConexoes.Count -1 Downto 0 do
  Begin
   If Trim(vConexoes[I]^.ID) = Trim(ID) Then
    Begin
     Result := vConexoes[I];
     Break;
    End;
  End;
 Finally
 End;
End;

procedure TfServerControl.UpdateItem(Value : TClientSetings);
Var
 ListItem : TListItem;
 dCliente : PCliente;
Begin
 ListItem := FindListItemID(Value.ID);
 If ListItem <> Nil Then
  Begin
   ListItem.SubItems[3] := Value.TargetID;
   ListItem.SubItems[4] := IntToStr(Value.EndPing);
  End;
 ListItem := FindListItemID(Value.TargetID);
 If ListItem <> Nil Then
  Begin
   ListItem.SubItems[3] := Value.ID;
   dCliente             := FindItemID(Value.TargetID);
   If dCliente <> Nil Then
    ListItem.SubItems[4] := IntToStr(dCliente^.EndPing);
  End;
End;

procedure TfServerControl.ClearItem(Value : TClientSetings);
Var
 ListItem : TListItem;
 dCliente : PCliente;
Begin
 ListItem := FindListItemID(Value.ID);
 If ListItem <> Nil Then
  Begin
   ListItem.SubItems[3] := '';
   ListItem.SubItems[4] := IntToStr(Value.EndPing);
  End;
 ListItem := FindListItemID(Value.TargetID);
 If ListItem <> Nil Then
  Begin
   ListItem.SubItems[3] := '';
   dCliente             := FindItemID(Value.TargetID);
   If dCliente <> Nil Then
    ListItem.SubItems[4] := IntToStr(dCliente^.EndPing);
  End;
End;

procedure TfServerControl.AddItems(Value : TClientSetings);
Var
 L : TListItem;
Begin
 L         := Connections_ListView.Items.Add;
 L.Caption := IntToStr(L.Index);
 L.SubItems.Add(Value.IP);
 L.SubItems.Add(Value.ID);
 L.SubItems.Add(Value.Password);
 L.SubItems.Add('');
 L.SubItems.Add('Calculating...');
 L.SubItems.Objects[4] := TObject(0);
End;

Function TfServerControl.FindListItemID(ID : string): TListItem;
Var
 i : Integer;
Begin
 i := 0;
 While i < Connections_ListView.Items.Count do
  Begin
   If (Connections_ListView.Items.Item[i].SubItems[1] = ID) Then
    Break;
   Inc(i);
  End;
 Result := Connections_ListView.Items.Item[i];
end;

procedure TfServerControl.TCPMainDataIn(ConnectionId : Integer);
Var
 vDifTeste : String;
 IpPort    : TIpPort;
 Function InCommands(s : String) : Boolean;
 Begin
  Result := (Pos('<|PONG|>',            Uppercase(s)) > 0) or (Pos('<|MAINSOCKET|>', Uppercase(s)) > 0)     or
            (Pos('<|DESKTOPSOCKET|>',   Uppercase(s)) > 0) or (Pos('<|KEYBOARDSOCKET|>', Uppercase(s)) > 0) or
            (Pos('WRITEOK',             Uppercase(s)) > 0) or (Pos('<|FINDID|>', Uppercase(s)) > 0)         or
            (Pos('<|CHECKIDPASSWORD|>', Uppercase(s)) > 0) or (Pos('<|RELATION|>', Uppercase(s)) > 0)       or
            (Pos('<|REDIRECT|>',        Uppercase(s)) > 0) or (Pos('<|$INITSTREAM$|>', Uppercase(s)) > 0)   or
            (Pos('<|GETMYIP|>',         Uppercase(s)) > 0);
 End;
Var
 vLine2,
 vLine,
 vTempTarget     : String;
 I               : Integer;
 vConexoes       : TList<PCliente>;
 vInitConnection,
 vFinalConnection : PCliente;
 Function IdInUse(Value : String) : Boolean;
 Var
  I : Integer;
  vConexoes : TList<PCliente>;
 Begin
  Result := False;
  vConexoes := FConexoes;
  For I := vConexoes.Count -1 Downto 0 do
   Begin
    Result := Trim(vConexoes[I]^.TargetID) = Trim(Value);
    If Result Then
     Break;
   End;
//  FConexoes.UnlockList;
 End;
 Function TestDiff(Ping1, Ping2 : Integer) : String;
 Begin
  Result := 'DIFF';
  If //(Ping1 > 60) Or
     (Ping2 > 100) Then
   Result := 'BLOCKS';
 End;
 Procedure PeerConnectedDesc(ConnectionId : String; Var Value : String);
 Var
  I : Integer;
  vConexoes : TList<PCliente>;
 Begin
  vConexoes := FConexoes;
  Value     := '';
  For I := vConexoes.Count -1 Downto 0 do
   Begin
    If Trim(vConexoes[I]^.TargetID) = Trim(ConnectionId) Then
     Begin
      Value := Format('%s<|>%s<|>%d!%d<|>%s<|>%s<|>%d!%d<|>%s<|>%s<|>%d!%d',
                      [vConexoes[I]^.ipDesktop.Host,
                       vConexoes[I]^.ipDesktop.LocalIP,
                       vConexoes[I]^.ipDesktop.UpdPort,
                       vConexoes[I]^.ipDesktop.UdpLocalPort,
                       vConexoes[I]^.ipFiles.Host,
                       vConexoes[I]^.ipFiles.LocalIP,
                       vConexoes[I]^.ipFiles.UpdPort,
                       vConexoes[I]^.ipFiles.UdpLocalPort,
                       vConexoes[I]^.ipOthers.Host,
                       vConexoes[I]^.ipOthers.LocalIP,
                       vConexoes[I]^.ipOthers.UpdPort,
                       vConexoes[I]^.ipOthers.UdpLocalPort]);
      Break;
     End;
   End;
//  FConexoes.UnlockList;
 End;
 Function CheckIDPassword(Target, TargetPass : String) : Boolean;
 Var
  I : Integer;
  vConexoes : TList<PCliente>;
 Begin
  vConexoes := FConexoes;
  Result := False;
  Try
  For I := vConexoes.Count -1 Downto 0 do
   Begin
    If Trim(vConexoes[I]^.ID) = Trim(Target) Then
     Begin
      Result := vConexoes[I]^.Password = TargetPass;
      Break;
     End;
   End;
  Finally

  End;
 End;
 Function CheckIDExists(ID : String)  : Boolean;
 Var
  I : Integer;
  vConexoes : TList<PCliente>;
 Begin
  vConexoes := FConexoes;
  Result := False;
  Try
  For I := vConexoes.Count -1 Downto 0 do
   Begin
    Result := Trim(vConexoes[I]^.ID) = Trim(ID);
    If Result Then
     Break;
   End;
  Finally

  End;
 End;
Begin
 If ipPSTCPMain.HasBuffer(ConnectionId) > 0 Then
  Begin
   vLine := ipPSTCPMain.ReadAll(ConnectionId);
   vConexoes := FConexoes;
   Try
    For I := vConexoes.Count -1 Downto 0 do
     Begin
      If StrToInt(vConexoes[I]^.ipMain.Main) = ConnectionId Then
       Begin
        While (vLine <> '') Do
         Begin
          Try
           If ipPSTCPMain.CheckConnect(ConnectionId) Then
            Begin
   //          vLine := ipPSTCPMain.ReadAll(ConnectionId);
             If (Pos('<|PONG|>', vLine) > 0) Then
              Begin
               Delete(vLine, 1, Pos(vCommandEnd, vLine) + Length(vCommandEnd) -1);
               vConexoes[I]^.EndPing := (GetTickCount - vConexoes[I]^.StartPing);
               UpdateItem(vConexoes[I]^);
//               FindListItemID(vConexoes[I]^.ID).
              End;
             If (Pos('<|GETMYIP|>', vLine) > 0) Then
              Begin
               Delete(vLine, 1, Pos(vCommandEnd, vLine) + Length(vCommandEnd) -1);
               IpPort := ipPSTCPMain.GetIP(ConnectionId);
               ipPSTCPMain.Write(StrToInt(vConexoes[I]^.ipMain.Main), '<|MYIP|>' + IpPort.IP + vCommandEnd);
              End;
             If (Pos('<|MAINSOCKET|>', vLine) > 0) Then
              Begin
               If (Pos('<|GROUP|>', vLine) > 0) Then
                Begin
                 // Get the Group
                 vLine2 := vLine;
                 Delete(vLine2, 1, Pos('<|MAINSOCKET|>', vLine2) + 22);
                 vConexoes[I]^.Group := vLine2;
                 vConexoes[I]^.Group := Copy(vLine2, 1, Pos('<|>', vLine2) - 1);
                 Delete(vLine2, 1, Pos('<|>', vLine2) + 2);
                 // Get the PC Name
   //              vLine2 := vLine;
                 Delete(vLine2, 1, Pos('<|MACHINE|>', vLine2)+ 10);
                 vConexoes[I]^.Machine := vLine2;
                 vConexoes[I]^.Machine := Copy(vLine2, 1, Pos('<|>', vLine2) - 1);
                 Delete(vLine2, 1, Pos('<|>', vLine2) + 2);
                 // Get the MAC Adress
   //              vLine2 := vLine;
                 Delete(vLine2, 1, Pos('<|MAC|>', vLine2)+ 6);
                 vConexoes[I]^.MAC := vLine2;
                 vConexoes[I]^.MAC := Copy(vLine2, 1, Pos('<|>', vLine2) - 1);
                 Delete(vLine2, 1, Pos('<|>', vLine2) + 2);
                 // Get the HD Adress
   //              vLine2 := vLine;
                 Delete(vLine2, 1, Pos('<|HD|>', vLine2)+ 5);
                 vConexoes[I]^.HD := vLine2;
                 vConexoes[I]^.HD := Copy(vLine2, 1, Pos('<|>', vLine2) - 1);
                 Delete(vLine2, 1, Pos('<|>', vLine2) + 2);
                 // Get the HD Adress
   //              vLine2 := vLine;
                 Delete(vLine2, 1, Pos('<|LASTPASSWORD|>', vLine2)+ 15);
                 vConexoes[I]^.LastPassword := Copy(vLine2, 1, Pos(vCommandEnd, vLine2) - 1);
                 Delete(vLine2, 1, Pos(vCommandEnd, vLine2) + Length(vCommandEnd) -1);
                 vLine := vLine2;
                End;
               vConexoes[I]^.ID           := GenerateID(vConexoes[I]^.MAC, vConexoes[I]^.HD);
               If (vConexoes[I]^.LastPassword = '') Then
                Begin
                 vConexoes[I]^.Password     := GeneratePassword(vConexoes[I]^.Password);
                 vConexoes[I]^.LastPassword := vConexoes[I]^.Password;
                End
               Else
                vConexoes[I]^.Password     := vConexoes[I]^.LastPassword;
               If vConexoes[I]^.LastPassword > '' Then
                ipPSTCPMain.Write(StrToInt(vConexoes[I]^.ipMain.Main), '<|ID|>' + vConexoes[I]^.ID + '<|>' + vConexoes[I]^.LastPassword + vCommandEnd)
               Else
                ipPSTCPMain.Write(StrToInt(vConexoes[I]^.ipMain.Main), '<|ID|>' + vConexoes[I]^.ID + '<|>' + vConexoes[I]^.Password + vCommandEnd);
               AddItems(vConexoes[I]^);
              End
             Else If (Pos('<|GETCONNECTCLIENTINFO|>', vLine) > 0) Then
              Begin
               vLine2 := '';
               PeerConnectedDesc(vConexoes[I]^.ID, vTempTarget);
               ipPSTCPMain.Write(StrToInt(vConexoes[I]^.ipMain.Main), '<|RESULTCONNECTCLIENTINFO|>' + vTempTarget + vCommandEnd);
               vLine := vLine2;
              End
             Else If (Pos('<|FINDID|>', vLine) > 0) Then
              Begin
               vLine2 := vLine;
               Delete(vLine2, 1, Pos('<|FINDID|>', vLine2) + 9);
               vTempTarget := Copy(vLine2, 1, Pos('<|>', vLine2) - 1);
               If Not IdInUse(vTempTarget) Then
                Begin
                 If (CheckIDExists(vTempTarget)) Then
                  ipPSTCPMain.Write(StrToInt(vConexoes[I]^.ipMain.Main),  '<|IDEXISTS!REQUESTPASSWORD|>' + vCommandEnd)
                 Else
                  ipPSTCPMain.Write(StrToInt(vConexoes[I]^.ipMain.Main),  '<|IDNOTEXISTS|>' + vCommandEnd);
                End
               Else
                ipPSTCPMain.Write  (StrToInt(vConexoes[I]^.ipMain.Main),    '<|ACCESSBUSY|>' + vCommandEnd);
               Delete(vLine2, 1, Pos(vCommandEnd, vLine2) + Length(vCommandEnd) -1);
               vLine := vLine2;
              End
             Else If (Pos('<|CHECKIDPASSWORD|>', vLine) > 0) Then
              Begin
               vLine2 := vLine;
               Delete(vLine2, 1, Pos('<|CHECKIDPASSWORD|>', vLine2) + 18);
               vTempTarget := Copy(vLine2, 1, Pos('<|>', vLine2) - 1);
               Delete(vLine2, 1, Pos('<|>', vLine2) + 2);
               vConexoes[I]^.TargetPassword := Copy(vLine2, 1, Pos('<|>', vLine2) - 1);
               Delete(vLine2, 1, Pos(vCommandEnd, vLine2) + Length(vCommandEnd) -1);
               If (CheckIDPassword(vTempTarget, vConexoes[I]^.TargetPassword)) then
                Begin
                 vConexoes[I]^.TargetID := vTempTarget;
                 ipPSTCPMain.Write(StrToInt(vConexoes[I]^.ipMain.Main), '<|ACCESSGRANTED|>' + vCommandEnd);
                End
               Else
                ipPSTCPMain.Write(StrToInt(vConexoes[I]^.ipMain.Main),  '<|ACCESSDENIED|>' + vCommandEnd);
               Delete(vLine2, 1, Pos(vCommandEnd, vLine2) + Length(vCommandEnd) -1);
               vLine := vLine2;
              End
             Else If (Pos('<|REDIRECT|>', vLine) > 0) Then
              Begin
               If vConexoes[I]^.ipMain.Target <> '-1' Then
                If ipPSTCPMain.CheckConnect(StrToInt(vConexoes[I]^.ipMain.Target)) Then
                 ipPSTCPMain.Write( StrToInt(vConexoes[I]^.ipMain.Target), vLine)
                Else
                 Begin
                  DisconnectClient(vConexoes[I]^.TargetID);
                  DisconnectClient(vConexoes[I]^.ID);
                 End;
               Delete(vLine, 1, Pos(vCommandEnd, vLine) + Length(vCommandEnd) -1);
              End;
             If (Pos('<|RELATION|>', vLine) > 0) Then
              Begin
               vLine2 := vLine;
               Delete(vLine2, 1, Pos('<|RELATION|>', vLine2) + 11);
               vConexoes[I]^.ID := Copy(vLine2, 1, Pos('<|>', vLine2) - 1);
               Delete(vLine2, 1, Pos('<|>', vLine2) + 2);
               vConexoes[I]^.TargetID := Copy(vLine2, 1, Pos('<|>', vLine2) - 1);
               Delete(vLine2, 1, Pos('<|>', vLine2) + 2);
               vConexoes[I]^.lItem    := FindListItemID(vConexoes[I]^.ID);
               vConexoes[I]^.lItem2   := FindListItemID(vConexoes[I]^.TargetID);
               vInitConnection  := FindItemID(vConexoes[I]^.ID);
               vFinalConnection := FindItemID(vConexoes[I]^.TargetID);
               vFinalConnection^.TargetID := vConexoes[I]^.ID;
               UpdateItem(vConexoes[I]^);
//               InsertTargetID(vConexoes[I]);
               // Relates the main Sockets
               vInitConnection^.ipMain.Target  := vFinalConnection^.ipMain.Main;
               vFinalConnection^.ipMain.Target := vInitConnection^.ipMain.Main;
               //Teste para escolha automática de tipo de captura
               vDifTeste := TestDiff(vInitConnection^.EndPing,
                                     vFinalConnection^.EndPing);
               If PingTest Then
                Begin
                 ipPSTCPMain.Write(StrToInt(vInitConnection^.ipMain.Main),  '<|TESTEDIFF|>' + vDifTeste + vCommandEnd);
                 ipPSTCPMain.Write(StrToInt(vFinalConnection^.ipMain.Main), '<|TESTEDIFF|>' + vDifTeste + vCommandEnd);
                End;
               // XyberX
               if Pos(vCommandEnd, vLine2) = 0 then
                vLine2 := vLine2 + vCommandEnd;
               //Liga as Conexões Secundárias
               ipPSTCPMain.Write(StrToInt(vInitConnection^.ipMain.Main),  '<|STARTCONNECTIONS|>' + vCommandEnd);
               ipPSTCPMain.Write(StrToInt(vFinalConnection^.ipMain.Main), '<|STARTCONNECTIONS|>' + vCommandEnd);
               //Diz ao Cliente que está pedindo o Desktop para se colocar em posição de começar a Receber os dados
{
               ipPSTCPMain.Write(PCliente(vConexoes[I]^.lItem.SubItems.Objects[0])^.ipMain.Main,  '<|STARTEXEC|>'        + vLine2);
               ipPSTCPMain.Write(PCliente(vConexoes[I]^.lItem2.SubItems.Objects[0])^.ipMain.Main, '<|STARTEXEC|>'        + vLine2);
}
               //Diz ao Cliente que está enviando o Desktop para se colocar em posição de começar a Enviar os dados
               {
               ipPSTCPMain.Write(PCliente(vConexoes[I]^.lItem2.SubItems.Objects[0])^.ipMain.Main, '<|ACCESSING|>'        + vCommandEnd);
               ipPSTCPMain.Write(PCliente(vConexoes[I]^.lItem2.SubItems.Objects[0])^.ipMain.Main, '<|STARTEXECOMMANDS|>' + vLine2);
               ipPSTCPMain.Write(PCliente(vConexoes[I]^.lItem.SubItems.Objects[0])^.ipMain.Main,  '<|INITCAPTURE|>'      + vCommandEnd);
               }
               Delete(vLine2, 1, Pos(vCommandEnd, vLine2) + Length(vCommandEnd) -1);
               vLine := vLine2;
              End;
            End;
          Except
           Break;
          End;
          vLine := '';
         End;
        Break;
       End;
     End;
   Finally
//    FConexoes.unlockList;
   End;
  End;
End;

Procedure TfServerControl.UpdateFilesUDPPort(ClientRect : TPeerConnected);
Var
 I         : Integer;
 vConexoes : TList<PCliente>;
Begin
 Try
  vConexoes := FConexoes;
  System.TMonitor.Enter(vConexoes);
  For I := vConexoes.Count -1 DownTo 0 Do
   Begin
    If vConexoes[I]^.ID = ClientRect.WelcomeMessage Then
     Begin
      vConexoes[I]^.ipFiles.Host         := ClientRect.RemoteIP;
      vConexoes[I]^.ipFiles.UpdPort      := ClientRect.Port;
      vConexoes[I]^.ipFiles.LocalIP      := ClientRect.LocalIP;
      vConexoes[I]^.ipFiles.UdpLocalPort := ClientRect.LocalPort;
      If vConexoes[I]^.ipFiles.UpdPort = 0 Then
       vConexoes[I]^.ipFiles.UpdPort     := vConexoes[I]^.ipFiles.UdpLocalPort;
      Break;
     End;
   End;
 Finally
  System.TMonitor.PulseAll(vConexoes);
  System.TMonitor.Exit(vConexoes);
 End;
End;

Procedure TfServerControl.UpdateDesktopUDPPort(ClientRect : TPeerConnected);
Var
 I         : Integer;
 vConexoes : TList<PCliente>;
Begin
 Try
  vConexoes := FConexoes;
  System.TMonitor.Enter(vConexoes);
  For I := vConexoes.Count -1 DownTo 0 Do
   Begin
    If vConexoes[I]^.ID = ClientRect.WelcomeMessage Then
     Begin
      vConexoes[I]^.ipDesktop.Host         := ClientRect.RemoteIP;
      vConexoes[I]^.ipDesktop.UpdPort      := ClientRect.Port;
      vConexoes[I]^.ipDesktop.LocalIP      := ClientRect.LocalIP;
      vConexoes[I]^.ipDesktop.UdpLocalPort := ClientRect.LocalPort;
      If vConexoes[I]^.ipDesktop.UpdPort = 0 Then
       vConexoes[I]^.ipDesktop.UpdPort     := vConexoes[I]^.ipDesktop.UdpLocalPort;
      Break;
     End;
   End;
 Finally
  System.TMonitor.PulseAll(vConexoes);
  System.TMonitor.Exit(vConexoes);
 End;
End;

procedure TfServerControl.DeskTopReadyToSend(ABinding: TIdSocketHandle);
Var
 ClientRect : TPeerConnected;
begin
 ipPSDeskTop.PeersConnected.GetPeerInfo(ABinding.PeerIP, ABinding.PeerPort, ClientRect);
 If ClientRect <> Nil Then
  UpdateDesktopUDPPort(ClientRect);
End;

procedure TfServerControl.CommandsReadyToSend(ABinding: TIdSocketHandle);
Var
 ClientRect : TPeerConnected;
begin
 ipCommandsServer.PeersConnected.GetPeerInfo(ABinding.PeerIP, ABinding.PeerPort, ClientRect);
 If ClientRect <> Nil Then
  UpdateCommandsUDPPort(ClientRect);
End;

Procedure TfServerControl.UpdateCommandsUDPPort(ClientRect : TPeerConnected);
Var
 I         : Integer;
 vConexoes : TList<PCliente>;
Begin
 Try
  vConexoes := FConexoes;
  System.TMonitor.Enter(vConexoes);
  For I := vConexoes.Count -1 DownTo 0 Do
   Begin
    If vConexoes[I]^.ID = ClientRect.WelcomeMessage Then
     Begin
      vConexoes[I]^.ipOthers.Host         := ClientRect.RemoteIP;
      vConexoes[I]^.ipOthers.UpdPort      := ClientRect.Port;
      vConexoes[I]^.ipOthers.LocalIP      := ClientRect.LocalIP;
      vConexoes[I]^.ipOthers.UdpLocalPort := ClientRect.LocalPort;
      If vConexoes[I]^.ipOthers.UpdPort = 0 Then
       vConexoes[I]^.ipOthers.UpdPort     := vConexoes[I]^.ipOthers.UdpLocalPort;
      Break;
     End;
   End;
 Finally
  System.TMonitor.PulseAll(vConexoes);
  System.TMonitor.Exit(vConexoes);
 End;
End;

procedure TfServerControl.TCPMainReadyToSendServer(ConnectionId : Integer);
Var
 ClienteNovo : PCliente;
 IpPort      : TIpPort;
begin
 New(ClienteNovo);
 ClienteNovo^.ipMain.Target          := '-1';
 ClienteNovo^.ipMain.Main            := IntToStr(ConnectionId);
 IpPort                              := ipPSTCPMain.GetIP(ConnectionId);
 ClienteNovo^.ipMain.Host            := IpPort.IP;
 ClienteNovo^.ipDesktop.Main         := '-1';
 ClienteNovo^.ipDesktop.Target       := '-1';
 ClienteNovo^.ipDesktop.Host         := '';
 ClienteNovo^.ipDesktop.UpdPort      := -1;
 ClienteNovo^.ipDesktop.UdpLocalPort := -1;
 ClienteNovo^.ipFiles.Main           := '-1';
 ClienteNovo^.ipFiles.Target         := '-1';
 ClienteNovo^.ipFiles.Host           := '';
 ClienteNovo^.ipFiles.UpdPort        := -1;
 ClienteNovo^.ipFiles.UdpLocalPort   := -1;
 ClienteNovo^.ipOthers.Main          := '-1';
 ClienteNovo^.ipOthers.Target        := '-1';
 ClienteNovo^.ipOthers.Host          := '';
 ClienteNovo^.ipOthers.UpdPort       := -1;
 ClienteNovo^.ipOthers.UdpLocalPort  := -1;
 ClienteNovo^.lItem                  := Nil;
 ClienteNovo^.lItem2                 := Nil;
 ClienteNovo^.IP                     := IpPort.IP;
 FConexoes.Add(ClienteNovo);
End;

procedure TfServerControl.FormCreate(Sender: TObject);
begin
 CriticalSection                    := TCriticalSection.Create;
 ipPSDeskTop.Port                   := 8082;
 ipPSDeskTop.TCPPort                := 8083;
 ipPSDeskTop.BufferSize             := MaxBuffer;
 ipPSDeskTop.OnClientConnected      := DeskTopReadyToSend;
 ipCommandsServer.Port              := 8079;
 ipCommandsServer.TCPPort           := 8084;
 ipCommandsServer.BufferSize        := MaxBuffer;
 ipCommandsServer.OnClientConnected := CommandsReadyToSend;
 ipPSFiles.Port                     := 8081;
 ipPSFiles.TCPPort                  := 8085;
 ipPSFiles.OnClientConnected        := ReadyToSendFiles;
 ipPSFiles.BufferSize               := MaxBuffer;
 ipPSTCPMain                        := TipPoolerService.Create(Self);
 ipPSTCPMain.Compression            := False;
 ipPSTCPMain.EncryptData            := False;
 ipPSTCPMain.Port                   := 8078;
 ipPSTCPMain.TimeOut                := TimeOutC;
 ipPSTCPMain.TCPObject              := idTCPMain;
 ipPSTCPMain.OnDisconnectedServer   := TCPMainDisconnected;
 ipPSTCPMain.OnConnectedServer      := TCPMainConnected;
 ipPSTCPMain.OnDataInServer         := TCPMainDataIn;
 ipPSTCPMain.OnReadyToSendServer    := TCPMainReadyToSendServer;
 ipPSTCPMain.GarbageTime            := GarbageTimeC;
 ipPSTCPMain.AutoGarbage            := True;
 ipPSTCPMain.LineBreak              := vEOB;
 FConexoes                          := TListClients.Create;
 Ping_Timer.Interval                := TPingTimer;
 StartServer;
end;

Function TfServerControl.GenerateID(strMAC, strHD : String): String;
Var
 I         : Integer;
 ID        : String;
 Exists    : Boolean;
 vConexoes : TList<PCliente>;
Begin
 Randomize;
 ID := GenerateIDUnique(strMAC, strHD);
 Exists := False;
 vConexoes := FConexoes;
 Try
  For I := vConexoes.Count - 1 DownTo 0 do
   Begin
    Exists := (vConexoes[i].ID = ID);
    If Exists Then
     Break;
   End;
 Finally
  Result := ID;
//  FConexoes.UnlockList;
 End;
End;

Function TfServerControl.GeneratePassword(LastPassword : String) : String;
Begin
 Randomize;
 If (LastPassword <> '') Then
  Result := LastPassword
 Else
  Result := IntToStr(Random(9)) + IntToStr(Random(9)) + IntToStr(Random(9)) + IntToStr(Random(9));
End;

procedure TfServerControl.DesligarServidor1Click(Sender: TObject);
begin
 StartServer;
end;

procedure TfServerControl.LigarServidor1Click(Sender: TObject);
begin
 StartServer(True);
end;

procedure TfServerControl.Ping_TimerTimer(Sender: TObject);
Var
 vTarget,
 I         : Integer;
 vConexoes : TList<PCliente>;
Begin
 vConexoes := FConexoes;
 Try
  For I := vConexoes.Count - 1 DownTo 0 do
   Begin
    vTarget := StrToInt(vConexoes[I].ipMain.Main);
    vConexoes[I]^.StartPing := GetTickCount;
    ipPSTCPMain.Write(vTarget, '<|PING|>' + vCommandEnd);
   End;
 Finally
 End;
End;

procedure TfServerControl.Sair1Click(Sender: TObject);
begin
 Close;
end;

end.
