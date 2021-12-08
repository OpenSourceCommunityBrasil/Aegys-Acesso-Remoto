unit uCtrl_Conexoes;

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
  System.Classes, System.Generics.Collections, uConstants, System.Win.ScktComp,
  uCtrl_ThreadsService,IdHashMessageDigest;

type
  TConexao = class
  private
    FID,
    FMAC,
    FHD,
    FLatencia,
    FProtocolo,
    FSenha,
    FSenhaGerada  : String;
    FThreadAreaRemota : TThreadConexaoAreaRemota;
    FThreadArquivos   : TThreadConexaoArquivos;
    FThreadPrincipal  : TThreadConexaoPrincipal;
    FThreadTeclado    : TThreadConexaoTeclado;
    aSocketAreaRemota,
    aSocketArquivos,
    aSocketPrincipal,
    aSocketTeclado    : TCustomWinSocket;
    FPingInicial,
    FPingFinal        : Int64;
    procedure SetID(const Value: string);
    procedure SetLatencia(const Value: string);
    procedure SetProtocolo(const Value: string);
    procedure SetSenha(const Value: string);
    procedure SetSenhaGerada(const Value: string);
    procedure SetThreadAreaRemota(const Value: TThreadConexaoAreaRemota);
    procedure SetThreadArquivos(const Value: TThreadConexaoArquivos);
    procedure SetThreadPrincipal(const Value: TThreadConexaoPrincipal);
    procedure SetThreadTeclado(const Value: TThreadConexaoTeclado);
    procedure SetPingInicial(const Value: Int64);
    procedure SetPingFinal(const Value: Int64);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Ping;
    procedure CriarThread(AThread: IDThreadType; ASocket: TCustomWinSocket);
    procedure LimparThread(AThread: IDThreadType);
    property MAC : String Read FMAC Write FMAC;
    property HD  : String Read FHD  Write FHD;
    property ID: string read FID write SetID;
    property Latencia: string read FLatencia write SetLatencia;
    property PingFinal: Int64 read FPingFinal write SetPingFinal;
    property PingInicial: Int64 read FPingInicial write SetPingInicial;
    property Protocolo: string read FProtocolo write SetProtocolo;
    property Senha: string read FSenha write SetSenha;
    property SenhaGerada: string read FSenhaGerada write SetSenhaGerada;
    property ThreadAreaRemota: TThreadConexaoAreaRemota read FThreadAreaRemota write SetThreadAreaRemota;
    property ThreadArquivos: TThreadConexaoArquivos read FThreadArquivos write SetThreadArquivos;
    property ThreadPrincipal: TThreadConexaoPrincipal read FThreadPrincipal write SetThreadPrincipal;
    property ThreadTeclado: TThreadConexaoTeclado read FThreadTeclado write SetThreadTeclado;
    property SocketAreaRemota : TCustomWinSocket Read aSocketAreaRemota;
    property SocketArquivos   : TCustomWinSocket Read aSocketArquivos;
    property SocketPrincipal  : TCustomWinSocket Read aSocketPrincipal;
    property SocketTeclado    : TCustomWinSocket Read aSocketTeclado;
  end;

  TConexoes = class
  private
    FListaConexoes: TObjectList<TConexao>;
  public
    constructor Create;
    destructor Destroy; override;
    Function GenerateID(strMAC, strHD : String) : String;
    function GerarID: string;
    function GerarSenha(psw:string): string;
    function RetornaItemPorConexao(AConexao: string): TConexao;
    function RetornaItemPorHandle(AHandle: Integer): TConexao;
    function RetornaItemPorID(AID: string): TConexao;
    function VerificaID(AID: string): Boolean;
    function VerificaIDSenha(AID, ASenha: string): Boolean;
    procedure AdicionarConexao(AProtocolo,
                               MAC, HD, DSenha : String);Overload;
    //procedure AdicionarConexao(AProtocolo: string);Overload;
    procedure RemoverConexao(AProtocolo: string);
    property ListaConexoes: TObjectList<TConexao> read FListaConexoes;
    function MD5String(const texto: string): string;
  end;

implementation

uses
  System.SysUtils, Winapi.Windows;

{ TConexao }

procedure TConexao.SetPingFinal(const Value: Int64);
begin
  FPingFinal := Value;
end;

procedure TConexao.SetPingInicial(const Value: Int64);
begin
  FPingInicial := Value;
end;

procedure TConexao.SetProtocolo(const Value: string);
begin
  FProtocolo := Value;
end;

constructor TConexao.Create;
begin
  PingInicial := 0;
  PingFinal := 256;
end;

procedure TConexao.CriarThread(AThread: IDThreadType; ASocket: TCustomWinSocket);
begin
  case AThread of
    ttPrincipal:
      begin
       LimparThread(ttPrincipal);
       aSocketPrincipal := ASocket;
       FThreadPrincipal := TThreadConexaoPrincipal.Create(ASocket, Protocolo);
      end;
    ttAreaRemota:
      begin
       LimparThread(ttAreaRemota);
       aSocketAreaRemota := ASocket;
       FThreadAreaRemota := TThreadConexaoAreaRemota.Create(ASocket, Protocolo);
      end;
    ttTeclado:
      begin
       LimparThread(ttTeclado);
       aSocketTeclado := ASocket;
       FThreadTeclado := TThreadConexaoTeclado.Create(ASocket, Protocolo);
      end;
    ttArquivos:
      begin
       LimparThread(ttArquivos);
       aSocketArquivos := ASocket;
       FThreadArquivos := TThreadConexaoArquivos.Create(ASocket, Protocolo);
      end;
  end;
end;

destructor TConexao.Destroy;
begin
  if Assigned(FThreadPrincipal) then
    LimparThread(ttPrincipal);
  if Assigned(FThreadAreaRemota) then
    LimparThread(ttAreaRemota);
  if Assigned(FThreadTeclado) then
    LimparThread(ttTeclado);
  if Assigned(FThreadArquivos) then
    LimparThread(ttArquivos);
  inherited;
end;

procedure TConexao.LimparThread(AThread: IDThreadType);
var
  FThread: TThread;
begin
  Protocolo := Protocolo;
  case AThread of
    ttPrincipal:
      begin
        if Assigned(FThreadPrincipal) then
        begin
          if not FThreadPrincipal.Finished then
            FThreadPrincipal.Terminate;
          FThreadPrincipal := nil;
        end;
      end;
    ttAreaRemota:
      begin
        if Assigned(FThreadAreaRemota) then
        begin
          if not FThreadAreaRemota.Finished then
            FThreadAreaRemota.Terminate;
          FThreadAreaRemota := nil;
        end;
      end;
    ttTeclado:
      begin
        if Assigned(FThreadTeclado) then
        begin
          if not FThreadTeclado.Finished then
            FThreadTeclado.Terminate;
          FThreadTeclado := nil;
        end;
      end;
    ttArquivos:
      begin
        if Assigned(FThreadArquivos) then
        begin
          if not FThreadArquivos.Finished then
            FThreadArquivos.Terminate;
          FThreadArquivos := nil;
        end;
      end;
  end;
end;

procedure TConexao.Ping;
var
  FSocket: TCustomWinSocket;
begin
  try
    if ThreadPrincipal = nil then
      Exit;

    FSocket := ThreadPrincipal.scClient;

    if (FSocket = nil) or not(FSocket.Connected) then
      Exit;

    FSocket.SendText('<|PING|>');
    PingInicial := GetTickCount;

    if Latencia <> 'Calculando...' then
      FSocket.SendText('<|SETPING|>' + IntToStr(PingFinal) + '<|END|>');
  except
  end;
end;

procedure TConexao.SetID(const Value: string);
begin
  FID := Value;
end;

procedure TConexao.SetLatencia(const Value: string);
begin
  FLatencia := Value;
end;

procedure TConexao.SetSenha(const Value: string);
begin
  FSenha := Value;
end;

procedure TConexao.SetSenhaGerada(const Value: string);
begin
 FSenhaGerada := Value;
end;

procedure TConexao.SetThreadAreaRemota(const Value: TThreadConexaoAreaRemota);
begin
  FThreadAreaRemota := Value;
end;

procedure TConexao.SetThreadArquivos(const Value: TThreadConexaoArquivos);
begin
  FThreadArquivos := Value;
end;

procedure TConexao.SetThreadPrincipal(const Value: TThreadConexaoPrincipal);
begin
  FThreadPrincipal := Value;
end;

procedure TConexao.SetThreadTeclado(const Value: TThreadConexaoTeclado);
begin
  FThreadTeclado := Value;
end;

{ TConexoes }

procedure TConexoes.AdicionarConexao(AProtocolo,
                                     MAC, HD, DSenha : String);
Var
 I : Integer;
Begin
 FListaConexoes.Add(TConexao.Create);
 i := FListaConexoes.Count - 1;
 FListaConexoes[i].Protocolo := AProtocolo;
 FListaConexoes[i].MAC       := MAC;
 FListaConexoes[i].HD        := HD;
 FListaConexoes[i].ID        := GenerateID(MAC, HD);
 FListaConexoes[i].Senha     := GerarSenha(DSenha);
 FListaConexoes[i].SenhaGerada := GerarSenha('');
End;

//procedure TConexoes.AdicionarConexao(AProtocolo: string);
//Var
// I : Integer;
//Begin
// FListaConexoes.Add(TConexao.Create);
// i := FListaConexoes.Count - 1;
// FListaConexoes[i].Protocolo := AProtocolo;
// FListaConexoes[i].ID := GerarID;
// FListaConexoes[i].Senha := GerarSenha('');
//End;

constructor TConexoes.Create;
begin
  inherited;
  FListaConexoes := TObjectList<TConexao>.Create;
end;

destructor TConexoes.Destroy;
begin
  FreeAndNil(FListaConexoes);
end;

Function GenerateIDUnique(mac, hd : String) : String;
 Function LetToNum(Str : String) : String;
 Const
  Cad1: String = 'ABCDEF';
  Cad2: String = '123456';
 Var
  x, y : Integer;
 Begin
  Result := '';
  For y := 1 To Length(Str) Do
   Begin
    x := Pos(Str[y], Cad1);
    If x > 0 Then Result := Result + Copy(Cad2,x,1)
    Else Result := Result + Copy(str,y,1);
   End;
 End;
 Function RemoveChrInvalidos(Str: string): string;
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
 sID3   : String;
Begin
 AMac := RemoveChrInvalidos(mac);
 AHD := RemoveChrInvalidos(hd);
 S := LetToNum(AMac + AHD); // Trocando as letras pelos numeros;
 sID1 := Copy(s,StrToIntDef(Copy(s,1,1),1),3);
 sID2 := Copy(s,StrToIntDef(Copy(s,10,1),2),3);
 sID3 := Copy(s,StrToIntDef(Copy(s,length(s)-3,1),3),3);
 Result := sID1 + '-'+ sID2  +'-'+ sID3;
End;

Function TConexoes.GenerateID(strMAC, strHD : String) : String;
Var
 I       : Integer;
 ID      : String;
 Exists  : Boolean;
 Conexao : TConexao;
Begin
 Randomize;
 ID := GenerateIDUnique(strMAC, strHD);
 Exists := False;
 Try
  For Conexao in FListaConexoes do
   Begin
    Exists := (Conexao.ID = ID);
    If Exists Then
     Break;
   End;
 Finally
  Result := ID;
 End;
End;

function TConexoes.GerarID: string;
var
  xID: string;
  bExists: Boolean;
  Conexao: TConexao;
begin
  bExists := False;
  while True do
  begin
    Randomize;
    xID := IntToStr(Random(9)) + IntToStr(Random(9)) + IntToStr(Random(9)) + '-' + IntToStr(Random(9)) + IntToStr(Random(9)) + IntToStr(Random(9)) + '-' + IntToStr(Random(9)) + IntToStr(Random(9)) + IntToStr(Random(9));
    for Conexao in FListaConexoes do
    begin
      if Conexao.ID = xID then
      begin
        bExists := True;
        Break;
      end;
    end;
    if not(bExists) then
      Break;
  end;
  Result := xID;
end;

function TConexoes.GerarSenha(psw:string): string;
begin
 if psw = '' then
 begin
    Randomize;
    Result := IntToStr(Random(9)) + IntToStr(Random(9)) + IntToStr(Random(9)) + IntToStr(Random(9));
 end
 else
 result := psw;
end;

function TConexoes.MD5String(const texto: string): string;
var
  idmd5: TIdHashMessageDigest5;
begin
  idmd5 := TIdHashMessageDigest5.Create;
  try
    Result := LowerCase(idmd5.HashStringAsHex(texto));
  finally
    idmd5.Free;
  end;
end;

procedure TConexoes.RemoverConexao(AProtocolo: string);
var
  Conexao: TConexao;
begin
  if AProtocolo = '' then
    Exit;
  for Conexao in FListaConexoes do
  begin
    if Conexao.Protocolo = AProtocolo then
    begin
      FListaConexoes.Remove(Conexao);
      Break;
    end;
  end;
end;

function TConexoes.RetornaItemPorConexao(AConexao: string): TConexao;
var
  Conexao: TConexao;
begin
  Result := nil;
  if AConexao = '' then
    Exit;
  for Conexao in FListaConexoes do
  begin
    if Conexao.Protocolo = AConexao then
    begin
      Result := Conexao;
      Break;
    end;
  end;
end;

function TConexoes.RetornaItemPorHandle(AHandle: Integer): TConexao;
var
  Conexao: TConexao;
begin
  Result := nil;
  if AHandle = 0 then
    Exit;
  for Conexao in FListaConexoes do
  begin
    if Conexao.ThreadPrincipal.scClient.Handle = AHandle then
    begin
      Result := Conexao;
      Break;
    end;
  end;
end;

function TConexoes.RetornaItemPorID(AID: string): TConexao;
var
  Conexao: TConexao;
begin
  Result := nil;
  if AID = '' then
    Exit;
  for Conexao in FListaConexoes do
  begin
    if Conexao.ID = AID then
    begin
      Result := Conexao;
      Break;
    end;
  end;
end;

function TConexoes.VerificaID(AID: string): Boolean;
var
  Conexao: TConexao;
begin
  for Conexao in FListaConexoes do
  begin
    if Conexao.ID = AID then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TConexoes.VerificaIDSenha(AID, ASenha: string): Boolean;
var
  Conexao: TConexao;
begin
  Result := False;
  for Conexao in FListaConexoes do
  begin
    if (Conexao.ID = AID) and (Conexao.Senha = ASenha) or (Conexao.ID = AID) and (Conexao.SenhaGerada = ASenha) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

end.
