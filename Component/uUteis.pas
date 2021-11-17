unit uUteis;

interface
  uses System.IniFiles, System.SysUtils, Vcl.Forms, Winapi.Windows, ULanguage, System.Win.ScktComp, uProxy, IdTCPClient;

  //Marcones Freitas - 16/10/2015 -> Algumas Constantes Novas
 const
 cGeneral            = 'General';
 cHost               = '201.73.143.69';//'144.217.189.109';
 cPort               = '0';
 cGroup              = 'Group';
 cMachine            = 'Machine';
 cConnectTimeOut     = '100000';
 cStarterWithWindows = '0';
 cYes                = 'YES';
 cNO                 = 'NO';
 cLanguage           = '0';
 cProxy              = '0';
 cHostProxy          = '';
 cPortProxy          = '0';
 cVersion            = '001';
 cUrlUpdates         = 'UrlUpdates';

 procedure SaveIni(Param, Value, ArqFile, Name: String; encrypted: Boolean);
 function GetIni(Path, Section, Key, KeyValue : string; encrypted: Boolean): string;
 function EnDecryptString(StrValue : String; Key: Word) : String;
 function ActiveProcess(AValue: String = ''): Boolean;
 procedure ReadCaptions(language : Integer);
 procedure SetHostPortGroupMach;
 procedure SetLanguage;
 procedure HideApplication;
 procedure ShowApplication;
 procedure CloseAplication;

 // Add 05/11/2015 - Solivan Araujo
  Function SerialNumHardDisk(FDrive: String): String;
  Function SystemDrive: string;
  Function MacAddress: string;
  Function GetComputerNameFunc : String;

var
 xLanguage : Integer;
 Languages : TLanguage;
 Host, vGroup, vMachine, vMAC, vHD : string;
 Port, ConnectionTimeout : Integer;
 vParID, vParSenha: string;
 FirstExecute : Boolean;
 { |INICIO| Cleiton 20/10/2015 }
 Proxy: Boolean;
 HostProxy: String;
 PortProxy: integer;

implementation

uses Form_Main;

Function GetComputerNameFunc : String;
Var
 lpBuffer : PChar;
 nSize : DWord;
const
 Buff_Size = MAX_COMPUTERNAME_LENGTH + 1;
Begin
 nSize := Buff_Size;
 lpBuffer := StrAlloc(Buff_Size);
 GetComputerName(lpBuffer,nSize);
 Result := String(lpBuffer);
 StrDispose(lpBuffer);
End;

procedure SaveIni(Param, Value, ArqFile, Name: String; encrypted: Boolean);
Var
 ArqIni : TIniFile;
begin
  ArqIni := TIniFile.Create(ArqFile);
  IF encrypted THEN
     Value := EnDecryptString(Value,250);

  ArqIni.WriteString(Name, Param, Value);
  ArqIni.Free;
end;

function GetIni(Path, Section, Key, KeyValue : string; encrypted: Boolean): string;
var ArqIni : TIniFile;
    ValueINI : string;
begin
  ArqIni := TIniFile.Create(Path);
  ValueINI := ArqIni.ReadString(Section, Key, KeyValue);
  if ValueINI = '' then
     ValueINI := '0'
  else
  IF encrypted THEN
     ValueINI := EnDecryptString(ValueINI,250);

  Result := ValueINI;
  ArqIni.Free;
end;


function EnDecryptString(StrValue : String; Key: Word) : String;
var I: Integer; OutValue : String;
begin
  OutValue := '';
  for I := 1 to Length(StrValue) do
      OutValue := OutValue + char(Not(ord(StrValue[I])-Key));

  Result := OutValue;
end;


function ActiveProcess(AValue: String = ''): Boolean;
begin
  if AnsiSameStr(AValue, EmptyStr) then
     AValue := ExtractFileName(Application.ExeName);

  CreateSemaphore(nil, 1, 1, PChar(AValue));
  Result := (GetLastError = ERROR_ALREADY_EXISTS);
end;

procedure ReadCaptions(language : Integer);
Var
  IniFile : string;
begin
//  IniFile := gsAppPath + 'Language';
  Languages.Free;
  Languages := TLanguage.Create();
{
  if(language = 0)then
   begin
     Languages.YourID_Label       := String(GetIni(IniFile +'\US.ini','CAPTIONS','YourID_Label',false));
     Languages.YourPassword_Label := String(GetIni(IniFile +'\US.ini','CAPTIONS','YourPassword_Label',false));
     Languages.TargetID_Label     := String(GetIni(IniFile +'\US.ini','CAPTIONS','TargetID_Label',false));
     Languages.Language_Label     := String(GetIni(IniFile +'\US.ini','CAPTIONS','Language_Label',false));
   end
  else
  if(language = 1)then
     begin
      Languages.YourID_Label       := String(GetIni(IniFile +'\Pt_Br.ini','CAPTIONS','YourID_Label',false));
      Languages.YourPassword_Label := String(GetIni(IniFile +'\Pt_Br.ini','CAPTIONS','YourPassword_Label',false));
      Languages.TargetID_Label     := String(GetIni(IniFile +'\Pt_Br.ini','CAPTIONS','TargetID_Label',false));
      Languages.Language_Label     := String(GetIni(IniFile +'\Pt_Br.ini','CAPTIONS','Language_Label',false));
     end
     else
     begin
      Languages.YourID_Label       := 'Sua ID';
      Languages.YourPassword_Label := 'Senha';
      Languages.TargetID_Label     := 'ID do Parceiro';
      Languages.Language_Label     := 'Português';
     end;
}
end;

procedure SetHostPortGroupMach;
Var
 s    : String;
 Code,
 I    : integer;
begin
 If Proxy Then
  Begin
   frm_Main.ipPSMain_Socket.Host    := HostProxy;
   frm_Main.ipPSMain_Socket.Port    := PortProxy;
   frm_Main.ipPSDeskTopClient.Host  := HostProxy;
   frm_Main.ipPSDeskTopClient.Port  := PortProxy;
   frm_Main.ipCommandsClient.Host   := HostProxy;
   frm_Main.ipCommandsClient.Port   := PortProxy;
   frm_Main.ipPSFilesClient.Host    := HostProxy;
   frm_Main.ipPSFilesClient.Port    := PortProxy;
  End
 Else
  Begin
   frm_Main.ipPSMain_Socket.Host      := Host;
   frm_Main.ipPSMain_Socket.Port      := 8078;
   frm_Main.ipCommandsClient.Host     := Host;
   frm_Main.ipCommandsClient.Port     := 8079;
   frm_Main.ipCommandsClient.TCPPort  := 8084;
   frm_Main.ipPSDeskTopClient.Host    := Host;
   frm_Main.ipPSDeskTopClient.Port    := 8082;
   frm_Main.ipPSDeskTopClient.TCPPort := 8083;
   frm_Main.ipPSFilesClient.Host      := Host;
   frm_Main.ipPSFilesClient.Port      := 8081;
   frm_Main.ipPSFilesClient.TCPPort   := 8085;
  End;
End;

procedure SetLanguage;
begin
 xLanguage := strtoint(GetIni( ExtractFilePath(Application.ExeName) + Application.Title + '.ini', cGeneral, 'language', '0', False));
 ReadCaptions(xLanguage);
{
  frm_Main.YourID_Label.Caption       := Languages.YourID_Label;
  frm_Main.YourPassword_Label.Caption := Languages.YourPassword_Label;
  frm_Main.TargetID_Label.Caption     := Languages.TargetID_Label;
}
end;

procedure HideApplication;
begin
 If frm_Main <> Nil Then
  Begin
   PostMessage(frm_Main.Handle,    $0112, SC_MINIMIZE,  0);//Self.Show;
   PostMessage(Application.Handle, $0112, SC_MINIMIZE,  0);
  End;
end;

procedure ShowApplication;
begin
 If frm_Main <> Nil Then
  Begin
   PostMessage(Application.Handle, $0112, SC_RESTORE,  0);
   If frm_Main <> Nil Then
    PostMessage(frm_Main.Handle,    $0112, SC_RESTORE,  0);
  End;
end;

procedure CloseAplication;
begin
 Application.ProcessMessages;
 if Application.MessageBox(PChar('Confirm close the Application?'), PChar(frm_Main.Caption), mb_YesNo + mb_DefButton2 + mb_IconQuestion) = IdYes then
    Halt;
end;

Function MacAddress: string;
var
  Lib: Cardinal;
  Func: function(GUID: PGUID): longint; stdcall;
  GUID1, GUID2: TGUID;

begin
  Result := '';
  Lib := LoadLibrary('rpcrt4.dll');
  if Lib <> 0 then
  begin
    @Func := GetProcAddress(Lib, 'UuidCreateSequential');
    if assigned(Func) then
    begin
      if (Func(@GUID1) = 0) and (Func(@GUID2) = 0) and (GUID1.D4[2] = GUID2.D4[2]) and (GUID1.D4[3] = GUID2.D4[3]) and (GUID1.D4[4] = GUID2.D4[4]) and (GUID1.D4[5] = GUID2.D4[5])
        and (GUID1.D4[6] = GUID2.D4[6]) and (GUID1.D4[7] = GUID2.D4[7]) then
      begin
        Result := IntToHex(GUID1.D4[2], 2) + '-' + IntToHex(GUID1.D4[3], 2) + '-' + IntToHex(GUID1.D4[4], 2) + '-' + IntToHex(GUID1.D4[5], 2) + '-' + IntToHex(GUID1.D4[6], 2) + '-'
          + IntToHex(GUID1.D4[7], 2);
      end;
    end;
  end;
end;

function SystemDrive: string;
var
  DirWin, SystemDriv: string;
begin
  SetLength(DirWin, MAX_PATH);
  GetSystemDirectory(PChar(DirWin), MAX_PATH);
  SystemDriv := Copy(DirWin, 1, 3);
  Result := SystemDriv

end;

Function SerialNumHardDisk(FDrive: String): String;
Var
  Serial: DWORD;
  DirLen, Flags: DWORD;
  DLabel: Array [0 .. 11] of Char;
begin
  Try
    GetVolumeInformation(PChar(Copy(FDrive, 1, 1) + ':\'), DLabel, 12, @Serial, DirLen, Flags, nil, 0);
    Result := IntToHex(Serial, 8);
  Except
    Result := '';
  end;
end;

Initialization
Finalization
 FreeAndNil(Languages);
end.
