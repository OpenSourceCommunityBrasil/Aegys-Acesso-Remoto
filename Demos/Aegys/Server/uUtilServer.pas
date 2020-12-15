unit uUtilServer;

interface
  uses IniFiles, SysUtils, Forms, Windows;

  //Marcones Freitas - 16/10/2015 -> Algumas Constantes Novas
 const
 cGeneral            = 'General';
 cHost               = 'Host';
 cPort               = 'Port';
 cGroup              = 'Group';
 cMachine            = 'Machine';
 cConnectTimeOut     = 'ConnectTimeOut';
 cStarterWithWindows = 'StarterWithWindows';
 cYes                = 'YES';
 cNO                 = 'NO';
 cLanguage           = 'Language';

 procedure SaveIni(Param, Value, ArqFile, Name: String; encrypted: Boolean);
 function GetIni(Path, Key, KeyValue : string; encrypted: Boolean): string;
 function EnDecryptString(StrValue : String; Key: Word) : String;
 function ActiveProcess(AValue: String = ''): Boolean;
 function GetPort : Integer;
 function GenerateIDUnique(mac, hd: string): string;

var
  Port : Integer;
  Group, Machine, MAC, HD, LastPassword: string;

 implementation

procedure SaveIni(Param, Value, ArqFile, Name: String; encrypted: Boolean);
var ArqIni : TIniFile;
begin
  ArqIni := TIniFile.Create(ArqFile);
  IF encrypted THEN
     Value := EnDecryptString(Value,250);

  ArqIni.WriteString(Name, Param, Value);
  ArqIni.Free;
end;

function GetIni(Path, Key, KeyValue : string; encrypted: Boolean): string;
var ArqIni : TIniFile;
    ValueINI : string;
begin
  ArqIni := TIniFile.Create(Path);

  ValueINI := ArqIni.ReadString(Key, KeyValue, ValueINI);
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

function GetPort : Integer;
begin
{
  if GetIni(ExtractFilePath(Application.ExeName) + Application.Title+'.ini', cGeneral, cPort, True) = '0' then
     begin
       frm_Config := Tfrm_Config.Create(Application);
       frm_Config.ShowModal;
       FreeAndNil(frm_Config);
     end;
  Result := StrToInt(GetIni(ExtractFilePath(Application.ExeName) + Application.Title+'.ini', cGeneral, cPort, True));
}
end;

function GenerateIDUnique(mac, hd: string): string;
function LetToNum(Str: String): String;
const
  Cad1: String = 'ABCDEF';
  Cad2: String = '123456';
var
  x, y: integer;
begin
  Result := '';
  for y := 1 to Length(Str) do
  begin
    x := Pos(Str[y], Cad1);
    if x > 0 then
    Result := Result + Copy(Cad2,x,1)
    else
    Result := Result + Copy(str,y,1);
  end;
end;
  function RemoveChrInvalidos(Str: string): string;
  var
    x: integer;
    ret: string;
  begin
    ret := '';
    for x := 1 to Length(Str) do
    begin
      if (Str[x] <> '-') and (Str[x] <> '.') and (Str[x] <> ',') and (Str[x] <> '/') then
        ret := ret + Str[x];
    end;
    RemoveChrInvalidos := Trim(TrimRight(ret));
  end;

var
  AMac, AHD, S: string;
  sID1, sID2, sID3:string;
begin
  AMac := RemoveChrInvalidos(mac);
  AHD := RemoveChrInvalidos(hd);

  S := LetToNum(AMac + AHD); // Trocando as letras pelos numeros;

  sID1 := Copy(s,StrToIntDef(Copy(s,1,1),1),2);
  sID2 := Copy(s,StrToIntDef(Copy(s,10,1),2),3);
  sID3 := Copy(s,StrToIntDef(Copy(s,length(s)-3,1),3),3);
  Result := sID1 + '-'+ sID2  +'-'+ sID3;

end;

end.

