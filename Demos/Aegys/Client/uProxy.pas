unit uProxy;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls,
  Dialogs, ScktComp, StdCtrls, WinSock, RTLConsts, Registry;

const
  LF = #10;
  CR = #13;
  EOL = CR + LF;
  DefTimeout: Longint = 60000;
  DefMaxLineLength: integer = MaxWord;

function SockWaitForData(ASocket: TSocket; ATimeout: Longint): Boolean;
function LCharPos(Str: PChar; char: char; Size: integer): integer;
function CharPos(const Str: string; char: char): integer;
function Fetch(var AInput: string; const ADelim: char): string;
function SockReadLn(ASocket: TSocket; ATerminator: char = LF;
  AMaxLineLength: integer = MaxWord; ATimeout: Longint = 0): string;
procedure WriteBuffer(ASocket: TSocket; const Buffer; Count: Longint;
  ATimeout: Longint = 0);
procedure SockWriteLn(ASocket: TSocket; const AOut: string = '');
// function BasicAuthentication(const Username,Password:string): String;
function GetProxy(var Host: string; var Port: integer;
  var ProxyEnabled: Boolean): Boolean;

implementation

function SockWaitForData(ASocket: TSocket; ATimeout: Longint): Boolean;
var
  FDSet: TFDSet;
  TimeVal: TTimeVal;
begin
  if ATimeout = -1 then
    ATimeout := DefTimeout;
  TimeVal.tv_sec := ATimeout div 1000;
  TimeVal.tv_usec := (ATimeout mod 1000) * 1000;
  FD_ZERO(FDSet);
  FD_SET(ASocket, FDSet);
  Result := select(0, @FDSet, nil, nil, @TimeVal) > 0;
end;

function LCharPos(Str: PChar; char: char; Size: integer): integer;
asm  { Str - EAX, char - DL (EDX), Size - ECX }
  PUSH    EDI
  MOV     EDI, EAX { Point EDI to str }

  MOV     AL,DL        { AL = Search char }
  MOV     EDX,ECX   { remember Length }

  REPNE   SCASB
  JNE     @@fail
  MOV     EAX,EDX { Calc offset }
  SUB     EAX,ECX
  JMP     @@fin
@@fail:
  XOR     EAX,EAX
@@fin:
  POP     EDI
end;

function CharPos(const Str: string; char: char): integer;
asm
  TEST    EAX, EAX
  JE     @@exit
  MOV     ECX, [EAX-4]
  call    LCharPos
@@exit:
end;

function Fetch(var AInput: string; const ADelim: char): string;
var
  LPos: integer;
begin
  LPos := CharPos(AInput, ADelim);
  if LPos = 0 then
  begin
    Result := AInput;
    AInput := '';
  end
  else
  begin
    Result := Copy(AInput, 1, LPos - 1);
    AInput := Copy(AInput, LPos + 1, MaxInt);
  end;
end;

function SockReadLn(ASocket: TSocket; ATerminator: char = LF;
  AMaxLineLength: integer = MaxWord; ATimeout: Longint = 0): string;
var
  Buf: array [byte] of char;
  SizeBuf: integer;
  r, i: integer;
  s: string;
begin
  if ATimeout = 0 then
    ATimeout := DefTimeout;
  SizeBuf := SizeOf(Buf);
  Result := '';
  while SockWaitForData(ASocket, ATimeout) do
  begin
    r := recv(ASocket, Buf, SizeBuf, MSG_PEEK);
    if r = 0 then
      Break;
    if r = SOCKET_ERROR then
      Raise Exception.Create('recv');

    i := LCharPos(@Buf, ATerminator, r);
    if i <> 0 then
      r := i;

    r := recv(ASocket, Buf, r, 0);
    if r = 0 then
      Break;
    if r = SOCKET_ERROR then
      Raise Exception.Create('recv');

    SetString(s, Buf, r);
    Result := Result + s;
    if i <> 0 then
      Break;
  end;
  if length(Result) = 0 then
    exit;
  i := length(Result) - 1;
  if (ATerminator = LF) and (Result[i] = CR) then
    Dec(i);
  SetLength(Result, i);
end;

procedure WriteBuffer(ASocket: TSocket; const Buffer; Count: Longint;
  ATimeout: Longint = 0);
var
  Overlapped: TOverlapped;
  ErrorCode, Result: integer;
begin
  if ATimeout = 0 then
    ATimeout := DefTimeout;
  FillChar(Overlapped, SizeOf(Overlapped), 0);
  Overlapped.hEvent := CreateEvent(nil, True, False, '');
  try
    if not WriteFile(ASocket, Buffer, Count, DWORD(Result), @Overlapped) and
      (GetLastError <> ERROR_IO_PENDING) then
    begin
      ErrorCode := GetLastError;
      raise ESocketError.CreateResFmt(@sSocketIOError,
        [sSocketWrite, ErrorCode, SysErrorMessage(ErrorCode)]);
    end;
    if WaitForSingleObject(Overlapped.hEvent, ATimeout) <> WAIT_OBJECT_0 then
      Result := 0
    else
      GetOverlappedResult(ASocket, Overlapped, DWORD(Result), False);
  finally
    CloseHandle(Overlapped.hEvent);
  end;
end;

procedure SockWriteLn(ASocket: TSocket; const AOut: string = '');
begin
  if length(AOut) > 0 then
    WriteBuffer(ASocket, Pointer(AOut)^, length(AOut));
  WriteBuffer(ASocket, EOL, length(EOL));
end;

(*
  function BasicAuthentication(const Username,Password:string): String;
  begin
  with TIdEncoderMIME.Create do
  try
  Result := 'Basic ' + Encode(Username + ':' + Password); {do not localize}
  finally
  Free;
  end;
  end;
*)

function GetProxy(var Host: string; var Port: integer;
  var ProxyEnabled: Boolean): Boolean;
const
  sProxyEnable = 'ProxyEnable';
var
  s: string;
  p: integer;
  sPortTmp: String;
begin
  with TRegistry.Create do
  begin
    RootKey := HKEY_CURRENT_USER;
    ProxyEnabled := False;
    s := '';
    OpenKey('\Software\Microsoft\Windows\CurrentVersion\Internet Settings',
      True);
    if ValueExists('ProxyServer') then
      s := ReadString('ProxyServer');

    if s <> '' then
    begin
      p := pos(':', s);
      if p = 0 then
        p := length(s) + 1;
      Host := Copy(s, 1, p - 1);
      try
        Port := StrToInt(Copy(s, p + 1, 999));
      except
        Port := 80;
      end;

      ProxyEnabled := True;
    end;

    if ValueExists('ProxyEnable') then
    begin
      case GetDataType(sProxyEnable) of
        rdString, rdExpandString:
          begin
            sPortTmp := AnsiLowerCase(ReadString(sProxyEnable));
            ProxyEnabled := True;
            if pos(' ' + sPortTmp + ' ', ' yes true t enabled 1 ') > 0 then
              ProxyEnabled := True
            else if pos(' ' + sPortTmp + ' ', ' no false f none disabled 0 ') > 0
            then
              ProxyEnabled := False
          end;
        rdInteger:
          begin
            ProxyEnabled := ReadBool(sProxyEnable);
          end;
        rdBinary:
          begin
            ProxyEnabled := True;
            ReadBinaryData(sProxyEnable, ProxyEnabled, 1);
          end;
      end;
    end;

    Free;
  end;

  Result := s <> '';
end;

end.
