unit uAegysUtils;

interface

uses Windows;

{$I platform.inc}

Const
  Pi2 = Pi * 2;
  PiD2 = Pi/2;
  SimpleQuad : array [0..3] of TPoint =
    ((x: 0; y: 0), (x: 1; y: 0), (x: 1; y: 1), (x: 0; y: 1));
  MaxCPUCores = 16; // enough for nearest future?
Type
  PFloatType = ^TFloatType;
  TFloatType = Single;
//  TFloatType = Double;

  PSinCos = ^TSinCos;
  TSinCos = record
    Case Integer of
      0: (iSin, iCos : Integer);
      1: (rSin, rCos : TFloatType);
  end;

  TSinCosTable = array of TSinCos;

  PMinMax = ^TMinMax;
  TMinMax = record
    iMin, iMax: Integer;
  end;

  PRealPoint = ^TRealPoint;
  TRealPoint = record
    x, y : TFloatType;
  end;

  PByteArr = ^TByteArr;
  TByteArr = array [0..MAXINT-1] of Byte;
  DByteArr = array of Byte;

  DWordArr = array of Word;

  PIntArr = ^TIntArr;
  TIntArr = array [0..(MAXINT div SizeOf(Integer))-1] of Integer;
  DIntArr = array of Integer;
  DSingleArr = array of Single;

  PPointArr = ^TPointArr;
  TPointArr = array [0..(MAXINT div SizeOf(TPoint))-1] of TPoint;
  DPointArr = array of TPoint;

  PRPointArr = ^TRPointArr;
  TRPointArr = array [0..(MAXINT div SizeOf(TRealPoint))-1] of TRealPoint;

Var
  PCFreq : Int64;
  FPSMeasIntMks : Integer = 1000000;
  PCEnabled : Boolean = False;

function Num_Min( A, B: Integer ): Integer; overload;
function Num_Min(A,B: TFloatType): TFloatType; overload;
function Num_Max( A, B: Integer ): Integer; overload;
function Num_Max(A,B: TFloatType): TFloatType; overload;
procedure Num_Swap(Var a,b);
procedure Num_Swap64(a,b : PInt64);
function Num_Ceil(X: TFloatType): Integer;

function Num_InRange(Var Value : Integer; rMin, rMax : Integer): Boolean; overload;
function Num_InRange(Var Value : TFloatType; rMin, rMax : TFloatType): Boolean; overload;
procedure Num_CycleRange(Var Value : Integer; rMin, rMax : Integer);
function Num_IncOver(Var Value : Integer; IncVal, Min, Max : Integer): Boolean;
function Num_Binary(num : Integer; Smaller : Boolean = False) : Integer;
function Num_2Str( Value : Integer ) : String;

function Point(AX, AY: Integer): TPoint;
function RealPoint(Const p : TPoint; k : TFloatType = 1): TRealPoint;
procedure Points_Extreme(Const p : TPoint; Min, Max : PPoint;
                         ForceMinMax : Boolean = False);
function Point_InRect(Var p : TPoint; Const r : TRect): Boolean;

function Rect(ALeft, ATop, ARight, ABottom: Integer): TRect;
function Rect_Clip(Var dRect, sRect : TRect; Const vpRect : TRect): Boolean;

function Math_Ceil(Const x: Double): Integer;
procedure Math_SinCos(const theta, radius : Single; var rSin, rCos: TFloatType);
procedure Math_GetSinCosTable(TablePtr : PSinCos; Count : Integer;
                              MulValue : Single; StartAngle : Single = 0;
                              Copy2End : Integer = 0; AsReals : Boolean = False);
function Math_IntPower(Base : Integer; Exponent : Integer): Integer; overload;
function Math_IntPower(Const Base : TFloatType; Exponent : Integer): TFloatType; overload;

function Mem_Equal(P1, P2: pointer; Length: Integer): Boolean;
procedure Mem_Swap(a,b : PByte; Size : Integer);
function Arr_Aligned(Var Arr : DByteArr; ByteSize : Integer;
                     AShift : Integer = 3): Pointer;
function File_ExtractExt(const FileName: string): string;
function File_Exists(const FileName: string; CheckDirectory : Boolean = False): Boolean;

function CreateWnd(Const Hdr : String; Flags : DWord; Width, Height : Integer;
                   WndProc : Pointer; XPos : Integer = 0; YPos : Integer = 0): THandle;

procedure Msg2File(Const FName, Msg : String; Add2End : Boolean = True);
procedure SetPriority(High : Boolean; RealtimeND : Boolean = True);
procedure ZeroDXS(struct : PDWord; Size : DWord);


procedure TimeReset(Idx : Integer = 0; AvgCycles : Integer = 0);
function TimeMs: Integer;
function TimeMks(Idx : Integer = 0; FPS : PInteger = nil): Integer;
 // max. measured interval 2147 s = 35 min

function TimeRealMks: Double;
function TimeStrMs: String;
function TimeStrMks: String;
procedure TimeShowMks;

implementation
Const MaxTmBufSize = 32;
      MaxCounters = 8;
      DynFreqUpdate = True;
//      DynFreqUpdate = False;
Type
  TTiming = record
    FPSTimeSum, FPSFrameCnt : array[0..MaxCounters-1] of  Integer;
    Buffer : array [0..MaxTmBufSize-1] of Integer;
    BufSize, BufCnt, BufIdx : Integer;
  end;

Var
  PCTime1, PCTime2, PCFreqA : array[0..MaxCounters-1] of Int64;
  Time : TTiming;

procedure Msg2File(Const FName, Msg : String; Add2End : Boolean = True);
Var f : TextFile;
begin
AssignFile(f,FName);
{$I-}
If ( Add2End ) then begin
  Append(f);
  If (IOResult <> 0) then Add2End := False;
end;
If ( not Add2End ) then ReWrite(f);
{$I+}
If IOResult=0 then WriteLn(f,Msg);
CloseFile(f);
end;

procedure SetPriority(High : Boolean; RealtimeND : Boolean = True);
Var tp, pp : Integer;
begin
If High then
  If (RealtimeND) and (DebugHook = 0) then begin
    pp := REALTIME_PRIORITY_CLASS; tp := THREAD_PRIORITY_TIME_CRITICAL;
  end else begin
    //very high priority is dangerous for debugging...
    pp := HIGH_PRIORITY_CLASS; tp := THREAD_PRIORITY_ABOVE_NORMAL;
  end
else begin
  pp := NORMAL_PRIORITY_CLASS; tp := THREAD_PRIORITY_NORMAL;
end;
SetPriorityClass(GetCurrentProcess, pp);
SetThreadPriority(GetCurrentThread, tp);
end;

procedure ZeroDXS(struct : PDWord; Size : DWord);
begin
ZeroMemory(struct, Size); struct^:=Size;
end;

function Num_Min( A, B: Integer ): Integer;
begin
If A < B then Result := A else Result := B;
end;

function Num_Min(A,B: TFloatType): TFloatType;
begin
If A < B then Result := A else Result := B;
end;

function Num_Max( A, B: Integer ): Integer;
begin
If A > B then Result := A else Result := B;
end;

function Num_Max(A,B: TFloatType): TFloatType;
begin
If A > B then Result := A else Result := B;
end;

procedure Num_Swap(Var a,b);
Var i : Integer;
begin
i := Integer(a); Integer(a) := Integer(b); Integer(b) := i;
end;

procedure Num_Swap64(a,b : PInt64);
Var i : Int64;
begin
i := a^; a^ := b^; b^ := i;
end;

function Num_Ceil(X: TFloatType): Integer;
begin
Result := Integer(Trunc(X));
If Frac(X) > 0 then Inc(Result);
end;

function Num_InRange(Var Value : Integer; rMin, rMax : Integer): Boolean;
begin
Result:=True;
If Value<rMin then begin Result:=False; Value:=rMin end else
  If Value>rMax then begin Result:=False; Value:=rMax; end;
end;

function Num_InRange(Var Value : TFloatType; rMin, rMax : TFloatType): Boolean;
begin
Result:=True;
If Value<rMin then begin Result:=False; Value:=rMin end else
  If Value>rMax then begin Result:=False; Value:=rMax; end;
end;

procedure Num_CycleRange(Var Value : Integer; rMin, rMax : Integer);
Var n : Integer;
begin
n:=rMax-rMin+1;
If Value<rMin then Inc(Value, n) else If Value>rMax then Dec(Value, n);
end;

function Num_IncOver(Var Value : Integer; IncVal, Min, Max : Integer): Boolean;
begin
Inc(Value, IncVal);
If (Value > Max) then begin
  Result := True; Value := Min;
end else
  If (Value < Min) then begin
    Result := True; Value := Max;
  end else
    Result := False;
end;

function Num_Binary(num : Integer; Smaller : Boolean = False) : Integer;
begin
Result := 1;
While (Result < num) do Result := Result shl 1;
If (Smaller) and (Result > num) and (Result > 1) then Result := Result shr 1;
end;

function Num_2Str( Value : Integer ) : String;
begin
Str(Value, Result);
end;

function Point(AX, AY: Integer): TPoint;
begin
Result.x := AX; Result.y := AY;
end;

function RealPoint(Const p : TPoint; k : TFloatType = 1): TRealPoint;
begin
Result.x := p.x*k; Result.y := p.y*k;
end;

procedure Points_Extreme(Const p : TPoint; Min, Max : PPoint;
                         ForceMinMax : Boolean = False);
begin
If Min<>nil then
  If (ForceMinMax) then Min^ := p else begin
    If (p.x<Min.x) then Min.x:=p.x;
    If (p.y<Min.y) then Min.y:=p.y;
  end;
If Max<>nil then
  If (ForceMinMax) then Max^ := p else begin
    If (p.x>Max.x) then Max.x:=p.x;
    If (p.y>Max.y) then Max.y:=p.y;
  end;
end;

function Point_InRect(Var p : TPoint; Const r : TRect): Boolean;
begin
Result:=True;
If (p.x<r.Left)   then begin p.x:=r.Left;   Result:=False; end;
If (p.y<r.Top)    then begin p.y:=r.Top;    Result:=False; end;
If (p.x>r.Right)  then begin p.x:=r.Right;  Result:=False; end;
If (p.y>r.Bottom) then begin p.y:=r.Bottom; Result:=False; end;
end;

function Rect(ALeft, ATop, ARight, ABottom: Integer): TRect;
begin
with Result do begin
  Left := ALeft;
  Top := ATop;
  Right := ARight;
  Bottom := ABottom;
end;
end;

function Rect_Clip(Var dRect, sRect : TRect; Const vpRect : TRect): Boolean;
Var r : TRect;
begin
Result:=False;
IntersectRect(r,dRect,vpRect);
If IsRectEmpty(r) then Exit;
If not EqualRect(r,dRect) then With sRect do begin
  Dec(Left,   dRect.Left   - r.Left);
  Dec(Top,    dRect.Top    - r.Top);
  Dec(Right,  dRect.Right  - r.Right);
  Dec(Bottom, dRect.Bottom - r.Bottom);
  dRect:=r;
end;
Result:=True;
end;

function Math_Ceil(Const x: Double): Integer;
begin
  Result:=Trunc(x);
  if Frac(x)>0 then Inc(Result);
end;

procedure Math_SinCos(const theta, radius : Single; var rSin, rCos: TFloatType);
begin
rSin := Sin(Theta) * radius; rCos := Cos(Theta) * radius;
end;

procedure Math_GetSinCosTable(TablePtr : PSinCos; Count : Integer;
                              MulValue : Single; StartAngle : Single = 0;
                              Copy2End : Integer = 0; AsReals : Boolean = False);
Var n : Integer;
    a,ast : Single;
    pt : PSinCos;
    SinT, CosT: Single;
begin
pt:=TablePtr; a:=StartAngle; ast:=Pi2/Count;

If AsReals then
  For n:=0 to Count-1 do begin
    Math_SinCos(a, MulValue, TablePtr.rSin, TablePtr.rCos);
    Inc(TablePtr); a:=a+ast;
  end
else
  For n:=0 to Count-1 do begin
    Math_SinCos(a, MulValue, SinT, CosT);
    TablePtr.iCos:=Round(CosT);
    TablePtr.iSin:=Round(SinT);
    Inc(TablePtr);
    a:=a+ast;
  end;
If Copy2End>0 then Move(pt^, TablePtr^, Copy2End * SizeOf(TSinCos));
end;

function Math_IntPower(Base : Integer; Exponent : Integer): Integer;
begin
If (Base = 0) then begin Result := 0; Exit; end;
Result := 1;
While Exponent > 0 do begin
  Result := Result * Base; Dec(Exponent);
end;
end;

function Math_IntPower(Const Base : TFloatType; Exponent : Integer): TFloatType; 
begin
If (Base = 0) then begin Result := 0; Exit; end;
Result := 1;
While Exponent > 0 do begin
  Result := Result * Base; Dec(Exponent);
end;
end;


function Mem_Equal(P1, P2: pointer; Length: Integer): Boolean;
begin
Result := False;
While (Length >= 8) do begin
  If (PInt64(p1)^ <> PInt64(p2)^) then Exit;
  Inc(PInt64(p1)); Inc(PInt64(p2)); Dec(Length, 8);
end;
While (Length > 0) do begin
  If (PByte(p1)^ <> PByte(p2)^) then Exit;
  Inc(PByte(p1)); Inc(PByte(p2)); Dec(Length);
end;
Result := True;
end;

procedure Mem_Swap(a,b : PByte; Size : Integer);
Var i : Integer;
    bt : Byte;
begin
While (Size >= 4) do begin
  i := PInteger(a)^; PInteger(a)^ := PInteger(b)^; PInteger(b)^ := i;
  Inc(a,4); Inc(b,4); Dec(Size,4);
end;
While (Size > 0) do begin
  bt := a^; a^ := b^; b^ := bt;
  Inc(a); Inc(b); Dec(Size);
end;
end;

function Arr_Aligned(Var Arr : DByteArr; ByteSize : Integer;
                     AShift : Integer = 3): Pointer;
Var i, Offs : NativeInt;
    AVal : Integer;
begin
AVal := (1 shl AShift);
//Mem_GrowArray(Arr, ByteSize, 1);
SetLength(Arr, ByteSize + AVal);
i := NativeInt(Arr);
While ( ((i shr AShift) shl AShift) <> i ) do
  Inc(i, 4);
Result := Pointer(i);
//Offs := i and (AVal-1);
//Result := Pointer(i + Offs);
end;

function File_ExtractExt(const FileName: string): string;
Var n, m : Integer;
    c : Char;
begin
n := 0;
For m := Length(FileName) downto 1 do begin
  c := FileName[m];
  If (c = '.') then begin n := m; Break; end
    else If (c = '\') then Break;
end;
If (n > 0) then Result := Copy(FileName, n+1, Length(Filename) - n )
           else Result := '';
end;

function File_Exists(const FileName: string; CheckDirectory : Boolean = False): Boolean;
Var Code: DWord;
begin
Code := GetFileAttributes( PChar(FileName) );
If (Code <> $FFFFFFFF) then begin
  Code := Code and FILE_ATTRIBUTE_DIRECTORY;
  Result := not ( (CheckDirectory) xor (Code <> 0) );
end else
  Result := False;
end;


function CreateWnd(Const Hdr : String; Flags : DWord; Width, Height : Integer;
                   WndProc : Pointer; XPos : Integer = 0; YPos : Integer = 0): THandle;
begin
Result := CreateWindow('#32770', PChar(Hdr), Flags, XPos, YPos,
    Width + (GetSystemMetrics(SM_CXFRAME)*2),
    Height + (GetSystemMetrics(SM_CYFRAME)*2) + GetSystemMetrics(SM_CYCAPTION),
    0, 0, hInstance, nil);
SetWindowLong(Result, GWL_WNDPROC, NativeInt(WndProc));
end;

///////////////////////////////// Timer ////////////////////////////////////////

procedure TimeReset(Idx : Integer = 0; AvgCycles : Integer = 0);
begin
If (Idx >= MaxCounters) then Exit;
QueryPerformanceCounter(PCTime1[Idx]);
If (Idx = 0) then begin
  If AvgCycles > MaxTmBufSize then AvgCycles := MaxTmBufSize;
  Time.BufSize := AvgCycles; Time.BufCnt := 0; Time.BufIdx := 0;
end;
end;

function TimeMs: Integer;
begin
If PCEnabled then begin
  QueryPerformanceCounter(PCTime2[0]);
  Result:=( (PCTime2[0]-PCTime1[0]) * 1000 ) div PCFreq;
  PCTime1[0]:=PCTime2[0];
end else Result:=0;
end;


function TimeMks(Idx : Integer = 0; FPS : PInteger = nil): Integer;
Var n : Integer;
begin
If (not PCEnabled) or (Idx >= MaxCounters) then begin Result := 0; Exit; end;

QueryPerformanceCounter(PCTime2[Idx]);
Result := ( (PCTime2[Idx] - PCTime1[Idx]) * 1000000 ) div PCFreqA[Idx];
PCTime1[Idx] := PCTime2[Idx];

If (DynFreqUpdate) or (FPS <> nil) then
  With Time do begin
    Inc(FPSTimeSum[Idx], Result); Inc(FPSFrameCnt[Idx]);
    If (FPSTimeSum[Idx] >= FPSMeasIntMks) then begin
      // update counter frequency for CPUs with variable clock rate
      If (DynFreqUpdate) then QueryPerformanceFrequency(PCFreqA[Idx]);
      If (FPS <> nil) then
        FPS^ := Round( FPSFrameCnt[Idx] * (1000000 / FPSTimeSum[Idx]));
      FPSTimeSum[Idx] := 0; FPSFrameCnt[Idx] := 0;
    end;
  end;

If (Idx = 0) and (Time.BufSize > 0) then
  With Time do begin
    Buffer[BufIdx] := Result;
    Inc(BufIdx); If (BufIdx >= BufSize) then BufIdx := 0;
    If (BufCnt < BufSize) then Inc(BufCnt);
    If (BufCnt > 1) then begin
      Result := 0;
      For n:=0 to BufCnt-1 do Inc(Result, Buffer[n]);
      Result := Result div BufCnt;
    end;
  end;
end;


function TimeRealMks: Double;
begin
If PCEnabled then begin
  QueryPerformanceCounter(PCTime2[0]);
  Result:=( (PCTime2[0]-PCTime1[0]) * 1000000 ) / PCFreq;
  PCTime1[0]:=PCTime2[0];
end else Result:=0;
end;

function TimeStrMs: String;
Var i : Integer;
begin
i := TimeMs; Str(i, Result);
end;

function TimeStrMks: String;
Var i : Integer;
begin
i := TimeMks; Str(i, Result);
end;

procedure TimeShowMks;
Var i : Integer;
    s : String;
begin
i := TimeMks; Str(i, s);
MessageBox(0, PChar(s), 'Time elapsed, mks ', MB_OK or MB_ICONWARNING);
end;

procedure xTimeInit;
Var n : Integer;
begin
QueryPerformanceFrequency(PCFreq);
PCEnabled := (PCFreq<>0);
For n:=0 to MaxCounters-1 do PCFreqA[n] := PCFreq;
end;

initialization
xTimeInit;
finalization

end.
