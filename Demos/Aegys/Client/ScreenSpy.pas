unit ScreenSpy;

interface

Uses
 Windows, Classes, SysUtils, Graphics, Math, ZLibEx, uUDPSuperComponents;

Const
 BSIZE = 16;
 BNUMS = BSIZE * BSIZE;

Type
 PSpyCmd = ^TSpyCmd;
 TSpyCmd = Packed record
  Cmd  :  Byte;
  Size : Integer;
End;

Type
 PCtlCmd = ^TCtlCmd;
 TCtlCmd = Packed Record
  Cmd:  Byte;
  X, Y: Word;
End;

Type
 TScreenSpy = class(TThread)
 Private
  FSocket      : TUDPSuperClient;
  FmsScr,
  FmsSend      : TMemoryStream;
  FWidth,
  FHeight,
  FBWidth,
  FBHeight,
  FSize        : Integer;
  FBmps        : Array[0..BNUMS]     Of TBitmap;
  FFocus       : Array[0..BNUMS - 1] Of Integer;
  FFlag1       : Array[0..BNUMS - 1] Of Boolean;
  FFlag2       : Array[0..BNUMS - 1] Of Boolean;
  FDC          : HDC;
  FCmd         : TSpyCmd;
  FPixelFormat : TPixelFormat;
  Function  CheckScr : Boolean;
  Procedure SendData;
  Procedure GetFirst;
  Procedure GetNext;
 Protected
  Procedure Execute; Override;
 Public
  Constructor Create;  Reintroduce;
  Destructor  Destroy; Override;
  Property Socket      : TUDPSuperClient Read FSocket      Write FSocket;
  Property PixelFormat : TPixelFormat    Read FPixelFormat Write FPixelFormat;
End;

implementation

constructor TScreenSpy.Create;
Var
 i : Integer;
Begin
 FreeOnTerminate := True;
 FmsScr          := TMemoryStream.Create;
 FmsSend         := TMemoryStream.Create;
 FWidth          := 0;
 FHeight         := 0;
 FPixelFormat    := pf8bit;
 For I := 0 To BNUMS Do
  FBmps[i] := TBitmap.Create;
 Inherited Create(True);
End;

Destructor TScreenSpy.Destroy;
Var
 i : Integer;
Begin
 FSocket.Active := False;
 FmsScr.Free;
 FmsSend.Free;
 For i := 0 To BNUMS Do
  FBmps[i].Free;
 Inherited;
End;

Procedure TScreenSpy.Execute;
Begin
 Try
  While (Not Terminated) and (FSocket.Active) Do
   Begin
    Try
     If CheckScr Then
      GetFirst
     Else
      GetNext;
    Except
    End;
//    Sleep(30);
    If FSocket = Nil Then
     Break;
   End;
 Except
 End;
End;

Function TScreenSpy.CheckScr : Boolean;
Var
 nWidth,
 nHeight,
 I        : Integer;
Begin
 Result  := False;
 nWidth  := GetSystemMetrics(SM_CXSCREEN);
 nHeight := GetSystemMetrics(SM_CYSCREEN);
 If (nWidth <> FWidth) Or (nHeight <> FHeight) Then
  Begin
   FWidth   := nWidth;
   FHeight  := nHeight;
   FBWidth  := (FWidth  + BSIZE - 1) Div BSIZE;
   FBHeight := (FHeight + BSIZE - 1) Div BSIZE;
   For i := 0 To BNUMS Do
    Begin
     FBmps[i].Width       := FBWidth;
     FBmps[i].Height      := FBHeight;
     FBmps[i].PixelFormat := FPixelFormat;
    End;
   Case FPixelFormat Of
    pf1bit  : FSize := BytesPerScanline(FBWidth, 1, 32)  * FBHeight;
    pf4bit  : FSize := BytesPerScanline(FBWidth, 4, 32)  * FBHeight;
    pf8bit  : FSize := BytesPerScanline(FBWidth, 8, 32)  * FBHeight;
    pf16bit : FSize := BytesPerScanline(FBWidth, 16, 32) * FBHeight;
    pf24bit : FSize := BytesPerScanline(FBWidth, 24, 32) * FBHeight;
    pf32bit : FSize := BytesPerScanline(FBWidth, 32, 32) * FBHeight;
    Else
     FSize  := BytesPerScanline(FBWidth, 8, 32) * FBHeight;
   End;
    Result := True;
  End;
End;

procedure TScreenSpy.GetFirst;
Var
 rt : TRect;
 i,
 l,
 t  : Integer;
Begin
 FmsScr.Clear;
 FDC := GetDC(0);
 For I := 0 To BNUMS - 1 Do
  Begin
   l  := (I mod BSIZE) * FBWidth;
   t  := (I div BSIZE) * FBHeight;
   BitBlt(FBmps[i].Canvas.Handle, 0, 0, FBWidth, FBHeight, FDC, l, t, SRCCOPY);
   SetRect(rt, l, t, l + FBWidth, t + FBHeight);
   FmsScr.WriteBuffer(rt, SizeOf(rt));
   FBmps[i].SaveToStream(FmsScr);
  End;
 ReleaseDC(0, FDC);
 SendData;
 FillChar(FFocus, SizeOf(FFocus), #0);
End;

Procedure TScreenSpy.GetNext;
Var
 pt : TPoint;
 rt : TRect;
 i,
 l,
 t  : Integer;
Begin
 FmsScr.Clear;
 For i := 0 To BNUMS - 1 Do
  Begin
   FFlag1[i] := FFocus[i] > 0;
   FFlag2[i] := False;
  End;
 GetCursorPos(pt);
 FFlag1[pt.X div FBWidth + pt.Y div FBHeight * BSIZE] := True;
 FDC := GetDC(0);
 i   := 0;
 While (i < BNUMS) Do
  Begin
   If (FFlag1[i] And (Not FFlag2[i])) Then
    Begin
     FFlag2[i] := True;
     l := (i mod BSIZE) * FBWidth;
     t := (i div BSIZE) * FBHeight;
     FBmps[BNUMS].Canvas.Lock;
     Try
      BitBlt(FBmps[BNUMS].Canvas.Handle, 0, 0, FBWidth, FBHeight, FDC, l, t, SRCCOPY);
     Finally
      FBmps[BNUMS].Canvas.Unlock;
     End;
     If CompareMem(FBmps[i].ScanLine[FBHeight - 1], FBmps[BNUMS].ScanLine[FBHeight - 1], FSize) Then
      FFocus[i] := Max(FFocus[i] - 1, 0)
     Else
      Begin
       FBmps[i].Canvas.Lock;
       Try
        BitBlt(FBmps[i].Canvas.Handle, 0, 0, FBWidth, FBHeight, FDC, l, t, SRCCOPY);
       Finally
        FBmps[i].Canvas.Unlock;
       End;
       FFocus[i] := 5;
       SetRect(rt, l, t, l +  FBWidth, t + FBHeight);
       FmsScr.WriteBuffer(rt, SizeOf(rt));
       FBmps[i].SaveToStream(FmsScr);
       If ((i - BSIZE) >= 0)             Then FFlag1[i - BSIZE] := True;
       If ((i + BSIZE) <= (BNUMS - 1))   Then FFlag1[i + BSIZE] := True;
       If ((i Mod BSIZE) <> 0)           Then FFlag1[i - 1]     := True;
       If ((i Mod BSIZE) <> (BSIZE - 1)) Then FFlag1[i + 1]     := True;
       i := Max(Min(i - BSIZE, i - 1), 0);
       Continue;
      End;
    End;
   Inc(i);
  End;
 ReleaseDC(0, FDC);
 If (FmsScr.Size > 0) Then
  SendData;
End;

Function GetIpSend(Client : TUDPSuperClient;
                   PeerConnected : TPeerConnected) : String;
Begin
 If PeerConnected.RemoteIP = Client.OnLineIP Then
  Result := PeerConnected.LocalIP
 Else
  Result := PeerConnected.RemoteIP;
End;

Function MemoryStreamToString(Stream : TMemoryStream) : String; //Alterado por XyberX
Var
 StringBytes : TBytes;
Begin
 Result := '';
 Try
  Stream.Position := 0;
  SetLength(StringBytes, Stream.Size);
  Stream.ReadBuffer(StringBytes, Stream.Size);
  Result          := TEncoding.ANSI.GetString(StringBytes);
 Finally
 End;
End;

Procedure TScreenSpy.SendData;
Var
 PeerConnected : TPeerConnected;
Begin
 Try
  FmsSend.Clear;
  FmsScr.Position := 0;
  If FSocket.Active Then
   Begin
    ZCompressStream(FmsScr, FmsSend);
    FmsSend.Position := 0;
    PeerConnected := FSocket.GetActivePeer;
    If PeerConnected <> Nil Then
     FSocket.SendLongBuffer(GetIpSend(FSocket, PeerConnected), PeerConnected.Port,
                            MemoryStreamToString(FmsSend), False, dtt_Async);
   End;
 Except
 End;
End;

End.
