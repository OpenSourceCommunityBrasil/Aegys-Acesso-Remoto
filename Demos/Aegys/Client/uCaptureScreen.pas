unit uCaptureScreen;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  Vcl.ExtCtrls, System.DateUtils, System.SyncObjs, Windows,
  Vcl.Graphics, Forms, StreamManager, System.NetEncoding, ZLibEX,
  Form_Main, AwResizeImage, System.Math, uCaptureDeviceMode,
  vfw, jpeg, ImageCapture, JDRMGraphics,
  uUDPSuperComponents, uScanlineComparer;

Type
  TInitCapture = Procedure(Var ActualFrame: String; FramesPerSecond: Integer;
    DrawCur: Boolean = False; ImageViewQ: TImageViewQ = tiv_Medium); StdCall;
  TStopCapture = Procedure; Stdcall;

Type
  TListOfPictures = TList;

type
  TdmCaptureScreen = class(TDataModule)
    FastDesktop: TJDRMDesktop;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure FastDesktopNewBlock(Sender: TObject; Block: TJDRMImageBlock);
  private
    { Private declarations }
    ListOfPictures: TListOfPictures;
    vImageViewQ: TImageViewQ;
    vMaxNewFrame, bmpWidth, bmpHeight, vCols, vRows, vAtualCol,
      vAtualRow: Integer;
    vDiferencial: Boolean;
    FVideoCs, FMirrorListCs: TCriticalSection;
    vInitImageCaptureDif, vInitImageCapture: TImageCapture;
    ImageCatcher: TImageCatcher;
    procedure ScreenCapture; Overload;
    Function ScreenCaptureF: String; Overload;
    procedure ScreenCaptureNoDiff(Var Mybmp: TBitmap);
    Procedure CaptureScreenShots(Time, FPS: LongInt);
    Function CaptureScreenShot: String;
    Procedure SetImageViewQ(Value: TImageViewQ);
    // Function  Capture : Boolean;
    Procedure Lock;
    Procedure UnLock;
  public
    { Public declarations }
    Procedure StopCapture;
    Procedure InitCapture;
    Function AviStreamOfBmpstreams(Time, FPS: LongInt): String;
    Procedure ClearAtualFrame;
    Procedure RenewCommand;
    Property ImageViewQ: TImageViewQ Read vImageViewQ Write SetImageViewQ;
    Property CaptureWidth: Integer Read bmpWidth Write bmpWidth;
    Property CaptureHeight: Integer Read bmpHeight Write bmpHeight;
    Property Cols: Integer Read vCols Write vCols;
    Property Rows: Integer Read vRows Write vRows;
    Property Diferencial: Boolean Read vDiferencial Write vDiferencial;
    Property MaxNewFrame: Integer Read vMaxNewFrame Write vMaxNewFrame;
    Procedure OnProgress(Sender: TObject);
  end;

Procedure CompareStreamASM(Const s, d: Pointer; Var c: Pointer); Assembler;
Procedure ResumeStream(MyFirstStream, MySecondStream, MyCompareStream
  : TMemoryStream);

var
  dmCaptureScreen: TdmCaptureScreen;
  ShutdownEvent: TLightweightEvent;
  vAtualMemoryStream: String;
  vNumFramesToNew: Integer = 0;
  ASMSize, muASM: Integer;
  pdst: Pointer;
  CapWnd: THandle; // Objeto de Captura
  CapParms: TCaptureParms; // Parametros de Captura
  BMPINFO: TBitmapInfo; // Informações do BMP Capturado
  dllPath, ResultString: String;
  vCaptureInit: Boolean = False;
  DLL: THandle;
  InitCaptureDll: TInitCapture;
  StopCaptureDll: TStopCapture;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}
{$R *.dfm}

// Compress Stream with zLib
Function CompressStream(Const SrcStream: TMemoryStream;
  DestStream: TMemoryStream): Boolean;
Var
  zipFile: TZCompressionStream;
begin
  Result := False;
  Try
    zipFile := TZCompressionStream.Create(DestStream, zcDefault);
    SrcStream.Position := 0;
    zipFile.CopyFrom(SrcStream, SrcStream.Size);
    Result := True;
  Finally
    zipFile.Free;
  End;
End;

Function MemoryStreamToString(M: TMemoryStream): String;
Var
  _MemStr: TStringStream;
Begin
  _MemStr := TStringStream.Create;
  Try
    _MemStr.LoadFromStream(M);
  Finally
    Result := _MemStr.DataString;
    _MemStr.Free;
  End;
End;

Function StreamToString(M: TStream): String;
Var
  _MemStr: TStringStream;
Begin
  _MemStr := TStringStream.Create;
  Try
    _MemStr.LoadFromStream(M);
  Finally
    Result := _MemStr.DataString;
    _MemStr.Free;
  End;
End;

Procedure StringToMemoryStream(Value: TMemoryStream; Str: String);
Var
  _MemStr: TStringStream;
Begin
  _MemStr := TStringStream.Create(Str);
  Try
    _MemStr.SaveToStream(Value);
  Finally
    _MemStr.Free;
  End;
end;

Function StringToHex(s: String): String;
Begin
  SetLength(Result, Length(s) * 4);
  BinToHex(s[1], PWideChar(Result), Length(s) * SizeOf(Char));
End;

Procedure TdmCaptureScreen.OnProgress(Sender: TObject);
Begin
{$IFDEF MSWINDOWS}
{$IFNDEF FMX}Application.Processmessages;
{$ELSE}FMX.Forms.TApplication.Processmessages; {$ENDIF}
{$ENDIF}
End;

Function ZCompressStr(const s: String;
  level: TZCompressionLevel = zcMax): String;
Var
  Compress: TZCompressionStream;
  SrcStream, OutPut: TStringStream;
Begin
  OutPut := TStringStream.Create('');
  SrcStream := TStringStream.Create(s, TEncoding.UTF8);
  OutPut.Position := 0;
  Try
    Compress := TZCompressionStream.Create(OutPut, zcMax);
    Compress.OnProgress := dmCaptureScreen.OnProgress;
    Compress.CopyFrom(SrcStream, 0);
    FreeAndNil(Compress);
    OutPut.Position := 0;
    Result := OutPut.DataString;
    FreeAndNil(OutPut);
    FreeAndNil(SrcStream);
  Except
  End;
End;

Function CompressStreamS(Const SrcS: String; Var DestS: String): Boolean;
Var
  zipFile: TZCompressionStream;
  strInput, strOutput: TStringStream;
begin
  Result := False;
  Try
    strInput := TStringStream.Create(SrcS);
    strOutput := TStringStream.Create;
    Try
      zipFile := TZCompressionStream.Create(strOutput, zcMax);
      zipFile.CopyFrom(strInput, strInput.Size);
    Finally
      zipFile.Free;
    End;
    DestS := strOutput.DataString;
    Result := True;
  Finally
    strInput.Free;
    strOutput.Free;
  End;
End;

Function StringToMemoryStreamF(Str: String): TMemoryStream;
Var
  _MemStr: TStringStream;
Begin
  _MemStr := TStringStream.Create(Str);
  Try
    Result := TMemoryStream.Create;
    _MemStr.SaveToStream(Result);
  Finally
    _MemStr.Free;
  End;
end;

Function FrameCallBack(hWnd: hWnd; lpVHdr: PVIDEOHDR): LongInt; Stdcall;
Var
  hd: THandle;
  memStream: TStringStream;
  Bitmap: TBitmap;
Begin
  // Criando BMP
  Bitmap := TBitmap.Create;
{$IFDEF MSWINDOWS}
  Bitmap.Width := BMPINFO.bmiHeader.biWidth; // New size of Bitmap
  Bitmap.Height := BMPINFO.bmiHeader.biHeight;
  hd := DrawDibOpen;
  DrawDibDraw(hd, Bitmap.Canvas.Handle, 0, 0, BMPINFO.bmiHeader.biWidth,
    BMPINFO.bmiHeader.biHeight, @BMPINFO.bmiHeader, lpVHdr^.lpData, 0, 0,
    BMPINFO.bmiHeader.biWidth, BMPINFO.bmiHeader.biHeight, 0);
  DrawDibClose(hd);
{$ENDIF}
  // TMemoryStream Criando e Mexendo
  memStream := TStringStream.Create;
  Try
    Bitmap.SaveToStream(memStream);
    ResultString := memStream.DataString;
  Finally
    memStream.Free;
    Bitmap.Free;
  End;
End;

Procedure TdmCaptureScreen.RenewCommand;
Begin
  vAtualMemoryStream := '';
  vNumFramesToNew := 0;
  If (vInitImageCapture <> Nil) Then
    vInitImageCapture.ImageBase := Nil;
  If vInitImageCaptureDif <> Nil Then
    vInitImageCaptureDif.ImageBase := Nil;
End;

Procedure ResumeStream(MyFirstStream, MySecondStream, MyCompareStream
  : TMemoryStream);
Var
  I: Integer;
  P1, P2, P3: ^AnsiChar;
Begin
  P1 := MyFirstStream.Memory;
  MyFirstStream.Position := 0;
  MySecondStream.SetSize(MyCompareStream.Size);
  P2 := MySecondStream.Memory;
  P3 := MyCompareStream.Memory;
  For I := 0 to MyCompareStream.Size - 1 do
  Begin
    If I > (MyFirstStream.Size - 1) Then
      P2^ := P3^
    Else
    Begin
      If P3^ = '0' Then
        P2^ := P1^
      Else
        P2^ := P3^;
    End;
    If I <= (MyFirstStream.Size - 1) Then
      Inc(P1);
    Inc(P2);
    Inc(P3);
  End;
  MySecondStream.Position := 0;
  MyFirstStream.Clear;
  MyFirstStream.CopyFrom(MySecondStream, 0);
  MySecondStream.Position := 0;
End;

Procedure TdmCaptureScreen.SetImageViewQ(Value: TImageViewQ);
Begin
  vImageViewQ := Value;
  If (vInitImageCapture <> Nil) Then
  Begin
    If (vInitImageCapture.PixelFormat <> TPixelFormat(vImageViewQ)) then
    Begin
      If vImageViewQ = tiv_MonoC Then
        vInitImageCapture.PixelFormat := pf8Bit
      Else
        vInitImageCapture.PixelFormat := TPixelFormat(vImageViewQ);
      vInitImageCaptureDif.PixelFormat := vInitImageCapture.PixelFormat;
    End;
  End;
End;

Procedure TdmCaptureScreen.ClearAtualFrame;
Begin
  vAtualMemoryStream := '';
End;

Procedure TdmCaptureScreen.Lock;
Begin
  FMirrorListCs.Enter;
End;

Procedure TdmCaptureScreen.UnLock;
Begin
  FMirrorListCs.Leave;
End;

{
  Function TdmCaptureScreen.Capture : Boolean;
  Var
  forewnd : THandle;
  n,idx : Integer;
  mskd : boolean;
  rc : Boolean;
  Begin
  FVideoCs.Enter;
  Try
  Lock;
  try
  FCurrentDesktopName := SwitchToActiveDesktop;
  Result := False;

  if not RefreshMirrorList and not result then begin
  result:=FMirrorListChanged;
  exit;
  end;
  result:=FMirrorListChanged;

  forewnd:=GetForegroundWindow;
  for n:=FMirrorList.Count-1 downto 0 do begin
  wm:=FMirrorList[n];
  if not assigned(wm.FBitmapCache) and not wm.FByPass then begin
  wm.FBitmapCache:=wm.CreateBitmap;
  end;
  rc:=wm.Capture;
  result:=result or rc;
  end;

  if result then Inc(FCaptureIndex);
  finally
  Unlock;
  end;
  Finally
  FVideoCs.Leave;
  End;
  End;
}

Procedure TdmCaptureScreen.CaptureScreenShots(Time, FPS: LongInt);
Var
  vInterval, vTempWait: Integer;
  vGetScreen: TDateTime;
  vTempWaitScreen: LongWord;
Begin
  If frm_Main.CloseConnection Then
    Exit;
  vInterval := (1000 div FPS);
  vInterval := (Time div vInterval);
  vGetScreen := Now;
  vTempWaitScreen := (1000 div FPS);
  vTempWait := System.DateUtils.MilliSecondsBetween(Now, vGetScreen);
  While (vInterval >= vTempWait) Do
  Begin
    If frm_Main.CloseConnection Then
      Exit;
    ScreenCapture;
    vTempWait := System.DateUtils.MilliSecondsBetween(Now, vGetScreen);
    ShutdownEvent.WaitFor(1);
  End;
End;

Function TdmCaptureScreen.CaptureScreenShot: String;
Begin
  Result := ScreenCaptureF;
End;

Function TdmCaptureScreen.AviStreamOfBmpstreams(Time, FPS: LongInt): String;
Var
  I, A, vInterval: Integer;
  vGetScreen: TDateTime;
  vTempWaitScreen: LongWord;
  vNewFrame, vInfoPack, vTempMemoryStream, vResultMemoryStream,
    vCompressMemoryStream, vTempMem: String;
  vImageBase: TBitmap;
  Procedure StringToStringStream(Var Result: String; Source: String);
  Var
    vResult: TStringStream;
  Begin
    vResult := TStringStream.Create(Source);
    Try
      Result := vResult.DataString;
    Finally
      vResult.SetSize(0);
      FreeAndNil(vResult);
    End;
  End;
  Function ImageToString(Mybmp: TBitmap): String;
  Var
    Stream: TStringStream;
  Begin
    Result := '';
    If Mybmp <> Nil Then
    Begin
      Try
        Stream := TStringStream.Create;
        Mybmp.SaveToStream(Stream);
        Stream.Position := 0;
        Result := Stream.DataString;
      Finally
        FreeAndNil(Stream);
      End;
    End;
  End;
  Procedure IncrementColsLines;
  Begin
    Inc(vAtualCol);
    If ((vAtualCol + 1) > vCols) then
    Begin
      vAtualCol := 0;
      Inc(vAtualRow);
    End;
    If (vAtualRow + 1) > vRows then
    Begin
      vAtualRow := 0;
      vAtualCol := 0;
    End;
  End;

Begin
  If Not vCaptureInit Then
    InitCapture;
  ShutdownEvent.SetEvent;
  Result := '';
  I := 0;
  If frm_Main.CloseConnection Then
    Exit;
  vTempMemoryStream := '';
  Result := '';
  Try
    If frm_Main.CloseConnection Then
      Exit;
    If (vDiferencial) Then
    Begin
      If DllCapture Then
        vTempMemoryStream := ResultString
      Else
        vTempMemoryStream := CaptureScreenShot;
      If Comparer Then
      Begin
        If (Length(vAtualMemoryStream) > 0) And
          (vNumFramesToNew < vMaxNewFrame) Then
        Begin
          vNewFrame := '';
          If Length(vResultMemoryStream) > 0 Then
            vResultMemoryStream := '';
          vTempMem := vAtualMemoryStream;
          If CompareStreamS(vTempMem, vTempMemoryStream, vResultMemoryStream,
            frm_Main.MouseCapture) Then
          Begin
            If vTempMem = vAtualMemoryStream Then
              vAtualMemoryStream := vTempMemoryStream;
            If (Length(vTempMemoryStream) > 0) Then
              vTempMemoryStream := '';
            If (Length(vResultMemoryStream) > 0) Then
              vTempMemoryStream := vResultMemoryStream;
            // StringToStringStream(vTempMemoryStream, vResultMemoryStream);
          End
          Else
          Begin
            vResultMemoryStream := '';
            vTempMemoryStream := '';
          End;
        End
        Else If ((Length(vTempMemoryStream) > 0) And
          (vNumFramesToNew >= vMaxNewFrame)) Or
          ((vAtualMemoryStream = '') And (Length(vTempMemoryStream) > 0)) Then
        Begin
          vNumFramesToNew := 0;
          vAtualMemoryStream := vTempMemoryStream;
          vResultMemoryStream := '';
          vNewFrame := TNewFrameData;
        End;
      End
      Else
      Begin
        vNumFramesToNew := 0;
        vAtualMemoryStream := vTempMemoryStream;
        vResultMemoryStream := '';
        vNewFrame := TNewFrameData;
      End;
    End
    Else
    Begin
      // Informações de Geração do Pacote
      If vInitImageCapture.Colums <> vCols Then
      Begin
        vInitImageCapture.Colums := vCols;
        vInitImageCaptureDif.Colums := vCols;
      End;
      If vInitImageCapture.Rows <> vRows Then
      Begin
        vInitImageCapture.Rows := vRows;
        vInitImageCaptureDif.Rows := vRows;
      End;
      If vInitImageCapture.PixelFormat <> TPixelFormat(ImageViewQ) Then
        vInitImageCapture.PixelFormat := TPixelFormat(ImageViewQ);
      vImageBase := Nil;
      ScreenCaptureNoDiff(vImageBase);
      vInitImageCapture.ImageBase := vImageBase;
      If (vNumFramesToNew >= vMaxNewFrame) Or (vNumFramesToNew = 0) Then
      Begin
        vInitImageCaptureDif.ImageBase := vImageBase;
        If vImageBase <> Nil Then
          FreeAndNil(vImageBase);
        If (vNumFramesToNew > 0) Then
          vNumFramesToNew := 0
        Else
          Inc(vNumFramesToNew);
        vTempMemoryStream := ImageToString(vInitImageCapture.ImageBase);
        vAtualCol := 0;
        vAtualRow := 0;
        vInfoPack := '<|>IMAGETYPE=FULL';
      End
      Else
      Begin
        If (Length(vInitImageCapture.Images) > 0) And
          (Length(vInitImageCaptureDif.Images) > 0) Then
        Begin
          If Not(vInitImageCapture.CompareImages(vInitImageCapture.Images
            [vAtualCol][vAtualRow].Bitmap, vInitImageCaptureDif.Images
            [vAtualCol][vAtualRow].Bitmap)) then
          Begin
            vTempMemoryStream :=
              ImageToString(vInitImageCapture.Images[vAtualCol]
              [vAtualRow].Bitmap);
            vInitImageCaptureDif.GetAltImage(vInitImageCapture.Images[vAtualCol]
              [vAtualRow].Bitmap, vImageBase, vAtualCol, vAtualRow, True);
            vInitImageCaptureDif.ImageBase := vImageBase;
          End
          Else
            vTempMemoryStream := '';
        End
        Else
          vTempMemoryStream := '';
        vInfoPack := '<|>IMAGETYPE=SQUARE|' + IntToStr(vAtualCol) + ':' +
          IntToStr(vAtualRow);
        If vTempMemoryStream = '' Then
          Result := Result + '<|$initstream$|>' + vInfoPack + '<|>SIZE=' +
            IntToStr(Length(vTempMemoryStream)) + '<|>' + vTempMemoryStream +
            frm_Main.CommandEnd;
        If vImageBase <> Nil Then
        Begin
          // vImageBase.FreeImage;
          FreeAndNil(vImageBase);
        End;
        IncrementColsLines;
        Inc(vNumFramesToNew);
      End;
    End;
    If Length(vTempMemoryStream) > 0 Then
    Begin
{$IFDEF MSWINDOWS}
{$IFNDEF FMX}Application.Processmessages;
{$ELSE}FMX.Forms.TApplication.Processmessages; {$ENDIF}
{$ENDIF}
      Inc(vNumFramesToNew);
      If (vDiferencial) Then
      Begin
        If (vAtualMemoryStream = '') Then
          vAtualMemoryStream := vTempMemoryStream;
        // vCompressMemoryStream := ZCompressStr(vTempMemoryStream);
        Result := vTempMemoryStream;
        // Result + '<|$initstream$|>' + vNewFrame + '<|>SIZE=' + IntToStr(Length(vCompressMemoryStream)) + '<|>' + vCompressMemoryStream + frm_Main.CommandEnd;
      End
      Else
      Begin
        // vCompressMemoryStream := ZCompressStr(vTempMemoryStream);
        Result := vTempMemoryStream;
        // Result + '<|$initstream$|>' + vInfoPack + '<|>SIZE=' + IntToStr(Length(vCompressMemoryStream)) + '<|>' + vCompressMemoryStream + frm_Main.CommandEnd;
      End;
    End;
  Finally
    If Length(vCompressMemoryStream) > 0 Then
      vCompressMemoryStream := '';
    vTempMemoryStream := '';
    If (vDiferencial) Then
    Begin
      If vResultMemoryStream <> '' Then
      Begin
        If Length(vResultMemoryStream) > 0 Then
          vResultMemoryStream := '';
      End;
    End;
    If vImageBase <> Nil Then
    Begin
      // vImageBase.FreeImage;
      FreeAndNil(vImageBase);
    End;
  End;
end;

Procedure TdmCaptureScreen.InitCapture;
Begin
  If DllCapture Then
  Begin
    If Not vCaptureInit Then
    Begin
      DLL := LoadLibrary(PChar(dllPath));
      If (DLL <> 0) Then
      Begin
        InitCaptureDll := GetProcAddress(DLL, PChar('InitCapture'));
        StopCaptureDll := GetProcAddress(DLL, PChar('StopCapture'));
        InitCaptureDll(ResultString, FrameRate, MouseCaptureC, vImageViewQ);
      End;
      vCaptureInit := True;
    End;
  End;
End;

Procedure TdmCaptureScreen.StopCapture;
Begin
  If DllCapture Then
  Begin
    If vCaptureInit Then
    Begin
      vCaptureInit := False;
      If DLL <> 0 Then
      Begin
        StopCaptureDll;
        FreeLibrary(DLL);
      End;
    End;
  End;
End;

procedure TdmCaptureScreen.DataModuleCreate(Sender: TObject);
begin
  ListOfPictures := TListOfPictures.Create;
  ShutdownEvent := TLightweightEvent.Create;
  vInitImageCapture := TImageCapture.Create(Self);
  vInitImageCaptureDif := TImageCapture.Create(Self);
  vImageViewQ := tiv_Medium;
  bmpWidth := Screen.Width;
  bmpHeight := Screen.Height;
  vMaxNewFrame := 20;
  vAtualCol := 0;
  vAtualRow := 0;
  dllPath := ExtractFilePath(ParamStr(0)) + 'FastUtils.dll';
  ImageCatcher := TImageCatcher.Create;
  ImageCatcher.CatchType := ctWinapi; // ctWinapi, ctDirectX, ctDDraw
  ImageCatcher.TargetHandle := 0;
end;

procedure TdmCaptureScreen.DataModuleDestroy(Sender: TObject);
begin
  Try
    If Length(vAtualMemoryStream) > 0 Then
      vAtualMemoryStream := '';
    If vInitImageCapture <> Nil Then
      FreeAndNil(vInitImageCapture);
    If vInitImageCaptureDif <> Nil Then
      FreeAndNil(vInitImageCaptureDif);
    ListOfPictures.Free;
    ShutdownEvent.SetEvent;
    ShutdownEvent.Free;
    FreeAndNil(ImageCatcher);
  Except

  End;
end;

procedure TdmCaptureScreen.FastDesktopNewBlock(Sender: TObject;
  Block: TJDRMImageBlock);
Var
  I: Integer;
  vInfoPack, vLineSend, vCompressMemoryStream, vTempMemoryStream: String;
  PeerConnected: TPeerConnected;
  Function StreamToString(Mybmp: TMemoryStream): String; // Alterado por XyberX
  Var
    StringBytes: TBytes;
  Begin
    Result := '';
    If Mybmp <> Nil Then
    Begin
      Try
        Mybmp.Position := 0;
        SetLength(StringBytes, Mybmp.Size);
        Mybmp.ReadBuffer(StringBytes, Mybmp.Size);
        Result := TEncoding.ANSI.GetString(StringBytes);
      Finally
      End;
    End;
  End;
  Function BmpToString(Mybmp: TBitmap): String; // Alterado por XyberX
  Var
    StringBytes: TBytes;
    Stream: TMemoryStream;
  Begin
    Result := '';
    If Mybmp <> Nil Then
    Begin
      Try
        Stream := TMemoryStream.Create;
        Mybmp.SaveToStream(Stream);
        Stream.Position := 0;
        SetLength(StringBytes, Stream.Size);
        Stream.ReadBuffer(StringBytes, Stream.Size);
        Result := TEncoding.ANSI.GetString(StringBytes);
      Finally
        FreeAndNil(Stream);
      End;
    End;
  End;

Begin
  If FastDesktop.Active Then
  Begin
    vInfoPack := Format('<|>BLOCKDEFS=L%d|T%d|W%d|H%d|SW%d|SH%d',
      [Block.Left, Block.Top, Block.Width, Block.Height, Block.ScreenWidth,
      Block.ScreenHeight]);
    vTempMemoryStream := BmpToString(Block.AsBitmap);
    vCompressMemoryStream := ZCompressStr(vTempMemoryStream);
    vLineSend := '<|$initstream$|>' + vInfoPack + '<|>SIZE=' +
      IntToStr(Length(vCompressMemoryStream)) + '<|>' + vCompressMemoryStream +
      frm_Main.CommandEnd;
    If frm_Main <> Nil Then
    Begin
      If frm_Main.ipPSDeskTopClient.Active Then
      Begin
        PeerConnected := frm_Main.ipPSDeskTopClient.GetActivePeer;
        If PeerConnected <> Nil Then
          frm_Main.ipPSDeskTopClient.SendLongBuffer
            (frm_Main.ipPSDeskTopClient.GetIpSend(PeerConnected),
            PeerConnected.Port, vLineSend, False, dtt_ASync);
      End;
    End;
  End;
End;

Procedure ResizeBmp(bmp: TBitmap; Width, Height: Integer);
Var
  SrcBMP: TBitmap;
  DestBMP: TBitmap;
Begin
  SrcBMP := TBitmap.Create;
  DestBMP := TBitmap.Create;
  Try
    SrcBMP.Assign(bmp);
    Try
      DestBMP.Width := Width;
      DestBMP.Height := Height;
      SetStretchBltMode(DestBMP.Canvas.Handle, HALFTONE);
      StretchBlt(DestBMP.Canvas.Handle, 0, 0, DestBMP.Width, DestBMP.Height,
        SrcBMP.Canvas.Handle, 0, 0, SrcBMP.Width, SrcBMP.Height, SRCCOPY);
      bmp.Assign(DestBMP);
    Finally
      DestBMP.FreeImage;
      FreeAndNil(DestBMP);
    End;
  Finally
    SrcBMP.FreeImage;
    FreeAndNil(SrcBMP);
  End;
End;

Procedure TdmCaptureScreen.ScreenCaptureNoDiff(Var Mybmp: TBitmap);
Var
  Mycan: TCanvas;
  dc: HDC;
  R: TRect;
  vListOfPictures: ^String;
  Procedure MakeGrey(Bitmap: TBitmap);
  Var
    w, h, y, x: Integer;
    sl: PRGBTripleArray;
    grey: Byte;
  Begin
    Bitmap.PixelFormat := pf32bit;
    w := Bitmap.Width;
    h := Bitmap.Height;
    For y := 0 To h - 1 Do
    Begin
      sl := Bitmap.ScanLine[y];
      For x := 0 To w - 1 Do
      Begin
        With sl[x] Do
        Begin
          grey := (B + G + R) div 3;
          B := grey;
          G := grey;
          R := grey;
        End;
      End;
    End;
  End;
  Function BmpToString: String;
  Var
    Stream: TStringStream;
  Begin
    Result := '';
    If Mybmp <> Nil Then
    Begin
      Try
        Stream := TStringStream.Create;
        Mybmp.SaveToStream(Stream);
        FreeAndNil(Mybmp);
        Result := Stream.DataString;
      Finally
        FreeAndNil(Stream);
      End;
    End;
  End;
  Procedure GetScreenToBmp(Var Mybmp: TBitmap; Width, Height: Integer;
    DrawCur: Boolean = False);
  Var
    Cursorx, Cursory: Integer;
    dc: HDC;
    Mycan: TCanvas;
    R: TRect;
    DrawPos: TPoint;
    MyCursor: TIcon;
    hld: hWnd;
    Threadld: dword;
    mp: TPoint;
    pIconInfo: TIconInfo;
  Begin
    Try
      Mycan := TCanvas.Create;
      dc := GetWindowDC(0);
      Mycan.Handle := dc;
      R := Rect(0, 0, GetSystemMetrics(SM_CXSCREEN),
        GetSystemMetrics(SM_CYSCREEN));
      Mybmp.Width := R.Right;
      Mybmp.Height := R.Bottom;
      Mybmp.Canvas.CopyRect(R, Mycan, R);
    Finally
      ReleaseDC(0, dc);
      FreeAndNil(Mycan);
    End;
    If DrawCur Then
    Begin
      GetCursorPos(DrawPos);
      MyCursor := TIcon.Create;
      GetCursorPos(mp);
      hld := WindowFromPoint(mp);
      Threadld := GetWindowThreadProcessId(hld, nil);
      AttachThreadInput(GetCurrentThreadId, Threadld, True);
      MyCursor.Handle := Getcursor();
      AttachThreadInput(GetCurrentThreadId, Threadld, False);
      GetIconInfo(MyCursor.Handle, pIconInfo);
      Cursorx := DrawPos.x - round(pIconInfo.xHotspot);
      Cursory := DrawPos.y - round(pIconInfo.yHotspot);
      Mybmp.Canvas.Draw(Cursorx, Cursory, MyCursor);
      DeleteObject(pIconInfo.hbmColor);
      DeleteObject(pIconInfo.hbmMask);
      MyCursor.ReleaseHandle;
      MyCursor.Free;
    End;
    {
      If (bmpWidth  > 0) And
      (bmpHeight > 0) Then
      SmoothResize(Mybmp, bmpWidth, bmpHeight); //, clBlack);
    }
    Try
      If vImageViewQ = tiv_MonoC Then
      Begin
        MakeGrey(Mybmp);
        Mybmp.PixelFormat := pf4bit;
      End
      Else
        Mybmp.PixelFormat := TPixelFormat(ImageViewQ);
    Except
      FreeAndNil(Mybmp);
    End;
  End;

Begin
  Try
    If Mybmp = Nil Then
      Mybmp := TBitmap.Create;
    GetScreenToBmp(Mybmp, bmpWidth, bmpHeight, frm_Main.MouseCapture);
  Except
    // Mybmp.FreeImage;
    FreeAndNil(Mybmp);
    Exit;
  End;
End;

Procedure CompareStreamASM(Const s, d: Pointer; Var c: Pointer); Assembler;
Var
  src: ^AnsiChar;
  dest: ^AnsiChar;
  n1, n2: Cardinal;
  Asm
    mov muASM, 0
    mov pdst, ECX              // Move resolutado pra PDST
    mov src, EAX               // Move S pra src
    mov dest, EDX              // Move D pra dest
    call System.@LStrLen       // Tamanho de string S
    mov n1, EAX                // Move tamanho do S para n1
    mov EAX, dest              // Move dest para EAX
    call System.@LStrLen       // Tamanho do dst/D
    mov n2, EAX                // Move Tamanho D para n2
    mov EDX, EAX               // Move tamanho D para EDX segundo parametro setlenght
    mov EAX, pdst              // Move Result/pdst para EAX primeiro parametro strlenght
    call System.@LStrSetLength // Seta parametro pdst para tamanjo n2
    mov ECX, ASMSize           // Mov n2 para ECX para controlar loopings
    test ECX, ECX              // Testa ECX
    jz @@end                   // Se EXX = 0 Termina
  push ESI                   // Guarda ESI na pilha
  push EDI
  mov EAX, pdst              // EAX := pdst; //Endereço da string de resultado
  mov ESI, src               // ESI := src; //String de origem
  mov EDI, dest
  mov EDX, [EAX]             // EDX := pdst^; //String de resultado
@@cycle:
  mov AL, [ESI]             // Move um caracter do primeiro stream para AL
  cmp AL, [EDI]             // Copara caracter com o segundo stream
  je @@igual               // Se for igual pula para igual
  mov AL, [EDI]             // Se defente copia Carcter do Segund stream para AL
  mov [EDX], AL             // Coloca caracter no terceiro stream
  mov muASM, 1
  cmp AL, AL                // Apenas para gerra um Je
  je @@incremento           // Incrementa caracter
@@igual:
  mov AL, '0'               // Se for igual Coloca '0' em AL
  mov [EDX], AL             // Move '0' para terceiro Stream
@@incremento:
  inc ESI
  inc EDI
  inc EDX
  dec ECX
  cmp ECX, 0
  ja @@cycle
  pop EDI
  pop ESI                   // Recupera ESI na pilha
@@end:
End;

Function TdmCaptureScreen.ScreenCaptureF: String;
Var
  Mycan: TCanvas;
  dc: HDC;
  R: TRect;
  vListOfPictures: ^String;
  Procedure MakeGrey(Bitmap: TBitmap);
  Var
    w, h, y, x: Integer;
    sl: PRGBTripleArray;
    grey: Byte;
  Begin
    Bitmap.PixelFormat := pf32bit;
    w := Bitmap.Width;
    h := Bitmap.Height;
    For y := 0 To h - 1 Do
    Begin
      sl := Bitmap.ScanLine[y];
      For x := 0 To w - 1 Do
      Begin
        With sl[x] Do
        Begin
          grey := (B + G + R) div 3;
          B := grey;
          G := grey;
          R := grey;
        End;
      End;
    End;
  End;
  Function BmpToString(Mybmp: TBitmap): String; // Alterado por XyberX
  Var
    Stream: TStringStream;
  Begin
    Result := '';
    If Mybmp <> Nil Then
    Begin
      Stream := TStringStream.Create('', CompressionDecoding);
      // TMemoryStream.Create;
      Try
        Mybmp.SaveToStream(Stream);
        Stream.Position := 0;
        Result := Stream.DataString;
      Finally
        FreeAndNil(Stream);
      End;
    End;
  End;
  Procedure SmoothResize(abmp: TBitmap; NuWidth, NuHeight: Integer);
  Var
    xscale, yscale, sfrom_y, sfrom_x, weight, total_red, total_green,
      total_blue: Single;
    ifrom_y, ifrom_x, to_y, to_x, new_red, new_green, new_blue, ix, iy, liSize,
      loSize: Integer;
    weight_x, weight_y: Array [0 .. 1] of Single;
    bTmp: TBitmap;
    sli, slo: pRGBArray;
    { pointers for scanline access }
    liPByte, loPByte, p: PByte;
  Begin
    abmp.PixelFormat := pf24bit;
    bTmp := TBitmap.Create;
    bTmp.PixelFormat := pf24bit;
    bTmp.Width := NuWidth;
    bTmp.Height := NuHeight;
    xscale := bTmp.Width / (abmp.Width - 1);
    yscale := bTmp.Height / (abmp.Height - 1);
    liPByte := abmp.ScanLine[0];
    liSize := Integer(abmp.ScanLine[1]) - Integer(liPByte);
    loPByte := bTmp.ScanLine[0];
    loSize := Integer(bTmp.ScanLine[1]) - Integer(loPByte);
    For to_y := 0 To (bTmp.Height - 1) Do
    Begin
      sfrom_y := (to_y / yscale);
      ifrom_y := Trunc(sfrom_y);
      weight_y[1] := (sfrom_y - ifrom_y);
      weight_y[0] := (1 - weight_y[1]);
      For to_x := 0 To bTmp.Width - 1 Do
      Begin
        sfrom_x := to_x / xscale;
        ifrom_x := Trunc(sfrom_x);
        weight_x[1] := sfrom_x - ifrom_x;
        weight_x[0] := 1 - weight_x[1];
        total_red := 0.0;
        total_green := 0.0;
        total_blue := 0.0;
        For ix := 0 To 1 Do
        Begin
          For iy := 0 To 1 Do
          Begin
            p := liPByte;
            Inc(p, liSize * (ifrom_y + iy));
            sli := pRGBArray(p);
            new_red := sli[ifrom_x + ix].rgbtRed;
            new_green := sli[ifrom_x + ix].rgbtGreen;
            new_blue := sli[ifrom_x + ix].rgbtBlue;
            weight := weight_x[ix] * weight_y[iy];
            total_red := total_red + new_red * weight;
            total_green := total_green + new_green * weight;
            total_blue := total_blue + new_blue * weight;
          End;
        End;
        p := loPByte;
        Inc(p, (loSize * to_y));
        slo := pRGBArray(p);
        slo[to_x].rgbtRed := round(total_red);
        slo[to_x].rgbtGreen := round(total_green);
        slo[to_x].rgbtBlue := round(total_blue);
      End;
    End;
    abmp.Width := bTmp.Width;
    abmp.Height := bTmp.Height;
    abmp.Canvas.Draw(0, 0, bTmp);
    FreeAndNil(bTmp);
  End;
  Procedure GetScreenToBmp(out Mybmp: TBitmap; Width, Height: Integer;
    DrawCur: Boolean = False);
  Var
    Cursorx, Cursory: Integer;
    DrawPos: TPoint;
    MyCursor: TIcon;
    hld: hWnd;
    Threadld: dword;
    mp: TPoint;
    pIconInfo: TIconInfo;
  Const
    CAPTUREBLT = $40000000;
  Var
    hdcScreen, hdcCompatible: HDC;
    hbmScreen: HBITMAP;
  Begin
    Mycan := TCanvas.Create;
    {
      dc := GetWindowDC(0);
      Try
      Mycan.Handle := dc;
      R := Rect(0, 0, GetSystemMetrics(SM_CXSCREEN), GetSystemMetrics(SM_CYSCREEN));
      Mybmp.Width := R.Right;
      Mybmp.Height := R.Bottom;
      Mybmp.Canvas.CopyRect(R, Mycan, R);
      Finally
      releaseDC(0, dc);
      End;
      Mycan.Handle := 0;
      Mycan.Free;
    }
    hdcScreen := CreateDC('DISPLAY', nil, nil, nil);
    hdcCompatible := CreateCompatibleDC(hdcScreen);
    hbmScreen := CreateCompatibleBitmap(hdcScreen,
      GetDeviceCaps(hdcScreen, HORZRES), GetDeviceCaps(hdcScreen, VERTRES));
    SelectObject(hdcCompatible, hbmScreen);
    Mybmp := TBitmap.Create;
    Mybmp.Handle := hbmScreen;
    BitBlt(hdcCompatible, 0, 0, Mybmp.Width, Mybmp.Height, hdcScreen, 0,
      0, SRCCOPY);
    DeleteDC(hdcScreen);
    DeleteDC(hdcCompatible);
    If DrawCur Then
    Begin
      GetCursorPos(DrawPos);
      MyCursor := TIcon.Create;
      GetCursorPos(mp);
      hld := WindowFromPoint(mp);
      Threadld := GetWindowThreadProcessId(hld, nil);
      AttachThreadInput(GetCurrentThreadId, Threadld, True);
      MyCursor.Handle := Getcursor();
      AttachThreadInput(GetCurrentThreadId, Threadld, False);
      GetIconInfo(MyCursor.Handle, pIconInfo);
      Cursorx := DrawPos.x - round(pIconInfo.xHotspot);
      Cursory := DrawPos.y - round(pIconInfo.yHotspot);
      Mybmp.Canvas.Draw(Cursorx, Cursory, MyCursor);
      DeleteObject(pIconInfo.hbmColor);
      DeleteObject(pIconInfo.hbmMask);
      MyCursor.ReleaseHandle;
      FreeAndNil(MyCursor);
    End;
    Try
      If vImageViewQ = tiv_MonoC Then
      Begin
        MakeGrey(Mybmp);
        Mybmp.PixelFormat := pf4bit;
      End
      Else
        Mybmp.PixelFormat := TPixelFormat(ImageViewQ);
    Except
      FreeAndNil(Mybmp);
    End;
  End;

Begin
  Result := '';
  Try
    If CompareFromDelphi Then
      ImageCatcher.PixelFormat := pf24bit
    Else
      ImageCatcher.PixelFormat := TPixelFormat(ImageViewQ);
    ImageCatcher.GetScreenShot;
    Result := BmpToString(ImageCatcher.Bitmap);
  Except
    Exit;
  End;
End;

procedure TdmCaptureScreen.ScreenCapture;
Var
  Mycan: TCanvas;
  dc: HDC;
  R: TRect;
  Mybmp: TBitmap;
  vListOfPictures: ^String;
  Procedure MakeGrey(Bitmap: TBitmap);
  Var
    w, h, y, x: Integer;
    sl: PRGBTripleArray;
    grey: Byte;
  Begin
    Bitmap.PixelFormat := pf32bit;
    w := Bitmap.Width;
    h := Bitmap.Height;
    For y := 0 To h - 1 Do
    Begin
      sl := Bitmap.ScanLine[y];
      For x := 0 To w - 1 Do
      Begin
        With sl[x] Do
        Begin
          grey := (B + G + R) div 3;
          B := grey;
          G := grey;
          R := grey;
        End;
      End;
    End;
  End;
  Function BmpToString: String;
  Var
    Stream: TStringStream;
  Begin
    Stream := TStringStream.Create;
    Try
      Mybmp.SaveToStream(Stream);
      Result := Stream.DataString;
    Finally
      FreeAndNil(Stream);
    End;
  End;
  Procedure SmoothResize(abmp: TBitmap; NuWidth, NuHeight: Integer);
  Var
    xscale, yscale, sfrom_y, sfrom_x, weight, total_red, total_green,
      total_blue: Single;
    ifrom_y, ifrom_x, to_y, to_x, new_red, new_green, new_blue, ix, iy, liSize,
      loSize: Integer;
    weight_x, weight_y: Array [0 .. 1] of Single;
    bTmp: TBitmap;
    sli, slo: pRGBArray;
    { pointers for scanline access }
    liPByte, loPByte, p: PByte;
  Begin
    abmp.PixelFormat := pf24bit;
    bTmp := TBitmap.Create;
    bTmp.PixelFormat := pf24bit;
    bTmp.Width := NuWidth;
    bTmp.Height := NuHeight;
    xscale := bTmp.Width / (abmp.Width - 1);
    yscale := bTmp.Height / (abmp.Height - 1);
    liPByte := abmp.ScanLine[0];
    liSize := Integer(abmp.ScanLine[1]) - Integer(liPByte);
    loPByte := bTmp.ScanLine[0];
    loSize := Integer(bTmp.ScanLine[1]) - Integer(loPByte);
    For to_y := 0 To (bTmp.Height - 1) Do
    Begin
      sfrom_y := (to_y / yscale);
      ifrom_y := Trunc(sfrom_y);
      weight_y[1] := (sfrom_y - ifrom_y);
      weight_y[0] := (1 - weight_y[1]);
      For to_x := 0 To bTmp.Width - 1 Do
      Begin
        sfrom_x := to_x / xscale;
        ifrom_x := Trunc(sfrom_x);
        weight_x[1] := sfrom_x - ifrom_x;
        weight_x[0] := 1 - weight_x[1];
        total_red := 0.0;
        total_green := 0.0;
        total_blue := 0.0;
        For ix := 0 To 1 Do
        Begin
          For iy := 0 To 1 Do
          Begin
            p := liPByte;
            Inc(p, liSize * (ifrom_y + iy));
            sli := pRGBArray(p);
            new_red := sli[ifrom_x + ix].rgbtRed;
            new_green := sli[ifrom_x + ix].rgbtGreen;
            new_blue := sli[ifrom_x + ix].rgbtBlue;
            weight := weight_x[ix] * weight_y[iy];
            total_red := total_red + new_red * weight;
            total_green := total_green + new_green * weight;
            total_blue := total_blue + new_blue * weight;
          End;
        End;
        p := loPByte;
        Inc(p, (loSize * to_y));
        slo := pRGBArray(p);
        slo[to_x].rgbtRed := round(total_red);
        slo[to_x].rgbtGreen := round(total_green);
        slo[to_x].rgbtBlue := round(total_blue);
      End;
    End;
    abmp.Width := bTmp.Width;
    abmp.Height := bTmp.Height;
    abmp.Canvas.Draw(0, 0, bTmp);
    bTmp.Free;
  End;
  Procedure GetScreenToBmp(Var Mybmp: TBitmap; Width, Height: Integer;
    DrawCur: Boolean = False);
  Var
    Cursorx, Cursory: Integer;
    dc: HDC;
    Mycan: TCanvas;
    R: TRect;
    DrawPos: TPoint;
    MyCursor: TIcon;
    hld: hWnd;
    Threadld: dword;
    mp: TPoint;
    pIconInfo: TIconInfo;
  Begin
    Mycan := TCanvas.Create;
    dc := GetWindowDC(0);
    Try
      Mycan.Handle := dc;
      R := Rect(0, 0, GetSystemMetrics(SM_CXSCREEN),
        GetSystemMetrics(SM_CYSCREEN));
      Mybmp.Width := R.Right;
      Mybmp.Height := R.Bottom;
      Mybmp.Canvas.CopyRect(R, Mycan, R);
    Finally
      ReleaseDC(0, dc);
    End;
    Mycan.Handle := 0;
    Mycan.Free;
    If DrawCur Then
    Begin
      GetCursorPos(DrawPos);
      MyCursor := TIcon.Create;
      GetCursorPos(mp);
      hld := WindowFromPoint(mp);
      Threadld := GetWindowThreadProcessId(hld, nil);
      AttachThreadInput(GetCurrentThreadId, Threadld, True);
      MyCursor.Handle := Getcursor();
      AttachThreadInput(GetCurrentThreadId, Threadld, False);
      GetIconInfo(MyCursor.Handle, pIconInfo);
      Cursorx := DrawPos.x - round(pIconInfo.xHotspot);
      Cursory := DrawPos.y - round(pIconInfo.yHotspot);
      Mybmp.Canvas.Draw(Cursorx, Cursory, MyCursor);
      DeleteObject(pIconInfo.hbmColor);
      DeleteObject(pIconInfo.hbmMask);
      MyCursor.ReleaseHandle;
      MyCursor.Free;
    End;
    If (bmpWidth > 0) And (bmpHeight > 0) Then
      SmoothResize(Mybmp, bmpWidth, bmpHeight); // , clBlack);
    If vImageViewQ = tiv_MonoC Then
    Begin
      MakeGrey(Mybmp);
      Mybmp.PixelFormat := pf4bit;
    End
    Else
      Mybmp.PixelFormat := TPixelFormat(ImageViewQ);
  End;

Begin
  Try
    Mybmp := TBitmap.Create;
    GetScreenToBmp(Mybmp, bmpWidth, bmpHeight, frm_Main.MouseCapture);
  Except
    FreeAndNil(Mybmp);
    Exit;
  End;
  New(vListOfPictures);
  vListOfPictures^ := BmpToString;
  ListOfPictures.Add(vListOfPictures);
  FreeAndNil(Mybmp);
End;

end.
