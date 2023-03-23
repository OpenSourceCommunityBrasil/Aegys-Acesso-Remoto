unit Execute.DesktopDuplicationAPI;
{
  Desktop Duplication (c)2017 Execute SARL
  http://www.execute.fr
}
interface

uses
  Winapi.Windows,
  DX12.D3D11,
  DX12.D3DCommon,
  DX12.DXGI,
  DX12.DXGI1_2,
  Vcl.Graphics;

type
{$POINTERMATH ON} // Pointer[x]
  TDesktopDuplicationWrapper = class
  private
    FError: HRESULT;
  // D3D11
    FDevice: ID3D11Device;
    FContext: ID3D11DeviceContext;
    FFeatureLevel: TD3D_FEATURE_LEVEL;
  // DGI
    FOutput: TDXGI_OUTPUT_DESC;
    FDuplicate: IDXGIOutputDuplication;
    FTexture: ID3D11Texture2D;
  // update information
    FMetaData: array of Byte;
    FMoveRects: PDXGI_OUTDUPL_MOVE_RECT; // array of
    FMoveCount: Integer;
    FDirtyRects: PRECT; // array of
    FDirtyCount: Integer;
  public
    constructor Create;
    function GetFrame(aFullFrame : Boolean;
                      aMouse     : Boolean = False): Boolean;
    procedure DrawFrame(var Bitmap  : TBitmap;
                        PixelFormat : TPixelFormat = pf32Bit);
    property Error: HRESULT read FError;
    property MoveCount: Integer read FMoveCount;
    property MoveRects: PDXGI_OUTDUPL_MOVE_RECT read FMoveRects;
    property DirtyCount: Integer read FDirtyCount;
    property DirtyRects: PRect read FDirtyRects;
  end;

implementation

{ TDesktopDuplicationWrapper }

constructor TDesktopDuplicationWrapper.Create;
var
  GI: IDXGIDevice;
  GA: IDXGIAdapter;
  GO: IDXGIOutput;
  O1: IDXGIOutput1;
begin
  FError := D3D11CreateDevice(
    nil, // Default adapter
    D3D_DRIVER_TYPE_HARDWARE, // A hardware driver, which implements Direct3D features in hardware.
    0,
    Ord(D3D11_CREATE_DEVICE_SINGLETHREADED),
    nil, 0, // default feature
    D3D11_SDK_VERSION,
    FDevice,
    FFeatureLevel,
    FContext
  );
  if Failed(FError) then
    Exit;

  FError := FDevice.QueryInterface(IID_IDXGIDevice, GI);
  if Failed(FError) then
    Exit;

  FError := GI.GetParent(IID_IDXGIAdapter, Pointer(GA));
  if Failed(FError) then
    Exit;

  FError := GA.EnumOutputs(0, GO);
  if Failed(FError) then
    Exit;

  FError := GO.GetDesc(FOutput);
  if Failed(FError) then
    Exit;

  FError := GO.QueryInterface(IID_IDXGIOutput1, O1);
  if Failed(FError) then
    Exit;

  FError := O1.DuplicateOutput(FDevice, FDuplicate);
  if Failed(FError) then
    Exit;
end;

procedure TDesktopDuplicationWrapper.DrawFrame(var Bitmap: TBitmap;
                                               PixelFormat : TPixelFormat = pf32Bit);
var
  Desc: TD3D11_TEXTURE2D_DESC;
  Temp: ID3D11Texture2D;
  Resource: TD3D11_MAPPED_SUBRESOURCE;
  i: Integer;
  p: PByte;
  aBitmap : TBitmap;
begin
  FTexture.GetDesc(Desc);
  aBitmap := TBitmap.Create;
  if Bitmap = nil then
    Bitmap := TBitmap.Create;
  aBitmap.PixelFormat := pf32bit;
  aBitmap.SetSize(Desc.Width, Desc.Height);

  Desc.BindFlags := 0;
  Desc.CPUAccessFlags := Ord(D3D11_CPU_ACCESS_READ) or Ord(D3D11_CPU_ACCESS_WRITE);
  Desc.Usage := D3D11_USAGE_STAGING;
  Desc.MiscFlags := 0;

  //  READ/WRITE texture
  FError := FDevice.CreateTexture2D(@Desc, nil, Temp);
  if Failed(FError) then
    Exit;

  // copy original to the RW texture
  FContext.CopyResource(Temp, FTexture);

  // get texture bits
  FContext.Map(Temp, 0, D3D11_MAP_READ_WRITE, 0, Resource);
  p := Resource.pData;

  // copy pixels - we assume a 32bits bitmap !
  for i := 0 to Desc.Height - 1 do
  begin
    Move(p^, aBitmap.ScanLine[i]^, 4 * Desc.Width);
    Inc(p, 4 * Desc.Width);
  end;
  Bitmap.SetSize(Desc.Width, Desc.Height);
  Bitmap.PixelFormat := PixelFormat;
  BitBlt(BitMap.Canvas.Handle, 0, 0, Desc.Width, Desc.Height, aBitmap.Canvas.Handle, 0, 0, SRCCOPY);
  aBitmap.Free;
  FTexture := nil;
  FDuplicate.ReleaseFrame;
end;

Function TDesktopDuplicationWrapper.GetFrame(aFullFrame : Boolean;
                                             aMouse     : Boolean = False) : Boolean;
Var
 FrameInfo: TDXGI_OUTDUPL_FRAME_INFO;
 Resource: IDXGIResource;
 BufLen : Integer;
 BufSize: Uint;
Begin
 Result := False;
 If (FTexture <> Nil) Then
  Begin
   FTexture := nil;
   FDuplicate.ReleaseFrame;
  End
 Else If aFullFrame Then
  FDuplicate.ReleaseFrame;
 FError := FDuplicate.AcquireNextFrame(0, FrameInfo, Resource);
 If Failed(FError) Then
  Exit;
 While True Do
  Begin
   If FrameInfo.TotalMetadataBufferSize > 0 Then
    Begin
     FError      := Resource.QueryInterface(IID_ID3D11Texture2D, FTexture);
     If failed(FError) Then
      Exit;
     Resource    := Nil;
     BufLen      := FrameInfo.TotalMetadataBufferSize;
     If Length(FMetaData) < BufLen Then
      SetLength(FMetaData, BufLen);
     FMoveRects  := Pointer(FMetaData);
     FError      := FDuplicate.GetFrameMoveRects(BufLen, FMoveRects, BufSize);
     If Failed(FError) Then
      Exit;
     FMoveCount  := BufSize div sizeof(TDXGI_OUTDUPL_MOVE_RECT);
     FDirtyRects := @FMetaData[BufSize];
     Dec(BufLen, BufSize);
     FError := FDuplicate.GetFrameDirtyRects(BufLen, FDirtyRects, BufSize);
     If Failed(FError) Then
      Exit;
     FDirtyCount := BufSize div sizeof(TRECT);
     Result      := True;
    End
   Else
    Begin
     If aFullFrame Then
      Begin
       FError    := FDuplicate.AcquireNextFrame(0, FrameInfo, Resource);
       If Failed(FError) Then
        Exit;
      End
     Else
      Begin
       FDuplicate.ReleaseFrame;
       Break;
      End;
    End;
   If Result Then
    Break;
  End;
End;

end.
