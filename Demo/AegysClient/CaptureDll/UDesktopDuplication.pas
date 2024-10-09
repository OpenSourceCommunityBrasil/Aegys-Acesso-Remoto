unit UDesktopDuplication;

interface

uses
  System.Classes,
  System.Types,
  System.SysUtils,
  Winapi.Windows,
  ActiveX,
  DX12.DXGI,
  DX12.DXGI1_2,
  DX12.D3DCommon,
  DX12.D3D11,
  FMX.Graphics,
  FMX.Surfaces,
  FMX.Forms;

type
  TVCLDesktopDuplication = class(TObject)
  private
    vFeatureLevel: TD3D_FEATURE_LEVEL;
    vOutputDuplication: IDXGIOutputDuplication;
    vDX11Deice: ID3D11Device;
    vDX11DeviceContext: ID3D11DeviceContext;
  public
    vImageStream: TMemoryStream;
    vActiveMonitor: Integer;
    vBitmapSurface: TBitmapSurface;
    vCodecManager: TBitmapCodecManager;
    vQualityParam: TBitmapCodecSaveParams;
    constructor CreateParamMonitor(AMonitorNum: Integer);
    destructor Destroy; override;
    function FrameGet(quality: Integer = 1): boolean;Overload;
    function FrameGet(Var Stream : TStream): boolean;Overload;
    property ImageStream: TMemoryStream read vImageStream;
  end;

implementation

const
 cConvertType = 'DELPHI';
//  cConvertType = 'SKIA';

constructor TVCLDesktopDuplication.CreateParamMonitor(AMonitorNum: Integer);
var
  vDXGIDevice: IDXGIDevice;
  vDXGIAdapter: IDXGIAdapter;
  vDXGIOutput: IDXGIOutput;
  vDXGIOutput1: IDXGIOutput1;
begin
  vImageStream := nil;
  vBitmapSurface := nil;
  vCodecManager := nil;

  vImageStream := TMemoryStream.Create;
  vBitmapSurface := TBitmapSurface.Create;
  vCodecManager := TBitmapCodecManager.Create;

  vActiveMonitor := AMonitorNum;

  if Failed(D3D11CreateDevice(nil, D3D_DRIVER_TYPE_HARDWARE, 0,
    Ord(D3D11_CREATE_DEVICE_SINGLETHREADED), nil, 0, D3D11_SDK_VERSION, vDX11Deice,
    vFeatureLevel, @vDX11DeviceContext)) then
    Exit;

  if Failed(vDX11Deice.QueryInterface(IID_IDXGIDevice, vDXGIDevice)) then
    Exit;

  if Failed(vDXGIDevice.GetParent(IID_IDXGIAdapter, Pointer(vDXGIAdapter))) then
    Exit;

  if Failed(vDXGIAdapter.EnumOutputs(AMonitorNum, vDXGIOutput)) then
    Exit;

  if Failed(vDXGIOutput.QueryInterface(IID_IDXGIOutput1, vDXGIOutput1)) then
    Exit;

  if Failed(vDXGIOutput1.DuplicateOutput(vDX11Deice, vOutputDuplication)) then
    Exit;
end;

destructor TVCLDesktopDuplication.Destroy;
begin
  FreeAndNil(vImageStream);

  FreeAndNil(vBitmapSurface);

  FreeAndNil(vCodecManager);

  inherited;
end;

function TVCLDesktopDuplication.FrameGet(quality: Integer = 1): boolean;
var
  i: Integer;
  vMappedResourceBytes: PByte;
  vTextureDesc: TD3D11_TEXTURE2D_DESC;
  vTexture2D, vTexture2DTemp: ID3D11Texture2D;
  vMappedResource: TD3D11_MAPPED_SUBRESOURCE;
  vDXGIResource: IDXGIResource;
  vDXGIFrameInfo: TDXGI_OUTDUPL_FRAME_INFO;
begin
  try
    Result := false;
    vQualityParam.quality := quality;

    vOutputDuplication.ReleaseFrame;

    if Failed(vOutputDuplication.AcquireNextFrame(33, vDXGIFrameInfo, vDXGIResource)) then
      Exit;

    if vDXGIFrameInfo.TotalMetadataBufferSize > 0 then
    begin
      if Failed(vDXGIResource.QueryInterface(IID_ID3D11Texture2D, vTexture2D)) then
        Exit;

      vTexture2D.GetDesc(vTextureDesc);

      vTextureDesc.BindFlags := 0;

      vTextureDesc.CPUAccessFlags := Ord(D3D11_CPU_ACCESS_READ) or
        Ord(D3D11_CPU_ACCESS_WRITE);

      vTextureDesc.Usage := D3D11_USAGE_STAGING;

      vTextureDesc.MiscFlags := 0;

      if Failed(vDX11Deice.CreateTexture2D(vTextureDesc, nil, vTexture2DTemp)) then
        Exit;

      vDX11DeviceContext.CopyResource(vTexture2DTemp, vTexture2D);

      vDX11DeviceContext.Map(vTexture2DTemp, 0, D3D11_MAP_READ_WRITE, 0, vMappedResource);
      vMappedResourceBytes := vMappedResource.pData;
      vBitmapSurface.SetSize(vTextureDesc.Width, vTextureDesc.Height);
      for i := 0 to vTextureDesc.Height - 1 do
       begin
        Move(vMappedResourceBytes^, vBitmapSurface.ScanLine[i]^, vMappedResource.RowPitch);
        Inc(vMappedResourceBytes, vMappedResource.RowPitch);
       end;
      vImageStream.Clear;
      CoInitializeEx(nil, COINIT_MULTITHREADED);
      Try
       vCodecManager.SaveToStream(vImageStream, vBitmapSurface, '.bmp', @vQualityParam);
      Finally
       CoUninitialize();
      End;
      vImageStream.Position := 0;
      Result := true;
    end;
  except
    Result := false;
  end;
end;

function TVCLDesktopDuplication.FrameGet(Var Stream : TStream): boolean;
var
  i: Integer;
  vMappedResourceBytes: PByte;
  vTextureDesc: TD3D11_TEXTURE2D_DESC;
  vTexture2D, vTexture2DTemp: ID3D11Texture2D;
  vMappedResource: TD3D11_MAPPED_SUBRESOURCE;
  vDXGIResource: IDXGIResource;
  vDXGIFrameInfo: TDXGI_OUTDUPL_FRAME_INFO;
begin
  try
    Result := false;
    vQualityParam.quality := 8;

    vOutputDuplication.ReleaseFrame;

    if Failed(vOutputDuplication.AcquireNextFrame(1, vDXGIFrameInfo, vDXGIResource)) then
      Exit;

    if vDXGIFrameInfo.TotalMetadataBufferSize > 0 then
    begin
      if Failed(vDXGIResource.QueryInterface(IID_ID3D11Texture2D, vTexture2D)) then
        Exit;

      vTexture2D.GetDesc(vTextureDesc);

      vTextureDesc.BindFlags := 0;

      vTextureDesc.CPUAccessFlags := Ord(D3D11_CPU_ACCESS_READ) or
        Ord(D3D11_CPU_ACCESS_WRITE);

      vTextureDesc.Usage := D3D11_USAGE_STAGING;

      vTextureDesc.MiscFlags := 0;

      if Failed(vDX11Deice.CreateTexture2D(vTextureDesc, nil, vTexture2DTemp)) then
        Exit;

      vDX11DeviceContext.CopyResource(vTexture2DTemp, vTexture2D);

      vDX11DeviceContext.Map(vTexture2DTemp, 0, D3D11_MAP_READ_WRITE, 0, vMappedResource);
      vMappedResourceBytes := vMappedResource.pData;
      vBitmapSurface.SetSize(vTextureDesc.Width, vTextureDesc.Height);
      for i := 0 to vTextureDesc.Height - 1 do
       begin
        Move(vMappedResourceBytes^, vBitmapSurface.ScanLine[i]^, vMappedResource.RowPitch);
        Inc(vMappedResourceBytes, vMappedResource.RowPitch);
        Application.Processmessages;
       end;
      TMemoryStream(Stream).Clear;
      CoInitializeEx(nil, COINIT_MULTITHREADED);
      Try
       vCodecManager.SaveToStream(Stream, vBitmapSurface, '.bmp', @vQualityParam);
       Application.Processmessages;
      Finally
       CoUninitialize();
      End;
      Stream.Position := 0;
      Result := true;
    end;
  except
    Result := false;
  end;
end;

end.
