library AegysData;

uses
  System.SysUtils, System.Classes,
  Windows, Variants,
  Vcl.Forms, Vcl.Graphics,
  Winapi.Wincodec, Winapi.ActiveX;

Const
 cJPGQual = 20;

Var
 BitsPerPixel : Byte;

{$R *.res}

Function CaptureScreen : TStream; StdCall; Export;
Var
 Bitmap    : TBitmap;
 hdcScreen : HDC;
 Procedure SaveBitmapAsJpeg(ImageQuality : Single;
                            Var Result   : TStream);
 Const
  PROPBAG2_TYPE_DATA = 1;
 Var
  ImagingFactory : IWICImagingFactory;
  Width,
  Height         : Integer;
  LoadStream     : IStream;
  Stream         : IWICStream;
  Encoder        : IWICBitmapEncoder;
  Frame          : IWICBitmapFrameEncode;
  PropBag        : IPropertyBag2;
  PropBagOptions : TPropBag2;
  V              : Variant;
  PixelFormat    : TGUID;
  Buffer         : TBytes;
  BitmapInfo     : TBitmapInfo;
  hBmp           : HBITMAP;
  WICBitmap      : IWICBitmap;
  Rect           : WICRect;
  aStreamAdapter : TStreamAdapter;
 Begin
  Try
   Width                           := Bitmap.Width;
   Height                          := Bitmap.Height;
   CoCreateInstance         (CLSID_WICImagingFactory, Nil,
                             CLSCTX_INPROC_SERVER     Or
                             CLSCTX_LOCAL_SERVER,
                             IUnknown, ImagingFactory);
   ImagingFactory.CreateStream(Stream);
   aStreamAdapter                  := TStreamAdapter.Create(Result);
   LoadStream                      := aStreamAdapter;
   Stream.InitializeFromIStream(LoadStream);
   ImagingFactory.CreateEncoder(GUID_ContainerFormatJpeg, GUID_NULL, Encoder);
   Encoder.Initialize(Stream, WICBitmapEncoderNoCache);
   Application.ProcessMessages;
   Encoder.CreateNewFrame(Frame, PropBag);
   PropBagOptions                  := Default(TPropBag2);
   PropBagOptions.pstrName         := 'ImageQuality';
   PropBagOptions.dwType           := PROPBAG2_TYPE_DATA;
   PropBagOptions.vt               := VT_R4;
   V := VarAsType(0.01 * ImageQuality, varSingle);
   PropBag.Write(1, @PropBagOptions, @V);
   Frame.Initialize(PropBag);
   Frame.SetSize(Width, Height);
   If Bitmap.AlphaFormat = afDefined Then
    PixelFormat                    := GUID_WICPixelFormat32bppBGRA
   Else
    PixelFormat                    := GUID_WICPixelFormat32bppBGR;
   Bitmap.PixelFormat              := pf32bit;
   SetLength(Buffer, 4 * Width*Height);
   BitmapInfo                      := Default(TBitmapInfo);
   BitmapInfo.bmiHeader.biSize     := SizeOf(BitmapInfo);
   BitmapInfo.bmiHeader.biWidth    := Width;
   BitmapInfo.bmiHeader.biHeight   := -Height;
   BitmapInfo.bmiHeader.biPlanes   := 1;
   BitmapInfo.bmiHeader.biBitCount := 32;
   hBmp                            := Bitmap.Handle;
   GetDIBits(Bitmap.Canvas.Handle, hBmp, 0, Height, @Buffer[0], BitmapInfo, DIB_RGB_COLORS);
   ImagingFactory.CreateBitmapFromMemory(Width, Height, PixelFormat, 4 * Width,
                                         Length(Buffer), @Buffer[0], WICBitmap);
   Rect.X                          := 0;
   Rect.Y                          := 0;
   Rect.Width                      := Width;
   Rect.Height                     := Height;
   Frame.WriteSource(WICBitmap, @Rect);
   Frame.Commit;
   Encoder.Commit;
   Application.ProcessMessages;
   SetLength(Buffer, 0);
  Finally
   Encoder        := Nil;
   ImagingFactory := Nil;
   ReleaseDC(0, hBmp);
  End;
 End;
Begin
 Bitmap      := TBitmap.Create;
 Result      := TMemoryStream.Create;
 CoInitializeEx(Nil, COINIT_MULTITHREADED);
 Try
  hdcScreen  := GetDC(0);
  Bitmap.SetSize(Screen.Width, Screen.Height);
  Case BitsPerPixel Of
   8  : Bitmap.PixelFormat := pf8bit;
   16 : Bitmap.PixelFormat := pf16bit;
   24 : Bitmap.PixelFormat := pf24bit;
   32 : Bitmap.PixelFormat := pf32bit;
  End;
  bitblt(Bitmap.Canvas.Handle, 0 ,0 , screen.Width, screen.Height, hdcScreen, 0, 0, SRCCOPY);
  SaveBitmapAsJpeg(cJPGQual, Result);
 Finally
  ReleaseDC (0, hdcScreen);
  Bitmap.FreeImage;
  FreeAndNil(Bitmap);
  Application.ProcessMessages;
  CoUninitialize();
 End;
End;

Exports
 CaptureScreen;

procedure DLLMain(dwReason: DWORD);
Begin
  case dwReason of
  DLL_PROCESS_ATTACH:
    begin
     BitsPerPixel           := GetDeviceCaps(GetDC(0), BITSPIXEL);
    end {= DLL_PROCESS_ATTACH =};
  DLL_PROCESS_DETACH:
    Begin
    End{= DLL_PROCESS_DETACH =};
  end {= case =};
End {= DLLMain =};

Begin
 DLLProc := @DLLMain;
 DLLMain(DLL_PROCESS_ATTACH);
end.

