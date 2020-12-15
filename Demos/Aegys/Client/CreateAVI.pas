unit CreateAVI;

interface

uses
  Windows, Classes, Forms, Graphics, SysUtils, Dialogs, VfW, System.UITypes,
   System.Generics.Collections;

Type
 TListFrames = TList<TGraphic>;

type
  TResRestriction = (rrAny, rr2x2, rr4x4, rr8x8);
  TAVICreatorState = (csStopped, csWriting);

  TAVICreator = class(TComponent)
  private
    AVIBitmap: TBitmap;
    AVIFrameIndex: integer;
    pfile: IAVIFile;
    asi: TAVIStreamInfoW;
    ps, ps_c: IAVIStream;
    BitmapInfo: PBitmapInfoHeader;
    BitmapBits: Pointer;
    BitmapSize: DWORD;
    WorkFile: string;
    FFilename: string;
    FFPS: byte;
    FWidth: integer;
    FHeight: integer;
    FResRestriction: TResRestriction;
    CreatorState: TAVICreatorState;
    procedure SetHeight(const value: integer);
    procedure SetWidth(const value: integer);
    procedure SetResRestriction(const value: TResRestriction);
  protected
    function Restriction(s: integer): integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    Function   CreateAVIFile(CompressDialog : Boolean = False) : Boolean;
    Function   CreateAVIMemoryStream(Frames         : TListFrames  = Nil;
                                     DeleteTempFile : Boolean      = True;
                                     PixelFormat    : TPixelFormat = pf24Bit;
                                     CompressDialog : Boolean      = False;
                                     TempFileName   : String       = 'temp.avi') : TMemoryStream;
    procedure  AddAVIFrame(graphic: TGraphic);
    procedure  CloseAVIFile;
  published
    property FPS: byte read FFPS write FFPS default 25;
    property Width: integer read FWidth write SetWidth;
    property Height: integer read FHeight write SetHeight;
    property Filename: string read FFilename write FFilename;
    property ResRestriction: TResRestriction read FResRestriction write SetResRestriction default rr8x8;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TAVICreator]);
end;

procedure InitializeBitmapInfoHeader(Bitmap: HBITMAP; var BI: TBitmapInfoHeader);
var
  bmp: Windows.TBitmap;
begin
  GetObject(Bitmap, SizeOf(bmp), @bmp);
  with BI do
  begin
    biSize := SizeOf(BI);
    biWidth := bmp.bmWidth;
    biHeight := bmp.bmHeight;
    biPlanes := 1;
    biXPelsPerMeter := 0;
    biYPelsPerMeter := 0;
    biClrUsed := 0;
    biClrImportant := 0;
    biCompression := BI_RGB;
    biBitCount := 24;
    biSizeImage := (((biWidth * biBitCount) + 31) div 32) * 4 * biHeight;
  end;
end;

procedure InternalGetDIBSizes(Bitmap: HBITMAP; var InfoHeaderSize: Integer; var ImageSize: DWORD);
var
  BI: TBitmapInfoHeader;
begin
  InitializeBitmapInfoHeader(Bitmap, BI);
  with BI do
    InfoHeaderSize := SizeOf(TBitmapInfoHeader);
  ImageSize := BI.biSizeImage;
end;

function InternalGetDIB(Bitmap: HBITMAP; var BitmapInfo; var Bits): Boolean;
var
  Focus: HWND;
  DC: HDC;
begin
  InitializeBitmapInfoHeader(Bitmap, TBitmapInfoHeader(BitmapInfo));
  Focus := GetFocus;
  DC := GetDC(Focus);
  try
    Result := GetDIBits(DC, Bitmap, 0, TBitmapInfoHeader(BitmapInfo).biHeight, @Bits,
      TBitmapInfo(BitmapInfo), DIB_RGB_COLORS) <> 0;
  finally
    ReleaseDC(Focus, DC);
  end;
end;

constructor TAVICreator.Create(AOwner: TComponent);
begin
  inherited;
  FWidth := 320;
  FHeight := 240;
  FFPS := 25;
  CreatorState := csStopped;
  FResRestriction := rr8x8;
end;

destructor TAVICreator.Destroy;
begin
  inherited;
end;

function TAVICreator.Restriction(s: integer): integer;
begin
  case FResRestriction of
    rr2x2: result := (s div 2) * 2;
    rr4x4: result := (s div 4) * 4;
    rr8x8: result := (s div 8) * 8;
  else
    result := s;
  end;
end;

procedure TAVICreator.SetHeight(const value: integer);
begin
  if CreatorState = csWriting then exit;
  if value <> FHeight then
    if value > 0 then FHeight := Restriction(value);
end;

procedure TAVICreator.SetWidth(const value: integer);
begin
  if CreatorState = csWriting then exit;
  if value <> FWidth then
    if value > 0 then FWidth := Restriction(value);
end;

procedure TAVICreator.SetResRestriction(const value: TResRestriction);
begin
  if value <> FResRestriction then
  begin
    FResRestriction := value;
    FHeight := Restriction(FHeight);
    FWidth := Restriction(FWidth);
  end;
end;

procedure TAVICreator.AddAVIFrame(graphic: TGraphic);
begin
  if CreatorState <> csWriting then
    raise Exception.create('Não foi possível adicionar o Frame. O Arquivo AVI não foi inicializado.');
  Assert(graphic <> nil);
  with AVIBitmap do
  begin
    Canvas.Draw(0, 0, graphic);
    InternalGetDIB(Handle, BitmapInfo^, BitmapBits^);
    if AVIStreamWrite(ps_c, AVIFrameIndex, 1, BitmapBits, BitmapSize, AVIIF_KEYFRAME, nil, nil) <> AVIERR_OK then
      raise Exception.Create('Não foi possível adicionar o Frame');
    Inc(AVIFrameIndex);
  end;
end;

Function TAVICreator.CreateAVIMemoryStream(Frames         : TListFrames  = Nil;
                                           DeleteTempFile : Boolean      = True;
                                           PixelFormat    : TPixelFormat = pf24Bit;
                                           CompressDialog : Boolean      = False;
                                           TempFileName   : String       = 'temp.avi') : TMemoryStream;
Var
 gaAVIOptions   : TAVICOMPRESSOPTIONS;
 galpAVIOptions : PAVICOMPRESSOPTIONS;
 I,
 BitmapInfoSize : Integer;
 fileStream     : TfileStream;
begin
 Result    := TMemoryStream.Create;
 Try
  WorkFile := ExtractFilePath(ParamStr(0)) + TempFileName;
  FileName := WorkFile;
  if WorkFile = '' then
   Exit;
  If FileExists(WorkFile) Then
   DeleteFile(WorkFile);
  AVIFileInit;
  AVIBitmap     := TBitmap.Create;
  AVIFrameIndex := 0;
  CreatorState := csWriting;
  Try
   AVIBitmap.PixelFormat := PixelFormat;
   AVIBitmap.Width       := FWidth;
   AVIBitmap.Height      := FHeight;
   If AVIFileOpen(pfile, PChar(WorkFile), OF_WRITE or OF_CREATE, Nil) <> AVIERR_OK Then //OF_WRITE or
    Raise Exception.Create('Não foi possível criar um arquivo AVI . Disco cheio ou arquivo em uso?');
   With AVIBitmap do
    Begin
     InternalGetDIBSizes(Handle, BitmapInfoSize, BitmapSize);
     BitmapInfo := AllocMem(BitmapInfoSize);
     BitmapBits := AllocMem(BitmapSize);
     InternalGetDIB(Handle, BitmapInfo^, BitmapBits^);
    End;
   FillChar(asi, sizeof(asi), 0);
   With asi do
    Begin
     fccType := streamtypeVIDEO;
     fccHandler := 0;
     dwScale := 1;
     dwRate := FFPS;
     dwSuggestedBufferSize := BitmapSize;
     rcFrame.Right := BitmapInfo^.biWidth;
     rcFrame.Bottom := BitmapInfo^.biHeight;
    End;
   If AVIFileCreateStream(pfile, ps, asi) <> AVIERR_OK Then
    Raise Exception.Create('Não foi possível gerar fluxo de AVI.');
   With AVIBitmap Do
    InternalGetDIB(Handle, BitmapInfo^, BitmapBits^);
   galpAVIOptions := @gaAVIOptions;
   fillchar(gaAVIOptions, sizeof(gaAVIOptions), 0);
   if CompressDialog then
    AVISaveOptions(Application.Handle, ICMF_CHOOSE_KEYFRAME or ICMF_CHOOSE_DATARATE, 1, ps, galpAVIOptions);
   if AVIMakeCompressedStream(ps_c, ps, galpAVIOptions, nil) <> AVIERR_OK then
     raise Exception.Create('Não foi possível gerar um fluxo comprimido.');
   if AVIStreamSetFormat(ps_c, 0, BitmapInfo, BitmapInfoSize) <> AVIERR_OK then
     raise Exception.Create('AVIStreamSetFormat-Erro');
   For I := 0 to Frames.Count -1 do
    AddAVIFrame(Frames[I]);
   CloseAVIFile;
   fileStream := TfileStream.Create(WorkFile, fmOpenRead);
   Result.LoadFromStream(fileStream);
   Result.Position := 0;
   FreeAndNil(fileStream);
   if DeleteTempFile then
    DeleteFile(WorkFile);
  Except
   CloseAVIFile;
   DeleteFile(WorkFile);
  End;
 Finally
 End;
end;

function TAVICreator.CreateAVIFile(CompressDialog : Boolean = False) : boolean;
var
  gaAVIOptions: TAVICOMPRESSOPTIONS;
  galpAVIOptions: PAVICOMPRESSOPTIONS;
  BitmapInfoSize: Integer;
begin
  Result := false;
  WorkFile := FFilename;
  if WorkFile = '' then
    with TSaveDialog.Create(Application) do
    try
      Options := [ofHideReadOnly, ofNoReadOnlyReturn];
      DefaultExt := '.avi';
      Filter := 'AVI-Arquivos (*.avi)|*.avi';
      if not Execute then exit;
      WorkFile := FileName;
      FFilename := FileName;
      if (FileExists(FileName)) and (MessageDlg(Format('"%s" sobrescrever?', [FileName]), mtConfirmation, [mbYes, mbNo], 0) <> idyes) then exit;
    finally
      free;
    end;
  AVIFileInit;
  AVIBitmap := TBitmap.Create;
  AVIFrameIndex := 0;
  CreatorState := csWriting;
  try
    AVIBitmap.PixelFormat := pf24Bit;
    AVIBitmap.Width := FWidth;
    AVIBitmap.Height := FHeight;
    if AVIFileOpen(pfile, PChar(WorkFile), OF_WRITE or OF_CREATE, nil) <> AVIERR_OK then
      raise Exception.Create('Não foi possível criar um arquivo AVI . Disco cheio ou arquivo em uso?');
    with AVIBitmap do
    begin
      InternalGetDIBSizes(Handle, BitmapInfoSize, BitmapSize);
      BitmapInfo := AllocMem(BitmapInfoSize);
      BitmapBits := AllocMem(BitmapSize);
      InternalGetDIB(Handle, BitmapInfo^, BitmapBits^);
    end;
    FillChar(asi, sizeof(asi), 0);
    with asi do
    begin
      fccType := streamtypeVIDEO;
      fccHandler := 0;
      dwScale := 1;
      dwRate := FFPS;
      dwSuggestedBufferSize := BitmapSize;
      rcFrame.Right := BitmapInfo^.biWidth;
      rcFrame.Bottom := BitmapInfo^.biHeight;
    end;
    if AVIFileCreateStream(pfile, ps, asi) <> AVIERR_OK then raise Exception.Create('Não foi possível gerar fluxo de AVI.');
    with AVIBitmap do
      InternalGetDIB(Handle, BitmapInfo^, BitmapBits^);
    galpAVIOptions := @gaAVIOptions;
    fillchar(gaAVIOptions, sizeof(gaAVIOptions), 0);
    if CompressDialog then
     AVISaveOptions(Application.Handle, ICMF_CHOOSE_KEYFRAME or ICMF_CHOOSE_DATARATE, 1, ps, galpAVIOptions);
    if AVIMakeCompressedStream(ps_c, ps, galpAVIOptions, nil) <> AVIERR_OK then
      raise Exception.Create('Não foi possível gerar um fluxo comprimido.');
    if AVIStreamSetFormat(ps_c, 0, BitmapInfo, BitmapInfoSize) <> AVIERR_OK then
      raise Exception.Create('AVIStreamSetFormat-Erro');
  except
    CloseAVIFile;
    DeleteFile(WorkFile);
  end;
  Result := true;
end;

procedure TAVICreator.CloseAVIFile;
begin
  if CreatorState <> csWriting then
    raise Exception.create('Não foi possível fechar o Arquivo AVI. Arquivo AVI não foi criado .');

  AVIStreamRelease(ps);
  AVIStreamRelease(ps_c);
  AVIFileRelease(pfile);
  AVIFileExit;

  AVIBitmap.Free;
  FreeMem(BitmapInfo);
  FreeMem(BitmapBits);

  CreatorState := csStopped;
end;

end.

