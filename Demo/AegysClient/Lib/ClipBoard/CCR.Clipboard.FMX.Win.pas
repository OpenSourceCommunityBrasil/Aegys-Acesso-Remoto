{**************************************************************************************}
{                                                                                      }
{ CCR.Clipboard - FMX-specific extensions to generic Windows backend                   }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 2.0      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at https://www.mozilla.org/MPL/2.0              }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2012-15 Chris Rolliston. All Rights Reserved.      }
{                                                                                      }
{**************************************************************************************}

unit CCR.Clipboard.FMX.Win;

interface

{$POINTERMATH ON}
{$WARN SYMBOL_PLATFORM OFF}

{$IF DEFINED(MSWINDOWS)}
uses
  WinAPI.Windows, WinAPI.ActiveX, WinAPI.ShlObj, WinAPI.ShellApi, System.Win.ComObj,
  System.Types, System.SysUtils, System.Classes, System.Math, System.StrUtils,
  System.Rtti, System.Generics.Collections, System.UITypes, System.UIConsts,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Platform, FMX.Platform.Win,
  {$IF NOT DECLARED(TPixelFormat) AND (RTLVersion > 23)}FMX.PixelFormats, {$IFEND}
  CCR.Clipboard, CCR.Clipboard.Consts, CCR.Clipboard.Win, CCR.Clipboard.FMX;

type
  TClipboardHelper = class helper(CCR.Clipboard.FMX.TClipboardHelper) for TClipboard
  public
    class function CreateForDataObject(const AObj: IDataObject;
      DummyChangeCountBehaviour: TDummyChangeCountBehaviour = dbIncrementing): TClipboard;
    procedure EnumDataObject(const Callback: TEnumDataObjectCallback);
    function GetAsHandle(const Format: TClipboardFormat): THandle;
    procedure SetAsHandle(const Format: TClipboardFormat; Value: THandle); overload;
    procedure SetAsHandleDelayed(const Format: TClipboardFormat; const ValueGetter: TFunc<THandle>); overload;
  end;

  TCreateDDBOptions = set of (cdForceOpaque, cdRemovePureBlack, cdUnPreMultiply);

procedure GetDIB5Header(const Bitmap: TBitmap; var Header: TBitmapV5Header);
procedure GetDIB5Bits(const Bitmap: TBitmap; Options: TCreateDDBOptions; var Buffer);
function CreateDDB(const Bitmap: TBitmap; Options: TCreateDDBOptions = []): HBITMAP;
procedure LoadBitmapFromDDB(Bitmap: TBitmap; Handle: HBITMAP);
procedure LoadBitmapFromDIB5HGlobal(Bitmap: TBitmap; Handle: HGLOBAL);
{$IFEND}

implementation

{$IF DEFINED(MSWINDOWS)}

{$IF RTLVersion >= 28}
type
  TWinClipboardDragDrop = class(TInterfacedObject, IFMXClipboardDragDropService)
  strict private
    FFMXDropTargetAtom: PChar;
    FWinDropTargetField: TRttiField;
  protected
    procedure DoDrag(const Source: TObject; const Form: TCommonCustomForm;
      const Bitmap: TBitmap; const PrepareClipboard: TProc<TClipboard>);
    function RegisterForm(const Form: TCommonCustomForm): IClipboardDropInfo;
    procedure UnregisterForm(const Form: TCommonCustomForm);
  end;

  TClipboardDropTarget = class(FMX.Platform.Win.TWinDropTarget, IDropTarget, IClipboardDropInfo,
    IWinClipboardDropInfo)
  strict private
    FClipboard: TClipboard;
    FDropTargetHelper: IDropTargetHelper;
    FEffect: DWORD;
    FEffectSet: Boolean;
    FFMXDataObject: FMX.Types.TDragObject;
    [Weak] FForm: TCommonCustomForm;
  protected
    { IDropTarget }
    function DragEnter(const dataObj: IDataObject; grfKeyState: LongInt;
      pt: TPoint; var dwEffect: LongInt): HRESULT; stdcall;
    function DragOver(grfKeyState: LongInt; pt: TPoint; var dwEffect: LongInt): HRESULT; stdcall;
    function DragLeave: HRESULT; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: LongInt; pt: TPoint;
      var dwEffect: LongInt): HRESULT; stdcall;
    { IClipboardDropInfo }
    function GetClipboard: TClipboard;
    { IWinClipboardDropInfo }
    procedure SetEffect(Effect: DWORD);
  public
    constructor Create(const AForm: TCommonCustomForm); reintroduce;
  end;
{$IFEND}

{ DIB5 helpers }

procedure GetDIB5Header(const Bitmap: TBitmap; var Header: TBitmapV5Header); overload;
begin
  Header.bV5Size := SizeOf(TBitmapV5Header);
  Header.bV5Planes := 1;
  Header.bV5Width := Max(1, Bitmap.Width);
  Header.bV5Height := -Max(1, Bitmap.Height); //top-down DIB
  Header.bV5SizeImage := Bitmap.Width * 4 * Bitmap.Height;
  Header.bV5Compression := BI_BITFIELDS;
  Header.bV5BitCount := 32;
  Header.bV5RedMask   := $00FF0000;
  Header.bV5GreenMask := $0000FF00;
  Header.bV5BlueMask  := $000000FF;
  Header.bV5AlphaMask := $FF000000;
end;

function MakeOpaque(Color: TAlphaColor): TAlphaColor; inline;
begin
  Result := Color or TAlphaColors.Alpha;
end;

function IsTransparent(Color: TAlphaColor): Boolean; inline;
begin
  Result := TAlphaColorRec(Color).A = TAlphaColors.Null;
end;

procedure GetDIB5Bits(const Bitmap: TBitmap; Options: TCreateDDBOptions; var Buffer);
var
  DestPitch: Integer;
  DestLine: PAlphaColor;
  R: TRectF;
  Source: TBitmap;
  SourceData: TBitmapData;
  SourceLine: PAlphaColor;
  X, Y: Integer;
begin
  DestPitch := Bitmap.Width * 4;
  DestLine := @Buffer;
  Source := Bitmap;
  try
    if cdForceOpaque in Options then
    begin
      Source := TBitmap.Create(Bitmap.Width, Bitmap.Height);
      Source.Canvas.BeginScene;
      try
        Source.Canvas.Clear(TAlphaColors.White);
        R := RectF(0, 0, Source.Width, Source.Height);
        Source.Canvas.DrawBitmap(Bitmap, R, R, 1);
      finally
        Source.Canvas.EndScene;
      end;
      Exclude(Options, cdUnPreMultiply); //nothing to un-premultiply when opaque
    end;
    if not Source.Map(TMapAccess.Read, SourceData) then
      raise EInvalidOperation.CreateRes(@SCannotMapBitmap);
    try
      SourceLine := SourceData.Data;
      for Y := 0 to Source.Height - 1 do
      begin
        if not (cdUnPreMultiply in Options) then Move(SourceLine^, DestLine^, DestPitch);
        if Options * [cdRemovePureBlack, cdUnPreMultiply] <> [] then
        begin
          for X := 0 to Source.Width - 1 do
          begin
            DestLine[X] := UnpremultiplyAlpha(SourceLine[X]);
            if (cdRemovePureBlack in Options) and not IsTransparent(DestLine[X]) and
               (MakeOpaque(DestLine[X]) = claBlack) then
              DestLine[X] := (DestLine[X] and TAlphaColors.Alpha) or $0A0A0A;
            if not (cdUnPreMultiply in Options) then DestLine[X] := PremultiplyAlpha(DestLine[X]);
          end;
        end;
        Inc(PByte(SourceLine), SourceData.Pitch);
        Inc(PByte(DestLine), DestPitch);
      end;
    finally
      Source.Unmap(SourceData);
    end;
  finally
    if Source <> Bitmap then Source.Free;
  end;
end;

function CreateDDB(const Bitmap: TBitmap; Options: TCreateDDBOptions = []): HBITMAP;
var
  Bits: TBytes;
  DC: HDC;
  Header: TBitmapV5Header;
begin
  GetDIB5Header(Bitmap, Header);
  SetLength(Bits, Bitmap.Width * 4 * Bitmap.Height);
  GetDIB5Bits(Bitmap, Options, Bits[0]);
  DC := GetDC(0);
  try
    Result := CreateDIBitmap(DC, PBitmapInfoHeader(@Header)^, CBM_INIT,
      @Bits[0], PBitmapInfo(@Header)^, DIB_RGB_COLORS);
  finally
    ReleaseDC(0, DC);
  end;
end;

function CreateDIB5Handle(const Bitmap: TBitmap): THandle;
var
  DestPtr: PBitmapV5Header;
  DestSize: Integer;
begin
  DestSize := SizeOf(TBitmapV5Header) + Bitmap.Width * 4 * Bitmap.Height;
  Result := GlobalAlloc(GMEM_MOVEABLE, DestSize);
  try
    DestPtr := GlobalLock(Result);
    if DestPtr = nil then RaiseLastOSError;
    try
      GetDIB5Header(Bitmap, DestPtr^);
      Inc(DestPtr);
      GetDIB5Bits(Bitmap, [cdForceOpaque], DestPtr^);
    finally
      GlobalUnlock(Result);
    end;
  except
    GlobalFree(Result);
    raise;
  end;
end;

function TryLoadBitmapFromDDB(Dest: TBitmap; const Handle: HBITMAP): Boolean;
var
  DC: HDC;
  Info: TBitmapInfo;
  MapData: TBitmapData;
  Rec: tagBITMAP;
begin
  Result := (Handle <> 0);
  if not Result then Exit;
  GetObject(Handle, SizeOf(Rec), @Rec);
  ZeroMemory(@Info, SizeOf(Info));
  Info.bmiHeader.biSize := SizeOf(Info.bmiHeader);
  Info.bmiHeader.biPlanes := 1;
  Info.bmiHeader.biWidth := Max(1, Rec.bmWidth);
  Info.bmiHeader.biHeight := -Max(1, Rec.bmHeight); //top-down DIB
  Info.bmiHeader.biSizeImage := Rec.bmWidth * 4 * Rec.bmHeight;
  Info.bmiHeader.biCompression := BI_RGB;
  Info.bmiHeader.biBitCount := 32;
  DC := GetDC(0);
  try
    Dest.SetSize(Rec.bmWidth, Rec.bmHeight);
    Dest.Map(TMapAccess.Write, MapData);
    try
      GetDIBits(DC, Handle, 0, Dest.Height, MapData.Data, Info, DIB_RGB_COLORS);
    finally
      Dest.Unmap(MapData);
    end;
  finally
    ReleaseDC(0, DC)
  end;
end;

function TryLoadBitmapFromDIB5HGlobal(Bitmap: TBitmap; const DIBMemHandle: HGLOBAL): Boolean;
var
  BitmapInfoPtr: PBitmapV5Header;
  Header: TBitmapFileHeader;
  Stream: TMemoryStream;
begin
  BitmapInfoPtr := GlobalLock(DIBMemHandle);
  Result := (BitmapInfoPtr <> nil) and (GlobalSize(DIBMemHandle) > SizeOf(TBitmapV5Header));
  if not Result then Exit;
  Stream := TMemoryStream.Create;
  try
    FillChar(Header, SizeOf(Header), 0);
    Header.bfType := $4D42;
    Header.bfSize := SizeOf(Header) + GlobalSize(DIBMemHandle);
    Header.bfOffBits := SizeOf(Header) + BitmapInfoPtr.bV5Size;
    Stream.WriteBuffer(Header, SizeOf(Header));
    Stream.WriteBuffer(BitmapInfoPtr^, Header.bfSize - SizeOf(Header));
    Stream.Position := 0;
    Bitmap.LoadFromStream(Stream);
    Result := True;
  finally
    GlobalUnlock(DIBMemHandle);
    Stream.Free;
  end;
end;

procedure LoadBitmapFromDDB(Bitmap: TBitmap; Handle: HBITMAP);
begin
  if not TryLoadBitmapFromDDB(Bitmap, Handle) then
    RaiseLastOSError;
end;

procedure LoadBitmapFromDIB5HGlobal(Bitmap: TBitmap; Handle: HGLOBAL);
begin
  if not TryLoadBitmapFromDIB5HGlobal(Bitmap, Handle) then
    RaiseLastOSError;
end;

{ TClipboardHelper }

class function TClipboardHelper.CreateForDataObject(const AObj: IDataObject;
  DummyChangeCountBehaviour: TDummyChangeCountBehaviour): TClipboard;
begin
  Result := TWinClipboardCore.CreateClipboardForDataObject(AObj, DummyChangeCountBehaviour);
end;

procedure TClipboardHelper.EnumDataObject(const Callback: TEnumDataObjectCallback);
begin
  Open;
  try
    (Core as IWinClipboardCore).EnumDataObject(Callback);
  finally
    Close;
  end;
end;

function TClipboardHelper.GetAsHandle(const Format: TClipboardFormat): THandle;
begin
  Open;
  try
    Result := (Core as IWinClipboardCore).DoGetAsHandle(Format);
  finally
    Close;
  end;
end;

procedure TClipboardHelper.SetAsHandle(const Format: TClipboardFormat; Value: THandle);
begin
  Add(procedure
      begin
        (Core as IWinClipboardCore).DoSetAsHandle(Format, Value);
      end);
end;

procedure TClipboardHelper.SetAsHandleDelayed(const Format: TClipboardFormat;
  const ValueGetter: TFunc<THandle>);
begin
  Add(procedure
      begin
        (Core as IWinClipboardCore).DoSetAsHandleDelayed(Format, ValueGetter);
      end);
end;

{$IF DECLARED(TWinClipboardDragDrop)}

{ TClipboardDropTarget }

constructor TClipboardDropTarget.Create(const AForm: TCommonCustomForm);
begin
  inherited Create(nil);
  FForm := AForm;
end;

function TClipboardDropTarget.DragEnter(const dataObj: IDataObject; grfKeyState: LongInt;
  pt: TPoint; var dwEffect: LongInt): HRESULT;
begin
  try
    FClipboard := TClipboard.CreateForDataObject(dataObj, dbFixed);
    FClipboard.InitializeDragObject(FFMXDataObject);
    FEffect := DROPEFFECT_NONE;
    FForm.DragEnter(FFMXDataObject, pt);
    DWORD(dwEffect) := FEffect;
    if (Succeeded(CoCreateInstance(CLSID_DragDropHelper, nil, CLSCTX_INPROC_SERVER, IDropTargetHelper,
      FDropTargetHelper))) and (FDropTargetHelper <> nil) then
    begin
      Result := FDropTargetHelper.DragEnter(FormToHWND(FForm), dataObj, pt, dwEffect);
      if Failed(Result) then FDropTargetHelper := nil;
    end;
    Result := S_OK;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TClipboardDropTarget.DragOver(grfKeyState: LongInt; pt: TPoint; var dwEffect: LongInt): HRESULT;
var
  P: TPointF;
  Operation: TDragOperation;
begin
  try
    FEffectSet := False;
    P := PointF(pt.X, pt.Y);
    Operation := TDragOperation.None;
    FForm.DragOver(FFMXDataObject, P, Operation);
    if not FEffectSet then
      case Operation of
        TDragOperation.None: FEffect := DROPEFFECT_NONE;
        TDragOperation.Move: FEffect := DROPEFFECT_MOVE;
        TDragOperation.Copy: FEffect := DROPEFFECT_COPY;
        TDragOperation.Link: FEffect := DROPEFFECT_LINK;
      else Assert(False);
      end;
    DWORD(dwEffect) := FEffect;
    if FDropTargetHelper <> nil then FDropTargetHelper.DragOver(pt, dwEffect);
    Result := S_OK;
  except
    dwEffect := DROPEFFECT_NONE;
    Result := E_UNEXPECTED;
  end;
end;

function TClipboardDropTarget.DragLeave: HRESULT;
begin
  try
    try
      FForm.DragLeave;
      if FDropTargetHelper <> nil then FDropTargetHelper.DragLeave;
      FDropTargetHelper := nil;
      Result := S_OK;
    finally
      FreeAndNil(FClipboard);
    end;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TClipboardDropTarget.Drop(const dataObj: IDataObject; grfKeyState: LongInt; pt: TPoint;
  var dwEffect: LongInt): HRESULT;
var
  P: TPointF;
begin
  try
    try
      P := PointF(pt.X, pt.Y);
      FForm.DragDrop(FFMXDataObject, P);
      DWORD(dwEffect) := FEffect;
      if FDropTargetHelper <> nil then FDropTargetHelper.Drop(dataObj, pt, dwEffect);
      Result := S_OK;
    finally
      FDropTargetHelper := nil;
      FreeAndNil(FClipboard);
    end;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TClipboardDropTarget.GetClipboard: TClipboard;
begin
  Result := FClipboard;
end;

procedure TClipboardDropTarget.SetEffect(Effect: DWORD);
begin
  FEffect := Effect;
  FEffectSet := True;
end;

{ TWinClipboardDragDrop }

procedure TWinClipboardDragDrop.DoDrag(const Source: TObject; const Form: TCommonCustomForm;
  const Bitmap: TBitmap; const PrepareClipboard: TProc<TClipboard>);
var
  Control: IControl;
  OleDrag: IOleDragHelper;
begin
  OleDrag := TOleDragHelper.Create;
  OleDrag.SetBitmap(CreateDDB(Bitmap, [cdRemovePureBlack, cdUnPreMultiply]));
  if Supports(Source, IControl, Control) then
    OleDrag.SetOffset(Control.ScreenToLocal(OleDrag.MousePos).Truncate);
  PrepareClipboard(OleDrag.Clipboard);
  if not OleDrag.Execute and (Control <> nil) then
    Control.DragEnd;
end;

function TWinClipboardDragDrop.RegisterForm(const Form: TCommonCustomForm): IClipboardDropInfo;
var
  Obj: TWinWindowHandle;
  DropTarget: TWinDropTarget;
begin
  //we need to replace the standard TWinDropTarget implementation with a custom one
  Obj := WindowHandleToPlatform(Form.Handle);
  if FWinDropTargetField = nil then
    FWinDropTargetField := SharedContext.GetType(Obj.ClassType).GetField('FWinDropTarget');
  DropTarget := FWinDropTargetField.GetValue(Obj).AsType<TWinDropTarget>;
  //atom is set up in FMX.Platform.Win but not actually used... we'll follow anyhow
  if FFMXDropTargetAtom = nil then
    FFMXDropTargetAtom := MAKEINTATOM(GlobalFindAtom(PChar(Format('FIREMONKEYDROP%.8X', [GetCurrentProcessID]))));
  RevokeDragDrop(Obj.Wnd);
  if FFMXDropTargetAtom <> nil then RemoveProp(Obj.Wnd, FFMXDropTargetAtom);
  DropTarget.Free;
  DropTarget := TClipboardDropTarget.Create(Form);
  FWinDropTargetField.SetValue(Obj, DropTarget);
  if FFMXDropTargetAtom <> nil then SetProp(Obj.Wnd, FFMXDropTargetAtom, THandle(DropTarget));
  RegisterDragDrop(Obj.Wnd, TClipboardDropTarget(DropTarget));
  Result := DropTarget as IClipboardDropInfo;
end;

procedure TWinClipboardDragDrop.UnregisterForm(const Form: TCommonCustomForm);
var
  Obj: TWinWindowHandle;
begin
  Obj := WindowHandleToPlatform(Form.Handle);
  RevokeDragDrop(Obj.Wnd)
end;
{$IFEND}

{ TBitmapClipper }

type
  TBitmapClipper = class(TInterfacedObject, ICustomClipper, IObjectClipper<TBitmap>)
  protected
    function CanLoadFromClipboard(const Clipboard: TClipboard): Boolean;
    procedure LoadFromClipboard(const Clipboard: TClipboard;
      const Callback: TLoadObjectsCallback<TBitmap>);
    procedure SaveToClipboard(const Clipboard: TClipboard; PreferDelayed: Boolean;
      const BitmapGetter: TFunc<TBitmap>);
  end;

function TBitmapClipper.CanLoadFromClipboard(const Clipboard: TClipboard): Boolean;
var
  FN: string;
begin
  Result := Clipboard.HasFormat([cfPNG, cfDIBv5, cfBitmap, cfTIFF, cfJpeg, cfGIF]);
  if Result then Exit;
  for FN in Clipboard.GetFileNames do
    if HasBitmapFileExt(FN) then Exit(True);
  for FN in Clipboard.GetVirtualFileDescriptors do
    if HasBitmapFileExt(FN) then Exit(True);
end;

procedure TBitmapClipper.LoadFromClipboard(const Clipboard: TClipboard;
  const Callback: TLoadObjectsCallback<TBitmap>);
var
  AssignProc: TProc<TBitmap>;
  Bitmap: TBitmap;
  LookForMore: Boolean;
  FileName: string;
begin
  Bitmap := TBitmap.Create;
  try
    Clipboard.EnumDataObject(
      procedure (const Obj: IDataObject; const FormatEtc: TFormatEtc; var Continue: Boolean)
      var
        Medium: TStgMedium;

        function TryGetMedium(Format: TClipFormat; MediumType: LongInt): Boolean;
        begin
          Result := (FormatEtc.cfFormat = Format) and (FormatEtc.tymed = MediumType) and
                    (Obj.GetData(FormatEtc, Medium) = S_OK) //and (Medium.hGlobal <> 0);
        end;
      var
        Len: NativeInt;
        Stream: TStream;
      begin
        Continue := False;
        //do we have a PNG or JPEG or TIFF or GIF in memory?
        if TryGetMedium(cfPNG.Handle, TYMED_HGLOBAL) or
          TryGetMedium(CF_TIFF, TYMED_HGLOBAL) or
          TryGetMedium(cfJpeg.Handle, TYMED_HGLOBAL) or
          TryGetMedium(cfGIF.Handle, TYMED_HGLOBAL) then
        try
          Len := GlobalSize(Medium.hGlobal);
          Stream := TUserMemoryStream.Create(GlobalLock(Medium.hGlobal), Len);
          try
            Bitmap.LoadFromStream(Stream);
            Exit;
          finally
            GlobalUnlock(Medium.hGlobal);
            Stream.Free;
          end;
        finally
          ReleaseStgMedium(Medium);
        end;
        //do we have a streamed PNG?
        if TryGetMedium(cfPNG.Handle, TYMED_ISTREAM) then
        try
          Stream := TOleStreamWrapper.Create(IStream(Medium.stm));
          try
            Stream.Seek(0, soBeginning);
            Bitmap.LoadFromStream(Stream);
            Exit;
          finally
            Stream.Free;
          end;
        finally
          ReleaseStgMedium(Medium);
        end;
        //do we have a DIB v5?
        if TryGetMedium(CF_DIBV5, TYMED_HGLOBAL) then
        try
          if TryLoadBitmapFromDIB5HGlobal(Bitmap, Medium.hGlobal) then Exit;
        finally
          ReleaseStgMedium(Medium);
        end;
    //    //do we have a metafile image?
    //    if TryGetMedium(CF_METAFILEPICT, TYMED_MFPICT) then
    //    try
    //      GetBitmapFromMetafilePict(Medium.hMetaFilePict, Bitmap);
    //      Exit(True);
    //    finally
    //      ReleaseStgMedium(Medium);
    //    end;
        if TryGetMedium(CF_BITMAP, TYMED_GDI) then
        try
          if TryLoadBitmapFromDDB(Bitmap, Medium.hBitmap) then Exit;
        finally
          ReleaseStgMedium(Medium);
        end;
        Continue := True;
      end);
    { If the TClipboard passed to us represents the system clipboard rather than an
      arbitrary IDataObject, try for a system-generated DIB v5 or DDB. }
    if Bitmap.IsEmpty and Clipboard.IsSystemClipboard then
      if not TryLoadBitmapFromDIB5HGlobal(Bitmap, GetClipboardData(CF_DIBV5)) then
        TryLoadBitmapFromDDB(Bitmap, GetClipboardData(CF_BITMAP));
    AssignProc :=
      procedure (Dest: TBitmap)
      begin
        Dest.Assign(Bitmap);
      end;
    LookForMore := True;
    if not Bitmap.IsEmpty then
    begin
      Callback(AssignProc, LookForMore);
      Exit;
    end;
    { If more than one in-memory raster format exists, only return one because they
      should in principle be the 'same' graphic. OTOH, if there is no in-memory
      graphic but more than one image file or virtual image file, load as many as the
      caller doesn't say no to}
    for FileName in Clipboard.GetFileNames do
      if HasBitmapFileExt(FileName) and TryLoadBitmapFromFile(Bitmap, FileName) then
      begin
        Callback(AssignProc, LookForMore);
        if not LookForMore then Exit;
      end;
    Clipboard.EnumVirtualFiles(
      procedure (const Descriptor: string; const SaveToStream: TProc<TStream>; var Continue: Boolean)
      var
        Stream: TMemoryStream;
      begin
        if not HasBitmapFileExt(Descriptor) then Exit;
        Stream := TMemoryStream.Create;
        try
          SaveToStream(Stream);
          Stream.Position := 0;
          if not TryLoadBitmapFromStream(Bitmap, Stream) then Exit;
        finally
          Stream.Free;
        end;
        Callback(AssignProc, LookForMore);
        Continue := LookForMore;
      end);
  finally
    Bitmap.Free;
    AssignProc := nil; //needed to break reference cycle (nah, dunno either...)
  end;
end;

procedure TBitmapClipper.SaveToClipboard(const Clipboard: TClipboard;
  PreferDelayed: Boolean; const BitmapGetter: TFunc<TBitmap>);
var
  Bitmap: TBitmap;
begin
  if PreferDelayed then
  begin
    Clipboard.AssignDelayed(cfPNG,
      function : TBytes
      begin
        Result := BytesOf(BitmapGetter() as IStreamPersist);
      end);
    Clipboard.SetAsHandleDelayed(cfDIBv5,
      function : THandle
      begin
        Result := CreateDIB5Handle(BitmapGetter);
      end);
    if not Clipboard.IsSystemClipboard then
      Clipboard.SetAsHandleDelayed(cfBitmap,
        function : THandle
        begin
          Result := CreateDDB(BitmapGetter, [cdForceOpaque])
        end);
  end
  else
  begin
    Bitmap := BitmapGetter;
    Clipboard.Assign(cfPNG, Bitmap as IStreamPersist);
    Clipboard.SetAsHandle(cfDIBv5, CreateDIB5Handle(Bitmap));
    if not Clipboard.IsSystemClipboard then
      Clipboard.SetAsHandle(cfBitmap, CreateDDB(Bitmap, [cdForceOpaque]));
  end;
end;

initialization
  TClipboard.RegisterClipper<TBitmap>(TBitmapClipper.Create);
{$IF DECLARED(TWinClipboardDragDrop)}
  TPlatformServices.Current.AddPlatformService(IFMXClipboardDragDropService, TWinClipboardDragDrop.Create);
{$IFEND}
{$IFEND}
end.
