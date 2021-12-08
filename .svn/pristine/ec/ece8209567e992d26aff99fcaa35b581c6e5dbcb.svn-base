{**************************************************************************************}
{                                                                                      }
{ CCR.Clipboard - VCL-specific extensions to generic Windows backend                   }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                  }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2012-15 Chris Rolliston. All Rights Reserved.      }
{                                                                                      }
{**************************************************************************************}

unit CCR.Clipboard.VCL;
{
  This unit provides TClipboard-based drag and drop support plus clippers for TPicture
  and all stock TGraphic descendants. The clippers for TGIFImage, TJPEGImage and
  TPNGImage all read and write to CF_BITMAP as well as the applicable native in-memory
  format for compatibility (particularly important for drag and drop); conversely, the
  TWICImage clipper writes to an appropriate in-memory format as well as CF_BITMAP for
  best fidelity, e.g. a TIFF file with transparent areas. In all cases multiple objects
  may be read if there is more than one suitable file or virtual file *and* there is
  not a compatible in-memory format on the clipboard too. The latter is because, in
  principle, all the formats held in a TClipboard at any one time should, in principle,
  represent the same data. When copying an image in IE, for example, the program will
  therefore copy both the image itself as one or more in-memory formats and the cache
  file path to CF_HDROP.
}
interface

{$IF DEFINED(MSWINDOWS)}
{$POINTERMATH ON}
uses
  WinAPI.Windows, WinAPI.Messages, WinAPI.ActiveX, WinAPI.ShlObj, System.Win.ComObj,
  System.Types,System.SysUtils, System.Classes, System.IOUtils, System.RTLConsts,
  System.StrUtils, System.UITypes, System.Generics.Collections,
  VCL.Consts, VCL.Graphics, VCL.Imaging.GIFImg, VCL.Imaging.JPEG, VCL.Imaging.PNGImage,
  VCL.Controls, VCL.Forms,
  CCR.Clipboard, CCR.Clipboard.Consts, CCR.Clipboard.Win;

type
  TClipboardBasedDragEvent = procedure (const Control: TWinControl;
    const Clipboard: TClipboard) of object;

  TClipboardHelper = class helper for TClipboard
  strict private class var
    FGraphicFileExts: string;
    FRegisteredWindowedDropTargets: TDictionary<TWinControl, IClipboardDropInfo>;
    class constructor InitializeClass;
    class destructor FinalizeClass;
  strict private
    function GetFormatCount: Integer;
    function GetFormat(Index: Integer): TClipboardFormat;
  private class var
    cfPicture: TClipboardFormat;
    class procedure DropTargetDestroying(const Control: TWinControl); inline;
    class function DoRegisterDropTarget(const Control: TControl; ChildrenOnly: Boolean): IClipboardDropInfo;
  protected
    class function HasGraphicFileExt(const FileName: string): Boolean; overload; static;
    class function HasGraphicFileExt(const FileNames: TArray<string>): Boolean; overload; static;
  public
    class function CreateForDataObject(const AObj: IDataObject;
      DummyChangeCountBehaviour: TDummyChangeCountBehaviour = dbIncrementing): TClipboard;
    procedure AssignRTF(const RichEditLines: TStrings); overload;
    procedure EnumDataObject(const Callback: TEnumDataObjectCallback);
    function GetAsHandle(const Format: TClipboardFormat): THandle;
    procedure SetAsHandle(const Format: TClipboardFormat; Value: THandle); overload;
    procedure SetAsHandleDelayed(const Format: TClipboardFormat; const ValueGetter: TFunc<THandle>); overload;
    { compatibility props }
    property FormatCount: Integer read GetFormatCount;
    property Formats[Index: Integer]: TClipboardFormat read GetFormat;
    { where the application is the source of the drag }
    class procedure BeginDrag(const DraggedObject: TObject; const DragImage: TBitmap;
      const OnGetData: TAssignClipboardForDragEvent = nil); overload;
    class procedure BeginDrag(const Control: TControl; const OnGetData: TAssignClipboardForDragEvent); overload;
    class procedure BeginDrag(const Control: TControl; const OnGetData: TProc<TClipboard>); overload;
    { when the application is the target of the drop }
    class function RegisterDropTarget(const Control: TControl): IClipboardDropInfo; inline;
    class function RegisterDropTargets(const Controls: array of TControl): TArray<IClipboardDropInfo>;
  end;

function cfPicture: TClipboardFormat; inline; deprecated 'Refactor to use Clipboard.HasFormatFor(TPicture) instead';
{$IFEND}

implementation

{$IF DEFINED(MSWINDOWS)}
uses
  System.Contnrs;

function cfPicture: TClipboardFormat;
begin
  Result := TClipboard.cfPicture;
end;

type
  TControlAccess = class(TControl);

  IInternalClipboardDropInfo = interface
  ['{DB704A98-A2F7-4EAD-9083-C2B8135DEADD}']
    procedure RegisterChild(const Child: TControl);
    procedure SetChildrenOnly(Value: Boolean);
    property ChildrenOnly: Boolean write SetChildrenOnly;
  end;

  TClipboardDropInfo = class(TComponent, IInterface, IDropTarget, IClipboardDropInfo,
    IWinClipboardDropInfo, IInternalClipboardDropInfo)
  strict private
    FChildrenOnly: Boolean;
    FClipboard: TClipboard;
    FControlBeingDraggedOver: TControl;
    FDropTargetHelper: IDropTargetHelper;
    FEffect: DWORD;
    FEffectSet: Boolean;
    FOldWndProc: TWndMethod;
    FPrevParent: TWinControl;
    FNeedToRegister: Boolean;
    FRefCount: Integer;
    FRegisteredChildren: TComponentList;
    FWinControl: TWinControl;
    procedure NewWndProc(var Msg: TMessage);
    function GetControlBeingDraggedOver(const ScreenPos: TPoint): TControl;
    procedure DoDragOver(Target: TControl; const ScreenPos: TPoint;
      State: TDragState; var dwEffect: LongInt);
    procedure DoDragDrop(Target: TControl; const ScreenPos: TPoint);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    { IInterface }
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
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
    { IInternalClipboardDropInfo }
    procedure RegisterChild(const Child: TControl);
    procedure SetChildrenOnly(Value: Boolean);
  public
    constructor Create(const AWinControl: TWinControl; ChildrenOnly: Boolean); reintroduce;
    destructor Destroy; override;
  end;

constructor TClipboardDropInfo.Create(const AWinControl: TWinControl; ChildrenOnly: Boolean);
begin
  inherited Create(nil);
  FChildrenOnly := ChildrenOnly;
  AWinControl.FreeNotification(Self);
  FWinControl := AWinControl;
  FOldWndProc := AWinControl.WindowProc;
  AWinControl.WindowProc := NewWndProc;
  FNeedToRegister := True;
  FRegisteredChildren := TComponentList.Create(False);
end;

destructor TClipboardDropInfo.Destroy;
begin
  if FWinControl <> nil then FWinControl.WindowProc := FOldWndProc;
  FRegisteredChildren.Free;
  inherited;
end;

function TClipboardDropInfo._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TClipboardDropInfo._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if FRefCount = 0 then Destroy;
end;

procedure TClipboardDropInfo.Notification(AComponent: TComponent; Operation: TOperation);
var
  Control: TWinControl;
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FWinControl) then
  begin
    Control := FWinControl;
    FWinControl := nil;
    Control.WindowProc := FOldWndProc;
    TClipboard.DropTargetDestroying(Control);
  end;
end;

procedure TClipboardDropInfo.RegisterChild(const Child: TControl);
begin
  if FRegisteredChildren.IndexOf(Child) < 0 then
    FRegisteredChildren.Add(Child);
end;

procedure TClipboardDropInfo.SetChildrenOnly(Value: Boolean);
begin
  FChildrenOnly := Value;
end;

procedure TClipboardDropInfo.NewWndProc(var Msg: TMessage);
begin
  case Msg.Msg of
    WM_CREATE: FNeedToRegister := True;
    WM_DESTROY: RevokeDragDrop(FWinControl.Handle);
  else
    if FNeedToRegister and FWinControl.HandleAllocated then
    begin
      FNeedToRegister := False;
      RegisterDragDrop(FWinControl.Handle, Self)
    end;
    //'soft-register' everyone up the food chain so that drag images work
    if not FNeedToRegister and (FWinControl.Parent <> FPrevParent) and (FWinControl.Parent <> nil) then
    begin
      FPrevParent := FWinControl.Parent;
      TClipboard.DoRegisterDropTarget(FPrevParent, True);
    end;
  end;
  FOldWndProc(Msg);
end;

function TClipboardDropInfo.GetControlBeingDraggedOver(const ScreenPos: TPoint): TControl;
begin
  if FRegisteredChildren.Count = 0 then
    Result := nil
  else
  begin
    Result := FWinControl.ControlAtPos(FWinControl.ScreenToClient(ScreenPos), False);
    if (Result <> nil) and (FRegisteredChildren.IndexOf(Result) < 0) then Result := nil;
  end;
  if (Result = nil) and not FChildrenOnly then Result := FWinControl;
end;

procedure TClipboardDropInfo.DoDragOver(Target: TControl;
  const ScreenPos: TPoint; State: TDragState; var dwEffect: LongInt);
var
  Accept: Boolean;
  ClientPos: TPoint;
begin
  FEffectSet := False;
  if Target = nil then
    Accept := False
  else
  begin
    ClientPos := Target.ScreenToClient(ScreenPos);
    TControlAccess(Target).DragOver(FClipboard, ClientPos.X, ClientPos.Y, State, Accept);
  end;
  if not FEffectSet then
    if Accept then
      SetEffect(DROPEFFECT_COPY)
    else
      SetEffect(DROPEFFECT_NONE);
  DWORD(dwEffect) := FEffect
end;

procedure TClipboardDropInfo.DoDragDrop(Target: TControl; const ScreenPos: TPoint);
var
  ClientPos: TPoint;
begin
  Assert(Target <> nil);
  ClientPos := Target.ScreenToClient(ScreenPos);
  TControlAccess(Target).DragDrop(FClipboard, ClientPos.X, ClientPos.Y);
end;

function TClipboardDropInfo.DragEnter(const dataObj: IDataObject; grfKeyState: LongInt;
  pt: TPoint; var dwEffect: LongInt): HRESULT;
begin
  try
    FClipboard := TWinClipboardCore.CreateClipboardForDataObject(dataObj, dbFixed);
    FControlBeingDraggedOver := GetControlBeingDraggedOver(pt);
    DoDragOver(FControlBeingDraggedOver, Pt, dsDragEnter, dwEffect);
    if (Succeeded(CoCreateInstance(CLSID_DragDropHelper, nil, CLSCTX_INPROC_SERVER, IDropTargetHelper,
      FDropTargetHelper))) and (FDropTargetHelper <> nil) then
    begin
      Result := FDropTargetHelper.DragEnter(FWinControl.Handle, dataObj, pt, dwEffect);
      if Failed(Result) then FDropTargetHelper := nil;
    end;
    Result := S_OK;
  except
    dwEffect := DROPEFFECT_NONE;
    Result := E_UNEXPECTED;
  end;
end;

function TClipboardDropInfo.DragOver(grfKeyState: LongInt; pt: TPoint; var dwEffect: LongInt): HRESULT;
var
  NewControl: TControl;
begin
  try
    NewControl := GetControlBeingDraggedOver(pt);
    if NewControl = FControlBeingDraggedOver then
      DoDragOver(NewControl, Pt, dsDragMove, dwEffect)
    else
    begin
      DoDragOver(FControlBeingDraggedOver, Pt, dsDragLeave, dwEffect);
      FControlBeingDraggedOver := NewControl;
      DoDragOver(NewControl, Pt, dsDragEnter, dwEffect);
    end;
    if FDropTargetHelper <> nil then
      FDropTargetHelper.DragOver(pt, dwEffect);
    Result := S_OK;
  except
    dwEffect := DROPEFFECT_NONE;
    Result := E_UNEXPECTED;
  end;
end;

function TClipboardDropInfo.DragLeave: HRESULT;
var
  Effect: LongInt;
  Pt: TPoint;
begin
  try
    try
      GetCursorPos(Pt);
      ScreenToClient(FWinControl.Handle, Pt);
      DoDragOver(FControlBeingDraggedOver, Pt, dsDragLeave, Effect);
      if FDropTargetHelper <> nil then
        FDropTargetHelper.DragLeave;
      FDropTargetHelper := nil;
      Result := S_OK;
    finally
      FreeAndNil(FClipboard);
    end;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TClipboardDropInfo.Drop(const dataObj: IDataObject; grfKeyState: LongInt; pt: TPoint;
  var dwEffect: LongInt): HRESULT;
begin
  try
    try
      DWORD(dwEffect) := FEffect;
      DoDragDrop(FControlBeingDraggedOver, pt);
      if (FDropTargetHelper <> nil) then
        FDropTargetHelper.Drop(dataObj, pt, dwEffect);
      Result := S_OK;
    finally
      FDropTargetHelper := nil;
      FreeAndNil(FClipboard);
    end;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TClipboardDropInfo.GetClipboard: TClipboard;
begin
  Result := FClipboard;
end;

procedure TClipboardDropInfo.SetEffect(Effect: DWORD);
begin
  FEffect := Effect;
  FEffectSet := True;
end;

{ TClipboardHelper }

class constructor TClipboardHelper.InitializeClass;
begin
  cfPicture := TWinClipboardCore.RegisterFormat('Delphi Picture');
  FRegisteredWindowedDropTargets := TDictionary<TWinControl, IClipboardDropInfo>.Create;
end;

class destructor TClipboardHelper.FinalizeClass;
begin
  FRegisteredWindowedDropTargets.Free;
end;

class function TClipboardHelper.CreateForDataObject(const AObj: IDataObject;
  DummyChangeCountBehaviour: TDummyChangeCountBehaviour): TClipboard;
begin
  Result := TWinClipboardCore.CreateClipboardForDataObject(AObj, DummyChangeCountBehaviour);
end;

procedure TClipboardHelper.AssignRTF(const RichEditLines: TStrings);
var
  Stream: TBytesStream;
begin
  Stream := TBytesStream.Create;
  try
    RichEditLines.SaveToStream(Stream);
    Stream.SetSize(Stream.Position);
    AssignRTF(Stream.Bytes);
  finally
    Stream.Free;
  end;
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

function TClipboardHelper.GetFormatCount: Integer;
begin
  if IsSystemClipboard then
    Result := CountClipboardFormats
  else
    Result := Length(GetFormats);
end;

function TClipboardHelper.GetFormat(Index: Integer): TClipboardFormat;
var
  Arr: TArray<TClipboardFormat>;
begin
  { Vcl.ClipBrd.Clipboard.Formats doesn't raise an exception on an out-of-range
    index, so we won't either }
  Arr := GetFormats;
  if Arr = nil then
    Result := TClipboardFormat.Wrap(0)
  else
  begin
    if Index >= Length(Arr) then Index := High(Arr);
    Result := Arr[Index];
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

class procedure TClipboardHelper.BeginDrag(const DraggedObject: TObject;
  const DragImage: TBitmap; const OnGetData: TAssignClipboardForDragEvent = nil);
var
  Bitmap: TBitmap;
  Callback: TAssignClipboardForDragEvent;
  Line: PAlphaColor;
  X, Y: Integer;
  OleDrag: IOleDragHelper;
  TransparentColor: TColorRef;
begin
  OleDrag := TOleDragHelper.Create;
  Bitmap := TBitmap.Create;
  try
    TransparentColor := 0;
    if DragImage.PixelFormat = pf32bit then
    begin
      Bitmap.Assign(DragImage);
      if Bitmap.AlphaFormat = afPremultiplied then
        Bitmap.AlphaFormat := afDefined;
    end
    else
    begin
      if DragImage.Transparent then
        TransparentColor := TColorRef(DragImage.TransparentColor and not $02000000);
      Bitmap.Canvas.Brush.Color := TransparentColor;
      Bitmap.PixelFormat := pf32bit;
      Bitmap.SetSize(DragImage.Width, DragImage.Height);
      Bitmap.Canvas.Draw(0, 0, DragImage);
    end;
    for Y := 0 to Bitmap.Height - 1 do
    begin
      Line := Bitmap.ScanLine[Y];
      for X := 0 to Bitmap.Width - 1 do
        if Line[X] or $FF000000 = TAlphaColors.Black then
          Line[X] := (Line[X] and $FF000000) or $0A0A0A;
    end;
    OleDrag.SetBitmap(Bitmap.ReleaseHandle, TransparentColor);
  finally
    Bitmap.Free;
  end;
  if DraggedObject is TControl then
    OleDrag.SetOffset(TControl(DraggedObject).ScreenToClient(OleDrag.MousePos));
  Callback := CreateCombinedGetDragDataCallback(OnGetData);
  Callback(DraggedObject, OleDrag.Clipboard);
  SetObjectBeingDragged(DraggedObject);
  try
    OleDrag.Execute;
  finally
    SetObjectBeingDragged(nil);
  end;
end;

class procedure TClipboardHelper.BeginDrag(const Control: TControl;
  const OnGetData: TAssignClipboardForDragEvent);
var
  DragImage: TBitmap;
begin
  DragImage := TBitmap.Create;
  try
    DragImage.Canvas.Brush.Color := clBtnFace;
    DragImage.SetSize(Control.Width, Control.Height);
    Control.Perform(WM_PAINT, DragImage.Canvas.Handle, 0);
    DragImage.Transparent := not (csOpaque in Control.ControlStyle);
    if DragImage.Transparent then
      DragImage.TransparentColor := DragImage.Canvas.Brush.Color;
    BeginDrag(Control, DragImage, OnGetData);
  finally
    DragImage.Free;
  end;
end;

class procedure TClipboardHelper.BeginDrag(const Control: TControl; const OnGetData: TProc<TClipboard>);
begin
  BeginDrag(Control,
    procedure (DraggedObject: TObject; Clipboard: TClipboard)
    begin
      OnGetData(Clipboard);
    end);
end;

class procedure TClipboardHelper.DropTargetDestroying(const Control: TWinControl);
begin
  FRegisteredWindowedDropTargets.Remove(Control);
end;

class function TClipboardHelper.DoRegisterDropTarget(const Control: TControl; ChildrenOnly: Boolean): IClipboardDropInfo;
var
  WinControl: TWinControl;
begin
  if Control is TWinControl then
  begin
    WinControl := TWinControl(Control);
    if FRegisteredWindowedDropTargets.TryGetValue(WinControl, Result) then
    begin
      if not ChildrenOnly then (Result as IInternalClipboardDropInfo).ChildrenOnly := False;
    end
    else
    begin
      Result := TClipboardDropInfo.Create(WinControl, ChildrenOnly);
      FRegisteredWindowedDropTargets.Add(WinControl, Result);
    end
  end
  else if not ChildrenOnly then
  begin
    if not Control.HasParent then
      raise EClipboardException.CreateRes(@SDropTargetMustHaveParent);
    Result := DoRegisterDropTarget(Control.Parent, True);
    (Result as IInternalClipboardDropInfo).RegisterChild(Control);
  end;
end;

class function TClipboardHelper.RegisterDropTarget(const Control: TControl): IClipboardDropInfo;
begin
  Result := DoRegisterDropTarget(Control, False);
end;

class function TClipboardHelper.RegisterDropTargets(const Controls: array of TControl): TArray<IClipboardDropInfo>;
var
  I: Integer;
begin
  SetLength(Result, Length(Controls));
  for I := Low(Controls) to High(Controls) do
    Result[I] := RegisterDropTarget(Controls[I]);
end;

class function TClipboardHelper.HasGraphicFileExt(const FileName: string): Boolean;
var
  Ext: string;
begin
  Ext := ExtractFileExt(FileName);
  if Ext = '' then Exit(False);
  if FGraphicFileExts = '' then FGraphicFileExts := LowerCase(GraphicFileMask(TGraphic));
  Result := (Pos(LowerCase(Ext) + ';', FGraphicFileExts) <> 0);
end;

class function TClipboardHelper.HasGraphicFileExt(const FileNames: TArray<string>): Boolean;
var
  FN: string;
begin
  for FN in FileNames do
    if HasGraphicFileExt(FN) then Exit(True);
  Result := False;
end;

type
  { Delayed rendering of a TPicture is not possible because we do not know up front what
    type the Graphic property will become, and therefore, what clipboard formats to promise.
    As such, the callback is always invoked immediately. }
  TPictureClipper = class(TInterfacedObject, ICustomClipper, IObjectClipper<TPicture>)
  strict private
    function TryLoadFromMemFormat(const Clipboard: TClipboard; Picture: TPicture): Boolean;
  protected
    function CanLoadFromClipboard(const Clipboard: TClipboard): Boolean;
    procedure LoadFromClipboard(const Clipboard: TClipboard;
      const Callback: TLoadObjectsCallback<TPicture>);
    procedure SaveToClipboard(const Clipboard: TClipboard; PreferDelayed: Boolean;
      const PictureGetter: TFunc<TPicture>);
  end;

  TRasterGraphicClipper<T: TGraphic, constructor> = class(TStreamableObjectClipper<T>)
  strict private
    procedure DoSaveBitmapToClipboard(const Clipboard: TClipboard;
      const Format: TClipboardFormat; PreferDelayed: Boolean; const BitmapGetter: TFunc<TBitmap>);
  protected
    function CreateBlankObject: T; override;
    procedure LoadFromClipboardFormat(const Clipboard: TClipboard;
      const Format: TClipboardFormat; const Callback: TLoadObjectsCallback<T>); override;
    procedure SaveToClipboardFormat(Clipboard: TClipboard; const Format: TClipboardFormat;
      PreferDelayed: Boolean; const ObjGetter: TFunc<T>); override;
  end;

  TWICImageClipper = class(TRasterGraphicClipper<TWICImage>)
  protected
    procedure SaveToClipboard(const Clipboard: TClipboard; PreferDelayed: Boolean;
      const ObjGetter: TFunc<TWICImage>); override;
  public
    constructor Create;
  end;

function TPictureClipper.CanLoadFromClipboard(const Clipboard: TClipboard): Boolean;
var
  Format: TClipboardFormat;
  S: string;
begin
  Result := Clipboard.HasFormat([cfBitmap, cfMetafilePict, cfPNG,
    cfEnhMetafile, cfGIF, cfJpeg]);
  if Result then Exit;
  for Format in Clipboard.GetFormats do
    if TPicture.SupportsClipboardFormat(Format.Handle) then Exit(True);
  for S in Clipboard.GetFileNames do
    if Clipboard.HasGraphicFileExt(S) then Exit(True);
  for S in Clipboard.GetVirtualFileDescriptors do
    if Clipboard.HasGraphicFileExt(S) then Exit(True);
end;

function TPictureClipper.TryLoadFromMemFormat(const Clipboard: TClipboard; Picture: TPicture): Boolean;
var
  Data: THandle;
  Format: TClipboardFormat;
  Palette: HPALETTE;
  Graphic: TGraphic;
  GraphicClass: TGraphicClass;
begin
  { 1. Try for a format we've explictly coded for in this unit, prioritising cfPNG etc.
       over cfBitmap for best fidelity }
  if Clipboard.HasFormat([cfPNG, cfGIF, cfJPEG, cfTIFF, cfBitmap, cfEnhMetafile, cfMetafilePict], Format) then
  begin
    case Format.Handle of
      CF_BITMAP: GraphicClass := TBitmap;
      CF_METAFILEPICT, CF_ENHMETAFILE: GraphicClass := TMetafile;
      CF_TIFF: GraphicClass := TWICImage;
    else
      if Format = cfPNG then
        GraphicClass := TPngImage
      else if Format = cfGIF then
        GraphicClass := TGIFImage
      else if Format = cfJpeg then
        GraphicClass := TJPEGImage
      else
      begin
        Assert(False);
        GraphicClass := nil;
      end;
    end;
    Graphic := GraphicClass.Create;
    try
      Graphic.Assign(Clipboard);
      Picture.Assign(Graphic);
      Exit(True);
    finally
      Graphic.Free;
    end;
  end;
  { 2. If a 3rd party class has registered with TPicture, let them get into the action }
  for Format in Clipboard.GetFormats do
    if TPicture.SupportsClipboardFormat(Format.Handle) then
    begin
      Data := Clipboard.GetAsHandle(Format);
      Palette := Clipboard.GetAsHandle(cfPalette);
      Picture.LoadFromClipboardFormat(Format.Handle, Data, Palette);
      Exit(True);
    end;
  Result := False;
end;

procedure TPictureClipper.LoadFromClipboard(const Clipboard: TClipboard;
  const Callback: TLoadObjectsCallback<TPicture>);
var
  AssignToProc: TProc<TPicture>;
  LookForMore: Boolean;
  FN: string;
  Picture: TPicture;
  TempDir: string;
begin
  LookForMore := True;
  Picture := TPicture.Create;
  try
    AssignToProc :=
      procedure (Dest: TPicture)
      begin
        Dest.Assign(Picture);
      end;
    if TryLoadFromMemFormat(Clipboard, Picture) then
    begin
      Callback(AssignToProc, LookForMore);
      Exit;
    end;
    for FN in Clipboard.GetFileNames do
      if Clipboard.HasGraphicFileExt(FN) then
      begin
        Picture.LoadFromFile(FN);
        Callback(AssignToProc, LookForMore);
        if not LookForMore then Exit;
      end;
    TempDir := TPath.GetTempPath;
    Clipboard.SaveVirtualFiles(
      procedure (const Descriptor: string; const SaveToFile: TSaveVirtualFileFunc; var LookForMore: Boolean)
      var
        FileName: TFileName;
      begin
        if not TClipboard.HasGraphicFileExt(Descriptor) then Exit;
        FileName := SaveToFile(TempDir);
        try
          Picture.LoadFromFile(FileName);
        finally
          DeleteFile(FileName);
        end;
        Callback(AssignToProc, LookForMore);
      end);
  finally
    AssignToProc := nil; //break reference cycle
    Picture.Free;
  end;
end;

procedure TPictureClipper.SaveToClipboard(const Clipboard: TClipboard;
  PreferDelayed: Boolean; const PictureGetter: TFunc<TPicture>);
begin
  if PictureGetter.Graphic <> nil then Clipboard.Assign(PictureGetter.Graphic);
end;

function TRasterGraphicClipper<T>.CreateBlankObject: T;
begin
  Result := T.Create;
end;

procedure TRasterGraphicClipper<T>.LoadFromClipboardFormat(const Clipboard: TClipboard;
  const Format: TClipboardFormat; const Callback: TLoadObjectsCallback<T>);
var
  LookForMore: Boolean;
begin
  if Format <> cfBitmap then
  begin
    inherited;
    Exit;
  end;
  Callback(
    procedure (Dest: T)
    var
      Bitmap: TBitmap;
    begin
      if T.InheritsFrom(TBitmap) then
        Bitmap := TBitmap(Dest)
      else
        Bitmap := TBitmap.Create;
      try
        Bitmap.LoadFromClipboardFormat(Format.Handle, Clipboard.GetAsHandle(cfBitmap),
          Clipboard.GetAsHandle(cfPalette));
        Dest.Assign(Bitmap);
      finally
        if Bitmap <> TObject(Dest) then Bitmap.Free;
      end;
    end, LookForMore);
end;

procedure TRasterGraphicClipper<T>.DoSaveBitmapToClipboard(const Clipboard: TClipboard;
  const Format: TClipboardFormat; PreferDelayed: Boolean; const BitmapGetter: TFunc<TBitmap>);
begin
  if PreferDelayed then
    Clipboard.SetAsHandleDelayed(cfBitmap,
      function : THandle
      begin
        Result := BitmapGetter.ReleaseHandle;
      end)
  else
    Clipboard.SetAsHandle(Format, BitmapGetter.ReleaseHandle);
end;

procedure TRasterGraphicClipper<T>.SaveToClipboardFormat(Clipboard: TClipboard;
  const Format: TClipboardFormat; PreferDelayed: Boolean; const ObjGetter: TFunc<T>);
var
  BitmapGetter: TFunc<TBitmap>;
begin
  if Format <> cfBitmap then
    inherited
  else if T.InheritsFrom(TBitmap) then
    DoSaveBitmapToClipboard(Clipboard, Format, PreferDelayed, TFunc<TBitmap>(ObjGetter))
  else
  begin
    BitmapGetter := TRenderedObject<TBitmap>.Create(
      procedure (Bitmap: TBitmap)
      begin
        Bitmap.SetSize(ObjGetter.Width, ObjGetter.Height);
        Bitmap.Canvas.Draw(0, 0, ObjGetter);
      end);
    DoSaveBitmapToClipboard(Clipboard, Format, PreferDelayed, BitmapGetter);
  end;
end;

constructor TWICImageClipper.Create;
begin
  inherited Create([cfPNG, cfJpeg, cfGIF, cfBitmap], [cfBitmap],
    ['.bmp', '.gif', '.jpeg', '.jpg', '.png', '.tiff', '.wmp']);
end;

procedure TWICImageClipper.SaveToClipboard(const Clipboard: TClipboard;
  PreferDelayed: Boolean; const ObjGetter: TFunc<TWICImage>);
begin
  case ObjGetter.ImageFormat of
    wifPng: SaveToClipboardFormat(Clipboard, cfPNG, PreferDelayed, ObjGetter);
    wifJpeg: SaveToClipboardFormat(Clipboard, cfJpeg, PreferDelayed, ObjGetter);
    wifGif: SaveToClipboardFormat(Clipboard, cfGIF, PreferDelayed, ObjGetter);
    wifTiff: SaveToClipboardFormat(Clipboard, cfTIFF, PreferDelayed, ObjGetter);
  end;
  inherited;
end;

procedure LoadMetafileFromClipboard(const Clipboard: TClipboard;
  const Format: TClipboardFormat; Metafile: TMetafile);
var
  Bytes: TBytes;
  Handle: HGLOBAL;
  PictPtr: PMetafilePict;
begin
  if Format = cfEnhMetafile then
  begin
    Metafile.Handle := Clipboard.GetAsHandle(Format);
    Exit;
  end;
  Assert(Format = cfMetafilePict);
  Handle := Clipboard.GetAsHandle(Format);
  PictPtr := GlobalLock(Handle);
  try
    SetLength(Bytes, GetMetaFileBitsEx(PictPtr.hMF, 0, nil));
    GetMetaFileBitsEx(PictPtr.hMF, Length(Bytes), Bytes);
    Metafile.Handle := SetWinMetaFileBits(Length(Bytes), @Bytes[0], 0, PictPtr^);
  finally
    GlobalFree(Handle)
  end;
end;

procedure SaveMetafileToClipboard(const Clipboard: TClipboard;
  const Format: TClipboardFormat; PreferDelayed: Boolean; const ObjGetter: TFunc<TMetafile>);
begin
  if PreferDelayed then
    Clipboard.SetAsHandleDelayed(Format,
      function : THandle
      begin
        Result := ObjGetter.ReleaseHandle;
      end)
  else
    Clipboard.SetAsHandle(Format, ObjGetter.ReleaseHandle);
end;

procedure RegisterClippers;
begin
  TClipboard.RegisterClipper<TBitmap>(TRasterGraphicClipper<TBitmap>.Create(
    [cfBitmap], ['.bmp']));
  TClipboard.RegisterClipper<TGIFImage>(TRasterGraphicClipper<TGIFImage>.Create(
    [cfGIF, cfBitmap], ['.gif']));
  TClipboard.RegisterClipper<TJPEGImage>(TRasterGraphicClipper<TJPEGImage>.Create(
    [cfJPEG, cfBitmap], ['.jpeg', '.jpg']));
  TClipboard.RegisterClipper<TPNGImage>(TRasterGraphicClipper<TPNGImage>.Create(
    [cfPNG, cfBitmap], ['.png']));
  TClipboard.RegisterClipper<TWICImage>(TWICImageClipper.Create);
  TClipboard.RegisterClipper<TMetafile>([cfEnhMetafile, cfMetafilePict],
    [cfEnhMetafile], ['.wmf', '.emf'], LoadMetafileFromClipboard, SaveMetafileToClipboard);
  TClipboard.RegisterClipper<TPicture>(TPictureClipper.Create);
  TWinClipboardCore.LegacyFormatToClassMap.Add(TClipboard.cfPicture, TPicture);
end;

initialization
  RegisterClippers;
{$IFEND}
end.
