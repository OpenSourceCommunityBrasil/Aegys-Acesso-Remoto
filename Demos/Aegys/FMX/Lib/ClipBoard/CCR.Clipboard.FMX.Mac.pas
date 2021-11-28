{**************************************************************************************}
{                                                                                      }
{ CCR.Clipboard - FMX-specific extensions to generic OS X backend                      }
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

unit CCR.Clipboard.FMX.Mac;

interface

{$IF DEFINED(MACOS) AND NOT DEFINED(IOS)}
uses
  System.Types, System.SysUtils, System.Classes, System.Rtti, System.UITypes,
  MacApi.CoreFoundation, MacApi.CocoaTypes, MacApi.CoreGraphics,
  MacApi.ObjectiveC, MacApi.Foundation, MacApi.AppKit,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Platform.Mac,
  CCR.Clipboard.Consts, CCR.Clipboard, CCR.Clipboard.Apple, CCR.Clipboard.FMX;

type
  TClipboardHelper = class helper(CCR.Clipboard.FMX.TClipboardHelper) for TClipboard
  strict private
    function GetPasteboard: NSPasteboard;
  public
    constructor CreateForPasteboard(const AName: string); overload;
    constructor CreateForPasteboard(const AName: NSString); overload;
    constructor CreateForPasteboard(const APasteboard: NSPasteboard); overload;
    procedure AssignNSObject(const Obj: NSObject); overload;
    procedure AssignNSObject(const ObjID: Pointer); overload;
    property Pasteboard: NSPasteboard read GetPasteboard;
  end;
{$IFEND}

implementation

{$IF DEFINED(MACOS) AND NOT DEFINED(IOS)}
{$IF DECLARED(TFormSaveState)}
  {$DEFINE ImplDragAndDrop}
{$IFEND}
uses
  System.RTLConsts, System.IOUtils, System.TypInfo, System.StrUtils,
  Macapi.ObjCRuntime, FMX.Platform, {$IFDEF ImplDragAndDrop}FMX.Helpers.Mac, {$ENDIF}
  CCR.Clipboard.Apple.Helpers;

const
  AppKitLib = '/System/Library/Frameworks/AppKit.framework/AppKit';

type
  {$IF NOT DECLARED(TPlatformServices)}
  NSImage = interface(NSObject) //XE2's NSImage declaration was faulty, so redeclare with what we need only
    ['{D39B2515-517A-4EA4-BA74-729198211680}']
    function initWithContentsOfURL(url: NSURL): Pointer; cdecl;
    function CGImageForProposedRect(proposedDestRect: PNSRect; context: NSGraphicsContext; hints: NSDictionary): CGImageRef; cdecl;
  end;
  TNSImage = class(TOCGenericImport<NSImageClass, NSImage>);
  {$IFEND}

  {$IF DECLARED(TAlphaColorF)}
  TAlphaColorClipper = class(TInterfacedObject, ICustomClipper,
    IRecordClipper<TAlphaColor>, IRecordClipper<TAlphaColorF>)
  strict protected
    function CanLoadFromClipboard(const Clipboard: TClipboard): Boolean;
    procedure LoadFromClipboard(const Clipboard: TClipboard;
      const Callback: TGetInstancesCallback<TAlphaColor>); overload;
    procedure LoadFromClipboard(const Clipboard: TClipboard;
      const Callback: TGetInstancesCallback<TAlphaColorF>); overload;
    procedure SaveToClipboard(const Clipboard: TClipboard; PreferDelayed: Boolean;
      const ColorGetter: TFunc<TAlphaColor>); overload;
    procedure SaveToClipboard(const Clipboard: TClipboard; PreferDelayed: Boolean;
      const ColorGetter: TFunc<TAlphaColorF>); overload;
  protected
    class procedure Register;
  end;
  {$IFEND}

  TBitmapClipper = class(TInterfacedObject, ICustomClipper, IObjectClipper<TBitmap>)
  strict private class var
    FOptions: ICFDictionary;
    class constructor Create;
  protected
    { IObjectClipper<TBitmap> }
    function CanLoadFromClipboard(const Clipboard: TClipboard): Boolean;
    procedure LoadFromClipboard(const Clipboard: TClipboard;
      const Callback: TLoadObjectsCallback<TBitmap>);
    procedure SaveToClipboard(const Clipboard: TClipboard; PreferDelayed: Boolean;
      const BitmapGetter: TFunc<TBitmap>);
  end;

procedure CopyCGImageToBitmap(const Source: CGImageRef; const Dest: TBitmap);
var
  ColorSpace: CGColorSpaceRef;
  Context: CGContextRef;
  MapRec: TBitmapData;
begin
  Dest.SetSize(CGImageGetWidth(Source), CGImageGetHeight(Source));
  Dest.Clear(TAlphaColors.Null);
  ColorSpace := nil;
  Context := nil;
  if not Dest.Map(TMapAccess.Write, MapRec) then
    raise EInvalidOperation.CreateRes(@SCannotMapBitmap);
  try
    ColorSpace := CGColorSpaceCreateDeviceRGB;
    Context := CGBitmapContextCreate(MapRec.Data, Dest.Width, Dest.Height, 8,
      MapRec.Pitch, ColorSpace, kCGImageAlphaPremultipliedLast or kCGBitmapByteOrder32Big);
    if Context = nil then RaiseLastOSError;
    CGContextDrawImage(Context, CGRectMake(0, 0, Dest.Width, Dest.Height), Source);
  finally
    if Context <> nil then CGContextRelease(Context);
    if ColorSpace <> nil then CGColorSpaceRelease(ColorSpace);
    Dest.Unmap(MapRec);
  end;
end;

function TryLoadBitmapFromURL(Bitmap: TBitmap; const URL: NSURL): Boolean;
var
  ImageID: Pointer;
  Image: NSImage;
begin
  ImageID := TNSImage.Alloc.initWithContentsOfURL(URL);
  Result := (ImageID <> nil);
  Image := TNSImage.Wrap(ImageID);
  try
    CopyCGImageToBitmap(Image.CGImageForProposedRect(nil, nil, nil), Bitmap);
  finally
    Image.release;
  end;
end;

{ TClipboardHelper }

constructor TClipboardHelper.CreateForPasteboard(const APasteBoard: NSPasteboard);
begin
  inherited Create(TMacClipboardCore.Create(Self, APasteBoard));
end;

constructor TClipboardHelper.CreateForPasteboard(const AName: NSString);
begin
  CreateForPasteboard(TNSPasteboard.Wrap(TNSPasteboard.OCClass.pasteboardWithName(AName)));
end;

constructor TClipboardHelper.CreateForPasteboard(const AName: string);
begin
  CreateForPasteboard(StrToNSString(AName));
end;

function TClipboardHelper.GetPasteboard: NSPasteboard;
begin
  Result := (Core as IMacClipboardCore).Pasteboard;
end;

procedure TClipboardHelper.AssignNSObject(const Obj: NSObject);
begin
  AssignNSObject((Obj as ILocalObject).GetObjectID);
end;

procedure TClipboardHelper.AssignNSObject(const ObjID: Pointer);
begin
  Add(procedure
      begin
        (Core as IMacClipboardCore).AssignNSObject(ObjID);
      end);
end;

{$IF DECLARED(TAlphaColorClipper)}

function TAlphaColorClipper.CanLoadFromClipboard(const Clipboard: TClipboard): Boolean;
begin
  Result := Clipboard.HasFormat(cfColor)
end;

procedure TAlphaColorClipper.LoadFromClipboard(const Clipboard: TClipboard;
  const Callback: TGetInstancesCallback<TAlphaColor>);
begin
  LoadFromClipboard(Clipboard,
    procedure (const ColorF: TAlphaColorF; var LookForMore: Boolean)
    begin
      Callback(ColorF.ToAlphaColor, LookForMore);
    end);
end;

procedure TAlphaColorClipper.LoadFromClipboard(const Clipboard: TClipboard;
  const Callback: TGetInstancesCallback<TAlphaColorF>);
var
  Colors: NSArray;
  LookForMore: Boolean;
  I: NSUInteger;
  R, G, B, A: Single;
begin
  LookForMore := True;
  Colors := LoadNSObjectsFromPasteboard(Clipboard.Pasteboard, TNSColor.OCClass);
  for I := 0 to Colors.count - 1 do
  begin
    TNSColor.Wrap(Colors.objectAtIndex(I)).getRed(@R, @G, @B, @A);
    Callback(TAlphaColorF.Create(R, G, B, A), LookForMore);
    if not LookForMore then Exit;
  end;
end;

procedure TAlphaColorClipper.SaveToClipboard(const Clipboard: TClipboard;
  PreferDelayed: Boolean; const ColorGetter: TFunc<TAlphaColor>);
begin
  SaveToClipboard(Clipboard, PreferDelayed,
    function : TAlphaColorF
    begin
      Result := TAlphaColorF.Create(ColorGetter)
    end)
end;

procedure TAlphaColorClipper.SaveToClipboard(const Clipboard: TClipboard;
  PreferDelayed: Boolean; const ColorGetter: TFunc<TAlphaColorF>);
var
  F: TAlphaColorF;
begin
  F := ColorGetter;
  Clipboard.AssignNSObject(TNSColor.OCClass.colorWithCalibratedRed(F.R, F.G, F.B, F.A));
end;

class procedure TAlphaColorClipper.Register;
var
  Inst: TAlphaColorClipper;
begin
  Inst := Create;
  TClipboard.RegisterClipper<TAlphaColor>(Inst);
  TClipboard.RegisterClipper<TAlphaColorF>(Inst);
end;
{$IFEND}

{ TBitmapClipper }

class constructor TBitmapClipper.Create;
var
  ARPool: NSAutoreleasePool;
  KeyCallbacks: CFDictionaryKeyCallBacks;
  Keys: array[0..1] of CFStringRef;
  ValueCallbacks: CFDictionaryValueCallBacks;
  Values: array[0..1] of Pointer;
begin
  ARPool := TNSAutoreleasePool.Create;
  try
    Keys[0] := CFStringGetFrameworkConst(AppKitLib, 'NSPasteboardURLReadingFileURLsOnlyKey');
    Values[0] := TNSNumber.OCClass.numberWithBool(True);
    Keys[1] := CFStringGetFrameworkConst(AppKitLib, 'NSPasteboardURLReadingContentsConformToTypesKey');
    Values[1] := TNSArray.OCClass.arrayWithObject(kUTTypeImage);
    KeyCallbacks := kCFTypeDictionaryKeyCallBacks;
    ValueCallbacks := kCFTypeDictionaryValueCallBacks;
    FOptions := TCFDictionary.Wrap(CFDictionaryCreate(nil, @Keys, @Values,
      Length(Values), @KeyCallbacks, @ValueCallbacks), True);
  finally
    ARPool.drain;
  end;
end;

function TBitmapClipper.CanLoadFromClipboard(const Clipboard: TClipboard): Boolean;
var
  Ref: CFStringRef;
  S: string;
begin
  { In Yosemite at least there is an incompatibility between promised files and
    NSImage: 'Can't allocate a new block for a pasteboard. Creation
    of a new Pasteboard will fail.' }
  //Result := TNSImage.OCClass.canInitWithPasteboard(Clipboard.Pasteboard);
  Result := Clipboard.HasFormat([cfPNG, cfTIFF, cfPDF, cfJPEG, cfAppleICNS, cfGIF, cfTGA]);
  if Result then Exit;
  for S in Clipboard.GetFileNames do
    if HasBitmapFileExt(S) then Exit(True);
  for S in Clipboard.GetVirtualFileDescriptors do
  begin
    Ref := CFStringCreateWithCharacters(nil, PChar(S), Length(S));
    try
      if UTTypeConformsTo(Ref, kUTTypeImage) then Exit(True);
    finally
      CFRelease(Ref);
    end;
  end;
end;

procedure TBitmapClipper.LoadFromClipboard(const Clipboard: TClipboard;
  const Callback: TLoadObjectsCallback<TBitmap>);
var
  AssignToProc: TProc<TBitmap>;
  Bitmap: TBitmap;
  Data: NSData;
  DataType, Descriptor: NSString;
  I: Integer;
  Items: NSArray;
  LookForMore: Boolean;
  ObjID: Pointer;
  Obj: NSObject;
  Pasteboard: NSPasteboard;
  PBItem: NSPasteboardItem;
  PBoardRef: PasteboardRef;
  Stream: TStream;
  TempDir: string;
begin
  TempDir := TPath.GetTempPath;
  PBoardRef := nil;
  Bitmap := TBitmap.Create;
  try
    Pasteboard := Clipboard.Pasteboard;
    { go for a NSURL first because when a single image file is copied in Finder, its icon is
      added to the clipboard as well as its path }
    Items := LoadNSObjectsFromPasteboard(Pasteboard,
      [TNSURL.OCClass, TNSImage.OCClass, TNSPasteboardItem.OCClass], FOptions.ObjC);
    if Items = nil then Exit;
    AssignToProc :=
      procedure (Dest: TBitmap)
      begin
        Dest.Assign(Bitmap);
      end;
    LookForMore := True;
    for I := 0 to Items.count - 1 do
    begin
      ObjID := Items.objectAtIndex(I);
      Obj := TNSObject.Wrap(ObjID);
      if NSInheritsFrom(Obj, TNSImage.OCClass) then
        CopyCGImageToBitmap(TNSImage.Wrap(ObjID).CGImageForProposedRect(nil, nil, nil), Bitmap)
      else if NSInheritsFrom(Obj, TNSURL.OCClass) then
      begin
        if not TryLoadBitmapFromURL(Bitmap, TNSURL.Wrap(ObjID)) then
          Continue;
      end
      else
      begin
        PBItem := TNSPasteboardItem.Wrap(ObjID);
        //try for suitable in-memory image formats
        DataType := PBItem.availableTypeFromArray(ClipboardFormatsToNSArray([cfPNG, cfTIFF]));
        if DataType <> nil then
        begin
          Data := PBItem.dataForType(DataType);
          if Data = nil then Continue;
          Stream := TUserMemoryStream.Create(Data.bytes, Data.length);
          try
            Bitmap.LoadFromStream(Stream);
          finally
            Stream.Free;
          end;
        end
        else
        begin
          //try for a virtual image file
          Descriptor := PBItem.stringForType(cfVirtualFileDescriptor.ToNSString);
          if not UTTypeConformsTo((Descriptor as ILocalObject).GetObjectID, kUTTypeImage) then Continue;
          if PBoardRef = nil then
            OSStatusCheck(PasteboardCreate((Pasteboard.name as ILocalObject).GetObjectID, PBoardRef));
          if not TryLoadBitmapFromURL(Bitmap, SaveVirtualFile(PBoardRef, PBItem, TempDir)) then Continue;
        end;
      end;
      Callback(AssignToProc, LookForMore);
      if not LookForMore then Exit;
    end;
  finally
    CFReleaseAndNil(PBoardRef);
    Bitmap.Free;
    AssignToProc := nil;
  end;
end;

procedure TBitmapClipper.SaveToClipboard(const Clipboard: TClipboard;
  PreferDelayed: Boolean; const BitmapGetter: TFunc<TBitmap>);
begin
  if PreferDelayed then
    Clipboard.AssignDelayed(cfPNG,
      function : TBytes
      begin
        Result := BytesOf(BitmapGetter() as IStreamPersist);
      end)
  else
    Clipboard.Assign(cfPNG, BitmapGetter() as IStreamPersist);
end;

{$IFDEF ImplDragAndDrop}

{$IF NOT DECLARED(BitmapToMacBitmap)}
function BitmapToMacBitmap(Bitmap: TBitmap): NSImage;
var
  Stream: TMemoryStream;
  Data: NSData;
begin
  Stream := TMemoryStream.Create;
  try
    Bitmap.SaveToStream(Stream);
    Data := TNSData.Wrap(TNSData.OCClass.dataWithBytes(Stream.Memory, Stream.Size));
    Result := TNSImage.Wrap(TNSImage.Alloc.initWithData(Data));
  finally
    Stream.Free;
  end;
end;
{$IFEND}

type
  TMacClipboardDragDrop = class(TInterfacedObject, IFMXClipboardDragDropService, IClipboardDropInfo)
  strict private class var
    FCurrentClipboard: TClipboard;
    FCurrentForm: TCommonCustomForm;
    FDragOperationField: TRttiField;
    FSwizzledFMXWindow: Boolean;
    class destructor Destroy;
    class procedure NeedDragOperationField(const MWH: TMacWindowHandle); static;
    //swizzled-in method implementations
    class function draggingEntered(SelfID: Pointer; Selector: SEL; Sender: Pointer): NSDragOperation; cdecl; static;
    class function draggingUpdated(SelfID: Pointer; Selector: SEL; Sender: Pointer): NSDragOperation; cdecl; static;
    class procedure draggingExited(SelfID: Pointer; Selector: SEL; Sender: Pointer); cdecl; static;
    class function performDragOperation(SelfID: Pointer; Selector: SEL; Sender: Pointer): Boolean; cdecl; static;
  protected type
    TInfo = record
      DragObject: TDragObject;
      Form: TCommonCustomForm;
      ScreenPos: TPointF;
    end;
  protected class var
    class procedure GetDragInfo(WndObjID, DraggingInfoObjID: Pointer;
      out Info: TInfo);
    class procedure SetDragOperationField(Form: TCommonCustomForm; Value: NSDragOperation); static;
    class property Clipboard: TClipboard read FCurrentClipboard;
  protected
    { IFMXClipboardDragDropService }
    procedure DoDrag(const Source: TObject; const Form: TCommonCustomForm;
      const Bitmap: TBitmap; const PrepareClipboard: TProc<TClipboard>);
    function RegisterForm(const Form: TCommonCustomForm): IClipboardDropInfo;
    procedure UnregisterForm(const Form: TCommonCustomForm);
    { IClipboardDropInfo }
    function GetClipboard: TClipboard;
  end;

class destructor TMacClipboardDragDrop.Destroy;
begin
  FCurrentClipboard.Free;;
end;

class procedure TMacClipboardDragDrop.NeedDragOperationField(const MWH: TMacWindowHandle);
var
  InstType: TRttiInstanceType;
begin
  if FDragOperationField <> nil then Exit;
  InstType := SharedContext.GetType(MWH.Handle.ClassType).AsInstance;
  FDragOperationField := InstType.GetField('DragOperation');
  Assert(FDragOperationField <> nil);
end;

class procedure TMacClipboardDragDrop.SetDragOperationField(Form: TCommonCustomForm;
  Value: NSDragOperation);
var
  MWH: TMacWindowHandle;
begin
  MWH := WindowHandleToPlatform(Form.Handle);
  NeedDragOperationField(MWH);
  FDragOperationField.SetValue(MWH.Handle, TValue.From(Value));
end;

procedure TMacClipboardDragDrop.DoDrag(const Source: TObject; const Form: TCommonCustomForm;
  const Bitmap: TBitmap; const PrepareClipboard: TProc<TClipboard>);
const
  Offset: NSSize = (width: 0; height: 0);
var
  AutoreleasePool: NSAutoreleasePool;
  Clipboard: TClipboard;
  Control: IControl;
  Event: NSEvent;
  MWH: TMacWindowHandle;
  Img: NSImage;
  LocalPt: TPointF;
  Pt: NSPoint;
  Scale: Single;
begin
  MWH := WindowHandleToPlatform(Form.Handle);
  { According to Apple's docs the event param to dragImage should be for a left mouse-down;
    if it isn't, then an access violation may result in an FMX application at least.
    Unfortunately, if a control is dragged before its form is explicitly given the focus,
    then such an access violation is just what happens with the stock FMX drag and drop code
    in XE7. This is why we just exit instead of raising an exception here... }
  Event := MWH.Wnd.currentEvent;
  if (Event = nil) or (Event.&type <> NSLeftMouseDown) then
    //raise EClipboardException.CreateRes(@SBeginClipboardDragMustBeCalledOnMouseDown);
    Exit;
  { broadly follow XE7's default implementation, but with a correct offset... }
  if not Supports(Source, IControl, Control) then Control := nil;
  AutoreleasePool := TNSAutoreleasePool.Create;
  try
    Img := BitmapToMacBitmap(Bitmap);
    Scale := (Form as IScene).GetSceneScale;
    if Scale > 1 then
      Img.setSize(CGSizeMake(Bitmap.Width / Scale, Bitmap.Height / Scale));
    Pt := Event.locationInWindow;
    if Control <> nil then
    begin
      { Cocoa's coordinate system has (0, 0) bottom left not top left }
      LocalPt := Control.ScreenToLocal(Form.ClientToScreen(PointF(Pt.x,
        MWH.View.bounds.size.height - Pt.y)));
      Pt.x := Pt.x - LocalPt.X;
      if Control is TControl then
        Pt.y := Pt.y + LocalPt.Y - TControl(Control).Height
      else
        Pt.y := Pt.y - (Img.size.height / 2);
    end
    else
    begin
      Pt.x := Pt.x - (Img.size.width / 2);
      Pt.y := Pt.y - (Img.size.height / 2);
    end;
    SetDragOperationField(Form, NSDragOperationNone);
    Clipboard := DragClipboard;
    PrepareClipboard(Clipboard);
    MWH.Wnd.dragImage(Img, Pt, Offset, Event, Clipboard.Pasteboard,
      (MWH.View as ILocalObject).GetObjectID, True);
    if (FDragOperationField.GetValue(MWH.Handle).AsInteger = NSDragOperationNone) and (Control <> nil) then
      Control.DragEnd;
  finally
    if Img <> nil then Img.release; //the NSImage returned by BitmapToMacBitmap is not auto-released
    AutoreleasePool.release;
  end;
end;

{$WARN SYMBOL_PLATFORM OFF}
class procedure TMacClipboardDragDrop.GetDragInfo(WndObjID, DraggingInfoObjID: Pointer;
  out Info: TInfo);
var
  DragInfo: NSDraggingInfo;
  mp: NSPoint;
begin
  DragInfo := TNSDraggingInfo.Wrap(DraggingInfoObjID);
  if FCurrentClipboard = nil then
  begin
    FCurrentForm := TMacWindowHandle.FindForm(TNSWindow.Wrap(WndObjID));
    FCurrentClipboard := TClipboard.CreateForPasteboard(DragInfo.draggingPasteboard);
  end;
  mp := DragInfo.draggingLocation;
  mp.y := WindowHandleToPlatform(FCurrentForm.Handle).View.bounds.size.height - mp.y;
  FCurrentClipboard.InitializeDragObject(Info.DragObject);
  Info.Form := FCurrentForm;
  Info.ScreenPos := FCurrentForm.ClientToScreen(TPointF(mp));
end;

class function TMacClipboardDragDrop.draggingEntered(SelfID: Pointer; Selector: SEL; Sender: Pointer): NSDragOperation; cdecl;
var
  AutoReleasePool: NSAutoreleasePool;
  Info: TMacClipboardDragDrop.TInfo;
begin
  Result := NSDragOperationEvery;
  try
    AutoReleasePool := TNSAutoreleasePool.Create;
    try
      GetDragInfo(SelfID, Sender, Info);
      Info.Form.DragEnter(Info.DragObject, Info.ScreenPos);
    finally
      AutoReleasePool.release;
    end;
  except
    ApplicationHandleException(Info.Form);
    Result := NSDragOperationNone;
  end;
end;

class function TMacClipboardDragDrop.draggingUpdated(SelfID: Pointer; Selector: SEL; Sender: Pointer): NSDragOperation; cdecl;
var
  AutoReleasePool: NSAutoreleasePool;
  Info: TMacClipboardDragDrop.TInfo;
  Operation: TDragOperation;
begin
  Result := NSDragOperationNone;
  try
    AutoReleasePool := TNSAutoreleasePool.Create;
    try
      Operation := TDragOperation.None;
      GetDragInfo(SelfID, Sender, Info);
      Info.Form.DragOver(Info.DragObject, Info.ScreenPos, Operation);
      case Operation of
        TDragOperation.None: Result := NSDragOperationNone;
        TDragOperation.Move: Result := NSDragOperationMove;
        TDragOperation.Copy: Result := NSDragOperationCopy;
        TDragOperation.Link: Result := NSDragOperationLink;
      end;
      SetDragOperationField(Info.Form, Result);
    finally
      AutoReleasePool.release;
    end;
  except
    ApplicationHandleException(Info.Form);
  end;
end;

class procedure TMacClipboardDragDrop.draggingExited(SelfID: Pointer; Selector: SEL; Sender: Pointer); cdecl;
var
  Info: TMacClipboardDragDrop.TInfo;
begin
  try
    GetDragInfo(SelfID, Sender, Info);
    Info.Form.DragLeave;
    FreeAndNil(FCurrentClipboard);
  except
    ApplicationHandleException(Info.Form);
  end;
end;

class function TMacClipboardDragDrop.performDragOperation(SelfID: Pointer; Selector: SEL; Sender: Pointer): Boolean; cdecl;
var
  AutoReleasePool: NSAutoreleasePool;
  Info: TMacClipboardDragDrop.TInfo;
begin
  Result := True;
  try
    AutoReleasePool := TNSAutoreleasePool.Create;
    try
      GetDragInfo(SelfID, Sender, Info);
      Info.Form.DragDrop(Info.DragObject, Info.ScreenPos);
      FreeAndNil(FCurrentClipboard);
    finally
      AutoReleasePool.release;
    end;
  except
    ApplicationHandleException(Info.Form);
    Result := False;
  end;
end;

type
  TOCLocalAccess = class(TOCLocal);

function TMacClipboardDragDrop.RegisterForm(const Form: TCommonCustomForm): IClipboardDropInfo;
var
  FMXWindow: TOCLocal;
  FMXWindowClassID: Pointer;

  procedure Swizzle(const SelectorName: PAnsiChar; const NewImpl: Pointer);
  var
    Selector: SEL;
  begin
    Selector := sel_getUid(SelectorName);
    method_setImplementation(class_getInstanceMethod(FMXWindowClassID, Selector), NewImpl);
  end;
begin
  if not FSwizzledFMXWindow then
  begin
    FSwizzledFMXWindow := True;
    FMXWindow := WindowHandleToPlatform(Form.Handle).Handle;
    FMXWindowClassID := object_getClass(TOCLocalAccess(FMXWindow).GetObjectID); //ref count may be zero, in which case an interface cast would lead to premature destruction...
    Swizzle('draggingEntered:', @draggingEntered);
    Swizzle('draggingUpdated:', @draggingUpdated);
    Swizzle('draggingExited:', @draggingExited);
    Swizzle('performDragOperation:', @performDragOperation);
  end;
  WindowHandleToPlatform(Form.Handle).Wnd.registerForDraggedTypes(
    TMacClipboardCore.AllClipboardTypeKinds);
  Result := Self;
end;

procedure TMacClipboardDragDrop.UnregisterForm(const Form: TCommonCustomForm);
begin
  WindowHandleToPlatform(Form.Handle).Wnd.unregisterDraggedTypes;
end;

function TMacClipboardDragDrop.GetClipboard: TClipboard;
begin
  Result := FCurrentClipboard;
end;
{$ENDIF}

initialization
  TClipboard.RegisterClipper<TBitmap>(TBitmapClipper.Create);
{$IF DECLARED(TAlphaColorClipper)}
  TAlphaColorClipper.Register;
{$IFEND}
{$IF DECLARED(TMacClipboardDragDrop)}
  TPlatformServices.Current.AddPlatformService(IFMXClipboardDragDropService, TMacClipboardDragDrop.Create);
{$IFEND}
{$IFEND}
end.
