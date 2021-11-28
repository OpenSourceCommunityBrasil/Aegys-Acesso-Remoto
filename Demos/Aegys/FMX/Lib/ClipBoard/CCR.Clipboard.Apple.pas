{**************************************************************************************}
{                                                                                      }
{ CCR.Clipboard - OS X and iOS backends. In same unit for convenience of client code.  }
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

unit CCR.Clipboard.Apple;

interface

{$IFDEF NEXTGEN}
{$LEGACYIFEND ON}
{$ENDIF}

{$IFDEF MACOS}
uses
  {$IFDEF IOS} {$IF RTLVersion < 27}{$Message Fatal 'TClipboard for iOS requires XE6 or later'}{$IFEND}
  iOSapi.CocoaTypes, iOSapi.CoreGraphics, iOSapi.Foundation, iOSapi.UIKit,
  {$ELSE}
  MacApi.CocoaTypes, MacApi.CoreGraphics, MacApi.Foundation, MacApi.AppKit,
  {$ENDIF}
  System.Math, System.SysUtils, System.Classes, System.Generics.Collections, System.UITypes,
  Posix.Dlfcn, Posix.UniStd, MacApi.ObjCRuntime, MacApi.ObjectiveC, MacApi.CoreFoundation,
  CCR.Clipboard.Consts, CCR.Clipboard, CCR.Clipboard.Apple.Helpers;

type
  TAppleClipboardCore = class(TClipboard.TCore)
  strict private class var
    FRegisteredTypes: CFMutableSetRef;
    class constructor InitializeClass;
    class destructor FinalizeClass;
  strict protected class var
    FFindClipboard: TClipboard;
    class function SameFormat(const Format1, Format2: TClipboardFormat): Boolean; override;
    //for notification of RegisterFormat calls with a name not registered before
    class procedure NewFormatRegistered(const Name: string; const Format: TClipboardFormat); virtual;
  private
    class function RegisterFormatInternal(const TypeRef: CFStringRef): TClipboardFormat; overload;
    class function RegisterFormatInternal(const TypeLiteral: MarshaledAString): TClipboardFormat; overload; inline;
    class property FindClipboard: TClipboard read FFindClipboard;
  protected class var
    cfAppleICNS, cfColor, cfPDF, cfTGA: TClipboardFormat;
  protected
    class procedure InitializeFormats(StdFormatValues: TClipboard.TStdFormatValues;
      var CustomFormatsSupported: Boolean); override;
    class function IsCompatibleVirtualFileDescriptor(const Descriptor: string;
      const SupportedFileExtsOrUTIs: TArray<string>): Boolean; override;
    class function GetFormatName(const Format: TClipboardFormat): string; override;
    class function GetFormatDescription(const Format: TClipboardFormat): string; override;
    class function RegisterFormat(const TypeRef: CFStringRef; TakeOwnership: Boolean): TClipboardFormat; reintroduce; overload;
    class function RegisterFormat(const Name: string): TClipboardFormat; overload; override;
  strict protected
    function GetNSURLs: NSArray; virtual; abstract;
  protected
    function ReadFileNames: TArray<string>;
    function ReadURLs: TArray<string>;
  end;

  TClipboardFormatHelper = record helper for TClipboardFormat
  strict private
    function GetHandle: CFStringRef; inline;
  public
    class function Wrap(const PredefinedType: CFStringRef): TClipboardFormat; overload; static; inline;
    class function Wrap(const PredefinedType: NSString): TClipboardFormat; overload; static; inline;
    class function Wrap(const UTI: string): TClipboardFormat; overload; static; inline;
    function IsUTI: Boolean; inline;
    function ToNSString: NSString; inline;
    property Handle: CFStringRef read GetHandle;
  end;

function cfAppleICNS: TClipboardFormat; inline;
function cfColor: TClipboardFormat; inline;
function cfPDF: TClipboardFormat; inline;
function cfTGA: TClipboardFormat; inline;

function ClipboardFormatsToNSArray(const AFormats: array of TClipboardFormat): NSArray;
function NSArrayToClipboardFormats(const Types: NSArray): TArray<TClipboardFormat>;

function FindClipboard: TClipboard; inline;

{$IFDEF iOS}
type
  IiOSClipboardCore = interface
  ['{CCF96610-2292-480E-A733-68BC2746E700}']
    procedure AssignNSObject(const Format: TClipboardFormat; const ObjID: Pointer);
    function GetPasteboard: UIPasteboard;
    property Pasteboard: UIPasteboard read GetPasteboard;
  end;

  TiOSClipboardCore = class(TAppleClipboardCore, IiOSClipboardCore, TClipboard.IReadWriteBytes,
    TClipboard.IReadWriteFileNames, TClipboard.IReadWriteURL, TClipboard.IMultipleFormatSets)
  strict private
    FChangeListener: NSObject;
    FCurrentItem: NSMutableDictionary;
    FCustomPasteboard: UIPasteboard;
    FToAdd: NSMutableArray;
    FOnNotifyChanges: TProc;
    function GetPasteboard: UIPasteboard; inline;
    procedure NeedItem(const NewIfHasFormat: array of TClipboardFormat);
    procedure AssignNSObject(const Format: TClipboardFormat; const ObjID: Pointer);
  strict protected
    class function ConformingFormat(const FormatToTest, BaseFormat: TClipboardFormat): Boolean; override;
    class procedure InitializeFormats(StdFormatValues: TClipboard.TStdFormatValues;
      var CustomFormatsSupported: Boolean); override;
    function GetNSURLs: NSArray; override;
    property OnNotifyChanges: TProc read FOnNotifyChanges;
    { TClipboard.TCore }
    function GetChangeCount: TClipboardChangeCount; override;
    function SupportsChangeNotifications: Boolean; override;
    procedure EnableChangeNotifications(const Callback: TThreadMethod); override;
    procedure DisableChangeNotifications; override;
    function WrapsSystemClipboard: Boolean; override;
    procedure DoClear; override;
    function DoOpen: Boolean; override;
    procedure DoClose; override;
    function GetFormats: TArray<TClipboardFormat>; override;
    function HasFormat(Format: TClipboardFormat): Boolean; override;
    function HasFormat(const Formats: array of TClipboardFormat; var Matched: TClipboardFormat): Boolean; override;
    function ReadPlainText: TArray<string>; override;
    procedure WritePlainText(const Renderer: TFunc<string>; PreferDelayed: Boolean); override;
    { TClipboard.IMultipleFormatSets }
    procedure NewFormatSet;
    procedure EnumBytes(const Format: TClipboardFormat; const Callback: TGetBytesCallback);
    procedure EnumFormatSets(const Callback: TEnumFormatSetsCallback);
    { TClipboard.IReadWriteBytes }
    function ReadBytes(const Format: TClipboardFormat): TBytes;
    procedure WriteBytes(const Format: TClipboardFormat;
      const Renderer: TFunc<TBytes>; PreferDelayed: Boolean); overload;
    { TClipboard.IReadWriteFileNames }
    procedure WriteFileName(const Renderer: TFunc<TFileName>; PreferDelayed: Boolean);
    { TClipboard.IReadWriteURL }
    procedure WriteURL(const Renderer: TFunc<string>; PreferDelayed: Boolean);
  public
    class function CreateClipboardForPasteboard(const Pasteboard: UIPasteboard): TClipboard;
    constructor Create(const AOwner: TClipboard; const ACustomPasteboard: UIPasteboard); reintroduce; overload;
    destructor Destroy; override;
    property Pasteboard: UIPasteboard read GetPasteboard;
  end;

  TiOSClipboardCoreClass = class of TiOSClipboardCore;

{$ELSE}

type
  TGetPasteboardDataEvent = reference to procedure (const NSType: NSString;
    const DestItem: NSPasteboardItem);

  {$M+}
  IPasteboardItemDataProvider = interface(NSPasteboardItemDataProvider)
  ['{8EA66AE3-A7D1-4139-951E-B4EFBC1089F2}']
    { standard NSPasteboardItemDataProvider }
    procedure pasteboard(pasteboard: NSPasteboard; item: NSPasteboardItem; provideDataForType: CFStringRef); cdecl;
    procedure pasteboardFinishedWithDataProvider(Sender: Pointer); cdecl;
    { internal }
    function count: Integer; cdecl;
    function types: NSArray; cdecl;
    function hasType(PBType: CFStringRef): Boolean; cdecl;
    function hasOutstandingPromises: Boolean; cdecl;
    procedure clearContents; cdecl;
    procedure resolvePromises(Item: NSPasteboardItem; forceAnyVirtualFile: Boolean); cdecl;
  end;

  TPasteboardItemDataProvider = class(TOCLocal, NSPasteboardItemDataProvider, IPasteboardItemDataProvider)
  strict private type
    TSource = class
    strict private
      FFormat: TClipboardFormat;
      FOnGetData: TGetPasteboardDataEvent;
    public
      constructor Create(const Format: TClipboardFormat; const OnGetData: TGetPasteboardDataEvent);
      property Format: TClipboardFormat read FFormat;
      property GetData: TGetPasteboardDataEvent read FOnGetData;
    end;
  strict private
    FToUpload: TObjectList<TSource>;
  private
    procedure AddFormat(const Format: TClipboardFormat; const OnGetData: TGetPasteboardDataEvent);
  public
    constructor Create; overload;
    constructor Create(const Format: TClipboardFormat; const OnGetData: TGetPasteboardDataEvent); overload;
    destructor Destroy; override;
    { NSPasteboardItemDataProvider }
    procedure pasteboard(pasteboard: NSPasteboard; item: NSPasteboardItem;
      provideDataForType: CFStringRef); cdecl;
    procedure pasteboardFinishedWithDataProvider(Sender: Pointer); cdecl;
    { IPasteboardItemDataProvider }
    function count: Integer; cdecl;
    function types: NSArray; cdecl;
    function hasType(PBType: CFStringRef): Boolean; cdecl;
    function hasOutstandingPromises: Boolean; cdecl;
    procedure clearContents; cdecl;
    procedure resolvePromises(Item: NSPasteboardItem; forceAnyVirtualFile: Boolean); cdecl;
  end;
  {$M-}

  IMacClipboardCore = interface
  ['{44B2B2E1-1F2F-429D-A9CB-118F4252302F}']
    procedure AssignNSObject(const ObjID: Pointer);
    function GetPasteboard: NSPasteboard;
    property Pasteboard: NSPasteboard read GetPasteboard;
  end;

  TMacClipboardCore = class(TAppleClipboardCore, IMacClipboardCore, TClipboard.IReadWriteBytes,
    TClipboard.IReadWriteFileNames, TClipboard.IReadWriteURL, TClipboard.IReadWriteVirtualFile,
    TClipboard.IMultipleFormatSets, TClipboard.IDelayedRendering)
  strict private class var
    FAllClipboardTypeKinds: ICFMutableArray; //workaround for class destructors called after variable already nil'ed
    FDragClipboard, FFontClipboard, FRulerClipboard: TClipboard;
    class destructor FinalizeClass;
    class function GetAllClipboardTypeKinds: NSMutableArray; static;
  strict private
    FChangeNotifyCallback: TThreadMethod;
    FCustomPasteboard: NSPasteboard;
    FCurrentProvider: TPasteboardItemDataProvider;
    FLastChangeCount: NSInteger;
    FProviders: TList<IPasteboardItemDataProvider>;
    FProvidedItems: array of NSPasteboardItem;
    FNSObjectIDsToUpload: TList<Pointer>;
    FTimer: CFRunLoopTimerRef;
    class procedure CheckForChangesTimer(timer: CFRunLoopTimerRef; info: Pointer); cdecl; static;
    procedure CheckForChanges;
    procedure AddFormat(const Format: TClipboardFormat;
      const OnGetData: TGetPasteboardDataEvent);
    procedure NSObjectIDsToUploadNotify(Sender: TObject; const Item: Pointer;
      Action: TCollectionNotification);
    procedure DoEnumVirtualFiles(DescriptorsOnly: Boolean; const Callback: TSaveVirtualFilesCallback);
  private class var
    cfRTFD, cfTabularText, cfFont, cfRuler, cfSound, cfMultipleTextSelection,
    cfFindPanelSearchOptions, cfFileURLPromise, cfFilePromiseContent: TClipboardFormat;
    class property DragClipboard: TClipboard read FDragClipboard;
    class property FontClipboard: TClipboard read FFontClipboard;
    class property RulerClipboard: TClipboard read FRulerClipboard;
  strict protected
    class function ConformingFormat(const FormatToTest, BaseFormat: TClipboardFormat): Boolean; override;
    class procedure InitializeFormats(StdFormatValues: TClipboard.TStdFormatValues;
      var CustomFormatsSupported: Boolean); override;
    class procedure NewFormatRegistered(const Name: string; const Format: TClipboardFormat); override;
    function GetNSURLs: NSArray; override;
    { IMacClipboardCore }
    procedure AssignNSObject(const ObjID: Pointer);
    function GetPasteboard: NSPasteboard;
    { TClipboard.TCore }
    constructor Create(const AOwner: TClipboard); overload; override;
    function SupportsChangeNotifications: Boolean; override;
    procedure EnableChangeNotifications(const Callback: TThreadMethod); override;
    procedure DisableChangeNotifications; override;
    function GetChangeCount: TClipboardChangeCount; override;
    function WrapsSystemClipboard: Boolean; override;
    function GetFormats: TArray<TClipboardFormat>; override;
    function HasFormat(Format: TClipboardFormat): Boolean; override;
    function HasFormat(const Formats: array of TClipboardFormat; var Matched: TClipboardFormat): Boolean; override;
    function DoOpen: Boolean; override;
    procedure DoClose; override;
    procedure DoClear; override;
    function ReadPlainText: TArray<string>; override;
    procedure WritePlainText(const Renderer: TFunc<string>; PreferDelayed: Boolean); override;
    { TClipboard.IDelayedRendering }
    function HasOutstandingPromisesToOS: Boolean;
    procedure ResolveOutstandingPromisesToOS(IsExplicitlyRequested: Boolean);
    procedure CancelOutstandingPromisesToOS;
    { TClipboard.IReadWriteBytes }
    function ReadBytes(const Format: TClipboardFormat): TBytes;
    procedure WriteBytes(const Format: TClipboardFormat;
      const Renderer: TFunc<TBytes>; PreferDelayed: Boolean); overload;
    { TClipboard.IReadWriteFileNames }
    procedure WriteFileName(const Renderer: TFunc<TFileName>; PreferDelayed: Boolean);
    { TClipboard.IReadWriteURL }
    procedure WriteURL(const Renderer: TFunc<string>; PreferDelayed: Boolean);
    { TClipboard.IReadWriteVirtualFile }
    function ReadVirtualFileDescriptors: TArray<string>;
    procedure ReadVirtualFiles(const Callback: TEnumVirtualFilesStreamCallback);
    procedure SaveVirtualFiles(const Callback: TSaveVirtualFilesCallback);
    procedure WriteVirtualFile(const Details: TVirtualFileDetails;
      const Renderer: TProc<TStream>; PreferDelayed: Boolean);
    { TClipboard.IMultipleFormatSets }
    procedure NewFormatSet;
    procedure EnumBytes(const Format: TClipboardFormat; const Callback: TGetBytesCallback);
    procedure EnumFormatSets(const Callback: TEnumFormatSetsCallback);
  public
    constructor Create(const AOwner: TClipboard; const ACustomPasteboard: NSPasteboard); reintroduce; overload;
    destructor Destroy; override;
    property Pasteboard: NSPasteboard read GetPasteboard;
    class property AllClipboardTypeKinds: NSMutableArray read GetAllClipboardTypeKinds;
  end;

function LoadNSObjectsFromPasteboard(const Pasteboard: NSPasteboard; const NSClass: NSObjectClass): NSArray; overload;
function LoadNSObjectsFromPasteboard(const Pasteboard: NSPasteboard;
  const NSClasses: array of NSObjectClass; const Options: NSDictionary = nil): NSArray; overload;

function DragClipboard: TClipboard; inline;
function FontClipboard: TClipboard; inline;
function RulerClipboard: TClipboard; inline;

function cfRTFD: TClipboardFormat; inline;
function cfTabularText: TClipboardFormat; inline;
function cfFont: TClipboardFormat; inline;
function cfRuler: TClipboardFormat; inline;
function cfSound: TClipboardFormat; inline;
function cfMultipleTextSelection: TClipboardFormat; inline;
function cfFindPanelSearchOptions: TClipboardFormat; inline;
function cfFileURLPromise: TClipboardFormat; inline;
function cfFilePromiseContent: TClipboardFormat; inline;
{$ENDIF}

{$ENDIF}

implementation

{$IFDEF MACOS}
uses
  System.RTLConsts, System.IOUtils;

function ClipboardFormatsToNSArray(const AFormats: array of TClipboardFormat): NSArray;
begin
  Result := TNSArray.Wrap(TNSArray.OCClass.arrayWithObjects(@AFormats[0], Length(AFormats)));
end;

function NSArrayToClipboardFormats(const Types: NSArray): TArray<TClipboardFormat>;
var
  I: Integer;
begin
  if Types = nil then Exit(nil);
  SetLength(Result, Types.count);
  { can't just copy the CFStringRefs directly because caller may wish to hold
    onto the array -> retain counts }
  for I := 0 to High(Result) do
    Result[I] := TAppleClipboardCore.RegisterFormat(Types.objectAtIndex(I), False)
end;

function cfAppleICNS: TClipboardFormat;
begin
  Result := TAppleClipboardCore.cfAppleICNS;
end;

function cfColor: TClipboardFormat;
begin
  Result := TAppleClipboardCore.cfColor;
end;

function cfPDF: TClipboardFormat;
begin
  Result := TAppleClipboardCore.cfPDF;
end;

function cfTGA: TClipboardFormat;
begin
  Result := TAppleClipboardCore.cfTGA;
end;

function FindClipboard: TClipboard;
begin
  Result := TAppleClipboardCore.FindClipboard;
end;

{ TAppleClipboardCore }

function SetCompareFunc(Str1, Str2: Pointer): Boolean; cdecl;
begin
  Result := CFStringCompare(Str1, Str2, kCFCompareCaseInsensitive) = 0;
end;

class constructor TAppleClipboardCore.InitializeClass;
var
  Callbacks: CFSetCallBacks;
begin
  Callbacks := kCFTypeSetCallBacks;
  Callbacks.equal := SetCompareFunc;
  FRegisteredTypes := CFSetCreateMutable(nil, 0, @Callbacks);
end;

class destructor TAppleClipboardCore.FinalizeClass;
begin
  CFReleaseAndNil(FRegisteredTypes);
  FFindClipboard.Free;
end;

function CFStringContains(const StrToSearch, StrToFind: CFStringRef): Boolean; inline;
begin
  Result := (CFStringFind(StrToSearch, StrToFind, 0).length > 0);
end;

class function TAppleClipboardCore.SameFormat(const Format1, Format2: TClipboardFormat): Boolean;
begin
  if (Format1.Handle = nil) or (Format2.Handle = nil) then Exit(False);
  Result := UTTypeEqual(Format1.Handle, Format2.Handle);
  if not Result and not Format1.IsUTI and not Format2.IsUTI then
    Result := (CFStringCompare(Format1.Handle, Format2.Handle,
      kCFCompareCaseInsensitive) = kCFCompareEqualTo);
end;

class procedure TAppleClipboardCore.NewFormatRegistered(const Name: string; const Format: TClipboardFormat);
begin
end;

class procedure TAppleClipboardCore.InitializeFormats(
  StdFormatValues: TClipboard.TStdFormatValues; var CustomFormatsSupported: Boolean);
begin
  StdFormatValues.cfUTF8Text := RegisterFormatInternal(kUTTypeUTF8PlainText);
  StdFormatValues.cfUnicodeText := RegisterFormatInternal(kUTTypeUTF16PlainText);
  StdFormatValues.cfRTF := RegisterFormatInternal(kUTTypeRTF);
  StdFormatValues.cfGIF := RegisterFormatInternal(kUTTypeGIF);
  StdFormatValues.cfJPEG := RegisterFormatInternal(kUTTypeJPEG);
  StdFormatValues.cfPNG := RegisterFormatInternal(kUTTypePNG);
  StdFormatValues.cfTIFF := RegisterFormatInternal(kUTTypeTIFF);
  StdFormatValues.cfFileName := RegisterFormatInternal(kUTTypeFileURL);
  StdFormatValues.cfURL := RegisterFormatInternal(kUTTypeURL);
  //use UTI format rather than generic defaults taken from the VCL
  StdFormatValues.cfComponent := RegisterFormatInternal('com.embarcadero.dfm');
  StdFormatValues.cfComponents := RegisterFormatInternal('com.embarcadero.dfms');
  cfAppleICNS := RegisterFormatInternal(kUTTypeAppleICNS);
  cfPDF := RegisterFormatInternal(kUTTypePDF);
  cfTGA := RegisterFormatInternal('com.truevision.tga-image');
  CustomFormatsSupported := True;
end;

class function TAppleClipboardCore.IsCompatibleVirtualFileDescriptor(const Descriptor: string;
  const SupportedFileExtsOrUTIs: TArray<string>): Boolean;
var
  Ext: string;
  InUTI, BaseUTI: CFStringRef;
begin
  BaseUTI := nil;
  InUTI := CFStringCreateWithString(Descriptor);
  try
    for Ext in SupportedFileExtsOrUTIs do
    begin
      CFReleaseAndNil(BaseUTI);
      if PChar(Ext)^ = '.' then
        BaseUTI := CFStringCreateUTIForFileExt(Ext)
      else
        BaseUTI := CFStringCreateWithString(Ext);
      Result := UTTypeConformsTo(InUTI, BaseUTI);
      if Result then Exit;
    end;
  finally
    CFReleaseAndNil(InUTI);
  end;
  Result := False;
end;

class function TAppleClipboardCore.GetFormatName(const Format: TClipboardFormat): string;
begin
  Result := CFStringGetValue(CFStringRef(Format));
end;

class function TAppleClipboardCore.GetFormatDescription(const Format: TClipboardFormat): string;
begin
  Result := CFStringGetValue(UTTypeCopyDescription(CFStringRef(Format)));
end;

class function TAppleClipboardCore.RegisterFormatInternal(const TypeRef: CFStringRef): TClipboardFormat;
begin
  CFSetAddValue(FRegisteredTypes, TypeRef);
  Result := TClipboardFormat.Wrap(TypeRef);
end;

class function TAppleClipboardCore.RegisterFormatInternal(const TypeLiteral: MarshaledAString): TClipboardFormat;
begin
  Result := RegisterFormatInternal(__CFStringMakeConstantString(TypeLiteral));
end;

class function TAppleClipboardCore.RegisterFormat(const TypeRef: CFStringRef;
  TakeOwnership: Boolean): TClipboardFormat;
begin
  try
    if CFSetGetValueIfPresent(CFSetRef(FRegisteredTypes), TypeRef, @Result) then
      Exit;
    Result := RegisterFormatInternal(TypeRef);
  finally
    if TakeOwnership then CFRelease(TypeRef);
  end;
  NewFormatRegistered(CFStringGetValue(TypeRef), Result);
end;

class function TAppleClipboardCore.RegisterFormat(const Name: string): TClipboardFormat;
begin
  Result := RegisterFormat(CFStringCreateWithString(Name), True);
end;

function TAppleClipboardCore.ReadFileNames: TArray<string>;
var
  NSArr: NSArray;
  URL: NSURL;
  Count, I: Integer;
begin
  NSArr := GetNSURLs;
  if NSArr = nil then Exit(nil);
  SetLength(Result, NSArr.count);
  Count := 0;
  for I := 0 to High(Result) do
  begin
    URL := TNSURL.Wrap(NSArr.objectAtIndex(I));
    if URL.isFileURL then
    begin
      Result[Count] := NSStringGetValue(URL.path); //!!!may need to switch to using CFURLCopyFileSystemPath
      Inc(Count);
    end;
  end;
  SetLength(Result, Count);
end;

function TAppleClipboardCore.ReadURLs: TArray<string>;
var
  I: Integer;
  NSArr: NSArray;
begin
  NSArr := GetNSURLs;
  if NSArr = nil then Exit(nil);
  SetLength(Result, NSArr.count);
  for I := 0 to High(Result) do
    Result[I] := NSStringGetValue(TNSURL.Wrap(NSArr.objectAtIndex(I)).absoluteString);
end;

{ TClipboardFormatHelper }

class function TClipboardFormatHelper.Wrap(const PredefinedType: CFStringRef): TClipboardFormat;
begin
  Result := TClipboardFormat(PredefinedType);
end;

class function TClipboardFormatHelper.Wrap(const PredefinedType: NSString): TClipboardFormat;
begin
  Result := TClipboardFormat((PredefinedType as ILocalObject).GetObjectID);
end;

class function TClipboardFormatHelper.Wrap(const UTI: string): TClipboardFormat;
begin
  Result := TAppleClipboardCore.RegisterFormat(UTI);
end;

function TClipboardFormatHelper.GetHandle: CFStringRef;
begin
  Result := CFStringRef(Self);
end;

function TClipboardFormatHelper.IsUTI: Boolean;
begin
  Result := IsValidUTIString(Handle);
end;

function TClipboardFormatHelper.ToNSString: NSString;
begin
  Result := TNSString.Wrap(Handle);
end;

{$IFDEF iOS}
type
{$M+}
  IChangeListener = interface(IObjectiveC)
  ['{7F7F54AE-FECD-4F2A-AEBA-4E2F167CD46F}']
    procedure pasteboardChanged(notification: Pointer); cdecl;
  end;

  TChangeListener = class(TOCLocal, IChangeListener)
  strict private
    FCallback: TProc;
  public
    constructor Create(const Callback: TProc);
    procedure pasteboardChanged(notification: Pointer); cdecl;
  end;
{$M-}

constructor TChangeListener.Create(const Callback: TProc);
begin
  inherited Create;
  FCallback := Callback;
end;

procedure TChangeListener.pasteboardChanged(notification: Pointer);
begin
  FCallback();
end;

{ TiOSClipboardCore }

type
  TAdditionalClipboard = class(TClipboard)
  public
    constructor Create(const MinCoreClass: TiOSClipboardCoreClass;
      const Pasteboard: UIPasteboard);
  end;

constructor TAdditionalClipboard.Create(const MinCoreClass: TiOSClipboardCoreClass;
  const Pasteboard: UIPasteboard);
var
  LClass: TiOSClipboardCoreClass;
begin
  LClass := TiOSClipboardCoreClass(GetSuitableCoreClass(TiOSClipboardCore));
  inherited Create(LClass.Create(Self, Pasteboard));
end;

class function TiOSClipboardCore.CreateClipboardForPasteboard(const Pasteboard: UIPasteboard): TClipboard;
begin
  Result := TAdditionalClipboard.Create(Self, Pasteboard);
end;

class function TiOSClipboardCore.ConformingFormat(const FormatToTest, BaseFormat: TClipboardFormat): Boolean;
begin
  if (FormatToTest.Handle = nil) or (BaseFormat.Handle = nil) then
    Result := False
  else
    if FormatToTest.IsUTI then
      if BaseFormat.IsUTI then
        Result := UTTypeConformsTo(FormatToTest.Handle, BaseFormat.Handle)
      else
        Result := False
    else
      if BaseFormat.IsUTI then
        Result := False
      else
        Result := SameFormat(FormatToTest, BaseFormat);
end;

{$IF NOT DECLARED(UIPasteboardNameFind)}
const
  UIKitFwk = '/System/Library/Frameworks/UIKit.framework/UIKit';

function UIPasteboardNameFind: NSString;
begin
  Result := CocoaNSStringConst(UIKitFwk, 'UIPasteboardNameFind');
end;

function UIPasteboardChangedNotification: NSString;
begin
  Result := CocoaNSStringConst(UIKitFwk, 'UIPasteboardChangedNotification');
end;
{$IFEND}

class procedure TiOSClipboardCore.InitializeFormats(
  StdFormatValues: TClipboard.TStdFormatValues; var CustomFormatsSupported: Boolean);
begin
  inherited;
  cfColor := RegisterFormatInternal('com.apple.uikit.color');
  FFindClipboard := CreateClipboardForPasteboard(
    TUIPasteboard.Wrap(TUIPasteboard.OCClass.pasteboardWithName(UIPasteboardNameFind, False)));
end;

constructor TiOSClipboardCore.Create(const AOwner: TClipboard; const ACustomPasteboard: UIPasteboard);
begin
  if ACustomPasteboard = nil then
    raise EArgumentNilException.CreateRes(@SArgumentNil);
  Create(AOwner);
  FCustomPasteboard := ACustomPasteboard;
  FCustomPasteboard.retain;
end;

destructor TiOSClipboardCore.Destroy;
begin
  if FCustomPasteboard <> nil then FCustomPasteboard.release;
  inherited;
end;

function TiOSClipboardCore.GetChangeCount: TClipboardChangeCount;
begin
  Result := TClipboardChangeCount(Pasteboard.changeCount);
end;

function TiOSClipboardCore.WrapsSystemClipboard: Boolean;
begin
  Result := (FCustomPasteboard = nil);
end;

function TiOSClipboardCore.SupportsChangeNotifications: Boolean;
begin
  Result := True;
end;

procedure TiOSClipboardCore.EnableChangeNotifications(const Callback: TThreadMethod);
var
  NotifyCentre: NSNotificationCenter;
begin
  FOnNotifyChanges := Callback;
  //god the Objective-C bridge syntax is horrible...
  FChangeListener := TNSObject.Wrap(TChangeListener.Create(Callback).GetObjectID);
  NotifyCentre := TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter);
  NotifyCentre.addObserver((FChangeListener as ILocalObject).GetObjectID,
    sel_getUid('pasteboardChanged:'),
    (UIPasteboardChangedNotification as ILocalObject).GetObjectID,
    (Pasteboard as ILocalObject).GetObjectID);
end;

procedure TiOSClipboardCore.DisableChangeNotifications;
begin
  FOnNotifyChanges := nil;
  TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).removeObserver(
    (FChangeListener as ILocalObject).GetObjectID);
  FChangeListener.release;
  FChangeListener := nil;
end;

function TiOSClipboardCore.GetPasteboard: UIPasteboard;
begin
  if FCustomPasteboard = nil then
    Result := TUIPasteboard.Wrap(TUIPasteboard.OCClass.generalPasteboard)
  else
    Result := FCustomPasteboard;
end;

function TiOSClipboardCore.DoOpen: Boolean;
begin
  FToAdd := TNSMutableArray.Wrap(CFArrayCreateMutable(nil, 4, nil));
  Result := True;
end;

procedure TiOSClipboardCore.DoClear;
begin
  Pasteboard.setItems(nil);
  FToAdd.removeAllObjects;
  FCurrentItem := nil;
end;

procedure TiOSClipboardCore.DoClose;
begin
  try
    if FToAdd.count <> 0 then Pasteboard.setItems(FToAdd);
  finally
    FCurrentItem := nil;
    FToAdd.release;
    FToAdd := nil;
  end;
end;

{ reading }

function TiOSClipboardCore.GetNSURLs: NSArray;
begin
  Result := Pasteboard.URLs;
end;

function TiOSClipboardCore.ReadBytes(const Format: TClipboardFormat): TBytes;
begin
  Result := NSDataToBytes(Pasteboard.dataForPasteboardType(Format.ToNSString));
end;

procedure TiOSClipboardCore.EnumBytes(const Format: TClipboardFormat;
  const Callback: TGetBytesCallback);
var
  ContinueEnum: Boolean;
  Pasteboard: UIPasteboard;
  I: Integer;
  Values: NSArray;
begin
  ContinueEnum := True;
  Pasteboard := GetPasteboard;
  Values := Pasteboard.dataForPasteboardType(Format.ToNSString, nil);
  if Values <> nil then
    for I := 0 to Values.count - 1 do
    begin
      Callback(NSDataToBytes(TNSData.Wrap(Values.objectAtIndex(I))), ContinueEnum);
      if not ContinueEnum then Exit;
    end;
end;

procedure TiOSClipboardCore.EnumFormatSets(const Callback: TEnumFormatSetsCallback);
var
  LookForMore: Boolean;
  GetFormatBytes: TFunc<TClipboardFormat, TBytes>;
  I: NSUInteger;
  Items: NSArray;
  Pasteboard: UIPasteboard;
begin
  LookForMore := True;
  Pasteboard := GetPasteboard;
  Items := Pasteboard.pasteboardTypesForItemSet(nil);
  if Items = nil then Exit;
  GetFormatBytes :=
    function (ClipFormat: TClipboardFormat): TBytes
    var
      DataArr: NSArray;
    begin
      DataArr := Pasteboard.dataForPasteboardType(ClipFormat.ToNSString,
        TNSIndexSet.Wrap(TNSIndexSet.OCClass.indexSetWithIndex(I)));
      if (DataArr = nil) or (DataArr.count = 0) then
        Result := nil
      else
        Result := NSDataToBytes(TNSData.Wrap(DataArr.objectAtIndex(0)));
    end;
  for I := 0 to Items.count - 1 do
  begin
    Callback(NSArrayToClipboardFormats(TNSArray.Wrap(Items.objectAtIndex(I))),
      GetFormatBytes, LookForMore);
    if not LookForMore then Break;
  end;
  GetFormatBytes := nil;
end;

function NSArrayToStrings(const ArrRef: CFArrayRef): TArray<string>; overload;
var
  I: Integer;
begin
  SetLength(Result, CFArrayGetCount(ArrRef));
  for I := 0 to High(Result) do
    Result[I] := CFStringGetValue(CFArrayGetValueAtIndex(ArrRef, I));
end;

function NSArrayToStrings(const Arr: NSArray): TArray<string>; overload;
begin
  Result := NSArrayToStrings((Arr as ILocalObject).GetObjectID)
end;

function TiOSClipboardCore.ReadPlainText: TArray<string>;
begin
  Result := NSArrayToStrings(GetPasteboard.strings);
end;

function TiOSClipboardCore.GetFormats: TArray<TClipboardFormat>;
  procedure CanonizeResult;
  var
    I: Integer;
  begin
    for I := Low(Result) to High(Result) do
      Result[I] := RegisterFormat(Result[I].Handle, False);
  end;
var
  Dicts: array of CFDictionaryRef;
  FormatSet: Pointer;
  KeyCount: Integer;
  Keys: array of CFStringRef;
  I, J: NSUInteger;
  Items: NSArray;
  Range: NSRange;
  SetCallbacks: CFSetCallBacks;
begin
  { UIPasteboard.pasteboardTypes is documented to only return the first item's types.
    Which is a pain... }
  Items := Pasteboard.items;
  Range.location := 0;
  Range.length := Items.count;
  if Range.length = 0 then Exit(nil);
  SetLength(Dicts, Range.length);
  Items.getObjects(Dicts, Range);
  //if there's only one dictionary then output its keys to Result and exit
  if Range.length = 1 then
  begin
    SetLength(Result, CFDictionaryGetCount(Dicts[0]));
    CFDictionaryGetKeysAndValues(Dicts[0], Result, nil);
    CanonizeResult;
    Exit;
  end;
  //otherwise build up a set of keys
  SetCallbacks := kCFTypeSetCallBacks;
  FormatSet := CFSetCreateMutable(nil, 0, @SetCallbacks);
  for I := 0 to Range.length - 1 do
  begin
    KeyCount := CFDictionaryGetCount(Dicts[I]);
    if Length(Keys) < KeyCount then
    begin
      Keys := nil;
      SetLength(Keys, KeyCount);
    end;
    CFDictionaryGetKeysAndValues(Dicts[I], Keys, nil);
    for J := 0 to KeyCount - 1 do
      CFSetAddValue(FormatSet, Keys[J]);
  end;
  //get the combined list
  SetLength(Result, CFSetGetCount(FormatSet));
  CFSetGetValues(FormatSet, Result);
  CanonizeResult;
end;

function TiOSClipboardCore.HasFormat(Format: TClipboardFormat): Boolean;
begin
  Result := Pasteboard.containsPasteboardTypes(ClipboardFormatsToNSArray([Format]), nil)
end;

function TiOSClipboardCore.HasFormat(const Formats: array of TClipboardFormat;
  var Matched: TClipboardFormat): Boolean;
var
  DictRef: CFDictionaryRef;
  Key, Possible: TClipboardFormat;
  Keys: array of TClipboardFormat;
  Indices: NSIndexSet;
  Pasteboard: UIPasteboard;
begin
  Pasteboard := GetPasteboard;
  Indices := Pasteboard.itemSetWithPasteboardTypes(ClipboardFormatsToNSArray(Formats));
  if (Indices = nil) or (Indices.count = 0) then Exit(False);
  DictRef := Pasteboard.items.objectAtIndex(Indices.firstIndex);
  SetLength(Keys, CFDictionaryGetCount(DictRef));
  CFDictionaryGetKeysAndValues(DictRef, Keys, nil);
  for Key in Keys do
    for Possible in Formats do
      if Key = Possible then
      begin
        Matched := Possible;
        Result := True;
        Exit;
      end;
  Result := False;
end;

{ writing }

procedure TiOSClipboardCore.NeedItem(const NewIfHasFormat: array of TClipboardFormat);
var
  F: TClipboardFormat;
  ID: Pointer;
begin
  if FCurrentItem <> nil then
  begin
    for F in NewIfHasFormat do
      if FCurrentItem.objectForKey(CFStringRef(F)) <> nil then
      begin
        FCurrentItem := nil;
        Break;
      end;
    if FCurrentItem <> nil then Exit;
  end;
  ID := TNSMutableDictionary.OCClass.dictionaryWithCapacity(Min(1, Length(NewIfHasFormat)));
  FToAdd.addObject(ID);
  FCurrentItem := TNSMutableDictionary.Wrap(ID);
end;

procedure TiOSClipboardCore.NewFormatSet;
begin
  FCurrentItem := nil;
end;

procedure TiOSClipboardCore.AssignNSObject(const Format: TClipboardFormat; const ObjID: Pointer);
begin
  NeedItem([Format]);
  FCurrentItem.setObject(ObjID, CFStringRef(Format));
end;

procedure TiOSClipboardCore.WriteBytes(const Format: TClipboardFormat;
  const Renderer: TFunc<TBytes>; PreferDelayed: Boolean);
var
  Bytes: TBytes;
begin
  Bytes := Renderer;
  AssignNSObject(Format, TNSData.OCClass.dataWithBytes(Bytes, Length(Bytes)));
end;

procedure TiOSClipboardCore.WriteFileName(const Renderer: TFunc<TFileName>; PreferDelayed: Boolean);
begin
  AssignNSObject(cfFileName, TNSURL.OCClass.fileURLWithPath(StrToNSString(Renderer)));
end;

procedure TiOSClipboardCore.WritePlainText(const Renderer: TFunc<string>; PreferDelayed: Boolean);
var
  S: string;
begin
  S := Renderer;
  AssignNSObject(cfUTF8Text, TNSString.OCClass.stringWithCharacters(PChar(S), Length(S)));
end;

procedure TiOSClipboardCore.WriteURL(const Renderer: TFunc<string>; PreferDelayed: Boolean);
begin
  AssignNSObject(cfURL, TNSURL.OCClass.URLWithString(StrToNSString(Renderer)));
end;
{$ELSE}

function MakeFileURLPromiseFunc(const Pasteboard: NSPasteboard;
  const Details: TVirtualFileDetails; const RenderProc: TProc<TStream>): TGetPasteboardDataEvent;
var
  LDetails: TVirtualFileDetails;
begin
  LDetails := Details;
  Result :=
    procedure (const NSType: NSString; const Item: NSPasteboardItem)
    var
      CBoard: PasteboardRef;
      DestPath, DestName, DestExt, FullName: string;
      CFStr: CFStringRef;
      I: Integer;
      Stream: TFileStream;
      URL: CFURLRef;
    begin
      CBoard := nil;
      OSStatusCheck(PasteboardCreate((Pasteboard.name as ILocalObject).GetObjectID, CBoard));
      try
        OSStatusCheck(PasteboardSynchronize(CBoard));
        { Allow for the paste location not being set to support ResolveOutstandingPromisesToOS }
        if PasteboardCopyPasteLocation(CBoard, URL) <> 0 then
          URL := nil;
        CFStr := nil;
        try
          if URL = nil then
            DestPath := IncludeTrailingPathDelimiter(TPath.GetTempPath)
          else
          begin
            { Path returned from Finder will be a 'file ref' rather than a POSIX-style path;
              CFURLCopyFileSystemPath will convert accordingly }
            CFStr := CFURLCopyFileSystemPath(URL, kCFURLPOSIXPathStyle);
            DestPath := IncludeTrailingPathDelimiter(CFStringGetValue(CFStr));
            CFReleaseAndNil(CFStr);
          end;
          DestName := ChangeFileExt(ExtractFileName(LDetails.FileName), '');
          DestExt := ExtractFileExt(LDetails.FileName);
          FullName := DestPath + DestName + DestExt;
          { Ensure we aren't overwriting anything }
          I := 1;
          while FileExists(FullName) do
          begin
            Inc(I);
            FullName := DestPath + DestName + Format(' (%d)', [I]) + DestExt;
          end;
          { Create the file }
          Stream := TFileStream.Create(FullName, fmCreate);
          try
            RenderProc(Stream);
          finally
            Stream.Free;
          end;
          { If requested, set file properties }
          if vfAttr in LDetails.AdditionalData then
          begin
            if LDetails.Attr and faReadOnly <> 0 then
              FileSetReadOnly(FullName, True);
          end;
          if vfCreationDate in LDetails.AdditionalData then
            if vfDatesAreUTC in LDetails.AdditionalData then
              TFile.SetCreationTimeUtc(FullName, LDetails.CreationDate)
            else
              TFile.SetCreationTime(FullName, LDetails.CreationDate);
          if vfLastAccessDate in LDetails.AdditionalData then
            if vfDatesAreUTC in LDetails.AdditionalData then
              TFile.SetLastAccessTimeUtc(FullName, LDetails.LastAccessDate)
            else
              TFile.SetLastAccessTime(FullName, LDetails.LastAccessDate);
          if vfLastWriteDate in LDetails.AdditionalData then
            if vfDatesAreUTC in LDetails.AdditionalData then
              TFile.SetLastWriteTimeUtc(FullName, LDetails.LastWriteDate)
            else
              TFile.SetLastWriteTime(FullName, LDetails.LastWriteDate);
          { Explicitly making good the promise isn't required for Finder, but other drag
            targets may want to know the destination actually used. }
          CFStr := CFStringCreateWithString(FullName);
          CFReleaseAndNil(URL);
          URL := CFURLCreateWithFileSystemPath(nil, CFStr, kCFURLPOSIXPathStyle, False);
          Item.setString(TNSString.Wrap(CFURLGetString(URL)), NSType);
        finally
          CFReleaseAndNil(CFStr);
          CFReleaseAndNil(URL);
        end;
      finally
        CFReleaseAndNil(CBoard);
      end;
    end;
end;

function LoadNSObjectsFromPasteboard(const Pasteboard: NSPasteboard; const NSClass: NSObjectClass): NSArray; overload;
begin
  Result := Pasteboard.readObjectsForClasses(TNSArray.Wrap(
    TNSArray.OCClass.arrayWithObject((NSClass as ILocalObject).GetObjectID)), nil);
end;

function LoadNSObjectsFromPasteboard(const Pasteboard: NSPasteboard;
  const NSClasses: array of NSObjectClass; const Options: NSDictionary = nil): NSArray; overload;
var
  Arr: NSMutableArray;
  I: Integer;
begin
  Arr := TNSMutableArray.Wrap(TNSMutableArray.OCClass.arrayWithCapacity(Length(NSClasses)));
  for I := 0 to High(NSClasses) do
    Arr.addObject((NSClasses[I] as ILocalObject).GetObjectID);
  Result := Pasteboard.readObjectsForClasses(Arr, Options);
end;

{ TPasteboardItemDataProvider.TSource }

constructor TPasteboardItemDataProvider.TSource.Create(
  const Format: TClipboardFormat; const OnGetData: TGetPasteboardDataEvent);
begin
  inherited Create;
  FFormat := Format;
  FOnGetData := OnGetData;
end;

{ TPasteboardItemDataProvider }

constructor TPasteboardItemDataProvider.Create;
begin
  inherited Create;
  FToUpload := TObjectList<TSource>.Create;
end;

constructor TPasteboardItemDataProvider.Create(const Format: TClipboardFormat;
  const OnGetData: TGetPasteboardDataEvent);
begin
  Create;
  AddFormat(Format, OnGetData);
end;

destructor TPasteboardItemDataProvider.Destroy;
begin
  FToUpload.Free;
  inherited;
end;

procedure TPasteboardItemDataProvider.AddFormat(const Format: TClipboardFormat;
  const OnGetData: TGetPasteboardDataEvent);
var
  I: Integer;
begin
  for I := 0 to FToUpload.Count - 1 do
    if FToUpload[I].Format = Format then
    begin
      FToUpload.Delete(I);
      Break;
    end;
  FToUpload.Add(TSource.Create(Format, OnGetData));
end;

procedure TPasteboardItemDataProvider.clearContents;
begin
  FToUpload.Clear;
end;

procedure TPasteboardItemDataProvider.resolvePromises(Item: NSPasteboardItem;
  forceAnyVirtualFile: Boolean);
var
  Format: TClipboardFormat;
  I: Integer;
begin
  for I := FToUpload.Count - 1 downto 0 do
  begin
    Format := FToUpload[I].Format;
    if forceAnyVirtualFile or (Format <> cfFileURLPromise) then
      FToUpload[I].GetData(Format.ToNSString, Item);
  end;
end;

function TPasteboardItemDataProvider.count: Integer;
begin
  Result := FToUpload.Count;
end;

function TPasteboardItemDataProvider.types: NSArray;
var
  DelphiArr: TArray<TClipboardFormat>;
  I: Integer;
begin
  SetLength(DelphiArr, FToUpload.Count);
  if DelphiArr = nil then Exit(nil);
  for I := 0 to High(DelphiArr) do
    DelphiArr[I] := FToUpload[I].Format;
  Result := ClipboardFormatsToNSArray(DelphiArr)
end;

function TPasteboardItemDataProvider.hasType(PBType: CFStringRef): Boolean;
var
  I: Integer;
  WantedFormat: TClipboardFormat;
begin
  WantedFormat := TClipboardFormat.Wrap(PBType);
  for I := 0 to FToUpload.Count - 1 do
    if FToUpload[I].Format = WantedFormat then Exit(True);
  Result := False;
end;

function TPasteboardItemDataProvider.hasOutstandingPromises: Boolean;
begin
  Result := (FToUpload.Count > 0);
end;

procedure TPasteboardItemDataProvider.pasteboard(pasteboard: NSPasteboard; item: NSPasteboardItem;
  provideDataForType: CFStringRef);
var
  I: Integer;
  WantedFormat: TClipboardFormat;
begin
  try
    WantedFormat := TClipboardFormat.Wrap(provideDataForType);
    for I := FToUpload.Count - 1 downto 0 do
      if FToUpload[I].Format = WantedFormat then
      begin
        FToUpload[I].GetData(WantedFormat.ToNSString, item);
        if I < FToUpload.Count then FToUpload.Delete(I);
        Break;
      end;
  except
    { An unhandled Delphi exception can end up as a nasty runtime error in this
      situation, similar to an unhandled exception in a stdcall callback for a
      system COM object on Windows. }
    ApplicationHandleException(Self);
  end;
end;

procedure TPasteboardItemDataProvider.pasteboardFinishedWithDataProvider(Sender: Pointer);
begin
  FToUpload.Clear;
end;

{ TMacClipboardCore }

class destructor TMacClipboardCore.FinalizeClass;
begin
  FDragClipboard.Free;
  FFontClipboard.Free;
  FRulerClipboard.Free;
end;

class function TMacClipboardCore.GetAllClipboardTypeKinds: NSMutableArray;
begin
  if FAllClipboardTypeKinds = nil then
  begin
    FAllClipboardTypeKinds := TCFMutableArray.Create;
    FAllClipboardTypeKinds.ObjC.addObject(kUTTypeItem);
  end;
  Result := FAllClipboardTypeKinds.ObjC;
end;

class function TMacClipboardCore.ConformingFormat(const FormatToTest, BaseFormat: TClipboardFormat): Boolean;
var
  UTIToTest, BaseUTI: CFStringRef;
begin
  UTIToTest := FormatToTest.Handle;
  BaseUTI := BaseFormat.Handle;
  if not IsValidUTIString(UTIToTest) then
    UTIToTest := UTTypeCreatePreferredIdentifierForTag(kUTTagClassNSPboardType, UTIToTest, nil);
  if not IsValidUTIString(BaseUTI) then
    BaseUTI := UTTypeCreatePreferredIdentifierForTag(kUTTagClassNSPboardType, BaseUTI, nil);
  try
    Result := UTTypeConformsTo(UTIToTest, BaseUTI);
  finally
    if BaseUTI <> BaseFormat.Handle then CFReleaseAndNil(BaseUTI);
    if UTIToTest <> FormatToTest.Handle then CFReleaseAndNil(UTIToTest);
  end;
end;

class procedure TMacClipboardCore.NewFormatRegistered(const Name: string; const Format: TClipboardFormat);
var
  Ref: CFStringRef;
begin
  Ref := Format.Handle;
  if not UTTypeConformsTo(Ref, kUTTypeItem) then
    AllClipboardTypeKinds.addObject(Ref);
end;

type
  TClipboardHelper = class helper for TClipboard
  public
    constructor Create(const PasteboardName: NSString);
  end;

constructor TClipboardHelper.Create(const PasteboardName: NSString);
begin
  inherited Create(TMacClipboardCore.Create(Self,
    TNSPasteboard.Wrap(TNSPasteboard.OCClass.pasteboardWithName(PasteboardName))));
end;

class procedure TMacClipboardCore.InitializeFormats(
  StdFormatValues: TClipboard.TStdFormatValues; var CustomFormatsSupported: Boolean);
const
  AppKitLib = '/System/Library/Frameworks/AppKit.framework/AppKit';
var
  Lib: NativeUInt;

  function LoadCFStr(const Symbol: MarshaledAString): CFStringRef;
  begin
    Result := PCFStringRef(dlsym(Lib, Symbol))^;
  end;

  function LoadFormat(const Symbol: MarshaledAString): TClipboardFormat;
  begin
    Result := RegisterFormatInternal(LoadCFStr(Symbol));
  end;

  function LoadNSStr(const Symbol: MarshaledAString): NSString;
  begin
    Result := TNSString.Wrap(LoadCFStr(Symbol));
  end;
begin
  Lib := dlopen(AppKitLib, RTLD_NOLOAD);
  if Lib = 0 then RaiseLastOSError;
  try
    cfRTFD := LoadFormat('NSPasteboardTypeRTFD');
    cfTabularText := LoadFormat('NSPasteboardTypeTabularText');
    cfFont := LoadFormat('NSPasteboardTypeFont');
    cfRuler := LoadFormat('NSPasteboardTypeRuler');
    cfColor := LoadFormat('NSPasteboardTypeColor');
    cfSound := LoadFormat('NSPasteboardTypeSound');
    cfMultipleTextSelection := LoadFormat('NSPasteboardTypeMultipleTextSelection');
    cfFindPanelSearchOptions := LoadFormat('NSPasteboardTypeFindPanelSearchOptions');
    FDragClipboard := TClipboard.Create(LoadNSStr('NSDragPboard'));
    FFindClipboard := TClipboard.Create(LoadNSStr('NSFindPboard'));
    FFontClipboard := TClipboard.Create(LoadNSStr('NSFontPboard'));
    FRulerClipboard := TClipboard.Create(LoadNSStr('NSRulerPboard'));
  finally
    dlclose(Lib);
  end;
  cfFileURLPromise := RegisterFormatInternal(kPasteboardTypeFileURLPromise);
  cfFilePromiseContent := RegisterFormatInternal(kPasteboardTypeFilePromiseContent);
  { cfPNG, cfText etc. are mapped to the modern UTI types per the inherited
    implementation rather than the legacy NSPasteboardTypeXXX strings }
  inherited;
  StdFormatValues.cfVirtualFileDescriptor := cfFilePromiseContent;
end;

constructor TMacClipboardCore.Create(const AOwner: TClipboard);
begin
  inherited Create(AOwner);
  FProviders := TList<IPasteboardItemDataProvider>.Create;
  FNSObjectIDsToUpload := TList<Pointer>.Create;
  FNSObjectIDsToUpload.OnNotify := NSObjectIDsToUploadNotify;
end;

constructor TMacClipboardCore.Create(const AOwner: TClipboard; const ACustomPasteboard: NSPasteboard);
begin
  Create(AOwner);
  if ACustomPasteboard <> nil then ACustomPasteboard.retain;
  FCustomPasteboard := ACustomPasteboard;
end;

destructor TMacClipboardCore.Destroy;
begin
  if FCustomPasteboard <> nil then FCustomPasteboard.release;
  FNSObjectIDsToUpload.Free;
  FProviders.Free;
  inherited;
end;

function TMacClipboardCore.WrapsSystemClipboard: Boolean;
begin
  Result := (FCustomPasteboard = nil);
end;

procedure TMacClipboardCore.AssignNSObject(const ObjID: Pointer);
begin
  FNSObjectIDsToUpload.Add(ObjID);
end;

function TMacClipboardCore.GetPasteboard: NSPasteboard;
begin
  if FCustomPasteboard = nil then
    Result := TNSPasteboard.Wrap(TNSPasteboard.OCClass.generalPasteboard)
  else
    Result := FCustomPasteboard;
end;

procedure TMacClipboardCore.NSObjectIDsToUploadNotify(Sender: TObject;
  const Item: Pointer; Action: TCollectionNotification);
begin
  if Action = cnRemoved then
    TNSObject.Wrap(Item).release;
end;

procedure TMacClipboardCore.AddFormat(const Format: TClipboardFormat;
  const OnGetData: TGetPasteboardDataEvent);
begin
  if (FCurrentProvider = nil) or FCurrentProvider.hasType(Format.Handle) then
  begin
    FCurrentProvider := TPasteboardItemDataProvider.Create;
    FProviders.Add(FCurrentProvider);
  end;
  FCurrentProvider.AddFormat(Format, OnGetData);
end;

{ TClipboard.ICore }

function TMacClipboardCore.SupportsChangeNotifications: Boolean;
begin
  Result := True;
end;

procedure TMacClipboardCore.CheckForChanges;
var
  NewChangeCount: NSInteger;
begin
  NewChangeCount := Pasteboard.changeCount;
  if NewChangeCount = FLastChangeCount then Exit;
  FLastChangeCount := NewChangeCount;
  FChangeNotifyCallback;
end;

class procedure TMacClipboardCore.CheckForChangesTimer(timer: CFRunLoopTimerRef; info: Pointer);
begin
  TMacClipboardCore(info).CheckForChanges;
end;

procedure TMacClipboardCore.EnableChangeNotifications(const Callback: TThreadMethod);
const
  Interval = 5;
var
  Context: CFRunLoopTimerContext;
begin
  FChangeNotifyCallback := Callback;
  FillChar(Context, SizeOf(Context), 0);
  Context.info := Self;
  FTimer := CFRunLoopTimerCreate(nil, CFAbsoluteTimeGetCurrent + Interval, Interval,
    0, 0, CheckForChangesTimer, @Context);
  CFRunLoopAddTimer(CFRunLoopGetMain, FTimer, kCFRunLoopCommonModes);
end;

procedure TMacClipboardCore.DisableChangeNotifications;
begin
  CFRunLoopRemoveTimer(CFRunLoopGetMain, FTimer, kCFRunLoopCommonModes);
  CFRelease(FTimer);
end;

function TMacClipboardCore.GetChangeCount: TClipboardChangeCount;
begin
  Result := TClipboardChangeCount(Pasteboard.changeCount);
end;

function TMacClipboardCore.GetFormats: TArray<TClipboardFormat>;
begin
  Result := NSArrayToClipboardFormats(Pasteboard.types);
end;

function TMacClipboardCore.HasFormat(Format: TClipboardFormat): Boolean;
begin
  Result := HasFormat([Format], Format);
end;

function TMacClipboardCore.HasFormat(const Formats: array of TClipboardFormat;
  var Matched: TClipboardFormat): Boolean;
var
  NSArr: NSArray;
  NSStr: NSString;
begin
  NSArr := ClipboardFormatsToNSArray(Formats);
  NSStr := Pasteboard.availableTypeFromArray(NSArr);
  Result := (NSStr <> nil);
  if Result then
    Matched := TClipboardFormat.Wrap(NSStr);
end;

procedure TMacClipboardCore.DoClear;
begin
  Pasteboard.clearContents;
  FNSObjectIDsToUpload.Clear;
  FProviders.Clear;
  FProvidedItems := nil;
  FCurrentProvider := nil;
end;

function TMacClipboardCore.DoOpen: Boolean;
begin
  Result := True;
end;

procedure TMacClipboardCore.DoClose;
var
  I: Integer;
  ObjIDs: TArray<Pointer>;
  Provider: IPasteboardItemDataProvider;
  Types: NSArray;
begin
  if not Clipboard.IsOpenForWriting then Exit;
  try
    { walk through our custom providers; if empty delete, otherwise assign to its
      pasteboard item and add the item to our list of objects to upload }
    for I := FProviders.Count - 1 downto 0 do
      if FProviders[I].count = 0 then FProviders.Delete(I);
    SetLength(FProvidedItems, FProviders.Count);
    for I := 0 to FProviders.Count - 1 do
    begin
      Provider := FProviders[I];
      Types := Provider.types;
      FProvidedItems[I] := TNSPasteboardItem.Create;
      FProvidedItems[I].setDataProvider(Provider, Provider.types);
      FNSObjectIDsToUpload.Insert(I, (FProvidedItems[I] as ILocalObject).GetObjectID);
    end;
    { Convert the list of objects to a NSArray and upload }
    ObjIDs := FNSObjectIDsToUpload.ToArray;
    if ObjIDs = nil then Exit;
    Pasteboard.writeObjects(TNSArray.Wrap(TNSArray.OCClass.arrayWithObjects(
      ObjIDs, Length(ObjIDs))));
  finally
    FNSObjectIDsToUpload.Clear;
  end;
end;

function TMacClipboardCore.ReadPlainText: TArray<string>;
var
  I: Integer;
  Strs: NSArray;
begin
  Strs := LoadNSObjectsFromPasteboard(Pasteboard, TNSString.OCClass);
  if Strs = nil then Exit(nil);
  SetLength(Result, Strs.count);
  for I := 0 to High(Result) do
    Result[I] := CFStringGetValue(Strs.objectAtIndex(I));
end;

function GetCanonicalFormat(const Format: TClipboardFormat): TClipboardFormat;
begin
  if Format.IsUTI then
    Result := Format
  else
    Result := TMacClipboardCore.RegisterFormat(
      UTTypeCreatePreferredIdentifierForTag(kUTTagClassNSPboardType, Format.Handle, nil), True);
end;

procedure TMacClipboardCore.WriteBytes(const Format: TClipboardFormat;
  const Renderer: TFunc<TBytes>; PreferDelayed: Boolean);
begin
  { OS X only supports non-UTI pasteboard types when using the legacy, single-
    item API, but we use the 10.6+ multi-item API. As such, if we haven't been
    given a UTI, we need to get one. }
  AddFormat(GetCanonicalFormat(Format),
    procedure (const NSType: NSString; const Item: NSPasteboardItem)
    begin
      Item.setData(BytesToNSData(Renderer), NSType);
    end);
end;

procedure TMacClipboardCore.WritePlainText(const Renderer: TFunc<string>; PreferDelayed: Boolean);
begin
  AddFormat(cfUTF8Text,
    procedure (const NSType: NSString; const Item: NSPasteboardItem)
    begin
      Item.setString(StrToNSString(Renderer), NSType)
    end);
end;

procedure TMacClipboardCore.WriteVirtualFile(const Details: TVirtualFileDetails;
  const Renderer: TProc<TStream>; PreferDelayed: Boolean);
var
  UTI: NSString;
begin
  UTI := StrToNSString(FileExtToUTI(Details.FileName));
  AddFormat(cfFilePromiseContent,
    procedure (const NSType: NSString; const Item: NSPasteboardItem)
    begin
      Item.setString(UTI, NSType);
    end);
  AddFormat(cfFileURLPromise, MakeFileURLPromiseFunc(Pasteboard, Details, Renderer));
end;

procedure TMacClipboardCore.WriteFileName(const Renderer: TFunc<TFileName>; PreferDelayed: Boolean);
var
  FN: TFileName;
  GetData: TGetPasteboardDataEvent;
begin
  if PreferDelayed then
  begin
    GetData :=
      procedure (const NSType: NSString; const DestItem: NSPasteboardItem)
      var
        URL: NSURL;
      begin
        if FN = '' then
        begin
          FN := Renderer;
          URL := TNSURL.Wrap(TNSURL.OCClass.fileURLWithPath(StrToNSString(FN),
            DirectoryExists(FN)));
          FN := NSStringGetValue(URL.absoluteString);
        end;
        DestItem.setString(StrToNSString(FN), NSType)
      end;
    NewFormatSet;
    AddFormat(cfFileName, GetData);
    AddFormat(cfUTF8Text, GetData);
  end
  else
  begin
    FN := Renderer;
    FNSObjectIDsToUpload.Add(TNSURL.Alloc.initFileURLWithPath(StrToNSString(FN),
      DirectoryExists(FN, False)));
  end;
end;

function TMacClipboardCore.ReadBytes(const Format: TClipboardFormat): TBytes;
begin
  Result := NSDataToBytes(Pasteboard.dataForType(Format.ToNSString));
end;

procedure TMacClipboardCore.WriteURL(const Renderer: TFunc<string>; PreferDelayed: Boolean);
var
  GetData: TGetPasteboardDataEvent;
  Str: string;
begin
  if PreferDelayed then
  begin
    GetData :=
      procedure (const NSType: NSString; const DestItem: NSPasteboardItem)
      begin
        if Str = '' then Str := Renderer;
        DestItem.setString(StrToNSString(Str), NSType)
      end;
    NewFormatSet;
    AddFormat(cfURL, GetData);
    AddFormat(cfUTF8Text, GetData);
  end
  else
    FNSObjectIDsToUpload.Add(TNSURL.Alloc.initWithString(StrToNSString(Renderer())));
end;

procedure TMacClipboardCore.DoEnumVirtualFiles(DescriptorsOnly: Boolean;
  const Callback: TSaveVirtualFilesCallback);
var
  LookForMore: Boolean;
  DescriptorType, Descriptor: NSString;
  I: Integer;
  Item: NSPasteboardItem;
  Items: NSArray;
  Pasteboard: NSPasteboard;
  PBoardRef: PasteboardRef;
  SaveToFile: TSaveVirtualFileFunc;
begin
  Pasteboard := GetPasteboard;
  Items := LoadNSObjectsFromPasteboard(Pasteboard, TNSPasteboardItem.OCClass);
  if Items = nil then Exit;
  LookForMore := True;
  DescriptorType := TNSString.Wrap(kPasteboardTypeFilePromiseContent);
  PBoardRef := nil;
  try
    for I := 0 to Items.count - 1 do
    begin
      Item := TNSPasteboardItem.Wrap(Items.objectAtIndex(I));
      Descriptor := Item.stringForType(DescriptorType);
      if Descriptor = nil then Continue;
      if not DescriptorsOnly and (PBoardRef = nil) then
      begin
        OSStatusCheck(PasteboardCreate((Pasteboard.name as ILocalObject).GetObjectID, PBoardRef));
        SaveToFile :=
          function (const DestDirectory: string): TFileName
          var
            FileURL: NSURL;
          begin
            FileURL := SaveVirtualFile(PBoardRef, Item, DestDirectory);
            Result := NSStringGetValue(FileURL.path);
          end;
      end;
      Callback(NSStringGetValue(Descriptor), SaveToFile, LookForMore);
      if not LookForMore then Exit;
    end;
  finally
    CFReleaseAndNil(PBoardRef);
    SaveToFile := nil;
  end;
end;

function TMacClipboardCore.ReadVirtualFileDescriptors: TArray<string>;
var
  Arr: TArray<string>;
  Count: Integer;
begin
  Count := 0;
  DoEnumVirtualFiles(True,
    procedure (const Descriptor: string; const SaveToFile: TSaveVirtualFileFunc;
      var LookForMore: Boolean)
    begin
      if Length(Arr) = Count then SetLength(Arr, Count + 8);
      Arr[Count] := Descriptor;
      Inc(Count);
    end);
  Result := Copy(Arr, 0, Count);
end;

procedure TMacClipboardCore.ReadVirtualFiles(const Callback: TEnumVirtualFilesStreamCallback);
var
  TempDir: string;
begin
  TempDir := TPath.GetTempPath;
  DoEnumVirtualFiles(False,
    procedure (const Descriptor: string; const SaveToFile: TSaveVirtualFileFunc;
      var LookForMore: Boolean)
    var
      SaveToStream: TProc<TStream>;
    begin
      SaveToStream :=
        procedure (Stream: TStream)
        var
          FileStream: TFileStream;
          TempFile: string;
        begin
          TempFile := SaveToFile(TempDir);
          FileStream := TFileStream.Create(TempFile, fmOpenRead);
          try
            Stream.CopyFrom(FileStream, 0)
          finally
            FileStream.Free;
            DeleteFile(TempFile);
          end;
        end;
      Callback(Descriptor, SaveToStream, LookForMore);
    end);
end;

procedure TMacClipboardCore.SaveVirtualFiles(const Callback: TSaveVirtualFilesCallback);
begin
  DoEnumVirtualFiles(False, Callback);
end;

function TMacClipboardCore.GetNSURLs: NSArray;
begin
  Result := LoadNSObjectsFromPasteboard(Pasteboard, TNSURL.OCClass);
end;

function TMacClipboardCore.HasOutstandingPromisesToOS: Boolean;
var
  Provider: IPasteboardItemDataProvider;
begin
  for Provider in FProviders do
    if Provider.hasOutstandingPromises then Exit(True);
  Result := False;
end;

procedure TMacClipboardCore.ResolveOutstandingPromisesToOS(IsExplicitlyRequested: Boolean);
var
  Pasteboard: NSPasteboard;
  I: Integer;
begin
  { The C-based Pasteboard API has a handy PasteboardResolvePromises function directly
    equivalent to OleFlushClipboard on Windows; the Cocoa NSPasteboard API does not. Hmm... }
  Pasteboard := GetPasteboard;
  for I := 0 to FProviders.Count - 1 do
    FProviders[I].resolvePromises(FProvidedItems[I], IsExplicitlyRequested);
end;

procedure TMacClipboardCore.CancelOutstandingPromisesToOS;
begin
  Clipboard.Clear; //!!!can we do better? E.g., have resolvePromisesWithData method -> assign outstanding types as nil?
end;

procedure TMacClipboardCore.NewFormatSet;
begin
  FCurrentProvider := nil;
end;

procedure TMacClipboardCore.EnumBytes(const Format: TClipboardFormat; const Callback: TGetBytesCallback);
var
  Available, Wanted: TClipboardFormat;
  Bytes: TBytes;
  CheckForMore, GotAtLeastOne: Boolean;
  Items: NSArray;
  ItemIndex: NSUInteger;
  Item: NSPasteboardItem;
begin
  Items := LoadNSObjectsFromPasteboard(Pasteboard, TNSPasteboardItem.OCClass);
  if Items = nil then Exit;
  { Fun fun fun...
    - Only UTI types are findable when enumerating the pasteboard's items. While you can
      in principle get data for a non-UTI type using the old, single item API, attempting
      to do so for data that we ourselves have written will fail since in order to support
      multiple instances of the same type consistently we always write using
      NSPasteboardItem and therefore UTIs (any given non-UTI type can have a system-
      defined 'dynamic UTI' generated for it). As such, when getting data out we need to
      look for the canonical, UTI format like when putting data in.
    - The system will pretend TIFF data exists when only PNG data does. }
  CheckForMore := True;
  GotAtLeastOne := False;
  Wanted := GetCanonicalFormat(Format);
  for ItemIndex := 0 to Items.count - 1 do
  begin
    Item := TNSPasteboardItem.Wrap(Items.objectAtIndex(ItemIndex));
    for Available in NSArrayToClipboardFormats(Item.types) do
      if UTTypeConformsTo(Available.Handle, Wanted.Handle) then
      begin
        Bytes := NSDataToBytes(Item.dataForType(Available.ToNSString));
        Callback(Bytes, CheckForMore);
        if not CheckForMore then Exit;
        GotAtLeastOne := True;
      end;
  end;
  if GotAtLeastOne then Exit;
  Bytes := NSDataToBytes(Pasteboard.dataForType(Format.ToNSString));
  if Bytes <> nil then
    Callback(Bytes, CheckForMore);
end;

procedure TMacClipboardCore.EnumFormatSets(const Callback: TEnumFormatSetsCallback);
var
  ContinueEnum: Boolean;
  GetFormatBytes: TFunc<TClipboardFormat, TBytes>;
  I: NSUInteger;
  Item: NSPasteboardItem;
  Items: NSArray;
begin
  ContinueEnum := True;
  GetFormatBytes :=
    function (ClipFormat: TClipboardFormat): TBytes
    begin
      Result := NSDataToBytes(Item.dataForType(ClipFormat.ToNSString));
    end;
  Items := LoadNSObjectsFromPasteboard(Pasteboard, TNSPasteboardItem.OCClass);
  if Items = nil then Exit;
  for I := 0 to Items.count - 1 do
  begin
    Item := TNSPasteboardItem.Wrap(Items.objectAtIndex(I));
    Callback(NSArrayToClipboardFormats(Item.types), GetFormatBytes, ContinueEnum);
    if not ContinueEnum then Exit;
  end;
end;

function DragClipboard: TClipboard;
begin
  Result := TMacClipboardCore.DragClipboard;
end;

function FontClipboard: TClipboard;
begin
  Result := TMacClipboardCore.FontClipboard;
end;

function RulerClipboard: TClipboard;
begin
  Result := TMacClipboardCore.RulerClipboard;
end;

function cfRTFD: TClipboardFormat;
begin
  Result := TMacClipboardCore.cfRTFD;
end;

function cfTabularText: TClipboardFormat;
begin
  Result := TMacClipboardCore.cfTabularText;
end;

function cfFont: TClipboardFormat;
begin
  Result := TMacClipboardCore.cfFont;
end;

function cfRuler: TClipboardFormat;
begin
  Result := TMacClipboardCore.cfRuler;
end;

function cfSound: TClipboardFormat;
begin
  Result := TMacClipboardCore.cfSound;
end;

function cfMultipleTextSelection: TClipboardFormat;
begin
  Result := TMacClipboardCore.cfMultipleTextSelection;
end;

function cfFindPanelSearchOptions: TClipboardFormat;
begin
  Result := TMacClipboardCore.cfFindPanelSearchOptions;
end;

function cfFileURLPromise: TClipboardFormat;
begin
  Result := TMacClipboardCore.cfFileURLPromise;
end;

function cfFilePromiseContent: TClipboardFormat;
begin
  Result := TMacClipboardCore.cfFilePromiseContent;
end;
{$ENDIF}

initialization
  {$IFDEF iOS}
  TiOSClipboardCore.Register;
  {$ELSE}
  TMacClipboardCore.Register;
  {$ENDIF}
{$ENDIF}
end.

