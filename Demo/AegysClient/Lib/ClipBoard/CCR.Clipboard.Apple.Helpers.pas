{**************************************************************************************}
{                                                                                      }
{ CCR.Clipboard - helpers and missing native API declarations for OS X and iOS         }
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

unit CCR.Clipboard.Apple.Helpers;

interface

{$IFDEF NEXTGEN}
{$LEGACYIFEND ON}
{$ENDIF}

{$IFDEF MACOS}
uses
  Posix.Dlfcn, MacApi.ObjCRuntime, MacApi.ObjectiveC, MacApi.CoreFoundation,
  {$IFDEF IOS}
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.CoreGraphics,
  {$ELSE}
  MacApi.CocoaTypes, MacApi.Foundation, MacApi.AppKit,
  {$ENDIF}
  System.SysUtils, System.RTLConsts;

{$IF NOT DECLARED(MarshaledAString)}
type
  MarshaledAString = PAnsiChar;
{$IFEND}

procedure CFReleaseAndNil(var Ref);
function CFStringCreateWithString(const S: string): CFStringRef; inline;
function CFStringGetValue(const CFStr: CFStringRef): string;
function CFStringGetFrameworkConst(const Framework: NativeUInt; const ConstName: MarshaledAString): CFStringRef; overload;
function CFStringGetFrameworkConst(const Framework, ConstName: MarshaledAString): CFStringRef; overload;
function CGRectMake(x: Single; y: Single; width: Single; height: Single): CGRect; inline;
function IsValidUTIString(const S: string): Boolean; overload;      //warning - no OS API to delegate to!
function IsValidUTIString(const S: CFStringRef): Boolean; overload; //warning - no OS API to delegate to!
function NSDataToBytes(const Data: NSData): TBytes;
function BytesToNSData(const Bytes: TBytes): NSData; inline;
function URLContentsToNSData(const URL: string): NSData;
function NSInheritsFrom(const AObject: NSObject; const AClass: NSObjectClass): Boolean;
function NSStringGetValue(const NSStr: NSString): string; inline;
function StrToNSString(const S: string): NSString; overload;
function StrToNSString(const Buffer: PChar; NumChars: Integer): NSString; overload;
function StrToNSURL(const S: string): NSURL; overload; inline;

type
  ICFWrapper<CFType> = interface
  ['{03314EE4-EE09-44F0-9AA9-FC3BE3454FA9}']
    function GetRef: CFType;
    property Ref: CFType read GetRef;
  end;

  IObjCWrapper<CFType; ObjCClassType: IObjectiveCClass; ObjCType: IObjectiveCInstance> = interface
    function GetID: CFType;
    function GetObjCClass: ObjCClassType;
    function GetObjC: ObjCType;
    property ID: CFType read GetID;
    property ObjCClass: ObjCClassType read GetObjCClass;
    property ObjC: ObjCType read GetObjC;
  end;

  TCFWrapper = class(TInterfacedObject, ICFWrapper<CFTypeRef>)
  strict private
    FCFReleaseProc: procedure (cf: CFTypeRef); cdecl;
    FRef: CFTypeRef;
  strict protected
    constructor CreateInternal(const Ref: CFTypeRef; const CFReleaseProc: Pointer); overload;
    function GetRef: CFTypeRef;
    class function CastToPtr<T>(const Value: T): CFTypeRef; static;
  public
    class function Create<T>(const Ref: T; const CFReleaseProc: Pointer): ICFWrapper<T>; overload;
    class function Create<T>(const Ref: T): ICFWrapper<T>; overload;
    destructor Destroy; override;
  end;

  TObjCWrapper<CFType; ObjCClassType: IObjectiveCClass; ObjCType: IObjectiveCInstance> = class(
    TCFWrapper, IObjCWrapper<CFType, ObjCClassType, ObjCType>)
  strict private
    FID: CFType;
    FObjC: ObjCType;
  strict protected
    function GetID: CFType;
    function GetObjCClass: ObjCClassType;
    function GetObjC: ObjCType;
  public
    constructor Wrap(const ObjID: CFType; OwnsObject: Boolean); overload;
    constructor Create; overload;
  end;

  ICFArray = IObjCWrapper<CFArrayRef,NSArrayClass,NSArray>;
  TCFArray = TObjCWrapper<CFArrayRef,NSArrayClass,NSArray>;
  ICFDictionary = IObjCWrapper<CFDictionaryRef,NSDictionaryClass,NSDictionary>;
  TCFDictionary = TObjCWrapper<CFDictionaryRef,NSDictionaryClass,NSDictionary>;
  ICFMutableArray = IObjCWrapper<CFMutableArrayRef,NSMutableArrayClass,NSMutableArray>;
  TCFMutableArray = TObjCWrapper<CFMutableArrayRef,NSMutableArrayClass,NSMutableArray>;
  ICFMutableDictionary = IObjCWrapper<CFMutableDictionaryRef,NSMutableDictionaryClass,NSMutableDictionary>;
  TCFMutableDictionary = TObjCWrapper<CFMutableDictionaryRef,NSMutableDictionaryClass,NSMutableDictionary>;

function kUTTypeItem: CFStringRef;
function kUTTypeData: CFStringRef;
function kUTTypeImage: CFStringRef;
function kUTTypeURL: CFStringRef;
function kUTTypeFileURL: CFStringRef;
function kUTTypeGIF: CFStringRef;
function kUTTypeJPEG: CFStringRef;
function kUTTypePDF: CFStringRef;
function kUTTypePNG: CFStringRef;
function kUTTypeRTF: CFStringRef;
function kUTTypeSourceCode: CFStringRef;
function kUTTypeTIFF: CFStringRef;
function kUTTypeUTF8PlainText: CFStringRef;
function kUTTypeUTF16PlainText: CFStringRef;
function kUTTypeAppleICNS: CFStringRef;

function kUTTagClassFilenameExtension: CFStringRef;
function kUTTagClassNSPboardType: CFStringRef;

const
  LaunchServicesLib =
    {$IFDEF iOS}
    '/System/Library/Frameworks/MobileCoreServices.framework/MobileCoreServices'
    {$ELSE}
    '/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/LaunchServices'
    {$ENDIF};
  {$IF NOT DECLARED(_PU)}
  _PU = '_';
  {$IFEND}

function UTTypeEqual(inUTI1, inUTI2: CFStringRef): Boolean;
  cdecl; external LaunchServicesLib name _PU + 'UTTypeEqual';
function UTTypeConformsTo(inUTI, inConformsToUTI: CFStringRef): Boolean;
  cdecl; external LaunchServicesLib name _PU + 'UTTypeConformsTo';
function UTTypeCreatePreferredIdentifierForTag(inTagClass, inTag, inConformingToUTI: CFStringRef): CFStringRef;
  cdecl; external LaunchServicesLib name _PU + 'UTTypeCreatePreferredIdentifierForTag';
function UTTypeCopyDescription(inUTI: CFStringRef): CFStringRef;
  cdecl; external LaunchServicesLib name _PU + 'UTTypeCopyDescription';

function CFStringCreateUTIForFileExt(const FileName: string): CFStringRef;
function FileExtToUTI(const FileName: string): string;

{$IFNDEF iOS}

{ Pasteboard.h }

function kPasteboardTypeFilePromiseContent: CFStringRef; //UTF-8 encoded UTI describing what sort of data the virtual file is
function kPasteboardTypeFileURLPromise: CFStringRef;     //UTF-8 file URL of where the virtual file data has been written to

type
  OptionBits = UInt32;

  PasteboardRef = type Pointer;
  PasteboardSyncFlags = OptionBits;

function PasteboardCreate(inName: CFStringRef; out outPasteboard: PasteboardRef): OSStatus; cdecl;
function PasteboardSynchronize(inPasteboard: PasteboardRef): PasteboardSyncFlags; cdecl;
function PasteboardCopyPasteLocation(inPasteboard: PasteboardRef; out outPasteLocation: CFURLRef): OSStatus; cdecl;
function PasteboardSetPasteLocation(inPasteboard: PasteboardRef; inPasteLocation: CFURLRef): OSStatus; cdecl;

procedure OSStatusCheck(Status: OSStatus);
function SaveVirtualFile(const Pasteboard: PasteboardRef; const Item: NSPasteboardItem;
  const DestDirectory: string): NSURL;

const
	badPasteboardSyncErr = -25130;          //pasteboard has been modified and must be synchronized
	badPasteboardIndexErr = -25131;         //item index is invalid
	badPasteboardItemErr = -25132;          //item reference does not exist
	badPasteboardFlavorErr = -25133;        //item 'flavour' (format) does not exist
	duplicatePasteboardFlavorErr = -25134;  //item 'flavour' (format) already exists
	notPasteboardOwnerErr = -25135;         //client did not clear the pasteboard
{$ENDIF}

{$ENDIF}

implementation

{$POINTERMATH ON}

{$IFDEF MACOS}
type
  //XE2 and XE3's NSStringClass declaration is faulty, so redeclare with what we need
  NSStringClass = interface(NSObjectClass)
    ['{A21B9D92-0C1F-4BE4-9FA0-7E5357D46EDA}']
    function stringWithCharacters(characters: PChar; length: NSUInteger): Pointer; cdecl;
  end;
  TNSStringFix = class(TOCGenericImport<NSStringClass, NSString>);

var
  kFullStop: CFStringRef;
  kIllegalUTICharSet: CFCharacterSetRef;

procedure CFReleaseAndNil(var Ref);
var
  Ptr: Pointer;
begin
  Ptr := Pointer(Ref);
  Pointer(Ref) := nil;
  if Ptr <> nil then CFRelease(Ptr);
end;

function CFStringCreateWithString(const S: string): CFStringRef; inline;
begin
  Result := CFStringCreateWithCharacters(nil, PChar(S), Length(S));
end;

function CFStringGetValue(const CFStr: CFStringRef): string;
var
  Range: CFRange;
begin
  if CFStr = nil then Exit('');
  Range.location := 0;
  Range.length := CFStringGetLength(CFStr);
  SetLength(Result, Range.length);
  CFStringGetCharacters(CFStr, Range, PChar(Result));
end;

function CFStringGetFrameworkConst(const Framework: NativeUInt; const ConstName: MarshaledAString): CFStringRef; overload;
begin
  Result := PCFStringRef(dlsym(Framework, ConstName))^;
end;

function CFStringGetFrameworkConst(const Framework, ConstName: MarshaledAString): CFStringRef;
var
  Lib: NativeUInt;
begin
  Lib := dlopen(Framework, RTLD_NOLOAD);
  if Lib = 0 then RaiseLastOSError;
  try
    Result := PCFStringRef(dlsym(Lib, ConstName))^;
  finally
    dlclose(Lib);
  end;
end;

function CGRectMake(x: Single; y: Single; width: Single; height: Single): CGRect;
begin
  Result.origin.x := x;
  Result.origin.y := y;
  Result.size.width := width;
  Result.size.height := height;
end;

function IsValidUTIChar(const Ch: Char): Boolean; inline;
begin
  case Ch of
    'A'..'Z', 'a'..'z', '0'..'9', '.', '-': Result := True;
    Char(128)..High(Char): Result := True;
  else Result := False;
  end;
end;

function IsValidUTIString(const S: string): Boolean;
var
  I: Integer;
  HasFullStop: Boolean;
  SeekPtr: PChar;
begin
  Result := False;
  SeekPtr := Pointer(S);
  if SeekPtr = nil then Exit;
  HasFullStop := False;
  for I := 0 to Length(S) - 1 do
  begin
    if not IsValidUTIChar(SeekPtr^) then Exit;
    if SeekPtr^ = '.' then HasFullStop := True;
    Inc(SeekPtr);
  end;
  Result := HasFullStop and (SeekPtr[-1] <> '.');
end;

function IsValidUTIString(const S: CFStringRef): Boolean;
var
  InRange, OutRange: CFRange;
begin
  InRange.location := 0;
  InRange.length := CFStringGetLength(S);
  Result := not CFStringFindCharacterFromSet(S, kIllegalUTICharSet, InRange, 0, @OutRange);
  if not Result then Exit;
  OutRange := CFStringFind(S, kFullStop, kCFCompareBackwards);
  Result := (OutRange.length = 1) and (OutRange.location < Pred(InRange.length));
end;

function NSDataToBytes(const Data: NSData): TBytes;
begin
  if Data = nil then Exit(nil);
  SetLength(Result, Data.length);
  Data.getBytes(Result, Length(Result));
end;

function BytesToNSData(const Bytes: TBytes): NSData;
begin
  if Bytes = nil then
    Result := nil
  else
    Result := TNSData.Wrap(TNSData.OCClass.dataWithBytes(Bytes, Length(Bytes)));
end;

function URLContentsToNSData(const URL: string): NSData;
begin
  Result := TNSData.Wrap(TNSData.OCClass.dataWithContentsOfURL(StrToNSURL(URL)));
end;

function NSStringGetValue(const NSStr: NSString): string;
begin
  if NSStr = nil then
    Result := ''
  else
    Result := CFStringGetValue((NSStr as ILocalObject).GetObjectID);
end;

function StrToNSString(const S: string): NSString;
begin
  Result := TNSString.Wrap(TNSStringFix.OCClass.stringWithCharacters(PChar(S), Length(S)));
end;

function StrToNSString(const Buffer: PChar; NumChars: Integer): NSString;
begin
  Result := TNSString.Wrap(TNSStringFix.OCClass.stringWithCharacters(Buffer, NumChars));
end;

function StrToNSURL(const S: string): NSURL;
begin
  Result := TNSURL.Wrap(TNSURL.OCClass.URLWithString(StrToNSString(S)))
end;

function CFStringCreateUTIForFileExt(const FileName: string): CFStringRef;
var
  ExtRef: CFStringRef;
begin
  ExtRef := CFStringCreateWithString(Copy(ExtractFileExt(FileName), 2, MaxInt));
  try
    Result := UTTypeCreatePreferredIdentifierForTag(kUTTagClassFilenameExtension,
      ExtRef, kUTTypeData);
  finally
    CFReleaseAndNil(ExtRef);
  end;
end;

function FileExtToUTI(const FileName: string): string;
var
  UTIRef: CFStringRef;
begin
  UTIRef := CFStringCreateUTIForFileExt(FileName);
  try
    Result := CFStringGetValue(UTIRef);
  finally
    CFReleaseAndNil(UTIRef);
  end;
end;

function NSInheritsFrom(const AObject: NSObject; const AClass: NSObjectClass): Boolean;
begin
  Result := AObject.isKindOfClass((AClass as ILocalObject).GetObjectID);
end;

{ TCFWrapper<T> }

constructor TCFWrapper.CreateInternal(const Ref: CFTypeRef; const CFReleaseProc: Pointer);
begin
  inherited Create;
  FCFReleaseProc := CFReleaseProc;
  FRef := Ref;
end;

destructor TCFWrapper.Destroy;
begin
  if (FRef <> nil) and (@FCFReleaseProc <> nil) then
    FCFReleaseProc(FRef);
  inherited;
end;

class function TCFWrapper.CastToPtr<T>(const Value: T): CFTypeRef;
begin
  if SizeOf(T) <> SizeOf(CFTypeRef) then
    raise EArgumentException.Create('TCFWrapper must be instantiated with a typed pointer type');
  Move(Value, Result, SizeOf(CFTypeRef));
end;

class function TCFWrapper.Create<T>(const Ref: T; const CFReleaseProc: Pointer): ICFWrapper<T>;
begin
  Result := ICFWrapper<T>(CreateInternal(CastToPtr<T>(Ref), CFReleaseProc) as ICFWrapper<CFTypeRef>);
end;

class function TCFWrapper.Create<T>(const Ref: T): ICFWrapper<T>;
begin
  Result := Create(Ref, @CFRelease);
end;

function TCFWrapper.GetRef: CFTypeRef;
begin
  Result := FRef;
end;

{ TObjCWrapper }

constructor TObjCWrapper<CFType, ObjCClassType, ObjCType>.Wrap(const ObjID: CFType; OwnsObject: Boolean);
var
  RawID: CFTypeRef;
begin
  RawID := CastToPtr<CFType>(ObjID);
  if not OwnsObject then CFRetain(RawID);
  CreateInternal(RawID, @CFRelease);
  FID := ObjID;
end;

constructor TObjCWrapper<CFType, ObjCClassType, ObjCType>.Create;
var
  RawID: CFTypeRef;
begin
  FObjC := TOCGenericImport<ObjCClassType, ObjCType>.Create;
  RawID := (IInterface(FObjC) as ILocalObject).GetObjectID;
  CreateInternal(RawID, @CFRelease);
  Move(RawID, FID, SizeOf(RawID));
end;

function TObjCWrapper<CFType, ObjCClassType, ObjCType>.GetID: CFType;
begin
  Result := FID;
end;

function TObjCWrapper<CFType, ObjCClassType, ObjCType>.GetObjC: ObjCType;
begin
  if FObjC = nil then
    FObjC := TOCGenericImport<ObjCClassType, ObjCType>.Wrap(GetRef);
  Result := FObjC;
end;

function TObjCWrapper<CFType, ObjCClassType, ObjCType>.GetObjCClass: ObjCClassType;
begin
  Result := TOCGenericImport<ObjCClassType, ObjCType>.OCClass;
end;

{ UTIs }

type
  PCFStringRef = ^CFStringRef;

var
  LaunchServicesHandle: NativeUInt;

function GetLaunchServicesConst(const Name: MarshaledAString; var CachedPtr: PCFStringRef): CFStringRef;
begin
  if CachedPtr = nil then
    CachedPtr := dlsym(LaunchServicesHandle, Name);
  Result := CachedPtr^;
end;

var
  _kUTTypeItem,
  _kUTTypeData,
  _kUTTypeImage,
  _kUTTypeURL,
  _kUTTypeFileURL,
  _kUTTypeGIF,
  _kUTTypeJPEG,
  _kUTTypePDF,
  _kUTTypePNG,
  _kUTTypeRTF,
  _KUTTypeSourceCode,
  _kUTTypeTIFF,
  _kUTTypeUTF8PlainText,
  _kUTTypeUTF16PlainText,
  _kUTTypeAppleICNS: PCFStringRef;

  _kUTTagClassFilenameExtension, _kUTTagClassNSPboardType: PCFStringRef;

function kUTTypeItem: CFStringRef;
begin
  Result := GetLaunchServicesConst('kUTTypeItem', _kUTTypeItem);
end;

function kUTTypeData: CFStringRef;
begin
  Result := GetLaunchServicesConst('kUTTypeData', _kUTTypeData);
end;

function kUTTypeImage: CFStringRef;
begin
  Result := GetLaunchServicesConst('kUTTypeImage', _kUTTypeImage);
end;

function kUTTypeURL: CFStringRef;
begin
  Result := GetLaunchServicesConst('kUTTypeURL', _kUTTypeURL);
end;

function kUTTypeFileURL: CFStringRef;
begin
  Result := GetLaunchServicesConst('kUTTypeFileURL', _kUTTypeFileURL);
end;

function kUTTypeGIF: CFStringRef;
begin
  Result := GetLaunchServicesConst('kUTTypeGIF', _kUTTypeGIF);
end;

function kUTTypeJPEG: CFStringRef;
begin
  Result := GetLaunchServicesConst('kUTTypeJPEG', _kUTTypeJPEG);
end;

function kUTTypePDF: CFStringRef;
begin
  Result := GetLaunchServicesConst('kUTTypePDF', _kUTTypePDF);
end;

function kUTTypePNG: CFStringRef;
begin
  Result := GetLaunchServicesConst('kUTTypePNG', _kUTTypePNG);
end;

function kUTTypeRTF: CFStringRef;
begin
  Result := GetLaunchServicesConst('kUTTypeRTF', _kUTTypeRTF);
end;

function kUTTypeSourceCode: CFStringRef;
begin
  Result := GetLaunchServicesConst('kUTTypeSourceCode', _kUTTypeSourceCode);
end;

function kUTTypeTIFF: CFStringRef;
begin
  Result := GetLaunchServicesConst('kUTTypeTIFF', _kUTTypeTIFF);
end;

function kUTTypeUTF8PlainText: CFStringRef;
begin
  Result := GetLaunchServicesConst('kUTTypeUTF8PlainText', _kUTTypeUTF8PlainText);
end;

function kUTTypeUTF16PlainText: CFStringRef;
begin
  Result := GetLaunchServicesConst('kUTTypeUTF16PlainText', _kUTTypeUTF16PlainText);
end;

function kUTTypeAppleICNS: CFStringRef;
begin
  Result := GetLaunchServicesConst('kUTTypeAppleICNS', _kUTTypeAppleICNS);
end;

function kUTTagClassFilenameExtension: CFStringRef;
begin
  Result := GetLaunchServicesConst('kUTTagClassFilenameExtension', _kUTTagClassFilenameExtension);
end;

function kUTTagClassNSPboardType: CFStringRef;
begin
  Result := GetLaunchServicesConst('kUTTagClassNSPboardType', _kUTTagClassNSPboardType);
end;

function CreateIllegalUTICharSet: CFCharacterSetRef;
const
  ExtRange: CFRange = (location: 128; length: High(CFIndex));
var
  Ref: CFMutableCharacterSetRef;

  procedure AddRange(ChFrom, ChTo: Char);
  var
    Range: CFRange;
  begin
    Range.location := Ord(ChFrom);
    Range.length := Ord(ChTo) - Range.location + 1;
    CFCharacterSetAddCharactersInRange(Ref, Range);
  end;
begin
  Ref := CFCharacterSetCreateMutable(nil);
  AddRange('A', 'Z');
  AddRange('a', 'z');
  AddRange('0', '9');
  AddRange(Char(128), High(Char));
  AddRange('-', '-');
  AddRange('.', '.');
  Result := CFCharacterSetCreateInvertedSet(nil, CFCharacterSetRef(Ref));
  CFReleaseAndNil(Ref);;
end;

{$IFNDEF iOS}

var
  _kPasteboardTypeFileURLPromise, _kPasteboardTypeFilePromiseContent: CFStringRef;

function kPasteboardTypeFileURLPromise: CFStringRef;
begin
  Result := _kPasteboardTypeFileURLPromise;
end;

function kPasteboardTypeFilePromiseContent: CFStringRef;
begin
  Result := _kPasteboardTypeFilePromiseContent;
end;

const
  PasteboardLib = '/System/Library/Frameworks/ApplicationServices.framework/Frameworks/HIServices.framework/HIServices';

function PasteboardCreate; external PasteboardLib name _PU + 'PasteboardCreate';
function PasteboardSynchronize; external PasteboardLib name _PU + 'PasteboardSynchronize';
function PasteboardCopyPasteLocation; external PasteboardLib name _PU + 'PasteboardCopyPasteLocation';
function PasteboardSetPasteLocation; external PasteboardLib name _PU + 'PasteboardSetPasteLocation';

procedure OSStatusCheck(Status: OSStatus);
begin
  if Status <> 0 then
    RaiseLastOSError(Status);
end;

function SaveVirtualFile(const Pasteboard: PasteboardRef;
  const Item: NSPasteboardItem; const DestDirectory: string): NSURL;
var
  PathStr: CFStringRef;
  PathURL: CFURLRef;
begin
  OSStatusCheck(PasteboardSynchronize(Pasteboard));
  PathStr := CFStringCreateWithString(ExcludeTrailingPathDelimiter(DestDirectory));
  PathURL := CFURLCreateWithFileSystemPath(nil, PathStr, kCFURLPOSIXPathStyle, True);
  PasteboardSetPasteLocation(Pasteboard, PathURL);
  CFReleaseAndNil(PathURL);
  CFReleaseAndNil(PathStr);
  Result := TNSURL.Wrap(TNSURL.OCClass.URLWithString(Item.stringForType(
    TNSString.Wrap(kPasteboardTypeFileURLPromise))));
end;
{$ENDIF}

initialization
  LaunchServicesHandle := dlopen(LaunchServicesLib, RTLD_LAZY);
  kFullStop := __CFStringMakeConstantString('.');
  kIllegalUTICharSet := CreateIllegalUTICharSet;
  {$IF DECLARED(_kPasteboardTypeFileURLPromise)}
  _kPasteboardTypeFileURLPromise := __CFStringMakeConstantString('com.apple.pasteboard.promised-file-url');
  _kPasteboardTypeFilePromiseContent := __CFStringMakeConstantString('com.apple.pasteboard.promised-file-content-type');
  {$IFEND}
finalization
  dlclose(LaunchServicesHandle);
{$ENDIF}
end.
