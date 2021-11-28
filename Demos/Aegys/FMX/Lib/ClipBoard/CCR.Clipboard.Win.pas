{**************************************************************************************}
{                                                                                      }
{ CCR.Clipboard - Windows backend                                                      }
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

unit CCR.Clipboard.Win;

interface

{$IFDEF MSWINDOWS}
uses
  WinApi.Windows, WinApi.ShlObj, WinApi.ActiveX, System.Win.ComObj, System.Types,
  System.SysUtils, System.Classes, System.Generics.Collections, System.Generics.Defaults,
  CCR.Clipboard.Consts, CCR.Clipboard;

type
  TClipboardFormatHelper = record helper for TClipboardFormat
  strict private
    function GetHandle: TClipFormat; inline;
  public
    property Handle: TClipFormat read GetHandle;
    class function Wrap(const Value: TClipFormat): TClipboardFormat; static; inline;
  end;

  {$IF CompilerVersion > 28}
  IStream = interface(IUnknown) //redefined for compatibility
    ['{0000000C-0000-0000-C000-000000000046}']
    function Read(pv: Pointer; cb: LongInt; pcbRead: PLongInt): HRESULT; stdcall;
    function Write(pv: Pointer; cb: LongInt; pcbWritten: PLongInt): HRESULT; stdcall;
    function Seek(dlibMove: Int64; dwOrigin: DWORD; out libNewPosition: Int64): HRESULT; stdcall;
    function SetSize(libNewSize: Int64): HRESULT; stdcall;
    function CopyTo(stm: IStream; cb: Int64; out cbRead: Int64; out cbWritten: Int64): HRESULT; stdcall;
    function Commit(grfCommitFlags: DWORD): HRESULT; stdcall;
    function Revert: HRESULT; stdcall;
    function LockRegion(libOffset: Int64; cb: Int64; dwLockType: DWORD): HRESULT; stdcall;
    function UnlockRegion(libOffset: Int64; cb: Int64; dwLockType: DWORD): HRESULT; stdcall;
    function Stat(out statstg: TStatStg; grfStatFlag: DWORD): HRESULT; stdcall;
    function Clone(out stm: IStream): HRESULT; stdcall;
  end;
  {$IFEND}

{ TStream adaptor for IStream; provided since stock one is in a VCL unit }

  TOleStreamWrapper = class(TStream)
  private
    FStream: IStream;
  public
    constructor Create(const AStream: IStream);
    function Read(var Buffer; Count: LongInt): LongInt; override;
    function Write(const Buffer; Count: LongInt): LongInt; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

{ The following interface can be queried for from the IClipboardDropInfo instances
  returned by TClipboard.RegisterDropTarget }

  IWinClipboardDropInfo = interface(IClipboardDropInfo)
  ['{DE6743E9-4038-454D-B949-4B5DC4309873}']
    procedure SetEffect(Effect: DWORD);
  end;

{ Friendlier interface for building up a IDataObject; is implemented by both a class
  that acts on an arbitrary IDataObject + our own IDataObject implementation }

  TGetDataObjectDataEvent = reference to procedure (
    AllocResourceInMedium: Boolean; var Medium: TStgMedium);

  IDataObjectBuilder = interface
  ['{036C4A89-146B-4A9D-9459-A52C8E100566}']
    procedure AddFormat(const Format: TClipboardFormat; Index: LongInt;
      var StgMediumToOwn: TStgMedium); overload;
    procedure AddFormat(const Format: TClipboardFormat; Index: LongInt;
      PreferredTymed: LongInt; const OnGetData: TGetDataObjectDataEvent;
      FallbackTymed: LongInt = TYMED_NULL; const OnGetDataFallback: TGetDataObjectDataEvent = nil); overload;
    procedure AddFormat(const Format: TClipboardFormat; Index: LongInt;
      const OnGetData: TFunc<TBytes>); overload;
    procedure AddFormat(const Format: TClipboardFormat; Index: LongInt;
      const OnGetData: TProc<TStream>); overload;
    function GetEmpty: Boolean;
    property Empty: Boolean read GetEmpty;
  end;

  TDataObjectBuilderBase = class(TInterfacedObject)
  protected
    procedure AddFormat(const Format: TClipboardFormat; Index: LongInt;
      PreferredTymed: LongInt; const OnGetData: TGetDataObjectDataEvent;
      FallbackTymed: LongInt; const OnGetDataFallback: TGetDataObjectDataEvent); overload; virtual; abstract;
    procedure AddFormat(const Format: TClipboardFormat; Index: LongInt;
      const OnGetData: TFunc<TBytes>); overload;
    procedure AddFormat(const Format: TClipboardFormat; Index: LongInt;
      const OnGetData: TProc<TStream>); overload;
  end;

  TDataObjectBuilder = class(TDataObjectBuilderBase, IDataObjectBuilder)
  strict private
    FDataObject: IDataObject;
  protected
    procedure AddFormat(const Format: TClipboardFormat; Index: LongInt;
      var StgMediumToOwn: TStgMedium); overload;
    procedure AddFormat(const Format: TClipboardFormat; Index: LongInt;
      PreferredTymed: LongInt; const OnGetData: TGetDataObjectDataEvent;
      FallbackTymed: LongInt; const OnGetDataFallback: TGetDataObjectDataEvent); override;
    function GetDataObject: IDataObject;
    function GetEmpty: Boolean;
  public
    constructor Create(const ADataObject: IDataObject);
  end;

{ Our own IDataObject implementation; is pretty complete, in particular it has a substantive
  not dummy SetData implementation (needed for drag and drop). Nonetheless, from Delphi code
  the idea is to build instances up via the IDataObjectBuilder interface instead. }

  IClipboardDataObject = interface(IDataObject)
  ['{CE708417-23A8-4EA3-8674-4321D743FC94}']
    function GetIsOleClipboard: Boolean;
    procedure SetIsOleClipboard(Value: Boolean);
    property IsOleClipboard: Boolean read GetIsOleClipboard write SetIsOleClipboard;
  end;

  TDataObject = class(TDataObjectBuilderBase, IDataObject, IDataObjectBuilder, IClipboardDataObject)
  private type
    TOutputKind = (okAllocResourceInMedium, okGetInPlace, okQueryOnly);
  strict private type
    TSource = class
    strict private
      FDataRetrievedCount: Integer;
      FFormatEtc: TFormatEtc;
      FFallbackTymed: LongInt;
      FStgMedium: TStgMedium;
      FOnGetData, FOnGetDataFallback: TGetDataObjectDataEvent;
      procedure ReleaseStgMedium;
    public
      constructor Create(const ClipFormat: TClipFormat; Index: LongInt);
      destructor Destroy; override;
      procedure Assign(PreferredTymed: LongInt; const OnGetData: TGetDataObjectDataEvent;
        FallbackTymed: LongInt; const OnGetDataFallback: TGetDataObjectDataEvent); overload;
      procedure Assign(const StgMediumToOwn: TStgMedium); overload;
      function DoGetData(const WantedFormat: TFormatEtc; var Medium: TStgMedium;
        OutputKind: TOutputKind): HRESULT;
      property DataRetrievedCount: Integer read FDataRetrievedCount;
      property Aspect: LongInt read FFormatEtc.dwAspect;
      property ClipFormat: TClipFormat read FFormatEtc.cfFormat;
      property FormatEtc: TFormatEtc read FFormatEtc;
      property Index: LongInt read FFormatEtc.lindex;
      property PreferredTymed: LongInt read FFormatEtc.tymed;
      property FallbackTymed: LongInt read FFallbackTymed;
    end;
  protected type
    TEnumFormatEtc = class(TInterfacedObject, IEnumFormatEtc)
    strict private
      FNextIndex: Integer;
      FSource: TDataObject;
    protected
      function Next(celt: Longint; out elt;
        pceltFetched: PLongint): HRESULT; stdcall;
      function Skip(celt: Longint): HRESULT; stdcall;
      function Reset: HRESULT; stdcall;
      function Clone(out Enum: IEnumFormatEtc): HRESULT; stdcall;
    public
      constructor Create(const ASource: TDataObject; AStartAt: Integer = 0);
      destructor Destroy; override;
    end;
  strict private
    FSources: TList<TSource>;
    function FindOrAddSource(const ClipFormat: TClipFormat; Index: Integer): TSource;
    function DoGetData(const FormatEtc: TFormatEtc; var Medium: TStgMedium;
      OutputKind: TOutputKind): HRESULT; overload;
    function GetFormatCount: Integer;
    function GetFormatEtc(Index: Integer): TFormatEtc;
  protected
    { IDataObjectBuilder }
    procedure AddFormat(Format: TClipFormat; Index: LongInt; var StgMediumToOwn: TStgMedium); overload;
    procedure AddFormat(const Format: TClipboardFormat; Index: LongInt;
      var StgMediumToOwn: TStgMedium); overload;
    procedure AddFormat(const Format: TClipboardFormat; Index: LongInt;
      PreferredTymed: LongInt; const OnGetData: TGetDataObjectDataEvent;
      FallbackTymed: LongInt; const OnGetDataFallback: TGetDataObjectDataEvent); override;
    function GetDataObject: IDataObject;
    function GetEmpty: Boolean;
    { IDataObject }
    function GetData(const formatetcIn: TFormatEtc; out medium: TStgMedium): HRESULT; stdcall;
    function GetDataHere(const formatetc: TFormatEtc; out medium: TStgMedium): HRESULT; stdcall;
    function QueryGetData(const formatetc: TFormatEtc): HRESULT; stdcall;
    function GetCanonicalFormatEtc(const formatetc: TFormatEtc;
      out formatetcOut: TFormatEtc): HRESULT; stdcall;
    function SetData(const formatetc: TFormatEtc; var medium: TStgMedium; fRelease: BOOL): HRESULT; stdcall;
    function EnumFormatEtc(dwDirection: LongInt; out enumFormatEtc: IEnumFormatEtc): HRESULT; stdcall;
    function DAdvise(const formatetc: TFormatEtc; advf: LongInt;
      const advSink: IAdviseSink; out dwConnection: LongInt): HRESULT; stdcall;
    function DUnadvise(dwConnection: LongInt): HRESULT; stdcall;
    function EnumDAdvise(out enumAdvise: IEnumStatData): HRESULT; stdcall;
    { IClipboardDataObject }
    function HaveAllFormatsBeenRetrieved: Boolean;
    function GetIsOleClipboard: Boolean;
    procedure SetIsOleClipboard(Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    property FormatCount: Integer read GetFormatCount;
    property FormatEtcs[Index: Integer]: TFormatEtc read GetFormatEtc;
  end;

{ TDataObjectDestroyNotifier wraps an existing IDataObject purely to call a user-provided
  method on destruction. Needed because can't open the classic and OLE clipboards at the
  same time, so when the latter needs to be read the classic clipboard is closed but
  re-opened once the OLE clipboard has been finished with (goes out of scope/nil'ed) }

  TDataObjectDestroyNotifier = class(TInterfacedObject, IDataObject)
  strict private
    FSource: IDataObject;
    FOnDestroy: TNotifyEvent;
  protected
    function GetData(const formatetcIn: TFormatEtc; out medium: TStgMedium): HRESULT; stdcall;
    function GetDataHere(const formatetc: TFormatEtc; out medium: TStgMedium): HRESULT; stdcall;
    function QueryGetData(const formatetc: TFormatEtc): HRESULT; stdcall;
    function GetCanonicalFormatEtc(const formatetc: TFormatEtc;
      out formatetcOut: TFormatEtc): HRESULT; stdcall;
    function SetData(const formatetc: TFormatEtc; var medium: TStgMedium; fRelease: BOOL): HRESULT; stdcall;
    function EnumFormatEtc(dwDirection: LongInt; out enumFormatEtc: IEnumFormatEtc): HRESULT; stdcall;
    function DAdvise(const formatetc: TFormatEtc; advf: LongInt;
      const advSink: IAdviseSink; out dwConnection: LongInt): HRESULT; stdcall;
    function DUnadvise(dwConnection: LongInt): HRESULT; stdcall;
    function EnumDAdvise(out enumAdvise: IEnumStatData): HRESULT; stdcall;
  public
    constructor Create(const ASource: IDataObject; const OnDestroy: TNotifyEvent);
    procedure BeforeDestruction; override;
  end;

{ IWinClipboardCore is queried for by the VCL and FMX/Win TClipboard class helpers in
  order to implement GetAsHandle, SetAsHandle and EnumDataObject methods on TClipboard.
  Simply exposing the IDataObject as a property would risk a reference being held onto
  and so preventing use of the classic clipboard functions. }

  TEnumDataObjectCallback = reference to procedure (const DataObject: IDataObject;
    const FormatEtc: TFormatEtc; var LookForMore: Boolean);

  IWinClipboardCore = interface
  ['{DC1DB577-7255-4772-9A7D-C2FB0944F0EF}']
    function DoGetAsHandle(Format: TClipboardFormat): THandle;
    procedure DoSetAsHandle(Format: TClipboardFormat; Value: THandle); overload;
    procedure DoSetAsHandleDelayed(Format: TClipboardFormat; ValueGetter: TFunc<THandle>); overload;
    procedure EnumDataObject(const Callback: TEnumDataObjectCallback);
  end;

  TDummyChangeCountBehaviour = (dbIncrementing, dbFixed);

  TWinClipboardCore = class(TClipboard.TCore, IWinClipboardCore, TClipboard.IDelayedRendering,
    TClipboard.IReadWriteBytes, TClipboard.IReadWriteStream, TClipboard.IReadWriteFileNames,
    TClipboard.IReadWriteURL, TClipboard.IReadWriteVirtualFile)
  strict private type
    TVirtualFileInfo = class
      Details: TVirtualFileDetails;
      Renderer: TProc<TStream>;
    end;
  strict private class var
    FLegacyFormatToClassMap: TDictionary<TClipboardFormat, TClass>;
    FOEMEncoding: TEncoding;
    FChangeListenerWnd: HWND;
    FRegisteredListenerWndClass: Boolean;
    class constructor InitializeClass;
    class destructor FinalizeClass;
  strict private
    FCustomDataObjectToWrap: IDataObject;
    FDataObjectBuilder: IDataObjectBuilder;
    FDummyChangeCountBehaviour: TDummyChangeCountBehaviour;
    FFilesToWrite: TList<TFunc<TFileName>>;
    FStgMediumsToRelease: TList<TStgMedium>;
    FSysDataObjectToRead: Pointer; //weak ref for us plebs who don't have [Weak]
    FVirtualFilesToWrite: TObjectList<TVirtualFileInfo>;
    procedure StgMediumsToReleaseNotify(Sender: TObject; const Item: TStgMedium;
      Action: TCollectionNotification);
    procedure SysDataObjectToReadDestroy(Sender: TObject);
    function DoGetStrings(const Format: TClipboardFormat; const Encoding: TEncoding): TArray<string>;
    procedure DoSetString(const Format: TClipboardFormat; const Encoding: TEncoding;
      const Renderer: TFunc<string>);
    function DoReadFileNames(out FileNames: TArray<string>; ReturnCountOnly: Boolean = False): Integer;
    procedure DoReadVirtualFiles(DescriptorsOnly: Boolean; const Callback: TEnumVirtualFilesStreamCallback);
    procedure AssignFilesToWrite;
    procedure AssignVirtualFilesToWrite;
    class function ListenerWndProc(Wnd: HWND; Msg: UINT;
      wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall; static;
  private class var
    cfFileContents, cfAnsiFileName, cfUnicodeFileName: TClipboardFormat;
  private
    ChangeNotificationCallback: TThreadMethod;
  strict protected
    constructor Create(const AOwner: TClipboard); overload; override;
    function GetDataObjectToRead: IDataObject;
    property CustomDataObjectToWrap: IDataObject read FCustomDataObjectToWrap;
    property DataObjectBuilder: IDataObjectBuilder read FDataObjectBuilder;
    { TClipboard.TCore }
    function GetChangeCount: TClipboardChangeCount; override;
    function SupportsChangeNotifications: Boolean; override;
    procedure EnableChangeNotifications(const Callback: TThreadMethod); override;
    procedure DisableChangeNotifications; override;
    function WrapsSystemClipboard: Boolean; override;
    function GetFormats: TArray<TClipboardFormat>; override;
    function HasFormat(Format: TClipboardFormat): Boolean; override;
    function HasFormat(const Formats: array of TClipboardFormat; var Matched: TClipboardFormat): Boolean; override;
    function DoOpen: Boolean; override;
    procedure DoClose; override;
    procedure DoClear; override;
    function HasPlainText: Boolean; override;
    function ReadPlainText: TArray<string>; override;
    procedure WritePlainText(const Renderer: TFunc<string>; PreferDelayed: Boolean); override;
    { IWinClipboardCore }
    function DoGetAsHandle(Format: TClipboardFormat): THandle;
    procedure DoSetAsHandle(Format: TClipboardFormat; Value: THandle); overload;
    procedure DoSetAsHandleDelayed(Format: TClipboardFormat; ValueGetter: TFunc<THandle>); overload;
    procedure EnumDataObject(const Callback: TEnumDataObjectCallback);
    { TClipboard.IDelayedRendering }
    function HasOutstandingPromisesToOS: Boolean;
    procedure ResolveOutstandingPromisesToOS(IsExplicitlyRequested: Boolean);
    procedure CancelOutstandingPromisesToOS;
    { TClipboard.IReadWriteBytes }
    function ReadBytes(const Format: TClipboardFormat): TBytes;
    procedure WriteBytes(const Format: TClipboardFormat;
      const Renderer: TFunc<TBytes>; PreferDelayed: Boolean = True);
    { TClipboard.IReadWriteStream }
    function ReadStream(const Format: TClipboardFormat; const Dest: TStream): Boolean;
    procedure WriteStream(const Format: TClipboardFormat;
      const Renderer: TProc<TStream>; PreferDelayed: Boolean);
    { TClipboard.IReadWriteFileNames }
    function HasFile: Boolean;
    function ReadFileNames: TArray<string>;
    procedure WriteFileName(const Renderer: TFunc<TFileName>; PreferDelayed: Boolean);
    { TClipboard.IReadWriteURL }
    function ReadURLs: TArray<string>;
    procedure WriteURL(const Renderer: TFunc<string>; PreferDelayed: Boolean);
    { TClipboard.IReadWriteVirtualFile }
    function ReadVirtualFileDescriptors: TArray<string>;
    procedure ReadVirtualFiles(const Callback: TEnumVirtualFilesStreamCallback);
    procedure SaveVirtualFiles(const Callback: TSaveVirtualFilesCallback);
    procedure WriteVirtualFile(const Details: TVirtualFileDetails;
      const Renderer: TProc<TStream>; PreferDelayed: Boolean);
  protected
    class procedure InitializeFormats(StdFormatValues: TClipboard.TStdFormatValues;
      var CustomFormatsSupported: Boolean); override;
    class function GetFormatName(const Format: TClipboardFormat): string; override;
    class function IsCompatibleVirtualFileDescriptor(const Descriptor: string;
      const SupportedFileExts: TArray<string>): Boolean; override;
  public
    class function RegisterFormat(const Name: string): TClipboardFormat; override;
  public
    class function CreateClipboardForDataObject(const DataObject: IDataObject;
      DummyChangeCountBehaviour: TDummyChangeCountBehaviour): TClipboard;
    constructor Create(const AOwner: TClipboard; const DataObjectToWrap: IDataObject;
      DummyChangeCountBehaviour: TDummyChangeCountBehaviour); reintroduce; overload;
    procedure BeforeDestruction; override;
    destructor Destroy; override;
    class property OEMEncoding: TEncoding read FOEMEncoding;
    { For support of legacy HasFormat(cfPicture) syntax - preferred is HasFormatFor(TPicture).
      Implemented here not in TClipboard to discourage use. }
    class property LegacyFormatToClassMap: TDictionary<TClipboardFormat, TClass> read FLegacyFormatToClassMap;
  end;

  TWinClipboardCoreClass = class of TWinClipboardCore;

  IOleDragHelper = interface
  ['{5CD06C04-474A-4D49-AB47-365CD6B692F5}']
    function GetClipboard: TClipboard;
    function GetMousePos: TPoint;
    procedure SetBitmap(const Value: HBITMAP; TransparentColor: TColorRef = 0);
    procedure SetOffset(const Value: TPoint);
    function Execute: Boolean;
    property Clipboard: TClipboard read GetClipboard;
    property MousePos: TPoint read GetMousePos;
  end;

  TOleDragHelper = class(TDataObjectDestroyNotifier, IDropSource, IOleDragHelper)
  strict private
    FAutoOffset: Boolean;
    FBitmap: HBITMAP;
    FClipboard: TClipboard;
    FEffect: LongInt;
    FMousePos, FOffset: TPoint;
    FTransparentColor: TColorRef;
  protected
    { IDropSource }
    function QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: LongInt): HRESULT; stdcall;
    function GiveFeedback(dwEffect: LongInt): HRESULT; stdcall;
    { IOleDragHelper }
    function GetClipboard: TClipboard;
    function GetMousePos: TPoint;
    procedure SetBitmap(const Value: HBITMAP; TransparentColor: TColorRef = 0);
    procedure SetOffset(const Value: TPoint);
    function Execute: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

function cfBitmap: TClipboardFormat; inline;
function cfMetafilePict: TClipboardFormat; inline;
function cfSYLK: TClipboardFormat; inline;
function cfDIF: TClipboardFormat; inline;
function cfAnsiText: TClipboardFormat; inline;
function cfOemText: TClipboardFormat; inline;
function cfUnicodeText: TClipboardFormat; inline;
function cfDIB: TClipboardFormat; inline;
function cfPalette: TClipboardFormat; inline;
function cfPenData: TClipboardFormat; inline;
function cfRiff: TClipboardFormat; inline;
function cfWave: TClipboardFormat; inline;
function cfEnhMetafile: TClipboardFormat; inline;
function cfHDROP: TClipboardFormat; inline;
function cfLocale: TClipboardFormat; inline;
function cfDIBv5: TClipboardFormat; inline;
function cfDspText: TClipboardFormat; inline;
function cfDspBitmap: TClipboardFormat; inline;
function cfDspMetafilePict: TClipboardFormat; inline;
function cfDspEnhMetafile: TClipboardFormat; inline;
function cfFileContents: TClipboardFormat; inline;
function cfAnsiFileName: TClipboardFormat; inline;
function cfUnicodeFileName: TClipboardFormat; inline;
function cfDataObject: TClipboardFormat; inline;
{$ENDIF}

implementation

{$IFDEF MSWINDOWS}
uses
  WinApi.Messages, WinApi.RichEdit, WinApi.ShellApi,
  System.StrUtils, System.Math, System.RTLConsts;

function GetPriorityClipboardFormat(const paFormatPriorityList;
  cFormats: Integer): Integer; stdcall; external user32;

function TClipboardFormatHelper.GetHandle: TClipFormat;
begin
  Result := TClipFormat(NativeUInt(Self));
end;

class function TClipboardFormatHelper.Wrap(const Value: TClipFormat): TClipboardFormat;
begin
  NativeUInt(Result) := Value;
end;

function cfBitmap: TClipboardFormat;
begin
  Result := TClipboardFormat.Wrap(CF_BITMAP);
end;

function cfMetafilePict: TClipboardFormat;
begin
  Result := TClipboardFormat.Wrap(CF_METAFILEPICT);
end;

function cfSYLK: TClipboardFormat;
begin
  Result := TClipboardFormat.Wrap(CF_SYLK);
end;

function cfDIF: TClipboardFormat;
begin
  Result := TClipboardFormat.Wrap(CF_DIF);
end;

function cfAnsiText: TClipboardFormat;
begin
  Result := TClipboardFormat.Wrap(CF_TEXT);
end;

function cfOemText: TClipboardFormat;
begin
  Result := TClipboardFormat.Wrap(CF_OEMTEXT);
end;

function cfUnicodeText: TClipboardFormat;
begin
  Result := TClipboardFormat.Wrap(CF_UNICODETEXT);
end;

function cfDIB: TClipboardFormat;
begin
  Result := TClipboardFormat.Wrap(CF_DIB);
end;

function cfPalette: TClipboardFormat;
begin
  Result := TClipboardFormat.Wrap(CF_PALETTE);
end;

function cfPenData: TClipboardFormat;
begin
  Result := TClipboardFormat.Wrap(CF_PENDATA);
end;

function cfRiff: TClipboardFormat;
begin
  Result := TClipboardFormat.Wrap(CF_RIFF);
end;

function cfWave: TClipboardFormat;
begin
  Result := TClipboardFormat.Wrap(CF_WAVE);
end;

function cfEnhMetafile: TClipboardFormat;
begin
  Result := TClipboardFormat.Wrap(CF_ENHMETAFILE);
end;

function cfHDROP: TClipboardFormat;
begin
  Result := TClipboardFormat.Wrap(CF_HDROP);
end;

function cfLocale: TClipboardFormat;
begin
  Result := TClipboardFormat.Wrap(CF_LOCALE);
end;

function cfDIBv5: TClipboardFormat;
begin
  Result := TClipboardFormat.Wrap(CF_DIBV5);
end;

function cfDspText: TClipboardFormat;
begin
  Result := TClipboardFormat.Wrap(CF_DSPTEXT);
end;

function cfDspBitmap: TClipboardFormat;
begin
  Result := TClipboardFormat.Wrap(CF_DSPBITMAP);
end;

function cfDspMetafilePict: TClipboardFormat;
begin
  Result := TClipboardFormat.Wrap(CF_DSPMETAFILEPICT);
end;

function cfDspEnhMetafile: TClipboardFormat;
begin
  Result := TClipboardFormat.Wrap(CF_DSPENHMETAFILE);
end;

function cfFileContents: TClipboardFormat;
begin
  Result := TWinClipboardCore.cfFileContents;
end;

function cfAnsiFileName: TClipboardFormat;
begin
  Result := TWinClipboardCore.cfAnsiFileName;
end;

function cfUnicodeFileName: TClipboardFormat;
begin
  Result := TWinClipboardCore.cfUnicodeFileName;
end;

function cfDataObject: TClipboardFormat;
begin
  Result := TClipboardFormat.Wrap($C009);
end;

procedure Win32Check(RetVal: BOOL);
begin
  if not RetVal then RaiseLastOSError;
end;

function CreateBytesFromHGLOBAL(const Global: HGLOBAL): TBytes;
var
  Ptr: PByte;
begin
  if Global = 0 then
    Exit(nil);
  Ptr := GlobalLock(Global);
  try
    SetLength(Result, GlobalSize(Global));
    if Result <> nil then Move(Ptr^, Result[0], Length(Result));
  finally
    GlobalUnlock(Global);
  end;
end;

procedure InitMediumHGlobal(NeededSize: SIZE_T; AllocResourceInMedium: Boolean; var Medium: TStgMedium);
begin
  if AllocResourceInMedium then
    Medium.hGlobal := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE or GMEM_ZEROINIT, NeededSize)
  else if GlobalSize(Medium.hGlobal) < NeededSize then
    OleError(E_OUTOFMEMORY); //MSDN docs say not to realloc
  if Medium.hGlobal = 0 then RaiseLastOSError;
end;

procedure InitAndAssignMediumHGlobal(const Buffer; BufferSize: NativeInt;
  AllocResourceInMedium: Boolean; var Medium: TStgMedium); overload;
var
  Ptr: Pointer;
begin
  InitMediumHGlobal(BufferSize, AllocResourceInMedium, Medium);
  Ptr := GlobalLock(Medium.hGlobal);
  try
    Move(Buffer, Ptr^, BufferSize)
  finally
    GlobalUnlock(Medium.hGlobal);
  end;
end;

procedure InitAndAssignMediumHGlobal(const SourceToCopy: HGLOBAL;
  AllocResourceInMedium: Boolean; var Medium: TStgMedium); overload;
var
  SourcePtr, DestPtr: Pointer;
  Size: Integer;
begin
  Size := GlobalSize(SourceToCopy);
  InitMediumHGlobal(Size, AllocResourceInMedium, Medium);
  SourcePtr := GlobalLock(SourceToCopy);
  DestPtr := GlobalLock(Medium.hGlobal);
  try
    Move(SourcePtr^, DestPtr^, Size)
  finally
    GlobalUnlock(Medium.hGlobal);
    GlobalUnlock(SourceToCopy);
  end;
end;

function GetStgMediumFromStgMedium(const Source: TStgMedium; var Dest: TStgMedium;
  OutputKind: TDataObject.TOutputKind): HRESULT;
var
  Pos: Int64;
  PictToCopyPtr: PMetafilePict;
  NewPict: TMetafilePict;
begin
  { While this isn't a full 'TStgMedium copy' implementation, in the case of
    OutputKind = okGetInPlace, the only valid medium types *ever* to
    request per the MSDN docs are TYMED_HGLOBAL, TYMED_FILE, TYMED_ISTREAM and
    TYMED_ISTORAGE. }
  try
    case OutputKind of
      okQueryOnly: Exit(S_OK);
      okGetInPlace:
      begin
        if Dest.tymed <> Source.tymed then Exit(DV_E_TYMED);
        Dest.unkForRelease := nil;
      end;
      okAllocResourceInMedium:
      begin
        Dest.tymed := Source.tymed;
        Dest.unkForRelease := nil;
      end;
    end;
    case Source.tymed of
      TYMED_HGLOBAL:
      begin
        InitAndAssignMediumHGlobal(Source.hGlobal, OutputKind = okAllocResourceInMedium, Dest);
        Exit(S_OK);
      end;
      TYMED_ISTREAM:
      begin
        if OutputKind = okAllocResourceInMedium then
        begin
          IStream(Source.stm).Seek(0, STREAM_SEEK_END, Pos);
          IStream(Dest.stm) := IStream(Source.stm);
          Exit(S_OK);
        end;
        if Dest.stm = nil then Exit(DV_E_STGMEDIUM);
        IStream(Source.stm).Seek(0, STREAM_SEEK_SET, Pos);
        Result := IStream(Source.stm).CopyTo(IStream(Dest.stm),
          High(Int64), PInt64(nil)^, PInt64(nil)^);
        Exit;
      end;
      TYMED_GDI:
      begin
        if OutputKind <> okAllocResourceInMedium then Exit(DV_E_TYMED);
        Dest.hBitmap := CopyImage(Source.hBitmap, IMAGE_BITMAP, 0, 0, 0);
        if Dest.hBitmap = 0 then
          Exit(HResultFromWin32(GetLastError))
        else
          Exit(S_OK);
      end;
      TYMED_ENHMF:
      begin
        if OutputKind <> okAllocResourceInMedium then Exit(DV_E_TYMED);
        Dest.hEnhMetaFile := CopyEnhMetaFile(Source.hEnhMetaFile, nil);
        if Dest.hEnhMetaFile = 0 then
          Exit(HResultFromWin32(GetLastError))
        else
          Exit(S_OK);
      end;
      TYMED_MFPICT:
      begin
        if OutputKind <> okAllocResourceInMedium then Exit(DV_E_TYMED);
        PictToCopyPtr := GlobalLock(Source.hMetaFilePict);
        try
          NewPict := PictToCopyPtr^;
          NewPict.hMF := CopyMetaFile(NewPict.hMF, nil);
          if NewPict.hMF = 0 then
            Exit(HResultFromWin32(GetLastError))
          else
            Exit(S_OK);
        finally
          GlobalUnlock(Source.hMetaFilePict);
        end;
        InitAndAssignMediumHGlobal(NewPict, SizeOf(NewPict), True, Dest);
        Exit(S_OK);
      end;
    else
      Exit(DV_E_TYMED);
    end;
  except
    on E: EOleSysError do
      Result := E.ErrorCode
    else
      Result := E_UNEXPECTED;
  end;
end;

//procedure GetBitmapFromMetafilePict(const Source: HGLOBAL; Dest: TBitmap);
//var
//  BitmapInfo: TBitmapInfo;
//  Bytes: TBytes;
//  EnhMetafile: HENHMETAFILE;
//  EnhHeader: TEnhMetaHeader;
//  ScreenDC, MemDC: HDC;
//  MemBitmap, PrevBitmap: HBITMAP;
//  Pict: PMetafilePict;
//  MapData: TBitmapData;
//begin
//  Pict := GlobalLock(Source);
//  try
//    SetLength(Bytes, GetMetaFileBitsEx(Pict.hMF, 0, nil));
//    GetMetaFileBitsEx(Pict.hMF, Length(Bytes), Bytes);
//    EnhMetafile := 0;
//    ScreenDC := GetDC(0);
//    try
//      EnhMetafile := SetWinMetaFileBits(Length(Bytes), @Bytes[0], ScreenDC, Pict^);
//      GetEnhMetaFileHeader(EnhMetafile, SizeOf(EnhHeader), @EnhHeader);
//      MemDC := CreateCompatibleDC(ScreenDC);
//      if MemDC = 0 then RaiseLastOSError;
//      MemBitmap := CreateCompatibleBitmap(MemDC,
//        EnhHeader.rclBounds.Width + 1, EnhHeader.rclBounds.Height + 1);
//      if MemBitmap = 0 then RaiseLastOSError;
//      PrevBitmap := SelectObject(MemDC, MemBitmap);
//      if not PlayEnhMetaFile(MemDC, EnhMetafile, EnhHeader.rclBounds) then
//        RaiseLastOSError;
//      Dest.SetSize(EnhHeader.rclBounds.Width + 1, EnhHeader.rclBounds.Height + 1);
//      Dest.Clear(TAlphaColors.Null);
//      Dest.Map(TMapAccess.Write, MapData);
//      try
//        GetDIBits(MemDC, MemBitmap, 0, MapData.Height, MapData.Data, BitmapInfo, DIB_RGB_COLORS);
//      finally
//        Dest.Unmap(MapData);
//      end;
//      SelectObject(MemDC, PrevBitmap);
//      DeleteObject(MemBitmap);
//      DeleteDC(MemDC);
//    finally
//      if EnhMetafile <> 0 then DeleteEnhMetaFile(EnhMetafile);
//      ReleaseDC(0, ScreenDC);
//    end;
//  finally
//    GlobalUnlock(Source);
//  end;
//end;

{ IDataObject helpers }

function GetFormatCountInDataObject(const DataObject: IDataObject): Integer;
var
  Enumerator: IEnumFORMATETC;
  FormatEtc: TFormatEtc;
begin
  OleCheck(DataObject.EnumFormatEtc(DATADIR_GET, Enumerator));
  OleCheck(Enumerator.Reset);
  Result := 0;
  while Enumerator.Next(1, FormatEtc, nil) = S_OK do
    if FormatEtc.dwAspect = DVASPECT_CONTENT then
      Inc(Result);
end;

function GetFormatsInDataObject(const DataObject: IDataObject): TArray<TClipboardFormat>;
var
  Count: Integer;
  Enumerator: IEnumFORMATETC;
  FormatEtc: TFormatEtc;
begin
  OleCheck(DataObject.EnumFormatEtc(DATADIR_GET, Enumerator));
  OleCheck(Enumerator.Reset);
  Count := 0;
  while Enumerator.Next(1, FormatEtc, nil) = S_OK do
    if FormatEtc.dwAspect = DVASPECT_CONTENT then
    begin
      if Length(Result) = Count then
        SetLength(Result, Count + 16);
      Result[Count] := TClipboardFormat.Wrap(FormatEtc.cfFormat);
      Inc(Count);
    end;
  SetLength(Result, Count);
end;

function HasFormatInDataObject(const DataObject: IDataObject;
  const Format: TClipboardFormat): Boolean; overload;
var
  Etc: TFormatEtc;
begin
  Etc.cfFormat := Format.Handle;
  Etc.ptd := nil;
  Etc.dwAspect := DVASPECT_CONTENT;
  Etc.lindex := -1;
  case Format.Handle of
    CF_BITMAP: Etc.tymed := TYMED_GDI;
    CF_METAFILEPICT: Etc.tymed := TYMED_MFPICT;
    CF_ENHMETAFILE: Etc.tymed := TYMED_ENHMF;
  else Etc.tymed := TYMED_HGLOBAL;
  end;
  Result := (DataObject.QueryGetData(Etc) = S_OK);
  if not Result and (Etc.tymed = TYMED_HGLOBAL) then
  begin
    Etc.tymed := TYMED_ISTREAM;
    Result := (DataObject.QueryGetData(Etc) = S_OK);
  end;
end;

function HasFormatInDataObject(const DataObject: IDataObject;
  const Formats: array of TClipboardFormat; var Matched: TClipboardFormat): Boolean; overload;
var
  I: Integer;
begin
  for I := Low(Formats) to High(Formats) do
    if HasFormatInDataObject(DataObject, Formats[I]) then
    begin
      Matched := Formats[I];
      Exit(True);
    end;
  Result := False;
end;

function GetBytesInDataObject(const DataObject: IDataObject; Format: TClipboardFormat;
  Index: Integer = -1): TBytes;
var
  Etc: TFormatEtc;
  Medium: TStgMedium;
begin
  Result := nil;
  Etc.cfFormat := Format.Handle;
  Etc.ptd := nil;
  Etc.dwAspect := DVASPECT_CONTENT;
  Etc.lindex := Index;
  Etc.tymed := TYMED_HGLOBAL;
  if DataObject.GetData(Etc, Medium) <> S_OK then Exit;
  try
    Result := CreateBytesFromHGLOBAL(Medium.hGlobal)
  finally
    ReleaseStgMedium(Medium);
  end;
end;

{ TOleStreamWrapper }

constructor TOleStreamWrapper.Create(const AStream: IStream);
begin
  inherited Create;
  FStream := AStream;
end;

function TOleStreamWrapper.Read(var Buffer; Count: LongInt): LongInt;
begin
  OleCheck(FStream.Read(@Buffer, Count, @Result));
end;

function TOleStreamWrapper.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  OleCheck(FStream.Seek(Offset, Ord(Origin), Result));
end;

function TOleStreamWrapper.Write(const Buffer; Count: LongInt): LongInt;
begin
  OleCheck(FStream.Write(@Buffer, Count, @Result));
end;

{ TDataObjectBuilderBase }

procedure TDataObjectBuilderBase.AddFormat(const Format: TClipboardFormat; Index: LongInt;
  const OnGetData: TFunc<TBytes>);
begin
  AddFormat(Format, Index,
    TYMED_HGLOBAL,
      procedure (AllocResourceInMedium: Boolean; var Medium: TStgMedium)
      var
        Data: TBytes;
        Ptr: PByte;
      begin
        Data := OnGetData;
        InitMediumHGlobal(Length(Data), AllocResourceInMedium, Medium);
        if Data = nil then Exit;
        Ptr := GlobalLock(Medium.hGlobal);
        try
          Move(Data[0], Ptr^, Length(Data));
        finally
          GlobalUnlock(Medium.hGlobal);
        end;
      end,
    TYMED_ISTREAM,
      procedure (AllocResourceInMedium: Boolean; var Medium: TStgMedium)
      var
        Data: TBytes;
      begin
        Data := OnGetData;
        if AllocResourceInMedium then
          IStream(Medium.stm) := TStreamAdapter.Create(TBytesStream.Create(Data), soOwned) as IStream
        else
          IStream(Medium.stm).Write(Data, Length(Data), nil);
      end
    );
end;

procedure TDataObjectBuilderBase.AddFormat(const Format: TClipboardFormat; Index: LongInt;
  const OnGetData: TProc<TStream>);
begin
  AddFormat(Format, Index,
    TYMED_ISTREAM,
      procedure (AllocResourceInMedium: Boolean; var Medium: TStgMedium)
      var
        Stream: TStream;
      begin
        if AllocResourceInMedium then
        begin
          Stream := TMemoryStream.Create;
          try
            OnGetData(Stream);
            IStream(Medium.stm) := TStreamAdapter.Create(Stream, soOwned) as IStream;
          except
            Stream.Free;
            raise;
          end;
        end
        else
        begin
          Stream := TOleStreamWrapper.Create(IStream(Medium.stm));
          try
            OnGetData(Stream);
          finally
            Stream.Free;
          end;
        end;
      end,
    TYMED_HGLOBAL,
      procedure (AllocResourceInMedium: Boolean; var Medium: TStgMedium)
      var
        Stream: TMemoryStream;
      begin
        Stream := TMemoryStream.Create;
        try
          OnGetData(Stream);
          InitAndAssignMediumHGlobal(Stream.Memory^, Stream.Size, AllocResourceInMedium, Medium);
        finally
          Stream.Free;
        end;
      end
  );
end;

{ TDataObjectBuilder }

constructor TDataObjectBuilder.Create(const ADataObject: IDataObject);
begin
  inherited Create;
  FDataObject := ADataObject;
end;

procedure TDataObjectBuilder.AddFormat(const Format: TClipboardFormat; Index: LongInt;
  var StgMediumToOwn: TStgMedium);
var
  Etc: TFormatEtc;
  Result: HRESULT;
begin
  Etc.cfFormat := Format.Handle;
  Etc.ptd := nil;
  Etc.dwAspect := DVASPECT_CONTENT;
  Etc.lindex := Index;
  Etc.tymed := StgMediumToOwn.tymed;
  Result := FDataObject.SetData(Etc, StgMediumToOwn, True);
  if Result <> S_OK then
    ReleaseStgMedium(StgMediumToOwn);
  OleCheck(Result);
end;

procedure TDataObjectBuilder.AddFormat(const Format: TClipboardFormat; Index: LongInt;
  PreferredTymed: LongInt; const OnGetData: TGetDataObjectDataEvent;
  FallbackTymed: LongInt; const OnGetDataFallback: TGetDataObjectDataEvent);
var
  Etc: TFormatEtc;
  Medium: TStgMedium;
  Result: HRESULT;
begin
  Etc.cfFormat := Format.Handle;
  Etc.ptd := nil;
  Etc.dwAspect := DVASPECT_CONTENT;
  Etc.lindex := Index;
  Etc.tymed := PreferredTymed;
  ZeroMemory(@Medium, SizeOf(Medium));
  OnGetData(True, Medium);
  Result := FDataObject.SetData(Etc, Medium, True);
  if Result = S_OK then Exit;
  ReleaseStgMedium(Medium);
  Etc.cfFormat := FallbackTymed;
  ZeroMemory(@Medium, SizeOf(Medium));
  OnGetDataFallback(True, Medium);
  Result := FDataObject.SetData(Etc, Medium, True);
  if Result = S_OK then Exit;
  ReleaseStgMedium(Medium);
  OleCheck(Result);
end;

function TDataObjectBuilder.GetDataObject: IDataObject;
begin
  Result := FDataObject;
end;

function TDataObjectBuilder.GetEmpty: Boolean;
var
  Etc: TFormatEtc;
  Enumerator: IEnumFORMATETC;
begin
  OleCheck(FDataObject.EnumFormatEtc(DATADIR_GET, Enumerator));
  OleCheck(Enumerator.Reset);
  Result := (Enumerator.Next(1, Etc, nil) <> S_OK);
end;

{ TDataObject.TSource }

constructor TDataObject.TSource.Create(const ClipFormat: TClipFormat; Index: LongInt);
begin
  inherited Create;
  FFormatEtc.cfFormat := ClipFormat;
  FFormatEtc.ptd := nil;
  FFormatEtc.dwAspect := DVASPECT_CONTENT;
  FFormatEtc.lindex := Index;
  FStgMedium.tymed := TYMED_NULL;
end;

destructor TDataObject.TSource.Destroy;
begin
  ReleaseStgMedium;
  inherited;
end;

procedure TDataObject.TSource.ReleaseStgMedium;
begin
  if FStgMedium.tymed = TYMED_NULL then Exit;
  WinApi.ActiveX.ReleaseStgMedium(FStgMedium);
  ZeroMemory(@FStgMedium, SizeOf(FStgMedium));
end;

procedure TDataObject.TSource.Assign(PreferredTymed: LongInt;
  const OnGetData: TGetDataObjectDataEvent; FallbackTymed: LongInt;
  const OnGetDataFallback: TGetDataObjectDataEvent);
begin
  ReleaseStgMedium;
  FDataRetrievedCount := 0;
  FFormatEtc.tymed := PreferredTymed;
  FOnGetData := OnGetData;
  FFallbackTymed := FallbackTymed;
  FOnGetDataFallback := OnGetDataFallback;
end;

procedure TDataObject.TSource.Assign(const StgMediumToOwn: TStgMedium);
begin
  Assert(StgMediumToOwn.tymed <> TYMED_NULL);
  ReleaseStgMedium;
  FDataRetrievedCount := 0;
  FFormatEtc.tymed := StgMediumToOwn.tymed;
  FStgMedium := StgMediumToOwn;
end;

function TDataObject.TSource.DoGetData(const WantedFormat: TFormatEtc;
  var Medium: TStgMedium; OutputKind: TOutputKind): HRESULT;

  function TryTymed(Tymed: LongInt; const GetDataProc: TGetDataObjectDataEvent): Boolean;
  begin
    Result := (WantedFormat.tymed and Tymed <> 0) and Assigned(GetDataProc);
    if not Result then Exit;
    case OutputKind of
      okAllocResourceInMedium:
      begin
        Medium.tymed := Tymed;
        Medium.hGlobal := 0;
      end;
      okQueryOnly: Exit;
    end;
    Medium.unkForRelease := nil;
    GetDataProc(OutputKind = okAllocResourceInMedium, Medium);
  end;
begin
  if WantedFormat.cfFormat <> ClipFormat then Exit(DV_E_FORMATETC);
  if WantedFormat.dwAspect <> FFormatEtc.dwAspect then Exit(DV_E_DVASPECT);
  if (WantedFormat.lindex <> -1) and (WantedFormat.lindex <> FFormatEtc.lindex) then Exit(DV_E_LINDEX);
  if FStgMedium.tymed <> TYMED_NULL then
    if WantedFormat.tymed and FFormatEtc.tymed <> 0 then
      Result := GetStgMediumFromStgMedium(FStgMedium, Medium, OutputKind)
    else
      Result := DV_E_TYMED
  else
    if TryTymed(PreferredTymed, FOnGetData) or TryTymed(FallbackTymed, FOnGetDataFallback) then
      Result := S_OK
    else
      Result := DV_E_TYMED;
  if (Result = S_OK) and (OutputKind <> okQueryOnly) then
    Inc(FDataRetrievedCount);
end;

{ TDataObject.TEnumFormatEtc }

constructor TDataObject.TEnumFormatEtc.Create(const ASource: TDataObject; AStartAt: Integer);
begin
  inherited Create;
  FNextIndex := AStartAt;
  FSource := ASource;
  FSource._AddRef;
end;

destructor TDataObject.TEnumFormatEtc.Destroy;
begin
  FSource._Release;
  inherited;
end;

function TDataObject.TEnumFormatEtc.Clone(out Enum: IEnumFormatEtc): HRESULT;
begin
  Enum := TEnumFormatEtc.Create(FSource, FNextIndex);
  Result := S_OK;
end;

function TDataObject.TEnumFormatEtc.Next(celt: Integer; out elt;
  pceltFetched: PLongint): HRESULT;
var
  I: Integer;
  Ptr: PFormatEtc;
begin
  if FNextIndex + celt > FSource.FormatCount then
  begin
    celt := FSource.FormatCount - FNextIndex;
    if celt < 0 then celt := 0;
    Result := S_FALSE;
  end
  else
  begin
    Result := S_OK;
  end;
  Ptr := @elt;
  for I := FNextIndex to FNextIndex + celt - 1 do
  begin
    Ptr^ := FSource.FormatEtcs[I];
    Inc(Ptr);
  end;
  Inc(FNextIndex, celt);
  if pceltFetched <> nil then
    pceltFetched^ := celt;
end;

function TDataObject.TEnumFormatEtc.Reset: HRESULT;
begin
  FNextIndex := 0;
  Result := S_OK;
end;

function TDataObject.TEnumFormatEtc.Skip(celt: Integer): HRESULT;
begin
  if FNextIndex + celt > FSource.FormatCount then
  begin
    FNextIndex := FSource.FormatCount;
    Result := S_FALSE;
  end
  else
  begin
    Inc(FNextIndex, celt);
    Result := S_OK;
  end;
end;

{ TDataObject }

constructor TDataObject.Create;
begin
  inherited Create;
  FSources := TObjectList<TSource>.Create;
end;

destructor TDataObject.Destroy;
begin
  FSources.Free;
  inherited;
end;

function TDataObject.FindOrAddSource(const ClipFormat: TClipFormat; Index: Integer): TSource;
var
  I: Integer;
begin
  for I := 0 to FSources.Count - 1 do
    if (FSources[I].ClipFormat = ClipFormat) and (FSources[I].Index = Index) then
    begin
      Result := FSources[I];
      Exit;
    end;
  Result := TSource.Create(ClipFormat, Index);
  FSources.Add(Result)
end;

procedure TDataObject.AddFormat(Format: TClipFormat; Index: LongInt; var StgMediumToOwn: TStgMedium);
begin
  try
    FindOrAddSource(Format, Index).Assign(StgMediumToOwn);
  except
    ReleaseStgMedium(StgMediumToOwn);
    raise;
  end;
end;

procedure TDataObject.AddFormat(const Format: TClipboardFormat; Index: LongInt;
  var StgMediumToOwn: TStgMedium);
begin
  AddFormat(Format.Handle, Index, StgMediumToOwn);
end;

procedure TDataObject.AddFormat(const Format: TClipboardFormat; Index: LongInt;
  PreferredTymed: LongInt; const OnGetData: TGetDataObjectDataEvent;
  FallbackTymed: LongInt; const OnGetDataFallback: TGetDataObjectDataEvent);
begin
  if not Assigned(OnGetData) then
    raise EArgumentNilException.CreateRes(@SArgumentNil);
  FindOrAddSource(Format.Handle, Index).Assign(
    PreferredTymed, OnGetData, FallbackTymed, OnGetDataFallback);
end;

//procedure TDataObject.ClearFormats;
//begin
//  FSources.Clear;
//end;

function TDataObject.GetDataObject: IDataObject;
begin
  Result := Self;
end;

function TDataObject.GetEmpty: Boolean;
begin
  Result := FSources.Count = 0;
end;

function TDataObject.DAdvise(const formatetc: TFormatEtc; advf: Integer;
  const advSink: IAdviseSink; out dwConnection: Integer): HRESULT;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TDataObject.DUnadvise(dwConnection: Integer): HRESULT;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TDataObject.EnumDAdvise(out enumAdvise: IEnumStatData): HRESULT;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TDataObject.EnumFormatEtc(dwDirection: Integer;
  out enumFormatEtc: IEnumFormatEtc): HRESULT;
begin
  if dwDirection = DATADIR_GET then
  begin
    enumFormatEtc := TEnumFormatEtc.Create(Self);
    Result := S_OK;
  end
  else
    Result := E_NOTIMPL;
end;

function TDataObject.GetCanonicalFormatEtc(const formatetc: TFormatEtc;
  out formatetcOut: TFormatEtc): HRESULT;
begin
  formatetcOut := formatetc;
  formatetcOut.ptd := nil;
  Result := DATA_S_SAMEFORMATETC;
end;

function TDataObject.DoGetData(const FormatEtc: TFormatEtc;
  var Medium: TStgMedium; OutputKind: TOutputKind): HRESULT;
var
  I: Integer;
  ItemResult: HRESULT;
begin
  Result := DV_E_FORMATETC;
  for I := 0 to FSources.Count - 1 do
  begin
    ItemResult := FSources[I].DoGetData(FormatEtc, Medium, OutputKind);
    if Succeeded(ItemResult) then Exit(ItemResult);
    if Result = DV_E_FORMATETC then Result := ItemResult;
  end;
end;

function TDataObject.GetData(const formatetcIn: TFormatEtc; out medium: TStgMedium): HRESULT;
begin
  Result := DoGetData(formatetcIn, medium, okAllocResourceInMedium);
end;

function TDataObject.GetDataHere(const formatetc: TFormatEtc;
  out medium: TStgMedium): HRESULT;
begin
  Result := DoGetData(formatetc, medium, okGetInPlace);
end;

function TDataObject.GetFormatCount: Integer;
begin
  Result := FSources.Count;
end;

function TDataObject.GetFormatEtc(Index: Integer): TFormatEtc;
begin
  Result := FSources[Index].FormatEtc;
end;

function TDataObject.QueryGetData(const formatetc: TFormatEtc): HRESULT;
begin
  Result := DoGetData(formatetc, PStgMedium(nil)^, okQueryOnly);
end;

function TDataObject.SetData(const formatetc: TFormatEtc;
  var medium: TStgMedium; fRelease: BOOL): HRESULT;
var
  NewMedium: TStgMedium;
begin
  try
    if (formatetc.dwAspect <> DVASPECT_CONTENT) and (formatetc.ptd <> nil) then
      Result := DV_E_DVASPECT
    else
    begin
      if fRelease then
      begin
        AddFormat(formatetc.cfFormat, formatetc.lindex, medium);
        Result := S_OK;
      end
      else
      begin
        Result := GetStgMediumFromStgMedium(medium, NewMedium, okAllocResourceInMedium);
        if Result = S_OK then
          AddFormat(formatetc.cfFormat, formatetc.lindex, NewMedium);
      end;
    end;
  except
    on E: EOleSysError do
      Result := E.ErrorCode
    else
      Result := E_UNEXPECTED;
  end;
end;

function TDataObject.HaveAllFormatsBeenRetrieved: Boolean;
var
  Source: TSource;
begin
  for Source in FSources do
    if Source.DataRetrievedCount = 0 then Exit(False);
  Result := True;
end;

function TDataObject.GetIsOleClipboard: Boolean;
begin
  Result := (OleIsCurrentClipboard(Self) = S_OK);
end;

procedure TDataObject.SetIsOleClipboard(Value: Boolean);
begin
  if Value = GetIsOleClipboard then Exit;
  if Value then
    if GetEmpty then
      OleSetClipboard(nil)
    else
      OleCheck(OleSetClipboard(Self))
  else
    if OleSetClipboard(nil) <> S_OK then
      OleCheck(OleFlushClipboard);
end;

{ TWinClipboardCore }

class constructor TWinClipboardCore.InitializeClass;
begin
  FLegacyFormatToClassMap := TDictionary<TClipboardFormat, TClass>.Create;
  FOEMEncoding := TMBCSEncoding.Create(CP_OEMCP);
end;

class destructor TWinClipboardCore.FinalizeClass;
begin
  FLegacyFormatToClassMap.Free;
  FOEMEncoding.Free;
end;

type
  TAdditionalClipboard = class(TClipboard)
  public
    constructor Create(MinCoreClass: TWinClipboardCoreClass; const DataObject: IDataObject;
      DummyChangeCountBehaviour: TDummyChangeCountBehaviour);
  end;

constructor TAdditionalClipboard.Create(MinCoreClass: TWinClipboardCoreClass;
  const DataObject: IDataObject; DummyChangeCountBehaviour: TDummyChangeCountBehaviour);
var
  LClass: TWinClipboardCoreClass;
begin
  LClass := TWinClipboardCoreClass(GetSuitableCoreClass(MinCoreClass));
  inherited Create(LClass.Create(Self, DataObject, DummyChangeCountBehaviour));
end;

class function TWinClipboardCore.CreateClipboardForDataObject(const DataObject: IDataObject;
  DummyChangeCountBehaviour: TDummyChangeCountBehaviour): TClipboard;
begin
  Result := TAdditionalClipboard.Create(Self, DataObject, DummyChangeCountBehaviour);
end;

class procedure TWinClipboardCore.InitializeFormats(
  StdFormatValues: TClipboard.TStdFormatValues; var CustomFormatsSupported: Boolean);
begin
  StdFormatValues.cfUnicodeText := TClipboardFormat.Wrap(CF_UNICODETEXT);
  StdFormatValues.cfRTF := RegisterFormat(CF_RTF);
  StdFormatValues.cfFileName := cfUnicodeFileName;
  StdFormatValues.cfGIF := RegisterFormat('GIF Image'); //VCL TGifImage
  StdFormatValues.cfJPEG := RegisterFormat('JFIF');     //Java AWT
  StdFormatValues.cfPNG := RegisterFormat('PNG');       //MS Word
  StdFormatValues.cfTIFF := TClipboardFormat.Wrap(CF_TIFF);
  StdFormatValues.cfURL := RegisterFormat(CFSTR_INETURL);
  StdFormatValues.cfVirtualFileDescriptor := RegisterFormat(CFSTR_FILEDESCRIPTOR);
  CustomFormatsSupported := True;
end;

class function TWinClipboardCore.GetFormatName(const Format: TClipboardFormat): string;
var
  Buffer: array[Byte] of Char;
begin
  case Format.Handle of
    CF_TEXT: Result := 'Text';
    CF_BITMAP: Result := 'Bitmap';
    CF_METAFILEPICT: Result := 'Windows Metafile';
    CF_SYLK: Result := 'SYLK';
    CF_DIF: Result := 'DIF';
    CF_TIFF: Result := 'TIFF';
    CF_OEMTEXT: Result := 'OEM Text';
    CF_DIB: Result := 'DIB';
    CF_PALETTE: Result := 'Palette';
    CF_PENDATA: Result := 'Pen Data';
    CF_RIFF: Result := 'RIFF';
    CF_WAVE: Result := 'Wave Audio';
    CF_UNICODETEXT: Result := 'Unicode Text';
    CF_ENHMETAFILE: Result := 'Enhanced Metafile';
    CF_HDROP: Result := 'File List (HDROP)';
    CF_LOCALE: Result := 'Locale Identifier';
    CF_DIBV5: Result := 'DIB v5';
    CF_OWNERDISPLAY: Result := 'Owner-Drawn Display';
    CF_DSPTEXT: Result := 'Display Text';
    CF_DSPBITMAP: Result := 'Display Bitmap';
    CF_DSPMETAFILEPICT: Result := 'Display WMF';
    CF_DSPENHMETAFILE: Result := 'Display EMF';
  else
    SetString(Result, Buffer, GetClipboardFormatName(Format.Handle, Buffer, Length(Buffer)));
    if Result = '' then raise EArgumentException.CreateRes(@sArgumentInvalid);
  end;
end;

class function TWinClipboardCore.RegisterFormat(const Name: string): TClipboardFormat;
begin
  Result := TClipboardFormat.Wrap(RegisterClipboardFormat(PChar(Name)));
end;

class function TWinClipboardCore.IsCompatibleVirtualFileDescriptor(const Descriptor: string;
  const SupportedFileExts: TArray<string>): Boolean;
begin
  Result := MatchFileExt(Descriptor, SupportedFileExts);
end;

constructor TWinClipboardCore.Create(const AOwner: TClipboard);
begin
  inherited;
  FFilesToWrite := TList<TFunc<TFileName>>.Create;
  FStgMediumsToRelease := TList<TStgMedium>.Create;
  FStgMediumsToRelease.OnNotify := StgMediumsToReleaseNotify;
  FVirtualFilesToWrite := TObjectList<TVirtualFileInfo>.Create;
end;

constructor TWinClipboardCore.Create(const AOwner: TClipboard;
  const DataObjectToWrap: IDataObject; DummyChangeCountBehaviour: TDummyChangeCountBehaviour);
begin
  Create(AOwner);
  FCustomDataObjectToWrap := DataObjectToWrap;
  FDummyChangeCountBehaviour := DummyChangeCountBehaviour;
end;

procedure TWinClipboardCore.BeforeDestruction;
var
  Intf: IClipboardDataObject;
begin
  if Supports(FDataObjectBuilder, IClipboardDataObject, Intf) and Intf.IsOleClipboard then
    OleFlushClipboard;
  inherited;
end;

destructor TWinClipboardCore.Destroy;
begin
  FFilesToWrite.Free;
  FStgMediumsToRelease.Free;
  FVirtualFilesToWrite.Free;
  inherited;
end;

procedure TWinClipboardCore.StgMediumsToReleaseNotify(Sender: TObject;
  const Item: TStgMedium; Action: TCollectionNotification);
begin
  if Action = cnRemoved then ReleaseStgMedium(PStgMedium(@Item)^);
end;

function TWinClipboardCore.GetDataObjectToRead: IDataObject;
begin
  if FCustomDataObjectToWrap <> nil then
    Result := FCustomDataObjectToWrap
  else if FSysDataObjectToRead <> nil then
    Result := IDataObject(FSysDataObjectToRead)
  else
  begin
    if Clipboard.OpenCount > 0 then CloseClipboard;
    OleCheck(OleGetClipboard(Result));
    Result := TDataObjectDestroyNotifier.Create(Result, SysDataObjectToReadDestroy);
    FSysDataObjectToRead := Pointer(Result);
  end;
end;

procedure TWinClipboardCore.SysDataObjectToReadDestroy(Sender: TObject);
begin
  FSysDataObjectToRead := nil;
  if Clipboard.OpenCount > 0 then OpenClipboard(0);
end;

function TWinClipboardCore.DoGetStrings(const Format: TClipboardFormat;
  const Encoding: TEncoding): TArray<string>;
var
  Bytes: TBytes;
  Used: Integer;
begin
  Bytes := ReadBytes(Format);
  if Bytes = nil then Exit(nil);
  Used := Length(Bytes);
  if Encoding = TEncoding.Unicode then
    while (Used >= 2) and (Bytes[Pred(Used div 2)] = 0) do
      Dec(Used, 2)
  else
    while (Used >= 1) and (Bytes[Pred(Used)] = 0) do
      Dec(Used);
  SetLength(Result, 1);
  Result[0] := Encoding.GetString(Bytes, 0, Used);
end;

procedure TWinClipboardCore.DoSetString(const Format: TClipboardFormat;
  const Encoding: TEncoding; const Renderer: TFunc<string>);
begin
  WriteBytes(Format,
    function : TBytes
    begin
      Result := Encoding.GetBytes(Renderer() + #0);
    end);
end;

{ TWinClipboardCore.TClipboard.ICore }

function TWinClipboardCore.GetFormats: TArray<TClipboardFormat>;
var
  Count: Integer;
  Format: UINT;
begin
  if FCustomDataObjectToWrap <> nil then
    Result := GetFormatsInDataObject(FCustomDataObjectToWrap)
  else
  begin
    Count := 0;
    Format := 0;
    repeat
      Format := EnumClipboardFormats(Format);
      if Format = 0 then Break;
      if Length(Result) = Count then SetLength(Result, Count + 8);
      Result[Count] := TClipboardFormat.Wrap(Format);
      Inc(Count);
    until False;
    SetLength(Result, Count);
  end;
end;

function TWinClipboardCore.HasFormat(Format: TClipboardFormat): Boolean;
var
  ClassType: TClass;
begin
  if WrapsSystemClipboard then
    Result := IsClipboardFormatAvailable(Format.Handle)
  else
    Result := HasFormatInDataObject(FCustomDataObjectToWrap, Format);
  if not Result and FLegacyFormatToClassMap.TryGetValue(Format, ClassType) then
    Result := Clipboard.HasFormatFor(ClassType);
end;

function TWinClipboardCore.HasFormat(const Formats: array of TClipboardFormat;
  var Matched: TClipboardFormat): Boolean;
var
  Item: Integer;
begin
  if not WrapsSystemClipboard then
    Exit(HasFormatInDataObject(FCustomDataObjectToWrap, Formats, Matched));
  Item := GetPriorityClipboardFormat(Formats[0], Length(Formats));
  Result := (Item > 0);
  if Result then Matched := TClipboardFormat.Wrap(Item);
end;

procedure TWinClipboardCore.DoClear;
begin
  if FCustomDataObjectToWrap = nil then
    if FSysDataObjectToRead <> nil then
      OleCheck(OleSetClipboard(nil))
    else
      Win32Check(EmptyClipboard);
  FFilesToWrite.Clear;
  FVirtualFilesToWrite.Clear;
  if FCustomDataObjectToWrap = nil then
    FDataObjectBuilder := TDataObject.Create
  else
  begin
    FDataObjectBuilder := nil;
    if FCustomDataObjectToWrap.QueryInterface(IDataObjectBuilder, FDataObjectBuilder) <> S_OK then
      FDataObjectBuilder := TDataObjectBuilder.Create(FCustomDataObjectToWrap);
  end;
end;

function TWinClipboardCore.DoOpen: Boolean;
begin
  if FCustomDataObjectToWrap = nil then
    Result := OpenClipboard(0)
  else
    Result := True;
end;

procedure TWinClipboardCore.AssignFilesToWrite;
var
  Renderers: TArray<TFunc<TFileName>>;
begin
  Renderers := FFilesToWrite.ToArray;
  if Renderers = nil then Exit;
  FFilesToWrite.Clear; //not needed any more
  FDataObjectBuilder.AddFormat(cfHDROP, -1, TYMED_HGLOBAL,
    procedure (AllocResourceInMedium: Boolean; var Medium: TStgMedium)
    var
      Renderer: TFunc<TFileName>;
      Data: string;
      Ptr: PDropFiles;
      CharBytesLen: Integer;
    begin
      for Renderer in Renderers do
        Data := Data + Renderer() + #0;
      CharBytesLen := ByteLength(Data) + SizeOf(Char); //need closing null
      InitMediumHGlobal(SizeOf(TDropFiles) + CharBytesLen, AllocResourceInMedium, Medium);
      Ptr := GlobalLock(Medium.hGlobal);
      try
        Ptr.pFiles := SizeOf(TDropFiles);
        Ptr.fWide := True;
        Inc(Ptr);
        Move(Pointer(Data)^, Ptr^, CharBytesLen);
      finally
        GlobalUnlock(Medium.hGlobal);
      end;
    end);
end;

function TryGetDate(const Details: TVirtualFileDetails; Item: TVirtualFileAdditionalDetail;
  const ItemData: TDateTime; var Dest: TFileTime): Boolean;
var
  LocalFileTime: TFileTime;
  SysTime: TSystemTime;
begin
  Result := Item in Details.AdditionalData;
  if not Result then Exit;
  DateTimeToSystemTime(ItemData, SysTime);
  if vfDatesAreUTC in Details.AdditionalData then
    SystemTimeToFileTime(SysTime, Dest)
  else
  begin
    SystemTimeToFileTime(SysTime, LocalFileTime);
    LocalFileTimeToFileTime(LocalFileTime, Dest);
  end;
end;

procedure TWinClipboardCore.AssignVirtualFilesToWrite;
var
  I: Integer;
begin
  if FVirtualFilesToWrite.Count = 0 then Exit;
  FDataObjectBuilder.AddFormat(cfVirtualFileDescriptor, -1,
    function : TBytes
    var
      Info: TVirtualFileInfo;
      SeekPtr: PFileDescriptor;
    begin
      SetLength(Result, SizeOf(UINT) + SizeOf(TFileDescriptor) * FVirtualFilesToWrite.Count);
      ZeroMemory(Result, Length(Result));
      SeekPtr := @Result[0];
      PFileGroupDescriptor(SeekPtr).cItems := FVirtualFilesToWrite.Count;
      Inc(PUINT(SeekPtr));
      for Info in FVirtualFilesToWrite do
      begin
        if TryGetDate(Info.Details, vfCreationDate, Info.Details.CreationDate, SeekPtr.ftCreationTime) then
          SeekPtr.dwFlags := SeekPtr.dwFlags or FD_CREATETIME;
        if TryGetDate(Info.Details, vfLastAccessDate, Info.Details.LastAccessDate, SeekPtr.ftLastAccessTime) then
          SeekPtr.dwFlags := SeekPtr.dwFlags or FD_ACCESSTIME;
        if TryGetDate(Info.Details, vfLastWriteDate, Info.Details.LastWriteDate, SeekPtr.ftLastWriteTime) then
          SeekPtr.dwFlags := SeekPtr.dwFlags or FD_WRITESTIME;
        if vfAttr in Info.Details.AdditionalData then
        begin
          SeekPtr.dwFileAttributes := DWORD(Info.Details.Attr);
          SeekPtr.dwFlags := SeekPtr.dwFlags or FD_ATTRIBUTES;
        end;
        if vfSize in Info.Details.AdditionalData then
        begin
          SeekPtr.nFileSizeHigh := Int64Rec(Info.Details.Size).Hi;
          SeekPtr.nFileSizeLow := Int64Rec(Info.Details.Size).Lo;
          SeekPtr.dwFlags := SeekPtr.dwFlags or FD_FILESIZE;
        end;
        MoveChars(PChar(Info.Details.FileName)^, SeekPtr.cFileName,
          Min(MAX_PATH, Length(Info.Details.FileName)));
        Inc(SeekPtr);
      end;
    end);
  for I := 0 to FVirtualFilesToWrite.Count - 1 do
    if vfSize in FVirtualFilesToWrite[I].Details.AdditionalData then
      FDataObjectBuilder.AddFormat(cfFileContents, I, StreamRendererToBytesRenderer(FVirtualFilesToWrite[I].Renderer))
    else
      FDataObjectBuilder.AddFormat(cfFileContents, I, FVirtualFilesToWrite[I].Renderer);
end;

procedure TWinClipboardCore.DoClose;
var
  Intf: IClipboardDataObject;
begin
  if FCustomDataObjectToWrap = nil then CloseClipboard;
  if Clipboard.IsOpenForWriting then
  begin
    AssignFilesToWrite;
    AssignVirtualFilesToWrite;
    if (FCustomDataObjectToWrap = nil) and Supports(FDataObjectBuilder, IClipboardDataObject, Intf) then
      Intf.IsOleClipboard := True
    else
      FDataObjectBuilder := nil;
  end;
  FStgMediumsToRelease.Clear;
end;

function TWinClipboardCore.HasPlainText: Boolean;
var
  Matched: TClipboardFormat;
begin
  Result := HasFormat([cfUnicodeText, cfAnsiText, cfOemText, cfUTF8Text], Matched);
end;

function TWinClipboardCore.ReadPlainText: TArray<string>;
begin
  Result := DoGetStrings(cfUnicodeText, TEncoding.Unicode);
  if Result = nil then Result := DoGetStrings(cfAnsiText, TEncoding.ANSI);
  if Result = nil then Result := DoGetStrings(cfOemText, FOEMEncoding);
  if Result = nil then Result := DoGetStrings(cfUTF8Text, TEncoding.UTF8);
end;

procedure TWinClipboardCore.WritePlainText(const Renderer: TFunc<string>; PreferDelayed: Boolean);
var
  Cached: TFunc<string>;
begin
  if WrapsSystemClipboard then //system will create compatibility formats itself
    DoSetString(cfUnicodeText, TEncoding.Unicode, Renderer)
  else
  begin
    Cached := TCachedFunc<string>.Create(Renderer);
    DoSetString(cfUnicodeText, TEncoding.Unicode, Cached);
    DoSetString(cfAnsiText, TEncoding.ANSI, Cached);
    WriteBytes(cfLocale,
      function : TBytes
      var
        ID: LCID;
      begin
        ID := GetThreadLocale;
        SetLength(Result, SizeOf(ID));
        Move(ID, Result[0], SizeOf(ID));
      end);
    DoSetString(cfOemText, FOEMEncoding, Cached);
  end;
end;

{ TWinClipboardCore.IDelayedRendering }

function TWinClipboardCore.HasOutstandingPromisesToOS: Boolean;
var
  Intf: IClipboardDataObject;
begin
  Result := Supports(FDataObjectBuilder, IClipboardDataObject, Intf) and Intf.IsOleClipboard;
end;

procedure TWinClipboardCore.ResolveOutstandingPromisesToOS(IsExplicitlyRequested: Boolean);
begin
  //!!!virtual files don't like being flushed
  if IsExplicitlyRequested or (FVirtualFilesToWrite.Count = 0) then
    OleCheck(OleFlushClipboard)
end;

procedure TWinClipboardCore.CancelOutstandingPromisesToOS;
begin
  OleSetClipboard(nil);
end;

{ TWinClipboardCore.IWinClipboardCore }

function FormatToTymed(const Format: TClipboardFormat): LongInt;
begin
  case Format.Handle of
    CF_BITMAP: Result := TYMED_GDI;
    CF_METAFILEPICT: Result := TYMED_MFPICT;
    CF_ENHMETAFILE: Result := TYMED_ENHMF;
  else
    Result := TYMED_HGLOBAL;
  end;
end;

function TWinClipboardCore.DoGetAsHandle(Format: TClipboardFormat): THandle;
var
  Etc: TFormatEtc;
  Medium: TStgMedium;
  Obj: IDataObject;
begin
  if FCustomDataObjectToWrap = nil then
    Exit(GetClipboardData(Format.Handle));
  Etc.cfFormat := Format.Handle;
  Etc.ptd := nil;
  Etc.dwAspect := DVASPECT_CONTENT;
  Etc.lindex := -1;
  Etc.tymed := FormatToTymed(Format);
  ZeroMemory(@Medium, SizeOf(Medium));
  Obj := GetDataObjectToRead;
  if Obj.GetData(Etc, Medium) <> S_OK then
    Exit(0);
  FStgMediumsToRelease.Add(Medium);
  Result := Medium.hGlobal;
end;

procedure TWinClipboardCore.DoSetAsHandle(Format: TClipboardFormat; Value: THandle);
var
  Medium: TStgMedium;
begin
  Medium.tymed := FormatToTymed(Format);
  Medium.hGlobal := Value;
  Medium.unkForRelease := nil;
  FDataObjectBuilder.AddFormat(Format, -1, Medium);
end;

procedure TWinClipboardCore.DoSetAsHandleDelayed(Format: TClipboardFormat; ValueGetter: TFunc<THandle>);
var
  Tymed: LongInt;
begin
  Tymed := FormatToTymed(Format);
  FDataObjectBuilder.AddFormat(Format, -1, FormatToTymed(Format),
    procedure (AllocResourceInMedium: Boolean; var Medium: TStgMedium)
    var
      DataToCopy: HGLOBAL;
    begin
      if not AllocResourceInMedium then
      begin
        if Tymed <> TYMED_HGLOBAL then OleError(E_FAIL);
        DataToCopy := ValueGetter;
        try
          InitAndAssignMediumHGlobal(DataToCopy, True, Medium);
        finally
          GlobalFree(DataToCopy);
        end;
        Exit;
      end;
      Medium.tymed := Tymed;
      Medium.hGlobal := ValueGetter;
      Medium.unkForRelease := nil;
    end);
end;

procedure TWinClipboardCore.EnumDataObject(const Callback: TEnumDataObjectCallback);
var
  LookForMore: Boolean;
  Enumerator: IEnumFORMATETC;
  FormatEtc: TFormatEtc;
  Obj: IDataObject;
begin
  Obj := GetDataObjectToRead;
  OleCheck(Obj.EnumFormatEtc(DATADIR_GET, Enumerator));
  OleCheck(Enumerator.Reset);
  LookForMore := True;
  while Enumerator.Next(1, FormatEtc, nil) = S_OK do
  begin
    Callback(Obj, FormatEtc, LookForMore);
    if not LookForMore then Break;
  end;
end;

function TWinClipboardCore.GetChangeCount: TClipboardChangeCount;
begin
  if WrapsSystemClipboard then
    Result := TClipboardChangeCount(GetClipboardSequenceNumber)
  else if FDummyChangeCountBehaviour = dbFixed then
    Result := TClipboardChangeCount(-2) //-1 is already taken...
  else
    Result := inherited GetChangeCount;
end;

function TWinClipboardCore.SupportsChangeNotifications: Boolean;
begin
  Result := WrapsSystemClipboard and CheckWin32Version(6);
end;

class function TWinClipboardCore.ListenerWndProc(Wnd: HWND; Msg: UINT;
  wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  if Msg = WM_CLIPBOARDUPDATE then
  begin
    TWinClipboardCore(GetWindowLong(Wnd, GWL_USERDATA)).ChangeNotificationCallback();
    Result := 0;
    Exit;
  end;
  Result := DefWindowProc(Wnd, Msg, wParam, lParam);
end;

procedure TWinClipboardCore.EnableChangeNotifications(const Callback: TThreadMethod);
const
  WndClassName: PChar = 'TWinClipboardCoreListener';
var
  WndCls: TWndClassEx;
begin
  if not FRegisteredListenerWndClass then
  begin
    FRegisteredListenerWndClass := True;
    ZeroMemory(@WndCls, SizeOf(WndCls));
    WndCls.cbSize := SizeOf(WndCls);
    WndCls.hInstance := HInstance;
    WndCls.lpszClassName := WndClassName;
    WndCls.lpfnWndProc := @ListenerWndProc;
    RegisterClassEx(WndCls);
  end;
  FChangeListenerWnd := CreateWindowEx(0, WndClassName, '', 0, 0, 0, 0, 0,
    HWND_MESSAGE, 0, HInstance, nil);
  ChangeNotificationCallback := Callback;
  SetWindowLongPtr(FChangeListenerWnd, GWL_USERDATA, LONG_PTR(Self));
  AddClipboardFormatListener(FChangeListenerWnd);
end;

procedure TWinClipboardCore.DisableChangeNotifications;
begin
  RemoveClipboardFormatListener(FChangeListenerWnd);
  DestroyWindow(FChangeListenerWnd);
  FChangeListenerWnd := 0;
end;

function TWinClipboardCore.WrapsSystemClipboard: Boolean;
begin
  Result := (FCustomDataObjectToWrap = nil);
end;

{ TWinClipboardCore.TClipboard.IReadWriteBytes }

function TWinClipboardCore.ReadBytes(const Format: TClipboardFormat): TBytes;
begin
  if FCustomDataObjectToWrap <> nil then
    Result := GetBytesInDataObject(FCustomDataObjectToWrap, Format)
  else
    Result := CreateBytesFromHGLOBAL(GetClipboardData(Format.Handle));
end;

{ TWinClipboardCore.TClipboard.IReadWriteStream }

function TWinClipboardCore.ReadStream(const Format: TClipboardFormat; const Dest: TStream): Boolean;
var
  Adapter: IStream;
  Bytes: TBytes;
  FormatEtc: TFormatEtc;
  Medium: TStgMedium;
  Obj: IDataObject;
begin
  Result := True;
  //see if we can can get our destination stream written to directly first
  FormatEtc.cfFormat := Format.Handle;
  FormatEtc.ptd := nil;
  FormatEtc.dwAspect := DVASPECT_CONTENT;
  FormatEtc.lindex := -1;
  FormatEtc.tymed := TYMED_ISTREAM;
  ZeroMemory(@Medium, SizeOf(Medium));
  Medium.tymed := TYMED_ISTREAM;
  Adapter := TStreamAdapter.Create(Dest, soReference) as IStream;
  Medium.stm := Pointer(Adapter);
  Obj := GetDataObjectToRead;
  if (Obj <> nil) and (Obj.GetDataHere(FormatEtc, Medium) = S_OK) then Exit;
  //nope, so let's just try getting bytes instead
  Obj := nil;
  Bytes := ReadBytes(Format);
  if Bytes <> nil then
    Dest.WriteBuffer(Bytes[0], Length(Bytes))
  else
    Result := False;
end;

{ TWinClipboardCore.TClipboard.IReadWriteFileNames }

function TWinClipboardCore.DoReadFileNames(out FileNames: TArray<string>;
  ReturnCountOnly: Boolean = False): Integer;
const
  FormatEtc: TFormatEtc = (cfFormat: CF_HDROP; ptd: nil;
    dwAspect: DVASPECT_CONTENT; lindex: -1; tymed: TYMED_HGLOBAL);
var
  Buffer: array[0..MAX_PATH] of Char;
  I: Integer;
  Matched: TClipboardFormat;
  Medium: TStgMedium;
  Obj: IDataObject;
begin
  Result := 0;
  Obj := GetDataObjectToRead;
  if (Obj <> nil) and (Obj.GetData(FormatEtc, Medium) = S_OK) then
  try
    Result := DragQueryFile(Medium.hGlobal, $FFFFFFFF, nil, 0);
    if ReturnCountOnly then Exit;
    SetLength(FileNames, Result);
    for I := 0 to Result - 1 do
      SetString(FileNames[I], Buffer, DragQueryFile(Medium.hGlobal, I, Buffer, Length(Buffer)));
    Exit;
  finally
    ReleaseStgMedium(Medium);
  end;
  if ReturnCountOnly then
  begin
    if HasFormat([cfUnicodeFileName, cfAnsiFileName], Matched) then Result := 1;
    Exit;
  end;
  FileNames := DoGetStrings(cfUnicodeFileName, TEncoding.Unicode);
  if FileNames = nil then FileNames := DoGetStrings(cfAnsiFileName, TEncoding.ANSI);
  Result := Length(FileNames);
end;

function TWinClipboardCore.HasFile: Boolean;
var
  Matched: TClipboardFormat;
begin
  Result := HasFormat([cfHDROP, cfUnicodeFileName, cfAnsiFileName], Matched);
end;

function TWinClipboardCore.ReadFileNames: TArray<string>;
begin
  DoReadFileNames(Result);
end;

procedure TWinClipboardCore.WriteFileName(const Renderer: TFunc<TFileName>; PreferDelayed: Boolean);
var
  Cached: TFunc<TFileName>;
begin
  Cached := TCachedFunc<TFileName>.Create(Renderer);
  if FFilesToWrite.Count > 0 then
    FFilesToWrite.Add(Cached)
  else
  begin
    FFilesToWrite.Add(Cached);
    DoSetString(cfFileName, TEncoding.Unicode, TFunc<string>(Cached));
  end;
end;

{ TWinClipboardCore.TClipboard.IReadWriteURL }

function TWinClipboardCore.ReadURLs: TArray<string>;
begin
  Result := DoGetStrings(cfURL, TEncoding.Unicode);
end;

procedure TWinClipboardCore.WriteURL(const Renderer: TFunc<string>; PreferDelayed: Boolean);
begin
  DoSetString(cfURL, TEncoding.Unicode, Renderer);
end;

{ TWinClipboardCore.TClipboard.IReadWriteVirtualFile }

function MakeSaveToStreamProc(const DataObject: IDataObject; Index: Integer): TProc<TStream>;
begin
  Result :=
    procedure (Stream: TStream)
    var
      Ptr: Pointer;
      FormatEtc: TFormatEtc;
      Medium: TStgMedium;
      OleResult: HRESULT;
    begin
      FormatEtc.cfFormat := cfFileContents.Handle;
      FormatEtc.ptd := nil;
      FormatEtc.dwAspect := DVASPECT_CONTENT;
      FormatEtc.lindex := Index;
      FormatEtc.tymed := TYMED_ISTREAM;
      ZeroMemory(@Medium, SizeOf(Medium));
      Medium.tymed := TYMED_ISTREAM;
      IStream(Medium.stm) := TStreamAdapter.Create(Stream, soReference) as IStream;
      OleResult := DataObject.GetDataHere(FormatEtc, Medium);
      IStream(Medium.stm) := nil;
      if Succeeded(OleResult) then Exit;
      //if we couldn't output to our stream directly, try for a HGLOBAL
      case OleResult of
        DV_E_TYMED, DV_E_FORMATETC:
        begin
          Ptr := nil;
          FormatEtc.tymed := TYMED_HGLOBAL;
          ZeroMemory(@Medium, SizeOf(Medium));
          OleResult := DataObject.GetData(FormatEtc, Medium);
          //now this is getting a bit icky...
          if (OleResult = DV_E_FORMATETC) and (Index = 0) then
          begin
            FormatEtc.lindex := -1;
            ZeroMemory(@Medium, SizeOf(Medium));
            OleResult := DataObject.GetData(FormatEtc, Medium);
          end;
          OleCheck(OleResult);
          try
            Ptr := GlobalLock(Medium.hGlobal);
            Stream.WriteBuffer(Ptr^, GlobalSize(Medium.hGlobal));
          finally
            if Ptr <> nil then GlobalUnlock(Medium.hGlobal);
            ReleaseStgMedium(Medium);
          end;
        end;
      else
        OleError(OleResult);
      end;
    end;
end;

procedure TWinClipboardCore.DoReadVirtualFiles(DescriptorsOnly: Boolean;
  const Callback: TEnumVirtualFilesStreamCallback);
var
  Continue: Boolean;
  FormatEtc: TFormatEtc;
  Medium: TStgMedium;
  I: Integer;
  Obj: IDataObject;
  Ptr: PFileGroupDescriptor;
  SaveToStreamProc: TProc<TStream>;
begin
  Continue := True;
  FormatEtc.cfFormat := cfVirtualFileDescriptor.Handle;
  FormatEtc.ptd := nil;
  FormatEtc.dwAspect := DVASPECT_CONTENT;
  FormatEtc.lindex := -1;
  FormatEtc.tymed := TYMED_HGLOBAL;
  Obj := GetDataObjectToRead;
  Ptr := nil;
  if (Obj <> nil) and (Obj.GetData(FormatEtc, Medium) = S_OK) then
  try
    Ptr := GlobalLock(Medium.hGlobal);
    if Ptr <> nil then
      for I := 0 to Ptr.cItems - 1 do
      begin
        if not DescriptorsOnly then SaveToStreamProc := MakeSaveToStreamProc(Obj, I);
        Callback(PFileDescriptor(@Ptr.fgd)[I].cFileName, SaveToStreamProc, Continue);
        if not Continue then Exit;
      end;
  finally
    if Ptr <> nil then GlobalUnlock(Medium.hGlobal);
    ReleaseStgMedium(Medium);
  end;
end;

function TWinClipboardCore.ReadVirtualFileDescriptors: TArray<string>;
var
  Arr: TArray<string>;
  Count: Integer;
begin
  Count := 0;
  DoReadVirtualFiles(True,
    procedure (const Descriptor: string; const SaveToStream: TProc<TStream>;
      var LookForMore: Boolean)
    begin
      if Length(Arr) = Count then SetLength(Arr, Count + 8);
      Arr[Count] := Descriptor;
      Inc(Count);
    end);
  Result := Copy(Arr, 0, Count);
end;

procedure TWinClipboardCore.ReadVirtualFiles(const Callback: TEnumVirtualFilesStreamCallback);
begin
  DoReadVirtualFiles(False, Callback);
end;

procedure TWinClipboardCore.SaveVirtualFiles(const Callback: TSaveVirtualFilesCallback);
begin
  DoReadVirtualFiles(False,
    procedure (const Descriptor: string; const SaveToStream: TProc<TStream>;
      var LookForMore: Boolean)
    var
      SaveToFile: TSaveVirtualFileFunc;
    begin
      SaveToFile :=
        function (const DestDirectory: string): TFileName
        var
          BaseName, DestPath, Name, Ext: string;
          I: Integer;
          SeekPtr: PChar;
          Stream: TFileStream;
        begin
          { While the convention is a dummy file name, the descriptor technically
            could be anything }
          BaseName := Descriptor;
          if BaseName = '' then
            BaseName := 'File'
          else
          begin
            SeekPtr := PChar(BaseName);
            for I := Length(BaseName) downto 1 do
            begin
              case SeekPtr^ of
                #0, #1, #2, #3, #4, #5, #6, #7, #8, #9, #10, #11, #12,
                #13, #14, #15, #16, #17, #18, #19, #20, #21, #22, #23, #24,
                #25, #26, #27, #28, #29, #30, #31,
                '"', '*', '/', ':', '<', '>', '?', '\', '|': SeekPtr^ := ' ';
              end;
              Inc(SeekPtr);
            end;
          end;
          DestPath := IncludeTrailingPathDelimiter(DestDirectory);
          Result := DestPath + BaseName;
          if FileExists(Result) then
          begin
            Name := ChangeFileExt(BaseName, '');
            Ext := Copy(BaseName, Length(Name) + 1, MaxInt);
            I := 1;
            repeat
              Inc(I);
              Result := Format('%s%s (%d)%s', [DestPath, Name, I, Ext]);
            until not FileExists(Result);
          end;
          Stream := TFileStream.Create(Result, fmCreate);
          try
            SaveToStream(Stream);
          finally
            Stream.Free;
          end;
        end;
      Callback(Descriptor, SaveToFile, LookForMore);
    end);
end;

procedure TWinClipboardCore.WriteVirtualFile(const Details: TVirtualFileDetails;
  const Renderer: TProc<TStream>; PreferDelayed: Boolean);
var
  Info: TVirtualFileInfo;
begin
  Info := TVirtualFileInfo.Create;
  FVirtualFilesToWrite.Add(Info);
  Info.Details := Details;
  Info.Renderer := Renderer;
end;

procedure TWinClipboardCore.WriteBytes(const Format: TClipboardFormat;
  const Renderer: TFunc<TBytes>; PreferDelayed: Boolean);
begin
  FDataObjectBuilder.AddFormat(Format, -1, TCachedFunc<TBytes>.Create(Renderer));
end;

procedure TWinClipboardCore.WriteStream(const Format: TClipboardFormat;
  const Renderer: TProc<TStream>; PreferDelayed: Boolean);
begin
  FDataObjectBuilder.AddFormat(Format, -1, Renderer);
end;

{ TDataObjectDestroyNotifier }

constructor TDataObjectDestroyNotifier.Create(const ASource: IDataObject;
  const OnDestroy: TNotifyEvent);
begin
  inherited Create;
  FSource := ASource;
  FOnDestroy := OnDestroy;
end;

procedure TDataObjectDestroyNotifier.BeforeDestruction;
begin
  FSource := nil;
  if Assigned(FOnDestroy) then FOnDestroy(Self);
  inherited;
end;

function TDataObjectDestroyNotifier.DAdvise(const formatetc: TFormatEtc;
  advf: LongInt; const advSink: IAdviseSink; out dwConnection: LongInt): HRESULT;
begin
  Result := FSource.DAdvise(formatetc, advf, advSink, dwConnection);
end;

function TDataObjectDestroyNotifier.DUnadvise(dwConnection: LongInt): HRESULT;
begin
  Result := FSource.DUnadvise(dwConnection);
end;

function TDataObjectDestroyNotifier.EnumDAdvise(out enumAdvise: IEnumStatData): HRESULT;
begin
  Result := FSource.EnumDAdvise(enumAdvise)
end;

function TDataObjectDestroyNotifier.EnumFormatEtc(dwDirection: LongInt;
  out enumFormatEtc: IEnumFormatEtc): HRESULT;
begin
  Result := FSource.EnumFormatEtc(dwDirection, enumFormatEtc)
end;

function TDataObjectDestroyNotifier.GetCanonicalFormatEtc(
  const formatetc: TFormatEtc; out formatetcOut: TFormatEtc): HRESULT;
begin
  Result := FSource.GetCanonicalFormatEtc(formatetc, formatetcOut)
end;

function TDataObjectDestroyNotifier.GetData(const formatetcIn: TFormatEtc;
  out medium: TStgMedium): HRESULT;
begin
  Result := FSource.GetData(formatetcIn, medium)
end;

function TDataObjectDestroyNotifier.GetDataHere(const formatetc: TFormatEtc;
  out medium: TStgMedium): HRESULT;
begin
  Result := FSource.GetDataHere(formatetc, medium);
end;

function TDataObjectDestroyNotifier.QueryGetData(const formatetc: TFormatEtc): HRESULT;
begin
  Result := FSource.QueryGetData(formatetc);
end;

function TDataObjectDestroyNotifier.SetData(const formatetc: TFormatEtc;
  var medium: TStgMedium; fRelease: BOOL): HRESULT;
begin
  Result := FSource.SetData(formatetc, medium, fRelease);
end;

{ TOleDragHelper }

constructor TOleDragHelper.Create;
var
  DataObject: IDataObject;
begin
  DataObject := TDataObject.Create;
  inherited Create(DataObject, nil);
  FAutoOffset := True;
  GetCursorPos(FMousePos);
  FClipboard := TWinClipboardCore.CreateClipboardForDataObject(DataObject, dbIncrementing);
end;

destructor TOleDragHelper.Destroy;
begin
  SetBitmap(0);
  FClipboard.Free;
  inherited;
end;

function TOleDragHelper.GetClipboard: TClipboard;
begin
  Result := FClipboard;
end;

function TOleDragHelper.GetMousePos: TPoint;
begin
  Result := FMousePos;
end;

function TOleDragHelper.QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: LongInt): HRESULT;
var
  ContinueDrop: Boolean;
begin
  if fEscapePressed then
    Result := DRAGDROP_S_CANCEL
  else if (grfKeyState and (MK_LBUTTON or MK_RBUTTON) = 0) then
  begin
    ContinueDrop := True;
    if ContinueDrop then
      Result := DRAGDROP_S_DROP
    else
      Result := DRAGDROP_S_CANCEL;
  end
  else
    Result := S_OK;
end;

function TOleDragHelper.GiveFeedback(dwEffect: Integer): HResult;
begin
  Result := DRAGDROP_S_USEDEFAULTCURSORS;
end;

function TOleDragHelper.Execute: Boolean;
var
  BitmapInfo: tagBITMAP;
  DragSourceHelper: IDragSourceHelper;
  DragRec: TShDragImage;
  OleResult: HRESULT;
begin
  Result := False;
  OleCheck(CoCreateInstance(CLSID_DragDropHelper, nil, CLSCTX_INPROC_SERVER,
    IDragSourceHelper, DragSourceHelper));
  GetObject(FBitmap, SizeOf(BitmapInfo), @BitmapInfo);
  if FAutoOffset then
    FOffset := Point((BitmapInfo.bmWidth div 2), BitmapInfo.bmHeight div 2);
  FillChar(DragRec, SizeOf(DragRec), 0);
  DragRec.sizeDragImage.cx := BitmapInfo.bmWidth;
  DragRec.sizeDragImage.cy := BitmapInfo.bmHeight;
  DragRec.ptOffset.X := FOffset.X;
  DragRec.ptOffset.Y := FOffset.Y;
  DragRec.hbmpDragImage := FBitmap;
  DragRec.crColorKey := FTransparentColor;
  OleCheck(DragSourceHelper.InitializeFromBitmap(@DragRec, Self));
  FBitmap := 0; //DragSourceHelper has ownership
  OleResult := DoDragDrop(Self, Self,
    DROPEFFECT_LINK or DROPEFFECT_COPY or DROPEFFECT_MOVE, FEffect);
  case OleResult of
    DRAGDROP_S_DROP: Result := True;
    DRAGDROP_S_CANCEL: Result := False;
  else OleError(OleResult);
  end;
end;

procedure TOleDragHelper.SetBitmap(const Value: HBITMAP; TransparentColor: TColorRef = 0);
begin
  FTransparentColor := TransparentColor;
  if Value = FBitmap then Exit;
  if FBitmap <> 0 then DeleteObject(FBitmap);
  FBitmap := Value;
end;

procedure TOleDragHelper.SetOffset(const Value: TPoint);
begin
  FAutoOffset := False;
  FOffset := Value;
end;

initialization
  TWinClipboardCore.cfFileContents := TWinClipboardCore.RegisterFormat(CFSTR_FILECONTENTS);
  TWinClipboardCore.cfAnsiFileName := TWinClipboardCore.RegisterFormat(CFSTR_FILENAMEA);
  TWinClipboardCore.cfUnicodeFileName := TWinClipboardCore.RegisterFormat(CFSTR_FILENAMEW);
  TWinClipboardCore.Register;
{$ENDIF}
end.
