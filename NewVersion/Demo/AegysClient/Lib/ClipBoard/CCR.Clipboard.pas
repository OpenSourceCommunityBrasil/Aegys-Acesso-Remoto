{**************************************************************************************}
{                                                                                      }
{ CCR.Clipboard                                                                        }
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

unit CCR.Clipboard;
{
  Abstracts from the Windows clipboard, OLE drag and drop, and Cocoa pasteboard APIs to
  provide an interface very close to the stock VCL TClipboard, only with many added
  features; supports both FMX and the VCL.

  Currently Windows and OS X for XE2+ and iOS for XE7+ are supported; for unknown
  future target platforms, a default text-only implementation is provided that delegates
  to the text-only IFMXClipboardService.
}
interface

{$IFDEF NEXTGEN}
{$LEGACYIFEND ON}
{$ENDIF}

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Generics.Defaults,
  System.TypInfo, System.Rtti {$IF RTLVersion >= 29}, System.Hash{$IFEND};

type
  EClipboardException = class(Exception);
  EUnsupportedClipboardFeature = class(EClipboardException);

  TClipboard = class;

  { On Windows a TClipboardFormat holds a CF_XXX value, on OS X and iOS a CFStringRef.
    In general it's best to consider TClipboardFormat an opaque type however. If the
    underlying native representation is really required, CCR.Clipboard.Apple and
    CCR.Clipboard.Win both extend TClipboardFormat with a Handle instance property and
    Wrap class method appropriate for their respective platforms. }
  TClipboardFormat = record
  strict private
    OrdValue: NativeUInt;
    function GetName: string;
    function GetDescription: string;
  public
    class operator Equal(const Format1, Format2: TClipboardFormat): Boolean;
    class operator NotEqual(const Format1, Format2: TClipboardFormat): Boolean; inline;
    function ConformsTo(const BaseFormat: TClipboardFormat): Boolean;
    function ToHexString(MinDigits: Integer = 4): string; inline;
    property Description: string read GetDescription;
    property Name: string read GetName;
  end;

  TObjectHandlerList<T: IInterface> = class
  strict private type
    TItem = class
      Detail: TPair<TClass, T>;
      GenCount: Integer;
    end;
  strict private
    FItems: TObjectList<TItem>;
    function FindItem(const ClassFor: TClass; var Matched: TItem; Exact: Boolean): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddOrReplace(const ClassFor: TClass; const Handler: T);
    function Find(const ClassFor: TClass; var Handler: T): Boolean;
  end;

  TEnumFormatSetsCallback = reference to procedure (const Formats: TArray<TClipboardFormat>;
    const GetFormatBytes: TFunc<TClipboardFormat, TBytes>; var LookForMore: Boolean);
  TGetBytesCallback = reference to procedure (const Bytes: TBytes; var LookForMore: Boolean);
  TGetInstancesCallback<T> = reference to procedure (const Obj: T; var LookForMore: Boolean);
  TLoadObjectsCallback<T: class> = reference to procedure (
    const AssignTo: TProc<T>; var LookForMore: Boolean);

  TEnumVirtualFilesStreamCallback = reference to procedure (const Descriptor: string;
    const SaveToStream: TProc<TStream>; var LookForMore: Boolean);
  TEnumVirtualFileBytesCallback = reference to procedure (const Descriptor: string;
    const GetBytes: TFunc<TBytes>; var LookForMore: Boolean);
  TSaveVirtualFileFunc = reference to function (const DestDirectory: string): TFileName;
  TSaveVirtualFilesCallback = reference to procedure (const Descriptor: string;
    const SaveToFile: TSaveVirtualFileFunc; var LookForMore: Boolean);

  TVirtualTextFileOption = (tfNeverOutputBOM);
  TVirtualTextFileOptions = set of TVirtualTextFileOption;

  TAssignClipboardForDragEvent = reference to procedure (DraggedObject: TObject; Clipboard: TClipboard);

  IClipboardDropInfo = interface
  ['{9734706E-17CD-4624-91F0-0F9FAF720775}']
    function GetClipboard: TClipboard;
    property Clipboard: TClipboard read GetClipboard;
  end;

  ICustomClipper = interface
  ['{399C374B-854A-47D6-AA89-64642E8FC078}']
    function CanLoadFromClipboard(const Clipboard: TClipboard): Boolean;
  end;

  ICustomClipper<T> = interface(ICustomClipper)
    procedure SaveToClipboard(const Clipboard: TClipboard; PreferDelayed: Boolean;
      const Getter: TFunc<T>);
  end;

  IRecordClipper<T: record> = interface(ICustomClipper<T>)
    procedure LoadFromClipboard(const Clipboard: TClipboard;
      const Callback: TGetInstancesCallback<T>);
  end;

  IObjectClipper<T: class> = interface(ICustomClipper<T>)
  ['{A1F0A310-77BD-4DD7-A1A5-AF4BED2CA220}']
    procedure LoadFromClipboard(const Clipboard: TClipboard;
      const Callback: TLoadObjectsCallback<T>);
  end;

  TLoadFromClipboardFormatEvent<T: class> = reference to procedure (const Clipboard: TClipboard;
    const Format: TClipboardFormat; Obj: T);
  TSaveToClipboardFormatEvent<T: class> = reference to procedure (const Clipboard: TClipboard;
    const Format: TClipboardFormat; PreferDelayed: Boolean; const ObjGetter: TFunc<T>);
  TLoadFromClipboardStreamEvent<T: class> = reference to procedure (Obj: T; Stream: TStream);
  TSaveToClipboardStreamEvent<T: class> = reference to procedure (Obj: T;
    const Format: TClipboardFormat; Stream: TStream);

  TObjectClipper<T: class> = class abstract(TInterfacedObject, ICustomClipper, IObjectClipper<T>)
  strict private
    FReadFormats, FWrittenFormats: TArray<TClipboardFormat>;
  strict protected
    function CreateBlankObject: T; virtual; abstract;
    function CanLoadFromClipboardFile(const Clipboard: TClipboard): Boolean; virtual;
    procedure LoadFromClipboardFormat(const Clipboard: TClipboard;
      const Format: TClipboardFormat; const Callback: TLoadObjectsCallback<T>); virtual; abstract;
    procedure LoadFromClipboardFiles(const Clipboard: TClipboard;
      const Callback: TLoadObjectsCallback<T>); virtual;
    procedure SaveToClipboardFormat(Clipboard: TClipboard; const Format: TClipboardFormat;
      PreferDelayed: Boolean; const ObjGetter: TFunc<T>); virtual; abstract;
    { IObjectClipper<T> }
    function CanLoadFromClipboard(const Clipboard: TClipboard): Boolean;
    procedure LoadFromClipboard(const Clipboard: TClipboard; const Callback: TLoadObjectsCallback<T>); overload;
    procedure SaveToClipboard(const Clipboard: TClipboard; PreferDelayed: Boolean;
      const ObjGetter: TFunc<T>); overload; virtual;
  protected
    property ReadFormats: TArray<TClipboardFormat> read FReadFormats;
    property WrittenFormats: TArray<TClipboardFormat> read FWrittenFormats;
  public
    constructor Create(const ReadFormats, WrittenFormats: array of TClipboardFormat);
  end;

  TClipping = class(TComponent)
  strict private class var
    FFormatSettings: TFormatSettings;
    class constructor InitializeClass;
  strict private
    FClippedClass: string;
    FDateTime: string;
    FOriginator: string;
    FObj: TPersistent;
    FSetSubComponent: Boolean;
    FRTLVersion: Single;
    procedure SetObj(const Value: TPersistent);
  public
    constructor Create(const ObjToWrap: TPersistent); reintroduce;
    destructor Destroy; override;
  published
    //provide some metadata
    property TimeStamp: string read FDateTime write FDateTime;
    property Originator: string read FOriginator write FOriginator;
    property RTLVersion: Single read FRTLVersion write FRTLVersion;
    property ClippedClass: string read FClippedClass write FClippedClass;
    //provide the actual clipped object
    property Clipped: TPersistent read FObj write SetObj;
  end;

  ISimpleObjectClipper<T: TPersistent> = interface(IObjectClipper<T>)
    function GetOnBeginRead: TProc<TReader>;
    function GetOnEndRead: TProc<TClipping>;
    procedure SetOnBeginRead(const NewHandler: TProc<TReader>);
    procedure SetOnEndRead(const NewHandler: TProc<TClipping>);
    function GetFormat: TClipboardFormat;

    property Format: TClipboardFormat read GetFormat;
    property OnBeginRead: TProc<TReader> read GetOnBeginRead write SetOnBeginRead;
    property OnEndRead: TProc<TClipping> read GetOnEndRead write SetOnEndRead;
  end;

  TPersistentClipper<T: TPersistent, constructor> = class(TObjectClipper<T>, ISimpleObjectClipper<T>)
  strict private
    FOnBeginRead: TProc<TReader>;
    FOnEndRead: TProc<TClipping>;
  strict protected
    function CreateBlankObject: T; override;
    procedure LoadFromClipboardFormat(const Clipboard: TClipboard;
      const Format: TClipboardFormat; const Callback: TLoadObjectsCallback<T>); override;
    procedure SaveToClipboardFormat(Clipboard: TClipboard; const Format: TClipboardFormat;
      PreferDelayed: Boolean; const ObjGetter: TFunc<T>); override;
    { ISimpleObjectClipper<T> }
    function GetOnBeginRead: TProc<TReader>;
    function GetOnEndRead: TProc<TClipping>;
    procedure SetOnBeginRead(const NewHandler: TProc<TReader>);
    procedure SetOnEndRead(const NewHandler: TProc<TClipping>);
    function GetFormat: TClipboardFormat;
  protected
    procedure BeginRead(Reader: TReader); virtual;
    procedure EndRead(Clipping: TClipping); virtual;
  public
    constructor Create(const Format: TClipboardFormat);
  end;

  TStreamableObjectClipper<T: class> = class abstract(TObjectClipper<T>)
  strict private
    FFileExts: TArray<string>;
  protected
    class function ObjAsIStreamPersist(const Obj: TObject): IStreamPersist; static;
    function CanLoadFromClipboardFile(const Clipboard: TClipboard): Boolean; override;
    function IsCompatibleVirtualFile(const Descriptor: string): Boolean; inline;
    procedure LoadFromClipboardFormat(const Clipboard: TClipboard;
      const Format: TClipboardFormat; const Callback: TLoadObjectsCallback<T>); override;
    procedure LoadFromStream(Obj: T; const Stream: TStream); virtual;
    procedure LoadFromClipboardFiles(const Clipboard: TClipboard;
      const Callback: TLoadObjectsCallback<T>); override;
    procedure SaveToStream(const Obj: T; const Format: TClipboardFormat; Stream: TStream); virtual;
    procedure SaveToClipboardFormat(Clipboard: TClipboard;
      const Format: TClipboardFormat; PreferDelayed: Boolean; const ObjGetter: TFunc<T>); override;
    property FileExts: TArray<string> read FFileExts;
  public
    constructor Create(const Formats: array of TClipboardFormat; const FileExts: array of string); overload;
    constructor Create(const ReadFormats, WrittenFormats: array of TClipboardFormat;
      const FileExts: array of string); overload;
  end;

  TDelegatedObjectClipper<T: class, constructor> = class(TStreamableObjectClipper<T>)
  strict private
    FOnLoadFromClipboardFormat: TLoadFromClipboardFormatEvent<T>;
    FOnSaveToClipboardFormat: TSaveToClipboardFormatEvent<T>;
    FOnLoadFromStream: TLoadFromClipboardStreamEvent<T>;
    FOnSaveToStream: TSaveToClipboardStreamEvent<T>;
  strict protected
    function CreateBlankObject: T; override;
    procedure LoadFromClipboardFormat(const Clipboard: TClipboard;
      const Format: TClipboardFormat; const Callback: TLoadObjectsCallback<T>); override;
    procedure LoadFromStream(ObjToLoad: T; const Stream: TStream); override;
    procedure SaveToStream(const ObjToSave: T; const Format: TClipboardFormat; Stream: TStream); override;
    procedure SaveToClipboardFormat(Clipboard: TClipboard;
      const Format: TClipboardFormat; PreferDelayed: Boolean; const ObjGetter: TFunc<T>); override;
  public
    constructor Create(const ReadFormats, WrittenFormats: array of TClipboardFormat;
      const FileExts: array of string;
      const OnLoadFromClipboardFormat: TLoadFromClipboardFormatEvent<T>;
      const OnSaveToClipboardFormat: TSaveToClipboardFormatEvent<T>;
      const OnLoadFromStream: TLoadFromClipboardStreamEvent<T>;
      const OnSaveToStream: TSaveToClipboardStreamEvent<T>);
  end;

  TVirtualFileAdditionalDetail = (vfAttr, vfSize, vfCreationDate, vfLastAccessDate,
    vfLastWriteDate, vfDatesAreUTC);
  TVirtualFileAdditionalDetails = set of TVirtualFileAdditionalDetail;

  TVirtualFileDetails = record
  strict private
    FAttr: Integer;
    FCreationDate, FLastAccessDate, FLastWriteDate: TDateTime;
    FSize: Int64;
    procedure SetAttr(const Value: Integer);
    procedure SetCreationDate(const Value: TDateTime);
    procedure SetLastAccessDate(const Value: TDateTime);
    procedure SetLastWriteDate(const Value: TDateTime);
    procedure SetSize(const Value: Int64);
  public
    FileName: string;
    AdditionalData: TVirtualFileAdditionalDetails;
    constructor Create(const AFileName: string; const AKnownSize: Int64 = 0);
    property Attr: Integer read FAttr write SetAttr;
    property CreationDate: TDateTime read FCreationDate write SetCreationDate;
    property LastAccessDate: TDateTime read FLastAccessDate write SetLastAccessDate;
    property LastWriteDate: TDateTime read FLastWriteDate write SetLastWriteDate;
    property Size: Int64 read FSize write SetSize;
  end;

  TClipboardChangeCount = NativeUInt;

  IClipboardListener = interface
  ['{69E4BCA1-9CA9-4D5B-B536-DEF2FE1C53F6}']
    procedure ClipboardChanged(const Clipboard: TClipboard);
  end;

  TClipboard = class(TInterfacedPersistent)
  public type
    TEnumerator = record
    strict private
      FRegisteredFormats: TArray<TClipboardFormat>;
      FIndex: Integer;
      function GetCurrent: TClipboardFormat;
    public
      constructor Create(const AFormats: TArray<TClipboardFormat>);
      function MoveNext: Boolean;
      property Current: TClipboardFormat read GetCurrent;
    end;

    TStdFormatValues = class
      cfComponent, cfComponents, cfFileName, cfGIF, cfJPEG, cfPNG, cfRTF, cfUnicodeText,
      cfUTF8Text, cfTIFF, cfURL, cfVirtualFileDescriptor: TClipboardFormat;
    end;

    TCore = class(TInterfacedObject)
    protected
      class procedure Register;
      class function ConformingFormat(const FormatToTest, BaseFormat: TClipboardFormat): Boolean; virtual;
      class function SameFormat(const Format1, Format2: TClipboardFormat): Boolean; virtual;
      class procedure InitializeFormats(StdFormats: TStdFormatValues;
        var CustomFormatsSupported: Boolean); virtual; abstract;
      class function GetFormatName(const Format: TClipboardFormat): string; virtual; abstract;
      class function GetFormatDescription(const Format: TClipboardFormat): string; virtual;
      class function RegisterFormat(const Name: string): TClipboardFormat; virtual;
      class function IsCompatibleVirtualFileDescriptor(const Descriptor: string;
        const SupportedFileExts: TArray<string>): Boolean; virtual;
    strict private
      FDummyChangeCount: TClipboardChangeCount;
      FChangeNotifications: Boolean;
      FOwnerWeakRef: Pointer;
      function GetOwner: TClipboard; inline;
      procedure SetChangeNotifications(Enable: Boolean);
    strict protected //the HasXXX methods don't need to be virtual (are for interfaces)
      function HasFile: Boolean;
      function HasURL: Boolean;
      function HasVirtualFile: Boolean;
      procedure EnableChangeNotifications(const Callback: TThreadMethod); virtual;
      procedure DisableChangeNotifications; virtual;
    protected
      constructor Create(const AOwner: TClipboard); virtual;
      function WrapsSystemClipboard: Boolean; virtual;
      function SupportsChangeNotifications: Boolean; virtual;
      function GetChangeCount: TClipboardChangeCount; virtual;
      function GetFormats: TArray<TClipboardFormat>; virtual; abstract;
      function HasFormat(Format: TClipboardFormat): Boolean; overload; virtual; abstract;
      function HasFormat(const Formats: array of TClipboardFormat; var Matched: TClipboardFormat): Boolean; overload; virtual; abstract;
      function HasPlainText: Boolean; virtual;
      function DoOpen: Boolean; virtual; abstract;
      procedure DoClose; virtual; abstract;
      procedure DoClear; virtual; abstract;
      function ReadPlainText: TArray<string>; virtual; abstract;
      procedure WritePlainText(const Renderer: TFunc<string>; PreferDelayed: Boolean); virtual; abstract;
    public
      destructor Destroy; override;
      property Clipboard: TClipboard read GetOwner;
      property ChangeNotifications: Boolean read FChangeNotifications write SetChangeNotifications;
    end;

    TCoreClass = class of TCore;

    IReadWriteBytes = interface
    ['{D2DC7C1D-C701-4D6C-8002-B549349BEFA3}']
      function ReadBytes(const Format: TClipboardFormat): TBytes;
      procedure WriteBytes(const Format: TClipboardFormat;
        const Renderer: TFunc<TBytes>; PreferDelayed: Boolean);
    end;

    IMultipleFormatSets = interface
    ['{1096EEF3-E605-4629-8AD7-941AEA9B07DD}']
      procedure NewFormatSet;
      procedure EnumBytes(const Format: TClipboardFormat; const Callback: TGetBytesCallback);
      procedure EnumFormatSets(const Callback: TEnumFormatSetsCallback);
    end;

    IReadWriteStream = interface
    ['{C6E56A63-CB1D-480B-8A54-C75FC688C8C1}']
      function ReadStream(const Format: TClipboardFormat; const Stream: TStream): Boolean;
      procedure WriteStream(const Format: TClipboardFormat;
        const Renderer: TProc<TStream>; PreferDelayed: Boolean);
    end;

    IReadWriteFileNames = interface
    ['{C9221809-4AFF-4796-985A-3654B7F2C44F}']
      function HasFile: Boolean;
      function ReadFileNames: TArray<string>;
      procedure WriteFileName(const Renderer: TFunc<TFileName>; PreferDelayed: Boolean);
    end;

    IReadWriteURL = interface
    ['{EA8FDD4F-D91A-4D2C-A59D-D9BB9ECB9F39}']
      function HasURL: Boolean;
      function ReadURLs: TArray<string>;
      procedure WriteURL(const Renderer: TFunc<string>; PreferDelayed: Boolean);
    end;

    IReadWriteVirtualFile = interface
    ['{2D4B25F6-8502-4BD5-9068-7BAA2DA2E0B5}']
      function HasVirtualFile: Boolean;
      function ReadVirtualFileDescriptors: TArray<string>;
      procedure ReadVirtualFiles(const Callback: TEnumVirtualFilesStreamCallback);
      procedure SaveVirtualFiles(const Callback: TSaveVirtualFilesCallback);
      procedure WriteVirtualFile(const Details: TVirtualFileDetails;
        const Renderer: TProc<TStream>; PreferDelayed: Boolean);
    end;

    IDelayedRendering = interface
    ['{61A843F1-E035-4B8C-9A5D-839B269AC248}']
      function HasOutstandingPromisesToOS: Boolean;
      procedure ResolveOutstandingPromisesToOS(IsExplicitlyRequested: Boolean);
      procedure CancelOutstandingPromisesToOS;
    end;
  strict private type
    TFindClipper = function (TypeHandle: Pointer; var Clipper: ICustomClipper): Boolean;
  strict private const
    RTFStringSig = '{\rtf';
  strict private class var
    RTFBytesSig: TBytes;
    FObjectClippers: TObjectHandlerList<ICustomClipper>;
    FRecordClippers: TDictionary<PTypeInfo, ICustomClipper>;
    FCoreClass: TCoreClass;
    FCreateReaderDef: TFunc<TStream, TReader>;
    FFormatDescriptions: TDictionary<TClipboardFormat, string>;
    FObjectBeingDragged: TObject;
    FSingleton: TClipboard;
    FStdFormats: TStdFormatValues;
    FSupportsCustomFormats: Boolean;
    FOnGetDragData: TDictionary<Pointer, TAssignClipboardForDragEvent>;
    FOnStartDrag: TAssignClipboardForDragEvent;
    class constructor InitializeClass;
    class destructor FinalizeClass;
    class procedure NeedClassInitialized; static; //workaround for class constructor getting called out of order on OS X!
    class procedure CheckInterfaceAssigned<T: IInterface>(const Service: T); static;
    class function AllocFormat(ClassType: TClass): TClipboardFormat; static;
    class procedure NoClipperError(const TypeName: string); static;
    class function TypeInfo<T>: PTypeInfo; static;
    class function GetStdFormats: TStdFormatValues; static; inline;
    class procedure SetCoreClass(const CoreClass: TCoreClass);
    class function GetOnGetDragData(const DraggedObject: TObject): TAssignClipboardForDragEvent; static;
    class procedure SetOnGetDragData(const DraggedObject: TObject; const Handler: TAssignClipboardForDragEvent); static;
  private
    class function CreateDynArray<T>(const Items: array of T): TArray<T>; static;
    class function GetFormatDescription(const Format: TClipboardFormat): string;
    class property CreateReaderDef: TFunc<TStream, TReader> read FCreateReaderDef;
  strict private
    FAssignedFormatCount: Integer;
    FUserAssignDelayedCount: Integer;
    FChangeListeners: TList<Pointer>; //weak refs
    FComponentStreams: TObjectDictionary<TClipboardFormat, TBytesStream>;
    FCore: TCore;
    FCoreIntf: IInterface;
    FDelayedRenderingIntf: IDelayedRendering;
    FFileNamesCache: TArray<string>;
    FFileNamesCacheChangeCount: TClipboardChangeCount;
    FFormatsCache: TArray<TClipboardFormat>;
    FFormatsCacheChangeCount: TClipboardChangeCount;
    FFreeNotifyProxy: TComponent;
    FIsOpenForWriting: Boolean;
    FHasFormatForCache: array of TPair<Pointer, Boolean>;
    FHasFormatForCacheCount: Integer;
    FHasFormatForCacheChangeCount: TClipboardChangeCount;
    FMultipleFormatSetsIntf: IMultipleFormatSets;
    FReadWriteBytesIntf: IReadWriteBytes;
    FReadWriteStreamIntf: IReadWriteStream;
    FReadWriteFileNamesIntf: IReadWriteFileNames;
    FReadWriteURLIntf: IReadWriteURL;
    FReadWriteVirtualFileIntf: IReadWriteVirtualFile;
    FOpenCount: Integer;
    FURLsCache: TArray<string>;
    FURLsCacheChangeCount: TClipboardChangeCount;
    FVirtualFileDescriptorsCache: TArray<string>;
    FVirtualFileDescriptorsCacheChangeCount: TClipboardChangeCount;
    procedure AddFreeNotification(const Obj: TObject);
    procedure RemoveFreeNotification(const Obj: TObject);
    function HasFormatFor(TypeHandle: Pointer; const FindClipper: TFindClipper): Boolean; overload;
    class function FindClipperForClass(TypeHandle: Pointer; var Clipper: ICustomClipper): Boolean; static;
    class function FindClipperForRecord(TypeHandle: Pointer; var Clipper: ICustomClipper): Boolean; static;
    function GetChangeCount: TClipboardChangeCount;
    function GetEmpty: Boolean;
    function GetAsRTF: string;
    function GetAsURL: string;
    procedure DoAssignBytes(UserAssignDelayed: Boolean; const Format: TClipboardFormat;
      const Renderer: TFunc<TBytes>);
    procedure DoAssignText(UserAssignDelayed: Boolean; const Renderer: TFunc<string>);
    procedure DoAssignURL(UserAssignDelayed: Boolean; const Renderer: TFunc<string>);
    procedure DoAssignVirtualFile(UserAssignDelayed: Boolean;
      const Details: TVirtualFileDetails; const Renderer: TProc<TStream>); overload;
    procedure DoAssignVirtualFile<T: class, IStreamPersist>(UserDelayed: Boolean;
      const Details: TVirtualFileDetails; const ObjGetter: TFunc<T>); overload;
    procedure DoAssignVirtualTextFile(UserDelayed: Boolean; 
      Details: TVirtualFileDetails; const Renderer: TFunc<string>;
      Encoding: TEncoding; Options: TVirtualTextFileOptions);
    procedure FlushComponents;
    procedure ResolveOutstandingPromisesToOS(ExplicitlyRequested: Boolean); overload;
  strict protected
    class function CreateCombinedGetDragDataCallback(
      const OnGetDataOverride: TAssignClipboardForDragEvent = nil;
      const OnGetDataDefault: TProc = nil): TAssignClipboardForDragEvent; static;
    class procedure SetObjectBeingDragged(const Obj: TObject); static;
  protected
    class procedure CheckIsRTF(const Str: string); overload; static;
    class procedure CheckIsRTF(const Bytes: TBytes); overload; static;
    class function GetSuitableCoreClass(const MinClass: TCoreClass): TCoreClass; static;
    constructor Create(const ACore: TCore = nil);
    procedure AssignTo(Dest: TPersistent); override;
    procedure NotifyChangeListeners;
    procedure OpenForReading;
    procedure OpenForWriting;
    function TryOpenForReading(RaiseExceptionIfOpenForWriting: Boolean = True): Boolean;
    property Core: TCore read FCore;
    class function GetSingleton: TClipboard;
    class procedure NeedCoreClassSet;
    class property CoreClass: TCoreClass read FCoreClass;
    class property StdFormatValues: TStdFormatValues read GetStdFormats;
  {$IF RTLVersion < 24}
  public
  {$IFEND}
    procedure Add(const DoAssignCode: TProc);
  public
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    function GetEnumerator: TEnumerator;
    function IsSystemClipboard: Boolean; //as opposed to the drag and drop pasteboard or some other data object
    procedure Clear;
    function TryOpen: Boolean;
    procedure Open;
    procedure Close;
    function IsOpenForWriting: Boolean;
    procedure NewFormatSet;
    { Change listening }
    function SupportsChangeListeners: Boolean;
    procedure RegisterChangeListener(const Listener: IClipboardListener);
    procedure UnregisterChangeListener(const Listener: IClipboardListener);
    { Assign }
    procedure Assign(Source: TPersistent); overload; override;
    procedure Assign(const Source: TObject); reintroduce; overload;
    procedure Assign<T: record>(const Source: T); reintroduce; overload;
    procedure Assign(const Format: TClipboardFormat; const Source: IStreamPersist); reintroduce; overload; inline;
    procedure Assign(const Format: TClipboardFormat; const Source: TStream); reintroduce; overload; inline;
    procedure Assign(const Format: TClipboardFormat; const Data: TBytes); reintroduce; overload;
    procedure Assign(const Format: TClipboardFormat; const Buffer; Size: Integer); reintroduce; overload;
    { AssignDelayed }
    procedure AssignDelayed(const Format: TClipboardFormat; const Renderer: TFunc<TBytes>); reintroduce; overload;
    procedure AssignDelayed(const Format: TClipboardFormat; const Renderer: TProc<TStream>); reintroduce; overload;
    procedure AssignDelayed<T>(const ObjGetter: TFunc<T>); reintroduce; overload;
    procedure AssignDelayed<T: class, constructor>(const Renderer: TProc<T>); reintroduce; overload;
    { AssignFile/AssignFileDelayed }
    procedure AssignFile(const FileName: TFileName); overload;
    procedure AssignFileDelayed(const Renderer: TFunc<TFileName>); overload;
    { AssignRTF/AssignRTFDelayed }
    procedure AssignRTF(const Value: string); overload;
    procedure AssignRTF(const RTFBytes: TBytes); overload;
    procedure AssignRTFDelayed(const Renderer: TFunc<string>); overload;
    procedure AssignRTFDelayed(const Renderer: TFunc<TBytes>); overload;
    procedure AssignRTFDelayed(const Renderer: TProc<TStream>); overload;
    { AssignText/AssignTextDelayed }
    procedure AssignText(const Value: string); overload;
    procedure AssignTextDelayed(const Renderer: TFunc<string>); overload;
    { AssignURL/AssignURLDelayed }
    procedure AssignURL(const Value: string); overload;
    procedure AssignURLDelayed(const Renderer: TFunc<string>); overload;
    { AssignVirtualFile }
    procedure AssignVirtualFile(const VirtualFileName: string; const Data: TBytes); overload; //inline;
    procedure AssignVirtualFile(Details: TVirtualFileDetails; const Data: TBytes); overload;
    procedure AssignVirtualFile(const VirtualFileName: string; const Source: IStreamPersist); overload; //inline;
    procedure AssignVirtualFile(const Details: TVirtualFileDetails; const Source: IStreamPersist); overload; inline;
    procedure AssignVirtualFile(const VirtualFileName, Text: string; Encoding: TEncoding = nil;
      Options: TVirtualTextFileOptions = []); overload;
    procedure AssignVirtualFile(const Details: TVirtualFileDetails; const Text: string;
      Encoding: TEncoding = nil; Options: TVirtualTextFileOptions = []); overload;
    { AssignVirtualFileDelayed }
    procedure AssignVirtualFileDelayed(const VirtualFileName: string; const Renderer: TProc<TStream>); overload; inline;
    procedure AssignVirtualFileDelayed(const Details: TVirtualFileDetails; const Renderer: TProc<TStream>); overload; inline;
    procedure AssignVirtualFileDelayed(const VirtualFileName: string; const Renderer: TFunc<TBytes>); overload; inline;
    procedure AssignVirtualFileDelayed(const Details: TVirtualFileDetails;
      const Renderer: TFunc<TBytes>); overload; inline;
    procedure AssignVirtualFileDelayed<T: class, IStreamPersist>(
      const VirtualFileName: string; const ObjGetter: TFunc<T>); overload; inline;
    procedure AssignVirtualFileDelayed<T: class, IStreamPersist>(
      const Details: TVirtualFileDetails; const ObjGetter: TFunc<T>); overload; inline;
    procedure AssignVirtualFileDelayed<T: class, constructor, IStreamPersist>(
      const VirtualFileName: string; const Renderer: TProc<T>); overload; inline;
    procedure AssignVirtualFileDelayed<T: class, constructor, IStreamPersist>(
      const Details: TVirtualFileDetails; const Renderer: TProc<T>); overload; inline;
    procedure AssignVirtualFileDelayed(const VirtualFileName: string; const Renderer: TFunc<string>;
      Encoding: TEncoding = nil; Options: TVirtualTextFileOptions = []); overload; inline;
    procedure AssignVirtualFileDelayed(const Details: TVirtualFileDetails;
      const Renderer: TFunc<string>; Encoding: TEncoding = nil;
      Options: TVirtualTextFileOptions = []); overload; inline;
    { other writing }
    procedure LoadFromStream(const Format: TClipboardFormat; Stream: TStream);
    procedure SetComponent(const Component: TComponent); overload; inline;
    procedure SetComponent(const Format: TClipboardFormat; const Component: TComponent); overload;
    { promising status }
    function HasOutstandingPromisesToOS: Boolean; inline;
    procedure CancelOutstandingPromisesToOS; inline;
    procedure ResolveOutstandingPromisesToOS; overload; inline;
    { reading data }
    procedure EnumFormatSets(const Callback: TEnumFormatSetsCallback);
    function GetBytes(const Format: TClipboardFormat): TBytes; overload;
    procedure GetBytes(const Format: TClipboardFormat; const Callback: TGetBytesCallback); overload;
    function GetComponent(const Owner: TComponent; const Parent: TComponent = nil): TComponent; overload;
    function GetComponents(const Owner: TComponent; const Callback: TGetInstancesCallback<TComponent>): Boolean; overload; inline;
    function GetComponents(const Owner: TComponent; const ReaderCreator: TFunc<TStream, TReader>;
      const Callback: TGetInstancesCallback<TComponent>): Boolean; overload; inline;
    function GetComponents(const Format: TClipboardFormat; const Owner: TComponent;
      const Callback: TGetInstancesCallback<TComponent>): Boolean; overload; inline;
    function GetComponents(const Format: TClipboardFormat; const Owner: TComponent;
      const ReaderCreator: TFunc<TStream, TReader>; const Callback: TGetInstancesCallback<TComponent>): Boolean; overload;
    function GetFormats: TArray<TClipboardFormat>;
    function GetFileNames: TArray<string>; overload;
    procedure GetFileNames(Strings: TStrings); overload;
    procedure GetObjects<T: class>(const Callback: TLoadObjectsCallback<T>); overload;
    procedure GetObjects<T: class, constructor>(DestList: TObjectList<T>); overload;
    procedure GetValues<T: record>(const Callback: TGetInstancesCallback<T>); overload;
    procedure GetValues<T: record>(DestList: TList<T>); overload;
    function GetValues<T: record>: TArray<T>; overload;
    function GetText: TArray<string>; overload;
    procedure GetText(Strings: TStrings); overload;
    function GetURLs: TArray<string>; overload;
    procedure GetURLs(Strings: TStrings); overload;
    function GetVirtualFileDescriptors: TArray<string>; overload;
    procedure GetVirtualFileDescriptors(Strings: TStrings); overload;
    procedure EnumVirtualFiles(const Callback: TEnumVirtualFilesStreamCallback); overload; inline;
    procedure EnumVirtualFiles(const Callback: TEnumVirtualFileBytesCallback); overload;
    procedure SaveVirtualFiles(const Callback: TSaveVirtualFilesCallback); overload;
    procedure SaveVirtualFiles(const Directory: string; SavedFiles: TStrings); overload;
    procedure SaveVirtualFiles(const Directory: string); overload;
    function HasComponent: Boolean; inline;
    function HasFile: Boolean;
    function HasFormat(const Format: TClipboardFormat): Boolean; overload;
    function HasFormat(const Formats: array of TClipboardFormat; var Matched: TClipboardFormat): Boolean; overload;
    function HasFormat(const Formats: array of TClipboardFormat): Boolean; overload;
    function HasFormatFor(const ClassType: TClass): Boolean; overload; inline;
    function HasFormatFor<T>: Boolean; overload;
    function HasText: Boolean;
    function HasURL: Boolean;
    function HasVirtualFile: Boolean;
    function SaveToStream(const Format: TClipboardFormat; Stream: TStream): Boolean;
    function ToString: string; overload; override;
    property ChangeCount: TClipboardChangeCount read GetChangeCount;
    property Empty: Boolean read GetEmpty;
    property AsRTF: string read GetAsRTF write AssignRTF;
    property AsText: string read ToString write AssignText;
    property AsURL: string read GetAsURL write AssignURL;
    property OpenCount: Integer read FOpenCount;
  public
    { The range of possible clipboard formats (as distinct from the range of formats actually
      in the clipboard) is considered global not local to any particular TClipboard instance.
      Use the second version of RegisterFormat or RegisterFormatDescription to force
      a specific value to be returned by GetFormatDescription; otherwise, the latter will
      return either a system-provided description or the just format name. }
    class function GetFormatName(const Format: TClipboardFormat): string; deprecated 'Use Format.Name instead';
    class function RegisterFormat(const Name: string): TClipboardFormat; overload;
    class function RegisterFormat(const Name, Description: string): TClipboardFormat; overload;
    class procedure RegisterFormatDescription(const Format: TClipboardFormat; const Description: string);
    class function SupportsCustomFormats: Boolean;
    class function SupportsMultipleFormatSets: Boolean;
    { Returns True if a clipper has been registered for the type itself or an ancestor }
    class function HasClipper(ClassType: TClass): Boolean; overload;
    class function HasClipper<T>: Boolean; overload;
    { Retrieves registered clipper for class itself (preferred) or an ancestor }
    class function GetClipper(ClassType: TClass): ICustomClipper; overload;
    class function GetClipper<T>: ICustomClipper<T>; overload;
    class function GetObjectClipper<T: class>: IObjectClipper<T>; overload;
    class function GetRecordClipper<T: record>: IRecordClipper<T>; overload;
    { A 'clipper' acts as an intermediary between a TClipboard instance and an instance of
      another type that you want to be able to assign to or from. }
    class procedure RegisterClipper<T: class>(Clipper: IObjectClipper<T>); overload;
    class procedure RegisterClipper<T: record>(Clipper: IRecordClipper<T>); overload;
    { Convenience overloads for creating object clippers; the 'file extensions' can be UTIs
      on OS X, just make sure any actual file extensions start with a full stop (period). }
    class procedure RegisterClipper<T: class, constructor>(
      const Format: TClipboardFormat;
      const OnLoadFromClipboardFormat: TLoadFromClipboardFormatEvent<T>;
      const OnSaveToClipboardFormat: TSaveToClipboardFormatEvent<T>); overload;
    class procedure RegisterClipper<T: class, constructor>(
      const ReadFormats, WrittenFormats: array of TClipboardFormat;
      const OnLoadFromClipboardFormat: TLoadFromClipboardFormatEvent<T>;
      const OnSaveToClipboardFormat: TSaveToClipboardFormatEvent<T>); overload;
    class procedure RegisterClipper<T: class, constructor>(
      const ReadFormats, WrittenFormats: array of TClipboardFormat;
      const FileExts: array of string;
      const OnLoadFromClipboardFormat: TLoadFromClipboardFormatEvent<T>;
      const OnSaveToClipboardFormat: TSaveToClipboardFormatEvent<T>;
      const OnLoadFromStream: TLoadFromClipboardStreamEvent<T>;
      const OnSaveToStream: TSaveToClipboardStreamEvent<T>); overload;
    class procedure RegisterClipper<T: class, constructor>(
      const Format: TClipboardFormat; const FileExts: array of string;
      const OnLoadFromClipboardFormat: TLoadFromClipboardFormatEvent<T>;
      const OnSaveToClipboardFormat: TSaveToClipboardFormatEvent<T>;
      const OnLoadFromStream: TLoadFromClipboardStreamEvent<T>;
      const OnSaveToStream: TSaveToClipboardStreamEvent<T>); overload;
    { ... and more specifically, object clippers for IStreamPersist implementors }
    class procedure RegisterClipper<T: class, constructor, IStreamPersist>(
      const ReadFormats, WrittenFormats: array of TClipboardFormat;
      const FileExts: array of string;
      const OnLoadFromClipboardFormat: TLoadFromClipboardFormatEvent<T>;
      const OnSaveToClipboardFormat: TSaveToClipboardFormatEvent<T>); overload;
    class procedure RegisterClipper<T: class, constructor, IStreamPersist>(
      const Format: TClipboardFormat; const FileExts: array of string;
      const OnLoadFromClipboardFormat: TLoadFromClipboardFormatEvent<T>;
      const OnSaveToClipboardFormat: TSaveToClipboardFormatEvent<T>); overload;
    { ..or for any TPersistent descendant with a parameterless constructor, using DFM streaming }
    class function RegisterSimpleClipper<T: TPersistent, constructor>: ISimpleObjectClipper<T>; overload;
    class function RegisterSimpleClipper<T: TPersistent, constructor>(
      const FormatName: string): ISimpleObjectClipper<T>; overload;
    { Whether virtual files are supported at all; if False, an attempt to write a virtual file
      will raise an exception. Note that while returns True on OS X, Finder (unlike Explorer on
      Windows) does not support virtual files via the system clipboard, only via drag and drop. }
    class function SupportsVirtualFiles: Boolean;
    { The full drag and drop interface is implemented at the framework level with a
      class helper; these are the common parts }
    class property ObjectBeingDragged: TObject read FObjectBeingDragged;
    class property OnGetDragData[const DraggedObject: TObject]: TAssignClipboardForDragEvent read GetOnGetDragData write SetOnGetDragData;
    class property OnStartDrag: TAssignClipboardForDragEvent read FOnStartDrag write FOnStartDrag;
  end;

{ Helper classes }

  TUserMemoryStream = class(TCustomMemoryStream)
    constructor Create(AMemory: Pointer; const ALength: NativeInt);
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

{ Renderer adapters and helpers }

  ICachedFunc<T> = interface(TFunc<T>)
  ['{4D1F49E0-39BB-4118-B25C-FB085DED2C30}']
  end;

  TCachedFunc<T> = class(TInterfacedObject, TFunc<T>, ICachedFunc<T>)
  strict private
    FCoreRenderer: TFunc<T>;
    FRendered: Boolean;
    FValue: T;
  strict protected
    constructor CreateInternal(const CoreRenderer: TFunc<T>);
    function Invoke: T;
    property Rendered: Boolean read FRendered;
  public
    class function Create(const CoreRenderer: TFunc<T>): TFunc<T>; 
  end;

  TRenderedObject<T: class, constructor> = class(TCachedFunc<T>)
  public
    class function Create(const Renderer: TProc<T>): TFunc<T>; 
    {$IFNDEF AUTOREFCOUNT}
    destructor Destroy; override;
    {$ENDIF}
  end;

function BytesToStreamRenderer(const Bytes: TBytes): TProc<TStream>;
function BytesRendererToStreamRenderer(const RendererToWrap: TFunc<TBytes>): TProc<TStream>;
function StreamRendererToBytesRenderer(const RendererToWrap: TProc<TStream>): TFunc<TBytes>;

function BytesOf(const Obj: IStreamPersist): TBytes; overload;

function MatchFileExt(const FileName: string; const FileExts: TArray<string>): Boolean; overload;
function MatchFileExt(const FileNames, FileExts: TArray<string>;
  var MatchedFileName: string): Boolean; overload;
function MatchFileExt(const FileNames, FileExts: TArray<string>): Boolean; overload;

function Clipboard: TClipboard; inline;

function cfComponent: TClipboardFormat; inline;
function cfComponents: TClipboardFormat; inline;
function cfFileName: TClipboardFormat; inline;
function cfGIF: TClipboardFormat; inline;
function cfJpeg: TClipboardFormat; inline;
function cfPNG: TClipboardFormat; inline;
function cfRTF: TClipboardFormat; inline;
function cfUnicodeText: TClipboardFormat; inline;
function cfUTF8Text: TClipboardFormat; inline;
function cfTIFF: TClipboardFormat; inline;
function cfURL: TClipboardFormat; inline;
function cfVirtualFileDescriptor: TClipboardFormat; inline;

implementation

uses
  System.Math, System.SysConst, System.RTLConsts,
  {$IF DEFINED(MSWINDOWS)}
  CCR.Clipboard.Win,
  {$ELSEIF DEFINED(MACOS)}
  CCR.Clipboard.Apple,
//  {$ELSEIF DEFINED(ANDROID)}
//  CCR.Clipboard.Android;
  {$IFEND}
  CCR.Clipboard.Consts;

function HashBuffer(const Buffer; Size: Integer): Integer; inline;
begin
  {$IF DECLARED(THashBobJenkins)}
  Result := THashBobJenkins.GetHashValue(Buffer, Size);
  {$ELSE}
  Result := BobJenkinsHash(Buffer, Size, 0);
  {$IFEND}
end;

function Clipboard: TClipboard;
begin
  Result := TClipboard.GetSingleton;
end;

function cfUnicodeText: TClipboardFormat;
begin
  Result := TClipboard.StdFormatValues.cfUnicodeText
end;

function cfUTF8Text: TClipboardFormat;
begin
  Result := TClipboard.StdFormatValues.cfUTF8Text
end;

function cfComponent: TClipboardFormat;
begin
  Result := TClipboard.StdFormatValues.cfComponent;
end;

function cfComponents: TClipboardFormat;
begin
  Result := TClipboard.StdFormatValues.cfComponents;
end;

function cfFileName: TClipboardFormat;
begin
  Result := TClipboard.StdFormatValues.cfFileName;
end;

function cfGIF: TClipboardFormat;
begin
  Result := TClipboard.StdFormatValues.cfGIF;
end;

function cfJpeg: TClipboardFormat;
begin
  Result := TClipboard.StdFormatValues.cfJPEG;
end;

function cfPNG: TClipboardFormat;
begin
  Result := TClipboard.StdFormatValues.cfPNG
end;

function cfRTF: TClipboardFormat;
begin
  Result := TClipboard.StdFormatValues.cfRTF;
end;

function cfTIFF: TClipboardFormat;
begin
  Result := TClipboard.StdFormatValues.cfTIFF
end;

function cfURL: TClipboardFormat;
begin
  Result := TClipboard.StdFormatValues.cfURL;
end;

function cfVirtualFileDescriptor: TClipboardFormat;
begin
  Result := TClipboard.StdFormatValues.cfVirtualFileDescriptor;
end;

function ExpandAndValidateFileName(const FileName: TFileName): TFileName;
begin
  Result := ExpandFileName(FileName);
  if (Result = '') or not (FileExists(Result) or DirectoryExists(Result)) then
    raise EClipboardException.CreateResFmt(@SInvalidFileName, [FileName]);
end;

{$REGION 'TClipboardFormat'}

class operator TClipboardFormat.Equal(const Format1, Format2: TClipboardFormat): Boolean;
begin
  Result := (Format1.OrdValue = Format2.OrdValue) or
    (Assigned(TClipboard.CoreClass) and TClipboard.CoreClass.SameFormat(Format1, Format2));
end;

class operator TClipboardFormat.NotEqual(const Format1, Format2: TClipboardFormat): Boolean;
begin
  Result := not (Format1 = Format2);
end;

function TClipboardFormat.ConformsTo(const BaseFormat: TClipboardFormat): Boolean;
begin
  Result := (Self.OrdValue = BaseFormat.OrdValue) or
    ((TClipboard.CoreClass <> nil) and TClipboard.CoreClass.ConformingFormat(Self, BaseFormat));
end;

function TClipboardFormat.GetName: string;
begin
  if TClipboard.CoreClass = nil then
    Result := ''
  else
    Result := TClipboard.CoreClass.GetFormatName(Self);
end;

function TClipboardFormat.GetDescription: string;
begin
  if TClipboard.CoreClass = nil then
    Result := ''
  else
    Result := TClipboard.GetFormatDescription(Self);
end;

function TClipboardFormat.ToHexString(MinDigits: Integer = 4): string;
begin
  Result := IntToHex(OrdValue, MinDigits);
end;
{$ENDREGION}

{$REGION 'TObjectHandlerList<T>'}

constructor TObjectHandlerList<T>.Create;
begin
  inherited Create;
  FItems := TObjectList<TItem>.Create;
end;

destructor TObjectHandlerList<T>.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TObjectHandlerList<T>.AddOrReplace(const ClassFor: TClass; const Handler: T);
var
  Item: TItem;
  GenCount, InsIndex, I: Integer;
begin
  if PPointer(@Handler)^ = nil then
    raise EArgumentNilException.CreateRes(@SArgumentNil);
  if not FindItem(ClassFor, Item, True) then
  begin
    GenCount := CountGenerations(TObject, ClassFor);
    InsIndex := FItems.Count;
    for I := 0 to InsIndex - 1 do
      if GenCount >= FItems[I].GenCount then
      begin
        InsIndex := I;
        Break;
      end;
    Item := TItem.Create;
    FItems.Insert(InsIndex, Item);
    Item.Detail.Key := ClassFor;
  end;
  Item.Detail.Value := Handler;
end;

function TObjectHandlerList<T>.FindItem(const ClassFor: TClass; var Matched: TItem; Exact: Boolean): Boolean;
var
  Item: TItem;
begin
  for Item in FItems do
  begin
    if Exact then
      Result := (ClassFor = Item.Detail.Key)
    else
      Result := ClassFor.InheritsFrom(Item.Detail.Key);
    if Result then
    begin
      Matched := Item;
      Exit;
    end;
  end;
  Result := False;
end;

function TObjectHandlerList<T>.Find(const ClassFor: TClass; var Handler: T): Boolean;
var
  Item: TItem;
begin
  Result := FindItem(ClassFor, Item, False);
  if Result then
    Handler := Item.Detail.Value;
end;
{$ENDREGION}

{$REGION 'TVirtualFileDetails'}

constructor TVirtualFileDetails.Create(const AFileName: string; const AKnownSize: Int64 = 0);
begin
  FileName := AFileName;
  if AKnownSize = 0 then
    AdditionalData := []
  else
  begin
    AdditionalData := [vfSize];
    FSize := AKnownSize;
  end;
end;

procedure TVirtualFileDetails.SetAttr(const Value: Integer);
begin
  FAttr := Value;
  Include(AdditionalData, vfAttr);
end;

procedure TVirtualFileDetails.SetCreationDate(const Value: TDateTime);
begin
  FCreationDate := Value;
  Include(AdditionalData, vfCreationDate);
end;

procedure TVirtualFileDetails.SetLastAccessDate(const Value: TDateTime);
begin
  FLastAccessDate := Value;
  Include(AdditionalData, vfLastAccessDate);
end;

procedure TVirtualFileDetails.SetLastWriteDate(const Value: TDateTime);
begin
  FLastWriteDate := Value;
  Include(AdditionalData, vfLastWriteDate);
end;

procedure TVirtualFileDetails.SetSize(const Value: Int64);
begin
  FSize := Value;
  Include(AdditionalData, vfSize);
end;
{$ENDREGION}

{$REGION 'TClipboard.TCore'}

class procedure TClipboard.TCore.Register;
begin
  TClipboard.SetCoreClass(Self);
end;

constructor TClipboard.TCore.Create(const AOwner: TClipboard);
begin
  inherited Create;
  FOwnerWeakRef := Pointer(AOwner);
end;

destructor TClipboard.TCore.Destroy;
begin
  ChangeNotifications := False;
  inherited;
end;

function TClipboard.TCore.GetOwner: TClipboard;
begin
  Result := TClipboard(FOwnerWeakRef);
end;

function TClipboard.TCore.HasFile: Boolean;
begin
  Result := HasFormat(cfFileName);
end;

function TClipboard.TCore.HasPlainText: Boolean;
var
  Matched: TClipboardFormat;
begin
  Result := HasFormat([cfUnicodeText, cfUTF8Text], Matched);
end;

function TClipboard.TCore.HasURL: Boolean;
begin
  Result := HasFormat(cfURL)
end;

function TClipboard.TCore.HasVirtualFile: Boolean;
begin
  Result := HasFormat(cfVirtualFileDescriptor)
end;

function TClipboard.TCore.WrapsSystemClipboard: Boolean;
begin
  Result := (GetOwner = TClipboard.GetSingleton);
end;

class function TClipboard.TCore.IsCompatibleVirtualFileDescriptor(
  const Descriptor: string; const SupportedFileExts: TArray<string>): Boolean;
begin
  Result := False;
end;

class function TClipboard.TCore.ConformingFormat(const FormatToTest, BaseFormat: TClipboardFormat): Boolean;
begin
  Result := False;
end;

class function TClipboard.TCore.SameFormat(const Format1, Format2: TClipboardFormat): Boolean;
begin
  Result := False;
end;

class function TClipboard.TCore.GetFormatDescription(const Format: TClipboardFormat): string;
begin
  Result := GetFormatName(Format);
end;

class function TClipboard.TCore.RegisterFormat(const Name: string): TClipboardFormat;
begin
  raise EUnsupportedClipboardFeature.CreateRes(@SCustomClipboardFormatsNotSupported);
end;

function TClipboard.TCore.SupportsChangeNotifications: Boolean;
begin
  Result := False;
end;

procedure TClipboard.TCore.SetChangeNotifications(Enable: Boolean);
begin
  if Enable = FChangeNotifications then Exit;
  if Enable then
    if not SupportsChangeNotifications then
      raise EUnsupportedClipboardFeature.CreateRes(@SChangeListenersNotSupported)
    else
      EnableChangeNotifications(Clipboard.NotifyChangeListeners)
  else
    DisableChangeNotifications;
  FChangeNotifications := Enable;
end;

procedure TClipboard.TCore.EnableChangeNotifications(const Callback: TThreadMethod);
begin
end;

procedure TClipboard.TCore.DisableChangeNotifications;
begin
end;

function TClipboard.TCore.GetChangeCount: TClipboardChangeCount;
begin
  Inc(FDummyChangeCount);
  Result := FDummyChangeCount;
end;
{$ENDREGION}

{$REGION 'TClipboard.TEnumerator'}

constructor TClipboard.TEnumerator.Create(const AFormats: TArray<TClipboardFormat>);
begin
  FRegisteredFormats := AFormats;
  FIndex := -1;
end;

function TClipboard.TEnumerator.GetCurrent: TClipboardFormat;
begin
  Result := FRegisteredFormats[FIndex];
end;

function TClipboard.TEnumerator.MoveNext: Boolean;
begin
  Result := (FIndex < High(FRegisteredFormats));
  if Result then Inc(FIndex);
end;
{$ENDREGION}

{$REGION 'TClipboard - general class methods'}

class constructor TClipboard.InitializeClass;
begin
  NeedClassInitialized;
end;

class destructor TClipboard.FinalizeClass;
begin
  FSingleton.Free;
  FStdFormats.Free;
  FOnGetDragData.Free;
  FFormatDescriptions.Free;
  FObjectClippers.Free;
  FRecordClippers.Free;
end;

class procedure TClipboard.CheckInterfaceAssigned<T>(const Service: T);
begin
  if Service = nil then
    raise EUnsupportedClipboardFeature.CreateResFmt(@SUnsupportedClipboardInterface,
      [GetTypeName(TypeInfo<T>)]);
end;

class procedure TClipboard.NeedClassInitialized;
begin
  if FObjectClippers <> nil then Exit;
  FObjectClippers := TObjectHandlerList<ICustomClipper>.Create;
  FRecordClippers := TDictionary<PTypeInfo, ICustomClipper>.Create;
  FFormatDescriptions := TDictionary<TClipboardFormat, string>.Create;
  FOnGetDragData := TDictionary<Pointer, TAssignClipboardForDragEvent>.Create;
  FCreateReaderDef :=
    function (Stream: TStream): TReader
    begin
      Result := TReader.Create(Stream, $1000);
    end;
end;

class function TClipboard.GetSuitableCoreClass(const MinClass: TCoreClass): TCoreClass;
begin
  if (FCoreClass <> nil) and FCoreClass.InheritsFrom(MinClass) then
    Result := FCoreClass
  else
    Result := MinClass;
end;

class function TClipboard.GetFormatName(const Format: TClipboardFormat): string;
begin
  NeedCoreClassSet;
  Result := FCoreClass.GetFormatName(Format);
end;

class function TClipboard.GetFormatDescription(const Format: TClipboardFormat): string;
begin
  if FFormatDescriptions.TryGetValue(Format, Result) then Exit;
  NeedCoreClassSet;
  Result := FCoreClass.GetFormatDescription(Format);
end;

class function TClipboard.RegisterFormat(const Name: string): TClipboardFormat;
begin
  if SupportsCustomFormats then
    Result := FCoreClass.RegisterFormat(Name)
  else
    raise EUnsupportedClipboardFeature.CreateRes(@SCustomClipboardFormatsNotSupported);
end;

class function TClipboard.RegisterFormat(const Name, Description: string): TClipboardFormat;
begin
  if SupportsCustomFormats then
    Result := FCoreClass.RegisterFormat(Name)
  else
    raise EUnsupportedClipboardFeature.CreateRes(@SCustomClipboardFormatsNotSupported);
  RegisterFormatDescription(Result, Description);
end;

class procedure TClipboard.RegisterFormatDescription(const Format: TClipboardFormat;
  const Description: string);
begin
  FFormatDescriptions.AddOrSetValue(Format, Description);
end;

class function TClipboard.SupportsCustomFormats: Boolean;
begin
  NeedCoreClassSet;
  Result := FSupportsCustomFormats;
end;

class function TClipboard.SupportsMultipleFormatSets: Boolean;
begin
  NeedCoreClassSet;
  Result := Supports(FCoreClass, IMultipleFormatSets);
end;

class function TClipboard.HasClipper(ClassType: TClass): Boolean;
var
  Clipper: ICustomClipper;
begin
  Result := FObjectClippers.Find(ClassType, Clipper);
end;

class function TClipboard.HasClipper<T>: Boolean;
var
  Info: PTypeInfo;
begin
  Info := TypeInfo<T>;
  if Info.Kind = tkClass then
    Result := HasClipper(GetTypeData(Info).ClassType)
  else
    Result := FRecordClippers.ContainsKey(Info);
end;

class procedure TClipboard.RegisterClipper<T>(Clipper: IObjectClipper<T>);
begin
  NeedClassInitialized;
  FObjectClippers.AddOrReplace(T, Clipper);
end;

class procedure TClipboard.RegisterClipper<T>(Clipper: IRecordClipper<T>);
begin
  NeedClassInitialized;
  FRecordClippers.AddOrSetValue(TypeInfo<T>, Clipper);
end;

class procedure TClipboard.RegisterClipper<T>(const Format: TClipboardFormat;
  const FileExts: array of string;
  const OnLoadFromClipboardFormat: TLoadFromClipboardFormatEvent<T>;
  const OnSaveToClipboardFormat: TSaveToClipboardFormatEvent<T>);
begin
  RegisterClipper<T>(TDelegatedObjectClipper<T>.Create([Format], [Format], FileExts,
    OnLoadFromClipboardFormat, OnSaveToClipboardFormat, nil, nil));
end;

class procedure TClipboard.RegisterClipper<T>(
  const Format: TClipboardFormat;
  const OnLoadFromClipboardFormat: TLoadFromClipboardFormatEvent<T>;
  const OnSaveToClipboardFormat: TSaveToClipboardFormatEvent<T>);
begin
  RegisterClipper<T>(TDelegatedObjectClipper<T>.Create([Format], [Format], 
    [], OnLoadFromClipboardFormat, OnSaveToClipboardFormat, nil, nil));
end;

class procedure TClipboard.RegisterClipper<T>(
  const ReadFormats, WrittenFormats: array of TClipboardFormat;
  const OnLoadFromClipboardFormat: TLoadFromClipboardFormatEvent<T>;
  const OnSaveToClipboardFormat: TSaveToClipboardFormatEvent<T>);
begin
  RegisterClipper<T>(TDelegatedObjectClipper<T>.Create(ReadFormats, WrittenFormats,
    [], OnLoadFromClipboardFormat, OnSaveToClipboardFormat, nil, nil));
end;

class procedure TClipboard.RegisterClipper<T>(const ReadFormats, WrittenFormats: array of TClipboardFormat;
  const FileExts: array of string;
  const OnLoadFromClipboardFormat: TLoadFromClipboardFormatEvent<T>;
  const OnSaveToClipboardFormat: TSaveToClipboardFormatEvent<T>;
  const OnLoadFromStream: TLoadFromClipboardStreamEvent<T>;
  const OnSaveToStream: TSaveToClipboardStreamEvent<T>);
begin
  RegisterClipper<T>(TDelegatedObjectClipper<T>.Create(ReadFormats, WrittenFormats,
    FileExts, OnLoadFromClipboardFormat, OnSaveToClipboardFormat,
    OnLoadFromStream, OnSaveToStream));
end;

class procedure TClipboard.RegisterClipper<T>(const Format: TClipboardFormat;
  const FileExts: array of string;
  const OnLoadFromClipboardFormat: TLoadFromClipboardFormatEvent<T>;
  const OnSaveToClipboardFormat: TSaveToClipboardFormatEvent<T>;
  const OnLoadFromStream: TLoadFromClipboardStreamEvent<T>;
  const OnSaveToStream: TSaveToClipboardStreamEvent<T>);
begin
  RegisterClipper<T>(TDelegatedObjectClipper<T>.Create([Format], [Format],
    FileExts, OnLoadFromClipboardFormat, OnSaveToClipboardFormat,
    OnLoadFromStream, OnSaveToStream));
end;

class procedure TClipboard.RegisterClipper<T>(
  const ReadFormats, WrittenFormats: array of TClipboardFormat;
  const FileExts: array of string;
  const OnLoadFromClipboardFormat: TLoadFromClipboardFormatEvent<T>;
  const OnSaveToClipboardFormat: TSaveToClipboardFormatEvent<T>);
begin
  RegisterClipper<T>(TDelegatedObjectClipper<T>.Create(ReadFormats, WrittenFormats,
    FileExts, OnLoadFromClipboardFormat, OnSaveToClipboardFormat, nil, nil));
end;

class function TClipboard.AllocFormat(ClassType: TClass): TClipboardFormat;
const
  SystemSuffix = 'System.';
  FMXSuffix = 'FMX.';
  VCLSuffix = 'Vcl.';
var
  Name: string;
begin
  Name := ClassType.QualifiedClassName;
  if not (CompareMem(PChar(Name), PChar(SystemSuffix), Length(SystemSuffix) * SizeOf(Char)) or
          CompareMem(PChar(Name), PChar(FMXSuffix), Length(FMXSuffix) * SizeOf(Char)) or
          CompareMem(PChar(Name), PChar(VCLSuffix), Length(VCLSuffix) * SizeOf(Char))) then
    Name := ChangeFileExt(ExtractFileName(GetModuleName(0)), '.' + Name);
  Result := TClipboard.RegisterFormat(Name);
end;

class function TClipboard.RegisterSimpleClipper<T>: ISimpleObjectClipper<T>;
begin
  Result := TPersistentClipper<T>.Create(AllocFormat(T));
  RegisterClipper<T>(Result);
end;

class function TClipboard.RegisterSimpleClipper<T>(const FormatName: string): ISimpleObjectClipper<T>;
begin
  Result := TPersistentClipper<T>.Create(RegisterFormat(FormatName));
  RegisterClipper<T>(Result);
end;

class function TClipboard.CreateDynArray<T>(const Items: array of T): TArray<T>;
var
  I: Integer;
begin
  SetLength(Result, Length(Items));
  for I := Low(Result) to High(Result) do
    Result[I] := Items[I];
end;

function MatchFileExt(const FileName: string; const FileExts: TArray<string>): Boolean; overload;
var
  WantedExt, Ext: string;
begin
  if FileExts <> nil then
  begin
    WantedExt := ExtractFileExt(FileName);
    for Ext in FileExts do
      if SameText(WantedExt, Ext) then
      begin
        Result := True;
        Exit;
      end;
  end;
  Result := False;
end;

function MatchFileExt(const FileNames, FileExts: TArray<string>;
  var MatchedFileName: string): Boolean; overload;
var
  FileName: string;
begin
  for FileName in FileNames do
    if MatchFileExt(FileName, FileExts) then
    begin
      MatchedFileName := FileName;
      Result := True;
      Exit;
    end;
  Result := False;
end;

function MatchFileExt(const FileNames, FileExts: TArray<string>): Boolean; overload;
var
  FileName: string;
begin
  for FileName in FileNames do
    if MatchFileExt(FileName, FileExts) then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end;

class procedure TClipboard.SetCoreClass(const CoreClass: TCoreClass);
begin
  Assert(CoreClass <> nil);
  if FStdFormats <> nil then
    raise EClipboardException.CreateRes(@SCoreClassAlreadyInitialized);
  FCoreClass := CoreClass;
end;

class function TClipboard.GetStdFormats: TStdFormatValues;
begin
  NeedCoreClassSet;
  Result := FStdFormats;
end;

class procedure TClipboard.NeedCoreClassSet;
  procedure BackfillIfNec(var Format: TClipboardFormat; const Name: string);
  begin
    if NativeUInt(Format) = 0 then
      Format := FCoreClass.RegisterFormat(Name);
  end;
begin
  if FStdFormats <> nil then
    Exit;
  if FCoreClass = nil then
    raise EClipboardException.CreateRes(@SClipboardCoreClassMissing);
  FStdFormats := TStdFormatValues.Create;
  FCoreClass.InitializeFormats(FStdFormats, FSupportsCustomFormats);
  if FSupportsCustomFormats then
  begin
    BackfillIfNec(FStdFormats.cfComponent, 'Delphi Component');
    BackfillIfNec(FStdFormats.cfComponents, 'Delphi Components');
    BackfillIfNec(FStdFormats.cfUTF8Text, 'UTF-8 Text');
  end;
end;

class function TClipboard.GetSingleton: TClipboard;
begin
  if FSingleton = nil then
    FSingleton := TClipboard.Create;
  Result := FSingleton;
end;
{$ENDREGION}

{$REGION 'TClipboard - general instance methods'}

type
  TFreeNotifyProxy = class(TComponent)
  strict private
    FOnNotify: TProc<TComponent>;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(const OnNotify: TProc<TComponent>); reintroduce;
  end;

constructor TFreeNotifyProxy.Create(const OnNotify: TProc<TComponent>);
begin
  inherited Create(nil);
  FOnNotify := OnNotify;
end;

procedure TFreeNotifyProxy.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    FOnNotify(AComponent);
end;

constructor TClipboard.Create(const ACore: TCore);
begin
  inherited Create;
  if ACore <> nil then
    FCore := ACore
  else
  begin
    NeedCoreClassSet;
    FCore := FCoreClass.Create(Self);
  end;
  FCoreIntf := FCore;
  FCoreIntf.QueryInterface(IDelayedRendering, FDelayedRenderingIntf);
  FCoreIntf.QueryInterface(IMultipleFormatSets, FMultipleFormatSetsIntf);
  FCoreIntf.QueryInterface(IReadWriteBytes, FReadWriteBytesIntf);
  FCoreIntf.QueryInterface(IReadWriteStream, FReadWriteStreamIntf);
  FCoreIntf.QueryInterface(IReadWriteFileNames, FReadWriteFileNamesIntf);
  FCoreIntf.QueryInterface(IReadWriteURL, FReadWriteURLIntf);
  FCoreIntf.QueryInterface(IReadWriteVirtualFile, FReadWriteVirtualFileIntf);
  FComponentStreams := TObjectDictionary<TClipboardFormat, TBytesStream>.Create([doOwnsValues],
    TEqualityComparer<TClipboardFormat>.Construct(
      function (const Left, Right: TClipboardFormat): Boolean
      begin
        Result := Left = Right;
      end,
      function(const Value: TClipboardFormat): Integer
      begin
        Result := HashBuffer(Value, SizeOf(Value));
      end));
  FChangeListeners := TList<Pointer>.Create;
  FFreeNotifyProxy := TFreeNotifyProxy.Create(
    procedure (Component: TComponent)
    begin
      FChangeListeners.Remove(Component);
      FOnGetDragData.Remove(Component);
    end);
  FFileNamesCacheChangeCount := TClipboardChangeCount(-1);
  FFormatsCacheChangeCount := TClipboardChangeCount(-1);
  FHasFormatForCacheChangeCount := TClipboardChangeCount(-1);
  FURLsCacheChangeCount := TClipboardChangeCount(-1);
  FVirtualFileDescriptorsCacheChangeCount := TClipboardChangeCount(-1);
end;

destructor TClipboard.Destroy;
begin
  if FCore <> nil then FCore.ChangeNotifications := False;
  FFreeNotifyProxy.Free;
  FChangeListeners.Free;
  FComponentStreams.Free;
  inherited;
end;

procedure TClipboard.BeforeDestruction;
begin
  inherited BeforeDestruction;
  { Expecially if we're the singleton, any outstanding delayed rendering callbacks
    are likely to involve stale references by the time we're being destroyed. }
  CancelOutstandingPromisesToOS;
end;

procedure TClipboard.AddFreeNotification(const Obj: TObject);
begin
  if Obj is TComponent then
    FFreeNotifyProxy.FreeNotification(TComponent(Obj));
end;

procedure TClipboard.RemoveFreeNotification(const Obj: TObject);
begin
  if Obj is TComponent then
    FFreeNotifyProxy.RemoveFreeNotification(TComponent(Obj));
end;

function TClipboard.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(GetFormats);
end;

function TClipboard.IsOpenForWriting: Boolean;
begin
  Result := FIsOpenForWriting;
end;

function TClipboard.IsSystemClipboard: Boolean;
begin
  Result := FCore.WrapsSystemClipboard;
end;

function TClipboard.TryOpen: Boolean;
begin
  Result := (FOpenCount <> 0) or FCore.DoOpen;
  if Result then Inc(FOpenCount);
end;

procedure TClipboard.Open;
begin
  if not TryOpen then
    raise EClipboardException.CreateResFmt(@SCannotOpenClipboard, [SysErrorMessage(GetLastError)]);
end;

procedure TClipboard.OpenForReading;
begin
  if IsOpenForWriting then
    raise EClipboardException.CreateRes(@SCannotReadWhenOpenForWriting);
  Open;
end;

function TClipboard.TryOpenForReading(RaiseExceptionIfOpenForWriting: Boolean): Boolean;
begin
  if RaiseExceptionIfOpenForWriting and IsOpenForWriting then
    raise EClipboardException.CreateRes(@SCannotReadWhenOpenForWriting)
  else
    Result := not IsOpenForWriting and TryOpen;
end;

procedure TClipboard.OpenForWriting;
begin
  Open;
  if not IsOpenForWriting then
  begin
    FAssignedFormatCount := 0;
    FUserAssignDelayedCount := 0;
    FIsOpenForWriting := True;
    FCore.DoClear;
  end;
end;

procedure TClipboard.Clear;
begin
  Open;
  try
    FCore.DoClear;
    FAssignedFormatCount := 0;
    FUserAssignDelayedCount := 0;
  finally
    Close;
  end;
end;

procedure TClipboard.Close;
var
  Written: Boolean;
begin
  case FOpenCount of
    0: raise EClipboardException.CreateRes(@SUnbalancedClose);
    1: FlushComponents;
  end;
  Dec(FOpenCount);
  if FOpenCount <> 0 then Exit;
  try
    FCore.DoClose;
    if (FAssignedFormatCount <> 0) and (FUserAssignDelayedCount = 0) then
      ResolveOutstandingPromisesToOS(False);
    Written := FIsOpenForWriting;
  finally
    FUserAssignDelayedCount := 0;
    FIsOpenForWriting := False;
    FAssignedFormatCount := 0;
  end;
  if Written then NotifyChangeListeners;
end;

procedure TClipboard.Add(const DoAssignCode: TProc);
begin
  OpenForWriting;
  try
    DoAssignCode;
    Inc(FAssignedFormatCount);
  finally
    Close;
  end;
end;

procedure TClipboard.NewFormatSet;
begin
  if not IsOpenForWriting then
    raise EClipboardException.CreateRes(@SOrphanNewFormatSetCall);
  CheckInterfaceAssigned(FMultipleFormatSetsIntf);
  FMultipleFormatSetsIntf.NewFormatSet;
end;

class function TClipboard.SupportsVirtualFiles: Boolean;
begin
  NeedCoreClassSet;
  Result := Supports(FCoreClass, IReadWriteVirtualFile);
end;
{$ENDREGION}

{$REGION 'TClipboard - change counts and notifications'}

function TClipboard.GetChangeCount: TClipboardChangeCount;
begin
  Result := FCore.GetChangeCount;
end;

function TClipboard.SupportsChangeListeners: Boolean;
begin
  Result := FCore.SupportsChangeNotifications;
end;

procedure TClipboard.NotifyChangeListeners;
var
  Ref: Pointer;
begin
  if IsOpenForWriting then Exit; //avoid potential read-while-writing errors
  for Ref in FChangeListeners do
    IClipboardListener(Ref).ClipboardChanged(Self);
end;

procedure TClipboard.RegisterChangeListener(const Listener: IClipboardListener);
begin
  if not SupportsChangeListeners then
    raise EClipboardException.CreateRes(@SChangeNotificationsNotSupported);
  AddFreeNotification(Listener as TObject);
  FChangeListeners.Add(Pointer(Listener));
  FCore.ChangeNotifications := True;
end;

procedure TClipboard.UnregisterChangeListener(const Listener: IClipboardListener);
begin
  if FChangeListeners = nil then Exit;
  if FChangeListeners.Remove(Pointer(Listener)) < 0 then Exit;
  if FChangeListeners.Count = 0 then FCore.ChangeNotifications := False;
  RemoveFreeNotification(Listener as TObject);
end;
{$ENDREGION}

{$REGION 'TClipboard - reading'}

function TClipboard.GetEmpty: Boolean;
begin
  if IsOpenForWriting then
    Result := (FAssignedFormatCount = 0)
  else
    Result := (GetFormats = nil)
end;

function TClipboard.GetFormats: TArray<TClipboardFormat>;
begin
  if FFormatsCacheChangeCount <> ChangeCount then
  begin
    if not TryOpenForReading then Exit(nil);
    try
      FFormatsCacheChangeCount := ChangeCount;
      FFormatsCache := FCore.GetFormats;
    finally
      Close;
    end;
  end;
  Result := Copy(FFormatsCache);
end;

function TClipboard.HasComponent: Boolean;
begin
  Result := HasFormat([cfComponent, cfComponents]);
end;

function TClipboard.HasFile: Boolean;
begin
  Result := (FReadWriteFileNamesIntf <> nil) and FReadWriteFileNamesIntf.HasFile;
end;

function TClipboard.HasFormat(const Format: TClipboardFormat): Boolean;
begin
  if not TryOpenForReading then Exit(False); //avoid potential exception on Windows
  try
    Result := FCore.HasFormat(Format);
  finally
    Close;
  end;
end;

function TClipboard.HasFormat(const Formats: array of TClipboardFormat;
  var Matched: TClipboardFormat): Boolean;
begin
  if not TryOpenForReading then Exit(False); //avoid potential exception on Windows
  try
    Result := FCore.HasFormat(Formats, Matched);
  finally
    Close;
  end;
end;

function TClipboard.HasFormat(const Formats: array of TClipboardFormat): Boolean;
var
  Matched: TClipboardFormat;
begin
  Result := HasFormat(Formats, Matched);
end;

class function TClipboard.FindClipperForClass(TypeHandle: Pointer;
  var Clipper: ICustomClipper): Boolean;
begin
  Result := FObjectClippers.Find(TClass(TypeHandle), Clipper);
end;

class function TClipboard.FindClipperForRecord(TypeHandle: Pointer;
  var Clipper: ICustomClipper): Boolean;
begin
  Result := FRecordClippers.TryGetValue(TypeHandle, Clipper);
end;

function TClipboard.HasFormatFor(TypeHandle: Pointer; const FindClipper: TFindClipper): Boolean;
var
  Clipper: ICustomClipper;
  I: Integer;
begin
  Result := False;
  if FHasFormatForCacheChangeCount <> ChangeCount then
    FHasFormatForCacheCount := 0
  else
    for I := 0 to FHasFormatForCacheCount - 1 do
      if FHasFormatForCache[I].Key = TypeHandle then
        Exit(FHasFormatForCache[I].Value);
  if not FindClipper(TypeHandle, Clipper) then Exit;
  if not TryOpenForReading then Exit;
  try
    if FHasFormatForCacheCount = 0 then
      FHasFormatForCacheChangeCount := ChangeCount;
    Result := Clipper.CanLoadFromClipboard(Self);
    if Length(FHasFormatForCache) = FHasFormatForCacheCount then
      SetLength(FHasFormatForCache, FHasFormatForCacheCount + 8);
    FHasFormatForCache[FHasFormatForCacheCount].Key := TypeHandle;
    FHasFormatForCache[FHasFormatForCacheCount].Value := Result;
    Inc(FHasFormatForCacheCount);
  finally
    Close;
  end;
end;

function TClipboard.HasFormatFor(const ClassType: TClass): Boolean;
begin
  Result := HasFormatFor(ClassType, FindClipperForClass);
end;

function TClipboard.HasFormatFor<T>: Boolean;
var
  Info: PTypeInfo;
begin
  Info := TypeInfo<T>;
  if Info.Kind = tkClass then
    Result := HasFormatFor(Info, FindClipperForClass)
  else
    Result := HasFormatFor(Info, FindClipperForRecord);
end;

function TClipboard.HasText: Boolean;
begin
  Result := FCore.HasPlainText;
end;

function TClipboard.HasURL: Boolean;
begin
  Result := (FReadWriteURLIntf <> nil) and FReadWriteURLIntf.HasURL;
end;

function TClipboard.HasVirtualFile: Boolean;
begin
  Result := (FReadWriteVirtualFileIntf <> nil) and FReadWriteVirtualFileIntf.HasVirtualFile;
end;

class function TClipboard.TypeInfo<T>: PTypeInfo;
begin
  Result := System.TypeInfo(T);
  if Result = nil then
    raise EInsufficientRtti.CreateRes(@SInsufficientRtti);
end;

class procedure TClipboard.NoClipperError(const TypeName: string);
begin
  raise EClipboardException.CreateResFmt(@SNoRegisteredClipper, [TypeName]);
end;

class function TClipboard.GetClipper(ClassType: TClass): ICustomClipper;
begin
  if not FObjectClippers.Find(ClassType, Result) then
    NoClipperError(ClassType.ClassName);
end;

class function TClipboard.GetClipper<T>: ICustomClipper<T>;
var
  Info: PTypeInfo;
begin
  Info := TypeInfo<T>;
  if Info.Kind = tkClass then
    Result := ICustomClipper<T>(GetClipper(GetTypeData(Info).ClassType))
  else if not FRecordClippers.TryGetValue(Info, ICustomClipper(Result)) then
    NoClipperError(GetTypeName(Info));
end;

class function TClipboard.GetObjectClipper<T>: IObjectClipper<T>;
begin
  if not FObjectClippers.Find(T, ICustomClipper(Result)) then
    NoClipperError(T.ClassName);
end;

class function TClipboard.GetRecordClipper<T>: IRecordClipper<T>;
begin
  if not FRecordClippers.TryGetValue(TypeInfo<T>, ICustomClipper(Result)) then
    NoClipperError(GetTypeName(TypeInfo<T>));
end;

procedure TClipboard.AssignTo(Dest: TPersistent);
var
  Clipper: ICustomClipper;
  MadeAssignment: Boolean;
begin
  Clipper := GetClipper(Dest.ClassType);
  OpenForReading;
  try
    MadeAssignment := False;
    IObjectClipper<TObject>(Clipper).LoadFromClipboard(Self,
      procedure (const AssignTo: TProc<TObject>; var LookForMore: Boolean)
      begin
        AssignTo(Dest);
        LookForMore := False;
        MadeAssignment := True;
      end);
    if not MadeAssignment then raise EClipboardException.CreateRes(@SUnknownClipboardFormat);
  finally
    Close;
  end;
end;

procedure TClipboard.EnumFormatSets(const Callback: TEnumFormatSetsCallback);
begin
  CheckInterfaceAssigned(FMultipleFormatSetsIntf);
  OpenForReading;
  try
    FMultipleFormatSetsIntf.EnumFormatSets(Callback);
  finally
    Close;
  end;
end;

function TClipboard.GetBytes(const Format: TClipboardFormat): TBytes;
var
  Stream: TBytesStream;
begin
  if not TryOpenForReading then Exit(nil);
  try
    if FReadWriteBytesIntf <> nil then
      Result := FReadWriteBytesIntf.ReadBytes(Format)
    else if FReadWriteStreamIntf <> nil then
    begin
      Stream := TBytesStream.Create;
      try
        FReadWriteStreamIntf.ReadStream(Format, Stream);
        Stream.SetSize(Stream.Position); //this forces Capacity to match Size
        Result := Stream.Bytes;
      finally
        Stream.Free;
      end;
    end
    else
      Result := nil;
  finally
    Close;
  end;
end;

procedure TClipboard.GetBytes(const Format: TClipboardFormat; const Callback: TGetBytesCallback);
var
  Bytes: TBytes;
  Dummy: Boolean;
begin
  if not TryOpenForReading then Exit;
  try
    if FMultipleFormatSetsIntf <> nil then
      FMultipleFormatSetsIntf.EnumBytes(Format, Callback)
    else
    begin
      Dummy := True;
      Bytes := GetBytes(Format);
      if Bytes <> nil then Callback(Bytes, Dummy);
    end;
  finally
    Close;
  end;
end;

function TClipboard.GetComponent(const Owner, Parent: TComponent): TComponent;
var
  Callback: TGetInstancesCallback<TComponent>;
  ResultPtr: ^TComponent;
begin
  ResultPtr := @Result;
  Callback :=
    procedure (const Comp: TComponent; var CheckForMore: Boolean)
    begin
      ResultPtr^ := Comp;
      CheckForMore := False;
    end;
  if not GetComponents(cfComponent, Owner, CreateReaderDef, Callback) then
    if not GetComponents(cfComponents, Owner, CreateReaderDef, Callback) then
      Result := nil;
end;

function TClipboard.GetComponents(const Owner: TComponent;
  const Callback: TGetInstancesCallback<TComponent>): Boolean;
begin
  Result := GetComponents(cfComponents, Owner, CreateReaderDef, Callback) or
    GetComponents(cfComponent, Owner, CreateReaderDef, Callback);
end;

function TClipboard.GetComponents(const Owner: TComponent;
  const ReaderCreator: TFunc<TStream, TReader>; const Callback: TGetInstancesCallback<TComponent>): Boolean;
begin
  Result := GetComponents(cfComponents, Owner, ReaderCreator, Callback) or
    GetComponents(cfComponent, Owner, ReaderCreator, Callback);
end;

function TClipboard.GetComponents(const Format: TClipboardFormat;
  const Owner: TComponent; const Callback: TGetInstancesCallback<TComponent>): Boolean;
begin
  Result := GetComponents(Format, Owner, CreateReaderDef, Callback);
end;

function TClipboard.GetComponents(const Format: TClipboardFormat;
  const Owner: TComponent; const ReaderCreator: TFunc<TStream, TReader>;
  const Callback: TGetInstancesCallback<TComponent>): Boolean;
const
  FilerSignature: UInt32 = $30465054; //'TPF0'
var
  Signature: UInt32;
  CheckForMore: Boolean;
  Comp: TComponent;
  Reader: TReader;
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Result := SaveToStream(Format, Stream);
    if not Result then Exit;
    Stream.Seek(0, soBeginning);
    Reader := ReaderCreator(Stream);
    try
      CheckForMore := True;
      repeat
        Comp := Reader.ReadRootComponent(nil);
        try
          if Owner <> nil then
            Owner.InsertComponent(Comp);
          Callback(Comp, CheckForMore);
        except
          Comp.Free;
          raise;
        end;
        Reader.FlushBuffer;
        if CheckForMore then
          if (Stream.Read(Signature, SizeOf(Signature)) = SizeOf(Signature)) and
             (Signature = FilerSignature) then Stream.Seek(-SizeOf(Signature), soCurrent)
          else
            CheckForMore := False
      until not CheckForMore;
    finally
      Reader.Free;
    end;
  finally
    Stream.Free;
  end;
end;

function TClipboard.GetFileNames: TArray<string>;
begin
  if (FFileNamesCacheChangeCount <> ChangeCount) and (FReadWriteFileNamesIntf <> nil) then
  begin
    if not TryOpenForReading then Exit(nil);
    try
      FFileNamesCacheChangeCount := ChangeCount;
      FFileNamesCache := FReadWriteFileNamesIntf.ReadFileNames;
    finally
      Close;
    end;
  end;
  Result := Copy(FFileNamesCache);
end;

procedure TClipboard.GetFileNames(Strings: TStrings);
var
  FileNames: TArray<string>;
  S: string;
begin
  FileNames := GetFileNames;
  Strings.BeginUpdate;
  try
    Strings.Clear;
    for S in FileNames do
      Strings.Add(S)
  finally
    Strings.EndUpdate;
  end;
end;

procedure TClipboard.GetObjects<T>(const Callback: TLoadObjectsCallback<T>);
var
  Clipper: IObjectClipper<T>;
begin
  Clipper := GetObjectClipper<T>;
  if not TryOpenForReading then Exit;
  try
    Clipper.LoadFromClipboard(Self, Callback);
  finally
    Close;
  end;
end;

procedure TClipboard.GetObjects<T>(DestList: TObjectList<T>);
begin
  GetObjects<T>(
    procedure (const AssignTo: TProc<T>; var LookForMore: Boolean)
    var
      NewItem: T;
    begin
      NewItem := T.Create;
      DestList.Add(NewItem);
      AssignTo(NewItem);
    end);
end;

procedure TClipboard.GetValues<T>(const Callback: TGetInstancesCallback<T>);
var
  Clipper: IRecordClipper<T>;
begin
  Clipper := GetRecordClipper<T>;
  if not TryOpenForReading then Exit;
  try
    Clipper.LoadFromClipboard(Self, Callback);
  finally
    Close;
  end;
end;

procedure TClipboard.GetValues<T>(DestList: TList<T>);
begin
  GetValues<T>(
    procedure (const Value: T; var LookForMore: Boolean)
    begin
      DestList.Add(Value);
    end);
end;

function TClipboard.GetValues<T>: TArray<T>;
var
  List: TList<T>;
begin
  List := TList<T>.Create;
  try
    GetValues<T>(List);
    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

function TClipboard.GetText: TArray<string>;
begin
  if not TryOpenForReading then Exit(nil);
  try
    Result := FCore.ReadPlainText;
  finally
    Close;
  end;
end;

procedure TClipboard.GetText(Strings: TStrings);
var
  S: string;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    for S in GetText do
      Strings.Add(S);
  finally
    Strings.Free;
  end;
end;

function TClipboard.GetURLs: TArray<string>;
begin
  if (FURLsCacheChangeCount <> ChangeCount) and (FReadWriteURLIntf <> nil) then
  begin
    if not TryOpenForReading then Exit(nil);
    try
      FURLsCacheChangeCount := ChangeCount;
      FURLsCache := FReadWriteURLIntf.ReadURLs;
    finally
      Close;
    end;
  end;
  Result := Copy(FURLsCache);
end;

procedure TClipboard.GetURLs(Strings: TStrings);
var
  S: string;
  URLs: TArray<string>;
begin
  URLs := GetURLs;
  Strings.BeginUpdate;
  try
    Strings.Clear;
    for S in URLs do
      Strings.Add(S)
  finally
    Strings.EndUpdate;
  end;
end;

function TClipboard.GetVirtualFileDescriptors: TArray<string>;
begin
  if (FVirtualFileDescriptorsCacheChangeCount <> ChangeCount) and (FReadWriteVirtualFileIntf <> nil) then
  begin
    if not TryOpenForReading then Exit(nil);
    try
      FVirtualFileDescriptorsCacheChangeCount := ChangeCount;
      FVirtualFileDescriptorsCache := FReadWriteVirtualFileIntf.ReadVirtualFileDescriptors;
    finally
      Close;
    end;
  end;
  Result := FVirtualFileDescriptorsCache;
end;

procedure TClipboard.GetVirtualFileDescriptors(Strings: TStrings);
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    Strings.AddStrings(GetVirtualFileDescriptors);
  finally
    Strings.EndUpdate;
  end;
end;

procedure TClipboard.EnumVirtualFiles(const Callback: TEnumVirtualFilesStreamCallback);
begin
  if TryOpenForReading then
  try
    if FReadWriteVirtualFileIntf <> nil then
      FReadWriteVirtualFileIntf.ReadVirtualFiles(Callback);
  finally
    Close;
  end;
end;

procedure TClipboard.EnumVirtualFiles(const Callback: TEnumVirtualFileBytesCallback);
begin
  EnumVirtualFiles(
    procedure (const Descriptor: string; const SaveToStream: TProc<TStream>; var LookForMore: Boolean)
    begin
      Callback(Descriptor, StreamRendererToBytesRenderer(SaveToStream), LookForMore);
    end);
end;

procedure TClipboard.SaveVirtualFiles(const Callback: TSaveVirtualFilesCallback);
begin
  if TryOpenForReading then
  try
    if FReadWriteVirtualFileIntf <> nil then
      FReadWriteVirtualFileIntf.SaveVirtualFiles(Callback)
  finally
    Close;
  end;
end;

procedure TClipboard.SaveVirtualFiles(const Directory: string; SavedFiles: TStrings);
var
  Dir: string;
begin
  Dir := ExcludeTrailingPathDelimiter(Directory);
  if not DirectoryExists(Dir) then
    raise EClipboardException.CreateResFmt(@SDestinationDirectoryDoesNotExist, [Directory]);
  SavedFiles.BeginUpdate;
  try
    SavedFiles.Clear;
    SaveVirtualFiles(
      procedure (const Descriptor: string; const SaveToFile: TSaveVirtualFileFunc;
        var ContineSaving: Boolean)
      begin
        SavedFiles.Add(SaveToFile(Dir));
      end);
  finally
    SavedFiles.EndUpdate;
  end;
end;

procedure TClipboard.SaveVirtualFiles(const Directory: string);
var
  Dir: string;
begin
  Dir := ExcludeTrailingPathDelimiter(Directory);
  if not DirectoryExists(Dir) then
    raise EClipboardException.CreateResFmt(@SDestinationDirectoryDoesNotExist, [Directory]);
  SaveVirtualFiles(
    procedure (const Descriptor: string; const SaveToFile: TSaveVirtualFileFunc;
      var ContineSaving: Boolean)
    begin
      SaveToFile(Dir);
    end);
end;

function TClipboard.GetAsRTF: string;
begin
  Result := StringOf(GetBytes(cfRTF))
end;

function TClipboard.GetAsURL: string;
var
  URLs: TArray<string>;
begin
  URLs := GetURLs;
  if URLs <> nil then
    Result := URLs[0]
  else
    Result := '';
end;

function TClipboard.SaveToStream(const Format: TClipboardFormat; Stream: TStream): Boolean;
var
  Bytes: TBytes;
begin
  if FReadWriteBytesIntf = nil then
    CheckInterfaceAssigned(FReadWriteStreamIntf);
  if not TryOpenForReading then Exit(False);
  try
    if FReadWriteStreamIntf <> nil then
      Result := FReadWriteStreamIntf.ReadStream(Format, Stream)
    else 
    begin
      Bytes := FReadWriteBytesIntf.ReadBytes(Format);
      Result := (Bytes <> nil);
      if Result then Stream.Write(Bytes[0], Length(Bytes));
    end;
  finally
    Close;
  end;
end;

procedure WriteChars(var SeekPtr: PChar; const S: string); inline;
begin
  Move(Pointer(S)^, SeekPtr^, Length(S) * SizeOf(Char));
  Inc(SeekPtr, Length(S));
end;

function TClipboard.ToString: string;
var
  DelimiterStr: string;
  I, TotalLen: Integer;
  SeekPtr: PChar;
  Strings: TArray<string>;
begin
  Strings := GetText;
  case Length(Strings) of
    0: Result := '';
    1: Result := Strings[0];
  else
    DelimiterStr := SLineBreak;
    TotalLen := Length(Strings[0]);
    for I := 1 to High(Strings) do
      Inc(TotalLen, Length(DelimiterStr) + Length(Strings[I]));
    SetLength(Result, TotalLen);
    SeekPtr := PChar(Result);
    WriteChars(SeekPtr, Strings[0]);
    for I := 1 to High(Strings) do
    begin
      WriteChars(SeekPtr, DelimiterStr);
      WriteChars(SeekPtr, Strings[I]);
    end;
  end;
end;
{$ENDREGION}

{$REGION 'TClipboard - writing'}

procedure TClipboard.DoAssignBytes(UserAssignDelayed: Boolean;
  const Format: TClipboardFormat; const Renderer: TFunc<TBytes>);
begin
  if FReadWriteStreamIntf = nil then
    CheckInterfaceAssigned(FReadWriteBytesIntf);
  Add(procedure
      begin
        if FReadWriteBytesIntf <> nil then
          FReadWriteBytesIntf.WriteBytes(Format, Renderer, UserAssignDelayed)
        else
          FReadWriteStreamIntf.WriteStream(Format,
            BytesRendererToStreamRenderer(Renderer), UserAssignDelayed);
        if UserAssignDelayed then Inc(FUserAssignDelayedCount);
      end);
end;

procedure TClipboard.DoAssignText(UserAssignDelayed: Boolean; const Renderer: TFunc<string>);
begin
  Add(procedure
      begin
        FCore.WritePlainText(Renderer, UserAssignDelayed);
        if UserAssignDelayed then Inc(FUserAssignDelayedCount);
      end);
end;

procedure TClipboard.DoAssignURL(UserAssignDelayed: Boolean; const Renderer: TFunc<string>);
begin
  CheckInterfaceAssigned(FReadWriteURLIntf);
  Add(procedure
      begin
        FReadWriteURLIntf.WriteURL(Renderer, UserAssignDelayed);
        if UserAssignDelayed then Inc(FUserAssignDelayedCount);
      end);
end;

procedure TClipboard.DoAssignVirtualFile(UserAssignDelayed: Boolean;
  const Details: TVirtualFileDetails; const Renderer: TProc<TStream>);
var
  DetailsPtr: ^TVirtualFileDetails;
begin
  CheckInterfaceAssigned(FReadWriteVirtualFileIntf);
  DetailsPtr := @Details;
  Add(procedure
      begin
        FReadWriteVirtualFileIntf.WriteVirtualFile(DetailsPtr^, Renderer, UserAssignDelayed);
        if UserAssignDelayed then Inc(FUserAssignDelayedCount);
      end);
end;

procedure TClipboard.DoAssignVirtualFile<T>(UserDelayed: Boolean; 
  const Details: TVirtualFileDetails; const ObjGetter: TFunc<T>);
begin
  Open;
  try
    DoAssignVirtualFile(UserDelayed, Details, 
      procedure (Stream: TStream)
      begin
        ObjGetter.SaveToStream(Stream);
      end);
  finally
    Close;
  end;
end;

procedure TClipboard.DoAssignVirtualTextFile(UserDelayed: Boolean; 
  Details: TVirtualFileDetails; const Renderer: TFunc<string>;
  Encoding: TEncoding; Options: TVirtualTextFileOptions);
var
  BOM: TBytes;
  TextGetter: TFunc<string>;
begin
  if Details.FileName = '' then
    raise EArgumentException.CreateRes(@SFileNameCannotBeEmpty);
  CheckInterfaceAssigned(FReadWriteVirtualFileIntf);
  if Encoding = nil then
    Encoding := TEncoding.Default;
  if not (tfNeverOutputBOM in Options) then
    BOM := Encoding.GetPreamble;
  TextGetter := TCachedFunc<string>.Create(Renderer);
  if not UserDelayed then
    Details.Size := Length(BOM) + Encoding.GetByteCount(TextGetter);
  Open;
  try
    DoAssignVirtualFile(UserDelayed, Details,
      procedure (Stream: TStream)
      var
        Data: TBytes;
      begin
        if BOM <> nil then Stream.WriteBuffer(BOM[0], Length(BOM));
        Data := Encoding.GetBytes(TextGetter);
        if Data <> nil then Stream.WriteBuffer(Data[0], Length(Data));
      end);
  finally
    Close;
  end;
end;

procedure TClipboard.Assign<T>(const Source: T);
var
  Clipper: IRecordClipper<T>;
  Getter: TFunc<T>; { XE2/XE3 really don't like nested anonymous methods... }
begin
  Clipper := GetRecordClipper<T>;
  Getter :=
    function : T
    begin
      Result := Source
    end;
  Add(procedure
      begin
        Clipper.SaveToClipboard(Self, False, Getter)
      end);
end;

procedure TClipboard.Assign(const Source: TObject);
var
  Clipper: ICustomClipper;
begin
  if Source = nil then
  begin
    Clear;
    Exit;
  end;
  Clipper := GetClipper(Source.ClassType);
  Add(procedure
      var
        SavedUserAssignDelayedCount: Integer;
      begin
        SavedUserAssignDelayedCount := FUserAssignDelayedCount;
        try
          ICustomClipper<TObject>(Clipper).SaveToClipboard(Self, False,
            function : TObject
            begin
              Result := Source;
            end)
        finally
          FUserAssignDelayedCount := SavedUserAssignDelayedCount;
        end;
      end);
end;

procedure TClipboard.Assign(Source: TPersistent);
begin
  if (Source <> nil) and HasClipper(Source.ClassType) then
    Assign(TObject(Source))
  else
    inherited;
end;

type
  TBytesStreamAccess = class(TBytesStream);

procedure TClipboard.LoadFromStream(const Format: TClipboardFormat; Stream: TStream);
var
  Bytes: TBytes;
  Pos: Int64;
begin
  Pos := Stream.Position;
  if (Stream is TBytesStream) and (Pos = 0) and (TBytesStreamAccess(Stream).Capacity = Stream.Size) then
  begin
    Bytes := TBytesStreamAccess(Stream).Bytes;
    Stream.Seek(0, soEnd);
  end
  else
  begin
    SetLength(Bytes, Stream.Size - Pos);
    Stream.ReadBuffer(PByte(Bytes)^, Length(Bytes));
  end;
  if Bytes <> nil then Assign(Format, Bytes);
end;

procedure TClipboard.Assign(const Format: TClipboardFormat; const Source: TStream);
begin
  Source.Seek(0, soBeginning);
  LoadFromStream(Format, Source);
end;

procedure TClipboard.Assign(const Format: TClipboardFormat; const Source: IStreamPersist);
begin
  Assign(Format, BytesOf(Source));
end;

procedure TClipboard.Assign(const Format: TClipboardFormat; const Data: TBytes);
begin
  DoAssignBytes(False, Format,
    function : TBytes
    begin
      Result := Data;
    end);
end;

procedure TClipboard.Assign(const Format: TClipboardFormat; const Buffer; Size: Integer);
var
  Bytes: TBytes;
begin
  SetLength(Bytes, Size);
  if Size > 0 then Move(Buffer, Bytes[0], Size);
  Assign(Format, Bytes);
end;

procedure TClipboard.AssignDelayed(const Format: TClipboardFormat; const Renderer: TFunc<TBytes>);
begin
  DoAssignBytes(True, Format, Renderer);
end;

procedure TClipboard.AssignDelayed(const Format: TClipboardFormat; const Renderer: TProc<TStream>);
begin
  if FReadWriteBytesIntf = nil then
    CheckInterfaceAssigned(FReadWriteStreamIntf);
  Add(procedure
      begin
        if FReadWriteStreamIntf <> nil then
          FReadWriteStreamIntf.WriteStream(Format, Renderer, True)
        else
          FReadWriteBytesIntf.WriteBytes(Format, StreamRendererToBytesRenderer(Renderer), True);
        Inc(FUserAssignDelayedCount);
      end);
end;

procedure TClipboard.AssignDelayed<T>(const ObjGetter: TFunc<T>); 
begin
  Add(procedure
      var
        Clipper: ICustomClipper<T>;
        NewUserAssignDelayedCount: Integer;
      begin
        Clipper := GetClipper<T>;
        NewUserAssignDelayedCount := Succ(FUserAssignDelayedCount);
        Clipper.SaveToClipboard(Self, True, ObjGetter);
        FUserAssignDelayedCount := NewUserAssignDelayedCount;
      end);
end;

procedure TClipboard.AssignDelayed<T>(const Renderer: TProc<T>); 
var
  ObjGetter: TFunc<T>;
begin
  ObjGetter := TRenderedObject<T>.Create(Renderer);
  AssignDelayed<T>(ObjGetter);
end;

procedure TClipboard.AssignFile(const FileName: TFileName);
var
  CheckedFileName: TFileName;
begin
  CheckInterfaceAssigned(FReadWriteFileNamesIntf);
  CheckedFileName := ExpandAndValidateFileName(FileName);
  Add(procedure
      begin
        FReadWriteFileNamesIntf.WriteFileName(
          function : TFileName
          begin
            Result := CheckedFileName;
          end, False)
      end)
end;

procedure TClipboard.AssignFileDelayed(const Renderer: TFunc<TFileName>);
var
  CheckedRenderer: TFunc<TFileName>;
begin
  CheckInterfaceAssigned(FReadWriteFileNamesIntf);
  CheckedRenderer :=
    function : TFileName
    begin
      Result := ExpandAndValidateFileName(Renderer);
    end;
  Add(procedure
      begin
        FReadWriteFileNamesIntf.WriteFileName(CheckedRenderer, True);
        Inc(FUserAssignDelayedCount);
      end)
end;

procedure TClipboard.AssignText(const Value: string);
begin
  DoAssignText(False,
    function : string
    begin
      Result := Value;
    end);
end;

procedure TClipboard.AssignTextDelayed(const Renderer: TFunc<string>);
begin
  DoAssignText(True, Renderer);
end;

procedure TClipboard.AssignVirtualFile(const VirtualFileName, Text: string;
  Encoding: TEncoding; Options: TVirtualTextFileOptions);
begin
  DoAssignVirtualTextFile(False, TVirtualFileDetails.Create(VirtualFileName),
    function : string
    begin
      Result := Text
    end, Encoding, Options);
end;

procedure TClipboard.AssignVirtualFile(const Details: TVirtualFileDetails;
  const Text: string; Encoding: TEncoding; Options: TVirtualTextFileOptions);
begin
  DoAssignVirtualTextFile(False, Details,
    function : string
    begin
      Result := Text
    end, Encoding, Options);
end;

procedure TClipboard.AssignURL(const Value: string);
begin
  DoAssignURL(False,
    function : string
    begin
      Result := Value;
    end);
end;

procedure TClipboard.AssignURLDelayed(const Renderer: TFunc<string>);
begin
  DoAssignURL(True, Renderer);
end;

procedure TClipboard.AssignVirtualFile(const VirtualFileName: string; const Data: TBytes);
begin
  AssignVirtualFile(TVirtualFileDetails.Create(VirtualFileName), Data);
end;

procedure TClipboard.AssignVirtualFile(Details: TVirtualFileDetails; const Data: TBytes);
begin
  if Data = nil then Exit;
  Details.Size := Length(Data);
  DoAssignVirtualFile(False, Details,
    procedure (Stream: TStream)
    begin
      Stream.WriteBuffer(Data[0], Length(Data));
    end);
end;

procedure TClipboard.AssignVirtualFile(const VirtualFileName: string; 
  const Source: IStreamPersist);
begin
  AssignVirtualFile(VirtualFileName, BytesOf(Source));
end;

procedure TClipboard.AssignVirtualFile(const Details: TVirtualFileDetails; 
  const Source: IStreamPersist);
begin
  AssignVirtualFile(Details, BytesOf(Source));
end;

procedure TClipboard.AssignVirtualFileDelayed(const Details: TVirtualFileDetails;
  const Renderer: TProc<TStream>);
begin
  DoAssignVirtualFile(True, Details, Renderer);
end;

procedure TClipboard.AssignVirtualFileDelayed(const Details: TVirtualFileDetails;
  const Renderer: TFunc<TBytes>);
begin
  DoAssignVirtualFile(True, Details, BytesRendererToStreamRenderer(Renderer));
end;

procedure TClipboard.AssignVirtualFileDelayed(const VirtualFileName: string;
  const Renderer: TProc<TStream>);
begin
  DoAssignVirtualFile(True, TVirtualFileDetails.Create(VirtualFileName), Renderer);
end;

procedure TClipboard.AssignVirtualFileDelayed(const VirtualFileName: string;
  const Renderer: TFunc<TBytes>);
begin
  DoAssignVirtualFile(True, TVirtualFileDetails.Create(VirtualFileName),
    BytesRendererToStreamRenderer(Renderer));
end;

procedure TClipboard.AssignVirtualFileDelayed<T>(const VirtualFileName: string;
  const ObjGetter: TFunc<T>);
begin
  DoAssignVirtualFile<T>(True, TVirtualFileDetails.Create(VirtualFileName), ObjGetter);
end;

procedure TClipboard.AssignVirtualFileDelayed<T>(const Details: TVirtualFileDetails;
  const ObjGetter: TFunc<T>);
begin
  DoAssignVirtualFile<T>(True, Details, ObjGetter);
end;

procedure TClipboard.AssignVirtualFileDelayed<T>(const VirtualFileName: string;
  const Renderer: TProc<T>);
begin
  AssignVirtualFileDelayed<T>(TVirtualFileDetails.Create(VirtualFileName), Renderer);
end;

procedure TClipboard.AssignVirtualFileDelayed<T>(const Details: TVirtualFileDetails;
  const Renderer: TProc<T>);
var
  ObjGetter: TFunc<T>;
begin
  ObjGetter := TRenderedObject<T>.Create(Renderer);
  DoAssignVirtualFile<T>(True, Details, ObjGetter);
end;

procedure TClipboard.AssignVirtualFileDelayed(const VirtualFileName: string;
  const Renderer: TFunc<string>; Encoding: TEncoding; Options: TVirtualTextFileOptions);
begin
  DoAssignVirtualTextFile(True, TVirtualFileDetails.Create(VirtualFileName),
    Renderer, Encoding, Options);
end;

procedure TClipboard.AssignVirtualFileDelayed(const Details: TVirtualFileDetails;
  const Renderer: TFunc<string>; Encoding: TEncoding; Options: TVirtualTextFileOptions);
begin
  DoAssignVirtualTextFile(True, Details, Renderer, Encoding, Options);
end;

procedure TClipboard.FlushComponents;
var
  Pair: TPair<TClipboardFormat, TBytesStream>;
  SavedAssignedFormatCount: Integer;
begin
  SavedAssignedFormatCount := FAssignedFormatCount;
  try
    for Pair in FComponentStreams do
    begin
      Pair.Value.SetSize(Pair.Value.Position);
      Assign(Pair.Key, Pair.Value.Bytes);
    end;
  finally
    FAssignedFormatCount := SavedAssignedFormatCount;
    FComponentStreams.Clear;
  end;
end;

procedure TClipboard.SetComponent(const Component: TComponent);
begin
  SetComponent(cfComponent, Component);
end;

procedure TClipboard.SetComponent(const Format: TClipboardFormat; const Component: TComponent);
var
  Stream, OtherStream: TBytesStream;
begin
  if not FComponentStreams.TryGetValue(Format, Stream) then
  begin
    Stream := TBytesStream.Create;
    FComponentStreams.Add(Format, Stream);
  end
  else
    if Format = cfComponent then
    begin
      OtherStream := Stream;
      if not FComponentStreams.TryGetValue(cfComponents, Stream) then
      begin
        Stream := TBytesStream.Create;
        FComponentStreams.Add(cfComponents, Stream);
        Stream.WriteBuffer(OtherStream.Memory^, OtherStream.Size);
      end;
    end;
  Stream.WriteComponent(Component);
  Inc(FAssignedFormatCount);
end;

class procedure TClipboard.CheckIsRTF(const Str: string);
begin
  if Pos(RTFStringSig, Str) = 0 then
    raise EClipboardException.CreateRes(@SRTFExpected);
end;

class procedure TClipboard.CheckIsRTF(const Bytes: TBytes);
var
  GotIt: Boolean;
  I: Integer;
begin
  if RTFBytesSig = nil then RTFBytesSig := BytesOf(RTFStringSig);
  GotIt := False;
  for I := 0 to Min(High(Bytes) - Length(RTFBytesSig), 20) do
    if CompareMem(@Bytes[I], RTFBytesSig, Length(RTFBytesSig)) then
    begin
      GotIt := True;
      Break;
    end;
  if not GotIt then
    raise EClipboardException.CreateRes(@SRTFExpected);
end;

procedure TClipboard.AssignRTF(const Value: string);
begin
  CheckIsRTF(Value);
  Assign(cfRTF, BytesOf(Value));
end;

procedure TClipboard.AssignRTF(const RTFBytes: TBytes);
begin
  CheckIsRTF(RTFBytes);
  Assign(cfRTF, RTFBytes);
end;

procedure TClipboard.AssignRTFDelayed(const Renderer: TFunc<string>);
begin
  AssignDelayed(cfRTF,
    function : TBytes
    var
      Data: string;
    begin
      Data := Renderer;
      CheckIsRTF(Data);
      Result := BytesOf(Data);
    end);
end;

procedure TClipboard.AssignRTFDelayed(const Renderer: TFunc<TBytes>);
begin
  AssignDelayed(cfRTF,
    function : TBytes
    begin
      Result := Renderer;
      CheckIsRTF(Result);
    end);
end;

procedure TClipboard.AssignRTFDelayed(const Renderer: TProc<TStream>);
begin
  AssignDelayed(cfRTF, Renderer);
end;
{$ENDREGION}

{$REGION 'TClipboard.XXXOutstandingPromisesToOS'}

function TClipboard.HasOutstandingPromisesToOS: Boolean;
begin
  Result := (FDelayedRenderingIntf <> nil) and FDelayedRenderingIntf.HasOutstandingPromisesToOS;
end;

procedure TClipboard.CancelOutstandingPromisesToOS;
begin
  if HasOutstandingPromisesToOS then
    FDelayedRenderingIntf.CancelOutstandingPromisesToOS;
end;

procedure TClipboard.ResolveOutstandingPromisesToOS;
begin
  ResolveOutstandingPromisesToOS(True);
end;

procedure TClipboard.ResolveOutstandingPromisesToOS(ExplicitlyRequested: Boolean);
begin
  if HasOutstandingPromisesToOS then
    FDelayedRenderingIntf.ResolveOutstandingPromisesToOS(ExplicitlyRequested);
end;
{$ENDREGION}

{$REGION 'TClipboard - drag and drop'}

class function TClipboard.GetOnGetDragData(const DraggedObject: TObject): TAssignClipboardForDragEvent;
begin
  if not FOnGetDragData.TryGetValue(Pointer(DraggedObject), Result) then
    Result := nil;
end;

class procedure TClipboard.SetOnGetDragData(const DraggedObject: TObject;
  const Handler: TAssignClipboardForDragEvent);
begin
  if Assigned(Handler) then
  begin
    FOnGetDragData.AddOrSetValue(Pointer(DraggedObject), Handler);
    Clipboard.AddFreeNotification(DraggedObject);
  end
  else
  begin
    FOnGetDragData.Remove(Pointer(DraggedObject));
    Clipboard.RemoveFreeNotification(DraggedObject);
  end;
end;

class function TClipboard.CreateCombinedGetDragDataCallback(
  const OnGetDataOverride: TAssignClipboardForDragEvent = nil;
  const OnGetDataDefault: TProc = nil): TAssignClipboardForDragEvent;
begin
  Result :=
    procedure (DraggedObject: TObject; Clipboard: TClipboard)
    var
      OnGetData: TAssignClipboardForDragEvent;
    begin
      Clipboard.Open;
      try
        { always call OnStartDrag }
        if Assigned(FOnStartDrag) then
          FOnStartDrag(DraggedObject, Clipboard);
        { call the first one of the following that isn't nil: the explicitly-passed callback
          (if any); OnGetData array property; framework-defined default data getter }
        if Assigned(OnGetDataOverride) then
          OnGetDataOverride(DraggedObject, Clipboard)
        else if FOnGetDragData.TryGetValue(Pointer(DraggedObject), OnGetData) then
          OnGetData(DraggedObject, Clipboard)
        else if Assigned(OnGetDataDefault) then
          OnGetDataDefault();
      finally
        Clipboard.Close;
      end;
      if Clipboard.Empty then
        raise EClipboardException.CreateRes(@SNoDataAssignedToDrag);
    end;
end;

class procedure TClipboard.SetObjectBeingDragged(const Obj: TObject);
begin
  FObjectBeingDragged := Obj;
end;
{$ENDREGION}

{$REGION 'TUserMemoryStream'}

constructor TUserMemoryStream.Create(AMemory: Pointer; const ALength: NativeInt);
begin
  inherited Create;
  SetPointer(AMemory, ALength);
end;

function TUserMemoryStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := 0;
end;
{$ENDREGION}

{$REGION 'TCachedFunc<T>'}

constructor TCachedFunc<T>.CreateInternal(const CoreRenderer: TFunc<T>);
begin
  inherited Create;
  FCoreRenderer := CoreRenderer;
end;

class function TCachedFunc<T>.Create(const CoreRenderer: TFunc<T>): TFunc<T>;
begin
  if Supports(PUnknown(@CoreRenderer)^, ICachedFunc<T>, Result) then Exit;
  Result := CreateInternal(CoreRenderer);
end;

function TCachedFunc<T>.Invoke: T;
begin
  if not FRendered then
  begin
    FRendered := True;
    FValue := FCoreRenderer;
  end;
  Result := FValue;
end;
{$ENDREGION}

{$REGION 'TRenderedObject<T>'}

class function TRenderedObject<T>.Create(const Renderer: TProc<T>): TFunc<T>; 
begin
  Result := inherited Create(
    function : T
    begin
      Result := T.Create;
      Renderer(Result);
    end);
end;

{$IFNDEF AUTOREFCOUNT}
destructor TRenderedObject<T>.Destroy;
begin
  if Rendered then Invoke.Free;
  inherited;
end;
{$ENDIF}
{$ENDREGION}

{$REGION 'Renderer adapters'}

function BytesToStreamRenderer(const Bytes: TBytes): TProc<TStream>;
begin
  Result :=
    procedure (Stream: TStream)
    begin
      Stream.WriteBuffer(PByte(Bytes)^, Length(Bytes));
    end;
end;

function BytesRendererToStreamRenderer(const RendererToWrap: TFunc<TBytes>): TProc<TStream>;
var
  BytesGetter: TFunc<TBytes>;
begin
  BytesGetter := TCachedFunc<TBytes>.Create(RendererToWrap);
  Result :=
    procedure (Stream: TStream)
    var
      Bytes: TBytes;
    begin
      Bytes := BytesGetter;
      Stream.WriteBuffer(PByte(Bytes)^, Length(Bytes));
    end;
end;

function StreamRendererToBytesRenderer(const RendererToWrap: TProc<TStream>): TFunc<TBytes>;
begin
  Result := TCachedFunc<TBytes>.Create(
    function : TBytes
    var
      Stream: TBytesStream;
    begin
      Stream := TBytesStream.Create;
      try
        RendererToWrap(Stream);
        Stream.SetSize(Stream.Position); //this forces Capacity to match Size
        Result := Stream.Bytes;
      finally
        Stream.Free;
      end;
    end);
end;

function BytesOf(const Obj: IStreamPersist): TBytes;
var
  Stream: TBytesStream;
begin
  if Obj = nil then Exit(nil);
  Stream := TBytesStream.Create;
  try
    Obj.SaveToStream(Stream);
    Stream.SetSize(Stream.Position); //this forces Capacity to match Size
    Result := Stream.Bytes;
  finally
    Stream.Free;
  end;
end;
{$ENDREGION}

{$REGION 'TObjectClipper<T>'}

constructor TObjectClipper<T>.Create(const ReadFormats, WrittenFormats: array of TClipboardFormat);
begin
  inherited Create;
  FReadFormats := TClipboard.CreateDynArray<TClipboardFormat>(ReadFormats);
  FWrittenFormats := TClipboard.CreateDynArray<TClipboardFormat>(WrittenFormats);
end;

function TObjectClipper<T>.CanLoadFromClipboard(const Clipboard: TClipboard): Boolean;
begin
  Result := Clipboard.HasFormat(FReadFormats) or CanLoadFromClipboardFile(Clipboard);
end;

function TObjectClipper<T>.CanLoadFromClipboardFile(const Clipboard: TClipboard): Boolean;
begin
  Result := False;
end;

procedure TObjectClipper<T>.LoadFromClipboard(const Clipboard: TClipboard;
  const Callback: TLoadObjectsCallback<T>);
var
  Format: TClipboardFormat;
  LookForFiles: Boolean;
begin
  LookForFiles := True;
  if Clipboard.HasFormat(FReadFormats, Format) then
    LoadFromClipboardFormat(Clipboard, Format,
      procedure (const AssignTo: TProc<T>; var LookForMore: Boolean)
      begin
        Callback(AssignTo, LookForMore);
        LookForFiles := LookForMore;
      end);
  if LookForFiles then
    LoadFromClipboardFiles(Clipboard, Callback);
end;

procedure TObjectClipper<T>.LoadFromClipboardFiles(const Clipboard: TClipboard;
  const Callback: TLoadObjectsCallback<T>);
begin
  //do nothing - hook for TStreamableObjectClipper
end;

procedure TObjectClipper<T>.SaveToClipboard(const Clipboard: TClipboard;
  PreferDelayed: Boolean; const ObjGetter: TFunc<T>);
var
  Format: TClipboardFormat;
begin
  for Format in FWrittenFormats do
    SaveToClipboardFormat(Clipboard, Format, PreferDelayed, ObjGetter);
end;
{$ENDREGION}

{$REGION 'TClipping and TPersistentClipper<T>'}

class constructor TClipping.InitializeClass;
begin
  FFormatSettings := TFormatSettings.Create('en-us');
end;

constructor TClipping.Create(const ObjToWrap: TPersistent);
begin
  inherited Create(nil);
  FDateTime := FormatDateTime('yyyy-mm-dd"T"hh:mm:ss', Now, FFormatSettings); //XML format
  FRTLVersion := System.RTLVersion;
  FClippedClass := ObjToWrap.QualifiedClassName;
  FOriginator := ExtractFileName(GetModuleName(0));
  FObj := ObjToWrap;
  FSetSubComponent := (FObj is TComponent) and
    not (csSubComponent in TComponent(FObj).ComponentStyle);
  if FSetSubComponent then TComponent(FObj).SetSubComponent(True);
end;

destructor TClipping.Destroy;
begin
  if FSetSubComponent then TComponent(FObj).SetSubComponent(False);
  inherited;
end;

procedure TClipping.SetObj(const Value: TPersistent);
begin
  FObj.Assign(Value);
end;

constructor TPersistentClipper<T>.Create(const Format: TClipboardFormat);
begin
  inherited Create([Format], [Format]);
  RegisterClass(T);
end;

function TPersistentClipper<T>.GetOnBeginRead: TProc<TReader>;
begin
  Result := FOnBeginRead;
end;

function TPersistentClipper<T>.GetOnEndRead: TProc<TClipping>;
begin
  Result := FOnEndRead;
end;

procedure TPersistentClipper<T>.SetOnBeginRead(const NewHandler: TProc<TReader>);
begin
  FOnBeginRead := NewHandler;
end;

procedure TPersistentClipper<T>.SetOnEndRead(const NewHandler: TProc<TClipping>);
begin
  FOnEndRead := NewHandler;
end;

function TPersistentClipper<T>.GetFormat: TClipboardFormat;
begin
  Result := ReadFormats[0];
end;

procedure TPersistentClipper<T>.BeginRead(Reader: TReader);
begin
  if Assigned(FOnBeginRead) then
    FOnBeginRead(Reader);
end;

procedure TPersistentClipper<T>.EndRead(Clipping: TClipping);
begin
  if Assigned(FOnEndRead) then
    FOnEndRead(Clipping);
end;

function TPersistentClipper<T>.CreateBlankObject: T;
begin
  Result := T.Create;
end;

procedure TPersistentClipper<T>.LoadFromClipboardFormat(const Clipboard: TClipboard;
  const Format: TClipboardFormat; const Callback: TLoadObjectsCallback<T>);
begin
  Clipboard.GetBytes(Format,
    procedure (const Bytes: TBytes; var LookForMore: Boolean)
    begin
      Callback(
        procedure (Dest: T)
        var
          Reader: TReader;
          Stream: TBytesStream;
          Clipping: TClipping;
        begin
          Clipping := nil;
          Reader := nil;
          Stream := TBytesStream.Create(Bytes);
          try
            Clipping := TClipping.Create(Dest);
            Reader := TReader.Create(Stream, 4096);
            BeginRead(Reader);
            Reader.ReadRootComponent(Clipping);
            EndRead(Clipping);
          finally
            Reader.Free;
            Stream.Free;
            Clipping.Free;
          end;
        end,
        LookForMore);
    end);
end;

procedure TPersistentClipper<T>.SaveToClipboardFormat(Clipboard: TClipboard;
  const Format: TClipboardFormat; PreferDelayed: Boolean; const ObjGetter: TFunc<T>);
var
  BytesGetter: TFunc<TBytes>;
begin
  BytesGetter :=
    function : TBytes
    var
      Stream: TBytesStream;
      Wrapper: TClipping;
    begin
      Wrapper := nil;
      Stream := TBytesStream.Create;
      try
        Wrapper := TClipping.Create(ObjGetter);
        Stream.WriteComponent(Wrapper);
        Stream.SetSize(Stream.Position);
        Result := Stream.Bytes;
      finally
        Stream.Free;
        Wrapper.Free;
      end;
    end;
  if PreferDelayed then
    Clipboard.AssignDelayed(Format, BytesGetter)
  else
    Clipboard.Assign(Format, BytesGetter());
end;
{$ENDREGION}

{$REGION 'TStreamableObjectClipper<T>'}

class function TStreamableObjectClipper<T>.ObjAsIStreamPersist(const Obj: TObject): IStreamPersist;
begin
  if not Obj.GetInterface(IStreamPersist, Result) then
    raise EIntfCastError.CreateRes(@SIntfCastError);
end;

constructor TStreamableObjectClipper<T>.Create(const Formats: array of TClipboardFormat;
  const FileExts: array of string);
begin
  Create(Formats, Formats, FileExts);
end;

constructor TStreamableObjectClipper<T>.Create(const ReadFormats, WrittenFormats: array of TClipboardFormat;
  const FileExts: array of string);
begin
  inherited Create(ReadFormats, WrittenFormats);
  FFileExts := TClipboard.CreateDynArray<string>(FileExts);
end;

function TStreamableObjectClipper<T>.CanLoadFromClipboardFile(const Clipboard: TClipboard): Boolean;
var
  Descriptor: string;
begin
  if FileExts = nil then Exit(False);
  if MatchFileExt(Clipboard.GetFileNames, FFileExts) then Exit(True);
  for Descriptor in Clipboard.GetVirtualFileDescriptors do
    if IsCompatibleVirtualFile(Descriptor) then Exit(True);
  Result := False;
end;

function TStreamableObjectClipper<T>.IsCompatibleVirtualFile(const Descriptor: string): Boolean;
begin
  Result := TClipboard.CoreClass.IsCompatibleVirtualFileDescriptor(Descriptor, FFileExts)
end;

procedure TStreamableObjectClipper<T>.LoadFromClipboardFiles(const Clipboard: TClipboard;
  const Callback: TLoadObjectsCallback<T>);
var
  AssignToProc: TProc<T>;
  FileName: string;
  LookForMore: Boolean;
  SaveToStreamProc: TProc<TStream>;
begin
  if FileExts = nil then Exit;
  AssignToProc :=
    procedure (Dest: T)
    var
      Stream: TFileStream;
    begin
      Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
      try
        LoadFromStream(Dest, Stream);
      finally
        Stream.Free;
      end;
    end;
  LookForMore := True;
  for FileName in Clipboard.GetFileNames do
    if MatchFileExt(FileName, FFileExts) then
    begin
      Callback(AssignToProc, LookForMore);
      if not LookForMore then Exit;
    end;
  AssignToProc :=
    procedure (Dest: T)
    var
      Stream: TMemoryStream;
    begin
      Stream := TMemoryStream.Create;
      try
        SaveToStreamProc(Stream);
        Stream.Position := 0;
        LoadFromStream(Dest, Stream);
      finally
        Stream.Free;
      end;
    end;
  Clipboard.EnumVirtualFiles(
    procedure (const Descriptor: string; const SaveToStream: TProc<TStream>;
      var LookForMore: Boolean)
    begin
      if not IsCompatibleVirtualFile(Descriptor) then Exit;
      { leaving aside any stylistic preference for not nesting anonymous methods,
        nesting the assign to proc definition here causes an internal compiler
        error in XE2 and XE3 }
      SaveToStreamProc := SaveToStream;
      Callback(AssignToProc, LookForMore);
    end);
  AssignToProc := nil;
end;

procedure TStreamableObjectClipper<T>.LoadFromClipboardFormat(
  const Clipboard: TClipboard; const Format: TClipboardFormat;
  const Callback: TLoadObjectsCallback<T>);
var
  AssignToProc: TProc<T>;
  BytesToRead: TBytes;
begin
  AssignToProc :=
    procedure (Dest: T)
    var
      Stream: TBytesStream;
    begin
      Stream := TBytesStream.Create(BytesToRead);
      try
        LoadFromStream(Dest, Stream);
      finally
        Stream.Free;
      end;
    end;
  Clipboard.GetBytes(Format,
    procedure (const Bytes: TBytes; var LookForMore: Boolean)
    begin
      BytesToRead := Bytes;
      Callback(AssignToProc, LookForMore);
    end);
  AssignToProc := nil; //break reference cycle
end;

procedure TStreamableObjectClipper<T>.LoadFromStream(Obj: T; const Stream: TStream);
begin
  ObjAsIStreamPersist(Obj).LoadFromStream(Stream);
end;

procedure TStreamableObjectClipper<T>.SaveToStream(const Obj: T;
  const Format: TClipboardFormat; Stream: TStream);
begin
  ObjAsIStreamPersist(Obj).SaveToStream(Stream);
end;

procedure TStreamableObjectClipper<T>.SaveToClipboardFormat(Clipboard: TClipboard;
  const Format: TClipboardFormat; PreferDelayed: Boolean; const ObjGetter: TFunc<T>);
var
  Stream: TBytesStream;
begin
  if PreferDelayed then
    Clipboard.AssignDelayed(Format,
      procedure (Stream: TStream)
      begin
        SaveToStream(ObjGetter, Format, Stream);
      end)
  else
  begin
    Stream := TBytesStream.Create;
    try
      SaveToStream(ObjGetter, Format, Stream);
      Clipboard.Assign(Format, Stream);
    finally
      Stream.Free;
    end;
  end;
end;
{$ENDREGION}

{$REGION 'TDelegatedObjectClipper<T>'}

constructor TDelegatedObjectClipper<T>.Create(const ReadFormats, WrittenFormats: array of TClipboardFormat;
  const FileExts: array of string;
  const OnLoadFromClipboardFormat: TLoadFromClipboardFormatEvent<T>;
  const OnSaveToClipboardFormat: TSaveToClipboardFormatEvent<T>;
  const OnLoadFromStream: TLoadFromClipboardStreamEvent<T>;
  const OnSaveToStream: TSaveToClipboardStreamEvent<T>);
begin
  if (Length(FileExts) <> 0) and not (Assigned(OnLoadFromStream) and Assigned(OnSaveToStream)) and
    not Supports(TClass(T), IStreamPersist) then
    raise EArgumentException.CreateRes(@SNoCallbacksIStreamPersistNeeded);
  inherited Create(ReadFormats, WrittenFormats, FileExts);
  FOnLoadFromClipboardFormat := OnLoadFromClipboardFormat;
  FOnSaveToClipboardFormat := OnSaveToClipboardFormat;
  FOnLoadFromStream := OnLoadFromStream;
  FOnSaveToStream := OnSaveToStream;
end;

function TDelegatedObjectClipper<T>.CreateBlankObject: T;
begin
  Result := T.Create;
end;

procedure TDelegatedObjectClipper<T>.LoadFromClipboardFormat(const Clipboard: TClipboard;
  const Format: TClipboardFormat; const Callback: TLoadObjectsCallback<T>);
var
  LookForMore: Boolean;
begin
  LookForMore := True;
  if Assigned(FOnLoadFromClipboardFormat) then
    Callback(
      procedure (Dest: T)
      begin
        FOnLoadFromClipboardFormat(Clipboard, Format, Dest)
      end, LookForMore)
  else
    inherited;
end;

procedure TDelegatedObjectClipper<T>.LoadFromStream(ObjToLoad: T; const Stream: TStream);
begin
  if Assigned(FOnLoadFromStream) then
    FOnLoadFromStream(ObjToLoad, Stream)
  else
    inherited;
end;

procedure TDelegatedObjectClipper<T>.SaveToClipboardFormat(Clipboard: TClipboard; 
  const Format: TClipboardFormat; PreferDelayed: Boolean; const ObjGetter: TFunc<T>);
begin
  if Assigned(FOnSaveToClipboardFormat)then
    FOnSaveToClipboardFormat(Clipboard, Format, PreferDelayed, ObjGetter)
  else
    inherited;
end;

procedure TDelegatedObjectClipper<T>.SaveToStream(const ObjToSave: T;
  const Format: TClipboardFormat; Stream: TStream);
begin
  if Assigned(FOnSaveToStream) then
    FOnSaveToStream(ObjToSave, Format, Stream)
  else
    inherited;
end;
{$ENDREGION}

end.
