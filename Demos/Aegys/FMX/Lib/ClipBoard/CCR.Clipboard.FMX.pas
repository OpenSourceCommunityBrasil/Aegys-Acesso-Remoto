{**************************************************************************************}
{                                                                                      }
{ CCR.Clipboard - platform-agnostic FMX-specific extensions                            }
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

unit CCR.Clipboard.FMX;

interface

{$IFDEF NEXTGEN}
{$LEGACYIFEND ON}
{$ENDIF}
{$POINTERMATH ON}
{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Rtti,
  System.UITypes, FMX.Types, FMX.Controls, FMX.Forms, FMX.Platform,
  {$IF FireMonkeyVersion >= 19}FMX.Graphics,{$IFEND}
  {$IF FireMonkeyVersion >= 19}FMX.Surfaces,{$IFEND}
  CCR.Clipboard;

type
  { In the stock FMX drag and drop support, a TDataObject record is completed during every
    drag-over event. In this, the record's Source field is set to the source object on an
    internal drag and nil otherwise, the Files field to any file names being dragged, and
    the Data field to any text being dragged. Immediately requesting any text and file
    names like this subverts delay rendering however, and as such, the default behaviour of
    TClipboard-based dragging is to set Source to the TClipboard instance representing the
    data being dragged (if you want the source object in the case of an internal drag,
    read TClipboard's ObjectBeingDragged class property), and leave the other fields blank.
    If however you need the legacy behaviour, include LegacyDragObject in
    TClipboard.DragDropOptions; when doing so, the TClipboard instance being dragged can
    be accessed via the Clipboard property of the IClipboardDropInfo object returned by
    RegisterDropTarget. }
  TClipboardDragDropOption = (LegacyDragObject);
  TClipboardDragDropOptions = set of TClipboardDragDropOption;

  IFMXClipboardDragDropService = interface
  ['{E8FCD28A-62D4-4E8E-A675-52FC5367597B}']
    procedure DoDrag(const Source: TObject; const Form: TCommonCustomForm;
      const Bitmap: TBitmap; const PrepareClipboard: TProc<TClipboard>);
    function RegisterForm(const Form: TCommonCustomForm): IClipboardDropInfo;
    procedure UnregisterForm(const Form: TCommonCustomForm);
  end;

  TClipboardHelper = class helper for TClipboard
{$IF DECLARED(IFMXDragDropService)}
  strict private type
    TFreeNotifierCallback = procedure (AObject: TObject);
    TFreeNotifier = class(TInterfacedObject, IFreeNotification)
    strict private
      FCallback: TFreeNotifierCallback;
    strict protected
      procedure FreeNotification(AObject: TObject);
    public
      constructor Create(const ACallback: TFreeNotifierCallback);
    end;
  strict private class var
    FDragDropIntf: IFMXClipboardDragDropService;
    FDragDropOptions: TClipboardDragDropOptions;
    FFormFreeNotifier: IFreeNotification;
    FRegisteredDropTargets: TDictionary<Pointer, IClipboardDropInfo>;
    FSupportsDragAndDrop: TUncertainState;
    class procedure FormFreeNotifierCallback(Form: TObject); static;
    class procedure NeedDragDropService;
  protected
    class procedure Finalize;
    class property DragDropIntf: IFMXClipboardDragDropService read FDragDropIntf;
  public
    procedure InitializeDragObject(var DragObject: TDragObject); platform; //internal - do not use
  public
    procedure Assign(const DragObject: TDragObject); overload;
    class function SupportsDragAndDrop: Boolean;
    { where the application is the source of the drag... }
    class procedure BeginDrag(const Form: TCommonCustomForm; const Data: TDragObject;
      const DragImage: TBitmap; const OnGetData: TAssignClipboardForDragEvent = nil); overload;
    class procedure BeginDrag(const DragImage: TBitmap; const OnGetData: TProc<TClipboard>); overload;
    class procedure BeginDrag(const Source: TObject; const DragImage: TBitmap; const OnGetData: TAssignClipboardForDragEvent = nil); overload;
    class procedure BeginDrag(const Source: IControl); overload;
    class procedure BeginDrag(const Source: TControl; const OnGetData: TProc<TClipboard>); overload;
    { when the application is the target of the drop... }
    class function RegisterForDragAndDrop(const Form: TCommonCustomForm): IClipboardDropInfo;
    class property DragDropOptions: TClipboardDragDropOptions read FDragDropOptions write FDragDropOptions;
{$IFEND}
  end;

{$IF FireMonkeyVersion >= 19}
  TBitmap = FMX.Graphics.TBitmap;
  TBitmapData = FMX.Graphics.TBitmapData;
  TMapAccess = FMX.Graphics.TMapAccess;
{$IFEND}

{$IF NOT DECLARED(TMapAccess)}
  {$DEFINE TBitmapHelperNeeded}
  TPixelFormat = (pfUnknown, pfA8R8G8B8);

  TBitmapData = record
    Data: Pointer;
    Pitch: Integer;
    PixelFormat: TPixelFormat;
    Source: TBitmap;
    Width, Height: Integer;
    function GetScanline(Row: Integer): Pointer;
  end;

  TMapAccess = (Read, Write, ReadWrite);

  TBitmapHelper = class helper for TBitmap
    constructor Create; overload;
    function Map(const Access: TMapAccess; var Data: TBitmapData): Boolean;
    procedure Unmap(var Data: TBitmapData);
  end;
{$ELSE}
  {$IF FireMonkeyVersion < 18}
  TMapAccessHelper = record helper for TMapAccess
  const
    Read = TMapAccess.maRead;
    Write = TMapAccess.maWrite;
    ReadWrite = TMapAccess.maReadWrite;
  end;

  TBitmapHelper = class helper for TBitmap
    constructor Create; overload;
  end;
  {$DEFINE TBitmapDataHelperNeeded}
  TBitmapDataHelper = record helper for TBitmapData
    function GetScanline(Row: Integer): Pointer;
  end;
  {$IFEND}
{$IFEND}

{$IF NOT DECLARED(EBitmapFormatUnsupported)}
  EBitmapFormatUnsupported = class(Exception);
{$IFEND}
{$IF NOT DECLARED(EBitmapSavingFailed)}
  EBitmapSavingFailed = class(Exception);
{$IFEND}

function IsBitmapOpaque(const Bitmap: TBitmap): Boolean;
function HasBitmapFileExt(const FileName: string): Boolean;
function TryLoadBitmapFromFile(Bitmap: TBitmap; const FileName: string): Boolean;
function TryLoadBitmapFromStream(Bitmap: TBitmap; Stream: TStream): Boolean;
procedure SaveBitmapToStream(Bitmap: TBitmap; const FileExtForFormat: string; Stream: TStream);

{$IF NOT DECLARED(SBitmapSavingFailed)}
resourcestring
  SBitmapSavingFailed = 'Saving bitmap failed';
{$IFEND}

implementation

uses
  {$IF DEFINED(MSWINDOWS)}
  CCR.Clipboard.FMX.Win,
  {$ELSEIF DEFINED(IOS)}
  CCR.Clipboard.FMX.iOS,
  {$ELSEIF DEFINED(MACOS)}
  CCR.Clipboard.FMX.Mac,
  {$IFEND}
  CCR.Clipboard.Consts;

function TryGetPlatformService(const GUID: TGUID; out Obj): Boolean; inline;
begin
  {$IF FireMonkeyVersion >= 17}
  Result := TPlatformServices.Current.SupportsPlatformService(GUID, IInterface(Obj));
  {$ELSE}
  Result := False;
  {$IFEND}
end;

function ReplacePlatformService(const GUID: TGUID; const NewImpl: IInterface): IInterface;
begin
  {$IF FireMonkeyVersion >= 17}
  Result := TPlatformServices.Current.GetPlatformService(GUID);
  {$IF FireMonkeyVersion >= 21}
  TPlatformServices.Current.RemovePlatformService(GUID);
  {$IFEND}
  TPlatformServices.Current.AddPlatformService(GUID, NewImpl);
  {$ELSE}
  Assert(False);
  {$IFEND}
end;

{ Default, no-frills implementation of TClipboard.TCore that delegates to IFMXClipboardService }

{$IF FireMonkeyVersion >= 19}
type
  TClipboardFormatHelper = record helper for TClipboardFormat
    class function Wrap(OrdValue: NativeUInt): TClipboardFormat; static; inline;
  end;

  TDefaultClipboardCore = class(TClipboard.TCore)
  strict private
    FService: IFMXClipboardService;
  strict protected
    class procedure InitializeFormats(StdFormats: TClipboard.TStdFormatValues;
      var CustomFormatsSupported: Boolean); override;
    class function GetFormatName(const Format: TClipboardFormat): string; override;
    function GetFormats: TArray<TClipboardFormat>; override;
    function HasFormat(Format: TClipboardFormat): Boolean; override;
    function HasFormat(const Formats: array of TClipboardFormat; var Matched: TClipboardFormat): Boolean; override;
    function ReadPlainText: TArray<string>; override;
    procedure WritePlainText(const Renderer: TFunc<string>; PreferDelayed: Boolean); override;
    function DoOpen: Boolean; override;
    procedure DoClose; override;
    procedure DoClear; override;
  public
    constructor Create(const AOwner: TClipboard); override;
  end;

class function TClipboardFormatHelper.Wrap(OrdValue: NativeUInt): TClipboardFormat;
begin
  Move(OrdValue, Result, SizeOf(Result));
end;

class procedure TDefaultClipboardCore.InitializeFormats(
  StdFormats: TClipboard.TStdFormatValues; var CustomFormatsSupported: Boolean);
begin
  StdFormats.cfUnicodeText := TClipboardFormat.Wrap(1);
end;

class function TDefaultClipboardCore.GetFormatName(const Format: TClipboardFormat): string;
begin
  if Format = cfUnicodeText then
    Result := 'Unicode Text'
  else
    Result := '';
end;

constructor TDefaultClipboardCore.Create(const AOwner: TClipboard);
begin
  inherited Create(AOwner);
  FService := TPlatformServices.Current.GetPlatformService(IFMXClipboardService) as IFMXClipboardService;
end;

function TDefaultClipboardCore.DoOpen: Boolean;
begin
  Result := True;
end;

procedure TDefaultClipboardCore.DoClose;
begin
end;

procedure TDefaultClipboardCore.DoClear;
begin
  FService.SetClipboard('');
end;

function TDefaultClipboardCore.ReadPlainText: TArray<string>;
var
  S: string;
begin
  S := FService.GetClipboard.ToString;
  if S <> '' then
    Result := TArray<string>.Create(S)
  else
    Result := nil;
end;

procedure TDefaultClipboardCore.WritePlainText(const Renderer: TFunc<string>; PreferDelayed: Boolean);
begin
  FService.SetClipboard(Renderer());
end;

function TDefaultClipboardCore.GetFormats: TArray<TClipboardFormat>;
begin
  if FService.GetClipboard.IsEmpty then
    Result := nil
  else
    Result := TArray<TClipboardFormat>.Create(cfUnicodeText)
end;

function TDefaultClipboardCore.HasFormat(Format: TClipboardFormat): Boolean;
begin
  Result := (Format = cfUnicodeText) and not FService.GetClipboard.IsEmpty;
end;

function TDefaultClipboardCore.HasFormat(const Formats: array of TClipboardFormat; var Matched: TClipboardFormat): Boolean;
var
  Fmt: TClipboardFormat;
begin
  for Fmt in Formats do
    if Fmt = cfUnicodeText then
    begin
      Result := not FService.GetClipboard.IsEmpty;
      if Result then Matched := Fmt;
      Exit;
    end;
  Result := False;
end;
{$IFEND}

{$IF DECLARED(IFMXDragDropService)}

{ TFMXDragDropService  }

type
  TFMXDragDropService = class(TInterfacedObject, IFMXDragDropService)
  protected
    procedure BeginDragDrop(Form: TCommonCustomForm; const Data: TDragObject; Bitmap: TBitmap);
  end;

procedure TFMXDragDropService.BeginDragDrop(Form: TCommonCustomForm;
  const Data: TDragObject; Bitmap: TBitmap);
begin
  TClipboard.BeginDrag(Form, Data, Bitmap);
end;

{ TClipboardHelper.TFreeNotifier }

constructor TClipboardHelper.TFreeNotifier.Create(const ACallback: TFreeNotifierCallback);
begin
  inherited Create;
  FCallback := ACallback;
end;

procedure TClipboardHelper.TFreeNotifier.FreeNotification(AObject: TObject);
begin
  FCallback(AObject);
end;

{ TClipboardHelper }

class procedure TClipboardHelper.Finalize;
begin
  FRegisteredDropTargets.Free;
end;

class function TClipboardHelper.SupportsDragAndDrop: Boolean;
var
  ReplacementService: IInterface;
begin
  if FSupportsDragAndDrop = TUncertainState.Maybe then
  begin
    FSupportsDragAndDrop := TUncertainState.No;
    {$IF FireMonkeyVersion >= 17}
    if TryGetPlatformService(IFMXClipboardDragDropService, FDragDropIntf) then
    begin
      ReplacementService := TFMXDragDropService.Create;
      ReplacePlatformService(IFMXDragDropService, ReplacementService);
      FRegisteredDropTargets := TDictionary<Pointer, IClipboardDropInfo>.Create;
      FSupportsDragAndDrop := TUncertainState.Yes;
      FFormFreeNotifier := TFreeNotifier.Create(FormFreeNotifierCallback);
    end;
    {$IFEND}
  end;
  Result := (FSupportsDragAndDrop = TUncertainState.Yes);
end;

class procedure TClipboardHelper.NeedDragDropService;
begin
  if not SupportsDragAndDrop then
    raise EUnsupportedClipboardFeature.CreateRes(@SClipboardDragDropNotSupported);
end;

class procedure TClipboardHelper.BeginDrag(const Form: TCommonCustomForm; const Data: TDragObject;
  const DragImage: TBitmap; const OnGetData: TAssignClipboardForDragEvent);
var
  DataPtr: ^TDragObject;
begin
  NeedDragDropService;
  SetObjectBeingDragged(Data.Source);
  try
    DataPtr := @Data;
    FDragDropIntf.DoDrag(Data.Source, Form, DragImage,
      procedure (Clipboard: TClipboard)
      var
        Proc: TAssignClipboardForDragEvent;
      begin
        Proc := CreateCombinedGetDragDataCallback(OnGetData,
          procedure
          begin
            Clipboard.Assign(DataPtr^);
          end);
        Proc(DataPtr.Source, Clipboard);
      end);
  finally
    SetObjectBeingDragged(nil);
  end;
end;

class procedure TClipboardHelper.BeginDrag(const Source: TObject;
  const DragImage: TBitmap; const OnGetData: TAssignClipboardForDragEvent);
var
  DragObject: TDragObject;
begin
  DragObject.Source := Source;
  BeginDrag(Screen.ActiveForm, DragObject, DragImage, OnGetData);
end;

class procedure TClipboardHelper.BeginDrag(const DragImage: TBitmap;
  const OnGetData: TProc<TClipboard>);
var
  DragObject: TDragObject;
begin
  DragObject.Source := nil;
  BeginDrag(Screen.ActiveForm, DragObject, DragImage,
    procedure (Sender: TObject; Clipboard: TClipboard)
    begin
      OnGetData(Clipboard);
    end);
end;

class procedure TClipboardHelper.BeginDrag(const Source: IControl);
begin
  NeedDragDropService;
  Source.BeginAutoDrag;
end;

class procedure TClipboardHelper.BeginDrag(const Source: TControl;
  const OnGetData: TProc<TClipboard>);
var
  DragObject: TDragObject;
  DragImage: TBitmap;
begin
  DragObject.Source := Source;
  DragImage := Source.MakeScreenshot;
  try
    BeginDrag(Source.Root as TCommonCustomForm, DragObject, DragImage,
      procedure (Sender: TObject; Clipboard: TClipboard)
      begin
        OnGetData(Clipboard);
      end);
  finally
    DragImage.Free;
  end;
end;

class function TClipboardHelper.RegisterForDragAndDrop(const Form: TCommonCustomForm): IClipboardDropInfo;
begin
  NeedDragDropService;
  if FRegisteredDropTargets.TryGetValue(Pointer(Form), Result) then Exit;
  Form.AddFreeNotify(FFormFreeNotifier);
  Result := FDragDropIntf.RegisterForm(Form);
  FRegisteredDropTargets.Add(Pointer(Form), Result);
end;

class procedure TClipboardHelper.FormFreeNotifierCallback(Form: TObject);
begin
  FDragDropIntf.UnregisterForm(Form as TCommonCustomForm);
  FRegisteredDropTargets.Remove(Form);
end;

procedure TClipboardHelper.Assign(const DragObject: TDragObject);

  function TryObject(const Source: TObject): Boolean;
  begin
    { while we could just call Assign and catch EConvertError, that would make
      the default debugging experience crap }
    Result := HasClipper(Source.ClassType);
    if Result then
      Assign(Source);
  end;

  function TryValue(const Value: TValue): Boolean;
  var
    S: string;
  begin
    if Value.IsEmpty then
      Result := False
    else if Value.IsObject then
      Result := TryObject(Value.AsObject)
    else
    begin
      Result := Value.TryAsType<string>(S) and (S <> '');
      if Result then AssignText(S);
    end;
  end;
var
  FmxObj: TFmxObject;
  S: string;
  Value: TValue;
begin
  Open;
  try
    { assign any data explicitly passed on the TDragObject record itself }
    TryValue(DragObject.Data);
    for S in DragObject.Files do
      AssignFile(S);
    { try assigning the source object directly, and failing that, its data if it is an FMX object }
    if DragObject.Source <> nil then
      if not TryObject(DragObject.Source) then
        if DragObject.Source is TFmxObject then
        begin
          //default GetData implementation returns Name!
          FmxObj := TFmxObject(DragObject.Source);
          Value := FmxObj.Data;
          if not Value.TryAsType<string>(S) or (S <> FmxObj.Name) then TryValue(Value);
        end;
  finally
    Close;
  end;
end;

procedure TClipboardHelper.InitializeDragObject(var DragObject: TDragObject);
begin
  if not (TClipboardDragDropOption.LegacyDragObject in DragDropOptions) then
    DragObject.Source := Self
  else
  begin
    DragObject.Source := ObjectBeingDragged;
    TArray<string>(DragObject.Files) := GetFileNames;
    DragObject.Data := AsText;
  end;
end;
{$IFEND}

{ Bitmap helpers }

function IsBitmapOpaque(const Bitmap: TBitmap): Boolean;
var
  Pixels: PAlphaColorRec;
  SourceBits: TBitmapData;
  X, Y: Integer;
begin
  if not Bitmap.Map(TMapAccess.Read, SourceBits) then
    raise EInvalidOperation.CreateRes(@SCannotMapBitmap);
  try
    for Y := 0 to Bitmap.Height - 1 do
    begin
      Pixels := SourceBits.GetScanline(Y);
      for X := 0 to Bitmap.Width - 1 do
        if Pixels[X].A <> $FF then Exit(False);
    end;
  finally
    Bitmap.Unmap(SourceBits);
  end;
  Result := True;
end;

var
  LCFileExts: string;

function HasBitmapFileExt(const FileName: string): Boolean;
var
  LCExt: string;
begin
  if LCFileExts = '' then LCFileExts := LowerCase(
    {$IF FireMonkeyVersion < 17}
    DefaultBitmapCodecClass
    {$ELSE}
    TBitmapCodecManager
    {$IFEND}.GetFileTypes) + ';';
  LCExt := LowerCase(ExtractFileExt(FileName));
  Result := (LCExt <> '') and (Pos(LCExt + ';', LCFileExts) <> 0);
end;

function TryLoadBitmapFromFile(Bitmap: TBitmap; const FileName: string): Boolean;
{$IF FireMonkeyVersion < 17}
var
  Filter: TBitmapCodec;
begin
  Filter := DefaultBitmapCodecClass.Create;
  try
    Bitmap.Clear(0);
    if Filter.LoadFromFile(FileName, 0.0, Bitmap) then
    begin
      Bitmap.UpdateHandles;
      Bitmap.BitmapChanged;
      Exit(True);
    end;
  finally
    Filter.Free;
  end;
  Result := False;
end;
{$ELSEIF FireMonkeyVersion < 18}
begin
  Result := TBitmapCodecManager.LoadFromFile(FileName, Bitmap);
end;
{$ELSE}
var
  Surface: TBitmapSurface;
begin
  Surface := TBitmapSurface.Create;
  try
    Result := TBitmapCodecManager.LoadFromFile(FileName, Surface);
    if Result then Bitmap.Assign(Surface);
  finally
    Surface.Free;
  end;
end;
{$IFEND}

function TryLoadBitmapFromStream(Bitmap: TBitmap; Stream: TStream): Boolean;
{$IF FireMonkeyVersion < 17}
var
  Filter: TBitmapCodec;
begin
  Filter := DefaultBitmapCodecClass.Create;
  try
    Bitmap.Clear(0);
    if Filter.LoadFromStream(Stream, Bitmap) then
    begin
      Bitmap.UpdateHandles;
      Bitmap.BitmapChanged;
      Exit(True);
    end;
  finally
    Filter.Free;
  end;
  Result := False;
end;
{$ELSEIF FireMonkeyVersion < 18}
begin
  Result := TBitmapCodecManager.LoadFromStream(Stream, Bitmap);
end;
{$ELSE}
var
  Surface: TBitmapSurface;
begin
  Surface := TBitmapSurface.Create;
  try
    Result := TBitmapCodecManager.LoadFromStream(Stream, Surface);
    if Result then Bitmap.Assign(Surface);
  finally
    Surface.Free;
  end;
end;
{$IFEND}

procedure SaveBitmapToStream(Bitmap: TBitmap; const FileExtForFormat: string; Stream: TStream);
{$IF FireMonkeyVersion < 17}
var
  Filter: TBitmapCodec;
begin
  Filter := DefaultBitmapCodecClass.Create;
  try
    if not Filter.SaveToStream(Stream, Bitmap, FileExtForFormat) then
      raise EBitmapSavingFailed.Create(SBitmapSavingFailed);
  finally
    Filter.Free;
  end;
end;
{$ELSEIF FireMonkeyVersion < 18}
begin
  if not TBitmapCodecManager.SaveToStream(Stream, Bitmap, FileExtForFormat) then
    raise EBitmapSavingFailed.Create(SBitmapSavingFailed);
end;
{$ELSE}
var
  Surface: TBitmapSurface;
begin
  Surface := TBitmapSurface.Create;
  try
    Surface.Assign(Bitmap);
    if not TBitmapCodecManager.SaveToStream(Stream, Surface, FileExtForFormat) then
      raise EBitmapSavingFailed.Create(SBitmapSavingFailed);
  finally
    Surface.Free;
  end;
end;
{$IFEND}

{$IFDEF TBitmapHelperNeeded}
function TBitmapData.GetScanline(Row: Integer): Pointer;
begin
  Result := Source.ScanLine[Row];
end;

constructor TBitmapHelper.Create;
begin
  Create(0, 0);
end;

function TBitmapHelper.Map(const Access: TMapAccess; var Data: TBitmapData): Boolean;
begin
  Data.Data := StartLine;
  Data.Pitch := Width * 4;
  Data.PixelFormat := TPixelFormat.pfA8R8G8B8;
  Data.Source := Self;
  Data.Width := Width;
  Data.Height := Height;
  Result := True;
end;

procedure TBitmapHelper.Unmap(var Data: TBitmapData);
begin
  UpdateHandles;
  BitmapChanged;
end;
{$ENDIF}

{$IFDEF TBitmapDataHelperNeeded}
constructor TBitmapHelper.Create;
begin
  Create(0, 0);
end;

function TBitmapDataHelper.GetScanline(Row: Integer): Pointer;
begin
  Result := @PByte(Data)[Pitch * Row];
end;
{$ENDIF}

initialization
{$IF DECLARED(TDefaultClipboardService)}
  TClipboard.SetCoreFactory(TDefaultClipboardService);
{$IFEND}
finalization
{$IF DECLARED(IFMXDragDropService)}
  TClipboard.Finalize;
{$IFEND}
end.
