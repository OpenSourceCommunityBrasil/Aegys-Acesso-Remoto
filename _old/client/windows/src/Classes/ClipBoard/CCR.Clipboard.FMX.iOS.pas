{**************************************************************************************}
{                                                                                      }
{ CCR.Clipboard - FMX-specific extensions to generic iOS backend                       }
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

unit CCR.Clipboard.FMX.iOS;

interface

{$IFDEF IOS}
uses
  MacApi.ObjectiveC, iOSapi.CoreGraphics, iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit,
  System.SysUtils, System.Classes, System.Math, System.Messaging, System.UITypes,
  FMX.Types, FMX.Platform,
  CCR.Clipboard, CCR.Clipboard.Apple, CCR.Clipboard.FMX;

type
  TClipboardHelper = class helper(CCR.Clipboard.FMX.TClipboardHelper) for TClipboard
  strict private
    function GetPasteboard: UIPasteboard;
    function GetPersistent: Boolean;
    procedure SetPersistent(NewValue: Boolean);
  public
    class function CreateForPasteboard(const APasteboard: UIPasteboard): TClipboard; overload;
    class function CreateForPasteboard(const AName: NSString;
      CreateIfNotExists: Boolean = True): TClipboard; overload;
    class function CreateForPasteboard(const AName: string;
      CreateIfNotExists: Boolean = True): TClipboard; overload;
    procedure AssignNSObject(const Format: TClipboardFormat; const Obj: NSObject); overload;
    procedure AssignNSObject(const Format: TClipboardFormat; const ObjID: Pointer); overload;
    property Pasteboard: UIPasteboard read GetPasteboard;
    property Persistent: Boolean read GetPersistent write SetPersistent;
  end;

  TiOSClipboardCoreFMX = class(TiOSClipboardCore)
  strict private
    FChangeCountBeforeBackgrounded: TClipboardChangeCount;
  strict protected
    procedure ApplicationEvent(const Sender: TObject; const M: TMessage);
    procedure EnableChangeNotifications(const Callback: TThreadMethod); override;
    procedure DisableChangeNotifications; override;
  end;

var
  MaxClipboardBitmapSize: Integer = 1024;
{$ENDIF}

implementation

{$IFDEF IOS}
uses
  System.RTLConsts, CCR.Clipboard.Consts, CCR.Clipboard.Apple.Helpers;

type
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

  TBitmapClipper = class(TInterfacedObject, ICustomClipper, IObjectClipper<TBitmap>)
  protected
    function CanLoadFromClipboard(const Clipboard: TClipboard): Boolean;
    procedure LoadFromClipboard(const Clipboard: TClipboard;
      const Callback: TLoadObjectsCallback<TBitmap>);
    procedure SaveToClipboard(const Clipboard: TClipboard; PreferDelayed: Boolean;
      const BitmapGetter: TFunc<TBitmap>);
  end;

{ TClipboardHelper }

class function TClipboardHelper.CreateForPasteboard(const APasteBoard: UIPasteboard): TClipboard;
begin
  Result := TiOSClipboardCoreFMX.CreateClipboardForPasteboard(APasteBoard);
end;

class function TClipboardHelper.CreateForPasteboard(const AName: NSString;
  CreateIfNotExists: Boolean): TClipboard;
var
  MakePersistent: Boolean;
  PasteboardID: Pointer;
begin
  { custom pasteboards are not persistent by default on iOS yet are on OS X;
    the fiddling here enforces the OS X model for consistency  }
  PasteboardID := TUIPasteboard.OCClass.pasteboardWithName(AName, False);
  if PasteboardID <> nil then
    MakePersistent := False
  else
  begin
    if CreateIfNotExists then
      PasteboardID := TUIPasteboard.OCClass.pasteboardWithName(AName, True);
    if PasteboardID = nil then
      raise EClipboardException.CreateResFmt(@SpasteboardWithNameFailed,
        [AName, BoolToStr(CreateIfNotExists, True)]);
    MakePersistent := True;
  end;
  Result := CreateForPasteboard(TUIPasteboard.Wrap(PasteboardID));
  if MakePersistent then
     Result.Pasteboard.setPersistent(True);
end;

class function TClipboardHelper.CreateForPasteboard(const AName: string;
  CreateIfNotExists: Boolean): TClipboard;
begin
  Result := CreateForPasteboard(StrToNSString(AName), CreateIfNotExists);
end;

function TClipboardHelper.GetPasteboard: UIPasteboard;
begin
  Result := (Core as IiOSClipboardCore).Pasteboard;
end;

function TClipboardHelper.GetPersistent: Boolean;
begin
  Result := Pasteboard.isPersistent;
end;

procedure TClipboardHelper.SetPersistent(NewValue: Boolean);
begin
  Pasteboard.setPersistent(NewValue);
end;

procedure TClipboardHelper.AssignNSObject(const Format: TClipboardFormat; const Obj: NSObject);
begin
  AssignNSObject(Format, (Obj as ILocalObject).GetObjectID);
end;

procedure TClipboardHelper.AssignNSObject(const Format: TClipboardFormat; const ObjID: Pointer);
begin
  Add(procedure
      begin
        (Core as IiOSClipboardCore).AssignNSObject(Format, ObjID);
      end);
end;

{ TAlphaColorClipper }

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
  Colors := Clipboard.Pasteboard.colors;
  for I := 0 to Colors.count - 1 do
  begin
    TUIColor.Wrap(Colors.objectAtIndex(I)).getRed(@R, @G, @B, @A);
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
  Clipboard.AssignNSObject(cfColor, TUIColor.OCClass.colorWithRed(F.R, F.G, F.B, F.A));
end;

class procedure TAlphaColorClipper.Register;
var
  Inst: TAlphaColorClipper;
begin
  Inst := Create;
  TClipboard.RegisterClipper<TAlphaColor>(Inst);
  TClipboard.RegisterClipper<TAlphaColorF>(Inst);
end;

{ TBitmapClipper }

procedure CopyCGImageToBitmap(const Source: CGImageRef; const Dest: TBitmap);
var
  ColorSpace: CGColorSpaceRef;
  Context: CGContextRef;
  MapRec: TBitmapData;
  W, H: LongWord;
  C: Extended;
begin
  W := CGImageGetWidth(Source);
  H := CGImageGetHeight(Source);
  C := Max(W, H) / MaxClipboardBitmapSize;
  if C > 1 then
  begin
    W := Round(W / C);
    H := Round(H / C);
  end;
  Dest.SetSize(W, H);
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

function TBitmapClipper.CanLoadFromClipboard(const Clipboard: TClipboard): Boolean;
begin
  Result := Clipboard.HasFormat([cfPNG, cfJPEG]);
end;

procedure TBitmapClipper.LoadFromClipboard(const Clipboard: TClipboard;
  const Callback: TLoadObjectsCallback<TBitmap>);
var
  LookForMore: Boolean;
  I: Integer;
  Images: NSArray;
begin
  Images := Clipboard.Pasteboard.images;
  if Images = nil then Exit;
  LookForMore := True;
  for I := 0 to Images.count - 1 do
  begin
    Callback(
      procedure (Dest: TBitmap)
      begin
        CopyCGImageToBitmap(TUIImage.Wrap(Images.objectAtIndex(I)).CGImage, Dest);
      end, LookForMore);
    if not LookForMore then Exit;
  end;
end;

procedure TBitmapClipper.SaveToClipboard(const Clipboard: TClipboard;
  PreferDelayed: Boolean; const BitmapGetter: TFunc<TBitmap>);
begin
  Clipboard.Assign(cfPNG, BitmapGetter() as IStreamPersist);
end;

{ TiOSClipboardCoreFMX }

procedure TiOSClipboardCoreFMX.ApplicationEvent(const Sender: TObject; const M: TMessage);
begin
  case (M as TApplicationEventMessage).Value.Event of
    TApplicationEvent.EnteredBackground: FChangeCountBeforeBackgrounded := GetChangeCount;
    TApplicationEvent.WillBecomeForeground:
      if (GetChangeCount <> FChangeCountBeforeBackgrounded) and Assigned(OnNotifyChanges) then
        OnNotifyChanges();
  end;
end;

procedure TiOSClipboardCoreFMX.EnableChangeNotifications(const Callback: TThreadMethod);
begin
  inherited;
  //OS only notifies us if we're in the foreground...
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEvent)
end;

procedure TiOSClipboardCoreFMX.DisableChangeNotifications;
begin
  inherited;
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEvent);
end;

initialization
  TiOSClipboardCoreFMX.Register;
  TAlphaColorClipper.Register;
  TClipboard.RegisterClipper<TBitmap>(TBitmapClipper.Create);
{$ENDIF}
end.
