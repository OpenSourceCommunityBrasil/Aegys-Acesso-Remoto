unit JDRMGraphics;

//Built in Delphi 7

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, JPeg, StrUtils, ExtCtrls,
  Forms, Controls;

type
  TJDRMImageBlock = class;
  TJDRMImageSplitter = class;
  TJDRMCustomDesktop = class;
  TJDRMDesktop = class;
  TJDRMCustomDesktopView = class;
  TJDRMDesktopView = class;
  TMouseMovements = Procedure (Sender: TObject; Button: TMouseButton; Shift: TShiftState;
                               X, Y: Integer) Of Object;
  TMouseMove      = Procedure (Sender: TObject; Shift: TShiftState; X, Y: Integer) Of Object;

  //TJDRMImageBlock
  //Represents a smaller portion of the larger screenshot
  TJDRMImageBlock = class(TComponent)
  private
    fBitmap: TBitmap; //Main instance of a bitmap (default)
    fLeft: Integer;   //Left (X) position of section on larger screenshot
    fTop: Integer;    //Top (Y) position of section on larger screenshot
    fCompression: Integer;
    fScreenWidth: Integer;
    fScreenHeight: Integer;
    procedure SetWidth(Value: Integer);
    procedure SetHeight(Value: Integer);
    procedure SetPixelFormat(Value: TPixelFormat);
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetPixelFormat: TPixelFormat;
    function GetJpeg: TJPEGImage;
    function GetStream: TMemoryStream;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AsBitmap: TBitmap read fBitmap;
    property AsJpeg: TJPEGImage read GetJpeg;
    property AsStream: TMemoryStream read GetStream;
  published
    property Left: Integer read fLeft write fLeft;
    property Top: Integer read fTop write fTop;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property ScreenWidth: Integer read fScreenWidth write fScreenWidth;
    property ScreenHeight: Integer read fScreenHeight write fScreenHeight;
    property PixelFormat: TPixelFormat read GetPixelFormat write SetPixelFormat;
    property Compression: Integer read fCompression write fCompression;
  end;



  //TJDRMImageSplitter
  //Represents the an entire screenshot image, and gives functionality
  //to split screenshot into smaller pieces
  TJDRMImageSplitter = class(TComponent)
  private
    fBitmap: TBitmap;     //Main instance of image
    fHCount: Integer;     //Number of blocks horizontally
    fVCount: Integer;     //Number of blocks vertically
    fBitWidth: Integer;   //Width of a block - used for performance
    fBitHeight: Integer;  //Height of a block - used for performance
    fCompression: Integer;//Compression of JPEG image
    procedure SetVCount(Value: Integer);
    procedure SetHCount(Value: Integer);
    procedure SetWidth(Value: Integer);
    procedure SetHeight(Value: Integer);
    procedure SetPixelFormat(Value: TPixelFormat);
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetPixelFormat: TPixelFormat;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetBlock(X, Y: Integer): TJDRMImageBlock;  //Returns a specified block
    procedure SetBlock(Value: TJDRMImageBlock);         //Assigns a specified block
    property Bitmap: TBitmap read fBitmap;
  published
    property VertCount: Integer read fVCount write SetVCount;
    property HorzCount: Integer read fHCount write SetHCount;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property PixelFormat: TPixelFormat read GetPixelFormat write SetPixelFormat;
    property Compression: Integer read fCompression write fCompression;
  end;



  //TJDRMCustomDesktop
  //Component which contains all necessary functionality for taking a
  // screenshot and triggering events when new sections of the screen are available

  //Triggered when a new section of screen is available
  TJDRMImageBlockEvent = procedure(Sender: TObject; Block: TJDRMImageBlock) of object;

  TJDRMCustomDesktop = class(TComponent)
  private
    fActive: Boolean;                     //Whether component is active or not
    fTimer: TTimer;                       //Loops through sections of screenshot
    fBitmap: TJDRMImageSplitter;          //Represents current screenshot image
    fLastBitmap: TJDRMImageSplitter;      //Represents previous screenshot image
    fBusy: Boolean;                       //Whether component is busy or not
    fDrawCursor: Boolean;                 //Whether or not to draw cursor on bitmap
    fCompression: Integer;                //JPEG Compression Value (1 - 100)

    fNewBlockEvent: TJDRMImageBlockEvent; //Event - triggered when new block is available

    procedure SetActive(Value: Boolean);            //Sets component active or inactive
    procedure SetVerticalCount(Value: Integer);     //Sets vertical count
    procedure SetHorizontalCount(Value: Integer);   //Sets horizontal count
    procedure SetPixelFormat(Value: TPixelFormat);  //Sets BMP pixel format
    procedure SetCompression(Value: Integer);       //Sets JPEG Compression
    procedure SetDelay(Value: Integer);

    function GetVerticalCount: Integer;             //Returns vertical count
    function GetHorizontalCount: Integer;           //Returns horizontal count
    function GetPixelFormat: TPixelFormat;          //Returns pixel format
    function GetDelay: Integer;

    procedure GetScreenshot;                        //Acquires new screenshot
    procedure NewBlock(Block: TJDRMImageBlock);     //Triggers fNewBlockEvent

    procedure TimerOnTimer(Sender: TObject);        //Assigned to Timer.OnTimer

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Compression: Integer read fCompression write SetCompression;
    property Active: Boolean read fActive write SetActive;
    property VerticalCount: Integer read GetVerticalCount write SetVerticalCount;
    property HorizontalCount: Integer read GetHorizontalCount write SetHorizontalCount;
    property PixelFormat: TPixelFormat read GetPixelFormat write SetPixelFormat;
    property DrawCursor: Boolean read fDrawCursor write fDrawCursor;
    property Delay: Integer read GetDelay write SetDelay;

    property OnNewBlock: TJDRMImageBlockEvent read fNewBlockEvent write fNewBlockEvent;

  end;

  //Final component representing entire screenshot process
  TJDRMDesktop = class(TJDRMCustomDesktop)
  published
    property Compression;
    property Active;
    property VerticalCount;
    property HorizontalCount;
    property PixelFormat;
    property DrawCursor;

    property OnNewBlock;
  end;




  //TJDRMCustomDesktopView
  //Visual component based on TScrollBox
  //Contains all necessary functionality to display desktop from
  //image blocks (from TJDRMDesktop) as well as recognizing mouse
  //and keyboard events for remote control
  TJDRMCustomDesktopView = class(TScrollingWinControl)
  private
    fBorderStyle: TBorderStyle;
    fSplitter: TJDRMImageSplitter;
    fImage: TImage;
    fTimer: TTimer;

    procedure SetImageWidth(Value: Integer);
    procedure SetImageHeight(Value: Integer);
    procedure SetRefreshRate(Value: Integer);

    function GetImageWidth: Integer;
    function GetImageHeight: Integer;
    function GetRefreshRate: Integer;

    procedure TimerOnTimer(Sender: TObject);        //Assigned to fTimer.OnTimer

    //From original TScrollBox component
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure WMNCHitTest(var Message: TMessage); message WM_NCHITTEST;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
  protected
    //From original TScrollBox component
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    Procedure  Resize;override;
    procedure DrawBlock(Block: TJDRMImageBlock);
    procedure UpdateImage;

    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property ImageWidth: Integer read GetImageWidth write SetImageWidth;
    property ImageHeight: Integer read GetImageHeight write SetImageHeight;
    property RefreshRate: Integer read GetRefreshRate write SetRefreshRate;
  end;

  TJDRMDesktopView = class(TJDRMCustomDesktopView)
  Private
   vOnClick,
   vOnDblClick   : TNotifyEvent;
   vOnMouseDown,
   vOnMouseUp    : TMouseMovements;
   vOnMouseMove  : TMouseMove;
   Procedure SetOnClick    (Value : TNotifyEvent);
   Procedure SetOnDblClick (Value : TNotifyEvent);
   Procedure SetOnMouseDown(Value : TMouseMovements);
   Procedure SetOnMouseMove(Value : TMouseMove);
   Procedure SetOnMouseUp  (Value : TMouseMovements);
  published
    property RefreshRate;
    //From original TScrollBox component
    property Align;
    property Anchors;
    property AutoScroll;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BorderStyle;
    property Constraints;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick      : TNotifyEvent    Read vOnClick     Write SetOnClick;
    property OnDblClick   : TNotifyEvent    Read vOnDblClick  Write SetOnDblClick;
    property OnMouseDown  : TMouseMovements Read vOnMouseDown Write SetOnMouseDown;
    property OnMouseMove  : TMouseMove      Read vOnMouseMove Write SetOnMouseMove;
    property OnMouseUp    : TMouseMovements Read vOnMouseUp   Write SetOnMouseUp;
  end;

  //Misc
  function BitmapsAreSame(Bitmap1, Bitmap2: TJDRMImageBlock): Boolean;
  function GetPixelSize(Format: TPixelFormat): Integer;
  function ScreenShot(DrawCursor: Boolean; Quality: TPixelFormat): TBitmap;

  procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('UDP Super Components', [TJDRMDesktop, TJDRMDesktopView]);
end;

Procedure TJDRMDesktopView.SetOnClick    (Value : TNotifyEvent);
Begin
 vOnClick := Value;
 Self.fImage.OnClick := vOnClick;
End;

Procedure TJDRMDesktopView.SetOnDblClick (Value : TNotifyEvent);
Begin
 vOnDblClick := Value;
 Self.fImage.OnDblClick  := vOnDblClick;
End;

Procedure TJDRMDesktopView.SetOnMouseDown(Value : TMouseMovements);
Begin
 vOnMouseDown := Value;
 Self.fImage.OnMouseDown := vOnMouseDown;
End;

Procedure TJDRMDesktopView.SetOnMouseMove(Value : TMouseMove);
Begin
 vOnMouseMove := Value;
 Self.fImage.OnMouseMove := vOnMouseMove;
End;

Procedure TJDRMDesktopView.SetOnMouseUp  (Value : TMouseMovements);
Begin
 vOnMouseUp    := Value;
 Self.fImage.OnMouseUp := vOnMouseUp;
End;

//Return proper byte size for bitmap comparison input
function GetPixelSize(Format: TPixelFormat): Integer;
begin
  case Format of
    pf8bit:  Result:= 1;
    pf16bit: Result:= 2;
    pf24bit: Result:= 3;
    pf32bit: Result:= 4;
  else
    Result:= 0;
  end;
end;

//Compares two bitmaps together to see if they're the same or not
function BitmapsAreSame(Bitmap1, Bitmap2: TJDRMImageBlock): Boolean;
var
  Ptr1, Ptr2: pointer;
  X: integer;
  PixelSize: byte;
begin
  //By default, assume images are different
  Result:= False;
  //First check if width, height, or pixel format are the same
  if (Bitmap1.Width = Bitmap2.Width) and
     (Bitmap1.Height = Bitmap2.Height) and
     (Bitmap1.PixelFormat = Bitmap2.PixelFormat) then
    begin
      //Obtain byte size of pixels based on pixel format
      PixelSize:= GetPixelSize(Bitmap1.PixelFormat);
      //Loop through height of bitmap and go row by row
      for X:= 0 to (Bitmap1.Height-1) do
        begin
          //Obtain rows of pixels from each bitmap
          Ptr1:= Bitmap1.AsBitmap.ScanLine[X];
          Ptr2:= Bitmap2.AsBitmap.ScanLine[X];
          //Compare bitmap rows together
          Result:= CompareMem(Ptr1, Ptr2, Bitmap1.Width * PixelSize);
          //If rows are different, then exit loop
          if Result = False then Break;
        end;
    end;
end;

function ScreenShot(DrawCursor: Boolean; Quality: TPixelFormat): TBitmap;
var
  DC: HDC;
  R: TRect;
  CursorInfo: TCursorInfo;
  Icon: TIcon;
  IconInfo: TIconInfo;
begin
  //Create bitmap result
  Result:= TBitmap.Create;
  //Get desktop handle
  DC:= GetDC(GetDesktopWindow);
  try
    //Set result to new screenshot image
    Result.Width:= GetDeviceCaps (DC, HORZRES);
    Result.Height:= GetDeviceCaps (DC, VERTRES);
    Result.PixelFormat:= Quality;
    //Actual acquiring of screenshot image
    BitBlt(Result.Canvas.Handle,
      0,
      0,
      Result.Width,
      Result.Height,
      DC,
      0,
      0,
      SRCCOPY);
  finally
    ReleaseDC(GetDesktopWindow, DC);
  end;
  //Draw cursor
  if DrawCursor then begin
    R:= Result.Canvas.ClipRect;
    Icon:= TIcon.Create;
    try
      CursorInfo.cbSize:= SizeOf(CursorInfo);
      if GetCursorInfo(CursorInfo) then
      if CursorInfo.Flags = CURSOR_SHOWING then
      begin
        Icon.Handle:= CopyIcon(CursorInfo.hCursor);
        if GetIconInfo(Icon.Handle, IconInfo) then
        begin
          //Draw cursor image on screenshot image
          Result.Canvas.Draw(
            CursorInfo.ptScreenPos.x - Integer(IconInfo.xHotspot) - r.Left,
            CursorInfo.ptScreenPos.y - Integer(IconInfo.yHotspot) - r.Top,
            Icon);
        end;
      end;
    finally
      Icon.Free;
    end;
  end;
end;



// ---------------------- TJDRMImageBlock ------------------------
constructor TJDRMImageBlock.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.fBitmap:= TBitmap.Create;
  Self.fLeft:= 0;
  Self.fTop:= 0;
end;

destructor TJDRMImageBlock.Destroy;
begin
  if assigned(Self.fBitmap) then Self.fBitmap.Free;
  inherited Destroy;
end;

procedure TJDRMImageBlock.SetWidth(Value: Integer);
begin
  Self.fBitmap.Width:= Value;
end;

procedure TJDRMImageBlock.SetHeight(Value: Integer);
begin
  Self.fBitmap.Height:= Value;
end;

procedure TJDRMImageBlock.SetPixelFormat(Value: TPixelFormat);
begin
  Self.fBitmap.PixelFormat:= Value;
end;

function TJDRMImageBlock.GetWidth: Integer;
begin
  Result:= Self.fBitmap.Width;
end;

function TJDRMImageBlock.GetHeight: Integer;
begin
  Result:= Self.fBitmap.Height;
end;

function TJDRMImageBlock.GetPixelFormat: TPixelFormat;
begin
  Result:= Self.fBitmap.PixelFormat;
end;

function TJDRMImageBlock.GetJpeg: TJPEGImage;
begin
  Result:= TJPEGImage.Create;
  Result.CompressionQuality:= Self.fCompression;
  Result.Assign(Self.fBitmap);
end;

function TJDRMImageBlock.GetStream: TMemoryStream;
var
  J: TJPEGImage;
begin
  J:= Self.GetJpeg;
  try
    Result:= TMemoryStream.Create;
    J.SaveToStream(Result);
    //Add more data to stream for image location, screen size, etc.
  finally
    J.Free;
  end;
end;



// ----------------------- TJDRMImageSplitter -----------------------------
constructor TJDRMImageSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.fBitmap:= TBitmap.Create;
  Self.PixelFormat:= pf24bit;
  Self.Compression:= 100;
end;

destructor TJDRMImageSplitter.Destroy;
begin
  if assigned(Self.fBitmap) then Self.fBitmap.Free;
  inherited Destroy;
end;

procedure TJDRMImageSplitter.SetWidth(Value: Integer);
begin
  Self.fBitmap.Width:= Value;
end;

procedure TJDRMImageSplitter.SetHeight(Value: Integer);
begin
  Self.fBitmap.Height:= Value;
end;

procedure TJDRMImageSplitter.SetPixelFormat(Value: TPixelFormat);
begin
  Self.fBitmap.PixelFormat:= Value;
end;

function TJDRMImageSplitter.GetWidth: Integer;
begin
  Result:= Self.fBitmap.Width;
end;

function TJDRMImageSplitter.GetHeight: Integer;
begin
  Result:= Self.fBitmap.Height;
end;

function TJDRMImageSplitter.GetPixelFormat: TPixelFormat;
begin
  Result:= Self.fBitmap.PixelFormat;
end;

//Obtains a specified section of the original full image
function TJDRMImageSplitter.GetBlock(X, Y: Integer): TJDRMImageBlock;
begin
  //X = column, Y = row
  //Make sure X and Y are within range
  if (X >= 0) and (X < Self.fHCount) and
     (Y >= 0) and (Y < Self.fVCount) then
  begin
    //Create result
    Result:= TJDRMImageBlock.Create(nil);
    try
      //Prepare result bitmap for new data
      Result.fBitmap.Width:= Self.fBitWidth;
      Result.fBitmap.Height:= Self.fBitHeight;
      if Result.PixelFormat <> Self.PixelFormat then
        Result.PixelFormat:= Self.PixelFormat;
      Result.Left:= fBitWidth * X;
      Result.Top:= fBitHeight * Y;
      //Set screen size
      Result.ScreenWidth:= Screen.Width;
      Result.ScreenHeight:= Screen.Height;
      //Set Compression for JPEG
      Result.Compression:= Self.Compression;
      //Copy section of the screenshot to new canvas
      Result.AsBitmap.Canvas.CopyRect(
        Rect(
          0,
          0,
          fBitWidth,
          fBitHeight
        ),
        Self.Bitmap.Canvas,
        Rect(
          fBitWidth * X,
          fBitHeight * Y,
          Self.Width - (fBitWidth * (Self.fHCount - X - 1)),
          Self.Height - (fBitHeight * (Self.fVCount - Y - 1))
        )
      );
    except
      on e: exception do begin
        raise exception.Create('Error during block acquisition: '+#10+
          e.Message);
      end;
    end;
  end else begin
    raise exception.Create('Block index out of bounds ['+IntToStr(X)+','+IntToStr(Y)+']');
  end;
end;

//Draw new block image on canvas
procedure TJDRMImageSplitter.SetBlock(Value: TJDRMImageBlock);
begin
  if assigned(Value) then begin
    if assigned(Value.fBitmap) then begin
      if Value.fScreenWidth <> Self.fBitmap.Width then
        Self.fBitmap.Width:= Value.fScreenWidth;
      if Value.fScreenHeight <> Self.fBitmap.Height then
        Self.fBitmap.Height:= Value.fScreenHeight;
      Self.Bitmap.Canvas.Draw(Value.Left, Value.Top, Value.fBitmap);
    end;
  end;
end;

procedure  TJDRMImageSplitter.SetVCount(Value: Integer);
begin
  //Property Setting Procedure
  Self.fVCount:= Value;
  Self.fBitHeight:= Round(Self.Height / Self.fVCount);
end;

procedure  TJDRMImageSplitter.SetHCount(Value: Integer);
begin
  //Property Setting Procedure
  Self.fHCount:= Value;
  Self.fBitWidth:= Round(Self.Width / Self.fHCount);
end;



// --------------------------- TJDRMCustomDesktop -----------------------------
constructor TJDRMCustomDesktop.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.fBusy:= True;
  try
    Self.fActive:= False;

    Self.fBitmap:= TJDRMImageSplitter.Create(Self);
      Self.fBitmap.Width:= Screen.Width;
      Self.fBitmap.Height:= Screen.Height;

    Self.fLastBitmap:= TJDRMImageSplitter.Create(Self);
      Self.fLastBitmap.Width:= Self.fBitmap.Width;
      Self.fLastBitmap.Height:= Self.fBitmap.Height;

    Self.fTimer:= TTimer.Create(Self);
      Self.fTimer.Enabled:= False;
      Self.fTimer.Interval:= 5;
      Self.fTimer.OnTimer:= Self.TimerOnTimer;

    //Set Defaults
    Self.PixelFormat:= pf32bit;
    Self.VerticalCount:= 15;
    Self.HorizontalCount:= 15;
    Self.Compression:= 100;
  finally
    Self.fBusy:= False;
  end;
end;

destructor TJDRMCustomDesktop.Destroy;
begin
  Self.fBusy:= True;
  if assigned(fBitmap) then fBitmap.Free;
  if assigned(fLastBitmap) then fLastBitmap.Free;
  if assigned(fTimer) then fTimer.Free;

  inherited Destroy;
end;

//Acquire next screenshot and process it
procedure TJDRMCustomDesktop.TimerOnTimer(Sender: TObject);
var
  X, Y: Integer;
  B, B2: TJDRMImageBlock;
begin
  //Only process if component is not busy
  if not Self.fBusy then begin
    Self.fBusy:= True;
    try
      if Self.fActive then begin
        //Acquire new screenshot and assign to fBitmap
        Self.GetScreenshot;
        //Loop through horizontal section count, then vertical
        for X:= 0 to Self.fBitmap.HorzCount - 1 do begin
          for Y:= 0 to Self.fBitmap.VertCount - 1 do begin
            //Exit if not active
            if not Self.fActive then begin
              Self.fBusy:= False;
              Exit;
            end;
            //Acquire blocks to compare
            B:= Self.fBitmap.GetBlock(X, Y);
            B2:= Self.fLastBitmap.GetBlock(X, Y);
            try
              //Compare blocks
              if not BitmapsAreSame(B, B2) then
              begin
                //Trigger new block event
                Self.NewBlock(B);
                //Set the 'last' bitmap's corresponding block to new one
                Self.fLastBitmap.SetBlock(B);
              end;
            finally
              B.Free;
              B2.Free;
            end;
          end;
        end;
      end;
    finally
      Self.fBusy:= False;
    end;
  end;
end;

procedure TJDRMCustomDesktop.SetActive(Value: Boolean);
begin
  //Property Setting Procedure
  Self.fActive:= Value;
  Self.fTimer.Enabled:= Value;
end;

procedure TJDRMCustomDesktop.SetVerticalCount(Value: Integer);
begin
  //Property Setting Procedure
  Self.fBitmap.VertCount:= Value;
  Self.fLastBitmap.VertCount:= Value;
end;

procedure TJDRMCustomDesktop.SetHorizontalCount(Value: Integer);
begin
  //Property Setting Procedure
  Self.fBitmap.HorzCount:= Value;
  Self.fLastBitmap.HorzCount:= Value;
end;

procedure TJDRMCustomDesktop.SetPixelFormat(Value: TPixelFormat);
begin
  //Property Setting Procedure
  Self.fBitmap.PixelFormat:= Value;
  Self.fLastBitmap.PixelFormat:= Value;
end;

procedure TJDRMCustomDesktop.SetCompression(Value: Integer);
begin
  //Property Setting Procedure
  if Value > 100 then Value:= 100;
  if Value < 0 then Value:= 0;
  Self.fCompression:= Value;
  Self.fBitmap.Compression:= Value;
  Self.fLastBitmap.Compression:= Value;
end;

procedure TJDRMCustomDesktop.SetDelay(Value: Integer);
begin
  //Property Setting Procedure
  Self.fTimer.Interval:= Value;
end;

function TJDRMCustomDesktop.GetVerticalCount: Integer;
begin
  //Property Acquiring Function
  Result:= Self.fBitmap.VertCount;
end;

function TJDRMCustomDesktop.GetHorizontalCount: Integer;
begin
  //Property Acquiring Function
  Result:= Self.fBitmap.HorzCount;
end;

function TJDRMCustomDesktop.GetPixelFormat: TPixelFormat;
begin
  //Property Acquiring Function
  Result:= Self.fBitmap.PixelFormat;
end;

function TJDRMCustomDesktop.GetDelay: Integer;
begin
  //Property Acquiring Function
  Result:= Self.fTimer.Interval;
end;

procedure TJDRMCustomDesktop.GetScreenshot;
var
  B: TBitmap;
begin
  //Get screenshot
  B:= ScreenShot(Self.DrawCursor, Self.PixelFormat);
  try
    //Assign new screenshot to fBitmap
    Self.fBitmap.Bitmap.Assign(B);
    //Prepare 'last' bitmap with new dimensions
    if Self.fLastBitmap.Width <> Self.fBitmap.Width then
      Self.fLastBitmap.Width:= Self.fBitmap.Width;
    if Self.fLastBitmap.Height <> Self.fBitmap.Height then
      Self.fLastBitmap.Height:= Self.fBitmap.Height;
  finally
    B.Free;
  end;
  {$IFDEF MSWINDOWS}
  {$IFNDEF FMX}Application.Processmessages;
       {$ELSE}FMX.Forms.TApplication.ProcessMessages;{$ENDIF}
  {$ENDIF}
end;

procedure TJDRMCustomDesktop.NewBlock(Block: TJDRMImageBlock);
begin
  //Trigger new block event
  if assigned(Self.fNewBlockEvent) then
  begin
    Self.fNewBlockEvent(Self, Block);
  end;
end;

Function ResizeBmp(bitmp: TBitmap; wid, hei : Integer) : Boolean;
Var
 TmpBmp: TBitmap;
 ARect: TRect;
Begin
 Result := False;
 try
   TmpBmp := TBitmap.Create;
   try
     TmpBmp.Width  := wid;
     TmpBmp.Height := hei;
     ARect := Rect(0,0, wid, hei);
     TmpBmp.Canvas.StretchDraw(ARect, Bitmp);
     bitmp.Assign(TmpBmp);
   finally
     TmpBmp.Free;
   end;
   Result := True;
 except
   Result := False;
 end;
end;

constructor TJDRMCustomDesktopView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csDoubleClicks];
  Width := 185;
  Height := 41;

  Self.VertScrollBar.Visible:= True;
  Self.HorzScrollBar.Visible:= True;

  Self.fImage:= TImage.Create(Self);
  Self.fImage.Parent:= TWinControl(Self);
  Self.fImage.Left:= 0;
  Self.fImage.Top:= 0;
  Self.fImage.AutoSize:= True;

  Self.fSplitter:= TJDRMImageSplitter.Create(Self);

  Self.fTimer:= TTimer.Create(Self);
    Self.fTimer.Interval:= 0;
    Self.fTimer.OnTimer:= Self.TimerOnTimer;
    Self.fTimer.Enabled:= True;

end;

Procedure  TJDRMCustomDesktopView.Resize;
Begin
  Self.fImage.Picture.Assign(Self.fSplitter.Bitmap);
  ResizeBmp(Self.fImage.Picture.Bitmap, Self.Width, Self.Height);
  {$IFDEF MSWINDOWS}
  {$IFNDEF FMX}Application.Processmessages;
       {$ELSE}FMX.Forms.TApplication.ProcessMessages;{$ENDIF}
  {$ENDIF}
End;

destructor TJDRMCustomDesktopView.Destroy;
begin
  if assigned(Self.fSplitter) then Self.fSplitter.Free;
  if assigned(Self.fImage) then Self.fImage.Free;

  inherited Destroy;
end;

procedure TJDRMCustomDesktopView.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

procedure TJDRMCustomDesktopView.SetBorderStyle(Value: TBorderStyle);
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TJDRMCustomDesktopView.WMNCHitTest(var Message: TMessage);
begin
  DefaultHandler(Message);
end;

procedure TJDRMCustomDesktopView.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then RecreateWnd;
  inherited;
end;

procedure TJDRMCustomDesktopView.SetImageWidth(Value: Integer);
begin
  Self.fSplitter.Width:= Value;
  Self.fImage.Picture.Assign(Self.fSplitter.Bitmap);
end;

procedure TJDRMCustomDesktopView.SetImageHeight(Value: Integer);
begin
  Self.fSplitter.Height:= Value;
  Self.fImage.Picture.Assign(Self.fSplitter.Bitmap);
end;

procedure TJDRMCustomDesktopView.SetRefreshRate(Value: Integer);
begin
  Self.fTimer.Interval:= Value;
end;

function TJDRMCustomDesktopView.GetImageWidth: Integer;
begin
  Result:= Self.fSplitter.Width;
end;

function TJDRMCustomDesktopView.GetImageHeight: Integer;
begin
  Result:= Self.fSplitter.Height;
end;

function TJDRMCustomDesktopView.GetRefreshRate: Integer;
begin
  Result:= Self.fTimer.Interval;
end;

procedure TJDRMCustomDesktopView.DrawBlock(Block: TJDRMImageBlock);
begin
  Self.fSplitter.SetBlock(Block);
end;

procedure TJDRMCustomDesktopView.UpdateImage;
begin
  Self.fImage.Picture.Assign(Self.fSplitter.Bitmap);
  ResizeBmp(Self.fImage.Picture.Bitmap, Self.Width, Self.Height);
  {$IFDEF MSWINDOWS}
  {$IFNDEF FMX}Application.Processmessages;
       {$ELSE}FMX.Forms.TApplication.ProcessMessages;{$ENDIF}
  {$ENDIF}
end;

procedure TJDRMCustomDesktopView.TimerOnTimer(Sender: TObject);
begin
  if Self.fTimer.Interval > 0 then begin
    Self.UpdateImage;
  end;
end;

end.
