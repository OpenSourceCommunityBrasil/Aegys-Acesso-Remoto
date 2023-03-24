unit FastGate; // FastDIB v3.9: sourceforge.net/projects/tfastdib
               //  by: gordy <gordy@dynamicsdirect.com>
interface      //   and cmr.Pent <cmr.Pent@gmail.com>
               // Last update: August 2006
{$R-}

{  Routines for fast conversion between FastDIB and TGraphic.  }

uses Windows, FastDIB, Vcl.Graphics;

// graphic -> TFastDIB
procedure Bitmap2FastDIB(Src:TBitmap;Dst:TFastDIB);
// assigns TBitmap to TFastDIB
//  freeing Bitmap will also free FastDIB!
//  GDI methods no longer work on FastDIB (bitmaps can be selected for only one device context at a time)

procedure ReleaseBitmap2FastDIB(Src:TBitmap;Dst:TFastDIB);
procedure CopyBitmap2FastDIB(Src:TBitmap;Dst:TFastDIB);
procedure Graphic2FastDIB(Src:TGraphic;Dst:TFastDIB);

// TFastDIB -> graphic (UseGDI=True except CopyFastDIB2Bitmap)
procedure FastDIB2Bitmap(Src:TFastDIB;Dst:TBitmap);
// assigns TFastDIB to TBitmap
//  freeing Bitmap will also free FastDIB!
//  GDI methods no longer work on FastDIB (bitmaps can be selected for only one device context at a time)

procedure ReleaseFastDIB2Bitmap(Src:TFastDIB;Dst:TBitmap);
procedure CopyFastDIB2Bitmap(Src:TFastDIB;Dst:TBitmap);
procedure ReleaseFastDIB2Graphic(Src:TFastDIB;Dst:TGraphic);

implementation

procedure Bitmap2FastDIB(Src:TBitmap;Dst:TFastDIB);
begin
  Dst.AttachToHandle(Src.Canvas.Handle,Src.Handle);
end;

procedure ReleaseBitmap2FastDIB(Src:TBitmap;Dst:TFastDIB);
begin
  Dst.AttachToHandle(Src.Canvas.Handle,Src.ReleaseHandle);
  Dst.FreeHandle:=True;
  if Dst.UseGDI then
  begin
    Dst.hDC:=CreateCompatibleDC(0);
    SelectObject(Dst.hDC,Dst.Handle);
    Dst.FreeDC:=True;
  end
  else Dst.FreeBits:=True;
end;

procedure CopyBitmap2FastDIB(Src:TBitmap;Dst:TFastDIB);
begin
  Dst.LoadFromHandle(Src.Handle);
end;

procedure Graphic2FastDIB(Src:TGraphic;Dst:TFastDIB);
var
  t: TBitmap;
begin
  t:=TBitmap.Create;
  t.Assign(Src);
  ReleaseBitmap2FastDIB(t,Dst);
  t.Free;
end;

procedure FastDIB2Bitmap(Src:TFastDIB;Dst:TBitmap);
begin
  if Src.Handle<>0 then
  begin
    Dst.Handle:=Src.Handle;
    // bitmaps can be selected for only one device context at a time
    if(Src.hDC<>0)and Src.FreeDC then DeleteDC(Src.hDC);
    if(Src.hPen<>0)then DeleteObject(Src.hPen);
    if(Src.hFont<>0)then DeleteObject(Src.hFont);
    if(Src.hBrush<>0)then DeleteObject(Src.hBrush);
    Src.hDC:=0;
    Src.FreeDC:=False;
    Src.FreeBits:=False;
    Src.FreeHandle:=False;
  end;
end;

procedure ReleaseFastDIB2Bitmap(Src:TFastDIB;Dst:TBitmap);
begin
  if Src.Handle<>0 then
  begin
    Dst.Handle:=Src.Handle;
    with Src do
    begin
      FreeDC:=True;
      FreeBits:=False;
      FreeHandle:=False;
      PreDestroy;
      Release;
    end;
  end;
end;

procedure CopyFastDIB2Bitmap(Src:TFastDIB;Dst:TBitmap);
var
  t: TFastDIB;
begin
  t:=TFastDIB.Create;
  t.MakeCopy(Src,True);
  ReleaseFastDIB2Bitmap(t,Dst);
  t.Free;
end;

procedure ReleaseFastDIB2Graphic(Src:TFastDIB;Dst:TGraphic);
var
  t: TBitmap;
begin
  t:=TBitmap.Create;
  ReleaseFastDIB2Bitmap(Src,t);
  Dst.Assign(t);
  t.Free;
end;

end.
