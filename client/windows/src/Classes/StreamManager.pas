unit StreamManager;

{
   Aegys Remote Access Project.
  Criado por XyberX (Gilbero Rocha da Silva), o Aegys Remote Access Project tem como objetivo o uso de Acesso remoto
  Gratuito para utilização de pessoas em geral.
   O Aegys Remote Access Project tem como desenvolvedores e mantedores hoje

  Membros do Grupo :

  XyberX (Gilberto Rocha)    - Admin - Criador e Administrador  do pacote.
  Wendel Fassarela           - Devel and Admin
  Mobius One                 - Devel, Tester and Admin.
  Gustavo                    - Devel and Admin.
  Roniery                    - Devel and Admin.
  Alexandre Abbade           - Devel and Admin.
  e Outros como você, venha participar também.
}

interface

uses
  System.Classes,
  System.Types,
  FMX.Forms,
  Vcl.Imaging.jpeg,
  Execute.DesktopDuplicationAPI,
  FMX.Objects
  ,Vcl.Graphics
  ,Winapi.Windows
  ,Vcl.Forms
  , uAegysBufferPack;

Const
  TNeutroColor = 255;

Type
  TRGBTriple = Packed Record
    B: Byte;
    G: Byte;
    R: Byte;
  End;

Type
  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = Array [0 .. 4095] of TRGBTriple;

Var
 aFullBmp : Vcl.Graphics.TBitmap;

procedure GetScreenToMemoryStream(Var aPackClass     : TPackClass;
                                  DrawCur            : Boolean;
                                  PixelFormat        : TPixelFormat = pf15bit;
                                  Monitor            : String       = '0';
                                  FullFrame          : Boolean      = False);

procedure ResizeBmp(AImage: TImage; AStream: TMemoryStream; AWidth, AHeight: Single);

Var
 pdst     : Pointer;
 ASMSize,
 muASM    : Integer;
 cpdst     : Pointer;
 cASMSize,
 cmuASM    : Integer;

implementation

uses
  System.SysUtils, uFormConexao, uAegysZlib, uAegysDataTypes;

procedure ResizeBmp(AImage: TImage; AStream: TMemoryStream; AWidth, AHeight: Single);
var
  bmpSrc, bmpDest: Vcl.Graphics.TBitmap;
  msImage: TMemoryStream;
begin
  try
    msImage := TMemoryStream.Create;
    msImage.Clear;
    bmpSrc := Vcl.Graphics.TBitmap.Create;
    bmpDest := Vcl.Graphics.TBitmap.Create;
    AStream.Position := 0;
    bmpSrc.LoadFromStream(AStream);
    bmpDest.Width := Trunc(AWidth);
    bmpDest.Height := Trunc(AHeight);
    SetStretchBltMode(bmpDest.Canvas.Handle, HALFTONE);
    StretchBlt(
      bmpDest.Canvas.Handle,
      0,
      0,
      bmpDest.Width,
      bmpDest.Height,
      bmpSrc.Canvas.Handle,
      0,
      0,
      bmpSrc.Width,
      bmpSrc.Height,
      SRCCOPY);
    bmpDest.SaveToStream(msImage);
    msImage.Position := 0;
    AImage.Bitmap.LoadFromStream(msImage);
  finally
    FreeAndNil(msImage);
    bmpSrc.Free;
    bmpDest.Free;
  end;
end;

Procedure DrawScreenCursor(Var Bmp: Vcl.Graphics.TBitmap; const MonitorID: Integer);
Var
 R          : TRect;
 CursorInfo : TCursorInfo;
 Left,
 Top        : Integer;
 Icon       : TIcon;
 IconInfo   : TIconInfo;
Begin
 R    := Bmp.Canvas.ClipRect;
 Icon := TIcon.Create;
 Try
  CursorInfo.cbSize := SizeOf(CursorInfo);
  If GetCursorInfo(CursorInfo) Then
   If CursorInfo.Flags = CURSOR_SHOWING Then
    Begin
     Icon.Handle:= CopyIcon(CursorInfo.hCursor);
     If GetIconInfo(Icon.Handle, IconInfo) Then
      Begin
       If CursorInfo.ptScreenPos.x > Screen.Monitors[MonitorID].Left Then
        Left := CursorInfo.ptScreenPos.x - Screen.Monitors[MonitorID].Left
       Else
        Left := CursorInfo.ptScreenPos.x;
       If CursorInfo.ptScreenPos.y > Screen.Monitors[MonitorID].Top  Then
        Top  := CursorInfo.ptScreenPos.y - Screen.Monitors[MonitorID].Top
       Else
        Top  := CursorInfo.ptScreenPos.y;
       Bmp.Canvas.Draw(Left - Integer(IconInfo.xHotspot) - R.Left,
                       Top  - Integer(IconInfo.yHotspot) - R.Top,
                       Icon);
      End;
    End;
 Finally
  Icon.Free;
 End;
End;

procedure GetScreenToMemoryStream(Var aPackClass     : TPackClass;
                                  DrawCur            : Boolean;
                                  PixelFormat        : TPixelFormat = pf15bit;
                                  Monitor            : String       = '0';
                                  FullFrame          : Boolean      = False);
Var
  Mybmp,
  MybmpPart      : Vcl.Graphics.TBitmap;
  aPackCountData : AeInteger;
  I,
  pRectTop,
  pRectLeft,
  pRectBottom,
  pRectRight,
  vMonitor    : Integer;
  aSizeData   : AeInt64;
  vFirstImg,
  vNewFrame,
  vMultiPoint : Boolean;
  aFinalBytes,
  aBytes,
  aPackBytes    : TAegysBytes;
  aMonitor,
  aResolution   : String;
  TargetMemoryStream,
  aMemoryStream : TStream;
Begin
 Mybmp      := Nil;
 aPackClass := Nil;
 vMonitor := StrToInt(Monitor) +1;
 aMonitor := IntToStr(FMX.Forms.Screen.DisplayCount);
 If (vMonitor > FMX.Forms.Screen.DisplayCount) then
  Exit;
 vMonitor := vMonitor -1;
 aResolution := Format('%s&%s&%s', [FloatToStr(Screen.Monitors[vMonitor].Height), FloatToStr(Screen.Monitors[vMonitor].Width), aMonitor]);
 Try
  vMultiPoint := False;
  If Not cRFB Then
   Begin
    vNewFrame   := FDuplication.GetFrame(False) Or (FullFrame);
    If vNewFrame Then
     Begin
      If Not FullFrame Then
       Begin
        Mybmp := Vcl.Graphics.TBitmap.Create;
        FDuplication.DrawFrame(Mybmp, PixelFormat);
        vMultiPoint := FDuplication.DirtyCount >= 1;
        If vMultiPoint Then
         Begin
          If FDuplication.DirtyCount = 1 Then
           Begin
           {$POINTERMATH ON}
            pRectTop    := FDuplication.DirtyRects[0].Top;
            pRectLeft   := FDuplication.DirtyRects[0].Left;
            pRectBottom := FDuplication.DirtyRects[0].Bottom;
            pRectRight  := FDuplication.DirtyRects[0].Right;
            vMultiPoint := Not((pRectTop   = 0) And
                               (pRectLeft  = 0) And
                               (pRectRight  = Screen.Monitors[vMonitor].Width) And
                               (pRectBottom = Screen.Monitors[vMonitor].Height));
           End;
         End;
       End
      Else
       Begin
        If Assigned(Mybmp) Then
         FreeAndNil(Mybmp);
        Mybmp := Vcl.Graphics.TBitmap.Create;
        Mybmp.Assign(aFullBmp);
       End;
     End;
   End
  Else
   Begin
    If FAegysVarredura.CaptureCursor <> DrawCur Then
     FAegysVarredura.CaptureCursor := DrawCur;
    vFirstImg   := Not FAegysVarredura.FirstScreen;
    FAegysVarredura.Process;
    vNewFrame   := (FAegysVarredura.FilaImagens.Count > 0);
    vMultiPoint := vNewFrame;
   End;
 Finally
 End;
 If vNewFrame Then
  Begin
   If Not cRFB Then
    If DrawCur Then
     DrawScreenCursor(Mybmp, StrToInt(Monitor));
   If Not vMultiPoint Then
    Begin
     TargetMemoryStream := TMemoryStream.Create;
     If Assigned(aFullBmp) Then
      FreeAndNil(aFullBmp);
     aFullBmp := Vcl.Graphics.TBitmap.Create;
     aFullBmp.Assign(Mybmp);
     Mybmp.SaveToStream(TargetMemoryStream);
     TargetMemoryStream.position := 0;
     If TargetMemoryStream.Size > 0 then
      Begin
       ZCompressStreamBytes(TargetMemoryStream, aFinalBytes);
       aPackClass  := TPackClass.Create;
       Try
        aPackClass.DataCheck    := tdcAsync;
        aPackClass.DataSize     := Length(aFinalBytes);
        aPackClass.ProxyToMyConnectionList := True;
        aPackClass.BufferSize   := aPackClass.DataSize;
        aPackClass.PacksGeral   := 0;
        aPackClass.PackNo       := 0;
        aPackClass.DataMode     := tdmClientCommand;
        aPackClass.DataType     := tdtDataBytes;
        aPackClass.CommandType  := tctScreenCapture;
        aPackClass.DataBytes    := aFinalBytes;
        aPackClass.BytesOptions := aResolution;
        aPackClass.Owner        := Conexao.Connection;
        aPackClass.Dest         := '';
       Finally
       End;
      End;
     FreeAndNil(TargetMemoryStream);
    End
   Else
    Begin
     aMemoryStream  := TMemoryStream.Create;
     If Not cRFB Then
      Begin
       aPackCountData := FDuplication.DirtyCount;
       aMemoryStream.Write(aPackCountData, SizeOf(aPackCountData));
       For I := 0 To FDuplication.DirtyCount -1 Do
        Begin
         With FDuplication.DirtyRects[I] Do
          Begin
           pRectTop    := Top;
           pRectLeft   := Left;
           pRectBottom := Bottom;
           pRectRight  := Right;
           MybmpPart := Vcl.Graphics.TBitmap.Create;
           Try
            MybmpPart.SetSize(pRectRight  - pRectLeft,
                              pRectBottom - pRectTop);
            MybmpPart.PixelFormat := PixelFormat;
            BitBlt(MybmpPart.Canvas.Handle, 0, 0,
                   pRectRight,              pRectBottom,
                   Mybmp.Canvas.Handle,     pRectLeft,
                   pRectTop,                SRCCOPY);
            BitBlt(aFullBmp.Canvas.Handle, pRectTop, pRectLeft,
                   pRectRight,              pRectBottom,
                   Mybmp.Canvas.Handle,     pRectLeft,
                   pRectTop,                SRCCOPY);
            TargetMemoryStream := TMemoryStream.Create;
            MybmpPart.SaveToStream(TargetMemoryStream);
            If TargetMemoryStream.Size > 0 Then
             Begin
              TargetMemoryStream.Position := 0;
              aSizeData                   := TargetMemoryStream.Size;
              aMemoryStream.Write   (pRectTop,           SizeOf(AeInteger));
              aMemoryStream.Write   (pRectLeft,          SizeOf(AeInteger));
              aMemoryStream.Write   (pRectBottom,        SizeOf(AeInteger));
              aMemoryStream.Write   (pRectRight,         SizeOf(AeInteger));
              aMemoryStream.Write   (aSizeData,          SizeOf(aSizeData));
              aMemoryStream.CopyFrom(TargetMemoryStream, aSizeData);
             End;
           Finally
            FreeAndNil(MybmpPart);
            FreeAndNil(TargetMemoryStream);
            Application.Processmessages;
           End;
          End;
        End;
      End
     Else
      Begin
       If Not vFirstImg Then
        Begin
         aPackCountData := FAegysVarredura.FilaImagens.Count;
         aMemoryStream.Write(aPackCountData, SizeOf(aPackCountData));
         For I := 0 To FAegysVarredura.FilaImagens.Count -1 Do
          Begin
           With FAegysVarredura.FilaImagens.Items[I] Do
            Begin
             pRectTop    := col;
             pRectLeft   := Line;
             pRectBottom := Imagem.Height;
             pRectRight  := Imagem.Width;
  //           MybmpPart := Vcl.Graphics.TBitmap.Create;
             Try
  //            MybmpPart.SetSize(pRectRight  - pRectLeft,
  //                              pRectBottom - pRectTop);
  //            MybmpPart.PixelFormat := PixelFormat;
  //            BitBlt(MybmpPart.Canvas.Handle, 0, 0,
  //                   pRectRight,              pRectBottom,
  //                   Imagem.Canvas.Handle,    pRectLeft,
  //                   pRectTop,                SRCCOPY);
              BitBlt(aFullBmp.Canvas.Handle, pRectTop, pRectLeft,
                     pRectRight,              pRectBottom,
                     Imagem.Canvas.Handle,    0,
                     0,                       SRCCOPY);
              TargetMemoryStream := TMemoryStream.Create;
              Imagem.SaveToStream(TargetMemoryStream);
              If TargetMemoryStream.Size > 0 Then
               Begin
                TargetMemoryStream.Position := 0;
                aSizeData                   := TargetMemoryStream.Size;
                aMemoryStream.Write   (pRectLeft,          SizeOf(AeInteger));
                aMemoryStream.Write   (pRectTop,           SizeOf(AeInteger));
                aMemoryStream.Write   (pRectBottom,        SizeOf(AeInteger));
                aMemoryStream.Write   (pRectRight,         SizeOf(AeInteger));
                aMemoryStream.Write   (aSizeData,          SizeOf(aSizeData));
                aMemoryStream.CopyFrom(TargetMemoryStream, aSizeData);
               End;
             Finally
  //            FreeAndNil(MybmpPart);
              FreeAndNil(TargetMemoryStream);
              Application.Processmessages;
             End;
            End;
          End;
        End
       Else
        Begin
         aPackCountData := 0;
         aFullBmp.PixelFormat := pf16bit;
         aFullBmp.SetSize(Screen.Width, Screen.Height);
         For I := 0 To FAegysVarredura.FilaImagens.Count -1 Do
          Begin
           With FAegysVarredura.FilaImagens.Items[I] Do
            Begin
             pRectTop    := col;
             pRectLeft   := Line;
             pRectBottom := Imagem.Height;
             pRectRight  := Imagem.Width;
             Try
              BitBlt(aFullBmp.Canvas.Handle, pRectTop, pRectLeft,
                     pRectRight,              pRectBottom,
                     Imagem.Canvas.Handle,    0,
                     0,                       SRCCOPY);
             Finally
             End;
            End;
          End;
         Application.Processmessages;
         TargetMemoryStream := TMemoryStream.Create;
         Try
          aMemoryStream.Position := 0;
//          aFullBmp.SaveToFile('0.bmp');
          aFullBmp.SaveToStream(aMemoryStream);
//          If TargetMemoryStream.Size > 0 Then
//           Begin
//            TargetMemoryStream.Position := 0;
//            aMemoryStream.CopyFrom(TargetMemoryStream, aSizeData);
//           End;
         Finally
//          FreeAndNil(TargetMemoryStream);
         End;
        End;
      End;
     aMemoryStream.Position  := 0;
     aPackClass              := TPackClass.Create;
     Try
      aPackClass.DataCheck    := tdcAsync;
      aPackClass.ProxyToMyConnectionList := True;
      aPackClass.PacksGeral   := aPackCountData;
      If aPackCountData = 0 Then
       aPackClass.PackNo      := 0
      Else
       aPackClass.PackNo      := 1;
      aPackClass.DataMode     := tdmClientCommand;
      aPackClass.DataType     := tdtDataBytes;
      aPackClass.CommandType  := tctScreenCapture;
//      SetLength(aBytes, aMemoryStream.Size);
//      aMemoryStream.Read(aBytes[0], Length(aBytes));
      ZCompressStreamBytes(aMemoryStream, aFinalBytes);
//      SetLength(aBytes, 0);
      aPackClass.DataSize     := Length(aFinalBytes);
      aPackClass.BufferSize   := aPackClass.DataSize;
      aPackClass.DataBytes    := aFinalBytes;
      aPackClass.BytesOptions := aResolution;
      aPackClass.Owner        := Conexao.Connection;
      aPackClass.Dest         := '';
     Finally
      SetLength(aFinalBytes, 0);
      FreeAndNil(aMemoryStream);
     End;
    End;
  End;
 If Assigned(Mybmp) Then
  FreeAndNil(Mybmp);
 If cRFB Then
  FAegysVarredura.FilaImagens.Clear;
End;

Initialization
 aFullBmp := Vcl.Graphics.TBitmap.Create;

Finalization
 FreeAndNil(aFullBmp);

end.

