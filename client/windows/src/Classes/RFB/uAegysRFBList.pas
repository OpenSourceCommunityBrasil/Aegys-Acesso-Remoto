Unit uAegysRFBList;

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

Interface

Uses
  Windows,  Messages, SysUtils, Variants, Classes, Vcl.Graphics,
  FastDIB,  FastSize, FConvert, Contnrs,  uAegysZlib, FastFiles,
  FMX.Forms;

Const
 BMPBlockSize = 64;
 MaxFilters   = 12;
 Filters      : Array [0 .. MaxFilters - 1] Of TResizeFilter = (rfBox,      rfBilinear,
                                                                rfHamming,  rfHermite,
                                                                rfBell,     rfCatrom,
                                                                rfBicubic,  rfSpline2,
                                                                rfMitchell, rfGaussian,
                                                                rfSpline3,  rfBlackman3);

Type
 TAegysBlock   = Class(TPersistent)
 Private
  Constructor Create;
  Destructor  Destroy; Override;
 Public
  P,
  LineCol : TPoint;
  Change  : Boolean;
  BMP     : TFastDIB;
 End;
 TBlockList    = Class(TObjectList)
 Private
  Function  GetItem    (Index       : Integer)     : TAegysBlock;
  Procedure SetItem    (Index       : Integer;
                        Const Value : TAegysBlock);
 Public
  Function  New                                    : TAegysBlock;
  Function  FindLineCol(Line,
                        Col     : Integer)         : TAegysBlock;
  Property  Items      [Index   : Integer]         : TAegysBlock Read GetItem Write SetItem;
  Procedure ChangeSize (Size    : Integer);
 End;
 TAegysCapture = Class(TPersistent)
 Private
  FImagem         : TBitmap;
  FLine,
  FCol,
  FSize           : Integer;
  Procedure SetCol   (Const Value : Integer);
  Procedure SetImagem(Const Value : TBitmap);
  Procedure SetLine  (Const Value : Integer);
  Procedure SetSize  (Const Value : Integer);
 Published
  Property  Line  : Integer Read FLine Write SetLine;
  Property  Col   : Integer Read FCol  Write SetCol;
  Property  Size  : Integer Read FSize Write SetSize;
  Constructor Create;
  Destructor  Destroy; override;
 Public
  Property Imagem : TBitmap Read FImagem Write SetImagem;
 End;
 TListCaptures = Class(TObjectList)
 Private
  Function  GetItem(Index       : Integer) : TAegysCapture; /// GetItem
  Procedure SetItem(Index       : Integer;
                    Const Value : TAegysCapture); /// SetItem
 Public
  Function  New                 : TAegysCapture;
  Property  Items[Index         : Integer] : TAegysCapture Read GetItem Write SetItem;
 End;
 TAegysBuffers = Class(TPersistent)
 Private
 Public
  Buffer : TStringStream;
  Constructor Create;
  Destructor  Destroy; override;
 Published
 End;
 TAegysRecordBuffers = Class(TObjectList)
 Private
  Function  GetItem(Index       : Integer) : TAegysBuffers;
  Procedure SetItem(Index       : Integer;
                    Const Value : TAegysBuffers);
 Public
  Function New          : TAegysBuffers;
  Function Exists(Index : Integer ) : Boolean;
  Property Items [Index : Integer]  : TAegysBuffers Read GetItem Write SetItem;
 End;
 TCoordinates    = Class(TPersistent)
 Public
  Pos,
  Tam : TPoint;
 End;
 TCoordinateList = Class(TObjectList)
 Private
  Function  GetItem(Index            : Integer) : TCoordinates;
  Procedure SetItem(Index            : Integer;
                    Const Value      : TCoordinates);
 Public
  Function  New                      : TCoordinates;
  Property  Items  [Index : Integer] : TCoordinates Read GetItem Write SetItem;
  Function  FindCoordinates(YFim,
                            IniX,
                            FimX     : Integer) : TCoordinates;
 End;
 TAegysVarredura = Class
 Protected
  WindowsRect       : TRect;
  FHandleCapture    : Cardinal;
  ScreenTemp,
  ScreenBMP         : TFastDIB;
  ScreenWidthReal,
  ScreenHeightReal  : Integer;
  Procedure DrawCursor(ScreenShotBitmap : TFastDIB);
  Procedure ScreenShoot;
  Procedure GetFirstBMP;
  Procedure GetNextBMP;
 Private
  FPixels,
  BlockCount,
  FQuality,
  FZoom,
  LastBlockX,
  LastBlockY       : Integer;
  BlockList        : TBlockList;
  FSrcActual,
  FSrcAnterior     : TFastDIB;
  FSrcSize         : TPoint;
  FSquare,
  FCaptureCursor,
  FLine            : Boolean;
  FToleranceChange : Double;
  Procedure SetZoom    (Const Value : Integer);
  Procedure DoResizeInt(Src,
                        Dst,
                        IntBuf      : TFastDIB;
                        Idx         : Integer;
                        MMX,
                        Sharp       : Boolean);
  Function  CompBMP    (Bmp1,
                        Bmp2        : TFastDIB;
                        Tolerance   : Double)      : Boolean;
  Procedure SetLine    (Const Value : Boolean);
  Procedure SetPixels  (Const Value : Integer);
  Function FDIB_DrawFromWin(Src     : THandle;
                            Dst     : TFastDIB;
                            PosX    : Integer = 0;
                            PosY    : Integer = 0) : Boolean;
  Procedure SetQuality        (Const Value : Integer);
  Procedure BuildLines;
  Procedure BuildSquares;
  Procedure SetToleranceChange(Const Value : Double);
  Procedure SetSquare         (Const Value : Boolean);
  Procedure SetCaptureCursor  (Const Value : Boolean);
 Public
  ScreenWidth,
  ScreenHeight    : Integer;
  FilaImagens     : TListCaptures;
  FirstScreen     : Boolean;
  Property Zoom            : Integer Read FZoom            Write SetZoom;
  Property Line            : Boolean Read FLine            Write SetLine;
  Property Square          : Boolean Read FSquare          Write SetSquare;
  Property CaptureCursor   : Boolean Read FCaptureCursor   Write SetCaptureCursor;
  Property Pixels          : Integer Read FPixels          Write SetPixels;
  Property Quality         : Integer Read FQuality         Write SetQuality;
  Property ToleranceChange : Double  Read FToleranceChange Write SetToleranceChange;
  Procedure   Process;
  Procedure   ProcessBuffer( NumBuffers: Integer; var Lista: TAegysRecordBuffers );
  Constructor Create(Const HandleCap : Cardinal = 0);
  Destructor  Destroy; Override;
 End;

Implementation

Function TAegysVarredura.FDIB_DrawFromWin(Src  : THandle;
                                          Dst  : TFastDIB;
                                          PosX : Integer = 0;
                                          PosY : Integer = 0) : Boolean;
Var
 DC : HDC;
Begin
 Result   := (Dst.HDC <> 0);
 If (Result) Then
  Begin
   DC     := GetDC(Src);
   Result := (DC <> 0);
   If Result Then
    Begin
     BitBlt(Dst.HDC, 0, 0, Dst.Width, Dst.Height, DC, PosX, PosY, SRCCOPY);
     ReleaseDC(Src, DC);
    End;
  End;
End;

Procedure TAegysVarredura.Process;
Var
 I        : Integer;
 TamNew,
 PosNew   : TPoint;
 BmpTemp  : TBitmap;
 First    : Boolean;
Begin
 First := False;
 If FirstScreen Then
  GetNextBMP
 Else
  Begin
   GetFirstBMP;
   First := True;
  End;
 If Square     And
   (Not First) Then
  BuildSquares
 Else If Line Then
  BuildLines
 Else
  Begin
   For I := 0  To BlockList.Count -1 Do
    Begin
     If BlockList.Items[I].Change Then
      Begin
       TamNew.X := BlockList.Items[I].BMP.Width;
       TamNew.Y := BlockList.Items[I].BMP.Height;
       PosNew.X := BlockList.Items[I].P.X ;
       PosNew.Y := BlockList.Items[I].P.Y ;
       With FilaImagens.New Do
        Begin
         Line := PosNew.Y;
         Col := PosNew.X;
         Size := 0;
         Imagem := TBitmap.Create;
         Imagem.Width := TamNew.X;
         Imagem.Height := TamNew.Y;
         BlockList.Items[I].BMP.Draw(Imagem.Canvas.Handle, 0, 0);
//         Imagem := TBitmap.Create;
//         Imagem.Assign( BmpTemp );
//         Imagem.CompressionQuality := Quality;
//         Imagem.Compress;
//         BmpTemp.Free;
//         BmpTemp := nil;
        End;
      End;
    End;
   Application.Processmessages;
  End;
End;

Procedure TAegysVarredura.ProcessBuffer(NumBuffers : Integer;
                                        Var Lista  : TAegysRecordBuffers);
Var
 I, Img,
 QuantosChange,
 QuantosPorBuffer,
 ColocadoNoBuffer,
 BufferActual,
 Ssize             : Integer;
 TamNew,
 PosNew            : TPoint;
 BmpTemp           : TBitmap;
//   ImgTemp: TJPEGImage;
 TmpStream         : TMemoryStream;
Begin
 Lista.Clear;
 Img              := 0;
 QuantosChange    := 0;
 QuantosPorBuffer := 0;
 ColocadoNoBuffer := 0;
 BufferActual     := 0;
 Img              := 0;
 If FirstScreen Then
  GetNextBMP
 Else
  GetFirstBMP;
 If Line Then
  BuildLines
 Else
  Begin
   For I := 0  to BlockList.Count - 1 do
    Begin
     If BlockList.Items[I].Change Then
      Begin
       TamNew.X := BlockList.Items[I].BMP.Width ;
       TamNew.Y := BlockList.Items[I].BMP.Height ;
       PosNew.X := BlockList.Items[I].P.X ;
       PosNew.Y := BlockList.Items[I].P.Y;
       With FilaImagens.New Do
        Begin
         Line          := PosNew.Y;
         Col           := PosNew.X;
         Size          := 0;
         Imagem        := TBitmap.Create;
         Imagem.Width  := TamNew.X;
         Imagem.Height := TamNew.Y;
         BlockList.Items[I].BMP.Draw(Imagem.Canvas.Handle, 0, 0 );
//                              Imagem := TJPEGImage.Create;
//                              Imagem.Assign(BmpTemp);
//                              Imagem.CompressionQuality := Quality;
//                              Imagem.Compress;
//                              BmpTemp.Free;
//                              BmpTemp := nil;

        End;
       Inc(Img);
      End;
    End;
  End;
 QuantosPorBuffer := FilaImagens.Count div NumBuffers;
 While FilaImagens.Count > 0 Do
  Begin
   If Not Lista.Exists(BufferActual) Then
    Begin
     With Lista.New Do
      Begin
       Buffer.Clear;
       Buffer.Size := 0;
      End;
    End;
   PosNew.Y           := FilaImagens.Items[0].Line;
   PosNew.X           := FilaImagens.Items[0].Col;
   TmpStream          := TMemoryStream.Create;
   FilaImagens.Items[0].FImagem.SaveToStream( TmpStream );
   TmpStream.Position := 0;
   Lista.Items[BufferActual].Buffer.WriteBuffer( PosNew, SizeOf( TPoint ));
   Ssize              := TmpStream.Size;
   Lista.Items[BufferActual].Buffer.WriteBuffer( Ssize, SizeOf( Ssize ) );
   TmpStream.Position := 0;
   Lista.Items[BufferActual].Buffer.CopyFrom( TmpStream, 0 );
   FreeAndNil(TmpStream);
   FilaImagens.Delete(0);
   Inc(ColocadoNoBuffer);
   If (ColocadoNoBuffer  = QuantosPorBuffer) And
      (Not (BufferActual = (NumBuffers -1))) Then
    Begin
     Inc(BufferActual);
     ColocadoNoBuffer := 0;
    End;
  End;
End;

Constructor TAegysVarredura.Create(const HandleCap : Cardinal = 0);
Var
 I : Integer;
Begin
 If HandleCap = 0 Then
  FHandleCapture  := GetDesktopWindow
 Else
  FHandleCapture  := HandleCap;
 GetWindowRect(FHandleCapture, WindowsRect);
 BlockList        := TBlockList.Create;
 FirstScreen      := False;
 FCaptureCursor   := False;
 FZoom            := 100;
 FLine            := True;
 FSquare          := False;
 FPixels          := 16;
 Quality          := 80;
 FToleranceChange := 0;
 ScreenBMP        := TFastDIB.Create;
 ScreenTemp       := TFastDIB.Create;
 ScreenWidthReal  := WindowsRect.Right  - WindowsRect.Left;
 ScreenHeightReal := WindowsRect.Bottom - WindowsRect.Top;
 ScreenWidth      := (ScreenWidthReal   * Zoom) Div 100;
 ScreenHeight     := (ScreenHeightReal  * Zoom) Div 100;
 BlockCount       := (ScreenWidth Div BMPBlockSize) * (ScreenHeight Div BMPBlockSize);
 LastBlockX       := (BMPBlockSize + (ScreenWidth  - ((ScreenWidth  Div BMPBlockSize) * BMPBlockSize)));
 LastBlockY       := (BMPBlockSize + (ScreenHeight - ((ScreenHeight Div BMPBlockSize) * BMPBlockSize)));
 BlockList.ChangeSize(BlockCount);
 FilaImagens      := TListCaptures.Create;
 Inherited Create;
End;

Destructor TAegysVarredura.Destroy;
Var
 i : Integer;
Begin
 FreeAndNil(ScreenTemp);
 FreeAndNil(ScreenBMP);
 While BlockList.Count > 0 Do
  Begin
   BlockList.Items[0].BMP.Free;
   BlockList.Items[0].BMP := Nil;
   BlockList.Delete( 0 );
  End;
 BlockList.Clear;
 BlockList.Free;
 BlockList := Nil;
 While FilaImagens.Count > 0 Do
  Begin
   FilaImagens.Items[0].Imagem.free;
   FilaImagens.Items[0].Imagem := nil;
   FilaImagens.Delete( 0);
  End;
 FreeAndNil(FilaImagens);
 Inherited destroy;
End;

Procedure TAegysVarredura.DoResizeInt(Src,
                                      Dst,
                                      IntBuf : TFastDIB;
                                      Idx    : Integer;
                                      MMX,
                                      Sharp  : Boolean);
Var
 rf : TResizeFilter;
Begin
 Case Idx of
  0 : FastResizeTo24(Src, Dst);
  2 : BilinearTo24(Src, Dst);
  3 : ;
  1, 4 : Begin
          If Idx = 4 Then
           SetStretchBltMode(Dst.hDC, HALFTONE)
          Else
           SetStretchBltMode(Dst.hDC, COLORONCOLOR);
          StretchBlt(Dst.hDC, 0, 0, Dst.Width, Dst.Height,
                     Src.hDC, 0, 0, Src.Width, Src.Height, SRCCOPY);
         End;
  Else
   Begin
    rf := Filters[Idx-5]; // excluding rfSinC3 - buggy filter
    SmoothResize(Src, Dst, rf, MMX, IntBuf);
   End;
 End;
End;

Function TAegysVarredura.CompBMP(Bmp1,
                                 Bmp2      : TFastDIB;
                                 Tolerance : Double)   : Boolean;
Var
 Tole,
 TotPixel,
 TotAlt,
 x, y, i : Integer;
 c1, c2  : PFColor;
Begin
 Result   := True;
 i        := 0;
 x        := 0;
 TotPixel := 0;
 TotAlt   := 0;
 GetMem(c1, SizeOf(TFColor));
 GetMem(c2, SizeOf(TFColor));
 While x < Bmp1.Width Do
  Begin
   y := 0;
   While y < Bmp1.Height Do
    Begin
     c1^ := Bmp1.Pixels[y, x];
     c2^ := Bmp2.Pixels[y, x];
     inc(TotPixel);
     If (c1^.r <> c2^.r) Or
        (c1^.g <> c2^.g) Or
        (c1^.b <> c2^.b) Then
      Inc(i);
     If i > 1 Then
      Break;
     Inc(y, 1);
    End;
   Tole := Round((TotPixel * Tolerance) / 100);
   If (i > Tole) Then
    Begin
     Result := False;
     Break;
    End;
   Inc(x, 1);
  End;
 FreeMem(c1);
 FreeMem(c2);
End;

Procedure TAegysVarredura.ScreenShoot;
Var
 DC    : HDC;
 Temp  : TFastDIB;
begin
 DC    := GetDC(FHandleCapture);
 Try
  Temp := TFastDIB.Create;
  Temp.SetSize(ScreenWidthReal,
               ScreenHeightReal,
               FPixels);
  Temp.Bpp := 16;
  BitBlt(Temp.hDC, 0, 0, ScreenWidthReal, ScreenHeightReal, DC, 0, 0, SRCCOPY);
  If Temp.Bpp <> Pixels Then
   ConvertTo(Temp, Pixels);
  ScreenBMP.SetSize(ScreenWidth, ScreenHeight, FPixels);
  DoResizeInt(Temp, ScreenBMP, nil, 4, True, False);
 Finally
  FreeAndNil(Temp);
  ReleaseDC(FHandleCapture, DC);
 End;
End;


Procedure TAegysVarredura.DrawCursor(ScreenShotBitmap : TFastDIB);
Var
 r    : TRect;
 CI   : TCursorInfo;
 Icon : TIcon;
 II   : TIconInfo;
Begin
 Icon := TIcon.Create;
 Try
  CI.cbSize := SizeOf(CI);
  If GetCursorInfo(CI) Then
   Begin
    If CI.Flags = CURSOR_SHOWING Then
     Begin
      Icon.Handle := CopyIcon(CI.hCursor);
      If GetIconInfo(Icon.Handle, II) Then
       Begin
        R.Left   := 0;
        R.Top    := 0;
        R.Right  := icon.Width;
        R.Bottom := Icon.Height;
        ScreenShotBitmap.DrawRect(Icon.Handle, R, ci.ptScreenPos.x, ci.ptScreenPos.y);
       End;
     End;
   End;
 Finally
  FreeAndNil(Icon);
 End;
End;

procedure TAegysVarredura.BuildLines;
Var
 Coordinates : TCoordinateList;
 I, ColAnt,
 QuantLines,
 QuantCols,
 Lin, Col    : Integer;
 Pini,
 PFim,
 TamNew,
 PosNew      : TPoint;
 FindFirst   : Boolean;
 ILine       : TFastDIB;
 BmpTemp     : TBitmap;
 BlockActual : TAegysBlock;
Begin
 Coordinates := TCoordinateList.Create;
 QuantLines  := ScreenHeight Div BMPBlockSize;
 QuantCols   := ScreenWidth  Div BMPBlockSize;
 FindFirst   := False;
 ColAnt      := 0;
 TamNew.X    := 0;
 TamNew.Y    := 0;
 PosNew.X    := 0;
 PosNew.Y    := 0;
 Pini.X      := 0;
 Pini.Y      := 0;
 PFim.X      := 0;
 PFim.Y      := 0;
 For Lin := 1 To QuantLines Do
  Begin
   FindFirst := False;
   For Col := 1 To QuantCols Do
    Begin
     BlockActual := BlockList.FindLineCol(Lin, Col);
     If Assigned(BlockActual) Then
      Begin
       With BlockActual Do
        Begin
         If Change Then
          Begin
           If Not FindFirst Then
            Begin
             ColAnt        := Col;
             Pini.X        := P.X;
             Pini.Y        := P.Y;
             PFim.X        := BMP.Width;
             PFim.Y        := BMP.Height;
             FindFirst     := True;
             If Col = QuantCols Then
              Begin
               With Coordinates.New Do
                Begin
                 Pos.X     := Pini.X;
                 Pos.Y     := Pini.Y;
                 Tam.X     := PFim.X;
                 Tam.Y     := PFim.Y;
                End;
               Pini.X      := 0;
               Pini.Y      := 0;
               PFim.X      := 0;
               PFim.Y      := 0;
               ColAnt      := 0;
               FindFirst   := False;
              End;
            End
           Else
            Begin
             If ColAnt = (Col -1) Then
              Begin
               ColAnt      := Col;
               PFim.X      := PFim.X + BMP.Width;
               If Col  = QuantCols Then
                Begin
                 With Coordinates.New Do
                  Begin
                   Pos.X   := Pini.X;
                   Pos.Y   := Pini.Y;
                   Tam.X   := PFim.X;
                   Tam.Y   := PFim.Y;
                  End;
                 Pini.X    := 0;
                 Pini.Y    := 0;
                 PFim.X    := 0;
                 PFim.Y    := 0;
                 ColAnt    := 0;
                 FindFirst := False;
                End;
              End
             Else
              Begin
               With Coordinates.New Do
                Begin
                 Pos.X     := Pini.X;
                 Pos.Y     := Pini.Y;
                 Tam.X     := PFim.X;
                 Tam.Y     := PFim.Y;
                End;
               ColAnt      := Col;
               Pini.X      := P.X;
               Pini.Y      := P.Y;
               PFim.X      := BMP.Width;
               PFim.Y      := BMP.Height;
               FindFirst   := True;
               If Col = QuantCols Then
                Begin
                 With Coordinates.New Do
                  Begin
                   Pos.X   := Pini.X;
                   Pos.Y   := Pini.Y;
                   Tam.X   := PFim.X;
                   Tam.Y   := PFim.Y;
                  End;
                 Pini.X    := 0;
                 Pini.Y    := 0;
                 PFim.X    := 0;
                 PFim.Y    := 0;
                 ColAnt    := 0;
                 FindFirst := False;
                End;
              End;
            End;
          End
         Else
          Begin
           If FindFirst         And
              (Col = QuantCols) Then
            Begin
             With Coordinates.New Do
              Begin
               Pos.X   := Pini.X;
               Pos.Y   := Pini.Y;
               Tam.X   := PFim.X;
               Tam.Y   := PFim.Y;
              End;
             Pini.X    := 0;
             Pini.Y    := 0;
             PFim.X    := 0;
             PFim.Y    := 0;
             ColAnt    := 0;
             FindFirst := False;
            End;
          End;
        End;
      End;
    End;
   Pini.X := 0;
   Pini.Y := 0;
   PFim.X := 0;
   PFim.Y := 0;
   ColAnt := 0;
   FindFirst := False;
  End;
 While Coordinates.Count > 0 do
  Begin
   Try
    With Coordinates.Items[0] do
     Begin
      ILine    := TFastDIB.Create;
      ILine.SetSize (Tam.X, Tam.Y, FPixels);
      ILine.CopyRect(ScreenBMP, 0, 0, Tam.X, Tam.Y, Pos.X, Pos.Y);
      TamNew.X := Tam.X;
      TamNew.Y := Tam.Y;
      PosNew.X := Pos.X;
      PosNew.Y := Pos.Y;
      With FilaImagens.New Do
       Begin
        Line          := PosNew.Y;
        Col           := PosNew.X;
        Size          := 0;
        Imagem        := TBitmap.Create;
        Imagem.Width  := TamNew.X;
        Imagem.Height := TamNew.Y;
        ILine.Draw(Imagem.Canvas.Handle, 0, 0);
//                                Imagem := TJPEGImage.Create;
//                                Imagem.Assign( BmpTemp );
//                                Imagem.CompressionQuality := Quality;
//                                Imagem.Compress;
//                                BmpTemp.Free;
//                                BmpTemp := nil;
       End;
      FreeAndNil(ILine);
     End;
    Coordinates.Delete(0);
   Except
    Coordinates.Delete(0);
   End;
  End;
 FreeAndNil(Coordinates);
End;

Procedure TAegysVarredura.BuildSquares;
Var
 CoordinatesTemp : TCoordinates;
 Coordinates     : TCoordinateList;
 I, ColAnt,
 QuantLines,
 QuantCols,
 Lin, Col    : Integer;
 Pini,
 PFim,
 TamNew,
 PosNew      : TPoint;
 FindFirst   : Boolean;
 ILine       : TFastDIB;
 BmpTemp     : TBitmap;
 BlockActual : TAegysBlock;
Begin
 Coordinates := TCoordinateList.Create;
 QuantLines  := ScreenHeight Div BMPBlockSize;
 QuantCols   := ScreenWidth  Div BMPBlockSize;
 FindFirst   := False;
 ColAnt      := 0;
 TamNew.X    := 0;
 TamNew.Y    := 0;
 PosNew.X    := 0;
 PosNew.Y    := 0;
 Pini.X      := 0;
 Pini.Y      := 0;
 PFim.X      := 0;
 PFim.Y      := 0;
 For Lin := 1 To QuantLines Do
  Begin
   FindFirst := False;
   For Col := 1 To QuantCols Do
    Begin
     BlockActual := BlockList.FindLineCol(Lin, Col);
     If Assigned(BlockActual) Then
      Begin
       With BlockActual do
        Begin
         If  Change Then
          Begin
           If Not FindFirst Then
            Begin
             ColAnt    := Col;
             Pini.X    := P.X;
             Pini.Y    := P.Y;
             PFim.X    := BMP.Width;
             PFim.Y    := BMP.Height;
             FindFirst := True;
             If Col    = QuantCols Then
              Begin
               CoordinatesTemp := Coordinates.FindCoordinates(Pini.Y, Pini.X, PFim.X);
               If Assigned(CoordinatesTemp) Then
                Begin
                 With CoordinatesTemp Do
                  Tam.Y := Tam.Y + PFim.Y;
                End
               Else
                Begin
                 With Coordinates.New Do
                  Begin
                   Pos.X := Pini.X;
                   Pos.Y := Pini.Y;
                   Tam.X := PFim.X;
                   Tam.Y := PFim.Y;
                  End;
                End;
               Pini.X    := 0;
               Pini.Y    := 0;
               PFim.X    := 0;
               PFim.Y    := 0;
               ColAnt    := 0;
               FindFirst := False;
              End;
            End
           Else
            Begin
             If ColAnt = (Col -1) Then
              Begin
               ColAnt := Col;
               PFim.X := PFim.X + BMP.Width;
               If Col = QuantCols then
                Begin
                 CoordinatesTemp := Coordinates.FindCoordinates(Pini.Y, Pini.X, PFim.X);
                 If Assigned(CoordinatesTemp) Then
                  Begin
                   With CoordinatesTemp Do
                    Tam.Y := Tam.Y + PFim.Y;
                  End
                 Else
                  Begin
                   With Coordinates.New Do
                    Begin
                     Pos.X := Pini.X;
                     Pos.Y := Pini.Y;
                     Tam.X := PFim.X;
                     Tam.Y := PFim.Y;
                    End;
                  End;
                 Pini.X    := 0;
                 Pini.Y    := 0;
                 PFim.X    := 0;
                 PFim.Y    := 0;
                 ColAnt    := 0;
                 FindFirst := False;
                End;
              End
             Else
              Begin
               CoordinatesTemp := Coordinates.FindCoordinates(Pini.Y, Pini.X, PFim.X);
               If Assigned(CoordinatesTemp) Then
                Begin
                 With CoordinatesTemp do
                  Tam.Y := Tam.Y + PFim.Y;
                End
               Else
                Begin
                 With Coordinates.New Do
                  Begin
                   Pos.X := Pini.X;
                   Pos.Y := Pini.Y;
                   Tam.X := PFim.X;
                   Tam.Y := PFim.Y;
                  End;
                End;
               ColAnt := Col;
               Pini.X := P.X;
               Pini.Y := P.Y;
               PFim.X := BMP.Width;
               PFim.Y := BMP.Height;
               FindFirst := True;
               If Col = QuantCols Then
                Begin
                 CoordinatesTemp := Coordinates.FindCoordinates(Pini.Y, Pini.X, PFim.X);
                 If Assigned(CoordinatesTemp) Then
                  Begin
                   With CoordinatesTemp Do
                    Tam.Y := Tam.Y + PFim.Y;
                  End
                 Else
                  Begin
                   With Coordinates.New Do
                    Begin
                     Pos.X := Pini.X;
                     Pos.Y := Pini.Y;
                     Tam.X := PFim.X;
                     Tam.Y := PFim.Y;
                    End;
                  End;
                 Pini.X    := 0;
                 Pini.Y    := 0;
                 PFim.X    := 0;
                 PFim.Y    := 0;
                 ColAnt    := 0;
                 FindFirst := False;
                End;
              End;
            End;
          End
         Else
          Begin
           If FindFirst        And
             (Col = QuantCols) Then
            Begin
             CoordinatesTemp := Coordinates.FindCoordinates(Pini.Y, Pini.X, PFim.X);
             If Assigned(CoordinatesTemp) Then
              Begin
               With CoordinatesTemp Do
                Tam.Y := Tam.Y + PFim.Y;
              End
             Else
              Begin
               With Coordinates.New do
                Begin
                 Pos.X := Pini.X;
                 Pos.Y := Pini.Y;
                 Tam.X := PFim.X;
                 Tam.Y := PFim.Y;
                End;
              End;
             Pini.X    := 0;
             Pini.Y    := 0;
             PFim.X    := 0;
             PFim.Y    := 0;
             ColAnt    := 0;
             FindFirst := False;
            End;
          End;
        End;
      End;
    End;
   Pini.X := 0;
   Pini.Y := 0;
   PFim.X := 0;
   PFim.Y := 0;
   ColAnt := 0;
   FindFirst := False;
  End;
 While Coordinates.Count > 0 do
  Begin
   Try
    with Coordinates.Items[0] do
     Begin
      ILine    := TFastDIB.Create;
      ILine.SetSize(Tam.X, Tam.Y, FPixels);
      ILine.CopyRect(ScreenBMP, 0, 0, Tam.X, Tam.Y, Pos.X, Pos.Y);
      TamNew.X := Tam.X;
      TamNew.Y := Tam.Y;
      PosNew.X := Pos.X;
      PosNew.Y := Pos.Y;
      With FilaImagens.New do
       Begin
        Line          := PosNew.Y;
        Col           := PosNew.X;
        Size          := 0;
        Imagem        := TBitmap.Create;
        Imagem.Width  := TamNew.X;
        Imagem.Height := TamNew.Y;
        ILine.Draw(Imagem.Canvas.Handle, 0, 0 );
//                                Imagem := TJPEGImage.Create;
//                                Imagem.Assign( BmpTemp );
//                                Imagem.CompressionQuality := Quality;
//                                Imagem.Compress;
//                                BmpTemp.Free;
//                                BmpTemp := nil;
       end;
      FreeAndNil(ILine);
     End;
    Coordinates.Delete(0);
   Except
    Coordinates.Delete(0);
   End;
  End;
 FreeAndNil(Coordinates);
End;

Procedure TAegysVarredura.GetFirstBMP;
Var
 x, y, i,
 Ssize,
 TamX, TamY,
 Lin, Col    : Integer;
Begin
 x           := 0;
 i           := 0;
 Lin         := 1;
 Col         := 1;
 ScreenShoot;
 TamX        := BMPBlockSize;
 TamY        := BMPBlockSize;
 FirstScreen := True;
 Application.Processmessages;
 While x < ScreenBMP.Width Do
  Begin
   y := 0;
   While y < ScreenBMP.Height Do
    Begin
     BlockList.Items[I].P.X := x;
     BlockList.Items[I].P.Y := y;
     If Not Assigned(BlockList.Items[I].BMP) Then
      BlockList.Items[I].BMP := TFastDIB.Create;
     If ((Y + LastBlockY) < (ScreenBMP.Height -1)) And
        ((x + LastBlockX) < (ScreenBMP.Width  -1)) Then
      Begin
       TamX := BMPBlockSize;
       TamY := BMPBlockSize;
      End;
     If (Y + LastBlockY) = (ScreenBMP.Height) Then
      TamY := LastBlockY
     Else
      TamY := BMPBlockSize;
     If (X + LastBlockX ) = (ScreenBMP.Width) Then
      TamX := LastBlockX
     Else
      TamX := BMPBlockSize;
     If (BlockList.Items[I].BMP.Width  <> TamX) Or
        (BlockList.Items[I].BMP.Height <> TamY) Then
      BlockList.Items[I].BMP.SetSize(TamX, TamY, FPixels);
     BlockList.Items[I].BMP.CopyRect(ScreenBMP, 0, 0, TamX, TamY, x, y);
     BlockList.Items[I].Change    := True;
     BlockList.Items[I].LineCol.Y := Lin;
     BlockList.Items[I].LineCol.X := Col;
     Inc(i);
     Inc(y, TamY);
     inc(Lin);
    End;
   Inc(x, TamX);
   Inc(Col);
   Lin := 1;
  End;
End;

Procedure TAegysVarredura.GetNextBMP;
Var
 x, y, i,
 TamX, TamY,
 Ssize, Lin,
 Col         : Integer;
Begin
 x := 0;
 i := 0;
 Application.Processmessages;
 ScreenShoot;
 TamX := BMPBlockSize;
 TamY := BMPBlockSize;
 Lin := 1;
 Col := 1;
 While x < ScreenBMP.Width Do
  Begin
   y := 0;
   While y < ScreenBMP.Height Do
    Begin
     BlockList.Items[I].P.X := x;
     BlockList.Items[I].P.Y := y;
     If ((Y + LastBlockY) < (ScreenBMP.Height -1)) And
        ((x + LastBlockX) < (ScreenBMP.Width  -1)) Then
      Begin
       TamX := BMPBlockSize;
       TamY := BMPBlockSize;
      End;
     If (Y + LastBlockY)  = ScreenBMP.Height Then
      TamY := LastBlockY
     Else
      TamY := BMPBlockSize;
     If (X + LastBlockX ) = ScreenBMP.Width  Then
      TamX := LastBlockX
     Else
      TamX := BMPBlockSize;
     If (ScreenTemp.Width  <> TamX) Or
        (ScreenTemp.Height <> TamY) Then
      ScreenTemp.SetSize(Tamx, TamY, FPixels);
     ScreenTemp.CopyRect(ScreenBMP, 0, 0, TamX, TamY, x, y);
     If Not CompBMP(BlockList.Items[I].BMP,
                    ScreenTemp,
                    FToleranceChange) Then
      Begin
       If Not Assigned(BlockList.Items[I].BMP) Then
        BlockList.Items[I].BMP    := TFastDIB.Create;
       BlockList.Items[I].BMP.CopyRect(ScreenTemp, 0, 0, TamX, TamY, 0, 0);
       BlockList.Items[I].Change  := True;
      End
     Else
      BlockList.Items[I].Change   := False;
     BlockList.Items[I].LineCol.X := Col;
     BlockList.Items[I].LineCol.Y := Lin;
     Inc(i);
     Inc(y, TamY);
     Inc(Lin);
    End;
   Inc(x, TamX);
   Inc(Col);
   Lin := 1;
  End;
End;

Procedure TAegysVarredura.SetCaptureCursor(Const Value : Boolean);
Begin
 FCaptureCursor := Value;
End;

Procedure TAegysVarredura.SetLine  (Const Value : Boolean);
Begin
 FLine := Value;
End;

Procedure TAegysVarredura.SetPixels(Const Value : Integer);
Begin
 FPixels := Value;
End;

Procedure TAegysVarredura.SetSquare(Const Value : Boolean);
Begin
 FSquare := Value;
End;

Procedure TAegysVarredura.SetQuality(const Value : Integer);
Begin
 FQuality := Value;
End;

Procedure TAegysVarredura.SetToleranceChange(Const Value : Double);
Begin
 FToleranceChange := Value;
End;

procedure TAegysVarredura.SetZoom(Const Value : Integer);
Var
 I : Integer;
Begin
 FZoom := Value;
 If Not FirstScreen Then
  Begin
   ScreenWidthReal  := WindowsRect.Right  - WindowsRect.Left;
   ScreenHeightReal := WindowsRect.Bottom - WindowsRect.Top;
   ScreenWidth      := (ScreenWidthReal   * Zoom) Div 100;
   ScreenHeight     := (ScreenHeightReal  * Zoom) Div 100;
   BlockCount       := (ScreenWidth Div BMPBlockSize) * (ScreenHeight Div BMPBlockSize);
   LastBlockX       := (BMPBlockSize + (ScreenWidth  - ((ScreenWidth  Div BMPBlockSize) * BMPBlockSize)));
   LastBlockY       := (BMPBlockSize + (ScreenHeight - ((ScreenHeight Div BMPBlockSize) * BMPBlockSize)));
   BlockList.ChangeSize(BlockCount);
  End;
End;

{ TAegysCapture }

Constructor TAegysCapture.Create;
Begin
 Inherited;
End;

Destructor TAegysCapture.Destroy;
Begin
 If Assigned(FImagem) Then
  FreeAndNil(FImagem);
 Inherited;
End;

Procedure TAegysCapture.SetCol(Const Value : Integer);
Begin
 FCol := Value;
End;

Procedure TAegysCapture.SetImagem(Const Value : TBitmap);
Begin
 FImagem := Value;
End;

Procedure TAegysCapture.SetLine  (Const Value : Integer);
Begin
 FLine := Value;
End;

Procedure TAegysCapture.SetSize  (Const Value : Integer);
Begin
 FSize := Value;
End;

{ TListCaptures }

Function TListCaptures.GetItem(Index: Integer): TAegysCapture;
Begin
 Result := TAegysCapture(Inherited Items[Index]);
End;

Function TListCaptures.New: TAegysCapture;
Begin
 Result := TAegysCapture.Create;
 Add(Result);
End;

procedure TListCaptures.SetItem(Index       : Integer;
                                Const Value : TAegysCapture);
Begin
 Put(Index, Value);
End;

{ TAegysRecordBuffers }

Function TAegysRecordBuffers.Exists(Index : Integer) : Boolean;
Var
 I : Integer;
Begin
 Result := Self.Count = 0;
 If Not Result Then
  Begin
   For I := 0 To Self.Count -1 Do
    Begin
     Result := I = Index;
     If Result Then
      Break;
    End;
  End;
End;

Function TAegysRecordBuffers.GetItem(Index : Integer) : TAegysBuffers;
Begin
 Result := TAegysBuffers(Inherited Items[Index]);
End;

Function TAegysRecordBuffers.New : TAegysBuffers;
Begin
 Result := TAegysBuffers.Create;
 Add(Result);
End;

Procedure TAegysRecordBuffers.SetItem(Index       : Integer;
                                      Const Value : TAegysBuffers);
Begin
 Put(Index, Value);
End;

{ TAegysBuffers }

Constructor TAegysBuffers.Create;
Begin
 Buffer          := TStringStream.Create;
 Buffer.SIZE     := 0;
 Buffer.Position := 0;
End;

Destructor TAegysBuffers.Destroy;
Begin
 FreeAndNil(Buffer);
 Inherited;
End;

{ TCoordinateList }

Function TCoordinateList.FindCoordinates(YFim,
                                         IniX,
                                         FimX  : Integer) : TCoordinates;
Var
 I : Integer;
Begin
 Result := Nil;
 For I := 0 To self.Count -1 Do
  Begin
   If ((Self.Items[I].Pos.Y + Self.Items[I].Tam.Y) = YFim) And
       (Self.Items[I].Pos.X = IniX)                        And
       (Self.Items[I].Tam.X = FimX)                        Then
    Begin
     Result := Self.Items[I];
     Break;
    End;
  End;
End;

Function TCoordinateList.GetItem(Index : Integer) : TCoordinates;
Begin
 Result := TCoordinates(Inherited Items[Index]);
End;

Function TCoordinateList.New : TCoordinates;
Begin
 Result := TCoordinates.Create;
 Add(Result);
End;

Procedure TCoordinateList.SetItem(Index       : Integer;
                                  Const Value : TCoordinates);
Begin
 Put(Index, Value);
End;

{ TBlockList }

Function TBlockList.FindLineCol(Line, Col: Integer) : TAegysBlock;
Var
 I : Integer;
Begin
 Result := Nil;
 For I := 0 To Self.Count -1 Do
  Begin
   If (Self.Items[I].LineCol.X  = Col)  And
      (Self.Items[I].LineCol.Y  = Line) Then
    Begin
     Result := Self.Items[I];
     Break;
    End;
  End;
End;

Function TBlockList.GetItem(Index : Integer) : TAegysBlock;
Begin
 Result := TAegysBlock(Inherited Items[Index]);
End;

Procedure TBlockList.ChangeSize(Size: Integer);
Var
 I : Integer;
Begin
 Self.Clear;
 For I := 1 To Size Do
  self.New;
End;

Function TBlockList.New: TAegysBlock;
Begin
 Result := TAegysBlock.Create;
 Add(Result);
End;

Procedure TBlockList.SetItem(Index       : Integer;
                             Const Value : TAegysBlock);
Begin
 Put(Index, Value);
End;

{ TAegysBlock }

Constructor TAegysBlock.Create;
Begin
 BMP := TFastDIB.Create;
 Change := False;
End;

Destructor TAegysBlock.Destroy;
Begin
 FreeAndNil(Bmp);
 Inherited;
End;

End.
