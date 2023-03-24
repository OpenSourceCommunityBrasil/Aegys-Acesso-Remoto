unit FastDraw; // FastDraw
               // Updated: 2/03/2001
interface      // http://gfody.com

{$R-}

uses Windows, FastDIB;

procedure GradientFill8(Bmp:TFastDIB;x1,y1,x2,y2:Integer;nw,ne,se,sw:Byte);
procedure GradientFill(Bmp:TFastDIB;x1,y1,x2,y2:Integer;nw,ne,se,sw:TFColor);
procedure Line8(Bmp:TFastDIB;x1,y1,x2,y2:Integer;c:Byte);
procedure Line(Bmp:TFastDIB;x1,y1,x2,y2:Integer;c:TFColor);
procedure SmoothLine8(Bmp:TFastDIB;x1,y1,x2,y2:Integer;c:Byte);
procedure SmoothLine(Bmp:TFastDIB;x1,y1,x2,y2:Integer;c:TFColor);
procedure Polyline8(Bmp:TFastDIB;pnts:array of TPoint;c:Byte);
procedure Polyline(Bmp:TFastDIB;pnts:array of TPoint;c:TFColor);
procedure SmoothPolyline8(Bmp:TFastDIB;pnts:array of TPoint;c:Byte);
procedure SmoothPolyline(Bmp:TFastDIB;pnts:array of TPoint;c:TFColor);

implementation

procedure GradientFill8(Bmp:TFastDIB;x1,y1,x2,y2:Integer;nw,ne,se,sw:Byte);
var
  pc: PByte;
  xc,yc,t,t2,z,iz,p2,p,dx,xx: Integer;
begin
  z:=0;
  iz:=65536;
  if x2<>x1 then t:=65536 div (x2-x1) else t:=0;
  if y2<>y1 then t2:=65536 div (y2-y1) else t2:=0;
  dx:=x2-x1;
  for yc:=y1 to y2-1 do
  begin
    xx:=((nw*iz+se*z)shr 16);
    p:=xx shl 16;
    p2:=(((ne*iz+sw*z)shr 16)-xx)*t;
    pc:=@Bmp.Pixels8[yc,x1];
    for xc:=0 to dx-1 do
    begin
      Inc(p,p2);
      pc^:=p shr 16;
      Inc(pc);
    end;
    Inc(z,t2);
    Dec(iz,t2);
  end;
end;

procedure GradientFill24(Bmp:TFastDIB;x1,y1,x2,y2:Integer;nw,ne,se,sw:TFColor);
var
  pc: PFColor;
  xc,yc,t,t2,z,iz,rp,
  rp2,gp,gp2,bp,bp2,dx,xx: Integer;
begin
  z:=0;
  iz:=65536;
  if x2<>x1 then t:=65536 div (x2-x1) else t:=0;
  if y2<>y1 then t2:=65536 div (y2-y1) else t2:=0;
  dx:=x2-x1;
  for yc:=y1 to y2-1 do
  begin
    xx:=((nw.r*iz+se.r*z)shr 16);
    rp:=xx shl 16;
    rp2:=(((ne.r*iz+sw.r*z)shr 16)-xx)*t;
    xx:=((nw.g*iz+se.g*z)shr 16);
    gp:=xx shl 16;
    gp2:=(((ne.g*iz+sw.g*z)shr 16)-xx)*t;
    xx:=((nw.b*iz+se.b*z)shr 16);
    bp:=xx shl 16;
    bp2:=(((ne.b*iz+sw.b*z)shr 16)-xx)*t;
    pc:=@Bmp.Pixels24[yc,x1];
    for xc:=0 to dx-1 do
    begin
      Inc(bp,bp2);
      Inc(gp,gp2);
      Inc(rp,rp2);
      pc.b:=bp shr 16;
      pc.g:=gp shr 16;
      pc.r:=rp shr 16;
      Inc(pc);
    end;
    Inc(z,t2);
    Dec(iz,t2);
  end;
end;

procedure GradientFill32(Bmp:TFastDIB;x1,y1,x2,y2:Integer;nw,ne,se,sw:TFColor);
var
  pc: PFColorA;
  xc,yc,t,t2,z,iz,rp,
  rp2,gp,gp2,bp,bp2,dx,xx: Integer;
begin
  z:=0;
  iz:=65536;
  if x2<>x1 then t:=65536 div (x2-x1) else t:=0;
  if y2<>y1 then t2:=65536 div (y2-y1) else t2:=0;
  dx:=x2-x1;
  for yc:=y1 to y2-1 do
  begin
    xx:=((nw.r*iz+se.r*z)shr 16);
    rp:=xx shl 16;
    rp2:=(((ne.r*iz+sw.r*z)shr 16)-xx)*t;
    xx:=((nw.g*iz+se.g*z)shr 16);
    gp:=xx shl 16;
    gp2:=(((ne.g*iz+sw.g*z)shr 16)-xx)*t;
    xx:=((nw.b*iz+se.b*z)shr 16);
    bp:=xx shl 16;
    bp2:=(((ne.b*iz+sw.b*z)shr 16)-xx)*t;
    pc:=@Bmp.Pixels32[yc,x1];
    for xc:=0 to dx-1 do
    begin
      Inc(bp,bp2);
      Inc(gp,gp2);
      Inc(rp,rp2);
      pc.b:=bp shr 16;
      pc.g:=gp shr 16;
      pc.r:=rp shr 16;
      Inc(pc);
    end;
    Inc(z,t2);
    Dec(iz,t2);
  end;
end;

procedure GradientFill(Bmp:TFastDIB;x1,y1,x2,y2:Integer;nw,ne,se,sw:TFColor);
begin
  case Bmp.Bpp of
    8:  GradientFill8(Bmp,x1,y1,x2,y2,
          ClosestColor(Bmp.Colors,255,nw),
          ClosestColor(Bmp.Colors,255,ne),
          ClosestColor(Bmp.Colors,255,se),
          ClosestColor(Bmp.Colors,255,sw));
    24: GradientFill24(Bmp,x1,y1,x2,y2,nw,ne,se,sw);
    32: GradientFill32(Bmp,x1,y1,x2,y2,nw,ne,se,sw);
  end;
end;

procedure Line8(Bmp:TFastDIB;x1,y1,x2,y2:Integer;c:Byte);
var
  d,ax,ay,sx,sy,dx,dy: Integer;
begin
  if Bmp.Height>0 then
  begin
    y1:=Bmp.AbsHeight-y1-1;
    y2:=Bmp.AbsHeight-y2-1;
  end;

  dx:=x2-x1; ax:=Abs(dx)shl 1; if dx<0 then sx:=-1 else sx:=1;
  dy:=y2-y1; ay:=Abs(dy)shl 1; if dy<0 then sy:=-1 else sy:=1;
  Bmp.Pixels8[y1,x1]:=c;
  if ax>ay then
  begin
    d:=ay-(ax shr 1);
    while x1<>x2 do
    begin
      if d>-1 then
      begin
        Inc(y1,sy);
        Dec(d,ax);
      end;
      Inc(x1,sx);
      Inc(d,ay);
      Bmp.Pixels8[y1,x1]:=c;
    end;
  end else
  begin
    d:=ax-(ay shr 1);
    while y1<>y2 do
    begin
      if d>=0 then
      begin
        Inc(x1,sx);
        Dec(d,ay);
      end;
      Inc(y1,sy);
      Inc(d,ax);
      Bmp.Pixels8[y1,x1]:=c;
    end;
  end;
end;

procedure Line16(Bmp:TFastDIB;x1,y1,x2,y2:Integer;c:TFColor);
var
  d,ax,ay,sx,sy,dx,dy: Integer;
  w: Word;
begin
  if Bmp.Height>0 then
  begin
    y1:=Bmp.AbsHeight-y1-1;
    y2:=Bmp.AbsHeight-y2-1;
  end;

  w:=c.b shr Bmp.BShr or c.g shr Bmp.GShr shl Bmp.GShl or c.r shr Bmp.RShr shl Bmp.RShl;
  dx:=x2-x1; ax:=Abs(dx)shl 1; if dx<0 then sx:=-1 else sx:=1;
  dy:=y2-y1; ay:=Abs(dy)shl 1; if dy<0 then sy:=-1 else sy:=1;
  Bmp.Pixels16[y1,x1]:=w;
  if ax>ay then
  begin
    d:=ay-(ax shr 1);
    while x1<>x2 do
    begin
      if d>-1 then
      begin
        Inc(y1,sy);
        Dec(d,ax);
      end;
      Inc(x1,sx);
      Inc(d,ay);
      Bmp.Pixels16[y1,x1]:=w;
    end;
  end else
  begin
    d:=ax-(ay shr 1);
    while y1<>y2 do
    begin
      if d>=0 then
      begin
        Inc(x1,sx);
        Dec(d,ay);
      end;
      Inc(y1,sy);
      Inc(d,ax);
      Bmp.Pixels16[y1,x1]:=w;
    end;
  end;
end;

procedure Line24(Bmp:TFastDIB;x1,y1,x2,y2:Integer;c:TFColor);
var
  d,ax,ay,sx,sy,dx,dy: Integer;
begin
  if Bmp.Height>0 then
  begin
    y1:=Bmp.AbsHeight-y1-1;
    y2:=Bmp.AbsHeight-y2-1;
  end;

  dx:=x2-x1; ax:=Abs(dx)shl 1; if dx<0 then sx:=-1 else sx:=1;
  dy:=y2-y1; ay:=Abs(dy)shl 1; if dy<0 then sy:=-1 else sy:=1;
  Bmp.Pixels24[y1,x1]:=c;
  if ax>ay then
  begin
    d:=ay-(ax shr 1);
    while x1<>x2 do
    begin
      if d>-1 then
      begin
        Inc(y1,sy);
        Dec(d,ax);
      end;
      Inc(x1,sx);
      Inc(d,ay);
      Bmp.Pixels24[y1,x1]:=c;
    end;
  end else
  begin
    d:=ax-(ay shr 1);
    while y1<>y2 do
    begin
      if d>=0 then
      begin
        Inc(x1,sx);
        Dec(d,ay);
      end;
      Inc(y1,sy);
      Inc(d,ax);
      Bmp.Pixels24[y1,x1]:=c;
    end;
  end;
end;

procedure Line32(Bmp:TFastDIB;x1,y1,x2,y2:Integer;c:TFColor);
var
  d,ax,ay,sx,sy,dx,dy: Integer;
begin
  if Bmp.Height>0 then
  begin
    y1:=Bmp.AbsHeight-y1-1;
    y2:=Bmp.AbsHeight-y2-1;
  end;

  dx:=x2-x1; ax:=Abs(dx)shl 1; if dx<0 then sx:=-1 else sx:=1;
  dy:=y2-y1; ay:=Abs(dy)shl 1; if dy<0 then sy:=-1 else sy:=1;
  Bmp.Pixels32[y1,x1].c:=c;
  if ax>ay then
  begin
    d:=ay-(ax shr 1);
    while x1<>x2 do
    begin
      if d>-1 then
      begin
        Inc(y1,sy);
        Dec(d,ax);
      end;
      Inc(x1,sx);
      Inc(d,ay);
      Bmp.Pixels32[y1,x1].c:=c;
    end;
  end else
  begin
    d:=ax-(ay shr 1);
    while y1<>y2 do
    begin
      if d>=0 then
      begin
        Inc(x1,sx);
        Dec(d,ay);
      end;
      Inc(y1,sy);
      Inc(d,ax);
      Bmp.Pixels32[y1,x1].c:=c;
    end;
  end;
end;

procedure Line(Bmp:TFastDIB;x1,y1,x2,y2:Integer;c:TFColor);
begin
  case Bmp.Bpp of
    8:  Line8(Bmp,x1,y1,x2,y2,ClosestColor(Bmp.Colors,255,c));
    16: Line16(Bmp,x1,y1,x2,y2,c);
    24: Line24(Bmp,x1,y1,x2,y2,c);
    32: Line32(Bmp,x1,y1,x2,y2,c);
  end;
end;

procedure SmoothLine8(Bmp:TFastDIB;x1,y1,x2,y2:Integer;c:Byte);
var
  dx,dy,d,s,ci,ea,ec: Integer;
  p: PByte;
begin
  if(y1=y2)or(x1=x2)then Line8(Bmp,x1,y1,x2,y2,c) else
  begin

    if Bmp.Height>0 then
    begin
      y1:=Bmp.AbsHeight-y1-1;
      y2:=Bmp.AbsHeight-y2-1;
    end;

    if y1>y2 then
    begin
      d:=y1; y1:=y2; y2:=d;
      d:=x1; x1:=x2; x2:=d;
    end;
    dx:=x2-x1;
    dy:=y2-y1;
    if dx>-1 then s:=1 else
    begin
      s:=-1;
      dx:=-dx;
    end;
    ec:=0;

    Bmp.Pixels8[y1,x1]:=c;
    if dy>dx then
    begin
      ea:=(dx shl 16)div dy;
      while dy>1 do
      begin
        Dec(dy);
        d:=ec;
        Inc(ec,ea);
        ec:=ec and $FFFF;
        if ec<=d then Inc(x1,s);
        Inc(y1);
        ci:=ec shr 8;
        p:=@Bmp.Pixels8[y1,x1];
        p^:=(p^-c)*ci shr 8 + c;
        p:=@Bmp.Pixels8[y1,x1+s];
        p^:=(c-p^)*ci shr 8 + p^;
      end;
    end else
    begin
      ea:=(dy shl 16)div dx;
      while dx>1 do
      begin
        Dec(dx);
        d:=ec;
        Inc(ec,ea);
        ec:=ec and $FFFF;
        if ec<=d then Inc(y1);
        Inc(x1,s);
        ci:=ec shr 8;
        p:=@Bmp.Pixels8[y1,x1];
        p^:=(p^-c)*ci shr 8 + c;
        p:=@Bmp.Pixels8[y1+1,x1];
        p^:=(c-p^)*ci shr 8 + p^;
      end;
    end;
    Bmp.Pixels8[y2,x2]:=c;
  end;
end;

procedure SmoothLine16(Bmp:TFastDIB;x1,y1,x2,y2:Integer;c:TFColor);
var
  dx,dy,s,d,ci,ea,ec: Integer;
  b,g,r,w: Word;
  p: PWord;
begin
  w:=c.b shr Bmp.BShr or c.g shr Bmp.GShr shl Bmp.GShl or c.r shr Bmp.RShr shl Bmp.RShl;

  if(y1=y2)or(x1=x2)then Line16(Bmp,x1,y1,x2,y2,c) else
  begin

    if Bmp.Height>0 then
    begin
      y1:=Bmp.AbsHeight-y1-1;
      y2:=Bmp.AbsHeight-y2-1;
    end;

    if y1>y2 then
    begin
      d:=y1; y1:=y2; y2:=d;
      d:=x1; x1:=x2; x2:=d;
    end;
    dx:=x2-x1;
    dy:=y2-y1;
    if dx>-1 then s:=1 else
    begin
      s:=-1;
      dx:=-dx;
    end;
    ec:=0;

    Bmp.Pixels16[y1,x1]:=w;
    if dy>dx then
    begin
      ea:=(dx shl 16)div dy;
      while dy>1 do
      begin
        Dec(dy);
        d:=ec;
        Inc(ec,ea);
        ec:=ec and $FFFF;
        if ec<=d then Inc(x1,s);
        Inc(y1);
        ci:=ec shr 8;

        p:=@Bmp.Pixels16[y1,x1];
        b:=( Integer((p^ shl Bmp.BShr)and Bmp.BMask) - c.b)*ci shr 8 + c.b;
        g:=( Integer((p^ shl Bmp.GShr shr Bmp.GShl)and Bmp.GMask) - c.g)*ci shr 8 + c.g;
        r:=( Integer((p^ shl Bmp.RShr shr Bmp.RShl)and Bmp.RMask) - c.r)*ci shr 8 + c.r;
        p^:=b shr Bmp.BShr or g shr Bmp.GShr shl Bmp.GShl or r shr Bmp.RShr shl Bmp.RShl;

        p:=@Bmp.Pixels16[y1,x1+s];
        b:=( c.b - Integer((p^ shl Bmp.BShr)and Bmp.BMask))*ci shr 8 + Integer((p^ shl Bmp.BShr)and Bmp.BMask);
        g:=( c.g - Integer((p^ shl Bmp.GShr shr Bmp.GShl)and Bmp.GMask))*ci shr 8 + Integer((p^ shl Bmp.GShr shr Bmp.GShl)and Bmp.GMask);
        r:=( c.r - Integer((p^ shl Bmp.RShr shr Bmp.RShl)and Bmp.RMask))*ci shr 8 + Integer((p^ shl Bmp.RShr shr Bmp.RShl)and Bmp.RMask);
        p^:=b shr Bmp.BShr or g shr Bmp.GShr shl Bmp.GShl or r shr Bmp.RShr shl Bmp.RShl;

      end;
    end else
    begin
      ea:=(dy shl 16)div dx;
      while dx>1 do
      begin
        Dec(dx);
        d:=ec;
        Inc(ec,ea);
        ec:=ec and $FFFF;
        if ec<=d then Inc(y1);
        Inc(x1,s);
        ci:=ec shr 8;

        p:=@Bmp.Pixels16[y1,x1];
        b:=( Integer((p^ shl Bmp.BShr)and Bmp.BMask) - c.b)*ci shr 8 + c.b;
        g:=( Integer((p^ shl Bmp.GShr shr Bmp.GShl)and Bmp.GMask) - c.g)*ci shr 8 + c.g;
        r:=( Integer((p^ shl Bmp.RShr shr Bmp.RShl)and Bmp.RMask) - c.r)*ci shr 8 + c.r;
        p^:=b shr Bmp.BShr or g shr Bmp.GShr shl Bmp.GShl or r shr Bmp.RShr shl Bmp.RShl;

        p:=@Bmp.Pixels16[y1+1,x1];
        b:=( c.b - Integer((p^ shl Bmp.BShr)and Bmp.BMask))*ci shr 8 + Integer((p^ shl Bmp.BShr)and Bmp.BMask);
        g:=( c.g - Integer((p^ shl Bmp.GShr shr Bmp.GShl)and Bmp.GMask))*ci shr 8 + Integer((p^ shl Bmp.GShr shr Bmp.GShl)and Bmp.GMask);
        r:=( c.r - Integer((p^ shl Bmp.RShr shr Bmp.RShl)and Bmp.RMask))*ci shr 8 + Integer((p^ shl Bmp.RShr shr Bmp.RShl)and Bmp.RMask);
        p^:=b shr Bmp.BShr or g shr Bmp.GShr shl Bmp.GShl or r shr Bmp.RShr shl Bmp.RShl;
      end;
    end;
    Bmp.Pixels16[y2,x2]:=w;
  end;
end;

procedure SmoothLine24(Bmp:TFastDIB;x1,y1,x2,y2:Integer;c:TFColor);
var
  dx,dy,d,s,ci,ea,ec: Integer;
  p: PFColor;
begin
  if(y1=y2)or(x1=x2)then Line24(Bmp,x1,y1,x2,y2,c) else
  begin

    if Bmp.Height>0 then
    begin
      y1:=Bmp.AbsHeight-y1-1;
      y2:=Bmp.AbsHeight-y2-1;
    end;

    if y1>y2 then
    begin
      d:=y1; y1:=y2; y2:=d;
      d:=x1; x1:=x2; x2:=d;
    end;
    dx:=x2-x1;
    dy:=y2-y1;
    if dx>-1 then s:=1 else
    begin
      s:=-1;
      dx:=-dx;
    end;
    ec:=0;

    Bmp.Pixels24[y1,x1]:=c;
    if dy>dx then
    begin
      ea:=(dx shl 16)div dy;
      while dy>1 do
      begin
        Dec(dy);
        d:=ec;
        Inc(ec,ea);
        ec:=ec and $FFFF;
        if ec<=d then Inc(x1,s);
        Inc(y1);
        ci:=ec shr 8;
        p:=@Bmp.Pixels24[y1,x1];
        p.b:=(p.b-c.b)*ci shr 8 + c.b;
        p.g:=(p.g-c.g)*ci shr 8 + c.g;
        p.r:=(p.r-c.r)*ci shr 8 + c.r;
        p:=@Bmp.Pixels24[y1,x1+s];
        p.b:=(c.b-p.b)*ci shr 8 + p.b;
        p.g:=(c.g-p.g)*ci shr 8 + p.g;
        p.r:=(c.r-p.r)*ci shr 8 + p.r;
      end;
    end else
    begin
      ea:=(dy shl 16)div dx;
      while dx>1 do
      begin
        Dec(dx);
        d:=ec;
        Inc(ec,ea);
        ec:=ec and $FFFF;
        if ec<=d then Inc(y1);
        Inc(x1,s);
        ci:=ec shr 8;
        p:=@Bmp.Pixels24[y1,x1];
        p.b:=(p.b-c.b)*ci shr 8 + c.b;
        p.g:=(p.g-c.g)*ci shr 8 + c.g;
        p.r:=(p.r-c.r)*ci shr 8 + c.r;
        p:=@Bmp.Pixels24[y1+1,x1];
        p.b:=(c.b-p.b)*ci shr 8 + p.b;
        p.g:=(c.g-p.g)*ci shr 8 + p.g;
        p.r:=(c.r-p.r)*ci shr 8 + p.r;
      end;
    end;
    Bmp.Pixels24[y2,x2]:=c;
  end;
end;

procedure SmoothLine32(Bmp:TFastDIB;x1,y1,x2,y2:Integer;c:TFColor);
var
  dx,dy,d,s,ci,ea,ec: Integer;
  p: PFColorA;
begin
  if(y1=y2)or(x1=x2)then Line32(Bmp,x1,y1,x2,y2,c) else
  begin

    if Bmp.Height>0 then
    begin
      y1:=Bmp.AbsHeight-y1-1;
      y2:=Bmp.AbsHeight-y2-1;
    end;

    if y1>y2 then
    begin
      d:=y1; y1:=y2; y2:=d;
      d:=x1; x1:=x2; x2:=d;
    end;
    dx:=x2-x1;
    dy:=y2-y1;
    if dx>-1 then s:=1 else
    begin
      s:=-1;
      dx:=-dx;
    end;
    ec:=0;

    Bmp.Pixels32[y1,x1].c:=c;
    if dy>dx then
    begin
      ea:=(dx shl 16)div dy;
      while dy>1 do
      begin
        Dec(dy);
        d:=ec;
        Inc(ec,ea);
        ec:=ec and $FFFF;
        if ec<=d then Inc(x1,s);
        Inc(y1);
        ci:=ec shr 8;
        p:=@Bmp.Pixels32[y1,x1];
        p.b:=(p.b-c.b)*ci shr 8 + c.b;
        p.g:=(p.g-c.g)*ci shr 8 + c.g;
        p.r:=(p.r-c.r)*ci shr 8 + c.r;
        p:=@Bmp.Pixels32[y1,x1+s];
        p.b:=(c.b-p.b)*ci shr 8 + p.b;
        p.g:=(c.g-p.g)*ci shr 8 + p.g;
        p.r:=(c.r-p.r)*ci shr 8 + p.r;
      end;
    end else
    begin
      ea:=(dy shl 16)div dx;
      while dx>1 do
      begin
        Dec(dx);
        d:=ec;
        Inc(ec,ea);
        ec:=ec and $FFFF;
        if ec<=d then Inc(y1);
        Inc(x1,s);
        ci:=ec shr 8;
        p:=@Bmp.Pixels32[y1,x1];
        p.b:=(p.b-c.b)*ci shr 8 + c.b;
        p.g:=(p.g-c.g)*ci shr 8 + c.g;
        p.r:=(p.r-c.r)*ci shr 8 + c.r;
        p:=@Bmp.Pixels32[y1+1,x1];
        p.b:=(c.b-p.b)*ci shr 8 + p.b;
        p.g:=(c.g-p.g)*ci shr 8 + p.g;
        p.r:=(c.r-p.r)*ci shr 8 + p.r;
      end;
    end;
    Bmp.Pixels32[y2,x2].c:=c;
  end;
end;

procedure SmoothLine(Bmp:TFastDIB;x1,y1,x2,y2:Integer;c:TFColor);
begin
  case Bmp.Bpp of
    8:  SmoothLine8(Bmp,x1,y1,x2,y2,ClosestColor(Bmp.Colors,255,c));
    16: SmoothLine16(Bmp,x1,y1,x2,y2,c);
    24: SmoothLine24(Bmp,x1,y1,x2,y2,c);
    32: SmoothLine32(Bmp,x1,y1,x2,y2,c);
  end;
end;

procedure Polyline8(Bmp:TFastDIB;pnts:array of TPoint;c:Byte);
var
  n,i: Integer;
begin
  n:=High(pnts)+1;
  for i:=0 to n-1 do
    Line8(Bmp,pnts[i].x,pnts[i].y,pnts[(i+1)mod n].x,pnts[(i+1)mod n].y,c);
end;

procedure Polyline(Bmp:TFastDIB;pnts:array of TPoint;c:TFColor);
var
  n,i: Integer;
begin
  if Bmp.Bpp=8 then Polyline8(Bmp,pnts,ClosestColor(Bmp.Colors,255,c))else
  begin
    n:=High(pnts)+1;
    for i:=0 to n-1 do
      Line(Bmp,pnts[i].x,pnts[i].y,pnts[(i+1)mod n].x,pnts[(i+1)mod n].y,c);
  end;
end;

procedure SmoothPolyline8(Bmp:TFastDIB;pnts:array of TPoint;c:Byte);
var
  n,i: Integer;
begin
  n:=High(pnts)+1;
  for i:=0 to n-1 do
    SmoothLine8(Bmp,pnts[i].x,pnts[i].y,pnts[(i+1)mod n].x,pnts[(i+1)mod n].y,c);
end;

procedure SmoothPolyline(Bmp:TFastDIB;pnts:array of TPoint;c:TFColor);
var
  n,i: Integer;
begin
  if Bmp.Bpp=8 then SmoothPolyline8(Bmp,pnts,ClosestColor(Bmp.Colors,255,c))else
  begin
    n:=High(pnts)+1;
    for i:=0 to n-1 do
      SmoothLine(Bmp,pnts[i].x,pnts[i].y,pnts[(i+1)mod n].x,pnts[(i+1)mod n].y,c);
  end;
end;

end.

