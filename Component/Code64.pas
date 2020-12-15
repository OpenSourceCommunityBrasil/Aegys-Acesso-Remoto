unit Code64;

interface

Type
 TByteArray = Array of Byte;

function Encode64(const S: ANSIString): ANSIString;
function Decode64(const S: ANSIString): ANSIString;
{$ifndef VER130}
function Encode64ByteArray(const s:TArray<Byte>):ANSIString;
function Decode64ByteArray(const S: ANSIString):TArray<Byte>;
function Encode64Unicode(const S: WideString): ANSIString;
function Decode64Unicode(const S: ANSIString): WideString;
{$endif}

implementation

const
  Codes64:ANSIString = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

function Encode64(const S: ANSIString): ANSIString;
var
  i: Integer;
  a: Integer;
  x: Integer;
  b: Integer;
begin
  Result := '';
  a := 0;
  b := 0;
  for i := 1 to Length(s) do
   begin
    x := Ord(s[i]);
    b := b * 256 + x;
    a := a + 8;
    while a >= 6 do
     begin
      a := a - 6;
      x := b SHR a;//div (1 shl a);
      b := b AND ((1 SHL a)-1); //mod (1 shl a);
      Result := Result + Codes64[x + 1];
     end;
   end;
  if a > 0 then
   begin
    x := b shl (6 - a);
    Result := Result + Codes64[x + 1];
   end;
end;

function Decode64(const S: ANSIString): ANSIString;
var
  i: Integer;
  a: Integer;
  x: Integer;
  b: Integer;
begin
  Result := '';
  a := 0;
  b := 0;
  for i := 1 to Length(s) do
  begin
    x := Pos(s[i], codes64) - 1;
    if x >= 0 then
    begin
      b := b * 64 + x;
      a := a + 6;
      if a >= 8 then
      begin
        a := a - 8;
        x := b shr a;
        b := b AND ((1 SHL a)-1);
        Result := Result + ANSIChar(chr(x));
      end;
    end
    else
      Exit;
  end;
end;

{$ifndef VER130}
function Encode64ByteArray(const s:TArray<Byte>):ANSIString;
var
  i: Integer;
  a: Integer;
  x: Integer;
  b: Integer;
begin
  Result := '';
  a := 0;
  b := 0;
  for i := Low(s) to High(s) do
   begin
    x := s[i];
    b := b * 256 + x;
    a := a + 8;
    while a >= 6 do
     begin
      a := a - 6;
      x := b SHR a;//div (1 shl a);
      b := b AND ((1 SHL a)-1); //mod (1 shl a);
      Result := Result + Codes64[x + 1];
     end;
   end;
  if a > 0 then
   begin
    x := b shl (6 - a);
    Result := Result + Codes64[x + 1];
   end;
end;

function Decode64ByteArray(const S: ANSIString):TArray<Byte>;
var
  i, ix: Integer;
  a: Integer;
  x: Integer;
  b: Integer;
begin
  SetLength(Result, Length(s) * 6 Div 8);
  a  := 0;
  b  := 0;
  ix := 0;
  for i := 1 to Length(s) do
  begin
    x := Pos(s[i], codes64) - 1;
    if x >= 0 then
    begin
      b := b * 64 + x;
      a := a + 6;
      if a >= 8 then
      begin
        a := a - 8;
        x := b shr a;
        b := b AND ((1 SHL a)-1);
        Result[ix] := x;
        Inc(ix);
      end;
    end
    else
      Exit;
  end;
end;
{$endif}

{$ifndef VER130}
function Encode64Unicode(const S: WideString): ANSIString;
var
  i: Integer;
  a: Integer;
  x: Integer;
  b: Integer;
  p : pByte;
  l :Integer;
begin
  Result := '';
  a := 0;
  b := 0;
  l:=Length(s)*SizeOf(WideChar);
  p:=pByte(@s[1]);
  for i := 0 to L-1 do
  begin
    x := p^;
    Inc(p);
    b := b * 256 + x;
    a := a + 8;
    while a >= 6 do
    begin
      a := a - 6;
      x := b SHR a;//div (1 shl a);
      b := b AND ((1 SHL a)-1); //mod (1 shl a);
      Result := Result + Codes64[x + 1];
    end;
  end;
  if a > 0 then
  begin
    x := b shl (6 - a);
    Result := Result + Codes64[x + 1];
  end;
end;

function Decode64Unicode(const S: ANSIString): WideString;
var
  i: Integer;
  a: Integer;
  x: Integer;
  b: Integer;
  p : pByte;
begin
  Result := '';
  a := 0;
  b := 0;
  SetLength(Result, (length(s)+ 2) div 3 * 4);
  p:=pByte(@Result[1]);
  for i := 1 to Length(s) do
  begin
    x := Pos(s[i], codes64) - 1;
    if x >= 0 then
    begin
      b := b * 64 + x;
      a := a + 6;
      if a >= 8 then
      begin
        a := a - 8;
        x := b shr a;
        b := b AND ((1 SHL a)-1);
        p^:= x;
        Inc(p);
        //Result := Result + chr(x);
      end;
    end
    else
      Exit;
  end;
end;

{$endif}

end.
