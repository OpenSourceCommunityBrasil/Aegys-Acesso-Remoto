unit uFormTelaRemota;

{
  Project Aegys Remote Support.

  Created by Gilberto Rocha da Silva in 04/05/2017 based on project Allakore, has by objective to promote remote access
  and other resources freely to all those who need it, today maintained by a beautiful community. Listing below our
  higly esteemed collaborators:

  Gilberto Rocha da Silva (XyberX) (Creator of Aegys Project/Main Developer/Admin)
  Wendel Rodrigues Fassarella (wendelfassarella) (Creator of Aegys FMX/CORE Developer)
  Rai Duarte Jales (Raí Duarte) (Aegys Server Developer)
  Roniery Santos Cardoso (Aegys Developer)
  Alexandre Carlos Silva Abade (Aegys Developer)
  Mobius One (Aegys Developer)
}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Layouts, System.Actions,
  FMX.ActnList, Winapi.Messages, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  uFunctions;

type
  TFormTelaRemota = class(TForm)
    sbChat: TSpeedButton;
    Path1: TPath;
    LChat: TLabel;
    sbArquivos: TSpeedButton;
    Path2: TPath;
    LFiles: TLabel;
    ActionList1: TActionList;
    PROC_ARQUIVOS: TAction;
    PROC_CHAT: TAction;
    tCapturarComandos: TTimer;
    imgTelaInicial: TImage;
    PROC_REDIMENSIONAR: TAction;
    PROC_LOG: TAction;
    btnMouse: TSpeedButton;
    Path3: TPath;
    LMouse: TLabel;
    PROC_MOUSE: TAction;
    pControles: TPanel;
    imgTelaRemota: TRectangle;
    procedure PROC_ARQUIVOSExecute(Sender: TObject);
    procedure PROC_CHATExecute(Sender: TObject);
    procedure imgTelaRemotaMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Single);
    procedure imgTelaRemotaMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure imgTelaRemotaMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure tCapturarComandosTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
    procedure PROC_REDIMENSIONARExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure PROC_MOUSEExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    Locale: TLocale;
    procedure RetornaMargem;
    procedure SendSocketKeys(AKeys: string);
    procedure WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
      message WM_GETMINMAXINFO;
    procedure Translate;
  public
    CtrlPressed, ShiftPressed, AltPressed, IgnoreKey: Boolean;
  end;

var
  FormTelaRemota: TFormTelaRemota;

implementation

{$R *.fmx}

uses uFormArquivos, uFormChat, uFormConexao, Winapi.Windows, uDM_Styles,
  FMX.Platform.Win, uConstants;

procedure TFormTelaRemota.tCapturarComandosTimer(Sender: TObject);
var
  i: Byte;
begin
  if Self <> Screen.ActiveForm then
    Exit;

  // The keys programmed here, may not match the keys on your keyboard. I recommend to undertake adaptation.
  try
    { Combo }
    // Alt
    if not(AltPressed) then
    begin
      if (GetKeyState(VK_MENU) < 0) then
      begin
        AltPressed := True;
        SendSocketKeys('<|ALTDOWN|>');
      end;
    end
    else
    begin
      if (GetKeyState(VK_MENU) > -1) then
      begin
        AltPressed := False;
        SendSocketKeys('<|ALTUP|>');
      end;
    end;

    // Ctrl
    if not(CtrlPressed) then
    begin
      if (GetKeyState(VK_CONTROL) < 0) then
      begin
        CtrlPressed := True;
        SendSocketKeys('<|CTRLDOWN|>');
      end;
    end
    else
    begin
      if (GetKeyState(VK_CONTROL) > -1) then
      begin
        CtrlPressed := False;
        SendSocketKeys('<|CTRLUP|>');
      end;
    end;

    // Shift
    if not(ShiftPressed) then
    begin
      if (GetKeyState(VK_SHIFT) < 0) then
      begin
        ShiftPressed := True;
        SendSocketKeys('<|SHIFTDOWN|>');
      end;
    end
    else
    begin
      if (GetKeyState(VK_SHIFT) > -1) then
      begin
        ShiftPressed := False;
        SendSocketKeys('<|SHIFTUP|>');
      end;
    end;

    for i := 8 to 228 do
    begin
      if (GetAsyncKeyState(i) = -32767) then
      begin
        case i of
          8:
            SendSocketKeys('{BS}');
          9:
            SendSocketKeys('{TAB}');
          13:
            SendSocketKeys('{ENTER}');
          27:
            begin
              if IgnoreKey then
                IgnoreKey := False
              else
                SendSocketKeys('{ESCAPE}');
            end;
          32:
            SendSocketKeys(' ');
          33:
            SendSocketKeys('{PGUP}');
          34:
            SendSocketKeys('{PGDN}');
          35:
            SendSocketKeys('{END}');
          36:
            SendSocketKeys('{HOME}');
          37:
            SendSocketKeys('{LEFT}');
          38:
            SendSocketKeys('{UP}');
          39:
            SendSocketKeys('{RIGHT}');
          40:
            SendSocketKeys('{DOWN}');
          44:
            SendSocketKeys('{PRTSC}');
          46:
            SendSocketKeys('{DEL}');
          91:
            begin
              IgnoreKey := True;
              Keybd_Event(VK_ESCAPE, 0, 0, 0);
              SendSocketKeys('{LWIN}');
            end;
          92:
            begin
              IgnoreKey := True;
              Keybd_Event(VK_ESCAPE, 0, 0, 0);
              SendSocketKeys('{RWIN}');
            end;
          145:
            SendSocketKeys('{SCROLLLOCK}');

          // Numbers: 1 2 3 4 5 6 7 8 9 and ! @ # $ % ¨& * ( )
          48:
            if (GetKeyState(VK_SHIFT) < 0) then
              SendSocketKeys(')')
            else
              SendSocketKeys('0');
          49:
            if (GetKeyState(VK_SHIFT) < 0) then
              SendSocketKeys('!')
            else
              SendSocketKeys('1');
          50:
            if (GetKeyState(VK_SHIFT) < 0) then
              SendSocketKeys('@')
            else
              SendSocketKeys('2');
          51:
            if (GetKeyState(VK_SHIFT) < 0) then
              SendSocketKeys('#')
            else
              SendSocketKeys('3');
          52:
            if (GetKeyState(VK_SHIFT) < 0) then
              SendSocketKeys('$')
            else
              SendSocketKeys('4');
          53:
            if (GetKeyState(VK_SHIFT) < 0) then
              SendSocketKeys('%')
            else
              SendSocketKeys('5');
          54:
            if (GetKeyState(VK_SHIFT) < 0) then
              SendSocketKeys('^')
            else
              SendSocketKeys('6');
          55:
            if (GetKeyState(VK_SHIFT) < 0) then
              SendSocketKeys('&')
            else
              SendSocketKeys('7');
          56:
            if (GetKeyState(VK_SHIFT) < 0) then
              SendSocketKeys('*')
            else
              SendSocketKeys('8');
          57:
            if (GetKeyState(VK_SHIFT) < 0) then
              SendSocketKeys('(')
            else
              SendSocketKeys('9');

          65 .. 90: // A..Z / a..z
            begin
              if (GetKeyState(VK_CAPITAL) = 1) then
                if (GetKeyState(VK_SHIFT) < 0) then
                  SendSocketKeys(LowerCase(Chr(i)))
                else
                  SendSocketKeys(UpperCase(Chr(i)))
              else if (GetKeyState(VK_SHIFT) < 0) then
                SendSocketKeys(UpperCase(Chr(i)))
              else
                SendSocketKeys(LowerCase(Chr(i)))

            end;

          96 .. 105: // Numpad 1..9
            SendSocketKeys(IntToStr(i - 96));

          106:
            SendSocketKeys('*');
          107:
            SendSocketKeys('+');
          109:
            SendSocketKeys('-');
          110:
            SendSocketKeys(',');
          111:
            SendSocketKeys('/');
          194:
            SendSocketKeys('.');

          // F1..F12
          112 .. 123:
            SendSocketKeys('{F' + IntToStr(i - 111) + '}');

          186:
            if (GetKeyState(VK_SHIFT) < 0) then
              SendSocketKeys('Ç')
            else
              SendSocketKeys('ç');
          187:
            if (GetKeyState(VK_SHIFT) < 0) then
              SendSocketKeys('+')
            else
              SendSocketKeys('=');
          188:
            if (GetKeyState(VK_SHIFT) < 0) then
              SendSocketKeys('<')
            else
              SendSocketKeys(',');
          189:
            if (GetKeyState(VK_SHIFT) < 0) then
              SendSocketKeys('_')
            else
              SendSocketKeys('-');
          190:
            if (GetKeyState(VK_SHIFT) < 0) then
              SendSocketKeys('>')
            else
              SendSocketKeys('.');
          191:
            if (GetKeyState(VK_SHIFT) < 0) then
              SendSocketKeys(':')
            else
              SendSocketKeys(';');
          192:
            if (GetKeyState(VK_SHIFT) < 0) then
              SendSocketKeys('"')
            else
              SendSocketKeys('''');
          193:
            if (GetKeyState(VK_SHIFT) < 0) then
              SendSocketKeys('?')
            else
              SendSocketKeys('/');
          219:
            if (GetKeyState(VK_SHIFT) < 0) then
              SendSocketKeys('`')
            else
              SendSocketKeys('´');
          220:
            if (GetKeyState(VK_SHIFT) < 0) then
              SendSocketKeys('}')
            else
              SendSocketKeys(']');
          221:
            if (GetKeyState(VK_SHIFT) < 0) then
              SendSocketKeys('{')
            else
              SendSocketKeys('[');
          222:
            if (GetKeyState(VK_SHIFT) < 0) then
              SendSocketKeys('^')
            else
              SendSocketKeys('~');
          226:
            if (GetKeyState(VK_SHIFT) < 0) then
              SendSocketKeys('|')
            else
              SendSocketKeys('\');
        end;
      end;
    end;
  except
  end;
end;

procedure TFormTelaRemota.Translate;
begin
  LMouse.Text := Locale.GetLocale(FRMS, 'RemoteMouse');
  LFiles.Text := Locale.GetLocale(FRMS, 'RemoteFile');
  LChat.Text := Locale.GetLocale(FRMS, 'RemoteChat');
end;

procedure TFormTelaRemota.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FormArquivos.Hide;
  FormChat.Hide;
  Conexao.SocketPrincipal.Socket.SendText('<|STOPACCESS|>');
  FormConexao.SetOnline;
  FormConexao.Show;
end;

procedure TFormTelaRemota.FormCreate(Sender: TObject);
begin
  Locale := TLocale.Create;
  SetWindowLong(FmxHandleToHWND(Handle), GWL_EXSTYLE, WS_EX_APPWINDOW);
end;

procedure TFormTelaRemota.FormDestroy(Sender: TObject);
begin
  Locale.DisposeOf;
end;

procedure TFormTelaRemota.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if (ssAlt in Shift) and (Key = vkF4) then
  begin
    Abort;
  end;
end;

procedure TFormTelaRemota.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  if not Active then
    Exit;

  Conexao.SocketPrincipal.Socket.SendText('<|REDIRECT|><|WHEELMOUSE|>' +
    IntToStr(WheelDelta) + '<|END|>');
end;

procedure TFormTelaRemota.FormResize(Sender: TObject);
begin
  PROC_REDIMENSIONARExecute(Sender);
end;

procedure TFormTelaRemota.FormShow(Sender: TObject);
begin
  CtrlPressed := False;
  ShiftPressed := False;
  AltPressed := False;
  tCapturarComandos.Enabled := True;
  PROC_REDIMENSIONARExecute(Sender);
  btnMouse.StaysPressed := Conexao.MostrarMouse;
end;

procedure TFormTelaRemota.PROC_ARQUIVOSExecute(Sender: TObject);
begin
  FormArquivos.Show;
end;

procedure TFormTelaRemota.PROC_CHATExecute(Sender: TObject);
begin
  FormChat.Show;
end;

procedure TFormTelaRemota.PROC_MOUSEExecute(Sender: TObject);
begin
  Conexao.MostrarMouse := not Conexao.MostrarMouse;
  btnMouse.StaysPressed := Conexao.MostrarMouse;
end;

procedure TFormTelaRemota.PROC_REDIMENSIONARExecute(Sender: TObject);
begin
//  imgTelaRemota.Align                := TAlignLayout.Client;
//  imgTelaRemota.Fill.Kind            := TbrushKind.Bitmap;
//  imgTelaRemota.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
  pControles.Position.X := (Self.Width / 2) - (pControles.Width / 2);
  RetornaMargem;
end;

procedure TFormTelaRemota.RetornaMargem;
var
  iImageH, iImageW, iTargetH, iTargetW, iMarginW, iMarginH: Single;
begin
  imgTelaRemota.Margins.Left := 0;
  imgTelaRemota.Margins.Right := 0;
  imgTelaRemota.Margins.Top := 0;
  imgTelaRemota.Margins.Bottom := 0;

  iImageH := imgTelaRemota.Height;
  iImageW := imgTelaRemota.Width;
  iTargetH := Conexao.ResolucaoAltura;
  iTargetW := Conexao.ResolucaoLargura;

  iMarginW := iTargetW * iImageH;
  iMarginW := iMarginW / iTargetH;
  if iImageW > iMarginW then
  begin
    iMarginH := 0;
    iMarginW := iImageW - iMarginW;
    iMarginW := iMarginW / 2;
  end
  else
  begin
    iMarginW := 0;
    iMarginH := iTargetH * iImageW;
    iMarginH := iMarginH / iTargetW;
    iMarginH := iImageH - iMarginH;
    iMarginH := iMarginH / 2;
  end;

//  imgTelaRemota.Margins.Left := iMarginW;
//  imgTelaRemota.Margins.Right := iMarginW;
//  imgTelaRemota.Margins.Top := iMarginH;
//  imgTelaRemota.Margins.Bottom := iMarginH;
end;

procedure TFormTelaRemota.imgTelaRemotaMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  iX, iY: Integer;
begin
  if not Active then
    Exit;

  iX := Trunc(X * Conexao.ResolucaoLargura) div Trunc(imgTelaRemota.Width);
  iY := Trunc(Y * Conexao.ResolucaoAltura) div Trunc(imgTelaRemota.Height);

  if (Button = TMouseButton.mbLeft) then
  begin
    Conexao.SocketPrincipal.Socket.SendText
      ('<|REDIRECT|><|SETMOUSELEFTCLICKDOWN|>' + IntToStr(iX) + '<|>' +
      IntToStr(iY) + '<|END|>');
  end
  else if (Button = TMouseButton.mbRight) then
  begin
    Conexao.SocketPrincipal.Socket.SendText
      ('<|REDIRECT|><|SETMOUSERIGHTCLICKDOWN|>' + IntToStr(iX) + '<|>' +
      IntToStr(iY) + '<|END|>');
  end
  else
  begin
    Conexao.SocketPrincipal.Socket.SendText('<|REDIRECT|><|SETMOUSEMIDDLEDOWN|>'
      + IntToStr(iX) + '<|>' + IntToStr(iY) + '<|END|>');
  end;
end;

procedure TFormTelaRemota.imgTelaRemotaMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Single);
var
  iX, iY: Integer;
begin
  if not Active then
    Exit;

  iX := Trunc(X * Conexao.ResolucaoLargura) div Trunc(imgTelaRemota.Width);
  iY := Trunc(Y * Conexao.ResolucaoAltura) div Trunc(imgTelaRemota.Height);

  Conexao.SocketPrincipal.Socket.SendText('<|REDIRECT|><|SETMOUSEPOS|>' +
    IntToStr(iX) + '<|>' + IntToStr(iY) + '<|END|>');
end;

procedure TFormTelaRemota.imgTelaRemotaMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  iX, iY: Integer;
begin
  if not Active then
    Exit;

  iX := Trunc(X * Conexao.ResolucaoLargura) div Trunc(imgTelaRemota.Width);
  iY := Trunc(Y * Conexao.ResolucaoAltura) div Trunc(imgTelaRemota.Height);
  if (Button = TMouseButton.mbLeft) then
  begin
    Conexao.SocketPrincipal.Socket.SendText
      ('<|REDIRECT|><|SETMOUSELEFTCLICKUP|>' + IntToStr(iX) + '<|>' +
      IntToStr(iY) + '<|END|>');
  end
  else if (Button = TMouseButton.mbRight) then
  begin
    Conexao.SocketPrincipal.Socket.SendText
      ('<|REDIRECT|><|SETMOUSERIGHTCLICKUP|>' + IntToStr(iX) + '<|>' +
      IntToStr(iY) + '<|END|>');
  end
  else
  begin
    Conexao.SocketPrincipal.Socket.SendText('<|REDIRECT|><|SETMOUSEMIDDLEUP|>' +
      IntToStr(iX) + '<|>' + IntToStr(iY) + '<|END|>');
  end;
end;

procedure TFormTelaRemota.SendSocketKeys(AKeys: string);
begin
  Conexao.SocketTeclado.Socket.SendText(AKeys);
end;

procedure TFormTelaRemota.WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
var
  MinMaxInfo: PMinMaxInfo;
begin
  inherited;
  MinMaxInfo := Message.MinMaxInfo;
  MinMaxInfo^.ptMinTrackSize.X := 800; // Minimum Width
  MinMaxInfo^.ptMinTrackSize.Y := 500; // Minimum Height
end;

end.
