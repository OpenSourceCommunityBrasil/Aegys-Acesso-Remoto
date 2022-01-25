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
  uFunctions, System.UIConsts, System.StrUtils, System.DateUtils, FMX.ListBox,
  System.ImageList, FMX.ImgList, ufrMonitorItem;

Type
  TTagEffect = (te_Open, te_Close);
  TExecuteProc = Reference to Procedure;

type
  TFormTelaRemota = class(TForm)
    ActionList1: TActionList;
    PROC_ARQUIVOS: TAction;
    PROC_CHAT: TAction;
    tCapturarComandos: TTimer;
    imgTelaInicial: TImage;
    PROC_REDIMENSIONAR: TAction;
    PROC_LOG: TAction;
    sbMouse: TSpeedButton;
    Path3: TPath;
    LMouse: TLabel;
    PROC_MOUSE: TAction;
    imgTelaRemota: TRectangle;
    sbBlockInput: TSpeedButton;
    Path4: TPath;
    lblockinput: TLabel;
    PROC_BLOCKINPUT: TAction;
    lyToolBar: TLayout;
    flToolBar: TFlowLayout;
    vsbPopupMonitor: TVertScrollBox;
    rPopupMonitor: TRectangle;
    frMonitorItem1: TfrMonitorItem;
    frMonitorItem2: TfrMonitorItem;
    rrToolBarToggle: TRoundRect;
    LToolBarToggle: TLabel;
    rToolBar: TRectangle;
    Line1: TLine;
    sbChat: TSpeedButton;
    Path1: TPath;
    LChat: TLabel;
    sbFiles: TSpeedButton;
    Path2: TPath;
    LFiles: TLabel;
    Line2: TLine;
    rMenuButton: TRectangle;
    irMenuButton: TImage;
    LirMenuButton: TLabel;
    rDDMonitor: TRectangle;
    LDDMonitor: TLabel;
    lyCloseDropDown: TLayout;
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
    procedure PROC_BLOCKINPUTExecute(Sender: TObject);
    procedure rDDMonitorClick(Sender: TObject);
    procedure rrToolBarToggleClick(Sender: TObject);
    procedure SetColors;
    procedure lyCloseDropDownClick(Sender: TObject);
  private
    vLastMon, vActualIDConnected: String;
    Locale: TLocale;
    MenuDirection: TTagEffect;
    cShowForm, FCollapsed: Boolean;
    procedure RetornaMargem;
    procedure SendSocketKeys(AKeys: string);
    procedure WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
      message WM_GETMINMAXINFO;
    procedure Translate;
    Procedure Wait(Value: Integer);
    procedure MonitorFrameClick(Sender: TObject);
    procedure ToggleDropDown(forceClose: Boolean = false);
  public
    CtrlPressed, ShiftPressed, AltPressed, IgnoreKey: Boolean;
    Procedure AddItems(MoniNum: Integer);
    Property ActualIDConnected: String Read vActualIDConnected
      Write vActualIDConnected;
    Property ActualScreen: String Read vLastMon Write vLastMon;
  end;

var
  FormTelaRemota: TFormTelaRemota;

implementation

{$R *.fmx}

uses uFormArquivos, uFileTransfer, uFormChat, uFormConexao, Winapi.Windows, uDM,
  FMX.Platform.Win, uConstants;

Procedure TFormTelaRemota.Wait(Value: Integer);
Var
  vActual: TDateTime;
  vTempWait: LongWord;
Begin
  vActual := Now;
  vTempWait := System.DateUtils.MilliSecondsBetween(Now, vActual);
  While (Value >= vTempWait) Do
  Begin
    vTempWait := System.DateUtils.MilliSecondsBetween(Now, vActual);
    sleep(1); // Application.ProcessMessages;
  End;
End;

procedure TFormTelaRemota.rrToolBarToggleClick(Sender: TObject);
begin
  if FCollapsed then
  begin
    rrToolBarToggle.Position.Y := lyToolBar.Height;
    LToolBarToggle.RotationAngle := 0;
    lyToolBar.Visible := true;
    FCollapsed := not FCollapsed;
  end
  else
  begin
    rrToolBarToggle.Position.Y := 0;
    LToolBarToggle.RotationAngle := 180;
    lyToolBar.Visible := false;
    FCollapsed := not FCollapsed;
  end;
end;

Procedure TFormTelaRemota.AddItems(MoniNum: Integer);
Var
  I: Integer;
  dummyframe: TfrMonitorItem;
Begin
  if MoniNum < 3 then
    rPopupMonitor.Height := MoniNum * 50
  else
    rPopupMonitor.Height := 100;

  vsbPopupMonitor.Clear;
  vsbPopupMonitor.BeginUpdate;
  for I := 0 to pred(MoniNum) do
  begin
    dummyframe := TfrMonitorItem.Create(nil);
    dummyframe.LiPopupMonitor.Text := (I + 1).ToString;
    dummyframe.OnClick := MonitorFrameClick;
    vsbPopupMonitor.AddObject(dummyframe);
  end;
  vsbPopupMonitor.EndUpdate;
End;

procedure TFormTelaRemota.tCapturarComandosTimer(Sender: TObject);
var
  I: Byte;
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
        AltPressed := true;
        SendSocketKeys('<|ALTDOWN|>');
      end;
    end
    else
    begin
      if (GetKeyState(VK_MENU) > -1) then
      begin
        AltPressed := false;
        SendSocketKeys('<|ALTUP|>');
      end;
    end;

    // Ctrl
    if not(CtrlPressed) then
    begin
      if (GetKeyState(VK_CONTROL) < 0) then
      begin
        CtrlPressed := true;
        SendSocketKeys('<|CTRLDOWN|>');
      end;
    end
    else
    begin
      if (GetKeyState(VK_CONTROL) > -1) then
      begin
        CtrlPressed := false;
        SendSocketKeys('<|CTRLUP|>');
      end;
    end;

    // Shift
    if not(ShiftPressed) then
    begin
      if (GetKeyState(VK_SHIFT) < 0) then
      begin
        ShiftPressed := true;
        SendSocketKeys('<|SHIFTDOWN|>');
      end;
    end
    else
    begin
      if (GetKeyState(VK_SHIFT) > -1) then
      begin
        ShiftPressed := false;
        SendSocketKeys('<|SHIFTUP|>');
      end;
    end;

    for I := 8 to 228 do
    begin
      if (GetAsyncKeyState(I) = -32767) then
      begin
        case I of
          8:
            SendSocketKeys('{BS}');
          9:
            SendSocketKeys('{TAB}');
          13:
            SendSocketKeys('{ENTER}');
          27:
            begin
              if IgnoreKey then
                IgnoreKey := false
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
              IgnoreKey := true;
              Keybd_Event(VK_ESCAPE, 0, 0, 0);
              SendSocketKeys('{LWIN}');
            end;
          92:
            begin
              IgnoreKey := true;
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
                  SendSocketKeys(LowerCase(Chr(I)))
                else
                  SendSocketKeys(UpperCase(Chr(I)))
              else if (GetKeyState(VK_SHIFT) < 0) then
                SendSocketKeys(UpperCase(Chr(I)))
              else
                SendSocketKeys(LowerCase(Chr(I)))

            end;

          96 .. 105: // Numpad 1..9
            SendSocketKeys(IntToStr(I - 96));

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
            SendSocketKeys('{F' + IntToStr(I - 111) + '}');

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

procedure TFormTelaRemota.ToggleDropDown(forceClose: Boolean = false);
begin
  if not rPopupMonitor.Visible then
  begin
    lyCloseDropDown.Visible := true;
    rPopupMonitor.Position.Y := lyToolBar.Height - 10;
    rPopupMonitor.Position.X := lyToolBar.Width + lyToolBar.Position.X -
      (rPopupMonitor.Width + 10);
    LDDMonitor.RotationAngle := 0;
  end
  else
  begin
    lyCloseDropDown.Visible := false;
    LDDMonitor.RotationAngle := 180;
  end;
  rPopupMonitor.Visible := not rPopupMonitor.Visible;
end;

procedure TFormTelaRemota.Translate;
begin
  LMouse.Text := Locale.GetLocale(FRMS, 'RemoteMouse');
  LFiles.Text := Locale.GetLocale(FRMS, 'RemoteFile');
  LChat.Text := Locale.GetLocale(FRMS, 'RemoteChat');
end;

procedure TFormTelaRemota.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  cShowForm := true;
  // FormArquivos.Hide;
  if Assigned(fFileTransfer) then
    fFileTransfer.Close;
  FormChat.Hide;
  Conexao.SocketPrincipal.Socket.SendText('<|STOPACCESS|>');
  FormConexao.SetOnline;
  FormConexao.Show;
end;

procedure TFormTelaRemota.FormCreate(Sender: TObject);
begin
  Locale := TLocale.Create;
  lyToolBar.Visible := false;
  rrToolBarToggle.Position.Y := 0;
  rPopupMonitor.Visible := false;
  FCollapsed := true;
  cShowForm := true;
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
    Abort;
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
  PROC_REDIMENSIONAR.Execute;
end;

procedure TFormTelaRemota.FormShow(Sender: TObject);
begin
  CtrlPressed := false;
  ShiftPressed := false;
  AltPressed := false;
  tCapturarComandos.Enabled := true;
  PROC_REDIMENSIONAR.Execute;

  sbMouse.StaysPressed := Conexao.MostrarMouse;
  Path4.Fill.Color := StringToAlphaColor('#FFFF1E1E');
  Conexao.BlockInputs := false;
  lblockinput.Text := Locale.GetLocale(FRMS, 'RemoteBlock');
  if cShowForm then
  Begin
    FCollapsed := true;
    rrToolBarToggle.OnClick(rrToolBarToggle);
  End;
  cShowForm := false;
end;

procedure TFormTelaRemota.PROC_ARQUIVOSExecute(Sender: TObject);
begin
  if Not Assigned(fFileTransfer) then
    Application.CreateForm(TfFileTransfer, fFileTransfer);
  fFileTransfer.Show;
end;

procedure TFormTelaRemota.PROC_BLOCKINPUTExecute(Sender: TObject);
begin
  if Path4.Fill.Color = StringToAlphaColor('#FFFF1E1E') then
  begin
    // block
    Path4.Fill.Color := StringToAlphaColor('#FF166600');
    Conexao.BlockInputs := true;
    lblockinput.Text := Locale.GetLocale(FRMS, 'RemoteRelease');
  end
  else
  begin
    Path4.Fill.Color := StringToAlphaColor('#FFFF1E1E');
    // unblock
    Conexao.BlockInputs := false;
    lblockinput.Text := Locale.GetLocale(FRMS, 'RemoteBlock');
  end;

end;

procedure TFormTelaRemota.PROC_CHATExecute(Sender: TObject);
begin
  FormChat.Show;
end;

procedure TFormTelaRemota.PROC_MOUSEExecute(Sender: TObject);
begin
  Conexao.MostrarMouse := not Conexao.MostrarMouse;
  sbMouse.StaysPressed := Conexao.MostrarMouse;
end;

procedure TFormTelaRemota.PROC_REDIMENSIONARExecute(Sender: TObject);
begin
  lyToolBar.Position.X := (imgTelaRemota.Width / 2) - (lyToolBar.Width / 2);
  rrToolBarToggle.Position.X := (imgTelaRemota.Width / 2) -
    (rrToolBarToggle.Width / 2);
  RetornaMargem;
end;

procedure TFormTelaRemota.rDDMonitorClick(Sender: TObject);
begin
  ToggleDropDown;
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

  // imgTelaRemota.Margins.Left := iMarginW;
  // imgTelaRemota.Margins.Right := iMarginW;
  // imgTelaRemota.Margins.Top := iMarginH;
  // imgTelaRemota.Margins.Bottom := iMarginH;
end;

procedure TFormTelaRemota.imgTelaRemotaMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  iX, iY: Integer;
  Sblockinput: String;
begin
  if not Active then
    Exit;
  Sblockinput := IfThen(Conexao.BlockInputs, '<|BLOCKINPUT|>', '');

  iX := Trunc(X * Conexao.ResolucaoLargura) div Trunc(imgTelaRemota.Width);
  iY := Trunc(Y * Conexao.ResolucaoAltura) div Trunc(imgTelaRemota.Height);

  if (Button = TMouseButton.mbLeft) then
  begin
    Conexao.SocketPrincipal.Socket.SendText
      ('<|REDIRECT|><|SETMOUSELEFTCLICKDOWN|>' + IntToStr(iX) + '<|>' +
      IntToStr(iY) + '<|END|>' + Sblockinput);
  end
  else if (Button = TMouseButton.mbRight) then
  begin
    Conexao.SocketPrincipal.Socket.SendText
      ('<|REDIRECT|><|SETMOUSERIGHTCLICKDOWN|>' + IntToStr(iX) + '<|>' +
      IntToStr(iY) + '<|END|>' + Sblockinput);
  end
  else
  begin
    Conexao.SocketPrincipal.Socket.SendText('<|REDIRECT|><|SETMOUSEMIDDLEDOWN|>'
      + IntToStr(iX) + '<|>' + IntToStr(iY) + '<|END|>' + Sblockinput);
  end;
end;

procedure TFormTelaRemota.imgTelaRemotaMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Single);
var
  iX, iY: Integer;
  Sblockinput: String;
begin
  if not Active then
    Exit;

  iX := Trunc(X * Conexao.ResolucaoLargura) div Trunc(imgTelaRemota.Width);
  iY := Trunc(Y * Conexao.ResolucaoAltura) div Trunc(imgTelaRemota.Height);
  Sblockinput := IfThen(Conexao.BlockInputs, '<|BLOCKINPUT|>', '');
  Conexao.SocketPrincipal.Socket.SendText('<|REDIRECT|><|SETMOUSEPOS|>' +
    IntToStr(iX) + '<|>' + IntToStr(iY) + '<|END|>' + Sblockinput);
end;

procedure TFormTelaRemota.imgTelaRemotaMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  iX, iY: Integer;
  Sblockinput: string;
begin
  if not Active then
    Exit;
  Sblockinput := IfThen(Conexao.BlockInputs, '<|BLOCKINPUT|>', '');

  iX := Trunc(X * Conexao.ResolucaoLargura) div Trunc(imgTelaRemota.Width);
  iY := Trunc(Y * Conexao.ResolucaoAltura) div Trunc(imgTelaRemota.Height);
  if (Button = TMouseButton.mbLeft) then
  begin
    Conexao.SocketPrincipal.Socket.SendText
      ('<|REDIRECT|><|SETMOUSELEFTCLICKUP|>' + IntToStr(iX) + '<|>' +
      IntToStr(iY) + '<|END|>' + Sblockinput);
  end
  else if (Button = TMouseButton.mbRight) then
  begin
    Conexao.SocketPrincipal.Socket.SendText
      ('<|REDIRECT|><|SETMOUSERIGHTCLICKUP|>' + IntToStr(iX) + '<|>' +
      IntToStr(iY) + '<|END|>' + Sblockinput);
  end
  else
  begin
    Conexao.SocketPrincipal.Socket.SendText('<|REDIRECT|><|SETMOUSEMIDDLEUP|>' +
      IntToStr(iX) + '<|>' + IntToStr(iY) + '<|END|>' + Sblockinput);
  end;
end;

procedure TFormTelaRemota.lyCloseDropDownClick(Sender: TObject);
begin
  ToggleDropDown(true);
  lyCloseDropDown.Visible := false;
end;

procedure TFormTelaRemota.MonitorFrameClick(Sender: TObject);
begin
  LirMenuButton.Text := (Sender as TfrMonitorItem).LiPopupMonitor.Text;

  If Not(cShowForm) Then
    If vLastMon <> (LirMenuButton.Text.ToInteger - 1).ToString then
    Begin
      vLastMon := (LirMenuButton.Text.ToInteger - 1).ToString;
      Conexao.SocketPrincipal.Socket.SendText('<|CHANGEMONITOR|>' +
        vActualIDConnected + '<|>' + vLastMon + '<|END|>');
    End;
  ToggleDropDown;
end;

procedure TFormTelaRemota.SendSocketKeys(AKeys: string);
var
  Sblockinput: string;
begin
  Sblockinput := IfThen(Conexao.BlockInputs, '<|BLOCKINPUT|>', '');
  Conexao.SocketTeclado.Socket.SendText(AKeys + Sblockinput);
end;

procedure TFormTelaRemota.SetColors;
begin
  rPopupMonitor.Fill.Color := SECONDARY_COLOR;
  rrToolBarToggle.Fill.Color := SECONDARY_COLOR;
  rToolBar.Fill.Color := SECONDARY_COLOR;
  rDDMonitor.Fill.Color := SECONDARY_COLOR;
  rMenuButton.Fill.Color := SECONDARY_COLOR;
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
