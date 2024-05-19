unit uFormTelaRemota;

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

Uses
 System.SysUtils, System.Types, System.UITypes, System.Classes, System.StrUtils,
 System.Variants, System.Actions, System.UIConsts, System.DateUtils,
 System.ImageList,
 FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
 FMX.StdCtrls, FMX.Controls.Presentation, FMX.Layouts, FMX.ActnList,
 FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.ListBox, FMX.ImgList,
 FMX.Platform.Win,
 Winapi.Messages, Winapi.Windows,
 uAegysBase, uAegysBufferPack, uAegysConsts, uAegysDataTypes,

 // ERRO
 Vcl.Graphics,
 // ERRO

 uFunctions, ufrMonitorItem, StreamManager, uLibClass,
 uConstants, uLocale
     ;

Type
 TTagEffect   = (te_Open, te_Close);
 TExecuteProc = Reference to Procedure;

type
  TFormTelaRemota = Class(TForm)
    ActionList1        : TActionList;
    PROC_ARQUIVOS      : TAction;
    PROC_CHAT          : TAction;
    tCapturarComandos  : TTimer;
    imgTelaInicial     : TImage;
    PROC_REDIMENSIONAR : TAction;
    PROC_LOG           : TAction;
    sbMouse            : TSpeedButton;
    Path3              : TPath;
    LMouse             : TLabel;
    PROC_MOUSE         : TAction;
    imgTelaRemota      : TRectangle;
    sbBlockInput       : TSpeedButton;
    Path4              : TPath;
    lblockinput        : TLabel;
    PROC_BLOCKINPUT    : TAction;
    lyToolBar          : TLayout;
    flToolBar          : TFlowLayout;
    vsbPopupMonitor    : TVertScrollBox;
    rPopupMonitor      : TRectangle;
    frMonitorItem1     : TfrMonitorItem;
    frMonitorItem2     : TfrMonitorItem;
    rrToolBarToggle    : TRoundRect;
    LToolBarToggle     : TLabel;
    rToolBar           : TRectangle;
    Line1              : TLine;
    sbChat             : TSpeedButton;
    Path1              : TPath;
    LChat              : TLabel;
    sbFiles            : TSpeedButton;
    Path2              : TPath;
    LFiles             : TLabel;
    Line2              : TLine;
    rMenuButton        : TRectangle;
    irMenuButton       : TImage;
    LirMenuButton      : TLabel;
    rDDMonitor         : TRectangle;
    LDDMonitor         : TLabel;
    lyCloseDropDown    : TLayout;
    taction: TTimer;
    procedure PROC_ARQUIVOSExecute  (Sender      : TObject);
    procedure PROC_CHATExecute      (Sender      : TObject);
    procedure imgTelaRemotaMouseMove(Sender      : TObject;
                                     Shift       : TShiftState;
                                     X, Y        : Single);
    procedure imgTelaRemotaMouseDown(Sender      : TObject;
                                     Button      : TMouseButton;
                                     Shift       : TShiftState;
                                     X, Y        : Single);
    procedure imgTelaRemotaMouseUp  (Sender      : TObject;
                                     Button      : TMouseButton;
                                     Shift       : TShiftState;
                                     X, Y        : Single);
    procedure tCapturarComandosTimer(Sender      : TObject);
    procedure FormClose             (Sender      : TObject;
                                     Var Action  : TCloseAction);
    procedure FormCreate            (Sender      : TObject);
    procedure FormMouseWheel        (Sender      : TObject;
                                     Shift       : TShiftState;
                                     WheelDelta  : Integer;
                                     Var Handled : Boolean);
    procedure FormShow              (Sender      : TObject);
    procedure PROC_REDIMENSIONARExecute(Sender   : TObject);
    procedure FormResize            (Sender      : TObject);
    procedure FormKeyDown           (Sender      : TObject;
                                     Var Key     : Word;
                                     Var KeyChar : Char;
                                     Shift       : TShiftState);
    procedure PROC_MOUSEExecute     (Sender      : TObject);
    procedure PROC_BLOCKINPUTExecute(Sender      : TObject);
    procedure rDDMonitorClick       (Sender      : TObject);
    procedure rrToolBarToggleClick  (Sender      : TObject);
    procedure SetColors;
    procedure lyCloseDropDownClick  (Sender      : TObject);
    procedure tactionTimer(Sender: TObject);
  private
    procedure RetornaMargem;
    procedure SendSocketKeys        (AKeys       : String);
    procedure SendSocketMouse       (AKeys       : String);
    procedure WMGetMinMaxInfo       (Var Message : TWMGetMinMaxInfo); Message WM_GETMINMAXINFO;
    procedure Translate;
    Procedure Wait(Value: Integer);
    procedure MonitorFrameClick     (Sender      : TObject);
    procedure ToggleDropDown        (forceClose  : Boolean = false);
    function TestResolution(X, Y,
                            aValue,
                            bValue : Integer): Boolean;
  public
   vInAction          : Boolean;
   vLastMon,
   vConnection,
   vActualIDConnected : String;
   aPackList          : TPackList;
   vMouseMove         : TStringList;
   Procedure AddItems(MoniNum  : Integer);
   Property  ActualIDConnected : String Read vActualIDConnected Write vActualIDConnected;
   Property  ActualScreen      : String Read vLastMon           Write vLastMon;
   Property  Connection        : String Read vConnection        Write vConnection;
  end;

Var
  FormTelaRemota     : TFormTelaRemota;
  Locale             : TLocale;
  MenuDirection      : TTagEffect;
  CtrlPressed,
  ShiftPressed,
  AltPressed,
  IgnoreKey,
  vMostrarMouse,
  vBlockInputs,
  cShowForm,
  FCollapsed         : Boolean;

Implementation

{$R *.fmx}

Uses
  uFileTransfer, uFormChat, uFormConexao, uDM;

Procedure TFormTelaRemota.Wait(Value: Integer);
Var
  vActual   : TDateTime;
  vTempWait : LongWord;
Begin
 vActual := Now;
 vTempWait := System.DateUtils.MilliSecondsBetween(Now, vActual);
 While (Value >= vTempWait) Do
  Begin
   vTempWait := System.DateUtils.MilliSecondsBetween(Now, vActual);
   sleep(cDelayThread);
  End;
End;

Procedure TFormTelaRemota.rrToolBarToggleClick(Sender: TObject);
Begin
 If FCollapsed Then
  Begin
   rrToolBarToggle.Position.Y := lyToolBar.Height;
   LToolBarToggle.RotationAngle := 0;
   lyToolBar.Visible := true;
   FCollapsed := not FCollapsed;
  End
 Else
  Begin
   rrToolBarToggle.Position.Y := 0;
   LToolBarToggle.RotationAngle := 180;
   lyToolBar.Visible := false;
   FCollapsed := not FCollapsed;
  End;
End;

Procedure TFormTelaRemota.AddItems(MoniNum: Integer);
Var
 I          : Integer;
 dummyframe : TfrMonitorItem;
Begin
 If MoniNum < 3 Then
  rPopupMonitor.Height := MoniNum * 50
 Else
  rPopupMonitor.Height := 100;
 vsbPopupMonitor.Clear;
 vsbPopupMonitor.BeginUpdate;
 For I := 0 To pred(MoniNum) Do
  Begin
   dummyframe := TfrMonitorItem.Create(nil);
   dummyframe.LiPopupMonitor.Text := (I + 1).ToString;
   dummyframe.OnClick := MonitorFrameClick;
   vsbPopupMonitor.AddObject(dummyframe);
  End;
 vsbPopupMonitor.EndUpdate;
End;

procedure TFormTelaRemota.tactionTimer(Sender: TObject);
Var
 sLineMouse : String;
begin
 vInAction       := True;
 taction.Enabled := False;
 Try
  If vMouseMove.Count > 0 Then
   Begin
    sLineMouse := StringReplace(vMouseMove.Text, sLineBreak, '', [rfReplaceAll]) + sLineMouse;
    vMouseMove.Clear;
    SendSocketMouse(sLineMouse);
   End;
 Finally
  vInAction       := False;
 End;
end;

Procedure TFormTelaRemota.tCapturarComandosTimer(Sender: TObject);
Var
 I : Byte;
Begin
 If Self <> Screen.ActiveForm Then
  Exit;
 // The keys programmed here, may not match the keys on your keyboard. I recommend to undertake adaptation.
 Try
  { Combo }
  // Alt
  If not(AltPressed) Then
   Begin
    If (GetKeyState(VK_MENU) < 0) Then
     Begin
      AltPressed := true;
      SendSocketKeys('<|ALTDOWN|>');
     End;
   End
  Else
   Begin
    If (GetKeyState(VK_MENU) > -1) Then
     Begin
      AltPressed := false;
      SendSocketKeys('<|ALTUP|>');
     End;
   End;
  // Ctrl
  If Not(CtrlPressed) Then
   Begin
    If (GetKeyState(VK_CONTROL) < 0) Then
     Begin
      CtrlPressed := true;
      SendSocketKeys('<|CTRLDOWN|>');
     End;
   End
  Else
   Begin
    If (GetKeyState(VK_CONTROL) > -1) Then
     Begin
      CtrlPressed := false;
      SendSocketKeys('<|CTRLUP|>');
     End;
   End;
   // Shift
  If Not(ShiftPressed) Then
   Begin
    If (GetKeyState(VK_SHIFT) < 0) Then
     Begin
      ShiftPressed := true;
      SendSocketKeys('<|SHIFTDOWN|>');
     End;
   End
  Else
   Begin
    If (GetKeyState(VK_SHIFT) > -1) Then
     Begin
      ShiftPressed := false;
      SendSocketKeys('<|SHIFTUP|>');
     End;
   End;
  For I := 8 To 228 Do
   Begin
    If (GetAsyncKeyState(I) = -32767) Then
     Begin
      Case I Of
       8   : SendSocketKeys('{BS}');
       9   : SendSocketKeys('{TAB}');
       13  : SendSocketKeys('{ENTER}');
       27  : Begin
              If IgnoreKey Then
               IgnoreKey := false
              Else
               SendSocketKeys('{ESCAPE}');
             End;
       32  : SendSocketKeys(' ');
       33  : SendSocketKeys('{PGUP}');
       34  : SendSocketKeys('{PGDN}');
       35  : SendSocketKeys('{END}');
       36  : SendSocketKeys('{HOME}');
       37  : SendSocketKeys('{LEFT}');
       38  : SendSocketKeys('{UP}');
       39  : SendSocketKeys('{RIGHT}');
       40  : SendSocketKeys('{DOWN}');
       44  : SendSocketKeys('{PRTSC}');
       46  : SendSocketKeys('{DEL}');
       91  : Begin
              IgnoreKey := true;
              Keybd_Event(VK_ESCAPE, 0, 0, 0);
              SendSocketKeys('{LWIN}');
             End;
       92  : Begin
              IgnoreKey := true;
              Keybd_Event(VK_ESCAPE, 0, 0, 0);
              SendSocketKeys('{RWIN}');
             End;
       145 : SendSocketKeys('{SCROLLLOCK}');
       // Numbers: 1 2 3 4 5 6 7 8 9 and ! @ # $ % ¨& * ( )
       48  : Begin
              If (GetKeyState(VK_SHIFT) < 0) Then
               SendSocketKeys(')')
              Else
              SendSocketKeys('0');
             End;
       49  : Begin
              If (GetKeyState(VK_SHIFT) < 0) Then
               SendSocketKeys('!')
              Else
               SendSocketKeys('1');
             End;
       50  : Begin
              If (GetKeyState(VK_SHIFT) < 0) Then
               SendSocketKeys('@')
              Else
               SendSocketKeys('2');
             End;
       51  : Begin
              If (GetKeyState(VK_SHIFT) < 0) Then
               SendSocketKeys('#')
              Else
               SendSocketKeys('3');
             End;
       52  : Begin
              If (GetKeyState(VK_SHIFT) < 0) Then
               SendSocketKeys('$')
              Else
               SendSocketKeys('4');
             End;
       53  : Begin
              If (GetKeyState(VK_SHIFT) < 0) Then
               SendSocketKeys('%')
              Else
               SendSocketKeys('5');
             End;
       54  : Begin
              If (GetKeyState(VK_SHIFT) < 0) Then
               SendSocketKeys('^')
              Else
               SendSocketKeys('6');
             End;
       55  : Begin
              If (GetKeyState(VK_SHIFT) < 0) Then
               SendSocketKeys('&')
              Else
               SendSocketKeys('7');
             End;
       56  : Begin
              If (GetKeyState(VK_SHIFT) < 0) Then
               SendSocketKeys('*')
              Else
               SendSocketKeys('8');
             End;
       57  : Begin
              If (GetKeyState(VK_SHIFT) < 0) Then
               SendSocketKeys('(')
              Else
               SendSocketKeys('9');
             End;
       65 .. 90 : Begin// A..Z / a..z
                   If (GetKeyState(VK_CAPITAL) = 1)    Then
                    Begin
                     If (GetKeyState(VK_SHIFT) < 0)    Then
                      SendSocketKeys(LowerCase(Chr(I)))
                     Else
                      SendSocketKeys(UpperCase(Chr(I)));
                    End
                   Else If (GetKeyState(VK_SHIFT) < 0) Then
                    SendSocketKeys(UpperCase(Chr(I)))
                   Else
                    SendSocketKeys(LowerCase(Chr(I)))
                  End;
       96 .. 105 : SendSocketKeys(IntToStr(I - 96)); // Numpad 1..9
       106 : SendSocketKeys('*');
       107 : SendSocketKeys('+');
       109 : SendSocketKeys('-');
       110 : SendSocketKeys(',');
       111 : SendSocketKeys('/');
       194 : SendSocketKeys('.');
       // F1..F12
       112 .. 123 : SendSocketKeys('{F' + IntToStr(I - 111) + '}');
       186 : Begin
              If (GetKeyState(VK_SHIFT) < 0) Then
               SendSocketKeys('Ç')
              Else
               SendSocketKeys('ç');
             End;
       187 : Begin
              If (GetKeyState(VK_SHIFT) < 0) Then
               SendSocketKeys('+')
              Else
               SendSocketKeys('=');
             End;
       188 : Begin
              If (GetKeyState(VK_SHIFT) < 0) Then
               SendSocketKeys('<')
              Else
               SendSocketKeys(',');
             End;
       189 : Begin
              If (GetKeyState(VK_SHIFT) < 0) Then
               SendSocketKeys('_')
              Else
               SendSocketKeys('-');
             End;
       190 : Begin
              If (GetKeyState(VK_SHIFT) < 0) Then
               SendSocketKeys('>')
              Else
               SendSocketKeys('.');
             End;
       191 : Begin
              If (GetKeyState(VK_SHIFT) < 0) Then
               SendSocketKeys(':')
              Else
               SendSocketKeys(';');
             End;
       192 : Begin
              If (GetKeyState(VK_SHIFT) < 0) Then
               SendSocketKeys('"')
              Else
               SendSocketKeys('''');
             End;
       193 : Begin
              If (GetKeyState(VK_SHIFT) < 0) Then
               SendSocketKeys('?')
              Else
               SendSocketKeys('/');
             End;
       219 : Begin
              If (GetKeyState(VK_SHIFT) < 0) Then
               SendSocketKeys('`')
              Else
               SendSocketKeys('´');
             End;
       220 : Begin
              If (GetKeyState(VK_SHIFT) < 0) Then
               SendSocketKeys('}')
              Else
               SendSocketKeys(']');
             End;
       221 : Begin
              If (GetKeyState(VK_SHIFT) < 0) Then
               SendSocketKeys('{')
              Else
               SendSocketKeys('[');
             End;
       222 : Begin
              If (GetKeyState(VK_SHIFT) < 0) Then
               SendSocketKeys('^')
              Else
               SendSocketKeys('~');
             End;
       226 : Begin
              If (GetKeyState(VK_SHIFT) < 0) Then
               SendSocketKeys('|')
              Else
               SendSocketKeys('\');
             End;
      End;
     End;
   End;
 Except
 End;
End;

Procedure TFormTelaRemota.ToggleDropDown(forceClose: Boolean = false);
Begin
 If Not rPopupMonitor.Visible Then
  Begin
   lyCloseDropDown.Visible := true;
   rPopupMonitor.Position.Y := lyToolBar.Height - 10;
   rPopupMonitor.Position.X := lyToolBar.Width + lyToolBar.Position.X - (rPopupMonitor.Width + 10);
   LDDMonitor.RotationAngle := 0;
  End
 Else
  Begin
   lyCloseDropDown.Visible := false;
   LDDMonitor.RotationAngle := 180;
  End;
 rPopupMonitor.Visible := Not rPopupMonitor.Visible;
End;

Procedure TFormTelaRemota.Translate;
Begin
 LMouse.Text := Locale.GetLocale(lsFORMS, lvFrmRemoteMouse);
 LFiles.Text := Locale.GetLocale(lsFORMS, lvFrmRemoteFile);
 LChat.Text  := Locale.GetLocale(lsFORMS, lvFrmRemoteChat);
End;

Procedure TFormTelaRemota.FormClose  (Sender      : TObject;
                                      Var Action  : TCloseAction);
Begin
 taction.Enabled  := False;
 cShowForm        := True;
 FormConexao.SetPeerDisconnected;
 Locale.DisposeOf;
 FreeAndNil(vMouseMove);
 If Assigned(fFileTransfer) then
  FreeAndNil(fFileTransfer);
 If Assigned(FormChat) then
  FreeAndNil(FormChat);
 Try
  Conexao.DisconnectAllPeers;
  FormConexao.SetOnline;
 Finally
  FormConexao.Show;
  FormTelaRemota := Nil;
  Release;
 End;
 FreeAndNil(aPackList);
End;

Procedure TFormTelaRemota.FormCreate (Sender      : TObject);
Begin
 Locale                     := TLocale.Create;
 lyToolBar.Visible          := False;
 rrToolBarToggle.Position.Y := 0;
 rPopupMonitor.Visible      := False;
 FCollapsed                 := True;
 cShowForm                  := True;
 aPackList                  := TPackList.Create;
 vMouseMove                 := TStringList.Create;
 vInAction                  := False;
 taction.Enabled            := vInAction;
 SetWindowLong(FmxHandleToHWND(Handle), GWL_EXSTYLE, WS_EX_APPWINDOW);
End;

Procedure TFormTelaRemota.FormKeyDown(Sender      : TObject;
                                      Var Key     : Word;
                                      Var KeyChar : Char;
                                      Shift       : TShiftState);
Begin
 If ((ssAlt in Shift) And (Key = vkF4)) Or
    ((Key = vkReturn)) Then
  Abort;
End;

Procedure TFormTelaRemota.FormMouseWheel(Sender      : TObject;
                                         Shift       : TShiftState;
                                         WheelDelta  : Integer;
                                         Var Handled : Boolean);
Begin
 If not Active Then
  Exit;
 SendSocketMouse('<|WHEELMOUSE|>' + IntToStr(WheelDelta) + cEndTag);
End;

Procedure TFormTelaRemota.FormResize(Sender: TObject);
Begin
 PROC_REDIMENSIONAR.Execute;
End;

Procedure TFormTelaRemota.FormShow(Sender: TObject);
Begin
 CtrlPressed               := False;
 ShiftPressed              := False;
 AltPressed                := False;
 tCapturarComandos.Enabled := True;
 PROC_REDIMENSIONAR.Execute;
 sbMouse.StaysPressed      := vMostrarMouse;
 Path4.Fill.Color          := StringToAlphaColor('#FFFF1E1E');
 vBlockInputs              := False;
 lblockinput.Text          := Locale.GetLocale(lsFORMS, lvFrmRemoteBlock);
 If cShowForm Then
  Begin
   FCollapsed := true;
   rrToolBarToggle.OnClick(rrToolBarToggle);
  End;
 cShowForm := false;
End;

Procedure TFormTelaRemota.PROC_ARQUIVOSExecute(Sender: TObject);
Begin
 if Not Assigned(fFileTransfer) then
  fFileTransfer := TfFileTransfer.Create(Nil);
 fFileTransfer.Show;
End;

Procedure TFormTelaRemota.PROC_BLOCKINPUTExecute(Sender: TObject);
Begin
 If Path4.Fill.Color = StringToAlphaColor('#FFFF1E1E') Then
  Begin
   Path4.Fill.Color := StringToAlphaColor('#FF166600');
   vBlockInputs     := True;
   lblockinput.Text := Locale.GetLocale(lsFORMS, lvFrmRemoteRelease);
  End
 Else
  Begin
   Path4.Fill.Color := StringToAlphaColor('#FFFF1E1E');
   vBlockInputs     := False;
   lblockinput.Text := Locale.GetLocale(lsFORMS, lvFrmRemoteBlock);
  End;
End;

Procedure TFormTelaRemota.PROC_CHATExecute(Sender: TObject);
Begin
 If Not Assigned(FormChat) Then
  FormChat := TFormChat.Create(Self);
 FormChat.Show;
End;

Procedure TFormTelaRemota.PROC_MOUSEExecute(Sender: TObject);
Begin
 vMostrarMouse        := Not vMostrarMouse;
 sbMouse.StaysPressed := vMostrarMouse;
 If vMostrarMouse Then
  vMouseMove.Add(cShowMouse)
 Else
  vMouseMove.Add(cHideMouse);
End;

Procedure TFormTelaRemota.PROC_REDIMENSIONARExecute(Sender: TObject);
Begin
 lyToolBar.Position.X       := (imgTelaRemota.Width / 2) - (lyToolBar.Width / 2);
 rrToolBarToggle.Position.X := (imgTelaRemota.Width / 2) - (rrToolBarToggle.Width / 2);
 RetornaMargem;
End;

Procedure TFormTelaRemota.rDDMonitorClick(Sender: TObject);
Begin
 ToggleDropDown;
End;

Procedure TFormTelaRemota.RetornaMargem;
Var
 iImageH,
 iImageW,
 iTargetH,
 iTargetW,
 iMarginW,
 iMarginH  : Single;
Begin
 imgTelaRemota.Margins.Left   := 0;
 imgTelaRemota.Margins.Right  := 0;
 imgTelaRemota.Margins.Top    := 0;
 imgTelaRemota.Margins.Bottom := 0;
 iImageH                      := imgTelaRemota.Height;
 iImageW                      := imgTelaRemota.Width;
 iTargetH                     := vResolucaoAltura;
 iTargetW                     := vResolucaoLargura;
 iMarginW                     := iTargetW * iImageH;
 iMarginW                     := iMarginW / iTargetH;
 If iImageW > iMarginW Then
  Begin
   iMarginH := 0;
   iMarginW := iImageW - iMarginW;
   iMarginW := iMarginW / 2;
  End
 Else
  Begin
   iMarginW := 0;
   iMarginH := iTargetH * iImageW;
   iMarginH := iMarginH / iTargetW;
   iMarginH := iImageH - iMarginH;
   iMarginH := iMarginH / 2;
  End;
End;

Procedure TFormTelaRemota.imgTelaRemotaMouseDown(Sender : TObject;
                                                 Button : TMouseButton;
                                                 Shift  : TShiftState;
                                                 X, Y   : Single);
Var
 iX, iY      : Integer;
 Sblockinput : String;
Begin
 If Not Active Then
  Exit;
 Sblockinput := IfThen(vBlockInputs, cBlockInput, '');
 If TestResolution(4, 3, vResolucaoLargura, vResolucaoAltura) Then
  Begin
   iX          := Trunc(X * vResolucaoLargura) Div Trunc(imgTelaRemota.Width);
   iY          := Trunc(Y * vResolucaoAltura)  Div Trunc(imgTelaRemota.Height);
  End
 Else
  Begin
   iX          := Round((X * 100) / Round(imgTelaRemota.Width));
   iY          := Round((Y * 100) / Round(imgTelaRemota.Height));
   iX          := ((Round(vResolucaoLargura) * iX) Div 100);
   iY          := ((Round(vResolucaoAltura)  * iY) Div 100);
  End;
 If (Button = TMouseButton.mbLeft)       Then
  SendSocketMouse(cMouseClickLeftDown   + IntToStr(iX) + '<|>' + IntToStr(iY) + cEndTag + Sblockinput)
 Else If (Button = TMouseButton.mbRight) Then
  SendSocketMouse(cMouseClickRightDown  + IntToStr(iX) + '<|>' + IntToStr(iY) + cEndTag + Sblockinput)
 Else
  SendSocketMouse(cMouseClickMiddleDown + IntToStr(iX) + '<|>' + IntToStr(iY) + cEndTag + Sblockinput);
End;

Function TFormTelaRemota.TestResolution(X, Y,
                                        aValue,
                                        bValue  : Integer) : Boolean;
Begin
 Result := (((aValue div Y) * X) = bValue) or
           (((bValue div Y) * X) = aValue);
End;

Procedure TFormTelaRemota.imgTelaRemotaMouseMove(Sender : TObject;
                                                 Shift  : TShiftState;
                                                 X, Y   : Single);
Var
 iX, iY      : Integer;
 sLineMouse,
 Sblockinput : String;
Begin
 taction.Enabled := False;
 If Not Active Then
  Exit;
 If vInAction  Then
  Exit;
 If TestResolution(4, 3, vResolucaoLargura, vResolucaoAltura) Then
  Begin
   iX          := Trunc(X * vResolucaoLargura) Div Trunc(imgTelaRemota.Width);
   iY          := Trunc(Y * vResolucaoAltura)  Div Trunc(imgTelaRemota.Height);
  End
 Else
  Begin
   iX          := Round((X * 100) / Round(imgTelaRemota.Width));
   iY          := Round((Y * 100) / Round(imgTelaRemota.Height));
   iX          := ((Round(vResolucaoLargura) * iX) Div 100);
   iY          := ((Round(vResolucaoAltura)  * iY) Div 100);
  End;
 Sblockinput := IfThen(vBlockInputs, cBlockInput, '');
 If vMouseMove.Count <= cMousePack Then
  vMouseMove.Add(cMousePos + IntToStr(iX) + '<|>' + IntToStr(iY) + cEndTag + Sblockinput)
 Else
  Begin
   sLineMouse := cMousePos + IntToStr(iX) + '<|>' + IntToStr(iY) + cEndTag + Sblockinput;
   sLineMouse := StringReplace(vMouseMove.Text, sLineBreak, '', [rfReplaceAll]) + sLineMouse;
   vMouseMove.Clear;
   SendSocketMouse(sLineMouse);
  End;
End;

Procedure TFormTelaRemota.imgTelaRemotaMouseUp(Sender : TObject;
                                               Button : TMouseButton;
                                               Shift  : TShiftState;
                                               X, Y   : Single);
Var
 iX,
 iY          : Integer;
 Sblockinput : String;
Begin
 If Not Active Then
  Exit;
 Sblockinput := IfThen(vBlockInputs, cBlockInput, '');
 If TestResolution(4, 3, vResolucaoLargura, vResolucaoAltura) Then
  Begin
   iX          := Trunc(X * vResolucaoLargura) Div Trunc(imgTelaRemota.Width);
   iY          := Trunc(Y * vResolucaoAltura)  Div Trunc(imgTelaRemota.Height);
  End
 Else
  Begin
   iX          := Round((X * 100) / Round(imgTelaRemota.Width));
   iY          := Round((Y * 100) / Round(imgTelaRemota.Height));
   iX          := ((Round(vResolucaoLargura) * iX) Div 100);
   iY          := ((Round(vResolucaoAltura)  * iY) Div 100);
  End;
 If (Button = TMouseButton.mbLeft)       Then
  SendSocketMouse(cMouseClickLeftUp   + IntToStr(iX) + '<|>' + IntToStr(iY) + cEndTag + Sblockinput)
 Else If (Button = TMouseButton.mbRight) Then
  SendSocketMouse(cMouseClickRightUp  + IntToStr(iX) + '<|>' + IntToStr(iY) + cEndTag + Sblockinput)
 Else
  SendSocketMouse(cMouseClickMiddleUp + IntToStr(iX) + '<|>' + IntToStr(iY) + cEndTag + Sblockinput);
End;

Procedure TFormTelaRemota.lyCloseDropDownClick(Sender : TObject);
Begin
 ToggleDropDown(True);
 lyCloseDropDown.Visible := False;
End;

Procedure TFormTelaRemota.MonitorFrameClick(Sender : TObject);
Begin
 LirMenuButton.Text := (Sender as TfrMonitorItem).LiPopupMonitor.Text;
 If Not(cShowForm) Then
  Begin
   If vLastMon <> (LirMenuButton.Text.ToInteger - 1).ToString then
    Begin
     vLastMon := (LirMenuButton.Text.ToInteger - 1).ToString;
     Conexao.SendMonitor(vActualIDConnected, cChangeMonitor + vActualIDConnected + '<|>' + vLastMon + cEndTag);
    End;
  End;
 ToggleDropDown;
End;

procedure TFormTelaRemota.SendSocketKeys(AKeys : String);
Var
 Sblockinput : String;
 aPackClass  : TPackClass;
Begin
 Sblockinput             := IfThen(vBlockInputs, cBlockInput, '');
 aPackClass              := TPackClass.Create;
 Try
  aPackClass.DataMode    := tdmClientCommand;
  aPackClass.DataCheck   := tdcAsync;
  aPackClass.CommandType := tctKeyboard;
  aPackClass.Command     := AKeys + Sblockinput;
  aPackClass.Dest        := vConnection;
  Conexao.SendBytes(vConnection, aPackClass.ToBytes, '', aPackClass.CommandType);
  Application.Processmessages;
 Finally
  FreeAndNil(aPackClass);
 End;
End;

procedure TFormTelaRemota.SendSocketMouse(AKeys : String);
Var
 Sblockinput : String;
 aPackClass  : TPackClass;
Begin
 Sblockinput             := IfThen(vBlockInputs, cBlockInput, '');
 aPackClass              := TPackClass.Create;
 Try
  aPackClass.DataMode    := tdmClientCommand;
  aPackClass.DataCheck   := tdcAsync;
  aPackClass.CommandType := tctMouse;
  aPackClass.Command     := AKeys + Sblockinput;
  aPackClass.Dest        := vConnection;
  Conexao.SendBytes(vConnection, aPackClass.ToBytes, '', aPackClass.CommandType);
  Application.Processmessages;
 Finally
  FreeAndNil(aPackClass);
 End;
End;

Procedure TFormTelaRemota.SetColors;
Begin
 rPopupMonitor.Fill.Color   := SECONDARY_COLOR;
 rrToolBarToggle.Fill.Color := SECONDARY_COLOR;
 rToolBar.Fill.Color        := SECONDARY_COLOR;
 rDDMonitor.Fill.Color      := SECONDARY_COLOR;
 rMenuButton.Fill.Color     := SECONDARY_COLOR;
End;

Procedure TFormTelaRemota.WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
Var
 MinMaxInfo : PMinMaxInfo;
Begin
 Inherited;
 MinMaxInfo := Message.MinMaxInfo;
 MinMaxInfo^.ptMinTrackSize.X := 800; // Minimum Width
 MinMaxInfo^.ptMinTrackSize.Y := 500; // Minimum Height
End;

End.
