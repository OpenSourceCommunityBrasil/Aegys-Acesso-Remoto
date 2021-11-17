unit Form_RemoteScreen;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Types,
  System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.pngimage,
  Vcl.ExtCtrls,
  Vcl.StdCtrls, Vcl.Imaging.jpeg, System.Generics.Collections, uUteis,
  Vcl.Buttons, AdvGlowButton, AdvGDIPicture, System.DateUtils,
  Vcl.ComCtrls, TThreadTimer, IdBaseComponent, IdComponent, IdUDPBase,
  uUDPSuperComponents, IdUDPServer, JDRMGraphics;

const
  SC_MAXIMIZE2 = 61490;

Type
  TTagEffect = (te_Open, te_Close);
  TMessageType = (mt_Keyboard = 3050,
    // Mouse Click Down/Up
    mt_MouseLCD = 3051, mt_MouseLCU = 3052, mt_MouseMCD = 4051,
    mt_MouseMCU = 4052, mt_MouseRCD = 5051, mt_MouseRCU = 5052,
    // Mouse Double Click
    mt_MouseDBC = 6051,
    // Mouse Wheel
    mt_MouseWheel = 7050,
    // Mouse Move
    mt_MouseMove = 8050, mt_Clipborad = 9050);
  TExecuteProc = Reference to Procedure;

Type
  TResquest = Packed Record
    MessageType: TMessageType;
    MessageString: String;
    TimeRequest: TDateTime;
  End;

Type
  TListRequest = TList<TResquest>;

Type
  TTypeCommandKey = (tck_Down, tck_Up);

type
  Tfrm_RemoteScreen = class(TForm)
    ScrollBox1: TScrollBox;
    iDesktopCapture: TAdvGDIPPicture;
    btn_ac: TAdvGlowButton;
    btn_aclose: TAdvGlowButton;
    DesktopViewCapture: TJDRMDesktopView;
    Menu_Panel: TPanel;
    Panel15: TPanel;
    Panel10: TPanel;
    Panel12: TPanel;
    Panel20: TPanel;
    MouseIcon_Image: TAdvGlowButton;
    KeyboardIcon_Image: TAdvGlowButton;
    Chat_Image: TAdvGlowButton;
    FileShared_Image: TAdvGlowButton;
    sbMinimize: TAdvGlowButton;
    sbRestaure: TAdvGlowButton;
    sbMiximize: TAdvGlowButton;
    btn_close: TAdvGlowButton;
    rzpDownload: TPanel;
    advCancel: TAdvGlowButton;
    pbDados: TProgressBar;
    procedure Resize_CheckBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure KeyboardRemote_CheckBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SendSocketKeys(Keys: string);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormResize(Sender: TObject);
    procedure Label1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btn_closeClick(Sender: TObject);
    procedure sbMiximizeClick(Sender: TObject);
    procedure sbRestaureClick(Sender: TObject);
    procedure sbMinimizeClick(Sender: TObject);
    procedure Chat_ImageClick(Sender: TObject);
    procedure ResizeIcon_ImageClick(Sender: TObject);
    procedure FileShared_ImageClick(Sender: TObject);
    Procedure KeyState;
    Procedure KeyStateD(Key: Word; TypeCommand: TTypeCommandKey);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure iDesktopCaptureDblClick(Sender: TObject);
    procedure iDesktopCaptureMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure iDesktopCaptureMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btn_acClick(Sender: TObject);
    procedure btn_acloseClick(Sender: TObject);
    procedure iDesktopCaptureMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure advCancelClick(Sender: TObject);
    procedure DesktopViewCaptureDblClick(Sender: TObject);
    procedure DesktopViewCaptureMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DesktopViewCaptureMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure DesktopViewCaptureMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure KeyboardIcon_ImageClick(Sender: TObject);
    procedure MouseIcon_ImageClick(Sender: TObject);
  private
    { Private declarations }
    ListRequest: TListRequest;
    MenuDirection: TTagEffect;
    // vLinhaKeys    : String;
    Procedure MoveButtons;
    Procedure Wait(Value: Integer);
    Procedure RollMenu(Value: TTagEffect = te_Open);
    Procedure SendEvents;
    procedure WMSysCommand(var Message: TWMSysCommand); message WM_SYSCOMMAND;
    procedure WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
      message WM_GETMINMAXINFO;
    Procedure ShowPanel(Value: Boolean = False);
    Procedure MovimentaObject(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Formulario: TForm);
  public
    { Public declarations }
    CtrlPressed, ShiftPressed, AltPressed: Boolean;
    procedure StopTransfer;
    Procedure SendEventsTimer;
  end;

var
  frm_RemoteScreen: Tfrm_RemoteScreen;
  LastX: Integer = 0;
  LastY: Integer = 0;
  vX, vY: Integer;
  vInClose: Boolean = False;
  vLinhaKeys: String = '';
  vSendEvents, vSendMouse: TDateTime;

implementation

{$R *.dfm}

uses
  Form_Main, Form_Chat, Form_ShareFiles;

Procedure Tfrm_RemoteScreen.SendEventsTimer;
Begin
  If (MilliSecondsBetween(Now, vSendEvents) >= TSendEvents) Then
  Begin
    vSendEvents := Now;
    Try
      If Self <> Nil Then
      Begin
        If (Active) And (MouseIcon_Image.Down) Then
        Begin
          If Not vInClose then

            SendEvents;

        End;
      End
      Else
      Except
      End;
    End;
  End;

  procedure Tfrm_RemoteScreen.MouseIcon_ImageClick(Sender: TObject);
  begin
    //
  end;

  Procedure Tfrm_RemoteScreen.MoveButtons;
  Begin
    btn_ac.Top := Menu_Panel.Top + Menu_Panel.Height;
    btn_ac.Left := Menu_Panel.Left + Menu_Panel.Width - btn_ac.Width;
    //
    btn_aclose.Top := btn_ac.Top;
    btn_aclose.Left := btn_ac.Left;
  End;

  Procedure Tfrm_RemoteScreen.Wait(Value: Integer);
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

  Procedure Tfrm_RemoteScreen.RollMenu(Value: TTagEffect = te_Open);
  Const
    TimerEvent = 10;
    ScrollSync = 3;
  Var
    vBarDec, I: Integer;
    Procedure ExecMethod(Execute: TExecuteProc = Nil);
    Begin
      TThread.CreateAnonymousThread(
        Procedure
        Begin
          // Se precisar interagir com a Thread da Interface
          If Assigned(Execute) Then
            TThread.Synchronize(TThread.CurrentThread,
              Procedure
              Begin
                Execute;
              End);
        End).Start;
    End;

  Begin
    Menu_Panel.Left := (Self.Width div 2) - (Menu_Panel.Width div 2);
    MoveButtons;
    rzpDownload.Top := Menu_Panel.Top;
    rzpDownload.Left := Menu_Panel.Left - rzpDownload.Width;
    btn_ac.Visible := Value = te_Close;
    btn_aclose.Visible := Not btn_ac.Visible;
    Case Value Of
      te_Open:
        Begin
          {
            While Menu_Panel.Top < 0 Do
            Begin
            Menu_Panel.Top := Menu_Panel.Top + ScrollSync;
            MoveButtons;
            Wait(TimerEvent);
            //Sleep(TimerEvent);
            End;
          }
          Menu_Panel.Top := 0;
          rzpDownload.Top := Menu_Panel.Top;
          MoveButtons;
          If frm_Main.MouseCapture Then
            iDesktopCapture.Cursor := crDefault;
        End;
      te_Close:
        Begin
          {
            While Menu_Panel.Top > (Menu_Panel.Height * -1) Do
            Begin
            Menu_Panel.Top := Menu_Panel.Top - ScrollSync;
            MoveButtons;
            Wait(TimerEvent);
            //Sleep(TimerEvent);
            End;
          }
          Menu_Panel.Top := Menu_Panel.Height * -1;
          rzpDownload.Top := Menu_Panel.Top;
          MoveButtons;
          If frm_Main.MouseCapture Then
            iDesktopCapture.Cursor := crNone;
        End;
    End;
  End;

  procedure Tfrm_RemoteScreen.WMSysCommand(var Message: TWMSysCommand);
  begin
    Case Message.CmdType Of
      SC_MAXIMIZE, SC_MAXIMIZE2:
        Self.BorderStyle := bsNone;
      SC_MINIMIZE, SC_RESTORE:
        Self.BorderStyle := bsSizeable;
    End;
    inherited;
  end;

  procedure Tfrm_RemoteScreen.WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
  { sets Size-limits for the Form }
  var
    MinMaxInfo: PMinMaxInfo;
  begin
    inherited;
    MinMaxInfo := Message.MinMaxInfo;
    MinMaxInfo^.ptMinTrackSize.X := 640; // Minimum Width
    MinMaxInfo^.ptMinTrackSize.Y := 480; // Minimum Height
    {
      if (ResizeIcon_Image.Down) then
      begin
      MinMaxInfo^.ptMaxTrackSize.X := frm_Main.ResolutionTargetWidth;
      MinMaxInfo^.ptMaxTrackSize.Y := frm_Main.ResolutionTargetHeight;
      end
      else
      begin
    }
    MinMaxInfo^.ptMaxTrackSize.X := frm_Main.ResolutionTargetWidth + 20;
    MinMaxInfo^.ptMaxTrackSize.Y := frm_Main.ResolutionTargetHeight + 120;
    // end;
  end;

  procedure Tfrm_RemoteScreen.SendSocketKeys(Keys: string);
  Var
    Resquest: TResquest;
  begin
    If (Active) Then
    Begin
      Resquest.MessageString := Keys + frm_Main.CommandEnd;
      Resquest.MessageType := mt_Keyboard;
      Resquest.TimeRequest := Now;
      ListRequest.Add(Resquest);
    End;
  end;

  Procedure Tfrm_RemoteScreen.SendEvents;
  Var
    I: Integer;
    vList: TList<TResquest>;
    PeerConnected: TPeerConnected;
    vDataSend: String;
  Begin
    If (Self <> Nil) And (Active) And (Not OnSendComands) Then
    Begin
      OnSendComands := True;
      vList := ListRequest;
      Try
        If frm_Main.ipCommandsClient = Nil Then
          Exit;
        If PeerConnected <> Nil Then
        Begin
          vDataSend := '';
          I := 0;
          While I <= vList.Count - 1 Do
          Begin
            vDataSend := vDataSend + Format('%s%s%s%d%s',
              [tInitCom, vList[I].MessageString, tDataSep,
              Word(vList[I].MessageType), tFinalCom]);
            Try
              System.TMonitor.Enter(ListRequest);
              vList.Delete(I);
            Finally
              System.TMonitor.PulseAll(ListRequest);
              System.TMonitor.Exit(ListRequest);
            End;
          End;
          If (vDataSend = '') And
            (MilliSecondsBetween(Now, vSendMouse) >= TSendMouseNone) Then
          Begin
            vDataSend := TNoneData;
            vSendMouse := Now;
          End
          Else If (vDataSend <> '') Then
            vSendMouse := Now;
          If (vDataSend <> '') Then
          Begin
            PeerConnected := frm_Main.ipCommandsClient.GetActivePeer;
            If PeerConnected <> Nil Then
              frm_Main.ipCommandsClient.SendBuffer
                (frm_Main.ipCommandsClient.GetIpSend(PeerConnected),
                PeerConnected.Port, vDataSend, False, dtt_Async);
          End;
{$IFDEF MSWINDOWS}
{$IFNDEF FMX}Application.Processmessages;
{$ELSE}FMX.Forms.TApplication.Processmessages; {$ENDIF}
{$ENDIF}
        End;
      Except
      End;
      OnSendComands := False;
    End;
  End;

  procedure Tfrm_RemoteScreen.StopTransfer;
  Begin
    frm_Main.LinhaKeys := '<|STOPTRANSFER|>' + frm_Main.CommandEnd;
    frm_Main.StopSendFile := True;
    frm_Main.CancelOPSendFile := True;
    If frm_ShareFiles <> Nil Then
    Begin
      // frm_ShareFiles.Directory_Edit.Enabled        := True;
      frm_ShareFiles.Upload_ProgressBar.Max := 0;
      frm_ShareFiles.Upload_ProgressBar.Position := 0;
      frm_ShareFiles.Download_ProgressBar.Max := 0;
      frm_ShareFiles.Download_ProgressBar.Position := 0;
      frm_ShareFiles.SizeUpload_Label.Caption := 'Tam: ' + frm_Main.GetSize(0) +
        ' / ' + frm_Main.GetSize(0);
    End;
    If frm_RemoteScreen <> Nil then
    Begin
      frm_RemoteScreen.rzpDownload.Visible := False;
      If frm_ShareFiles <> Nil Then
        frm_ShareFiles.FreeForClose :=
          Not(frm_RemoteScreen.rzpDownload.Visible);
      frm_RemoteScreen.pbDados.Max := 0;
      frm_RemoteScreen.pbDados.Position := 0;
    End;
    frm_Main.SendingFile := False;
    frm_Main.ipCommandsClient.SendBuffer('<|GETFOLDERS|>' +
      frm_ShareFiles.Directory_Edit + frm_Main.CommandEnd);
    If frm_ShareFiles <> Nil Then
    Begin
      frm_ShareFiles.Upload_BitBtn.Enabled := True;
      frm_ShareFiles.Download_BitBtn.Enabled := True;
    End;
    FileShared_Image.OnClick(FileShared_Image);
  End;

  procedure Tfrm_RemoteScreen.advCancelClick(Sender: TObject);
  begin
    If (Active) Then
      StopTransfer;
  end;

  procedure Tfrm_RemoteScreen.btn_acClick(Sender: TObject);
  begin
    MenuDirection := te_Open;
    RollMenu(MenuDirection);
  end;

  procedure Tfrm_RemoteScreen.btn_acloseClick(Sender: TObject);
  begin
    MenuDirection := te_Close;
    RollMenu(MenuDirection);
  end;

  procedure Tfrm_RemoteScreen.btn_closeClick(Sender: TObject);
  begin
    vInClose := True;
    If (frm_Main <> Nil) Then
      ShowApplication;
    If frm_RemoteScreen <> Nil Then
      Close;
    btn_close.Enabled := False;
    MouseIcon_Image.Down := False;
    KeyboardIcon_Image.Down := MouseIcon_Image.Down;
    frm_Main.SetOffline;
    frm_Main.CloseSockets;
  end;

  procedure Tfrm_RemoteScreen.sbMiximizeClick(Sender: TObject);
  begin
    PostMessage(Self.Handle, WM_SYSCOMMAND, SC_MAXIMIZE, 0);
  end;

  procedure Tfrm_RemoteScreen.sbMinimizeClick(Sender: TObject);
  begin
    PostMessage(Self.Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
  end;

  procedure Tfrm_RemoteScreen.sbRestaureClick(Sender: TObject);
  begin
    PostMessage(Self.Handle, WM_SYSCOMMAND, SC_RESTORE, 0);
  end;

  Procedure Tfrm_RemoteScreen.KeyStateD(Key: Word;
  TypeCommand: TTypeCommandKey);
  begin
    if vInClose then
      Exit;
    // The keys programmed here, may not match the keys on your keyboard. I recommend to undertake adaptation.
    Try
      { Combo }
      If KeyboardIcon_Image.Down Then
      Begin
        // Alt
        if (VK_MENU = Key) And (TypeCommand = tck_Down) then
        begin
          If (Not(AltPressed)) Then
          Begin
            AltPressed := True;
            SendSocketKeys('<|ALTDOWN|>');
          End
          Else
            AltPressed := False;
        end
        else if (VK_MENU = Key) And (TypeCommand = tck_Up) then
        begin
          if (AltPressed) then
          Begin
            AltPressed := False;
            SendSocketKeys('<|ALTUP|>');
          End
          Else
            AltPressed := False;
        end;
        // Ctrl
        if (VK_CONTROL = Key) And (TypeCommand = tck_Down) then
        begin
          If (Not(CtrlPressed)) Then
          Begin
            CtrlPressed := True;
            SendSocketKeys('<|CTRLDOWN|>');
          End
          Else
            CtrlPressed := False;
        end
        Else if (VK_CONTROL = Key) And (TypeCommand = tck_Up) then
        begin
          If (CtrlPressed) Then
          Begin
            CtrlPressed := False;
            SendSocketKeys('<|CTRLUP|>');
          End
          Else
            CtrlPressed := False;
        end;

        // Shift
        if (VK_SHIFT = Key) And (TypeCommand = tck_Down) then
        begin
          If (Not(ShiftPressed)) Then
          Begin
            ShiftPressed := True;
            SendSocketKeys('<|SHIFTDOWN|>');
          End
          Else
            ShiftPressed := False;
        end
        else if (VK_SHIFT = Key) And (TypeCommand = tck_Up) then
        begin
          If (ShiftPressed) Then
          Begin
            ShiftPressed := False;
            SendSocketKeys('<|SHIFTUP|>');
          End
          Else
            ShiftPressed := False;
        end;
      end;
      If (TypeCommand = tck_Down) Then
        Case Key of
          8:
            SendSocketKeys('{BS}');
          9:
            SendSocketKeys('{TAB}');
          13:
            SendSocketKeys('{ENTER}');
          27:
            SendSocketKeys('{ESCAPE}');
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
            Begin
              if (GetKeyState(VK_SHIFT) < 0) then
                SendSocketKeys('(')
              else
                SendSocketKeys('9');
            End;
          65 .. 90: // A..Z / a..z
            Begin
              If (TypeCommand = tck_Down) Then
              Begin
                If (GetKeyState(VK_CAPITAL) = 1) Then
                Begin
                  If (GetKeyState(VK_SHIFT) < 0) Then
                    SendSocketKeys(LowerCase(Chr(Key)))
                  Else
                    SendSocketKeys(UpperCase(Chr(Key)));
                End
                Else if (GetKeyState(VK_SHIFT) < 0) then
                  SendSocketKeys(UpperCase(Chr(Key)))
                Else
                  SendSocketKeys(LowerCase(Chr(Key)));
              End;
            End;
          96 .. 105: // Numpad 1..9
            SendSocketKeys(IntToStr(Key - 96));
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
            SendSocketKeys('{F' + IntToStr(Key - 111) + '}');
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
              SendSocketKeys('{') // Retirar // da Chaves
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
    except
    end;
  end;

  Procedure Tfrm_RemoteScreen.KeyState;
  var
    I: Byte;
  begin
    if vInClose then
      Exit;
    // The keys programmed here, may not match the keys on your keyboard. I recommend to undertake adaptation.
    Try
      { Combo }
      If KeyboardIcon_Image.Down Then
      Begin
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
              SendSocketKeys('{ESCAPE}');
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

  procedure Tfrm_RemoteScreen.Chat_ImageClick(Sender: TObject);
  begin
    if frm_Chat = Nil then
      frm_Chat := Tfrm_Chat.Create(Application);
    frm_Chat.Show;
    frm_Chat.ShowTab(True);
  end;

  procedure Tfrm_RemoteScreen.DesktopViewCaptureDblClick(Sender: TObject);
  Var
    vNewX, vNewY: Integer;
    Resquest: TResquest;
  begin
    If (Active) And (MouseIcon_Image.Down) Then
    Begin
      vNewX := frm_Main.ResolutionTargetWidth *
        Round(LastX * 100 / DesktopViewCapture.Width) div 100;
      // (X * frm_Main.ResolutionTargetWidth) div (Screen_Image.Width);
      vNewY := frm_Main.ResolutionTargetHeight *
        Round(LastY * 100 / DesktopViewCapture.Height) div 100;
      // * frm_Main.ResolutionTargetHeight div 100; //Y - (frm_Main.ResolutionTargetHeight - Screen_Image.Height);//* frm_Main.ResolutionTargetHeight) div (Screen_Image.Height);
      Resquest.MessageType := mt_MouseDBC;
      Resquest.MessageString := IntToStr(vNewX) + '<|>' + IntToStr(vNewY) +
        frm_Main.CommandEnd;
      Resquest.TimeRequest := Now;
      ListRequest.Add(Resquest);
      // If Not vInClose then
      // SendEvents;
    End;
  end;

  procedure Tfrm_RemoteScreen.DesktopViewCaptureMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  Var
    vNewX, vNewY: Integer;
    Resquest: TResquest;
  begin
    If (MouseIcon_Image.Down) Then
    Begin
      vNewX := frm_Main.ResolutionTargetWidth *
        Round(LastX * 100 / DesktopViewCapture.Width) div 100;
      // (X * frm_Main.ResolutionTargetWidth) div (Screen_Image.Width);
      vNewY := frm_Main.ResolutionTargetHeight *
        Round(LastY * 100 / DesktopViewCapture.Height) div 100;
      // * frm_Main.ResolutionTargetHeight div 100; //Y - (frm_Main.ResolutionTargetHeight - Screen_Image.Height);//* frm_Main.ResolutionTargetHeight) div (Screen_Image.Height);
      Resquest.TimeRequest := Now;
      If (Button = mbLeft) Then
        Resquest.MessageType := mt_MouseLCD
      Else If (Button = mbRight) Then
        Resquest.MessageType := mt_MouseRCD
      Else
        Resquest.MessageType := mt_MouseMCD;
      Resquest.MessageString := IntToStr(vNewX) + '<|>' + IntToStr(vNewY) +
        frm_Main.CommandEnd;
      ListRequest.Add(Resquest);
    End;
  end;

  procedure Tfrm_RemoteScreen.DesktopViewCaptureMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
  Var
    vNewX, vNewY: Integer;
    Resquest: TResquest;
  begin
    vX := X;
    vY := Y;
    If (Active) And (MouseIcon_Image.Down) Then
    Begin
      If (LastX <> vX) or (LastY <> vY) Then
      Begin
        LastX := vX;
        LastY := vY;
        vNewX := frm_Main.ResolutionTargetWidth *
          Round(vX * 100 / DesktopViewCapture.Width) div 100;
        // (X * frm_Main.ResolutionTargetWidth) div (Screen_Image.Width);
        vNewY := frm_Main.ResolutionTargetHeight *
          Round(vY * 100 / DesktopViewCapture.Height) div 100;
        // * frm_Main.ResolutionTargetHeight div 100; //Y - (frm_Main.ResolutionTargetHeight - Screen_Image.Height);//* frm_Main.ResolutionTargetHeight) div (Screen_Image.Height);
        Resquest.TimeRequest := Now;
        Resquest.MessageType := mt_MouseMove;
        Resquest.MessageString := IntToStr(vNewX) + '<|>' + IntToStr(vNewY) +
          frm_Main.CommandEnd;
        ListRequest.Add(Resquest);
      End;
    End;
    {
      If (Active) And (MouseIcon_Image.Down) Then
      Begin
      If Not vInClose then

      SendEvents;
      End;
    }
  end;

  procedure Tfrm_RemoteScreen.DesktopViewCaptureMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  Var
    vNewX, vNewY: Integer;
    Resquest: TResquest;
  Begin
    If (Active) and (MouseIcon_Image.Down) Then
    Begin
      vNewX := frm_Main.ResolutionTargetWidth *
        Round(LastX * 100 / DesktopViewCapture.Width) div 100;
      // (X * frm_Main.ResolutionTargetWidth) div (Screen_Image.Width);
      vNewY := frm_Main.ResolutionTargetHeight *
        Round(LastY * 100 / DesktopViewCapture.Height) div 100;
      // * frm_Main.ResolutionTargetHeight div 100; //Y - (frm_Main.ResolutionTargetHeight - Screen_Image.Height);//* frm_Main.ResolutionTargetHeight) div (Screen_Image.Height);
      Resquest.TimeRequest := Now;
      If (Button = mbLeft) Then
        Resquest.MessageType := mt_MouseLCU
      Else if (Button = mbRight) then
        Resquest.MessageType := mt_MouseRCU
      Else
        Resquest.MessageType := mt_MouseMCU;
      Resquest.MessageString := IntToStr(vNewX) + '<|>' + IntToStr(vNewY) +
        frm_Main.CommandEnd;
      ListRequest.Add(Resquest);
    End;
  End;

  procedure Tfrm_RemoteScreen.FileShared_ImageClick(Sender: TObject);
  begin
    If frm_ShareFiles = Nil Then
      frm_ShareFiles := Tfrm_ShareFiles.Create(Application);
    frm_ShareFiles.Show;
  end;

  procedure Tfrm_RemoteScreen.FormClose(Sender: TObject;
  var Action: TCloseAction);
  begin
    vInClose := True;
    ListRequest.DisposeOf;
    frm_RemoteScreen := Nil;
    Release;

  end;

  procedure Tfrm_RemoteScreen.FormCreate(Sender: TObject);
  begin
    // Separate Window
    LastX := 0;
    LastY := 0;
    vX := 0;
    vY := 0;
    CtrlPressed := False;
    ShiftPressed := False;
    AltPressed := False;
    SetWindowLong(Handle, GWL_EXSTYLE, WS_EX_APPWINDOW);
    ListRequest := TListRequest.Create;
    vInClose := False;
    vLinhaKeys := '';
    vSendMouse := Now;
    vSendEvents := Now;
    OnSendComands := False;
    iDesktopCapture.DoubleBuffered := True;
    If frm_Main.SendType = stNAT Then
      Self.Caption := 'Aegys - Computador Remoto - P2P'
    Else
      Self.Caption := 'Aegys - Computador Remoto - NAT';
    // tShowCursor.Enabled := True;
  end;

  procedure Tfrm_RemoteScreen.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  begin
    // If Not (Key in [65..90, 186..193, 195..226]) Then // A..Z / a..z
    If (Active) And (KeyboardIcon_Image.Down) And (frm_Main.InitCapture) Then
    Begin
      If Not vInClose then
        KeyStateD(Key, tck_Down);
    End;
  end;

  procedure Tfrm_RemoteScreen.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  begin
    // If Not (Key in [65..90, 186..193, 195..226]) Then // A..Z / a..z
    If (Active) And (KeyboardIcon_Image.Down) And (frm_Main.InitCapture) Then
    Begin
      If Not vInClose then
        KeyStateD(Key, tck_Up);
    End;
  end;

  Procedure Tfrm_RemoteScreen.FormMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
  Var
    vNewX, vNewY: Integer;
    Resquest: TResquest;
  begin
    If (Active) And (MouseIcon_Image.Down) And (frm_Main.InitCapture) Then
    Begin
      vNewX := frm_Main.ResolutionTargetWidth *
        Round(LastX * 100 / iDesktopCapture.Width) div 100;
      // (X * frm_Main.ResolutionTargetWidth) div (Screen_Image.Width);
      vNewY := frm_Main.ResolutionTargetHeight *
        Round(LastY * 100 / iDesktopCapture.Height) div 100;
      // * frm_Main.ResolutionTargetHeight div 100; //Y - (frm_Main.ResolutionTargetHeight - Screen_Image.Height);//* frm_Main.ResolutionTargetHeight) div (Screen_Image.Height);
      Resquest.MessageType := mt_MouseWheel;
      Resquest.MessageString := IntToStr(WheelDelta) + '<|>' + IntToStr(vNewX) +
        '<|>' + IntToStr(vNewY) + frm_Main.CommandEnd;
      ListRequest.Add(Resquest);
    end;
  end;

  procedure Tfrm_RemoteScreen.FormResize(Sender: TObject);
  begin
    Menu_Panel.Left := (Self.Width div 2) - (Menu_Panel.Width div 2);
    RollMenu(MenuDirection);
    rzpDownload.Top := Menu_Panel.Top;
    rzpDownload.Left := Menu_Panel.Left - rzpDownload.Width;
  end;

  procedure Tfrm_RemoteScreen.FormShow(Sender: TObject);
  begin
    CtrlPressed := False;
    ShiftPressed := False;
    AltPressed := False;
    RollMenu(MenuDirection);
  end;

  procedure Tfrm_RemoteScreen.iDesktopCaptureDblClick(Sender: TObject);
  Var
    vNewX, vNewY: Integer;
    Resquest: TResquest;
  begin
    If (Active) And (MouseIcon_Image.Down) And (frm_Main.InitCapture) Then
    Begin
      vNewX := frm_Main.ResolutionTargetWidth *
        Round(LastX * 100 / iDesktopCapture.Width) div 100;
      // (X * frm_Main.ResolutionTargetWidth) div (Screen_Image.Width);
      vNewY := frm_Main.ResolutionTargetHeight *
        Round(LastY * 100 / iDesktopCapture.Height) div 100;
      // * frm_Main.ResolutionTargetHeight div 100; //Y - (frm_Main.ResolutionTargetHeight - Screen_Image.Height);//* frm_Main.ResolutionTargetHeight) div (Screen_Image.Height);
      Resquest.MessageType := mt_MouseDBC;
      Resquest.MessageString := IntToStr(vNewX) + '<|>' + IntToStr(vNewY) +
        frm_Main.CommandEnd;
      Resquest.TimeRequest := Now;
      ListRequest.Add(Resquest);
      // If Not vInClose then
      // SendEvents;
    End;
  end;

  procedure Tfrm_RemoteScreen.iDesktopCaptureMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  Var
    vNewX, vNewY: Integer;
    Resquest: TResquest;
  begin
    If (MouseIcon_Image.Down) And (frm_Main.InitCapture) Then
    Begin
      vNewX := frm_Main.ResolutionTargetWidth *
        Round(LastX * 100 / iDesktopCapture.Width) div 100;
      // (X * frm_Main.ResolutionTargetWidth) div (Screen_Image.Width);
      vNewY := frm_Main.ResolutionTargetHeight *
        Round(LastY * 100 / iDesktopCapture.Height) div 100;
      // * frm_Main.ResolutionTargetHeight div 100; //Y - (frm_Main.ResolutionTargetHeight - Screen_Image.Height);//* frm_Main.ResolutionTargetHeight) div (Screen_Image.Height);
      Resquest.TimeRequest := Now;
      If (Button = mbLeft) Then
        Resquest.MessageType := mt_MouseLCD
      Else If (Button = mbRight) Then
        Resquest.MessageType := mt_MouseRCD
      Else
        Resquest.MessageType := mt_MouseMCD;
      Resquest.MessageString := IntToStr(vNewX) + '<|>' + IntToStr(vNewY) +
        frm_Main.CommandEnd;
      ListRequest.Add(Resquest);
      // If Not vInClose then
      // SendEvents;
    End;
  end;

  procedure Tfrm_RemoteScreen.iDesktopCaptureMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
  Var
    vNewX, vNewY: Integer;
    Resquest: TResquest;
  begin
    vX := X;
    vY := Y;
    If (Active) And (MouseIcon_Image.Down) And (frm_Main.InitCapture) Then
    Begin
      If (LastX <> vX) or (LastY <> vY) Then
      Begin
        LastX := vX;
        LastY := vY;
        vNewX := frm_Main.ResolutionTargetWidth *
          Round(vX * 100 / iDesktopCapture.Width) div 100;
        // (X * frm_Main.ResolutionTargetWidth) div (Screen_Image.Width);
        vNewY := frm_Main.ResolutionTargetHeight *
          Round(vY * 100 / iDesktopCapture.Height) div 100;
        // * frm_Main.ResolutionTargetHeight div 100; //Y - (frm_Main.ResolutionTargetHeight - Screen_Image.Height);//* frm_Main.ResolutionTargetHeight) div (Screen_Image.Height);
        Resquest.TimeRequest := Now;
        Resquest.MessageType := mt_MouseMove;
        Resquest.MessageString := IntToStr(vNewX) + '<|>' + IntToStr(vNewY) +
          frm_Main.CommandEnd;
        ListRequest.Add(Resquest);
      End;
    End;
    {
      If (Active) And (MouseIcon_Image.Down) Then
      Begin
      If Not vInClose then

      SendEvents;
      End;
    }
  end;

  procedure Tfrm_RemoteScreen.iDesktopCaptureMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  Var
    vNewX, vNewY: Integer;
    Resquest: TResquest;
  Begin
    If (Active) and (MouseIcon_Image.Down) And (frm_Main.InitCapture) Then
    Begin
      vNewX := frm_Main.ResolutionTargetWidth *
        Round(LastX * 100 / iDesktopCapture.Width) div 100;
      // (X * frm_Main.ResolutionTargetWidth) div (Screen_Image.Width);
      vNewY := frm_Main.ResolutionTargetHeight *
        Round(LastY * 100 / iDesktopCapture.Height) div 100;
      // * frm_Main.ResolutionTargetHeight div 100; //Y - (frm_Main.ResolutionTargetHeight - Screen_Image.Height);//* frm_Main.ResolutionTargetHeight) div (Screen_Image.Height);
      Resquest.TimeRequest := Now;
      If (Button = mbLeft) Then
        Resquest.MessageType := mt_MouseLCU
      Else if (Button = mbRight) then
        Resquest.MessageType := mt_MouseRCU
      Else
        Resquest.MessageType := mt_MouseMCU;
      Resquest.MessageString := IntToStr(vNewX) + '<|>' + IntToStr(vNewY) +
        frm_Main.CommandEnd;
      ListRequest.Add(Resquest);
    End;
  End;

  procedure Tfrm_RemoteScreen.KeyboardIcon_ImageClick(Sender: TObject);
  begin
    //
  end;

  procedure Tfrm_RemoteScreen.KeyboardRemote_CheckBoxKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
  begin
    if Key = VK_SPACE then
      Key := 0;
  end;

  Procedure Tfrm_RemoteScreen.Label1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
    // MovimentaObject(sender, button, shift, Menu_Panel.Left + x, Menu_Panel.Top + y, frm_RemoteScreen);
  end;

  procedure Tfrm_RemoteScreen.MovimentaObject(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Formulario: TForm);
  Var
    ObjectPos, MousePosMov, Pt: TPoint;
    fHandle: HWND;
  Begin
    GetCursorPos(Pt);
    ObjectPos.X := Formulario.Left;
    ObjectPos.Y := Formulario.Top;
    If (Sender is TForm) then
      fHandle := TWinControl(Sender).Handle
    Else
      fHandle := TWinControl(Sender).Parent.Handle;
    While DragDetect(fHandle, ObjectPos) Do
    Begin
      GetCursorPos(MousePosMov);
      Formulario.Left := MousePosMov.X - X - 3;
      Formulario.Top := MousePosMov.Y - Y - 3;
      // Application.ProcessMessages;
    End;
  End;

  procedure Tfrm_RemoteScreen.ResizeIcon_ImageClick(Sender: TObject);
  begin
    {
      if (ResizeIcon_Image.Down) then
      begin
      iDesktopCapture.AutoSize := false;
      iDesktopCapture.Stretch := true;
      iDesktopCapture.Align := alClient;
      //ResizeIcon_Image.Picture.Assign(ResizeIcon_checked_Image.Picture);
      end
      else
      begin
      iDesktopCapture.AutoSize := true;
      iDesktopCapture.Stretch := false;
      iDesktopCapture.Align := alNone;
      If iDesktopCapture.Align = alNone Then
      Begin
      frm_RemoteScreen.Width  := iDesktopCapture.Width;
      frm_RemoteScreen.Height := iDesktopCapture.Height;
      End;
      //ResizeIcon_Image.Picture.Assign(ResizeIcon_unchecked_Image.Picture);
      end;
    }
  end;

  procedure Tfrm_RemoteScreen.Resize_CheckBoxKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
  begin
    if Key = VK_SPACE then
      Key := 0;
  end;

  Procedure Tfrm_RemoteScreen.ShowPanel(Value: Boolean = False);
  Var
    I: Integer;
  Begin
    If Value Then
    Begin
      For I := Menu_Panel.Top to 0 Do
      Begin
        Menu_Panel.Top := I;
        rzpDownload.Top := Menu_Panel.Top;
        Application.Processmessages;
      End;
    End
    Else
    Begin
      For I := 0 Downto Menu_Panel.Height * -1 Do
      Begin
        Menu_Panel.Top := I;
        rzpDownload.Top := Menu_Panel.Top;
        Application.Processmessages;
      End;
    End;
  End;

End.
