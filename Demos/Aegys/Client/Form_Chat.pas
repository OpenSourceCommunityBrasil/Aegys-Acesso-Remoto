unit Form_Chat;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.Graphics, System.DateUtils, Vcl.ExtCtrls, AdvGDIPicture;

Const
  vWidthTab = 13;
  vWidthForm = 265;

type
  Tfrm_Chat = class(TForm)
    YourText_Edit: TEdit;
    Chat_RichEdit: TRichEdit;
    Image1: TImage;
    procedure YourText_EditKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure Image1Click(Sender: TObject);
  private
    { Private declarations }
    Closed: Boolean;
    Procedure WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
      message WM_GETMINMAXINFO;
    Procedure PositionTab;
    Procedure Wait(Value: Integer);
    Procedure MoveTab(Value: Boolean);
  public
    LastMessageAreYou: Boolean;
    FirstMessage: Boolean;
    Procedure ShowTab(Value: Boolean);
    { Public declarations }
  end;

var
  frm_Chat: Tfrm_Chat;

implementation

{$R *.dfm}

uses
  Form_Main;

procedure Tfrm_Chat.WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
{ sets Size-limits for the Form }
var
  MinMaxInfo: PMinMaxInfo;
begin
  inherited;
  MinMaxInfo := Message.MinMaxInfo;

  MinMaxInfo^.ptMinTrackSize.X := 230; // Minimum Width
  MinMaxInfo^.ptMinTrackSize.Y := 340; // Minimum Height
end;

procedure Tfrm_Chat.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  frm_Chat := Nil;
  Release;
end;

Procedure Tfrm_Chat.Wait(Value: Integer);
Var
  vActual: TDateTime;
  vTempWait: LongWord;
Begin
  vActual := Now;
  vTempWait := System.DateUtils.MilliSecondsBetween(Now, vActual);
  While (Value >= vTempWait) Do
  Begin
    vTempWait := System.DateUtils.MilliSecondsBetween(Now, vActual);
    sleep(1);
    Application.ProcessMessages;
  End;
End;

Procedure Tfrm_Chat.MoveTab(Value: Boolean);
Var
  I: Integer;
Begin
  If ((Value) And (Closed)) Then
  Begin
    For I := (Screen.Width - vWidthTab) DownTo (Screen.Width - vWidthForm) Do
    Begin
      Self.Left := I;
      // Application.ProcessMessages;
      // Wait(2);
    End;
    Closed := False;
  End
  Else If (Not(Value) And Not(Closed)) Then
  Begin
    For I := (Screen.Width - vWidthForm) To (Screen.Width - vWidthTab) Do
    Begin
      Self.Left := I;
      // Application.ProcessMessages;
    End;
    Closed := True;
  End;
End;

Procedure Tfrm_Chat.ShowTab(Value: Boolean);
Begin
  frm_Main.ExecMethod(
    Procedure
    Begin
      MoveTab(Value)
    End, True);
End;

Procedure Tfrm_Chat.PositionTab;
Begin
  Self.Width := vWidthForm;
  Self.Left := Screen.Width - vWidthTab;
  Self.Top := (Screen.Height div 2) - (Self.Height div 2);
  Closed := True;
End;

Procedure Tfrm_Chat.FormCreate(Sender: TObject);
begin
  // Separate Window
  SetWindowLong(Handle, GWL_EXSTYLE, WS_EX_APPWINDOW);

  FirstMessage := True;

  Left := Screen.WorkAreaWidth - Width;
  Top := Screen.WorkAreaHeight - Height;

  Chat_RichEdit.SelStart := Chat_RichEdit.GetTextLen;
  Chat_RichEdit.SelAttributes.Style := [fsBold];
  Chat_RichEdit.SelAttributes.Color := clWhite;
  Chat_RichEdit.SelText := 'Client Access Remote - Chat' + #13 + #13;
  PositionTab;
  Self.TransparentColor := True; // Ativa a transparencia
  Self.TransparentColorValue := clblack;
  // pode ser qualquer outra cor que você não vai usar em nenhum componente. Tudo que tiver essa cor ficará transparente.
  Self.Color := clblack; // O Form, estando dessa cor, ficará transparente.
  Self.BorderStyle := bsNone; // para que não apareça borda nenhuma
end;

procedure Tfrm_Chat.FormShow(Sender: TObject);
begin
  // Brush.Style := bsClear;
end;

procedure Tfrm_Chat.Image1Click(Sender: TObject);
begin
  ShowTab(Closed);
end;

procedure Tfrm_Chat.YourText_EditKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) then
  begin
    if (Length(YourText_Edit.Text) > 0) then
    begin
      FirstMessage := False;
      if not(LastMessageAreYou) then
      begin
        LastMessageAreYou := True;
        Chat_RichEdit.SelStart := Chat_RichEdit.GetTextLen;
        Chat_RichEdit.SelAttributes.Style := [fsBold];
        Chat_RichEdit.SelAttributes.Color := clYellow;
        Chat_RichEdit.SelText := #13 + #13 + 'Você disse:' + #13;
        Chat_RichEdit.SelStart := Chat_RichEdit.GetTextLen;
        Chat_RichEdit.SelAttributes.Color := clWhite;
        Chat_RichEdit.SelText := '   •   ' + YourText_Edit.Text;
      end
      else
      begin
        Chat_RichEdit.SelStart := Chat_RichEdit.GetTextLen;
        Chat_RichEdit.SelAttributes.Color := clWhite;
        Chat_RichEdit.SelText := #13 + '   •   ' + YourText_Edit.Text;
      end;

      frm_Main.ipPSMain_Socket.DataToSend('<|REDIRECT|><|CHAT|>' +
        YourText_Edit.Text + frm_Main.CommandEnd);
      YourText_Edit.Clear;

      SendMessage(Chat_RichEdit.Handle, WM_VSCROLL, SB_BOTTOM, 0);
    end;

    Key := #0;
  end;
end;

end.
