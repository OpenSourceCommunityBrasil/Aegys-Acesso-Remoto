unit uFormChat;

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
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls, FMX.Objects,
  FMX.Layouts, FMX.ListBox, Winapi.Messages, uFormConexao, uFunctions,
  System.Actions, FMX.ActnList, MMSystem, uConstants;

type
  TFormChat = class(TForm)
    Splitter1: TSplitter;
    Layout1: TLayout;
    lstMensagens: TListBox;
    Rectangle1: TRectangle;
    Rectangle8: TRectangle;
    mmMessage: TMemo;
    ActionList1: TActionList;
    actSendText: TAction;
    sbSendMessage: TSpeedButton;
    phSendMessage: TPath;
    Layout2: TLayout;
    procedure mmMessageKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure actSendTextExecute(Sender: TObject);
    procedure sbSendMessageClick(Sender: TObject);
  private
    aDest    : String;
    procedure WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
      message WM_GETMINMAXINFO;
    procedure ExtractSoundandPlay;
    procedure SetColors;
  public
    procedure Mensagem(AMensagem: string; AAtendente: Boolean = True);
  end;

var
  FormChat: TFormChat;

implementation

{$R *.fmx}

uses uDM, uFrameMensagemChat, Winapi.Windows;

procedure TFormChat.Mensagem(AMensagem: string; AAtendente: Boolean);
var
  ItemAdd: TListBoxItem;
  ARec: TMensagemRec;
  FItem: TFrameMensagemChat;
begin
  lstMensagens.BeginUpdate;

  ARec.Texto := AMensagem;
  ARec.Atendente := AAtendente;
  ItemAdd := TListBoxItem.Create(nil);
  FItem := TFrameMensagemChat.Create(ItemAdd);
  FItem.Parent := ItemAdd;
  FItem.Mensagem := ARec;
  ItemAdd.Height := FItem.Tamanho;
  FItem.Align := TAlignLayout.Client;
  FItem.ListBox := lstMensagens;
  lstMensagens.AddObject(ItemAdd);

  lstMensagens.EndUpdate;
  lstMensagens.ItemIndex := lstMensagens.Items.Count - 1;
  if (not Visible) or (not active) then
    ExtractSoundandPlay;
end;

procedure TFormChat.WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
var
  MinMaxInfo: PMinMaxInfo;
begin
  inherited;
  MinMaxInfo := Message.MinMaxInfo;
  MinMaxInfo^.ptMinTrackSize.X := 230; // Minimum Width
  MinMaxInfo^.ptMinTrackSize.Y := 340; // Minimum Height
end;

procedure TFormChat.actSendTextExecute(Sender: TObject);
begin
  if not mmMessage.Text.IsEmpty then
  begin
    Mensagem(mmMessage.Lines.Text, False);
    Conexao.SendMessage(aDest, '<|CHAT|>' + mmMessage.Lines.Text + '<|END|>');
    mmMessage.Lines.Clear;
    lstMensagens.ScrollToItem(lstMensagens.ListItems
      [lstMensagens.Items.Count - 1]);
  end;
end;

procedure TFormChat.ExtractSoundandPlay;
var
  resource: TResourceStream;
begin
  if not FileExists(ExtractFilePath(ParamStr(0)) + '\MessageBeep.wav') then
    try
      resource := TResourceStream.Create(HInstance, 'MESSAGE_BEEP', RT_RCDATA);
      try
        resource.SaveToFile(ExtractFilePath(ParamStr(0)) + '\MessageBeep.wav');
      finally
        FreeAndNil(resource);
      end;
      sndPlaySound(pwchar(ExtractFilePath(ParamStr(0)) + '\MessageBeep.wav'),
        SND_ASYNC);
    except
    end;
end;

procedure TFormChat.FormCreate(Sender: TObject);
begin
  // SetWindowLong(Handle, GWL_EXSTYLE, WS_EX_APPWINDOW);
  SetColors;
  FormChat.Top := Trunc(Screen.WorkAreaHeight - FormChat.Height);
  FormChat.Left := Trunc(Screen.WorkAreaWidth - FormChat.Width);
end;

procedure TFormChat.mmMessageKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  // if Key = vkReturn then
  // begin
  // actSendText.Execute;
  // Key := vkNone;
  // end;
end;

procedure TFormChat.sbSendMessageClick(Sender: TObject);
begin
  actSendText.Execute;
end;

procedure TFormChat.SetColors;
begin
  phSendMessage.Fill.Color := PRIMARY_COLOR;
end;

end.
