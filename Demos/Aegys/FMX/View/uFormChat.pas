unit uFormChat;

{
 Projeto Aegys.

  Criado por Gilberto Rocha da Silva em 05/04/2017 baseado no projeto Allakore, tem por objetivo promover acesso remoto e outros
 de forma gratuita a todos que necessitarem, hoje mantido por uma bela comunidade listando aqui nossos colaboradores de grande estima.

  Gilberto Rocha da Silva(XyberX) (Creator of Aegys Project/Main Desenveloper/Admin).
  Wendel Rodrigues Fassarella(wendelfassarella) (Creator of Aegys FMX/CORE Desenveloper).
  Rai Duarte Jales(Raí Duarte) (Aegys Server Desenveloper).
  Roniery Santos Cardoso (Aegys Desenveloper).
  Alexandre Carlos Silva Abade (Aegys Desenveloper).
}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls, FMX.Objects,
  FMX.Layouts, FMX.ListBox, Winapi.Messages, uFormConexao;

type
  TFormChat = class(TForm)
    Splitter1: TSplitter;
    Layout1: TLayout;
    lstMensagens: TListBox;
    Rectangle1: TRectangle;
    Rectangle8: TRectangle;
    txtMensagem: TMemo;
    procedure txtMensagemKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  private
    procedure WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
  public
    procedure Mensagem(AMensagem: string; AAtendente: Boolean = True);
  end;

var
  FormChat: TFormChat;

implementation

{$R *.fmx}


uses uDM_Styles, uFrameMensagemChat, Winapi.Windows;

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

procedure TFormChat.FormCreate(Sender: TObject);
begin
  // SetWindowLong(Handle, GWL_EXSTYLE, WS_EX_APPWINDOW);
  FormChat.Top := Trunc(Screen.WorkAreaHeight - FormChat.Height);
  FormChat.Left := Trunc(Screen.WorkAreaWidth - FormChat.Width);
end;

procedure TFormChat.txtMensagemKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkReturn then
  begin
    if Length(txtMensagem.Text) > 0 then
    begin
      Key := vkNone;
      Mensagem(TMemo(Sender).Lines.Text, False);
      Conexao.SocketPrincipal.Socket.SendText('<|REDIRECT|><|CHAT|>' + TMemo(Sender).Lines.Text + '<|END|>');
      TMemo(Sender).Lines.Clear;
      lstMensagens.ScrollToItem(lstMensagens.ListItems[lstMensagens.Items.Count - 1]);
    end;
  end;
end;

end.
