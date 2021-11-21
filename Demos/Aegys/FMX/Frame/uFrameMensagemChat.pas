unit uFrameMensagemChat;

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
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, FMX.ListBox;

type
  TMensagemRec = record
    Atendente: Boolean;
    Texto: string;
  end;

  TFrameMensagemChat = class(TFrame)
    Layout1: TLayout;
    lblTexto: TLabel;
    lblTitulo: TLabel;
  private
    FListBox: TListBox;
    procedure SetListBox(const Value: TListBox);
    procedure SetMensagem(const Value: TMensagemRec);
    function GetTamanho: Integer;
    { Private declarations }
  public
    { Public declarations }
  published
    property ListBox: TListBox read FListBox write SetListBox;
    property Mensagem: TMensagemRec write SetMensagem;
    property Tamanho: Integer read GetTamanho;
  end;

implementation

{$R *.fmx}

{ TFrame1 }

function TFrameMensagemChat.GetTamanho: Integer;
begin
  Result := Length(lblTexto.Text);
  if Result < 28 then
    Result := 28;
  Result := Trunc((Result / 28) * 20) + 30;
end;

procedure TFrameMensagemChat.SetListBox(const Value: TListBox);
begin
  FListBox := Value;
end;

procedure TFrameMensagemChat.SetMensagem(const Value: TMensagemRec);
begin
  if Value.Atendente then
  begin
    lblTitulo.Text := 'Atendente disse: ';
    lblTitulo.TextSettings.FontColor := $FFED3237;
  end
  else
  begin
    lblTitulo.Text := 'Você disse: ';
    lblTitulo.TextSettings.FontColor := TAlphaColorRec.Teal;
  end;
  lblTexto.Text := Value.Texto;
  lblTexto.Height := Tamanho - 30;
end;

end.
