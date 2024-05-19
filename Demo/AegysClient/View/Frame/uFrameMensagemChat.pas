unit uFrameMensagemChat;

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
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, FMX.ListBox, uFunctions;

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
uses
  uConstants;

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
    lblTitulo.Text := 'Ele''s';
    lblTitulo.TextSettings.FontColor := $FFED3237;
  end
  else
  begin
    lblTitulo.Text := 'Você';
    lblTitulo.TextSettings.FontColor := TAlphaColorRec.Teal;
  end;
  lblTexto.Text := Value.Texto;
  lblTexto.Height := Tamanho - 30;
end;

end.
