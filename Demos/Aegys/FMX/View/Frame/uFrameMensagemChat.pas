unit uFrameMensagemChat;

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
    Locale: TLocale;
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
    lblTitulo.Text := Locale.GetLocale(CHAT, 'They');
    lblTitulo.TextSettings.FontColor := $FFED3237;
  end
  else
  begin
    lblTitulo.Text := Locale.GetLocale(CHAT, 'You');
    lblTitulo.TextSettings.FontColor := TAlphaColorRec.Teal;
  end;
  lblTexto.Text := Value.Texto;
  lblTexto.Height := Tamanho - 30;
end;

end.
