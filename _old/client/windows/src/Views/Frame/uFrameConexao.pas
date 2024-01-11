unit uFrameConexao;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, FMX.Objects, FMX.ListBox;

type
  TConexaoRec = record
    ThreadPrincipal: TObject;
    ThreadAreaRemota: TObject;
    ThreadTeclado: TObject;
    ThreadArquivos: TObject;
    Conexao: string;
    IP: string;
    ID: string;
    Senha: string;
    IDParceiro: string;
    Latencia: string;
  end;

  TFrameConexao = class(TFrame)
    recFundo: TRectangle;
    layFundo: TLayout;
    lblConexao: TLabel;
    lblVazio: TLabel;
    lblLatencia: TLabel;
    lblIDParceiro: TLabel;
    lblSenha: TLabel;
    lblID: TLabel;
    lblIP: TLabel;
  private
    FConexao: TConexaoRec;
    FListBox: TListBox;
    procedure SetListBox(const Value: TListBox);
    procedure SetConexao(const Value: TConexaoRec);
    function GetConexao: TConexaoRec;
    { Private declarations }
  public
    property Conexao: TConexaoRec read GetConexao write SetConexao;
    property ListBox: TListBox read FListBox write SetListBox;
  end;

implementation

{$R *.fmx}

{ TFrame1 }

function TFrameConexao.GetConexao: TConexaoRec;
begin
  Result := FConexao;
end;

procedure TFrameConexao.SetConexao(const Value: TConexaoRec);
begin
  FConexao := Value;
  lblConexao.Text := Value.Conexao;
  lblIP.Text := Value.IP;
  lblID.Text := Value.ID;
  lblSenha.Text := Value.Senha;
  lblIDParceiro.Text := Value.IDParceiro;
  lblLatencia.Text := Value.Latencia;
end;

procedure TFrameConexao.SetListBox(const Value: TListBox);
begin
  FListBox := Value;
end;

end.
