unit uFormServidor;

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
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, Vcl.Grids, Vcl.DBGrids, Vcl.ExtCtrls;

type
  TFormServidor = class(TForm)
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    QryConexoes: TFDMemTable;
    tReload: TTimer;
    QryConexoesPROTOCOLO: TStringField;
    QryConexoesID: TStringField;
    QryConexoesSENHA: TStringField;
    QryConexoesLATENCIA: TStringField;
    procedure tReloadTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormServidor: TFormServidor;

implementation

{$R *.dfm}

uses uCtrl_Conexoes, uDMServer;

procedure TFormServidor.FormCreate(Sender: TObject);
begin
  if not Assigned(DMServer) then
    DMServer := TDMServer.Create(nil);
end;

procedure TFormServidor.FormDestroy(Sender: TObject);
begin
  tReload.Enabled := False;
  if Assigned(DMServer) then
    FreeAndNil(DMServer);
end;

procedure TFormServidor.tReloadTimer(Sender: TObject);
var
  Conexao: TConexao;
begin
  QryConexoes.EmptyDataSet;
  for Conexao in DMServer.Conexoes.ListaConexoes do
  begin
    QryConexoes.Append;
    QryConexoesPROTOCOLO.Value := Conexao.Protocolo;
    QryConexoesID.Value := Conexao.ID;
    QryConexoesSENHA.Value := Conexao.Senha;
    QryConexoesLATENCIA.Value := Conexao.Latencia;
    QryConexoes.Post;
  end;
end;

end.
