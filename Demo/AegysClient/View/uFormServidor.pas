unit uFormServidor;

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
    QryConexoesLATENCIA: TStringField;
    QryConexoesSENHA: TStringField;
    QryConexoesSENHA2: TStringField;
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
    QryConexoesSENHA2.Value := Conexao.SenhaGerada;
    QryConexoesLATENCIA.Value := Conexao.Latencia;
    QryConexoes.Post;
  end;
end;

end.
