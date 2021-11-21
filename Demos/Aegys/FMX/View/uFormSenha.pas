unit uFormSenha;

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
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Ani, FMX.Layouts,
  System.Actions, FMX.ActnList;

type
  TFormSenha = class(TForm)
    Layout1: TLayout;
    Path1: TPath;
    Layout5: TLayout;
    arcLogin: TArc;
    aniLogin: TFloatAnimation;
    btnLogin: TRoundRect;
    Label23: TLabel;
    aniBtnLogin: TFloatAnimation;
    Layout9: TLayout;
    Rectangle4: TRectangle;
    Label7: TLabel;
    Layout10: TLayout;
    txtSenha: TEdit;
    Layout2: TLayout;
    ActionList1: TActionList;
    PROC_SENHA: TAction;
    SpeedButton3: TSpeedButton;
    Path5: TPath;
    PROC_COLAR_SENHA: TAction;
    procedure PROC_SENHAExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure txtSenhaKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure PROC_COLAR_SENHAExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSenha: TFormSenha;
  Canceled: Boolean;

implementation

{$R *.fmx}


uses uFormTelaRemota, uFormConexao, uLibClass;

procedure TFormSenha.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Canceled then
  begin
    FormConexao.MudarStatusConexao(3, 'Acesso cancelado.');
    FormConexao.btnConectar.Enabled := True;
  end;
end;

procedure TFormSenha.FormCreate(Sender: TObject);
begin
  // SetWindowLong(Handle, GWL_EXSTYLE, WS_EX_APPWINDOW);
end;

procedure TFormSenha.FormShow(Sender: TObject);
begin
  Canceled := True;
  txtSenha.Text := '';
  txtSenha.SetFocus;
end;

procedure TFormSenha.PROC_COLAR_SENHAExecute(Sender: TObject);
begin
  txtSenha.Text := TRDLib.ColarTexto;
end;

procedure TFormSenha.PROC_SENHAExecute(Sender: TObject);
begin
  Conexao.SocketPrincipal.Socket.SendText('<|CHECKIDPASSWORD|>' + FormConexao.txtIDParceiro.Text + '<|>' + txtSenha.Text + '<|END|>');
  Canceled := False;
  Close;
end;

procedure TFormSenha.txtSenhaKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkReturn then
  begin
    PROC_SENHAExecute(Sender);
    Key := vkNone;
  end
end;

end.
