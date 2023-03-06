unit uFormSenha;

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
  System.Variants, System.Actions,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Ani, FMX.Layouts,
  FMX.ActnList,
  uLocale;

type
  TFormSenha = class(TForm)
    Layout1: TLayout;
    Path1: TPath;
    Layout5: TLayout;
    arcLogin: TArc;
    aniLogin: TFloatAnimation;
    btnLogin: TRoundRect;
    LOkButton: TLabel;
    aniBtnLogin: TFloatAnimation;
    Layout9: TLayout;
    RPassword: TRectangle;
    LPassword: TLabel;
    Layout10: TLayout;
    EPassword: TEdit;
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
    procedure EPasswordKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure PROC_COLAR_SENHAExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EPasswordKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
    { Private declarations }
    Locale: TLocale;
    procedure SetColors;
  public
    { Public declarations }
  end;

var
  FormSenha: TFormSenha;
  Canceled: Boolean;

implementation

{$R *.fmx}

uses uFormTelaRemota, uFormConexao, uLibClass, uConstants, uAegysConsts;

procedure TFormSenha.EPasswordKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if (Key = vkReturn) And (EPassword.Text <> '') then
  begin
    PROC_SENHAExecute(Sender);
    Key := vkNone;
  end
end;

procedure TFormSenha.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Canceled then
  begin
    FormConexao.MudarStatusConexao(3, Locale.GetLocale(lsMESSAGES, lvMsgCanceled));
    FormConexao.btnConectar.Enabled := True;
  end;
end;

procedure TFormSenha.FormCreate(Sender: TObject);
begin
  Locale := TLocale.Create;
  SetColors;
  // SetWindowLong(Handle, GWL_EXSTYLE, WS_EX_APPWINDOW);
end;

procedure TFormSenha.FormDestroy(Sender: TObject);
begin
  Locale.DisposeOf;
end;

procedure TFormSenha.FormShow(Sender: TObject);
begin
  Canceled := True;
  EPassword.Text := '';
  EPassword.SetFocus;
end;

procedure TFormSenha.PROC_COLAR_SENHAExecute(Sender: TObject);
begin
  EPassword.Text := TRDLib.ColarTexto;
end;

procedure TFormSenha.PROC_SENHAExecute(Sender: TObject);
begin
  Conexao.SendCommand(cCheckPass + Conexao.SessionID + '&' + FormConexao.EGuestID.Text + '&' + EPassword.Text);
  Canceled := False;
  Close;
end;

procedure TFormSenha.SetColors;
begin
  RPassword.Fill.Color := SECONDARY_COLOR;
  Path1.Fill.Color := PRIMARY_COLOR;
  btnLogin.Fill.Color := PRIMARY_COLOR;
end;

procedure TFormSenha.EPasswordKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkReturn then
  begin
    PROC_SENHAExecute(Sender);
    Key := vkNone;
  end
end;

end.
