unit uFormSenha;

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
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Ani, FMX.Layouts,
  System.Actions, FMX.ActnList, uFunctions;

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

uses uFormTelaRemota, uFormConexao, uLibClass, uConstants;

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
    FormConexao.MudarStatusConexao(3, Locale.GetLocale(MSGS, 'Cancelled'));
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
  Conexao.SocketPrincipal.Socket.SendText('<|CHECKIDPASSWORD|>' +
    FormConexao.EGuestID.Text + '<|>' + EPassword.Text + '<|END|>');
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
