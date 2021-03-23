unit Form_Password;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.pngimage,
  Vcl.ExtCtrls,
  Vcl.StdCtrls, Vcl.Buttons, System.MaskUtils;

type
  Tfrm_Password = class(TForm)
    Panel2: TPanel;
    Panel1: TPanel;
    Label11: TLabel;
    Label10: TLabel;
    Image1: TImage;
    Ok_BitBtn: TButton;
    Button2: TButton;
    Password_Edit: TEdit;
    procedure FormShow(Sender: TObject);
    procedure Password_EditKeyPress(Sender: TObject; var Key: Char);
    procedure Ok_BitBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Password_EditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    procedure Acessar_cliente;
  end;

var
  frm_Password: Tfrm_Password;
  Canceled: Boolean;

implementation

{$R *.dfm}

uses
  Form_RemoteScreen, Form_Main;

procedure Tfrm_Password.Ok_BitBtnClick(Sender: TObject);
begin
  Acessar_cliente;
end;

procedure Tfrm_Password.Acessar_cliente;
Var
  formato: String;
begin
  formato := StringReplace(frm_Main.TargetID_MaskEdit.Text, '-', '',
    [rfReplaceAll, rfIgnoreCase]);
  formato := Trim(formato);
  formato := MaskDoFormatText(mascara, formato, #0);
  frm_Main.ipPSMain_Socket.DataToSend('<|CHECKIDPASSWORD|>' + formato + '<|>' +
    Password_Edit.Text + '<|>' + '<|BESTQ|>' +
    IntToStr(frm_Main.cbQualidade.ItemIndex) + frm_Main.CommandEnd);
  frm_Main.LastPassWordClient := formato + '|' + Password_Edit.Text;
  Canceled := false;
  Close;
end;

procedure Tfrm_Password.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (Canceled) then
  begin
    with frm_Main do
    begin
      Status_Image.Picture.Assign(Image3.Picture);
      Status_Label.Caption := 'Access canceled.';
      TargetID_MaskEdit.Enabled := true;
      Connect_BitBtn.Enabled := true;
    end;
  end;
end;

procedure Tfrm_Password.FormShow(Sender: TObject);
begin
  Canceled := true;
  Password_Edit.Clear;

  // verifica se o último TargetID é igual ao TargetID Atual, ou seja, o mesmo client
  if copy(frm_Main.LastPassWordClient, 1, Pos('|', frm_Main.LastPassWordClient)
    - 1) = frm_Main.TargetID_MaskEdit.Text then
    Password_Edit.Text := copy(frm_Main.LastPassWordClient,
      Pos('|', frm_Main.LastPassWordClient) + 1);

  Password_Edit.SetFocus;

end;

procedure Tfrm_Password.Password_EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = vk_return then
    Ok_BitBtn.SetFocus;
end;

procedure Tfrm_Password.Password_EditKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Ok_BitBtn.Click;
    Key := #0;
  end
end;

end.
