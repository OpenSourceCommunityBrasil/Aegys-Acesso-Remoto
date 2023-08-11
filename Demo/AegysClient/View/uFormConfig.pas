unit uFormConfig;

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
  System.SysUtils, System.Types, System.UITypes, System.Variants,
  System.Classes, System.Actions, System.ImageList, System.JSON,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListBox,
  FMX.Objects, FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, FMX.Layouts,
  FMX.Ani, FMX.ActnList, FMX.ImgList,
  uFunctions, uConstants, uSQLiteConfig, uAegysBase, FMX.ComboEdit;

type
  TCallBack = procedure of object;

type
  TfConfig = class(TForm)
    lyBottomBar: TLayout;
    rrReturn: TRoundRect;
    LBackButton: TLabel;
    flBottomBar: TFlowLayout;
    rrApply: TRoundRect;
    LApplyButton: TLabel;
    VSB: TVertScrollBox;
    lyLanguage: TLayout;
    RLanguage: TRectangle;
    LLanguageSelector: TLabel;
    ImageList1: TImageList;
    language: TComboBox;
    startup: TSwitch;
    lyrunonstartup: TLayout;
    LStartup: TLabel;
    lyquicksuporte: TLayout;
    quicksupp: TSwitch;
    LQuickSupport: TLabel;
    lysystemtray: TLayout;
    systray: TSwitch;
    LSystray: TLabel;
    lyPassword: TLayout;
    RPassword: TRectangle;
    LlyPasswordCaption: TLabel;
    password: TEdit;
    lyServer: TLayout;
    RServer: TRectangle;
    LServer: TLabel;
    server: TComboEdit;
    Layout1: TLayout;
    Rectangle1: TRectangle;
    Label1: TLabel;
    eALIAS: TEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure rrReturnClick(Sender: TObject);
    procedure rrApplyClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure rrApplyMouseEnter(Sender: TObject);
    procedure rrApplyMouseLeave(Sender: TObject);
    procedure quicksuppClick(Sender: TObject);
  private
    { Private declarations }
    Locale: TLocale;
    Cfg: TSQLiteConfig;
    FCallBack: TCallBack;
    procedure SetColors;
  public
    { Public declarations }
    procedure Translate;
    property CallBackConfig: TCallBack read FCallBack write FCallBack;
  end;

var
  fConfig : TfConfig;
  Conexao : TAegysClient;

implementation

{$R *.fmx}

uses uFormConexao;

procedure TfConfig.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 fConfig := Nil;
 Release;
end;

procedure TfConfig.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  server.Enabled := SERVIDOR = '';
  if Not server.Enabled then
   server.ItemIndex := -1;
  Locale := TLocale.Create;
  Cfg := TSQLiteConfig.Create;
  try
    for I := 0 to pred(Componentcount) do
      if Components[I] is TComboBox then
        (Components[I] as TComboBox).ItemIndex :=
          StrToIntDef(Cfg.getValue((Components[I] as TComboBox).Name), -1)
      else if Components[I] is TEdit then
        (Components[I] as TEdit).Text :=
          Cfg.getValue((Components[I] as TEdit).Name)
      else if Components[I] is TSwitch then
        (Components[I] as TSwitch).IsChecked :=
          StrToIntDef(Cfg.getValue((Components[I] as TSwitch).Name), 0)
          .ToBoolean
      else if Components[I] is TComboEdit then
        (Components[I] as TComboEdit).Text :=
          Cfg.getValue((Components[I] as TComboEdit).Name);
  finally
    Cfg.DisposeOf;
  end;

  SetColors;
  Translate;
 eALIAS.Text         := FormConexao.aMyAlias;
 quicksupp.IsChecked := FormConexao.aUnAssistConn;
 password.Enabled := quicksupp.IsChecked;
 if Not password.Enabled then
  password.Text := ''
 Else
  password.Text := FormConexao.aMyFixPass;
end;

procedure TfConfig.FormDestroy(Sender: TObject);
begin
  Locale.DisposeOf;
end;

procedure TfConfig.quicksuppClick(Sender: TObject);
begin
 password.Enabled := quicksupp.IsChecked;
 if Not password.Enabled then
  password.Text := '';
end;

procedure TfConfig.rrApplyClick(Sender: TObject);
var
  Res: TResourceStream;
  aJSON: TJSONObject;
  I: Integer;
begin
  Cfg := TSQLiteConfig.Create;
  aJSON := TJSONObject.Create;
  try
//    for I := 0 to pred(Componentcount) do
//    begin
//      if Components[I] is TComboBox then
//        aJSON.AddPair((Components[I] as TComboBox).Name,
//          IntToStr((Components[I] as TComboBox).Selected.Index))
//      else if Components[I] is TEdit then
//        aJSON.AddPair((Components[I] as TEdit).Name,
//          (Components[I] as TEdit).Text)
//      else if Components[I] is TSwitch then
//        aJSON.AddPair((Components[I] as TSwitch).Name,
//          IntToStr((Components[I] as TSwitch).IsChecked.ToInteger))
//      else if Components[I] is TComboEdit then
//        aJSON.AddPair((Components[I] as TComboEdit).Name,
//          (Components[I] as TComboEdit).Text);
//    end;
//    Cfg.UpdateConfig(aJSON);
//
//    if language.ItemIndex > -1 then
//    begin
//      Res := TResourceStream.Create(HInstance,
//        language.Selected.ItemData.Detail, RT_RCDATA);
//      Res.SaveToFile(Locale.LocaleFileName);
//      Translate;
//    end;
    FormConexao.aMyAlias      := eALIAS.Text;
    FormConexao.aUnAssistConn := quicksupp.IsChecked;
    FormConexao.aMyFixPass    := password.Text;
//    RunOnStartup('AEGYS Remote Acess', Application.Name, startup.IsChecked);
    FormConexao.SendConfigs;
    Close;
  finally
    Cfg.DisposeOf;
  end;
end;

procedure TfConfig.rrApplyMouseEnter(Sender: TObject);
begin
  (Sender as TRoundRect).Stroke.Thickness := 2;
end;

procedure TfConfig.rrApplyMouseLeave(Sender: TObject);
begin
  (Sender as TRoundRect).Stroke.Thickness := 0;
end;

procedure TfConfig.rrReturnClick(Sender: TObject);
begin
 Close;
end;

procedure TfConfig.SetColors;
begin
  RPassword.Fill.Color := SECONDARY_COLOR;
  RLanguage.Fill.Color := SECONDARY_COLOR;
  RServer.Fill.Color := SECONDARY_COLOR;
  rrApply.Fill.Color := PRIMARY_COLOR;
  rrReturn.Fill.Color := PRIMARY_COLOR;
end;

procedure TfConfig.Translate;
begin
  Self.Caption := Locale.GetLocale(FRMS, 'ConfigTitle');
  LLanguageSelector.Text := Locale.GetLocale(FRMS, 'ConfigLanguage');
  LBackButton.Text := Locale.GetLocale(FRMS, 'ConfigBackButton');
  LApplyButton.Text := Locale.GetLocale(FRMS, 'ConfigApplyButton');
  LlyPasswordCaption.Text := Locale.GetLocale(FRMS, 'ConfigPassword');
  password.TextPrompt := Locale.GetLocale(FRMS, 'ConfigPassword');
  Locale.GetLocale(language, tcbLanguage);
  LStartup.Text := Locale.GetLocale(FRMS, 'ConfigStartup');
  LSystray.Text := Locale.GetLocale(FRMS, 'ConfigSystemTray');
  LQuickSupport.Text := Locale.GetLocale(FRMS, 'ConfigQuickSupport');
  LServer.Text := Locale.GetLocale(FRMS, 'ConfigServer');
end;

end.
