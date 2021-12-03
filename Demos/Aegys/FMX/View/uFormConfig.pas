unit uFormConfig;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Variants,
  System.Classes, System.Actions, System.ImageList, System.JSON,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListBox,
  FMX.Objects, FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, FMX.Layouts,
  FMX.Ani, FMX.ActnList, FMX.ImgList,
  uCtrl_Conexao, uFunctions, uConstants, uSQLiteConfig, FMX.ComboEdit

    ;

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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure rrReturnClick(Sender: TObject);
    procedure rrApplyClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure rrApplyMouseEnter(Sender: TObject);
    procedure rrApplyMouseLeave(Sender: TObject);
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
  fConfig: TfConfig;
  Conexao: TConexao;

implementation

{$R *.fmx}

procedure TfConfig.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
end;

procedure TfConfig.FormCreate(Sender: TObject);
var
  I: Integer;
begin
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
end;

procedure TfConfig.FormDestroy(Sender: TObject);
begin
  Locale.DisposeOf;
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
    for I := 0 to pred(Componentcount) do
    begin
      if Components[I] is TComboBox then
        aJSON.AddPair((Components[I] as TComboBox).Name,
          IntToStr((Components[I] as TComboBox).Selected.Index))
      else if Components[I] is TEdit then
        aJSON.AddPair((Components[I] as TEdit).Name,
          (Components[I] as TEdit).Text)
      else if Components[I] is TSwitch then
        aJSON.AddPair((Components[I] as TSwitch).Name,
          IntToStr((Components[I] as TSwitch).IsChecked.ToInteger))
      else if Components[I] is TComboEdit then
        aJSON.AddPair((Components[I] as TComboEdit).Name,
          (Components[I] as TComboEdit).Text);
    end;
    Cfg.UpdateConfig(aJSON);

    if language.ItemIndex > -1 then
    begin
      Res := TResourceStream.Create(HInstance,
        language.Selected.ItemData.Detail, RT_RCDATA);
      Res.SaveToFile(Locale.LocaleFileName);
      Translate;
    end;

    RunOnStartup('AEGYS Remote Acess', Application.Name, startup.IsChecked);
    if Assigned(FCallBack) then
      FCallBack;
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
  Self.DisposeOf;
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
