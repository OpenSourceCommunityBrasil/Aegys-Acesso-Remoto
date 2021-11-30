unit uFormConfig;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Variants,
  System.Classes, System.Actions, System.ImageList,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListBox,
  FMX.Objects, FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, FMX.Layouts,
  FMX.Ani, FMX.ActnList, FMX.ImgList,
  uCtrl_Conexao, uFunctions, uConstants

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
    Layout10: TLayout;
    lyPassword: TLayout;
    RPassword: TRectangle;
    LlyPasswordCaption: TLabel;
    Layout3: TLayout;
    EPassword: TEdit;
    ImageList1: TImageList;
    cbLanguages: TComboBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    ListBoxItem5: TListBoxItem;
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
    Cfg: TCFGINI;
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
begin
  Locale := TLocale.Create;
  Cfg := TCFGINI.Create;
  EPassword.Text := Cfg.LerCfg('cfg', 'ini', 'CFG', 'pass', false);
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
begin
  if cbLanguages.ItemIndex > -1 then
  begin
    Res := TResourceStream.Create(HInstance,
      cbLanguages.Selected.ItemData.Detail, RT_RCDATA);
    Res.SaveToFile(Locale.LocaleFileName);
    Translate;
  end;

  Cfg.SalvarCfg('cfg', 'ini', 'CFG', 'pass', EPassword.Text, false);

  if Assigned(FCallBack) then
    FCallBack;
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
  EPassword.TextPrompt := Locale.GetLocale(FRMS, 'ConfigPassword');
  Locale.GetLocale(cbLanguages, tcbLanguage);
end;

end.
