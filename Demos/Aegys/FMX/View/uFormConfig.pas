unit uFormConfig;

interface

uses
  System.SysUtils, System.Types, System.UITypes,
  System.Variants, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListBox,
  FMX.Objects, FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, FMX.Layouts,
  FMX.Ani,
  uLocaleFunctions, uConstants;

type
  TCallBackLanguage = procedure of object;

type
  TfConfig = class(TForm)
    lyLanguage: TLayout;
    Rectangle4: TRectangle;
    LLanguageSelector: TLabel;
    Layout10: TLayout;
    cbLanguages: TComboBox;
    lyBottomBar: TLayout;
    rrReturn: TRoundRect;
    LBackButton: TLabel;
    flBottomBar: TFlowLayout;
    rrApply: TRoundRect;
    LApplyButton: TLabel;
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
    FCallBackLanguage: TCallBackLanguage;
    procedure FillComboLanguages(aCombo: TComboBox);
    procedure SetColors;
  public
    { Public declarations }
    procedure Translate;
    property CallBackLang: TCallBackLanguage read FCallBackLanguage
      write FCallBackLanguage;
  end;

var
  fConfig: TfConfig;

implementation

{$R *.fmx}

procedure TfConfig.FillComboLanguages(aCombo: TComboBox);
begin
  aCombo.Items.Clear;
  aCombo.Items.Add('PT_BR');
  aCombo.Items.Add('EN_US');
  aCombo.Items.Add('ES_ES');
  // temporário, forma mais elegante em breve
end;

procedure TfConfig.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
end;

procedure TfConfig.FormCreate(Sender: TObject);
begin
  Locale := TLocale.Create;
  FillComboLanguages(cbLanguages);
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
    Res := TResourceStream.Create(HInstance, cbLanguages.Selected.ItemData.Text,
      RT_RCDATA);
    Res.SaveToFile(Locale.LocaleFileName);

    Translate;
    if Assigned(FCallBackLanguage) then
      FCallBackLanguage;
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
  // Self.Close;
end;

procedure TfConfig.SetColors;
begin
  rrApply.Fill.Color := PRIMARY_COLOR;
  rrReturn.Fill.Color := PRIMARY_COLOR;
end;

procedure TfConfig.Translate;
begin
  Self.Caption := Locale.GetLocale(FRMS, 'ConfigTitle');
  LLanguageSelector.Text := Locale.GetLocale(FRMS, 'ConfigLanguage');
  LBackButton.Text := Locale.GetLocale(FRMS, 'ConfigBackButton');
  LApplyButton.Text := Locale.GetLocale(FRMS, 'ConfigApplyButton');
end;

end.
