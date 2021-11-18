unit Form_Config;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin,
  Vcl.Buttons, Vcl.ExtCtrls, registry, uProxy, pngimage, acPNG;

type
  Tfrm_Config = class(TForm)
    lblHost: TLabel;
    lblTimeOut: TLabel;
    edtHost: TEdit;
    seTimeOut: TSpinEdit;
    chkStarter: TCheckBox;
    tmrCheck: TTimer;
    lblPort: TLabel;
    sePort: TSpinEdit;
    Language_Label: TLabel;
    cbxLanguage: TComboBox;
    gbxProxy: TGroupBox;
    lblHostProxy: TLabel;
    lblPortProxy: TLabel;
    edtHostProxy: TEdit;
    sePortProxy: TSpinEdit;
    chkEnableProxy: TCheckBox;
    Label2: TLabel;
    edtUrlUpdates: TEdit;
    pnltop: TPanel;
    Logo_Image: TImage;
    Title1_Label: TLabel;
    Label1: TLabel;
    pnlbottom: TPanel;
    sbSave: TSpeedButton;
    SpeedButton1: TSpeedButton;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure tmrCheckTimer(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure chkEnableProxyClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    arq_ini : string;
  end;

var
  frm_Config: Tfrm_Config;

implementation

{$R *.dfm}

uses Form_Main, uUteis;

procedure Tfrm_Config.chkEnableProxyClick(Sender: TObject);
var
  ProxyEnabled: boolean;
const
  YesNo: array [false .. True] of string = (' not ', '');
begin
  // get proxy information
  if chkEnableProxy.Checked and GetProxy(HostProxy, PortProxy, ProxyEnabled)
  then
    ShowMessage(Format('Your proxy is %s on port %d, it is%s enabled.',
      [Host, Port, YesNo[ProxyEnabled]]))
  else
  begin
    ShowMessage('No proxy detected');
    chkEnableProxy.OnClick := nil;
    try
      chkEnableProxy.Checked := false;
    finally
      chkEnableProxy.OnClick := chkEnableProxyClick;
    end;
  end;

  if not ProxyEnabled then
  begin
    edtHostProxy.Clear;
    sePortProxy.Value := 0;
  end
  else
  begin
    edtHostProxy.Text := HostProxy;
    sePortProxy.Value := PortProxy;
  end;

  lblHostProxy.Enabled := chkEnableProxy.Checked;
  edtHostProxy.Enabled := chkEnableProxy.Checked;

  lblPortProxy.Enabled := chkEnableProxy.Checked;
  sePortProxy.Enabled := chkEnableProxy.Checked;
end;

procedure Tfrm_Config.FormCreate(Sender: TObject);
var
  Reg: TRegistry;
  S: string;
begin
  arq_ini := ExtractFilePath(Application.ExeName) + 'Aegys.ini';

  edtHost.Text := GetIni(arq_ini, cGeneral, 'host', cHost, false);
  sePort.Text := GetIni(arq_ini, cGeneral, 'port', cPort, false);

  if sePort.Value = 0 then
    sePort.Value := 9078;

 { edtGroup.Text := GetIni(ExtractFilePath(Application.ExeName) +
    Application.Title + '.ini', cGeneral, cGroup, True);
  edtMachineName.Text := GetIni(ExtractFilePath(Application.ExeName) +
    Application.Title + '.ini', cGeneral, cMachine, True);    }

  seTimeOut.Text := GetIni(arq_ini, cGeneral, 'connectiontimeout', cConnectTimeOut, false);
  S := GetIni(arq_ini, cGeneral, 'StarterWithWindows', cStarterWithWindows, false);
  cbxLanguage.ItemIndex :=  StrToInt(GetIni(arq_ini, cGeneral, 'language', cLanguage, false));

  if S = cYes then
    chkStarter.Checked := True
  else
    chkStarter.Checked := false;

  chkEnableProxy.Checked := GetIni(arq_ini, cGeneral, 'proxy', cProxy, false) = cYes;

  if chkEnableProxy.Checked then
  begin
    edtHostProxy.Text := GetIni(arq_ini, cGeneral, 'hostproxy', cHostProxy, false);
    sePortProxy.Text := GetIni(arq_ini, cGeneral, 'portproxy', cPortProxy, false);
  end
  else
  begin
    edtHostProxy.Clear;
    sePortProxy.Clear;
  end;

end;

procedure Tfrm_Config.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_F4) or (Key = VK_ESCAPE) then
    Key := 0;
end;

procedure Tfrm_Config.FormKeyPress(Sender: TObject; var Key: Char);
begin
  IF Key = #13 THEN
  BEGIN
    Key := #0;
    Perform(Wm_NextDlgCtl, 0, 0);
  END;
end;

procedure Tfrm_Config.FormShow(Sender: TObject);
begin
  tmrCheck.Enabled := True;
end;

procedure Tfrm_Config.sbSaveClick(Sender: TObject);
var
  Reg: TRegistry;
  S: string;
begin
  Reg := TRegistry.Create;
  S := ExtractFileDir(Application.ExeName) + '\' +
    ExtractFileName(Application.ExeName);

  // grava se o cliente inicia com o windows
  Reg.rootkey := HKEY_LOCAL_MACHINE;
  Reg.Openkey('SOFTWARE\MICROSOFT\WINDOWS\CURRENTVERSION\RUN', false);
  if chkStarter.Checked then
  begin
    Reg.WriteString(Caption, S);
    SaveIni('StarterWithWindows', cYes, arq_ini, cGeneral, false);
  end
  else
  begin
    Reg.DeleteValue(Caption);
    SaveIni('StarterWithWindows', cNO, arq_ini, cGeneral, false);
  end;

  // salva as configurações no ini
  SaveIni('host', edtHost.Text, arq_ini, cGeneral, false);
  SaveIni('port', sePort.Text,arq_ini, cGeneral, false);

{  SaveIni(cMachine, edtMachineName.Text, ExtractFilePath(Application.ExeName) +
    Application.Title + '.ini', cGeneral, True);
  SaveIni(cGroup, edtGroup.Text, ExtractFilePath(Application.ExeName) +
    Application.Title + '.ini', cGeneral, True);      }

  SaveIni('connectiontimeout', seTimeOut.Text, arq_ini, cGeneral, false);

  SaveIni('language', IntToStr(cbxLanguage.ItemIndex), arq_ini, cGeneral, false);

  if chkEnableProxy.Checked then
    SaveIni('proxy', cYes,arq_ini, cGeneral, false)
  else
    SaveIni('proxy', cNO, arq_ini, cGeneral, false);

  SaveIni('UrlUpdates', edtUrlUpdates.Text, arq_ini, cGeneral, false);

  Host := edtHost.Text;
  Port := sePort.Value;
  //vGroup := edtGroup.Text;
 // vMachine := edtMachineName.Text;
  ConnectionTimeout := seTimeOut.Value;
  Proxy := chkEnableProxy.Checked;
  if Proxy then
  begin
    HostProxy := edtHostProxy.Text;
    PortProxy := sePortProxy.Value;
  end
  else
  begin
    HostProxy := '';
    PortProxy := 0;
  end;

  SetHostPortGroupMach;
  modalresult := mrok;
end;

procedure Tfrm_Config.SpeedButton1Click(Sender: TObject);
begin
  close;
end;

procedure Tfrm_Config.tmrCheckTimer(Sender: TObject);
begin
  if (edtHost.Text = '') //or (edtGroup.Text = '') or (edtMachineName.Text = '')
    or (seTimeOut.Value = 0) or (sePort.Value = 0) then
    sbSave.Enabled := false
  else
    sbSave.Enabled := True;
end;

end.
