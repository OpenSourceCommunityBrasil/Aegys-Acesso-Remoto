program AegysClient ;

uses
  Vcl.Forms,
  Windows,
  System.SysUtils,
  Form_Main in 'Form_Main.pas' {frm_Main},
  Form_Password in 'Form_Password.pas' {frm_Password},
  Form_RemoteScreen in 'Form_RemoteScreen.pas' {frm_RemoteScreen},
  Vcl.Themes,
  Vcl.Styles,
  Form_Chat in 'Form_Chat.pas' {frm_Chat},
  Form_ShareFiles in 'Form_ShareFiles.pas' {frm_ShareFiles},
  Form_Config in 'Form_Config.pas' {frm_Config},
  uProxy in 'uProxy.pas',
  uDGCompressor in 'uDGCompressor.pas',
  uCaptureScreen in 'uCaptureScreen.pas' {dmCaptureScreen: TDataModule},
  vfw in 'vfw.pas',
  AwResizeImage in 'AwResizeImage.pas',
  uComboChoose in 'uComboChoose.pas',
  uCaptureDeviceMode in 'uCaptureDeviceMode.pas',
  uIconsAssoc in 'uIconsAssoc.pas',
  uFilesFoldersOP in 'uFilesFoldersOP.pas',
  ImageCapture in 'ImageCapture.pas',
  UVideoDriver in 'UVideoDriver.pas',
  resizeunit in 'resizeunit.pas',
  uScanlineComparer in 'uScanlineComparer.pas',
  uConnectar in 'uConnectar.pas' {fConnectar};

{$R *.res}

Var
  MutexHandle: THandle;

begin
  // FullDebugMode := True;
  MutexHandle := CreateMutex(nil, True, 'AegysClient');
  If MutexHandle <> 0 Then
  Begin
    If GetLastError = ERROR_ALREADY_EXISTS Then
    Begin
      MessageBox(0, 'O Aegys já está em execução!', 'Informação !',
        mb_IconHand);
      CloseHandle(MutexHandle);
      Exit;
    End;
  End;
  Application.Initialize;
  // Seta a prioridade da aplicação
  SetPriorityClass(GetCurrentProcess, HIGH_PRIORITY_CLASS);
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Aegys - Controle Remoto';
  Application.CreateForm(Tfrm_Main, frm_Main);
  Application.Run;

end.
