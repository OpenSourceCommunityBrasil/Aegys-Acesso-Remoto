program ClientChat;

uses
  Vcl.Forms,
  uClientChat in 'uClientChat.pas' {Form2},
  uAegysBufferPack in '..\..\..\Source\Base\uAegysBufferPack.pas',
  uAegysConsts in '..\..\..\Source\Base\uAegysConsts.pas',
  uAegysDataTypes in '..\..\..\Source\Base\uAegysDataTypes.pas',
  uAegysTools in '..\..\..\Source\Base\uAegysTools.pas',
  uAegysBase in '..\..\..\Source\Base\uAegysBase.pas',
  uAegysThreads in '..\..\..\Source\Base\uAegysThreads.pas',
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Windows10');
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
