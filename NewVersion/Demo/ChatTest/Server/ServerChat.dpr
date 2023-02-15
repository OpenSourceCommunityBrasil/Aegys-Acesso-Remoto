program ServerChat;

uses
  Vcl.Forms,
  uServerChat in 'uServerChat.pas' {Form3},
  uAegysBufferPack in '..\..\..\Source\Base\uAegysBufferPack.pas',
  uAegysConsts in '..\..\..\Source\Base\uAegysConsts.pas',
  uAegysDataTypes in '..\..\..\Source\Base\uAegysDataTypes.pas',
  uAegysTools in '..\..\..\Source\Base\uAegysTools.pas',
  uAegysBase in '..\..\..\Source\Base\uAegysBase.pas',
  uAegysThreads in '..\..\..\Source\Base\uAegysThreads.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
