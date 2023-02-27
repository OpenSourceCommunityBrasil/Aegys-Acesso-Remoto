program ClientFMXChat;

uses
  System.StartUpCopy,
  FMX.Forms,
  uPrincipal in 'uPrincipal.pas' {Form4},
  uAegysBase in '..\..\..\..\Source\Base\uAegysBase.pas',
  uAegysBufferPack in '..\..\..\..\Source\Base\uAegysBufferPack.pas',
  uAegysConsts in '..\..\..\..\Source\Base\uAegysConsts.pas',
  uAegysDataTypes in '..\..\..\..\Source\Base\uAegysDataTypes.pas',
  uAegysThreads in '..\..\..\..\Source\Base\uAegysThreads.pas',
  uAegysTools in '..\..\..\..\Source\Base\uAegysTools.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
