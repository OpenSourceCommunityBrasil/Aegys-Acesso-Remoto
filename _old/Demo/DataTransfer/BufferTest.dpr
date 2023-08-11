program BufferTest;

uses
  Vcl.Forms,
  uPrincipal in 'uPrincipal.pas' {Form1},
  uAegysBufferPack in '..\..\Source\Base\uAegysBufferPack.pas',
  uAegysDataTypes in '..\..\Source\Base\uAegysDataTypes.pas',
  uAegysConsts in '..\..\Source\Base\uAegysConsts.pas',
  uAegysTools in '..\..\Source\Base\uAegysTools.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
