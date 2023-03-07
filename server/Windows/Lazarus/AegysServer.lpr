program AegysServer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, indylaz, 
  uAegysZlib in '..\..\..\Component\Utils\uAegysZlib.pas', 
  uAegysTools in '..\..\..\Component\Base\uAegysTools.pas', 
  uAegysThreads in '..\..\..\Component\Base\uAegysThreads.pas', 
  uAegysDataTypes in '..\..\..\Component\Base\uAegysDataTypes.pas',
  uAegysConsts in '..\..\..\Component\Base\uAegysConsts.pas', 
  uAegysBufferPack in '..\..\..\Component\Base\uAegysBufferPack.pas', 
  uAegysBase in '..\..\..\Component\Base\uAegysBase.pas',
  uConstants in '..\..\..\commons\uConstants.pas',
  uServer in 'src\uServer'
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfServer, fServer);
  Application.Run;
end.

