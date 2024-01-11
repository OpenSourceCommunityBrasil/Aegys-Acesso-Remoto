program AegysServer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uAegysZlib, uAegysTools, uAegysThreads, uAegysDataTypes, uAegysConsts,
  uAegysBufferPack, uAegysBase, uServerChat, indylaz;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.

