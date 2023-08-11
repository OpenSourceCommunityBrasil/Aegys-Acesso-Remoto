unit uFunctions;

{
  Project Aegys Remote Support.

  Created by Gilberto Rocha da Silva in 04/05/2017 based on project Allakore, has by objective to promote remote access
  and other resources freely to all those who need it, today maintained by a beautiful community. Listing below our
  higly esteemed collaborators:

  Gilberto Rocha da Silva (XyberX) (Creator of Aegys Project/Main Developer/Admin)
  Wendel Rodrigues Fassarella (wendelfassarella) (Creator of Aegys FMX/CORE Developer)
  Rai Duarte Jales (Raí Duarte) (Aegys Server Developer)
  Roniery Santos Cardoso (Aegys Developer)
  Alexandre Carlos Silva Abade (Aegys Developer)
  Mobius One (Aegys Developer)
}

interface

Uses
  System.SysUtils, System.Types, System.Classes,
  FMX.ListBox, FMX.Layouts,
  uConstants, Registry, Winapi.windows;

type
  TVertScrollBoxHelper = class helper for TVertScrollBox
  public
    procedure Clear;
  end;

function iif(bcondicao: boolean; vtrue, vfalse: variant): variant;
function RunOnStartup(sProgTitle, sCmdLine: string; bRunOnce: boolean): boolean;

implementation

function RunOnStartup(sProgTitle, sCmdLine: string; bRunOnce: boolean): boolean;
var
  sKey: string;
  reg: TRegIniFile;
begin
  if not bRunOnce then
    sKey := ''
  else
    sKey := 'Once';
  reg := TRegIniFile.Create('');
  reg.RootKey := HKEY_LOCAL_MACHINE;
  reg.WriteString('Software\Microsoft\Windows\CurrentVersion\Run' + sKey + #0,
    sProgTitle, sCmdLine);
  reg.Free;
end;

function iif(bcondicao: boolean; vtrue, vfalse: variant): variant;
begin
  if bcondicao then
    Result := vtrue
  else
    Result := vfalse;
end;

{ TVertScrollBoxHelper }

procedure TVertScrollBoxHelper.Clear;
var
  i: integer;
begin
  for i := pred(self.Content.ChildrenCount) downto 0 do
    self.Content.Children.Items[i].DisposeOf;
end;

end.
