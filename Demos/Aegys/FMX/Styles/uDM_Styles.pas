unit uDM_Styles;

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

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls;

type
  TDM_Styles = class(TDataModule)
    DesktopStyle: TStyleBook;
    Style1: TStyleBook;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DM_Styles: TDM_Styles;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

end.
