unit uDM_Styles;

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
