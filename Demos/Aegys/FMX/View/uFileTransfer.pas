unit uFileTransfer;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti,
  FMX.Grid.Style, FMX.Bind.GenData, Data.Bind.GenData, Data.Bind.EngExt,
  FMX.Bind.DBEngExt, System.Bindings.Outputs, FMX.Bind.Editors,
  Data.Bind.Components, Data.Bind.ObjectScope, FMX.Grid,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Objects, FMX.StdCtrls,
  FMX.Bind.Grid, Data.Bind.Grid, System.ImageList, FMX.ImgList, FMX.Edit,
  FMX.ComboEdit, FMX.Layouts;

type
  TfFileTransfer = class(TForm)
    Image1: TImage;
    Layout1: TLayout;
    Layout2: TLayout;
    SGLocal: TStringGrid;
    GlyphColumn1: TGlyphColumn;
    StringColumn1: TStringColumn;
    DateTimeColumn1: TDateTimeColumn;
    StringColumn2: TStringColumn;
    Layout3: TLayout;
    Layout4: TLayout;
    SGRemote: TStringGrid;
    GlyphColumn2: TGlyphColumn;
    StringColumn3: TStringColumn;
    DateTimeColumn2: TDateTimeColumn;
    StringColumn4: TStringColumn;
    Button1: TButton;
    ceLocalPath: TComboEdit;
    ceRemotePath: TComboEdit;
    sbUpload: TSpeedButton;
    sbDownload: TSpeedButton;
    procedure Button1Click(Sender: TObject);
    procedure SGLocalDrawColumnCell(Sender: TObject; const Canvas: TCanvas;
      const Column: TColumn; const Bounds: TRectF; const Row: Integer;
      const Value: TValue; const State: TGridDrawStates);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fFileTransfer: TfFileTransfer;

implementation

{$R *.fmx}

procedure TfFileTransfer.Button1Click(Sender: TObject);
begin
  SGLocal.RowCount := SGLocal.RowCount + 1;
  SGLocal.Cells[0, SGLocal.RowCount - 1] := 'd';
  SGLocal.Cells[1, SGLocal.RowCount - 1] := ExtractFileDir(ParamStr(0));
  SGLocal.Cells[2, SGLocal.RowCount - 1] := FormatDateTime('dd/mm/yyyy hh:nn:ss', now);
  SGLocal.Cells[3, SGLocal.RowCount - 1] := random(100).ToString;
  SGLocal.RowCount := SGLocal.RowCount + 1;
  SGLocal.Cells[0, SGLocal.RowCount - 1] := 'f';
  SGLocal.Cells[1, SGLocal.RowCount - 1] := ParamStr(0);
  SGLocal.Cells[2, SGLocal.RowCount - 1] := FormatDateTime('dd/mm/yyyy hh:nn:ss', now);
  SGLocal.Cells[3, SGLocal.RowCount - 1] := random(100).ToString;
end;

procedure TfFileTransfer.SGLocalDrawColumnCell(Sender: TObject;
  const Canvas: TCanvas; const Column: TColumn; const Bounds: TRectF;
  const Row: Integer; const Value: TValue; const State: TGridDrawStates);
begin
  if (Column.Index = 0) and (SGLocal.Cells[Column.Index, Row] = 'd') then
    Canvas.DrawBitmap(Image1.Bitmap, Image1.Bitmap.Bounds, Bounds, 1);
end;

end.
