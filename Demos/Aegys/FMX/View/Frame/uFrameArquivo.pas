unit uFrameArquivo;

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
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects, System.ImageList, FMX.ImgList,
  FMX.Layouts, FMX.ListBox;

type
  TArquivoRec = record
    Extensao: string;
    Nome: string;
  end;

  TFrameArquivo = class(TFrame)
    Layout1: TLayout;
    ImageList1: TImageList;
    imgArquivo: TImage;
    lblArquivo: TLabel;
    btnArquivo: TSpeedButton;
    procedure btnArquivoDblClick(Sender: TObject);
    procedure btnArquivoClick(Sender: TObject);
  private
    FListBox: TListBox;
    FArquivo: TArquivoRec;
    procedure SetArquivoRec(const Value: TArquivoRec);
    procedure SetListBox(const Value: TListBox);
    function GetArquivo: TArquivoRec;
    { Private declarations }
  public
    property Arquivo: TArquivoRec read GetArquivo write SetArquivoRec;
    property ListBox: TListBox read FListBox write SetListBox;
  end;

var
  ext: array [0 .. 17] of string = (
    '..',
    'folder',
    'file',
    '.exe',
    '.txt',
    '.rar',
    '.mp3',
    '.zip',
    '.jpeg',
    '.bat',
    '.html',
    '.sql',
    '.xml',
    '.xls',
    '.png',
    '.doc',
    '.pdf',
    '.dll'
  );

implementation

{$R *.fmx}
{ TFrameArquivo }

procedure TFrameArquivo.btnArquivoClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ListBox.Count - 1 do
  begin
    if (ListBox.ListItems[i].Components[0] is TFrameArquivo) and
      (ListBox.ListItems[i].Components[0] <> Self) then
      TFrameArquivo(ListBox.ListItems[i].Components[0])
        .btnArquivo.IsPressed := False;
  end;
  btnArquivo.IsPressed := True;
end;

procedure TFrameArquivo.btnArquivoDblClick(Sender: TObject);
begin
  ListBox.OnItemClick(ListBox, TListBoxItem(Self.Parent));
end;

function TFrameArquivo.GetArquivo: TArquivoRec;
begin
  Result := FArquivo;
end;

procedure TFrameArquivo.SetArquivoRec(const Value: TArquivoRec);
var
  i: Integer;
  size: TSizeF;
begin
  size := TSizeF.Create(25, 25);
  FArquivo := Value;
  TListBoxItem(Self.Parent).Tag := -1;
  if Value.Extensao <> '' then
  begin
    for i := Low(ext) to High(ext) do
    begin
      if ext[i] = LowerCase(Value.Extensao) then
      begin
        imgArquivo.Bitmap.Assign(ImageList1.Bitmap(size, i));
        TListBoxItem(Self.Parent).Tag := i;
        Break;
      end;
    end;
  end;
  if TListBoxItem(Self.Parent).Tag = -1 then
  begin
    imgArquivo.Bitmap.Assign(ImageList1.Bitmap(size, 2));
    TListBoxItem(Self.Parent).Tag := 2;
  end;
  lblArquivo.Text := Value.Nome;
  TListBoxItem(Self.Parent).TagString := Value.Nome;
end;

procedure TFrameArquivo.SetListBox(const Value: TListBox);
begin
  FListBox := Value;
end;

end.
