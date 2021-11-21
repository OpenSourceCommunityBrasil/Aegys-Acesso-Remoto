unit uFormArquivos;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.Objects, FMX.Layouts, FMX.ListView, FMX.StdCtrls, System.Actions,
  FMX.ActnList, FMX.Ani, FMX.Edit, FMX.Controls.Presentation, FMX.ListBox,
  Winapi.Messages;

type
  TFormArquivos = class(TForm)
    Layout1: TLayout;
    Line1: TLine;
    Layout2: TLayout;
    LFolder: TLabel;
    Rectangle3: TRectangle;
    EFolder: TEdit;
    Layout3: TLayout;
    Layout5: TLayout;
    Rectangle4: TRoundRect;
    ActionList1: TActionList;
    PROC_UPLOAD: TAction;
    PROC_DOWNLOAD: TAction;
    Layout4: TLayout;
    Rectangle5: TRoundRect;
    Layout6: TLayout;
    Layout8: TLayout;
    Layout9: TLayout;
    LDownloadProgress: TLabel;
    LDownloadSize: TLabel;
    pgbDownload: TProgressBar;
    Layout7: TLayout;
    Layout10: TLayout;
    LUploadProgress: TLabel;
    LUploadSize: TLabel;
    pgbUpload: TProgressBar;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    lstArquivos: TListBox;
    btnDownload: TSpeedButton;
    btnUpload: TSpeedButton;
    procedure PROC_UPLOADExecute(Sender: TObject);
    procedure PROC_DOWNLOADExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure EFolderKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure lstArquivosItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
    procedure AbrirPasta(APasta: string);
    procedure GoToDirectory(ADirectory: string);
    procedure WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
      message WM_GETMINMAXINFO;
    procedure Translate;
  public
    DirectoryToSaveFile: string;
    FileStream: TFileStream;
    procedure CarregarListaPastas(ALista: string);
    procedure CarregarListaArquivos(ALista: string);
  end;

var
  FormArquivos: TFormArquivos;

implementation

{$R *.fmx}

uses uFormConexao, uFrameArquivo, Winapi.Windows, FMX.Platform.Win,
  uLocaleFunctions, uConstants;

procedure TFormArquivos.AbrirPasta(APasta: string);
var
  ADirectory: string;
begin
  if not EFolder.Enabled then
    Exit;

  if APasta = 'Retorno' then
  begin
    ADirectory := EFolder.Text;
    Delete(ADirectory, Length(ADirectory), Length(ADirectory));
    EFolder.Text := ExtractFilePath(ADirectory + '..');
  end
  else
    EFolder.Text := EFolder.Text + APasta + '\';

  GoToDirectory(EFolder.Text);
end;

procedure TFormArquivos.CarregarListaArquivos(ALista: string);
var
  ItemAdd: TListBoxItem;
  ARec: TArquivoRec;
  FItem: TFrameArquivo;
  slLista: TStringList;
  i: Integer;
begin
  lstArquivos.BeginUpdate;

  try
    slLista := TStringList.Create;
    slLista.Text := ALista;
    for i := 0 to slLista.Count - 1 do
    begin

      ARec.Nome := slLista[i];
      ARec.Extensao := LowerCase(ExtractFileExt(slLista[i]));

      ItemAdd := TListBoxItem.Create(nil);
      FItem := TFrameArquivo.Create(ItemAdd);
      FItem.Parent := ItemAdd;
      FItem.Arquivo := ARec;
      ItemAdd.Height := FItem.Height;
      FItem.Align := TAlignLayout.Client;
      FItem.ListBox := lstArquivos;
      lstArquivos.AddObject(ItemAdd);
    end;
  finally
    FreeAndNil(slLista);
  end;

  lstArquivos.EndUpdate;
end;

procedure TFormArquivos.CarregarListaPastas(ALista: string);
var
  ItemAdd: TListBoxItem;
  ARec: TArquivoRec;
  FItem: TFrameArquivo;
  slLista: TStringList;
  i: Integer;
begin
  lstArquivos.Items.Clear;
  lstArquivos.BeginUpdate;

  try
    slLista := TStringList.Create;
    slLista.Text := ALista;
    for i := 0 to slLista.Count - 1 do
    begin

      if (slLista[i] = '..') then
      begin
        ARec.Nome := slLista[i];
        ARec.Extensao := '..';
      end
      else
      begin
        ARec.Nome := slLista[i];
        ARec.Extensao := 'folder';
      end;

      ItemAdd := TListBoxItem.Create(nil);
      FItem := TFrameArquivo.Create(ItemAdd);
      FItem.Parent := ItemAdd;
      FItem.Arquivo := ARec;
      ItemAdd.Height := FItem.Height;
      FItem.Align := TAlignLayout.Client;
      FItem.ListBox := lstArquivos;
      lstArquivos.AddObject(ItemAdd);
    end;
  finally
    FreeAndNil(slLista);
  end;

  lstArquivos.EndUpdate;
end;

procedure TFormArquivos.EFolderKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if (Key = vkReturn) then
  begin
    GoToDirectory(EFolder.Text);
    Key := vkNone;
  end;
end;

procedure TFormArquivos.FormCreate(Sender: TObject);
begin
  Translate;
  SetWindowLong(FmxHandleToHWND(Handle), GWL_EXSTYLE, WS_EX_APPWINDOW);
end;

procedure TFormArquivos.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
var
  i: Integer;
begin
  if (Key <> vkReturn) then
    Exit;
  for i := 0 to lstArquivos.Items.Count - 1 do
  begin
    if (lstArquivos.ListItems[i].Components[0] is TFrameArquivo) and
      (lstArquivos.ListItems[i].Tag < 2) then
      AbrirPasta(lstArquivos.ListItems[i].TagString);
  end;
end;

procedure TFormArquivos.FormShow(Sender: TObject);
begin
  GoToDirectory(EFolder.Text);
end;

procedure TFormArquivos.GoToDirectory(ADirectory: string);
begin
  EFolder.Enabled := False;

  if not(ADirectory[Length(ADirectory)] = '\') then
  begin
    ADirectory := ADirectory + '\';
    EFolder.Text := ADirectory;
  end;

  Conexao.SocketPrincipal.Socket.SendText('<|REDIRECT|><|GETFOLDERS|>' +
    ADirectory + '<|END|>');
end;

procedure TFormArquivos.lstArquivosItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
begin
  if Item.Tag < 2 then
    AbrirPasta(Item.TagString);
end;

procedure TFormArquivos.PROC_DOWNLOADExecute(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to lstArquivos.Items.Count - 1 do
  begin
    if (lstArquivos.ListItems[i].Components[0] is TFrameArquivo) and
      (TFrameArquivo(lstArquivos.ListItems[i].Components[0])
      .btnArquivo.IsPressed) and (lstArquivos.ListItems[i].Tag > 1) then
    begin
      SaveDialog1.FileName := TFrameArquivo(lstArquivos.ListItems[i].Components
        [0]).Arquivo.Nome;
      SaveDialog1.Filter := 'Arquivo (*' +
        ExtractFileExt(TFrameArquivo(lstArquivos.ListItems[i].Components[0])
        .Arquivo.Nome) + ')|*' + ExtractFileExt
        (TFrameArquivo(lstArquivos.ListItems[i].Components[0])
        .Arquivo.Extensao);

      if SaveDialog1.Execute then
      begin
        DirectoryToSaveFile := SaveDialog1.FileName +
          ExtractFileExt(TFrameArquivo(lstArquivos.ListItems[i].Components[0])
          .Arquivo.Nome);
        Conexao.SocketPrincipal.Socket.SendText('<|REDIRECT|><|DOWNLOADFILE|>' +
          EFolder.Text + TFrameArquivo(lstArquivos.ListItems[i].Components[0])
          .Arquivo.Nome + '<|END|>');
        btnDownload.Enabled := False;
      end;
      Break;
    end;
  end;
end;

procedure TFormArquivos.PROC_UPLOADExecute(Sender: TObject);
var
  FileName: string;
  Arquivo: TMemoryStream;
begin
  OpenDialog1.FileName := '';

  if OpenDialog1.Execute() then
  begin
    FileStream := TFileStream.Create(OpenDialog1.FileName, fmOpenRead);
    FileName := ExtractFileName(OpenDialog1.FileName);
    pgbUpload.Max := FileStream.Size;
    Conexao.SocketArquivos.Socket.SendText('<|DIRECTORYTOSAVE|>' + EFolder.Text
      + FileName + '<|><|SIZE|>' + intToStr(FileStream.Size) + '<|END|>');
    FileStream.Position := 0;
    Conexao.SocketArquivos.Socket.SendStream(FileStream);
    btnUpload.Enabled := False;
  end;
end;

procedure TFormArquivos.Translate;
begin
  LFolder.Text := Locale.GetLocale(FRMS, 'FileFolder');
  LDownloadProgress.Text := Locale.GetLocale(FRMS, 'FileDownloadProgress');
  LDownloadSize.Text := Format(Locale.GetLocale(APP, 'Size'), ['0 B', '0 B']);
  LUploadProgress.Text := Locale.GetLocale(FRMS, 'FileUploadProgress');
  LUploadSize.Text := Format(Locale.GetLocale(APP, 'Size'), ['0 B', '0 B']);
  btnDownload.Text := Locale.GetLocale(FRMS, 'FileDownloadButton');
  btnUpload.Text := Locale.GetLocale(FRMS, 'FileUploadButton');
end;

procedure TFormArquivos.WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
var
  MinMaxInfo: PMinMaxInfo;
begin
  inherited;
  MinMaxInfo := Message.MinMaxInfo;
  MinMaxInfo^.ptMinTrackSize.X := 515; // Minimum Width
  MinMaxInfo^.ptMinTrackSize.Y := 460; // Minimum Height
end;

end.
