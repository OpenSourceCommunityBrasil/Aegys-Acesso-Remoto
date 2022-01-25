unit uFileTransfer;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, Vcl.Graphics, FMX.Dialogs,
  System.Rtti,
  FMX.Grid.Style, FMX.Bind.GenData, Data.Bind.GenData, Data.Bind.EngExt,
  FMX.Bind.DBEngExt, System.Bindings.Outputs, FMX.Bind.Editors,
  Data.Bind.Components, Data.Bind.ObjectScope, FMX.Grid,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Objects, FMX.StdCtrls,
  FMX.Bind.Grid, Data.Bind.Grid, System.ImageList, FMX.ImgList, FMX.Edit,
  FMX.ComboEdit, FMX.Layouts, uFilesFoldersOP, uIconsAssoc, FMX.MultiResBitmap
{$IFDEF WIN32}, WinApi.Windows, FMX.Platform.Win{$ENDIF};

Type
  TIconIndex = Packed Record
    Index: Integer;
    Extension: String;
  End;

Type
  TIconsIndex = Tlist;

type
  TfFileTransfer = class(TForm)
    lLeituraDados: TLayout;
    lCabecalhoLeit: TLayout;
    SGLocal: TStringGrid;
    gcLeitura: TGlyphColumn;
    scNomeArqLeitura: TStringColumn;
    scFileSizeLeitura: TStringColumn;
    lLeituraDestino: TLayout;
    lCabecalhoDest: TLayout;
    SGRemote: TStringGrid;
    gcDestino: TGlyphColumn;
    scNomeArqDestino: TStringColumn;
    scTipoItemDestino: TStringColumn;
    cbLocalDrivers: TComboEdit;
    ceRemotePath: TComboEdit;
    sbUpload: TSpeedButton;
    sbDownload: TSpeedButton;
    scTipoItemLeitura: TStringColumn;
    scDataHoraLeitura: TStringColumn;
    scDataHoraDestino: TStringColumn;
    scFileSizeDestino: TStringColumn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lNomeComputadorLocal: TLabel;
    lPCRemoto: TLabel;
    lBottomPage: TLayout;
    Line1: TLine;
    Layout6: TLayout;
    Layout8: TLayout;
    Layout9: TLayout;
    LDownloadProgress: TLabel;
    pgbDownload: TProgressBar;
    LDownloadSize: TLabel;
    Layout7: TLayout;
    Layout10: TLayout;
    LUploadProgress: TLabel;
    pgbUpload: TProgressBar;
    LUploadSize: TLabel;
    ilimagens: TImageList;
    lyButtons: TLayout;
    FlowLayout1: TFlowLayout;
    procedure SGLocalDrawColumnCell(Sender: TObject;
      const Canvas: FMX.Graphics.TCanvas; const Column: TColumn;
      const Bounds: TRectF; const Row: Integer; const Value: TValue;
      const State: TGridDrawStates);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbLocalDriversChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SGLocalCellClick(const Column: TColumn; const Row: Integer);
    procedure SGRemoteCellClick(const Column: TColumn; const Row: Integer);
    procedure SGLocalCellDblClick(const Column: TColumn; const Row: Integer);
    procedure SGRemoteCellDblClick(const Column: TColumn; const Row: Integer);
  private
    { Private declarations }
    vIconsIndex: TIconsIndex;
    ShellProps: TShellProps;
    vDirectory_Local, vDirectory_Edit: String;
    Procedure ChangeLocalDir;
    Function GetIcon(FileName: String): FMX.Graphics.TBitmap;
    Function GetSize(Bytes: Int64): String;
  public
    { Public declarations }
  end;

var
  fFileTransfer: TfFileTransfer;

implementation

{$R *.fmx}

Function TfFileTransfer.GetIcon(FileName: String): FMX.Graphics.TBitmap;
Var
  Icon: TIcon;
  FileExt: String;
  SmallIcon: HICON;
  Stream: TMemoryStream;
Begin
  Result := Nil;
  FileExt := UpperCase(ExtractFileExt(FileName));
  Try
    GetAssociatedIcon(FileName, @SmallIcon);
    If SmallIcon <> 0 Then
    Begin
      Result := FMX.Graphics.TBitmap.Create;
      Icon := TIcon.Create;
      Icon.Handle := SmallIcon;
      Stream := TMemoryStream.Create;
      Icon.SaveToStream(Stream);
      Stream.Position := 0;
      Result.LoadFromStream(Stream);
      Icon.Free;
      Stream.Free;
    End;
  Finally
  End;
End;

Function TfFileTransfer.GetSize(Bytes: Int64): String;
Const
  K = Int64(1024);
  M = K * K;
  G = K * M;
  T = K * G;
Begin
  If Bytes < K Then
    Result := Format('%d B', [Bytes])
  Else If Bytes < M Then
    Result := Format('%f KB', [Bytes / K])
  Else If Bytes < G Then
    Result := Format('%f MB', [Bytes / M])
  Else If Bytes < T Then
    Result := Format('%f GB', [Bytes / G])
  Else
    Result := Format('%f TB', [Bytes / T]);
End;

procedure TfFileTransfer.cbLocalDriversChange(Sender: TObject);
begin
  If cbLocalDrivers.ItemIndex > -1 Then
  Begin
    ShellProps.Folder := Trim(cbLocalDrivers.Items[cbLocalDrivers.ItemIndex]);
    vDirectory_Local := ShellProps.Folder;
  End;
end;

Procedure TfFileTransfer.ChangeLocalDir;
Var
  I: Integer;
Begin
  SGLocal.RowCount := 1;
  SGLocal.Cells[0, SGLocal.RowCount - 1] := '';
  SGLocal.Cells[1, SGLocal.RowCount - 1] := '...';
  SGLocal.Cells[2, SGLocal.RowCount - 1] := '';
  SGLocal.Cells[3, SGLocal.RowCount - 1] := '';
  For I := 0 To ShellProps.FilesCount - 1 do
  Begin
    If (SGLocal.RowCount <> 1) Or
      ((SGLocal.RowCount = 1) And (SGLocal.Cells[1, SGLocal.RowCount - 1] <>
      '...')) Then
      SGLocal.RowCount := SGLocal.RowCount + 1;
    SGLocal.Cells[1, SGLocal.RowCount - 1] := ShellProps.Files[I].FileName;
    If ShellProps.Files[I].FileType <> fpDir Then
    Begin
      SGLocal.Cells[2, SGLocal.RowCount - 1] :=
        GetSize(ShellProps.Files[I].FileSize);
      SGLocal.Cells[3, SGLocal.RowCount - 1] := ShellProps.Files[I]
        .FileTypeDesc;
      SGLocal.Cells[4, SGLocal.RowCount - 1] :=
        FormatDateTime('dd/mm/yyyy hh:mm:ss', ShellProps.Files[I].LastWrite);
    End;
  End;
End;

procedure TfFileTransfer.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
  // fFileTransfer := Nil;
  // Release;
end;

procedure TfFileTransfer.FormCreate(Sender: TObject);
begin
  vIconsIndex := TIconsIndex.Create;
  ShellProps := TShellProps.Create;
  ShellProps.OnAfterChangeDir := ChangeLocalDir;
end;

procedure TfFileTransfer.FormShow(Sender: TObject);
Var
  I: Integer;
Begin
  cbLocalDrivers.Items.Clear;
  For I := 0 To ShellProps.Drivers.Count - 1 do
    cbLocalDrivers.Items.Add(' ' + ShellProps.Drivers[I]);
  If cbLocalDrivers.Items.Count > 0 Then
  Begin
    cbLocalDrivers.ItemIndex := 0;
    cbLocalDrivers.OnChange(cbLocalDrivers);
  End;
  lNomeComputadorLocal.Text := ShellProps.LocalStation;
end;
(*
  onCellClick e onCellDblClick são concorrentes, se ativar o onCellClick,
  muito provavelmente não vai conseguir disparar o onCellDblClick se houver algum
  diálogo modal ou então os 2 serão disparados simultaneamente se não houver
  diálogo modal.
*)
procedure TfFileTransfer.SGLocalCellClick(const Column: TColumn;
  const Row: Integer);
begin
//  ShowMessage('cliquei 1x em: ' + SGLocal.Cells[Column.Index, Row]);
end;

procedure TfFileTransfer.SGLocalCellDblClick(const Column: TColumn;
  const Row: Integer);
begin
//  ShowMessage('cliquei 2x em: ' + SGLocal.Cells[Column.Index, Row]);
end;

procedure TfFileTransfer.SGLocalDrawColumnCell(Sender: TObject;
  const Canvas: FMX.Graphics.TCanvas; const Column: TColumn;
  const Bounds: TRectF; const Row: Integer; const Value: TValue;
  const State: TGridDrawStates);
Var
  vImage, sbitmap: FMX.Graphics.TBitmap;
begin
  If (Column = gcLeitura) then
  Begin
    vImage := GetIcon(ShellProps.Folder + ShellProps.Files[Row].FileName);
    If (vImage <> Nil) Then
    Begin
      Canvas.DrawBitmap(vImage, vImage.Bounds, Bounds, 1);
      FreeAndNil(vImage);
    End
    Else If (vImage = Nil) And (ShellProps.Files[Row].FileType = fpDir) Then
    Begin
      sbitmap := ilimagens.Source.Items[0].MultiResBitmap[0].Bitmap;
      Canvas.DrawBitmap(sbitmap, sbitmap.Bounds, Bounds, 1);
    End
    Else If (vImage = Nil) And (ShellProps.Files[Row].FileType = fpFile) Then
    Begin
      sbitmap := ilimagens.Source.Items[1].MultiResBitmap[0].Bitmap;
      Canvas.DrawBitmap(sbitmap, sbitmap.Bounds, Bounds, 1);
    End
    Else If (vImage = Nil) And
      (ShellProps.Files[Row].FileType = fpShortcut) Then
    Begin
      sbitmap := ilimagens.Source.Items[2].MultiResBitmap[0].Bitmap;
      Canvas.DrawBitmap(sbitmap, sbitmap.Bounds, Bounds, 1);
    End
    Else If (vImage = Nil) And (ShellProps.Files[Row].FileType = fpDriver) Then
    Begin
      sbitmap := ilimagens.Source.Items[3].MultiResBitmap[0].Bitmap;
      Canvas.DrawBitmap(sbitmap, sbitmap.Bounds, Bounds, 1);
    End;
  End;
end;

procedure TfFileTransfer.SGRemoteCellClick(const Column: TColumn;
  const Row: Integer);
begin
//  ShowMessage('cliquei 1x em: ' + SGRemote.Cells[Column.Index, Row]);
end;

procedure TfFileTransfer.SGRemoteCellDblClick(const Column: TColumn;
  const Row: Integer);
begin
//  ShowMessage('cliquei 1x em: ' + SGRemote.Cells[Column.Index, Row]);
end;

end.
