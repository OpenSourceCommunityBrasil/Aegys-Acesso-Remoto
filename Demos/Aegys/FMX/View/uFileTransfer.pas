unit uFileTransfer;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, Vcl.Graphics, FMX.Dialogs, System.Rtti,
  FMX.Grid.Style, FMX.Bind.GenData, Data.Bind.GenData, Data.Bind.EngExt,
  FMX.Bind.DBEngExt, System.Bindings.Outputs, FMX.Bind.Editors,
  Data.Bind.Components, Data.Bind.ObjectScope, FMX.Grid,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Objects, FMX.StdCtrls,
  FMX.Bind.Grid, Data.Bind.Grid, System.ImageList, FMX.ImgList, FMX.Edit,
  FMX.ComboEdit, FMX.Layouts, uFilesFoldersOP, uIconsAssoc, FMX.MultiResBitmap
  {$IFDEF WIN32},WinApi.Windows, FMX.Platform.Win{$ENDIF}, uFormConexao;

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
    lBottonPage: TLayout;
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
    TActiveLoad: TTimer;
    tLoadAction: TTimer;
    procedure SGLocalDrawColumnCell(Sender: TObject; const Canvas: FMX.Graphics.TCanvas;
      const Column: TColumn; const Bounds: TRectF; const Row: Integer;
      const Value: TValue; const State: TGridDrawStates);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbLocalDriversChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TActiveLoadTimer(Sender: TObject);
    procedure ceRemotePathChange(Sender: TObject);
    procedure SGRemoteDrawColumnCell(Sender: TObject; const Canvas: FMX.Graphics.TCanvas;
      const Column: TColumn; const Bounds: TRectF; const Row: Integer;
      const Value: TValue; const State: TGridDrawStates);
    procedure tLoadActionTimer(Sender: TObject);
  private
    { Private declarations }
   vIconsIndex       : TIconsIndex;
   ShellProps        : TShellProps;
   vActiveFolder,
   vDirectory_Local,
   vDirectory_Edit   : String;
   vDestCount        : Integer;
   Procedure  ChangeLocalDir;
   Function   GetIcon      (FileName  : String) : FMX.Graphics.TBitmap;
   Function   GetSize      (Bytes     : Int64): String;
   Procedure  GoToDirectory(Directory : String);
  public
    { Public declarations }
   Procedure CarregarListaPastas  (Directory : String);
   Procedure CarregarListaArquivos(Directory : String);
   Property  DestCount : Integer   Read vDestCount;
   Property  ActiveFolder : String Read vActiveFolder;
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
 Stream    : TMemoryStream;
Begin
 Result := Nil;
 FileExt := UpperCase(ExtractFileExt(FileName));
 If FileExt = '' Then
  FileExt := FileName;
 Try
  GetAssociatedIcon(FileName, @SmallIcon);
  If SmallIcon = 0 Then
   GetAssociatedIcon(FileExt, @SmallIcon);
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

Function TfFileTransfer.GetSize(Bytes : Int64) : String;
Const
 K = Int64(1024);
 M = K * K;
 G = K * M;
 T = K * G;
Begin
      If Bytes < K Then Result := Format('%d B',  [Bytes])
 Else If Bytes < M Then Result := Format('%f KB', [Bytes / K])
 Else If Bytes < G Then Result := Format('%f MB', [Bytes / M])
 Else If Bytes < T Then Result := Format('%f GB', [Bytes / G])
 Else                   Result := Format('%f TB', [Bytes / T]);
End;

procedure TfFileTransfer.cbLocalDriversChange(Sender: TObject);
begin
 If cbLocalDrivers.ItemIndex > -1 Then
  Begin
   ShellProps.Folder := Trim(cbLocalDrivers.Items[cbLocalDrivers.ItemIndex]);
   Try
    vDirectory_Local := ShellProps.Folder;
   Except
   End;
  End;
end;

procedure TfFileTransfer.CarregarListaPastas(Directory : String);
Var
 I               : Integer;
 FoldersAndFiles : TStringList;
Begin
 vDestCount      := 0;
 FoldersAndFiles := TStringList.Create;
 FoldersAndFiles.Text := Directory;
 SGRemote.RowCount := 0;
 SGRemote.RowCount := 1;
 If FoldersAndFiles.Count = 0 Then
  Begin
   SGRemote.Cells[0, SGRemote.RowCount - 1] := '';
   SGRemote.Cells[1, SGRemote.RowCount - 1] := '...';
   SGRemote.Cells[2, SGRemote.RowCount - 1] := '';
   SGRemote.Cells[2, SGRemote.RowCount - 1] := '';
   SGRemote.Cells[3, SGRemote.RowCount - 1] := '';
  End;
 For I := 0 To FoldersAndFiles.Count - 1 do
  Begin
   If (FoldersAndFiles.Strings[i] = '.') Or
      (FoldersAndFiles.Strings[i] = '')  Then
    Continue;
   If Not((SGRemote.Cells[1, SGRemote.RowCount - 1] = '')     Or
          (SGRemote.Cells[1, SGRemote.RowCount - 1] = '...')) Then
    SGRemote.RowCount := SGRemote.RowCount + 1;
   SGRemote.Cells[1, SGRemote.RowCount - 1] := FoldersAndFiles.Strings[i];
   SGRemote.Cells[0, SGRemote.RowCount - 1] := '.';
   Inc(vDestCount);
  End;
 FoldersAndFiles.Free;
End;

procedure TfFileTransfer.CarregarListaArquivos(Directory : String);
Var
 I               : Integer;
 vLine           : String;
 FoldersAndFiles : TStringList;
 Function GetValue(Var Value : String) : String;
 Begin
  If Pos('|', Value) > 0 Then
   Begin
    Result := Copy(Value, 1, Pos('|', Value) -1);
    Delete(Value, 1, Pos('|', Value));
   End
  Else
   Begin
    Result := Copy(Value, 1, Length(Value));
    Delete(Value, 1, Length(Value));
   End;
 End;
Begin
 FoldersAndFiles := TStringList.Create;
 FoldersAndFiles.Text := Directory;
 For I := 0 To FoldersAndFiles.Count - 1 do
  Begin
   If (FoldersAndFiles.Strings[i] = '.')  Or
      (FoldersAndFiles.Strings[i] = '..') Then
    Continue;
   vLine        := FoldersAndFiles.Strings[i];
   SGRemote.RowCount := SGRemote.RowCount + 1;
   SGRemote.Cells[0, SGRemote.RowCount - 1] := ExtractFileExt(vLine);
   SGRemote.Cells[1, SGRemote.RowCount - 1] := GetValue(vLine);
   SGRemote.Cells[2, SGRemote.RowCount - 1] := GetSize(StrToInt(GetValue(vLine)));
   SGRemote.Cells[3, SGRemote.RowCount - 1] := GetFileTypeDescription(GetValue(vLine));
   SGRemote.Cells[4, SGRemote.RowCount - 1] := FormatDateTime('dd/mm/yyyy hh:mm:ss', StrToDateTime(GetValue(vLine)));
   Inc(vDestCount);
  End;
 FoldersAndFiles.Free;
End;

procedure TfFileTransfer.GoToDirectory(Directory : String);
Begin
 If Length(Directory) > 0 Then
  Begin
   If Not (Directory[Length(Directory)] = '\') Then
    Directory := Directory + '\';
   vDirectory_Edit := Directory;
   TThread.CreateAnonymousThread(Procedure
                                 Begin
                                  Conexao.SocketPrincipal.Socket.SendText('<|REDIRECT|><|GETFOLDERS|>' + vDirectory_Edit + '<|END_GETFOLDERS|>');
                                 End).Start;
   Application.ProcessMessages;
  End;
End;

procedure TfFileTransfer.ceRemotePathChange(Sender: TObject);
begin
 If ceRemotePath.ItemIndex > -1 Then
  Begin
   vDirectory_Edit := Trim(ceRemotePath.Items[ceRemotePath.ItemIndex]);
   vActiveFolder   := vDirectory_Edit;
   GoToDirectory(vDirectory_Edit);
  End;
end;

Procedure TfFileTransfer.ChangeLocalDir;
Var
 I : Integer;
Begin
 SGLocal.RowCount := 1;
 SGLocal.Cells[0, SGLocal.RowCount - 1] := '';
 SGLocal.Cells[1, SGLocal.RowCount - 1] := '...';
 SGLocal.Cells[2, SGLocal.RowCount - 1] := '';
 SGLocal.Cells[2, SGLocal.RowCount - 1] := '';
 SGLocal.Cells[3, SGLocal.RowCount - 1] := '';
 For I := 0 To ShellProps.FilesCount - 1 do
  Begin
   If (SGLocal.RowCount <> 1) Or
      ((SGLocal.RowCount = 1)  And (SGLocal.Cells[1, SGLocal.RowCount - 1] <> '...')) Then
    SGLocal.RowCount := SGLocal.RowCount + 1;
   SGLocal.Cells[1, SGLocal.RowCount - 1] := ShellProps.Files[I].FileName;
   If ShellProps.Files[I].FileType <> fpDir Then
    Begin
     SGLocal.Cells[2, SGLocal.RowCount - 1] := GetSize(ShellProps.Files[I].FileSize);
     SGLocal.Cells[3, SGLocal.RowCount - 1] := ShellProps.Files[I].FileTypeDesc;
     SGLocal.Cells[4, SGLocal.RowCount - 1] := FormatDateTime('dd/mm/yyyy hh:mm:ss', ShellProps.Files[I].LastWrite);
    End;
  End;
End;

procedure TfFileTransfer.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 fFileTransfer := Nil;
 Release;
end;

procedure TfFileTransfer.FormCreate(Sender: TObject);
begin
 vActiveFolder := '';
 vIconsIndex   := TIconsIndex.Create;
 ShellProps    := TShellProps.Create;
 ShellProps.OnAfterChangeDir := ChangeLocalDir;
end;

procedure TfFileTransfer.FormShow(Sender: TObject);
Var
 I : Integer;
Begin
 cbLocalDrivers.Items.Clear;
 For I := 0 To ShellProps.Drivers.Count - 1 do
  cbLocalDrivers.Items.Add(' ' + ShellProps.Drivers[I]);
 TActiveLoad.Enabled := True;
end;

procedure TfFileTransfer.SGLocalDrawColumnCell(Sender: TObject;
  const Canvas: FMX.Graphics.TCanvas; const Column: TColumn; const Bounds: TRectF;
  const Row: Integer; const Value: TValue; const State: TGridDrawStates);
Var
 vImage,
 sbitmap     : FMX.Graphics.TBitmap;
begin
 If (Column = gcLeitura) then
  Begin
   vImage := GetIcon(ShellProps.Folder + ShellProps.Files[Row].FileName);
   If (vImage <> Nil) Then
    Begin
     Canvas.DrawBitmap(vImage, vImage.Bounds, Bounds, 1);
     FreeAndNil(vImage);
    End
   Else If (vImage = Nil) And
      (ShellProps.Files[Row].FileType = fpDir) Then
    Begin
     sbitmap     := ilimagens.Source.Items[0].MultiResBitmap[0].Bitmap;
     Canvas.DrawBitmap(sbitmap, sbitmap.Bounds, Bounds, 1);
    End
   Else If (vImage = Nil) And
           (ShellProps.Files[Row].FileType = fpFile) Then
    Begin
     sbitmap     := ilimagens.Source.Items[1].MultiResBitmap[0].Bitmap;
     Canvas.DrawBitmap(sbitmap, sbitmap.Bounds, Bounds, 1);
    End
   Else If (vImage = Nil) And
           (ShellProps.Files[Row].FileType = fpShortcut) Then
    Begin
     sbitmap     := ilimagens.Source.Items[2].MultiResBitmap[0].Bitmap;
     Canvas.DrawBitmap(sbitmap, sbitmap.Bounds, Bounds, 1);
    End
   Else If (vImage = Nil) And
           (ShellProps.Files[Row].FileType = fpDriver) Then
    Begin
     sbitmap     := ilimagens.Source.Items[3].MultiResBitmap[0].Bitmap;
     Canvas.DrawBitmap(sbitmap, sbitmap.Bounds, Bounds, 1);
    End;
  End;
end;

procedure TfFileTransfer.SGRemoteDrawColumnCell(Sender: TObject;
  const Canvas: FMX.Graphics.TCanvas; const Column: TColumn; const Bounds: TRectF;
  const Row: Integer; const Value: TValue; const State: TGridDrawStates);
Var
 vImage,
 sbitmap     : FMX.Graphics.TBitmap;
begin
 If (Column = gcDestino) then
  Begin
   vImage := Nil;
   If Value.AsString <> '.' Then
    vImage := GetIcon(Value.AsString);
   If (vImage <> Nil) Then
    Begin
     Canvas.DrawBitmap(vImage, vImage.Bounds, Bounds, 1);
     FreeAndNil(vImage);
    End
   Else If (vImage = Nil)          And
           (Value.AsString = '.')  Then
    Begin
     sbitmap     := ilimagens.Source.Items[0].MultiResBitmap[0].Bitmap;
     Canvas.DrawBitmap(sbitmap, sbitmap.Bounds, Bounds, 1);
    End
   Else If (vImage = Nil) And
           (Value.AsString = 'f')  Then
    Begin
     sbitmap     := ilimagens.Source.Items[1].MultiResBitmap[0].Bitmap;
     Canvas.DrawBitmap(sbitmap, sbitmap.Bounds, Bounds, 1);
    End
   Else If (vImage = Nil)          And
           (Value.AsString = 'a')  Then
    Begin
     sbitmap     := ilimagens.Source.Items[2].MultiResBitmap[0].Bitmap;
     Canvas.DrawBitmap(sbitmap, sbitmap.Bounds, Bounds, 1);
    End
   Else If (vImage = Nil)          And
           (Value.AsString = 'd')  Then
    Begin
     sbitmap     := ilimagens.Source.Items[3].MultiResBitmap[0].Bitmap;
     Canvas.DrawBitmap(sbitmap, sbitmap.Bounds, Bounds, 1);
    End;
  End;
end;

procedure TfFileTransfer.TActiveLoadTimer(Sender: TObject);
begin
 TActiveLoad.Enabled := False;
 If cbLocalDrivers.Items.Count > 0 Then
  Begin
   cbLocalDrivers.ItemIndex := 0;
   cbLocalDrivers.OnChange(cbLocalDrivers);
  End;
 lNomeComputadorLocal.Text := ShellProps.LocalStation;
 Conexao.SocketPrincipal.Socket.SendText('<|REDIRECT|><|GETDRIVERS|><|END|>');
 Application.ProcessMessages;
end;

procedure TfFileTransfer.tLoadActionTimer(Sender: TObject);
begin
 tLoadAction.Enabled := False;
 ceRemotePath.OnChange(ceRemotePath);
end;

end.
