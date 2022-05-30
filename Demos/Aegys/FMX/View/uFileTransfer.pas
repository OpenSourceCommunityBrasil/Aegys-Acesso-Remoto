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
    procedure sbDownloadClick(Sender: TObject);
    procedure sbUploadClick(Sender: TObject);
  private
    { Private declarations }
   vIconsIndex       : TIconsIndex;
   ShellProps        : TShellProps;
   vLastFolder,
   vActiveFolder,
   vDirectory_Local,
   vDirectory_Edit   : String;
   vDestCount        : Integer;
   Procedure  ChangeLocalDir;
   Function   GetIcon      (FileName  : String) : FMX.Graphics.TBitmap;
   Function   GetSize      (Bytes     : Int64): String;
   Procedure  GoToDirectory(Directory : String);
   Procedure  LoadRemoteData;
   Procedure  OnRemoteDblClick    (Sender    : TObject);
  public
    { Public declarations }
   Procedure CarregarListaPastas  (Directory : String);
   Procedure CarregarListaArquivos(Directory : String);
   Property  DestCount : Integer   Read vDestCount;
   Property  Directory_Local : String Read vDirectory_Local;
   Property  ActiveFolder : String Read vActiveFolder;
  end;

var
  fFileTransfer: TfFileTransfer;

implementation

{$R *.fmx}

Uses uCtrl_Threads;

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

Procedure TfFileTransfer.LoadRemoteData;
Begin
 GoToDirectory(vActiveFolder);
End;

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
 SGRemote.Enabled := True;
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
 vFilename,
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
   vLine             := FoldersAndFiles.Strings[i];
   SGRemote.RowCount := SGRemote.RowCount + 1;
   vFilename         := GetValue(vLine);
   SGRemote.Cells[0, SGRemote.RowCount - 1] := ExtractFileExt(vFilename);
   SGRemote.Cells[1, SGRemote.RowCount - 1] := vFilename;
   SGRemote.Cells[2, SGRemote.RowCount - 1] := GetValue(vLine);
   SGRemote.Cells[3, SGRemote.RowCount - 1] := GetValue(vLine);
   SGRemote.Cells[4, SGRemote.RowCount - 1] := GetValue(vLine);
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
   SGRemote.Enabled := False;
   Conexao.SocketPrincipal.Socket.SendText('<|REDIRECT|><|GETFOLDERS|>' + vDirectory_Edit + '<|END_GETFOLDERS|>');
   Application.ProcessMessages;
  End;
End;

procedure TfFileTransfer.ceRemotePathChange(Sender: TObject);
begin
 If ceRemotePath.ItemIndex > -1 Then
  Begin
   vDirectory_Edit := Trim(ceRemotePath.Items[ceRemotePath.ItemIndex]);
   vActiveFolder   := vDirectory_Edit;
   GoToDirectory(vActiveFolder);
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

Procedure TfFileTransfer.OnRemoteDblClick(Sender : TObject);
Var
 vTempFolder : String;
 ARow        : Integer;
begin
 If SGRemote.Selected > -1 Then
  Begin
   ARow := SGRemote.Selected;
   If SGRemote.Cells[0, ARow] = '.' Then
    Begin
     If (SGRemote.Cells[1, ARow] = '..')  Or
        (SGRemote.Cells[1, ARow] = '..\') Then
      Begin
       SGRemote.Enabled := False;
       If Length(vLastFolder) > 0 Then
        vActiveFolder := Copy(vActiveFolder, 1, Length(vActiveFolder) - Length(vLastFolder));
       vTempFolder := vActiveFolder;
       If vTempFolder <> '' Then
        If vTempFolder[Length(vTempFolder)] = '\' Then
         Delete(vTempFolder, Length(vTempFolder), 1);
       Delete(vTempFolder, 1, LastDelimiter('\', vTempFolder));
       If Length(vTempFolder) > 0 Then
        vLastFolder := IncludeTrailingPathDelimiter(vTempFolder);
      End
     Else
      Begin
       vActiveFolder := vActiveFolder + IncludeTrailingPathDelimiter(SGRemote.Cells[1, ARow]);
       vLastFolder := IncludeTrailingPathDelimiter(SGRemote.Cells[1, ARow]);
      End;
     LoadRemoteData;
    End;
  End;
End;

procedure TfFileTransfer.sbDownloadClick(Sender: TObject);
Begin
 If (SGRemote.Selected > -1) Then
  Begin
   vDirectory_Local := vDirectory_Local + SGRemote.Cells[1, SGRemote.Selected];
   ActualDownloadFileName := SGRemote.Cells[1, SGRemote.Selected];
   Conexao.SocketPrincipal.Socket.SendText('<|REDIRECT|><|DOWNLOADFILE|>' +
                                           vDirectory_Edit + SGRemote.Cells[1, SGRemote.Selected] + '<|END|>');
  End;
End;

procedure TfFileTransfer.sbUploadClick(Sender: TObject);
Var
 FileStream : TFileStream;
 FileName   : String;
begin
 If (SGLocal.Selected > -1) Then
  Begin
   vDirectory_Local := vDirectory_Local + SGLocal.Cells[1, SGLocal.Selected];
   ActualDownloadFileName := SGRemote.Cells[1, SGLocal.Selected];
   FileStream := TFileStream.Create(vDirectory_Local, fmOpenRead);
   FileName := ExtractFileName(vDirectory_Local);
   pgbUpload.Max := FileStream.Size;
   Conexao.SocketArquivos.Socket.SendText('<|DIRECTORYTOSAVE|>' + vDirectory_Edit + FileName +
                                          '<|><|SIZE|>' + intToStr(FileStream.Size) + '<|END|>');
   FileStream.Position := 0;
   Conexao.SocketArquivos.Socket.SendStream(FileStream);
  End;
end;

procedure TfFileTransfer.FormCreate(Sender: TObject);
begin
 vActiveFolder               := '';
 vIconsIndex                 := TIconsIndex.Create;
 SGRemote.Enabled            := False;
 ShellProps                  := TShellProps.Create;
 ShellProps.OnAfterChangeDir := ChangeLocalDir;
 SGRemote.OnDblClick         := OnRemoteDblClick;
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
 Conexao.SocketPrincipal.Socket.SendText('<|REDIRECT|><|GETDRIVERS|><|END_GETDRIVERS|>');
 Application.ProcessMessages;
end;

procedure TfFileTransfer.tLoadActionTimer(Sender: TObject);
begin
 tLoadAction.Enabled := False;
 ceRemotePath.OnChange(ceRemotePath);
end;

end.
