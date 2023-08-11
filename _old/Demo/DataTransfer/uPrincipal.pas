unit uPrincipal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uAegysBufferPack, Vcl.StdCtrls,
  Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    eOwner: TEdit;
    eDest: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    rgPackDest: TRadioGroup;
    rgDataCheck: TRadioGroup;
    rgCommandType: TRadioGroup;
    rgDataType: TRadioGroup;
    eValue: TEdit;
    odFile: TOpenDialog;
    Label3: TLabel;
    eBufferSize: TEdit;
    SaveDialog1: TSaveDialog;
    eBytesOptions: TEdit;
    odbuf: TOpenDialog;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    eLoadOwner: TEdit;
    eLoadDest: TEdit;
    rgLoadPackDest: TRadioGroup;
    rgLoadDataCheck: TRadioGroup;
    rgLoadCommandType: TRadioGroup;
    rgLoadDataType: TRadioGroup;
    eLoadValue: TEdit;
    eLoadBufferSize: TEdit;
    eLoadBytesOptions: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure rgDataTypeClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure rgLoadDataTypeClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
   PackList  : TPackList;
  end;

var
  Form1: TForm1;

implementation

Uses uAegysDataTypes, uAegysConsts;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
Var
 aDataType    : TDataType;
 aDataMode    : TDataMode;
 aDataCheck   : TDataCheck;
 aCommandType : TCommandType;
 aStringValue : AeString;
 aBytesValue  : TAegysBytes;
 aFileStream  : TFileStream;
begin
 aCommandType := tctNone;
 If rgDataType.ItemIndex = 0 Then
  aDataType := tdtString
 Else
  aDataType := tdtDataBytes;
 If rgPackDest.ItemIndex = 0 Then
  aDataMode := tdmDirect
 Else
  aDataMode := tdmProxy;
 If rgDataCheck.ItemIndex = 0 Then
  aDataCheck := tdcSync
 Else
  aDataCheck := tdcAsync;
 If aDataType = tdtString Then
  aStringValue := eValue.Text
 Else
  Begin
   Case rgCommandType.ItemIndex Of
    0 : aCommandType := tctScreenCapture;
    1 : aCommandType := tctAudio;
    2 : aCommandType := tctVideo;
    3 : aCommandType := tctKeyboard;
    4 : aCommandType := tctMouse;
    5 : aCommandType := tctFileTransfer;
   End;
  End;
 If aDataType = tdtString Then
  Begin
   aStringValue := eValue.Text;
   PackList.Add(eOwner.Text, eDest.Text,   aDataMode,
                aDataCheck,  aStringValue, StrToInt(eBufferSize.Text));
  End
 Else
  Begin
   If odFile.Execute Then
    Begin
     aFileStream  := TFileStream.Create(odFile.FileName, fmOpenRead);
     eBytesOptions.Text := ExtractFilename(odFile.FileName);
     Try
      aFileStream.Position := 0;
      SetLength(aBytesValue, aFileStream.Size);
      If Length(aBytesValue) > 0 Then
       Begin
        aFileStream.Position := 0;
        aFileStream.Read(aBytesValue[0], aFileStream.Size);
        PackList.Add(eOwner.Text,        eDest.Text,   aDataMode,
                     aDataCheck,         aCommandType, aBytesValue,
                     eBytesOptions.Text, StrToInt(eBufferSize.Text));
       End;
     Finally
      FreeAndNil(aFileStream);
     End;
    End;
  End;
 If SaveDialog1.Execute Then
  Begin
   DeleteFile(SaveDialog1.FileName);
   aBytesValue := PackList.ReadPack(PackList.Count -1);
   If Length(aBytesValue) > 0 Then
    Begin
     aFileStream  := TFileStream.Create(SaveDialog1.FileName, fmCreate);
     Try
      aFileStream.WriteData(aBytesValue, Length(aBytesValue));
     Finally
      aFileStream.Position := 0;
      FreeAndNil(aFileStream);
     End;
    End;
  End
 Else
  Raise Exception.Create('Cancelado pelo Usuário...');
end;

procedure TForm1.Button2Click(Sender: TObject);
Var
 aDataType    : TDataType;
 aDataMode    : TDataMode;
 aDataCheck   : TDataCheck;
 aCommandType : TCommandType;
 aStringValue : AeString;
 aBytesValue  : TAegysBytes;
 aFileStream,
 aFileStreamB : TFileStream;
begin
 If odbuf.Execute Then
  Begin
   aFileStream := TFileStream.Create(odbuf.FileName, fmOpenRead);
   If aFileStream.Size > 0 Then
    Begin
     aFileStream.Position := 0;
     SetLength(aBytesValue, aFileStream.Size);
     aFileStream.Read(aBytesValue[0], aFileStream.Size);
     Try
      PackList.Add(aBytesValue);
      aCommandType         := PackList[PackList.Count -1].CommandType;
      aDataType            := PackList[PackList.Count -1].DataType;
      aDataMode            := PackList[PackList.Count -1].DataMode;
      aDataCheck           := PackList[PackList.Count -1].DataCheck;
      aDataType            := PackList[PackList.Count -1].DataType;
      eLoadValue.Text      := PackList[PackList.Count -1].Command;
      eLoadOwner.Text      := PackList[PackList.Count -1].Owner;
      eLoadDest.Text       := PackList[PackList.Count -1].Dest;
      eLoadBufferSize.Text := IntToStr(PackList[PackList.Count -1].BufferSize);
      If aDataType = tdtString Then
       rgLoadDataType.ItemIndex := 0
      Else
       rgLoadDataType.ItemIndex := 1;
      If aDataMode = tdmDirect Then
       rgLoadPackDest.ItemIndex := 0
      Else
       rgLoadPackDest.ItemIndex := 1;
      If aDataCheck = tdcSync Then
       rgLoadDataCheck.ItemIndex := 0
      Else
       rgLoadDataCheck.ItemIndex := 1;
      If aDataType = tdtString Then
       eLoadValue.Text := PackList[PackList.Count -1].Command
      Else
       Begin
        Case aCommandType Of
         tctScreenCapture : rgLoadCommandType.ItemIndex := 0;
         tctAudio         : rgLoadCommandType.ItemIndex := 1;
         tctVideo         : rgLoadCommandType.ItemIndex := 2;
         tctKeyboard      : rgLoadCommandType.ItemIndex := 3;
         tctMouse         : rgLoadCommandType.ItemIndex := 4;
         tctFileTransfer  : rgLoadCommandType.ItemIndex := 5;
        End;
        eLoadBytesOptions.Text := PackList[PackList.Count -1].BytesOptions;
        If eLoadBytesOptions.Text <> '' Then
         Begin
          aFileStreamB := tFileStream.Create('.\' + eLoadBytesOptions.Text, fmCreate);
          Try
           aBytesValue := PackList[PackList.Count -1].DataBytes;
           aFileStreamB.Write(aBytesValue[0], Length(aBytesValue));
          Finally
           aFileStreamB.Position := 0;
           FreeAndNil(aFileStreamB);
          End;
         End;
       End;
     Finally
      SetLength(aBytesValue, 0);
      FreeAndNil(aFileStream);
     End;
    End
   Else
    Raise Exception.Create(cInvalidBufferData);
  End;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 PackList.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 PackList := TPackList.Create;
end;

procedure TForm1.rgDataTypeClick(Sender: TObject);
begin
 eValue.Visible        := rgDataType.ItemIndex = 0;
 rgCommandType.Visible := Not eValue.Visible;
 eBytesOptions.Visible := rgCommandType.Visible;
end;

procedure TForm1.rgLoadDataTypeClick(Sender: TObject);
begin
 eLoadValue.Visible        := rgLoadDataType.ItemIndex = 0;
 rgLoadCommandType.Visible := Not eLoadValue.Visible;
 eLoadBytesOptions.Visible := rgLoadCommandType.Visible;
end;

end.
