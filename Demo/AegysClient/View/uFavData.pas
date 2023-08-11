unit uFavData;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, uFavoritos,
  FMX.Edit, FMX.Controls.Presentation, FMX.StdCtrls, System.ImageList,
  FMX.ImgList;

Const
 cTitleAction = 'Dados de Favoritos[%s]';

type
 TFavAction = (faInsert, faEdit, faDelete);
  TfFavData = class(TForm)
    Label1: TLabel;
    eConnectionID: TEdit;
    eConnectionName: TEdit;
    Label2: TLabel;
    eConnectionAlias: TEdit;
    Label3: TLabel;
    eConnectionGroup: TEdit;
    Label4: TLabel;
    cbRemoteAssist: TCheckBox;
    eConnectionPass: TEdit;
    Label5: TLabel;
    sbPost: TSpeedButton;
    ImageList1: TImageList;
    SpeedButton1: TSpeedButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure sbPostClick(Sender: TObject);
    procedure cbRemoteAssistChange(Sender: TObject);
  private
    { Private declarations }
   vFavAction : TFavAction;
   pPanelData : ^TPanelClass;
   vOldID     : String;
  public
    { Public declarations }
   Procedure Insert;
   Procedure Edit  (Var aPanelData : TPanelClass);
   Procedure Delete(Var aPanelData : TPanelClass);
  end;

var
  fFavData: TfFavData;

implementation

{$R *.fmx}

Uses uAegysConsts, uAegysBufferPack, uAegysDataTypes, uAegysTools, uFormConexao;

{ TfFavData }

procedure TfFavData.cbRemoteAssistChange(Sender: TObject);
begin
 eConnectionPass.Enabled := cbRemoteAssist.IsChecked;
 If Not eConnectionPass.Enabled Then
  eConnectionPass.Text := '';
end;

Procedure TfFavData.Delete(Var aPanelData : TPanelClass);
Begin
 vFavAction               := faDelete;
 vOldID                   := aPanelData.ID;
 Caption := Format(cTitleAction, ['Deleção']);
End;

Procedure TfFavData.Edit  (Var aPanelData : TPanelClass);
Begin
 vFavAction               := faEdit;
 pPanelData               := @aPanelData;
 vOldID                   := pPanelData^.ID;
 eConnectionID.Text       := pPanelData^.ID;
 eConnectionName.Text     := pPanelData^.ConnectionName;
 eConnectionAlias.Text    := pPanelData^.Alias;
 eConnectionGroup.Text    := pPanelData^.Grupo;
 cbRemoteAssist.IsChecked := pPanelData^.RemoteAssist;
 eConnectionPass.Enabled := cbRemoteAssist.IsChecked;
 If Not cbRemoteAssist.IsChecked Then
  eConnectionPass.Text := '';
 eConnectionPass.Text     := pPanelData^.Password;
 Caption                  := Format(cTitleAction, ['Edição']);
 Show;
End;

procedure TfFavData.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 fFavData := Nil;
 Release;
end;

procedure TfFavData.FormCreate(Sender: TObject);
begin
 pPanelData               := Nil;
end;

Procedure TfFavData.Insert;
Begin
 vFavAction               := faInsert;
 eConnectionID.Text       := '';
 eConnectionName.Text     := '';
 eConnectionAlias.Text    := '';
 eConnectionGroup.Text    := '';
 cbRemoteAssist.IsChecked := False;
 eConnectionPass.Text     := '';
 Caption := Format(cTitleAction, ['Inserção']);
 Show;
End;

procedure TfFavData.sbPostClick(Sender: TObject);
Var
 aPackClass : TPackClass;
 aBuf       : TAegysBytes;
 vData      : String;
begin
 Case vFavAction Of
  faInsert : Begin
              aPackClass                 := TPackClass.Create;
              Try
               aPackClass.DataMode  := tdmServerCommand;
               aPackClass.DataCheck := tdcAsync;
               vData                := Format('%s&%s&%s&%s&%d&%s', [EncodeStrings(eConnectionID.Text),
                                                                    EncodeStrings(eConnectionName.Text),
                                                                    EncodeStrings(eConnectionAlias.Text),
                                                                    EncodeStrings(eConnectionGroup.Text),
                                                                    Integer(cbRemoteAssist.IsChecked),
                                                                    EncodeStrings(eConnectionPass.Text)]);
               aPackClass.Command   := cPeerService + Format('%s%s&%s', [cInsertPeer, eConnectionID.Text, vData]);
               aBuf                 := aPackClass.ToBytes;
               Conexao.SendBytes(aBuf);
              Finally
               SetLength(aBuf, 0);
               FreeAndNil(aPackClass);
              End;
             End;
  faEdit   : Begin
              pPanelData^.ID             := eConnectionID.Text;
              pPanelData^.ConnectionName := eConnectionName.Text;
              pPanelData^.Alias          := eConnectionAlias.Text;
              pPanelData^.Grupo          := eConnectionGroup.Text;
              pPanelData^.RemoteAssist   := cbRemoteAssist.IsChecked;
              pPanelData^.Password       := eConnectionPass.Text;
              aPackClass                 := TPackClass.Create;
              Try
               aPackClass.DataMode  := tdmServerCommand;
               aPackClass.DataCheck := tdcAsync;
               vData                := Format('%s&%s&%s&%s&%d&%s', [EncodeStrings(pPanelData^.ID),
                                                                    EncodeStrings(pPanelData^.ConnectionName),
                                                                    EncodeStrings(pPanelData^.Alias),
                                                                    EncodeStrings(pPanelData^.Grupo),
                                                                    Integer(pPanelData^.RemoteAssist),
                                                                    EncodeStrings(pPanelData^.Password)]);
               aPackClass.Command   := cPeerService + Format('%s%s&%s', [cEditPeer, vOldID, vData]);
               aBuf                 := aPackClass.ToBytes;
               Conexao.SendBytes(aBuf);
              Finally
               SetLength(aBuf, 0);
               FreeAndNil(aPackClass);
              End;
             End;
  faDelete : Begin
              aPackClass                 := TPackClass.Create;
              Try
               aPackClass.DataMode  := tdmServerCommand;
               aPackClass.DataCheck := tdcAsync;
               aPackClass.Command   := cPeerService + Format('%s%s', [cDeletePeer, vOldID]);
               aBuf                 := aPackClass.ToBytes;
               Conexao.SendBytes(aBuf);
              Finally
               SetLength(aBuf, 0);
               FreeAndNil(aPackClass);
              End;
             End;
 End;
 Close;
end;

procedure TfFavData.SpeedButton1Click(Sender: TObject);
begin
 Close;
end;

End.
