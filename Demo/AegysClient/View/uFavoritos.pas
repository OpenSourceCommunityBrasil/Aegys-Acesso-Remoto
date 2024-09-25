unit uFavoritos;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.TreeView, FMX.Edit,
  FMX.ExtCtrls, FMX.Objects, System.ImageList, FMX.ImgList, FMX.Menus, uAegysBase;

Const
 cAllSelect = '[Todos]';

Type
 TPanelClass = Class;
 TPanelActionOnClick = Procedure(aPanel : TPanelClass) Of Object;
 TPanelClass = Class
 Private
  vPanelNum,
  vHeight,
  vWidth,
  vTop,
  vLeft          : Integer;
  pDados         : TPanel;
  ivDados        : TRectangle;
  vActionOnClick : TPanelActionOnClick;
  aLabActive,
  aLabID,
  aLabIDValue,
  aLabGrupo,
  aLabGrupoValue,
  aLabAlias,
  aLabAliasValue : TLabel;
  vActive,
  vRemoteAssist,
  vVisible       : Boolean;
  vID,
  vGrupo,
  vAlias,
  vConnectionName,
  vConnectionString,
  vPassword,
  vLastShot      : String;
  Procedure SetTop     (aTop     : Integer);
  Procedure SetLeft    (aLeft    : Integer);
  Procedure SetHeight  (aHeight  : Integer);
  Procedure SetWidth   (aWidth   : Integer);
  Procedure SetVisible (aVisible : Boolean);
  Procedure SetActive  (aActive  : Boolean);
  Procedure SetID      (aValue   : String);
  Procedure SetAlias   (aValue   : String);
  Procedure SetGrupo   (aValue   : String);
  Procedure SetLastShot(aValue   : String);
  Procedure SetConnectionString(aValue   : String);
  Procedure OldClick   (Sender: TObject);
 Public
  Constructor Create  (aSender  : TFmxObject);
  Destructor  Destroy;Override;
  Property    PanelNum         : Integer             Read vPanelNum;
  Property    Top              : Integer             Read vTop              Write SetTop;
  Property    Left             : Integer             Read vLeft             Write SetLeft;
  Property    Height           : Integer             Read vHeight           Write SetHeight;
  Property    Width            : Integer             Read vWidth            Write SetWidth;
  Property    Visible          : Boolean             Read vVisible          Write SetVisible;
  Property    Active           : Boolean             Read vActive           Write SetActive;
  Property    ConnectionString : String              Read vConnectionString Write SetConnectionString;
  Property    ConnectionName   : String              Read vConnectionName   Write vConnectionName;
  Property    RemoteAssist     : Boolean             Read vRemoteAssist     Write vRemoteAssist;
  Property    Password         : String              Read vPassword         Write vPassword;
  Property    ID               : String              Read vID               Write SetID;
  Property    Grupo            : String              Read vGrupo            Write SetGrupo;
  Property    Alias            : String              Read vAlias            Write SetAlias;
  Property    LastShot         : String              Read vLastShot         Write SetLastShot;
  Property    OnDoubleClick    : TPanelActionOnClick Read vActionOnClick    Write vActionOnClick;
End;

type
  TfFavoritos = class(TForm)
    tvGroups: TTreeView;
    Panel3: TPanel;
    vsbLista: TVertScrollBox;
    ImageList1: TImageList;
    TreeViewItem1: TTreeViewItem;
    pmDados: TPopupMenu;
    miInsertItem: TMenuItem;
    miEdit: TMenuItem;
    miDeleteItem: TMenuItem;
    MenuItem1: TMenuItem;
    miConnect: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure pmDadosPopup(Sender: TObject);
    procedure miConnectClick(Sender: TObject);
    procedure tvGroupsClick(Sender: TObject);
    procedure tvGroupsKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure miInsertItemClick(Sender: TObject);
    procedure miEditClick(Sender: TObject);
    procedure miDeleteItemClick(Sender: TObject);
  private
    { Private declarations }
   Procedure DestroyPanels;
   Procedure LoadData    (Var aPanel     : TPanelClass;
                          aConnection    : TAegysMyConnection);
   Procedure CreateGroups(Var tView      : TTreeView;
                          Connection     : TAegysMyConnection);
   Procedure SetFilterList;
   Procedure OnDoubleClick(aPanel: TPanelClass);
  public
    { Public declarations }
   Procedure CreatePanels(aCount         : Integer);Overload;
   Procedure CreatePanels(ConnectionList : TAegysMyConnectionList;
                          aFilterList    : String = cAllSelect);Overload;
   Procedure SetImage    (aConnection    : TAegysMyConnection;
                          aImage         : String);
  end;

var
  fFavoritos  : TfFavoritos;
  aDadosPanel : Array of TPanelClass;

implementation

Uses uAegysTools, uAegysConsts, uAegysBufferPack, uAegysDataTypes, uFormConexao,
  uFavData;

{$R *.fmx}

Procedure TPanelClass.OldClick(Sender : TObject);
Var
 vPanelClick : Integer;
Begin
 vPanelClick := -1;
 If Sender is TPanel Then
  vPanelClick         := TPanel(Sender).Tag
 Else If TComponent(Sender).Owner is TPanel Then
  vPanelClick         := TPanel(TComponent(Sender).Owner).Tag;
 If vPanelClick > -1 Then
  Begin
   If Assigned(vActionOnClick) Then
    vActionOnClick(aDadosPanel[vPanelClick]);
  End;
 ProcessMessages;
End;

Procedure TfFavoritos.OnDoubleClick(aPanel : TPanelClass);
Begin
 If (aPanel.Active)         And
    (aPanel.Password <> '') And
    (aPanel.RemoteAssist)   Then
  Begin
   TThread.CreateAnonymousThread(Procedure
                                 Begin
                                  TThread.Synchronize(Nil, Procedure
                                                           Begin
                                                            FormConexao.EGuestID.Text := aPanel.ID;
                                                            aLastPass                 := aPanel.Password;
                                                            ProcessMessages;
                                                            FormConexao.LbtnConectar.OnClick(FormConexao.LbtnConectar);
                                                           End);
                                 End).Start;
  End;
End;

procedure TfFavoritos.LoadData(Var aPanel  : TPanelClass;
                               aConnection : TAegysMyConnection);
Begin
 aPanel.Active           := aConnection.ConnectionActive;
 aPanel.RemoteAssist     := aConnection.RemoteAssist;
 aPanel.ID               := aConnection.ConnectionID;
 aPanel.Grupo            := aConnection.ConnectionGroup;
 aPanel.Password         := aConnection.ConnectionPass;
 aPanel.ConnectionString := aConnection.ConnectionString;
 aPanel.ConnectionName   := aConnection.ConnectionName;
 aPanel.Alias            := aConnection.ConnectionAlias;
 aPanel.LastShot         := aConnection.ConnectionLastShot;
End;

procedure TfFavoritos.miConnectClick(Sender: TObject);
Var
 vPanelClick : Integer;
begin
 vPanelClick := -1;
 If pmDados.PopupComponent is TPanel Then
  vPanelClick         := TPanel(pmDados.PopupComponent).Tag
 Else If pmDados.PopupComponent.GetParentComponent Is TPanel Then
  vPanelClick         := TPanel(pmDados.PopupComponent.GetParentComponent).Tag;
 If vPanelClick > -1 Then
  Begin
   TThread.CreateAnonymousThread(Procedure
                                 Begin
                                  TThread.Synchronize(Nil, Procedure
                                                           Begin
                                                            FormConexao.EGuestID.Text := aDadosPanel[vPanelClick].ID;
                                                            aLastPass                 := aDadosPanel[vPanelClick].Password;
                                                            FormConexao.LbtnConectar.OnClick(FormConexao.LbtnConectar);
                                                           End);
                                 End).Start;
  End;
end;

procedure TfFavoritos.miDeleteItemClick(Sender: TObject);
Var
 vActive     : Boolean;
 vPanelClick : Integer;
begin
 vPanelClick          := -1;
 If pmDados.PopupComponent is TPanel Then
  vPanelClick         := TPanel(pmDados.PopupComponent).Tag
 Else If pmDados.PopupComponent.GetParentComponent Is TPanel Then
  vPanelClick         := TPanel(pmDados.PopupComponent.GetParentComponent).Tag;
 If vPanelClick > -1 Then
  Begin
   If Not Assigned(fFavData) Then
    fFavData := TfFavData.Create(Self);
   fFavData.Delete(aDadosPanel[vPanelClick]);
   fFavData.sbPost.OnClick(Self);
   Conexao.FavConnectionList.Delete(aDadosPanel[vPanelClick].ConnectionString,
                                    aDadosPanel[vPanelClick].ID);
   fFavoritos.CreatePanels(Conexao.FavConnectionList);
   FreeAndNil(fFavData);
  End;
end;

procedure TfFavoritos.miEditClick(Sender: TObject);
Var
 vActive     : Boolean;
 vPanelClick : Integer;
begin
 vPanelClick          := -1;
 If pmDados.PopupComponent is TPanel Then
  vPanelClick         := TPanel(pmDados.PopupComponent).Tag
 Else If pmDados.PopupComponent.GetParentComponent Is TPanel Then
  vPanelClick         := TPanel(pmDados.PopupComponent.GetParentComponent).Tag;
 If vPanelClick > -1 Then
  Begin
   If Not Assigned(fFavData) Then
    fFavData := TfFavData.Create(Self);
   fFavData.Edit(aDadosPanel[vPanelClick]);
  End;
end;

procedure TfFavoritos.miInsertItemClick(Sender: TObject);
begin
 If Not Assigned(fFavData) Then
  fFavData := TfFavData.Create(Self);
 fFavData.Insert;
end;

procedure TfFavoritos.CreateGroups(Var tView  : TTreeView;
                                   Connection : TAegysMyConnection);
Var
 I      : Integer;
 aFound : Boolean;
 tvItem : TTreeViewItem;
Begin
 aFound := False;
 If tView.Count = 0 Then
  Begin
   tvItem := TTreeViewItem.Create(Self);
   tvItem.Parent := tView;
   tvItem.ImageIndex := 0;
   tvItem.TextSettings.Font.Style := [TFontStyle.fsBold];
   tvItem.TextSettings.FontColor  := TAlphaColorRec.Blue;
   tvItem.TextSettings.HorzAlign  := TTextAlign.Leading;
   tvItem.StyledSettings := tvItem.StyledSettings - [TStyledSetting.FontColor,
                                                     TStyledSetting.Style,
                                                     TStyledSetting.Size];
   tvItem.Text := cAllSelect;
  End;
 For I := 1 To tView.Count -1 Do
  Begin
   aFound := Uppercase(tView.Items[I].Text) = Uppercase(Connection.ConnectionGroup);
   If aFound Then
    Break;
  End;
 If Not aFound Then
  Begin
   tvItem := TTreeViewItem.Create(Self);
   tvItem.Parent := tView;
   tvItem.ImageIndex := 1;
   tvItem.TextSettings.Font.Style := [TFontStyle.fsBold];
   tvItem.TextSettings.FontColor  := TAlphaColorRec.Green;
   tvItem.TextSettings.HorzAlign  := TTextAlign.Leading;
   tvItem.StyledSettings := tvItem.StyledSettings - [TStyledSetting.FontColor,
                                                     TStyledSetting.Style,
                                                     TStyledSetting.Size];
   tvItem.Text := Connection.ConnectionGroup;
  End;
End;

procedure TfFavoritos.CreatePanels(ConnectionList : TAegysMyConnectionList;
                                   aFilterList    : String = cAllSelect);
Begin
 DestroyPanels;
 TThread.Synchronize(Nil, Procedure
                          Var
                           aCount,
                           I, A,
                           vOldTop : Integer;
                           Function CountList : Integer;
                           Var
                            I : Integer;
                           Begin
                            Result := 0;
                            For I := 0 To ConnectionList.Count -1 Do
                             Begin
                              If ConnectionList[I].ConnectionGroup = aFilterList Then
                               Inc(Result);
                             End;
                           End;
                          Begin
                           vOldTop := 0;
                           A       := 0;
                           vsbLista.BeginUpdate;
                           If aFilterList = cAllSelect Then
                            aCount := ConnectionList.Count
                           Else
                            aCount := CountList;
                           Try
                            SetLength(aDadosPanel, aCount);
                            For I := 0 To ConnectionList.Count -1 Do
                             Begin
                              If (aFilterList                       = cAllSelect) Or
                                 (ConnectionList[I].ConnectionGroup = aFilterList) Then
                               Begin
                                aDadosPanel[A]               := TPanelClass.Create(vsbLista);
                                aDadosPanel[A].Top           := (aDadosPanel[A].Height * A) + 1;
                                aDadosPanel[A].vPanelNum     := A;
                                aDadosPanel[A].pDados.Tag    := aDadosPanel[A].vPanelNum;
                                aDadosPanel[A].OnDoubleClick := OnDoubleClick;
                                LoadData(aDadosPanel[A], ConnectionList[I]);
                                aDadosPanel[A].Visible    := True;
                                Inc(A);
                               End;
                              CreateGroups(tvGroups, ConnectionList[I]);
                              ProcessMessages;
                             End;
                           Finally
                            vsbLista.EndUpdate;
                           End;
                          End);
End;

Procedure TfFavoritos.DestroyPanels;
Var
 I : Integer;
Begin
 vsbLista.BeginUpdate;
 Try
  For I := 0 To Length(aDadosPanel) -1 Do
   FreeAndNil(aDadosPanel[I]);
 Finally
  SetLength(aDadosPanel, 0);
  vsbLista.EndUpdate;
 End;
End;

Procedure TfFavoritos.CreatePanels(aCount : Integer);
Var
 I,
 vOldTop : Integer;
Begin
 DestroyPanels;
 vOldTop := 0;
 SetLength(aDadosPanel, aCount);
 For I := 0 To aCount Do
  Begin
   aDadosPanel[I]            := TPanelClass.Create(vsbLista);
   aDadosPanel[I].Top        := (aDadosPanel[I].Height * I) + 1;
   aDadosPanel[I].vPanelNum  := I;
   aDadosPanel[I].pDados.Tag := aDadosPanel[I].vPanelNum;
   aDadosPanel[I].OnDoubleClick := OnDoubleClick;
   aDadosPanel[I].Visible    := True;
  End;
End;

procedure TfFavoritos.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 TThread.Synchronize(Nil, Procedure
                     Begin
                      DestroyPanels;
                      fFavoritos := Nil;
                     End);
 fFavoritos.DisposeOf;
end;

procedure TfFavoritos.pmDadosPopup(Sender: TObject);
Var
 vActive     : Boolean;
 vPanelClick : Integer;
begin
 vPanelClick          := -1;
 miEdit.Enabled       := Length(aDadosPanel) > 0;
 miDeleteItem.Enabled := miEdit.Enabled;
 vActive              := miEdit.Enabled;
 miConnect.Enabled    := vActive;
 If miConnect.Enabled Then
  Begin
   If pmDados.PopupComponent is TPanel Then
    vPanelClick         := TPanel(pmDados.PopupComponent).Tag
   Else If pmDados.PopupComponent.GetParentComponent Is TPanel Then
    vPanelClick         := TPanel(pmDados.PopupComponent.GetParentComponent).Tag;
   If vPanelClick > -1 Then
    miConnect.Enabled   := aDadosPanel[vPanelClick].Active
   Else
    miConnect.Enabled   := False;
  End;
end;

procedure TfFavoritos.SetFilterList;
Var
 vGroup : String;
Begin
 vGroup := '';
 If tvGroups.Selected <> Nil Then
  Begin
   If tvGroups.Selected.Index > 0 Then //Filter
    vGroup := tvGroups.Selected.Text
   Else //All
    vGroup := cAllSelect;
   CreatePanels(Conexao.FavConnectionList, vGroup);
  End;
End;

Procedure TfFavoritos.SetImage(aConnection : TAegysMyConnection;
                               aImage      : String);
Var
 I          : Integer;
 aPackClass : TPackClass;
 aBuf       : TAegysBytes;
Begin
 For I := 0 To Length(aDadosPanel) -1 Do
  Begin
   If (aDadosPanel[I].ConnectionString = aConnection.ConnectionString) Or
      (aDadosPanel[I].ID               = aConnection.ConnectionID)     Then
    Begin
     aDadosPanel[I].SetLastShot(aImage);
     aPackClass            := TPackClass.Create;
     Try
      aPackClass.DataMode  := tdmServerCommand;
      aPackClass.DataCheck := tdcAsync;
      aPackClass.Command   := cPeerService + Format('%s%s&%s', [cSendImage,
                                                                FormConexao.EGuestID.Text,
                                                                aImage]);
      aBuf                 := aPackClass.ToBytes;
      Conexao.SendBytes(aBuf);
     Finally
      SetLength(aBuf, 0);
      FreeAndNil(aPackClass);
     End;
     Break;
    End;
  End;
End;

procedure TfFavoritos.tvGroupsClick(Sender: TObject);
Begin
 SetFilterList;
End;

procedure TfFavoritos.tvGroupsKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
 If Key in [vkUp, vkDown] then
  SetFilterList;
end;

{ TPanelClass }

constructor TPanelClass.Create(aSender: TFmxObject);
begin
 vVisible            := False;
 vActive             := False;
 vHeight             := 120;
 vWidth              := 446;
 vLeft               := 0;
 vTop                := 0;
 pDados              := TPanel.Create(aSender);
 pDados.Parent       := aSender;
 pDados.PopupMenu    := TVertScrollBox(aSender).PopupMenu;
 pDados.Position.X   := vLeft;
 pDados.Position.Y   := vTop;
 pDados.Size.Width   := vWidth;
 pDados.Size.Height  := vHeight;
 pDados.Visible      := vVisible;
 pDados.OnDblClick   := OldClick;
 ivDados             := TRectangle.Create(pDados);
 ivDados.Parent      := pDados;
 ivDados.Align       := TAlignLayout.Left;
 ivDados.Size.Width  := 120;
 ivDados.Size.Height := TVertScrollBox(aSender).Height;
 ivDados.OnDblClick  := OldClick;
 ivDados.PopupMenu   := pDados.PopupMenu;
 aLabID              := TLabel.Create(pDados);
 aLabID.Parent       := pDados;
 aLabID.Position.X   := 149;
 aLabID.Position.Y   := 21;
 aLabID.Size.Width   := 49;
 aLabID.Size.Height  := 16;
 aLabID.TextSettings.Font.Style := [TFontStyle.fsBold];
 aLabID.TextSettings.FontColor  := TAlphaColorRec.Blue;
 aLabID.TextSettings.HorzAlign  := TTextAlign.Trailing;
 aLabID.StyledSettings := aLabID.StyledSettings - [TStyledSetting.FontColor,
                                                   TStyledSetting.Style,
                                                   TStyledSetting.Size];
 aLabID.PopupMenu   := pDados.PopupMenu;
 aLabID.OnDblClick  := OldClick;
 aLabID.Text                    := 'ID:';
 aLabGrupo             := TLabel.Create(pDados);
 aLabGrupo.Parent      := pDados;
 aLabGrupo.Position.X  := 149;
 aLabGrupo.Position.Y  := 45;
 aLabGrupo.Size.Width  := 49;
 aLabGrupo.Size.Height := 16;
 aLabGrupo.TextSettings.Font.Style := [TFontStyle.fsBold];
 aLabGrupo.TextSettings.FontColor  := TAlphaColorRec.Blue;
 aLabGrupo.TextSettings.HorzAlign  := TTextAlign.Trailing;
 aLabGrupo.StyledSettings := aLabGrupo.StyledSettings - [TStyledSetting.FontColor,
                                                         TStyledSetting.Style,
                                                         TStyledSetting.Size];
 aLabGrupo.PopupMenu   := pDados.PopupMenu;
 aLabGrupo.OnDblClick  := OldClick;
 aLabGrupo.Text        := 'Grupo:';
 aLabAlias             := TLabel.Create(pDados);
 aLabAlias.Parent      := pDados;
 aLabAlias.Position.X  := 149;
 aLabAlias.Position.Y  := 72;
 aLabAlias.Size.Width  := 49;
 aLabAlias.Size.Height := 16;
 aLabAlias.TextSettings.Font.Style := [TFontStyle.fsBold];
 aLabAlias.TextSettings.FontColor  := TAlphaColorRec.Blue;
 aLabAlias.TextSettings.HorzAlign  := TTextAlign.Trailing;
 aLabAlias.StyledSettings := aLabGrupo.StyledSettings - [TStyledSetting.FontColor,
                                                         TStyledSetting.Style,
                                                         TStyledSetting.Size];
 aLabAlias.PopupMenu   := pDados.PopupMenu;
 aLabAlias.OnDblClick  := OldClick;
 aLabAlias.Text         := 'Alias:';
 aLabActive             := TLabel.Create(pDados);
 aLabActive.Parent      := pDados;
 aLabActive.Position.X  := 376;
 aLabActive.Position.Y  := 3;
 aLabActive.Size.Width  := 57;
 aLabActive.Size.Height := 19;
 aLabActive.TextSettings.Font.Size  := 14;
 aLabActive.TextSettings.Font.Style := [TFontStyle.fsBold];
 aLabActive.TextSettings.HorzAlign  := TTextAlign.Center;
 aLabActive.TextSettings.FontColor  := TAlphaColorRec.Red;
 aLabActive.StyledSettings := aLabActive.StyledSettings - [TStyledSetting.FontColor,
                                                           TStyledSetting.Style,
                                                           TStyledSetting.Size];
 aLabActive.PopupMenu   := pDados.PopupMenu;
 aLabActive.OnDblClick  := OldClick;
 aLabActive.Text         := 'Inativo';
 aLabIDValue             := TLabel.Create(pDados);
 aLabIDValue.Parent      := pDados;
 aLabIDValue.Position.X  := 205;
 aLabIDValue.Position.Y  := 21;
 aLabIDValue.Size.Width  := 236;
 aLabIDValue.Size.Height := 16;
 aLabIDValue.TextSettings.FontColor  := TAlphaColorRec.Blue;
 aLabIDValue.StyledSettings := aLabIDValue.StyledSettings - [TStyledSetting.FontColor,
                                                             TStyledSetting.Style,
                                                             TStyledSetting.Size];
 aLabIDValue.PopupMenu   := pDados.PopupMenu;
 aLabIDValue.OnDblClick  := OldClick;
 aLabIDValue.Text        := '...';
 aLabGrupoValue          := TLabel.Create(pDados);
 aLabGrupoValue.Parent   := pDados;
 aLabGrupoValue.Position.X := 205;
 aLabGrupoValue.Position.Y := 45;
 aLabGrupoValue.Size.Width := 236;
 aLabGrupoValue.Size.Height := 16;
 aLabGrupoValue.TextSettings.FontColor  := TAlphaColorRec.Blue;
 aLabGrupoValue.StyledSettings := aLabGrupoValue.StyledSettings - [TStyledSetting.FontColor,
                                                                   TStyledSetting.Style,
                                                                   TStyledSetting.Size];
 aLabGrupoValue.PopupMenu   := pDados.PopupMenu;
 aLabGrupoValue.OnDblClick  := OldClick;
 aLabGrupoValue.Text        := '...';
 aLabAliasValue          := TLabel.Create(pDados);
 aLabAliasValue.Parent   := pDados;
 aLabAliasValue.Position.X := 205;
 aLabAliasValue.Position.Y := 72;
 aLabAliasValue.Size.Width := 236;
 aLabAliasValue.Size.Height := 16;
 aLabAliasValue.TextSettings.FontColor  := TAlphaColorRec.Blue;
 aLabAliasValue.StyledSettings := aLabAliasValue.StyledSettings - [TStyledSetting.FontColor,
                                                                   TStyledSetting.Style,
                                                                   TStyledSetting.Size];
 aLabAliasValue.PopupMenu   := pDados.PopupMenu;
 aLabAliasValue.OnDblClick  := OldClick;
 aLabAliasValue.Text        := '...';
end;

destructor TPanelClass.Destroy;
begin
  FreeAndNil(ivDados);
  FreeAndNil(aLabActive);
  FreeAndNil(aLabID);
  FreeAndNil(aLabIDValue);
  FreeAndNil(aLabGrupo);
  FreeAndNil(aLabGrupoValue);
  FreeAndNil(aLabAlias);
  FreeAndNil(aLabAliasValue);
  FreeAndNil(pDados);
  inherited;
end;

Procedure TPanelClass.SetActive(aActive : Boolean);
Begin
 vActive := aActive;
 If vActive Then
  Begin
   aLabActive.TextSettings.FontColor  := TAlphaColorRec.Green;
   aLabActive.Text                    := 'Ativo';
  End
 Else
  Begin
   aLabActive.TextSettings.FontColor  := TAlphaColorRec.Red;
   aLabActive.Text                    := 'Inativo';
  End;
End;

Procedure TPanelClass.SetAlias(aValue : String);
Begin
 vAlias := aValue;
 aLabAliasValue.Text := vAlias;
 If aValue = '' Then
  aLabAliasValue.Text := '...';
End;

Procedure TPanelClass.SetConnectionString(aValue : String);
Begin
 vConnectionString := aValue;
End;

Procedure TPanelClass.SetGrupo(aValue : String);
Begin
 vGrupo              := aValue;
 aLabGrupoValue.Text := vGrupo;
 If aValue = '' Then
  aLabGrupoValue.Text := '...';
End;

Procedure TPanelClass.SetHeight(aHeight: Integer);
Begin
 vHeight       := aHeight;
 pDados.Height := vHeight;
End;

procedure TPanelClass.SetID(aValue : String);
begin
 vID              := aValue;
 aLabIDValue.Text := vID;
 If aValue = '' Then
  aLabIDValue.Text := '...';
end;

procedure TPanelClass.SetLastShot(aValue: String);
Var
 aStream : TStream;
Begin
 vLastShot := aValue;
 If Trim(vLastShot) <> '' Then
  Begin
   aStream   := DecodeStream(vLastShot);
   If Assigned(aStream) Then
    Begin
     Try
      aStream.Position             := 0;
      TThread.Synchronize(Nil, Procedure
                               Begin
                                ivDados.Fill.Kind            := TBrushKind.Bitmap;
                                ivDados.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
                                ivDados.Fill.Bitmap.Bitmap.LoadFromStream(aStream);
                               End);
     Finally
      FreeAndNil(aStream);
     End;
    End;
  End;
End;

Procedure TPanelClass.SetLeft(aLeft : Integer);
Begin
 pDados.Position.X  := aLeft;
End;

Procedure TPanelClass.SetTop(aTop : Integer);
Begin
 pDados.Position.Y  := aTop;
End;

procedure TPanelClass.SetVisible(aVisible: Boolean);
Begin
 vVisible       := aVisible;
 pDados.Visible := vVisible;
End;

Procedure TPanelClass.SetWidth(aWidth: Integer);
Begin
 vWidth       := aWidth;
 pDados.Width := vWidth;
End;

End.
