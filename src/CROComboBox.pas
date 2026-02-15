unit CROComboBox;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, CROOperation, Windows, TypInfo,
  CROComponentsCommon, Dialogs, uRODL, Messages, Graphics, Forms, Spin;

type                                                            
  TCROComboItem  = class;
  TCROComboItems = class;

  TCROComboBox = class(TCustomComboBox, ICROCustomControl, ICROEnumControl, ICROSkinControl)
  published
    property AutoComplete default True;
    property AutoDropDown default False;
    property AutoCloseUp default False;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property Anchors;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property ItemIndex default -1;
    property Items;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
  private
    { Private declarations }
    FLoading: Boolean;
    FFocused: Boolean;
    FDisplayText: String;
    FInternalItemIndex: Integer;
    FUpdatingValue: Boolean;
    FReadOnly: Boolean;
    FShowButton: Boolean;
    FProperties: TCROProperties;
    FItemValues: TCROComboItems;
    FEnterToTab: Boolean;
    procedure SetProperties(Value: TCROProperties);
    function GetSelected: String;
    procedure SetReadOnly(Value: Boolean);
    procedure SetShowButton(Value: Boolean);
    procedure InternalSetItemIndex(AItemIndex: Integer);
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
  protected
    { Protected declarations }
    procedure CreateWnd; override;
    procedure Change; override;
    procedure DoExit; override;
    procedure UpdateItemValues;
    procedure SetItemValues(const Value: TCROComboItems);
    procedure SetItems(const Value: TStrings); override;
    procedure OnUpdateExpression(Sender: TObject);
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Select; override;
    procedure Loaded; override;
  public
    { Public declarations }
    function GetProperties: TCROProperties;
    function CaptionOfName(const AName:String):String;
    procedure UpdateValue;
    procedure UpdateObject;
    procedure UpdateSkin;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property Properties: TCROProperties read GetProperties write SetProperties;
    property ItemValues: TCROComboItems read FItemValues write SetItemValues;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property Selected: String read GetSelected;
    property ShowButton: Boolean read FShowButton write SetShowButton default True;
    property EnterToTab: Boolean read FEnterToTab write FEnterToTab default True;
  end;

  TCROComboItem  = class(TCollectionItem)
  private
    FName: String;
    FCaption : String;
    FVisible : Boolean;
    procedure SetName(Value: String);
    procedure SetCaption(Value: String);
    procedure SetVisible(Value: Boolean);
    procedure DoUpdate;
  protected
    procedure AssignItem(Item: TCROComboItem);
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); overload; override;
    constructor Create(Collection: TCollection; AName, ACaption: String;
      AVisible: Boolean); reintroduce; overload;//se der problema tire o reintroduce
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Name: String read FName write SetName;
    property Caption: String read FCaption write SetCaption;
    property Visible: Boolean read FVisible write SetVisible;
  end;

  TCROComboItems = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TCROComboItem;
    procedure SetItem(Index: Integer; Value: TCROComboItem);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TComponent); overload;
    function CreateItem(AName, ACaption: String; AVisible: Boolean): TCROComboItem;
    procedure AssignValues(Value: TCROComboItems);
    function ItemByName(const Value: string): TCROComboItem;
    function ItemByCaption(const Value: string): TCROComboItem;
    function FindItem(const Value: string): TCROComboItem;
    property Items[Index: Integer]: TCROComboItem read GetItem write SetItem; default;
  end;


implementation

{ TCROComboBox }

procedure TCROComboBox.DoExit;
begin
  UpdateObject;
  inherited;
end;

function TCROComboBox.GetProperties: TCROProperties;
begin
  result := FProperties;
end;

procedure TCROComboBox.SetProperties(Value: TCROProperties);
begin
  if Value <> nil
  then FProperties := Value;
end;

procedure TCROComboBox.UpdateObject;
var
  APropInfo: PPropInfo;
  Instance: TObject;
  AExpression: String;
  V: Variant;
begin
  if Assigned(FProperties.CursorHandle) and
     Assigned(FProperties.CursorHandle.Data) and
     (Selected <> '') then
  begin
    if (ItemIndex > -1) then
    begin
      Instance := FProperties.CursorHandle.Data;
      AExpression := FProperties.Expression;
      GetPropInfoFromExpression(GetTrueOwner(Self.Owner,AExpression),AExpression,Instance,APropInfo);

      FProperties.GetValueFromExpression(V);
      if V <> Selected //para otimização, assim naum atualiza desnecessariamente todos os objetos
      then FProperties.SetValueToExpression(GetEnumValue(APropInfo^.PropType^,Selected));
    end
    else
      UpdateValue;
  end;
end;

procedure TCROComboBox.UpdateSkin;
begin
  if Assigned(FProperties.FontProperties) and Assigned(Font) then
  begin
    Font.Assign(FProperties.FontProperties.Font);
    FEnterToTab := FProperties.FontProperties.EnterToTab;
  end;
end;

procedure TCROComboBox.UpdateValue;
var V: Variant;
    sCaption: String;
    Item: TCROComboItem;
begin
  sCaption := '';

  if Assigned(FProperties.CursorHandle) and
     Assigned(FProperties.CursorHandle.Data) then
  begin
    FProperties.GetValueFromExpression(V);

    //deve ser feito este controle em todos os componentes de ediçãod e dados
    FLoading := True;

    Item := FItemValues.ItemByName(V);
    if Assigned(Item)
    then sCaption  := Item.Caption
    else sCaption  := '';
    ItemIndex := Items.IndexOf(sCaption);

    FLoading := False;
  end;
  FInternalItemIndex := ItemIndex;
  FDisplayText := sCaption;
//*****  PostMessage(Handle, WM_PAINT, 0, 0);
end;

constructor TCROComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FItemValues := TCROComboItems.Create(Self);
  FInternalItemIndex := -1;
  FProperties := TCROProperties.Create(Self);
  FProperties.OnInternalUpdateExpression := OnUpdateExpression;
  FShowButton := True;
  FUpdatingValue := False;
  FDisplayText := Text;
  FEnterToTab := True;
end;

destructor TCROComboBox.Destroy;
begin
  FItemValues.Free;
  FProperties.Free;
  inherited;
end;

procedure TCROComboBox.SetItemValues(const Value: TCROComboItems);
begin
  FItemValues.Assign(Value);
  UpdateItemValues;
end;

procedure TCROComboBox.SetItems(const Value: TStrings);
begin
//Does not inherit, does not allow to change the value of this property
end;

function TCROComboBox.GetSelected: String;
begin
  if ItemIndex = -1
  then Result := ItemValues.ItemByCaption(Items[0]).Name
  else Result := ItemValues.ItemByCaption(Items[ItemIndex]).Name;
end;

procedure TCROComboBox.OnUpdateExpression(Sender: TObject);
var
  i: Integer;
  Lib: TRODLLibrary;
  Enum: TRODLEnum;
  sProposedDisplay: String;
  ComboItems: TCROComboItems;
  ComboItem : TCROComboItem;
begin
  //Executed when Properties.Expression have been modified
  //Load ItemValues default list - its executed at design time, so CursorHandle.Data is not loaded
  if (csDesigning in ComponentState) and
     (FProperties.Expression <> '') and
     (FProperties.DataType <> '') and
     Assigned(FProperties.CursorHandle) and
     Assigned(FProperties.CursorHandle.SourceOperation) and
     Assigned(FProperties.CursorHandle.SourceOperation.RORemoteService) and
     FProperties.CursorHandle.SourceOperation.Attached  then
  begin
    Lib := FProperties.CursorHandle.SourceOperation.RORemoteService.GetRODLLibrary;

    Enum := FindROEnum(Lib,FProperties.DataType);
    if Assigned(Enum) then
    begin
      ComboItems := TCROComboItems.Create(nil);
      try
        for i := 0 to Enum.Count-1 do
        begin
          sProposedDisplay := GetProposedDisplay(Enum.Items[i].Info.Name);
          ComboItems.CreateItem(Enum.Items[i].Info.Name, sProposedDisplay, True);
        end;

        // Verify if any items was deleted
        i := 0;
        while i < FItemValues.Count do
        begin
          ComboItem := ComboItems.FindItem(FItemValues[i].Name);

          if not Assigned(ComboItem)
          then FItemValues.Delete(i)
          else Inc(i);
        end;

        // Verify if any items was created
        for i := 0 to ComboItems.Count - 1 do
        begin
          ComboItem := FItemValues.FindItem(ComboItems[i].Name);

          if not Assigned(ComboItem)
          then FItemValues.CreateItem(ComboItems[i].Name, ComboItems[i].Caption, True);
        end;
      finally
        ComboItems.Free;
      end;
    end
    else
      raise Exception.Create('CROComboBox - Expression is not an Enumeration property');
  end
  else
  begin
    if Trim(FProperties.Expression) = ''   //se naum fizer essa verificação a lista é apagada ao carregar o projeto
    then FItemValues.Clear;
  end;

  UpdateItemValues;
end;

procedure TCROComboBox.InternalSetItemIndex(AItemIndex: Integer);
begin
  try
    FUpdatingValue := True;
    ItemIndex := AItemIndex;
  finally
    FUpdatingValue := False;
  end;
end;

procedure TCROComboBox.SetReadOnly(Value: Boolean);
begin
  FReadOnly := Value;
end;

procedure TCROComboBox.SetShowButton(Value: Boolean);
begin
  FShowButton := Value;

  if FShowButton
  then inherited Style := csDropDownList
  else inherited Style := csSimple;
end;

procedure TCROComboBox.WMPaint(var Message: TWMPaint);
var Rect: TRect;
begin
  inherited;

  if (ItemIndex = -1) and (FShowButton) then
  begin
    Rect.Top    := 3;
    Rect.Left   := 3;
    Rect.Bottom := Self.Height-3;
    if FShowButton
    then Rect.Right  := Self.Width-19
    else Rect.Right  := Self.Width-4;

    if FFocused then
    begin
      Canvas.Brush.Color := clHighlight;
      Canvas.Font.Color  := clHighlightText;
    end
    else
    begin
      Canvas.Brush.Color := clWindow;
      Canvas.Font.Color  := clWindowText;
    end;

    Canvas.FillRect(Rect);
    Canvas.TextOut(3,3,FDisplayText);

    if FFocused
    then Canvas.DrawFocusRect(Rect);
  end;
end;

procedure TCROComboBox.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  FFocused := True;
//*****  PostMessage(Handle, WM_PAINT, 0, 0);
end;

procedure TCROComboBox.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  FFocused := False;
//*****  PostMessage(Handle, WM_PAINT, 0, 0);
end;

procedure TCROComboBox.KeyPress(var Key: Char);
begin
  if Assigned(FProperties.CursorHandle) then
  begin
    if (not FProperties.CursorHandle.AutoEdit) and
       (not (FProperties.CursorHandle.State in [csEdit,csInsert])) {and
       (not (Key in [#13]))}
    then Key := #0;
  end;

  if FReadOnly
  then Key := #0
  else inherited;

  CheckFocus(Self,Key,FEnterToTab);
end;

procedure TCROComboBox.Select;
begin
  if (not FReadOnly) or (FUpdatingValue) then
  begin
    inherited;
    FInternalItemIndex := ItemIndex;
  end
  else
  begin
    InternalSetItemIndex(FInternalItemIndex);

//*****    PostMessage(Handle, WM_PAINT, 0, 0);
  end;
end;

procedure TCROComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Assigned(FProperties.CursorHandle) then
  begin
    if (
         (not (FProperties.CursorHandle.AutoEdit)) and
         (not (FProperties.CursorHandle.State in [csEdit,csInsert]))
       ){ or (
         (not (Key in [27,13,VK_DOWN,VK_UP,VK_LEFT,VK_RIGHT]))
       )}
    then Key := 0;
  end;

  if FReadOnly then
  begin
    case Key of
      VK_DOWN: Key := VK_RIGHT;
      VK_UP  : Key := VK_LEFT;
    end;
  end;
  inherited;
end;

procedure TCROComboBox.UpdateItemValues;
var i: Word;
begin
  if Assigned(Self) and Assigned(Items) then
  begin
    Items.Clear;

    if Assigned(FItemValues) and (FItemValues.Count > 0) then
    begin
      for i := 0 to FItemValues.Count-1 do
      begin
        if FItemValues[i].Visible
        then Items.Add(FItemValues[i].Caption);
      end;
    end;
  end;
end;

function TCROComboBox.CaptionOfName(const AName: String): String;
var Item: TCROComboItem;
begin
  Item := ItemValues.ItemByName(AName);
  if Assigned(Item)
  then Result := Item.Caption
  else Result := '';
end;

procedure TCROComboBox.Loaded;
begin
  inherited;
  if Assigned(FProperties.CursorHandle)
  then FProperties.CursorHandle.FixExpression; //recarrega expression do handle e do properties e executa o OnUpdateExpression(nil);

  UpdateValue;
end;

procedure TCROComboBox.Change;
begin
  //deve ser feito este controle em todos os componentes de ediçãod e dados
  if Assigned(FProperties.CursorHandle) and
     Assigned(FProperties.CursorHandle.Data) and
     (not FLoading) and
     (FProperties.CursorHandle.AutoEdit) then
  begin
    FProperties.CursorHandle.Edit;
  end
  else
  begin
    if (not FLoading) and (not (FProperties.CursorHandle.State in [csEdit,csInsert]))
    then UpdateValue;
  end;
  inherited;
end;

procedure TCROComboBox.CreateWnd;
begin
  inherited;
  inherited Style := csDropDownList;
  ItemIndex := -1;
end;

{ TCROComboItem }

procedure TCROComboItem.Assign(Source: TPersistent);
begin
  if Source is TCROComboItem
  then AssignItem(TCROComboItem(Source))
  else inherited Assign(Source);
end;

procedure TCROComboItem.AssignItem(Item: TCROComboItem);
begin
  if Item <> nil then
  begin
    FName := Item.Name;
    Caption  := Item.Caption;
    Visible  := Item.Visible;
  end;
end;

constructor TCROComboItem.Create(Collection: TCollection);
begin
  inherited;
end;

constructor TCROComboItem.Create(Collection: TCollection; AName,
  ACaption: String; AVisible: Boolean);
begin
  Create(Collection);

  FName   := AName;
  Caption := ACaption;
  Visible := AVisible;
end;

destructor TCROComboItem.Destroy;
begin
  inherited;
end;

procedure TCROComboItem.DoUpdate;
var AOwner: TPersistent;
begin
  AOwner := Collection.Owner;
  if (Assigned(AOwner)) and
     (Collection.Owner.InheritsFrom(TCROComboBox))
  then TCROComboBox(Collection.Owner).UpdateItemValues;
end;

function TCROComboItem.GetDisplayName: string;
begin
  Result := FCaption;
end;

procedure TCROComboItem.SetCaption(Value: String);
begin
  FCaption := Value;
  DoUpdate;
end;

procedure TCROComboItem.SetName(Value: String);
begin
  FName := Value; //se naum for setado o delphi limpa ao carregar o projeto ou o aplicativo
end;

procedure TCROComboItem.SetVisible(Value: Boolean);
begin
  FVisible := Value;
  DoUpdate;
end;

{ TCROComboItems }

procedure TCROComboItems.AssignTo(Dest: TPersistent);
begin
  if Dest is TCROComboItems
  then TCROComboItems(Dest).Assign(Self)
  else inherited AssignTo(Dest);
end;

procedure TCROComboItems.AssignValues(Value: TCROComboItems);
var
  I: Integer;
  P: TCROComboItem;
begin
  for I := 0 to Value.Count - 1 do
  begin
    P := FindItem(Value[I].Name);
    if P <> nil then
      P.Assign(Value[I]);
  end;
end;

constructor TCROComboItems.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TCROComboItem);
end;

function TCROComboItems.CreateItem(AName, ACaption: String;
  AVisible: Boolean): TCROComboItem;
begin
  Result := TCROComboItem.Create(Self, AName, ACaption, AVisible);
end;

function TCROComboItems.FindItem(const Value: string): TCROComboItem;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := TCROComboItem(inherited Items[I]);
    if AnsiCompareText(Result.Name, Value) = 0 then Exit;
  end;
  Result := nil;
end;

function TCROComboItems.GetItem(Index: Integer): TCROComboItem;
begin
  Result := TCROComboItem(inherited Items[Index]);
end;

function TCROComboItems.ItemByCaption(const Value: string): TCROComboItem;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := TCROComboItem(inherited Items[I]);
    if AnsiCompareText(Result.Caption, Value) = 0
    then Exit;
  end;
  Result := nil;
end;

function TCROComboItems.ItemByName(const Value: string): TCROComboItem;
begin
  Result := FindItem(Value);
end;

procedure TCROComboItems.SetItem(Index: Integer; Value: TCROComboItem);
begin
  inherited SetItem(Index, TCollectionItem(Value));
end;

end.
