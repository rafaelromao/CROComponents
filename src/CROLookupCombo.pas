unit CROLookupCombo;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, CROOperation, Messages,
  Dialogs, TypInfo, CROComponentsCommon, Windows, Graphics, uROTypes;

type
  TCROLookupCombo = class(TCustomCombobox, ICROCustomControl, ICROCustomListControl, ICROSkinControl)
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
    FInternalItemIndex: Integer;
    FUpdatingValue: Boolean;
    FReadOnly: Boolean;
    FShowButton: Boolean;
    FProperties: TCROProperties;
    FLookupProperties: TCROLookupProperties;
    FHandleCursor: Boolean;
    FAllowNull: Boolean;
    FEnterToTab: Boolean;

    procedure SetProperties(Value: TCROProperties);
    function GetProperties: TCROProperties;
    procedure SetLookupProperties(const Value: TCROLookupProperties);
    function GetSelected: TObject;
    procedure SetReadOnly(Value: Boolean);
    procedure SetShowButton(Value: Boolean);
    procedure InternalSetItemIndex(AItemIndex: Integer);
  protected
    { Protected declarations }
    procedure CreateWnd; override;
    procedure Change; override;
    procedure DoExit; override;
    procedure Select; override;
    procedure SetItemIndex(const Value: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure UpdateValue;
    procedure UpdateObject;
    procedure UpdateItems;
    procedure UpdateIndex;

    procedure UpdateSkin;
  published
    { Published declarations }
    property AllowNull: Boolean read FAllowNull write FAllowNull default False;
    property Properties: TCROProperties read GetProperties write SetProperties;
    property LookupProperties: TCROLookupProperties read FLookupProperties write SetLookupProperties;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property ShowButton: Boolean read FShowButton write SetShowButton default True;
    property HandleCursor: Boolean read FHandleCursor write FHandleCursor;
    property EnterToTab: Boolean read FEnterToTab write FEnterToTab default True;
  end;

implementation

{ TCROLookupCombo }

procedure TCROLookupCombo.Change;
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

constructor TCROLookupCombo.Create(AOwner: TComponent);
begin
  inherited;
  FHandleCursor := False; //indicates if the cursorhandle must be updated when selection changes
  FProperties := TCROProperties.Create(Self);
  FLookupProperties := TCROLookupProperties.Create(Self);
  FShowButton := True;
  FUpdatingValue := False;
  FEnterToTab := True;
end;

procedure TCROLookupCombo.CreateWnd;
begin
  inherited;
  inherited Style := csDropDownList;
end;

destructor TCROLookupCombo.Destroy;
begin
  FProperties.Free;
  FLookupProperties.Free;
  inherited;
end;

procedure TCROLookupCombo.DoExit;
begin
  UpdateObject;
  inherited;
end;

function TCROLookupCombo.GetProperties: TCROProperties;
begin
  Result := FProperties;
end;

function TCROLookupCombo.GetSelected: TObject;
begin
  if (not HandleAllocated) or (ItemIndex = -1)
  then Result := nil
  else Result := FLookupProperties.ListHandle.Items[ItemIndex];
end;

procedure TCROLookupCombo.InternalSetItemIndex(AItemIndex: Integer);
begin
  try
    FUpdatingValue := True;
    ItemIndex := AItemIndex;
  finally
    FUpdatingValue := False;
  end;
end;

procedure TCROLookupCombo.KeyDown(var Key: Word; Shift: TShiftState);
var cKey : Char;
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

  inherited;

  if FReadOnly then
  begin
    case Key of
      VK_DOWN: Key := VK_RIGHT;
      VK_UP  : Key := VK_LEFT;
    end;
  end
  else
  begin
    if Key = VK_DELETE then
    begin
      cKey := Chr(Key);
      KeyPress(cKey);
    end;
  end;
end;

procedure TCROLookupCombo.KeyPress(var Key: Char);
begin
  if Assigned(FProperties.CursorHandle) then
  begin
    if (not FProperties.CursorHandle.AutoEdit) and
       (not (FProperties.CursorHandle.State in [csEdit,csInsert])) {and
       (not (Key in [#13]))}
    then Key := #0;
  end;

  if FReadOnly then
  begin
    Key := #0;
  end
  else
  begin
    if FAllowNull and (Ord(Key) in [VK_DELETE, VK_BACK]) then
    begin
      inherited;
      ItemIndex := -1;
    end
    else inherited;
  end;

  CheckFocus(Self,Key,FEnterToTab);
end;

procedure TCROLookupCombo.Loaded;
begin
  inherited;
  UpdateItems;
  UpdateValue;
end;

procedure TCROLookupCombo.Select;
begin
  if (not FReadOnly) or (FUpdatingValue) then
  begin
    inherited;
    if HandleCursor then
    begin
      if Assigned(FLookupProperties.ListHandle) then
      begin
        FInternalItemIndex := ItemIndex;
        FLookupProperties.ListHandle.ItemIndex := ItemIndex;
      end;
    end;
  end
  else
  begin
    InternalSetItemIndex(FInternalItemIndex);
  end;
end;

procedure TCROLookupCombo.SetItemIndex(const Value: Integer);
begin
  inherited;
  Select;
end;

procedure TCROLookupCombo.SetLookupProperties(
  const Value: TCROLookupProperties);
begin
  if Value <> nil
  then FLookupProperties := Value;
end;

procedure TCROLookupCombo.SetProperties(Value: TCROProperties);
begin
  if Value <> nil
  then FProperties := Value;
end;

procedure TCROLookupCombo.SetReadOnly(Value: Boolean);
begin
  FReadOnly := Value;
end;

procedure TCROLookupCombo.SetShowButton(Value: Boolean);
begin
  FShowButton := Value;

  if FShowButton
  then inherited Style := csDropDownList
  else inherited Style := csSimple;
end;

procedure TCROLookupCombo.UpdateIndex;
begin
  //does not update index for lookupcombos
end;

procedure TCROLookupCombo.UpdateItems;
var
  AData: TObject;
  AExpression: String;
  V: Variant;
  i: Integer;
  PropInfo: PPropInfo;
begin
  if Assigned(FLookupProperties.ListHandle) then
  begin
    Self.Items.Clear;
    //handle cursor, updatecursorhandle for listhandle
    if FLookupProperties.ListHandle.Count > 0 then
    begin
      for i := 0 to FLookupProperties.ListHandle.Count-1 do
      begin
        AData := FLookupProperties.ListHandle.Items[i];
        if Assigned(AData) then
        begin
          AExpression := FLookupProperties.Expression;
          GetPropInfoFromExpression(GetTrueOwner(Self.Owner,AExpression), AExpression, AData, PropInfo);
          V := GetPropValue(AData, PropInfo^.Name);
          Self.Items.Add(V); //**
          //Self.AddItem(V,AData);
        end;
      end;
    end;
    UpdateValue;
  end;
end;

procedure TCROLookupCombo.UpdateObject;
var AData,ASelected: TObject; ANewData: TROComplexType;
begin
  ASelected := GetSelected;
  if Assigned(FProperties.CursorHandle) and
     Assigned(FProperties.CursorHandle.Data) and
     Assigned(ASelected) then
  begin
    if (ItemIndex > -1) then
    begin
      FProperties.GetValueFromExpression(AData);
      if (AData <> ASelected) then
      begin
        //cria um novo objeto pq qnd se seta essa propriedade sem isso é usada a mesma referencia q está
        //no listhandle do LookupProperties e no Destroy do ROComplexType, o RO destroi todas as propriedades
        //do tipo tkClass, entre elas a referencia ainda mantida pelo seu verdadeiro dono, o ListHandle mensionado
        //logo um novo objeto é necessário para evitar um Access Violation
        ANewData := CreateROObject(ASelected.ClassName,FProperties.CursorHandle.SourceOperation.RODLLibrary);
        AssignROObject(ASelected,ANewData);
        FProperties.SetValueToExpression(ANewData, Self.LookupProperties.ListHandle.InternalCursorHandle.DataType)
      end;
    end  
    else UpdateValue;
  end;
end;

procedure TCROLookupCombo.UpdateSkin;
begin
  if Assigned(FProperties.FontProperties) and Assigned(Font) then
  begin
    Font.Assign(FProperties.FontProperties.Font);
    FEnterToTab := FProperties.FontProperties.EnterToTab;
  end;
end;

procedure TCROLookupCombo.UpdateValue;
var
  K: Integer;
  AIdentifierCode,AObjectCode: Variant;
  PropInfo: PPropInfo;
  Instance: TObject;
  FoundObject: Boolean;
  AExpression: String;
begin
  FLoading := True;
  FoundObject := False;
  if (not (csDestroying in ComponentState)) and
     (Assigned(FProperties.CursorHandle)) and
     (Assigned(FProperties.CursorHandle.Data)) then
  begin
    //select the object referenced by CursorHandle
    if (FLookupProperties.IdentifierExpression <> '') then
    begin
      //get value of identifier field
      if FProperties.Expression <> ''
      then AIdentifierCode := FProperties.CursorHandle.GetDataProperty(FProperties.Expression+'.'+FLookupProperties.IdentifierExpression)
      else AIdentifierCode := FProperties.CursorHandle.GetDataProperty(FLookupProperties.IdentifierExpression);
      //search for some item with the same identifier in objectlist
      for K := 0 to Items.Count-1 do
      begin
        Instance := FLookupProperties.ListHandle.Items[K];
        if Assigned(Instance) then
        begin
          try
            AExpression := FLookupProperties.IdentifierExpression;
            GetPropInfoFromExpression(GetTrueOwner(Self.Owner,AExpression), AExpression, Instance, PropInfo);
            if PropInfo^.PropType^.Kind in [tkInteger, tkChar, tkFloat,
                      tkString, tkWChar, tkLString, tkWString, tkVariant, tkInt64, tkEnumeration]
            then AObjectCode := GetPropValue(Instance, PropInfo^.Name)
            else raise Exception.Create('The IdentifierExpression Property is not from the expected type.');

            if AObjectCode = AIdentifierCode then
            begin
              InternalSetItemIndex(K);
              FoundObject := True;
              Break;
            end;
          except
            ShowMessage('debug message: Object Retrivied is a '+instance.ClassName);
          end;
        end;
      end;
    end;
  end;
  if not FoundObject
  then InternalSetItemIndex(-1);
  FLoading := False;
end;

end.
