unit CROListBox;

interface

uses
  Windows, SysUtils, Classes, Controls, StdCtrls, CROComponentsCommon, CROOperation,
  Graphics, uROTypes, TypInfo, Forms, Messages, Math, Dialogs;

type
  TCROListBoxItem = class
  public
    Data: TObject;
    Caption: String;
  end;

  TCROListBox = class(TCustomListBox, ICROCustomControl, ICROCustomListControl, ICROSkinControl)
  published
    property AutoComplete;
    property Align;
    property Anchors;
    property BiDiMode;
    property Color;
    property Columns;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property IntegralHeight;
    property ItemHeight;
    property Items;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollWidth;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnData;
    property OnDataFind;
    property OnDataObject;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  private
    FHiddenItemIndex: Integer;
    FProperties: TCROLookupProperties;
    FEnterToTab: Boolean;
    FHandleCursor: Boolean;
    FFrameStyle: TFrameStyle;
    FSelectedColor: TColor;
    FSelectedFont: TFont;
    FUnique: Boolean;
    procedure SetProperties(Value: TCROLookupProperties);
    function GetProperties: TCROLookupProperties;
    function GetSelected: TObject;
    procedure SetSelected(const Value: TObject);
    procedure SetFrameStyle(const Value: TFrameStyle);
    procedure SetSelectedFont(const Value: TFont);
    procedure SetHiddenItemIndex(const Value: Integer);
  protected
    procedure CreateWnd; override;//CreateWnd é criado logo após o Create e cria o handle para o componente
    function GetItemIndex: Integer; override;
    procedure SetItemIndex(const Value: Integer); override;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    property HiddenItemIndex: Integer read FHiddenItemIndex write SetHiddenItemIndex;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure Loaded; override;
  published
    property Properties: TCROLookupProperties read GetProperties write SetProperties;
    property EnterToTab: Boolean read FEnterToTab write FEnterToTab default True;
    property HandleCursor: Boolean read FHandleCursor write FHandleCursor default False;
    property Selected: TObject read GetSelected write SetSelected;
    property FrameStyle: TFrameStyle read FFrameStyle write SetFrameStyle;
    property SelectedFont: TFont read FSelectedFont write SetSelectedFont;
    property SelectedColor: TColor read FSelectedColor write FSelectedColor;
    property Unique: Boolean read FUnique write FUnique default False; 
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure UpdateValue; virtual; abstract;//lookupproperties naum chamam updatevalue
    procedure UpdateObject;
    procedure UpdateItems;
    procedure UpdateIndex;

    function CopyItem(const Index:Integer; Dest: TCROListBox): Boolean;
    function MoveItem(const Index:Integer; Dest: TCROListBox): Boolean;
    function MoveUp: Boolean;
    function MoveDown: Boolean;
    function CanReceiveItemFrom(ASource: TCROListBox; const Index: Integer): Boolean;

    procedure UpdateSkin;
    function GetItem(const Index: Integer): TCROListBoxItem;
  end;

implementation

const
  clSelectedColor: Integer = clHighlight;
  clSelectedFontColor: Integer = clHighlightText;

{ TCROListBox }

constructor TCROListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FProperties := TCROLookupProperties.Create(Self);
  FEnterToTab := True;
  FHandleCursor := False;
  FSelectedFont := TFont.Create;
  FSelectedFont.Color := clSelectedFontColor;
  FSelectedColor := clSelectedColor;
  FUnique := False;
end;

destructor TCROListBox.Destroy;
begin
  FSelectedFont.Free;
  FProperties.Free;
  inherited;
end;

procedure TCROListBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Message.DrawItemStruct^ do
  begin
    State := TOwnerDrawState(LongRec(itemState).Lo);
    Canvas.Handle := hDC;
    Canvas.Font := Font;
    Canvas.Brush := Brush;
    if (Integer(itemID) >= 0) and (
         {(odSelected in State) or}
         (FHiddenItemIndex = Integer(itemID))
       ) then
    begin
      Canvas.Brush.Color := FSelectedColor;
      Canvas.Font.Color := FSelectedFont.Color;
    end;
    if Integer(itemID) >= 0
    then DrawItem(itemID, rcItem, State)
    else Canvas.FillRect(rcItem);
    if (odFocused in State)
    then DrawFocusRect(hDC, rcItem);
    Canvas.Handle := 0;
  end;
end;

procedure TCROListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  Flags: Longint;
  Data: String;
  Item: TCROListBoxItem;
begin
  if Assigned(OnDrawItem)
  then OnDrawItem(Self, Index, Rect, State)
  else
  begin
    Canvas.FillRect(Rect);
    if Index < Count then
    begin
      Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
      if not UseRightToLeftAlignment then
        Inc(Rect.Left, 2)
      else
        Dec(Rect.Right, 2);
      Data := '';
      if (Style in [lbVirtual, lbVirtualOwnerDraw]) then
        Data := DoGetData(Index)
      else
      begin
        Item := GetItem(Index);
        if Assigned(Item)
        then Data := Item.Caption;
        Item.Free;
      end;
      DrawText(Canvas.Handle, PChar(Data), Length(Data), Rect, Flags);
    end;
  end;
end;

function TCROListBox.GetProperties: TCROLookupProperties;
begin
  Result := FProperties;
end;

function TCROListBox.GetSelected: TObject;
begin
  //(not HandleAllocated) garante q o Handle já foi criado para a janela, caso naum teste isso ocorrerá "Control Has No Parent Window"
  if (not HandleAllocated) or (FHiddenItemIndex = -1) or (FHiddenItemIndex > Items.Count-1)
  then Result := nil
  else Result := Items.Objects[FHiddenItemIndex];
end;

procedure TCROListBox.SetFrameStyle(const Value: TFrameStyle);
begin
  FFrameStyle := Value;
  case FFrameStyle of
    fs2D:
    begin
      Ctl3D := False;
      ControlStyle := ControlStyle - [csFramed];
      BorderStyle := bsNone;

      Ctl3D := False;
      ControlStyle := ControlStyle - [csFramed];
      BorderStyle := bsSingle;
    end;
    fs3D:
    begin
      Ctl3D := False;
      ControlStyle := ControlStyle - [csFramed];
      BorderStyle := bsNone;
      
      Ctl3D := True;
      ControlStyle := ControlStyle - [csFramed];
      BorderStyle := bsSingle;
    end;
    fsFramed:
    begin
      Ctl3D := True;
      ControlStyle := ControlStyle + [csFramed];
      BorderStyle := bsNone;
    end;
    fsNone:
    begin
      Ctl3D := False;
      ControlStyle := ControlStyle - [csFramed];
      BorderStyle := bsNone;
    end;
  end;
  Refresh;
end;

procedure TCROListBox.SetItemIndex(const Value: Integer);
begin
  inherited;
  HiddenItemIndex := Value;
end;

procedure TCROListBox.SetProperties(Value: TCROLookupProperties);
begin
  if Value <> nil
  then FProperties := Value;
end;

procedure TCROListBox.SetSelected(const Value: TObject);
begin
  SetItemIndex(Items.IndexOfObject(Value));
end;

procedure TCROListBox.UpdateIndex;
begin
  if FHandleCursor and Assigned(FProperties.ListHandle)
  then HiddenItemIndex := FProperties.ListHandle.ItemIndex;
end;

procedure TCROListBox.UpdateItems;
var i,AIndex: Integer; Item: TCROListBoxItem;
begin
  //carrega listhandle com os objetos da lista
  if Assigned(FProperties.ListHandle) and
     Assigned(FProperties.ListHandle.Data) then
  begin
    AIndex := FHiddenItemIndex;
    FProperties.ListHandle.DisableUpdate := True;
    Items.Clear;
    for i := 0 to FProperties.ListHandle.Count-1 do
    begin
      Item := GetItem(i);
      if Assigned(Item) then
      begin
        Items.AddObject(Item.Caption,Item.Data);
        Item.Free;
      end;
    end;
    if (FProperties.ListHandle.Count > 0)
    then HiddenItemIndex := Min(Max(0,AIndex),Items.Count-1)
    else HiddenItemIndex := -1;
    FProperties.ListHandle.DisableUpdate := False;
    UpdateIndex;
  end;
end;

procedure TCROListBox.UpdateObject;
var i: Integer; obj: TROComplexType;
begin
  //carrega listhandle com os objetos da lista
  if Assigned(FProperties.ListHandle) and
     Assigned(FProperties.ListHandle.Data) then
  begin
    FProperties.ListHandle.DisableUpdate := True;
    FProperties.ListHandle.Clear;
    for i := 0 to Items.Count-1 do
    begin
      obj := CreateROObject(FProperties.ListHandle.InternalCursorHandle.DataType, FProperties.ListHandle.SourceOperation.RODLLibrary);
      FProperties.ListHandle.AddObject(obj);
    end;
    FProperties.ListHandle.DisableUpdate := False;
  end;
end;

procedure TCROListBox.UpdateSkin;
begin
  if Assigned(FProperties.FontProperties) and Assigned(Font) then
  begin
    Font.Assign(FProperties.FontProperties.Font);
    FEnterToTab := FProperties.FontProperties.EnterToTab;
  end;
end;

function TCROListBox.GetItem(const Index: Integer): TCROListBoxItem;
var AData: TObject; S,AExpression:String; PropInfo: PPropInfo;
begin
  AData := FProperties.ListHandle.Items[Index];
  if Assigned(AData) then
  begin
    AExpression := FProperties.Expression;
    GetPropInfoFromExpression(GetTrueOwner(Self.Owner,AExpression), AExpression, AData, PropInfo);
    S := GetPropValue(AData, PropInfo^.Name);
    Result := TCROListBoxItem.Create;
    Result.Data := AData;
    Result.Caption := S;
  end
  else
    Result := nil;
end;

function TCROListBox.CopyItem(const Index: Integer; Dest: TCROListBox): Boolean;
begin
  Result := False;
  if Assigned(Dest) and
     Assigned(Properties.ListHandle) and
     Assigned(Dest.Properties.ListHandle) and
     Dest.CanReceiveItemFrom(Self,Index)
  then Result := Properties.ListHandle.CopyObject(Index,Dest.Properties.ListHandle)
end;

function TCROListBox.MoveItem(const Index: Integer; Dest: TCROListBox): Boolean;
begin
  Result := False;
  if Assigned(Dest) and
     Assigned(Properties.ListHandle) and
     Assigned(Dest.Properties.ListHandle) and
     Dest.CanReceiveItemFrom(Self,Index)
  then Result := Properties.ListHandle.MoveObject(Index,Dest.Properties.ListHandle)
end;

function TCROListBox.MoveDown: Boolean;
begin
  Result := False;
  if Assigned(Properties.ListHandle) and (FHiddenItemIndex < Count-1) then
  begin
    Result := Properties.ListHandle.MoveDown(FHiddenItemIndex);
    HiddenItemIndex := FHiddenItemIndex +1;
  end;
end;

function TCROListBox.MoveUp: Boolean;
begin
  Result := False;
  if Assigned(Properties.ListHandle) and (ItemIndex > 0) then
  begin
    Result := Properties.ListHandle.MoveUp(ItemIndex);
    HiddenItemIndex := FHiddenItemIndex -1;
  end;
end;

function TCROListBox.GetItemIndex: Integer;
var AResult: Integer;
begin
  AResult := inherited GetItemIndex;
  if (AResult = -1) and (Items.Count > 0) and (FHiddenItemIndex > -1) and (FHiddenItemIndex < Items.Count)
  then Result := FHiddenItemIndex
  else Result := AResult;
end;

procedure TCROListBox.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  HiddenItemIndex := Min(ItemIndex,Items.Count-1);
end;

procedure TCROListBox.SetSelectedFont(const Value: TFont);
begin
  FSelectedFont.Assign(Value);
end;

procedure TCROListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key in [VK_UP,VK_LEFT]
  then HiddenItemIndex := Max(ItemIndex-1,0)
  else
  if Key in [VK_DOWN,VK_RIGHT]
  then HiddenItemIndex := Min(ItemIndex+1,Items.Count-1);
end;

procedure TCROListBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  HiddenItemIndex := ItemIndex;
end;

procedure TCROListBox.SetHiddenItemIndex(const Value: Integer);
begin
  FHiddenItemIndex := Value;
  if FHandleCursor and (FProperties.ListHandle.ItemIndex <> FHiddenItemIndex)
  then FProperties.ListHandle.ItemIndex := FHiddenItemIndex;
  Refresh;
end;

procedure TCROListBox.CreateWnd;
begin
  inherited;
  Style := lbOwnerDrawFixed;
  PostMessage(Handle,WM_NCPAINT,0,0);
end;

procedure TCROListBox.CMShowingChanged(var Message: TMessage);
begin
  inherited;
  UpdateItems;
end;

function TCROListBox.CanReceiveItemFrom(ASource: TCROListBox; const Index: Integer): Boolean;
begin
  Result := (not FUnique) or (Items.IndexOf(ASource.Items[Index]) = -1);
end;

procedure TCROListBox.Loaded;
begin
  inherited;
  UpdateItems;
end;

end.
