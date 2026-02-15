unit CROGrid;

interface

uses
  SysUtils, Classes, Controls, Grids, CROOperation, Dialogs, ComCtrls, Contnrs,
  Types, uROTypes, Windows, TypInfo, CROComponentsCommon, CROCheckBox, CROComboBox,
  CROLookupCombo, StdCtrls, Messages, Math, Graphics, CROSkinProperties, uRODL;

type
  TCROGrid = class;
  TCROGridObject = class;
  TCROGridColumn = class;
  TCROGridColumns = class;
  TCROInplaceEdit = class;

  TCROInplaceEdit = class(TInplaceEdit, ICROCustomControl, ICROSkinControl, ICROEdit)
  private
    { Private declarations }
    FLoading: Boolean;
    FProperties: TCROProperties;

    procedure SetProperties(Value: TCROProperties);
    function GetProperties: TCROProperties;
  protected
    { Protected declarations }
    procedure Change; override;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WndProc(var Message: TMessage); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure UpdateValue;
    procedure UpdateObject;

    procedure UpdateSkin;
  published
    { Published declarations }
    property Properties: TCROProperties read GetProperties write SetProperties;
    property MaxLength;
  end;

  //container to objects
  TCROGridObject = class(TComponent)
  private
    FGrid: TCROGrid;
    FRow: Integer;
    function GetItemIndex: Integer;
  public
    property Row: Integer read FRow write FRow;
    property ItemIndex: Integer read GetItemIndex;
    constructor Create(AOwner: TCROGrid; const ARow: Integer); reintroduce;
    destructor Destroy; override;
    function GetValueFromExpression(const ACol: Word): String;
  end;

  //Grid Column Item
  TCROGridColumn = class(TCollectionItem)
  private
    FName: string;
    FWidth: Word;
    FDisplayName: String;
    FExpression: String;
    FEditor: TWinControl;
    FEditorType: String;
    FEditorTypes: TStrings;
    FReadOnly: Boolean;
    FAutoSize: Boolean;
    FIteratorColumn: Boolean;
    procedure LoadEditorProperties;
    procedure SetWidth(const Value: Word);
    procedure SetExpression(const Value: String);
    procedure SetEditorType(const Value: String);
    procedure SetEditor(const Value: TWinControl);
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property EditorTypes: TStrings read FEditorTypes;
  published
    property Name: string read FName write FName;
    property Width: Word read FWidth write SetWidth default 64;
    property Expression: String read FExpression write SetExpression;
    property DisplayName;
    property EditorType: String read FEditorType write SetEditorType;
    property Editor: TWinControl read FEditor write SetEditor;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property AutoSize: Boolean read FAutoSize write FAutoSize default True;
  end;

  //Grid Column Collection
  TCROGridColumns = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TCROGridColumn;
    procedure SetItem(Index: Integer; Value: TCROGridColumn);
    procedure UpdateCols;
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TComponent);
    function Add: TCROGridColumn;
    property Items[Index: Integer]: TCROGridColumn read GetItem write SetItem; default;
  end;

  //grid components
  TCROGrid = class(TCustomDrawGrid, ICROCustomListControl, ICROSkinControl)
  published
    property Align;
    property Anchors;
    property BiDiMode;
//    property BorderStyle; //naum deve ser alterado
//    property Color; //*** inserido pelo properties
    property Constraints;
//    property Ctl3D;       //naum deve ser alterado
    property DefaultColWidth;
    property DefaultRowHeight;
//    property DefaultDrawing; //naum deve ser alterado
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
//    property FixedColor;  //*** inserido pelo properties
//    property Font;     //*** inserido pelo properties
//    property GridLineWidth; //naum deve ser alterado
//    property Options;  //*** inserido pelo properties
    property ParentBiDiMode;
//    property ParentColor;
//    property ParentCtl3D;
//    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property Visible;
    property OnClick;
    property OnColumnMoved;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawCell;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetEditMask;
    property OnGetEditText;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnRowMoved;
    property OnSelectCell;
    property OnSetEditText;
    property OnStartDock;
    property OnStartDrag;
    property OnTopLeftChanged;
  private
    FFixedWidth: Integer;
    FOriginalWidth: Integer;
    FUsefullWidth: Integer;
    FAvailableWidth: Integer;
    FScrollAreaWidth: Integer;
    FResizingColumns: Boolean;
    FMouseDown: Boolean;
    FProperties: TCROGridProperties;
    FColumns: TCROGridColumns;
    FObjects: TObjectList;
    FFixedFont: TFont;
    FBorderColorBottom: TColor;
    FBorderColorTop: TColor;
    FBorderColorLeft: TColor;
    FBorderColorRight: TColor;
    FFocusedColor: TColor;
    FSelectedColor: TColor;
    FEvenColor: TColor;
    FSelectedFont: TFont;
    FFocusedFont: TFont;
    FFFocusedFont: TFont;
    function GetObjects(Index: Integer): TCROGridObject;
    procedure SetObjects(Index: Integer; const Value: TCROGridObject);
    function GetOwnsObjects: Boolean;
    procedure SetOwnsObjects(const Value: Boolean);

    procedure CheckBoxMouseDown(Sender: TObject; Button: TMouseButton;
       Shift: TShiftState; X, Y: Integer);
    procedure EditorKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditorExit(Sender: TObject);
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure SetFixedFont(const Value: TFont);
    procedure SetBorderColorBottom(const Value: TColor);
    procedure SetBorderColorLeft(const Value: TColor);
    procedure SetBorderColorRight(const Value: TColor);
    procedure SetBorderColorTop(const Value: TColor);
    procedure SetFocusedColor(const Value: TColor);
    procedure SetSelectedColor(const Value: TColor);
    procedure SetEvenColor(const Value: TColor);
    procedure SetSelectedFont(const Value: TFont);
    procedure SetFFocusedFont(const Value: TFont);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function CreateEditor: TInplaceEdit; override;
    procedure SetName(const NewName: TComponentName); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    function GetEditText(ACol, ARow: Longint): string; override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState); override;
    procedure ColWidthsChanged; override;
    procedure ColumnMoved(FromIndex, ToIndex: Longint); override;
    procedure RowMoved(FromIndex, ToIndex: Longint); override;
    function GetProperties: TCROGridProperties;
    procedure SetProperties(Value: TCROGridProperties);
    procedure UpdateColumn(const Index: Word);
    procedure UpdateColumns;
    procedure Loaded; override;
    procedure DoUpdateHandle(Sender: TObject);
    procedure DoRefresh(Sender: TObject);
    procedure Resize; override;
  public
    procedure UpdateItems;
    procedure UpdateIndex;
    procedure UpdateSkin;
    function AddObject: TCROGridObject;
    function RemoveObject: Boolean;
    function ObjectCount: Integer;
    procedure Clear;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Objects[Index: Integer]: TCROGridObject read GetObjects write SetObjects;
    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;

    property FixedFont: TFont read FFixedFont write SetFixedFont;
    property BorderColorTop: TColor read FBorderColorTop write SetBorderColorTop;
    property BorderColorLeft: TColor read FBorderColorLeft write SetBorderColorLeft;
    property BorderColorRight: TColor read FBorderColorRight write SetBorderColorRight;
    property BorderColorBottom: TColor read FBorderColorBottom write SetBorderColorBottom;
    property FocusedColor: TColor read FFocusedColor write SetFocusedColor;
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor;
    property EvenColor: TColor read FEvenColor write SetEvenColor;
    property SelectedFont: TFont read FSelectedFont write SetSelectedFont;
    property FocusedFont: TFont read FFFocusedFont write SetFFocusedFont;
  published
    property Properties: TCROGridProperties read GetProperties write SetProperties;
    property Columns: TCROGridColumns read FColumns write FColumns;
  end;

implementation

const
  EDITOR_NONE_INDEX = -1;
  EDITOR_DEFAULT_INDEX = 0;
  EDITOR_CHECKBOX_INDEX = 1;
  EDITOR_COMBOBOX_INDEX = 2;
  EDITOR_LOOKUPCOMBO_INDEX = 3;

  EDITOR_DEFAULT_NAME = 'Default';
  EDITOR_CHECKBOX_NAME = 'CheckBox';
  EDITOR_COMBOBOX_NAME = 'ComboBox';
  EDITOR_LOOKUPCOMBO_NAME = 'LookupCombo';

{ TCROInplaceEdit }

procedure TCROInplaceEdit.Change;
begin
  //deve ser feito este controle em todos os componentes de ediçãod e dados
  if Assigned(FProperties.CursorHandle) and
     Assigned(FProperties.CursorHandle.Data) and
     (not FLoading) and
     (FProperties.CursorHandle.AutoEdit) then
  begin
    //TODO implementar autoedit
    FProperties.CursorHandle.Edit;
  end;
  inherited;
end;

constructor TCROInplaceEdit.Create(AOwner: TComponent);
begin
  inherited;
  FProperties := TCROProperties.Create(Self);
end;

destructor TCROInplaceEdit.Destroy;
begin
  FProperties.Free;

  inherited;
end;

function TCROInplaceEdit.GetProperties: TCROProperties;
begin
  Result := FProperties;
end;

procedure TCROInplaceEdit.KeyDown(var Key: Word; Shift: TShiftState);
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

  if Assigned(OnKeyDown)
  then OnKeyDown(Self, Key, Shift);
  inherited;
end;

procedure TCROInplaceEdit.KeyPress(var Key: Char);
begin
  if Assigned(FProperties.CursorHandle) then
  begin
    if (not FProperties.CursorHandle.AutoEdit) and
       (not (FProperties.CursorHandle.State in [csEdit,csInsert])) {and
       (not (Key in [#13]))}
    then Key := #0;
  end;

  if not ValidateEditKey(FProperties.DataType,Key,Self)
  then Key := #0;
  
  inherited;
end;

procedure TCROInplaceEdit.SetProperties(Value: TCROProperties);
begin
  if Value <> nil
  then FProperties := Value;
end;

procedure TCROInplaceEdit.UpdateObject;
var V: Variant;
begin
  if Assigned(FProperties.CursorHandle) and
     Assigned(FProperties.CursorHandle.Data) then
  begin
    FProperties.GetValueFromExpression(V);
    if V <> Self.Text //para otimização, assim naum atualiza desnecessariamente todos os objetos
    then FProperties.SetValueToExpression(Self.Text);
  end;
end;

procedure TCROInplaceEdit.UpdateSkin;
begin
  if Assigned(FProperties.FontProperties) and Assigned(Font)
  then Font.Assign(FProperties.FontProperties.Font);
end;

procedure TCROInplaceEdit.UpdateValue;
var V: Variant;
begin
  if (Assigned(FProperties.CursorHandle)) and
     (Assigned(FProperties.CursorHandle.Data)) then
  begin
    FProperties.GetValueFromExpression(V);
    FLoading := True;
    Self.Text := V;
    FLoading := False;
  end
  else
    Self.Text := ''
end;

procedure TCROInplaceEdit.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  Change;
end;

procedure TCROInplaceEdit.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_KILLFOCUS: UpdateObject;
  end;
  inherited;
end;

{ TCROGridObject }

constructor TCROGridObject.Create(AOwner: TCROGrid; const ARow: Integer);
begin
  inherited Create(AOwner);
  FGrid := AOwner;
  Row := ARow;
end;

destructor TCROGridObject.Destroy;
begin
  inherited;
end;

function TCROGridObject.GetItemIndex: Integer;
begin
  Result := FRow - 1;
end;

function TCROGridObject.GetValueFromExpression(const ACol: Word): String;
var S,AExpression: String; AData: TObject; PropInfo: PPropInfo;
begin
  Result := '';
  if FGrid.Columns.Count <= ACol
  then Exit;

  //draw components
  if (ItemIndex > -1) and (ItemIndex < FGrid.FProperties.ListHandle.Count) then
  begin
    case FGrid.Columns[ACol].FEditorTypes.IndexOf(FGrid.Columns[ACol].EditorType) of
      //default editor
      EDITOR_DEFAULT_INDEX, EDITOR_CHECKBOX_INDEX, EDITOR_COMBOBOX_INDEX:
      begin
        AData := FGrid.FProperties.ListHandle.Items[ItemIndex];
        if Assigned(AData) then
        begin
          AExpression := FGrid.Columns[ACol].Expression;
          GetPropInfoFromExpression(GetTrueOwner(Self.Owner,AExpression), AExpression, AData, PropInfo);
          S := GetPropValue(AData, PropInfo^.Name);
        end
        else
          S := '';
        //combobox
        if FGrid.Columns[ACol].EditorType = EDITOR_COMBOBOX_NAME
        then S := TCROComboBox(FGrid.Columns[ACol].Editor).CaptionOfName(S);
      end;
      //lookupcombo
      EDITOR_LOOKUPCOMBO_INDEX:
      begin
        AData := FGrid.FProperties.ListHandle.Items[ItemIndex];
        if Assigned(AData) then
        begin
          AExpression := FGrid.Columns[ACol].Expression+'.'+TCROLookupCombo(FGrid.Columns[ACol].Editor).LookupProperties.Expression;
          GetPropInfoFromExpression(GetTrueOwner(Self.Owner,AExpression), AExpression, AData, PropInfo);
          S := GetPropValue(AData, PropInfo^.Name);
        end
        else
          S := '';
      end;
    end;
    Result := S;
  end;
end;

{ TCROGridColumns }

function TCROGridColumns.Add: TCROGridColumn;
begin
  Result := TCROGridColumn(inherited Add);
  UpdateCols;
end;

constructor TCROGridColumns.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TCROGridColumn);
end;

function TCROGridColumns.GetItem(Index: Integer): TCROGridColumn;
begin
  Result := TCROGridColumn(inherited Items[Index]);
end;

procedure TCROGridColumns.SetItem(Index: Integer; Value: TCROGridColumn);
begin
  inherited SetItem(Index, TCROGridColumn(Value));
end;

procedure TCROGridColumns.Update(Item: TCollectionItem);
begin
  inherited;
  if Item <> nil
  then TCROGrid(Owner).UpdateColumn(Item.Index)
  else TCROGrid(Owner).UpdateColumns;
end;

procedure TCROGridColumns.UpdateCols;
begin
  TCROGrid(Owner).UpdateColumns;
end;

{ TCROGridColumn }

constructor TCROGridColumn.Create(Collection: TCollection);
begin
  inherited;
  Name := TCROGrid(TComponent(Collection.Owner)).Name + 'Column' + IntToStr(Index+1);
  DisplayName := Name;
  Width := 64;
  FIteratorColumn := False;
  FEditorTypes := TStringList.Create;
  FEditorTypes.Insert(EDITOR_DEFAULT_INDEX,EDITOR_DEFAULT_NAME);
  FEditorTypes.Insert(EDITOR_CHECKBOX_INDEX,EDITOR_CHECKBOX_NAME);
  FEditorTypes.Insert(EDITOR_COMBOBOX_INDEX,EDITOR_COMBOBOX_NAME);
  FEditorTypes.Insert(EDITOR_LOOKUPCOMBO_INDEX,EDITOR_LOOKUPCOMBO_NAME);
  FEditorType := 'Default';
  FAutoSize := True;
end;

destructor TCROGridColumn.Destroy;
begin
  if Assigned(FEditor)
  then FEditor.Free;
  FEditorTypes.Free;
  inherited;
end;

procedure TCROGridColumn.SetEditorType(const Value: String);
begin
  if (FEditorTypes.IndexOf(Value) <> EDITOR_DEFAULT_INDEX) and
     (not Assigned(TCROGrid(Collection.Owner).Properties.ListHandle)) and
     (not (csLoading in TCROGrid(Collection.Owner).ComponentState))
  then raise Exception.Create('Grid must have a listhandle!');

  FEditorType := Value;
  if Assigned(FEditor)
  then FreeAndNil(FEditor);

  case FEditorTypes.IndexOf(FEditorType) of
    EDITOR_CHECKBOX_INDEX:
    begin
      FEditor := TCROCheckBox.Create(TWinControl(Collection.Owner));
      FEditor.Parent := TCROGrid(Collection.Owner);
      TCROCheckBox(FEditor).OnMouseDown := TCROGrid(Collection.Owner).CheckBoxMouseDown;
    end;
    EDITOR_COMBOBOX_INDEX:
    begin
      FEditor := TCROComboBox.Create(TWinControl(Collection.Owner));
      FEditor.Parent := TCROGrid(Collection.Owner).Parent;
      TCROComboBox(FEditor).OnKeyDown := TCROGrid(Collection.Owner).EditorKeyDown;
      TCROComboBox(FEditor).OnExit    := TCROGrid(Collection.Owner).EditorExit;
    end;
    EDITOR_LOOKUPCOMBO_INDEX:
    begin
      FEditor := TCROLookupCombo.Create(TWinControl(Collection.Owner));
      FEditor.Parent := TCROGrid(Collection.Owner).Parent;
      TCROLookupCombo(FEditor).OnKeyDown := TCROGrid(Collection.Owner).EditorKeyDown;
      TCROLookupCombo(FEditor).OnExit    := TCROGrid(Collection.Owner).EditorExit;
    end;
    else
    begin
      FEditorType := FEditorTypes[EDITOR_DEFAULT_INDEX];
    end;
  end;

  if Assigned(FEditor) then
  begin
    FEditor.Name   := TCROGrid(Collection.Owner).Name+'Editor'+IntToStr(Index+1);
    FEditor.Height := TCROGrid(Collection.Owner).DefaultRowHeight;
    FEditor.Visible := False;
    FEditor.SetSubComponent(True);
    FEditor.Left := -1000;

    //create properties if its was not created yet
    if not (csLoading in TWinControl(Collection.Owner).ComponentState) then
    begin
      LoadEditorProperties();
    end;
  end;
end;

function TCROGridColumn.GetDisplayName: string;
begin
  result := FDisplayName;
end;

procedure TCROGridColumn.SetDisplayName(const Value: string);
begin
  FDisplayName := Value;
  TCROGrid(Collection.Owner).Repaint;
end;

procedure TCROGridColumn.SetExpression(const Value: String);
begin
  FExpression := Value;
end;

procedure TCROGridColumn.SetWidth(const Value: Word);
begin
  FWidth := Value;
  TCROGrid(Collection.Owner).UpdateColumn(Index);
end;

//----------------- end support objects

procedure TCROGridColumn.SetEditor(const Value: TWinControl);
begin
 //
end;

procedure TCROGridColumn.LoadEditorProperties;

          procedure LoadCheckBoxes;
          begin
            TCROCheckBox(FEditor).Properties.CursorHandle := TCROGrid(Collection.Owner).Properties.ListHandle.InternalCursorHandle;
            TCROCheckBox(FEditor).Properties.Expression := Self.Expression;
            if (TCROCheckBox(FEditor).Properties.DataType <> 'Boolean') then
            begin
              FEditorType := FEditorTypes[EDITOR_DEFAULT_INDEX];
              FreeAndNil(FEditor);
              raise Exception.Create('Expression is not a boolean property');
            end
            else
            begin
              TCROCheckBox(FEditor).Width := 13;
              TCROCheckBox(FEditor).Height := 13;
              TCROCheckBox(FEditor).Caption := '';
            end;
          end;

          procedure LoadComboBox;
          begin
            try
              if Assigned(TCROGrid(Collection.Owner).Properties.ListHandle) and
                 (TCROGrid(Collection.Owner).Properties.ListHandle.InternalCursorHandle.DataType <> '')  then//**
              begin
                TCROGrid(Collection.Owner).Properties.ListHandle.Expression := TCROGrid(Collection.Owner).Properties.ListHandle.Expression;//***
                TCROComboBox(FEditor).Properties.CursorHandle := TCROGrid(Collection.Owner).Properties.ListHandle.InternalCursorHandle;
                TCROComboBox(FEditor).Properties.Expression := Self.Expression;
              end;
              if TCROComboBox(FEditor).ShowButton
              then TCROComboBox(FEditor).Width := Self.Width + 4
              else TCROComboBox(FEditor).Width := Self.Width;
            except
              on E:Exception do
              begin
                FEditorType := FEditorTypes[EDITOR_DEFAULT_INDEX];
                FreeAndNil(FEditor);
                raise;
              end;
            end;
          end;

          procedure LoadLookupCombo;
          begin
            TCROLookupCombo(FEditor).Properties.CursorHandle := TCROGrid(Collection.Owner).Properties.ListHandle.InternalCursorHandle;
            TCROLookupCombo(FEditor).Properties.Expression := Self.Expression;
            if TCROLookupCombo(FEditor).ShowButton
            then TCROLookupCombo(FEditor).Width := Self.Width + 4
            else TCROLookupCombo(FEditor).Width := Self.Width;
          end;

begin
  if Assigned(FEditor) and (not (csLoading in TCROGrid(Collection.Owner).ComponentState)) then
  begin
    case FEditorTypes.IndexOf(FEditorType) of
      EDITOR_CHECKBOX_INDEX:
      begin
        LoadCheckBoxes;
      end;
      EDITOR_COMBOBOX_INDEX:
      begin
        LoadComboBox;
      end;
      EDITOR_LOOKUPCOMBO_INDEX:
      begin
        LoadLookupCombo;
      end;
    end;
  end;  
end;

procedure TCROGridColumn.AssignTo(Dest: TPersistent);
begin
  inherited;
  TCROGridColumn(Dest).Width := FWidth;
  TCROGridColumn(Dest).DisplayName := FDisplayName;
  TCROGridColumn(Dest).Expression := FExpression;
  TCROGridColumn(Dest).EditorType := FEditorType;
  TCROGridColumn(Dest).ReadOnly := FReadOnly;
  TCROGridColumn(Dest).AutoSize := FAutoSize;
end;

{ TCROGrid }

constructor TCROGrid.Create(AOwner: TComponent);
begin
  inherited;
  ParentCtl3D := False;
  Ctl3D := False;

//  if Assigned(Owner) and (Owner.InheritsFrom(TWinControl)) and (Parent = nil)
//  then Parent := TWinControl(Owner);

  FProperties := TCROGridProperties.Create(Self);
  FProperties.OnInternalUpdateHandle := DoUpdateHandle;

  FColumns := TCROGridColumns.Create(Self);
  FObjects := TObjectList.Create;
  FObjects.OwnsObjects := True;

  FMouseDown := False;
  FFixedFont := TFont.Create;
  FFixedFont.OnChange := DoRefresh;
  DefaultDrawing := False;

  Color := clColor;
  FBorderColorTop := clBorderColorTop;
  FBorderColorLeft := clBorderColorLeft;
  FBorderColorRight := clBorderColorRight;
  FBorderColorBottom := clBorderColorBottom;
  FFocusedColor := clFocusedColor;
  FSelectedColor := clSelectedColor;
  FEvenColor := clEvenColor;

  FSelectedFont := TFont.Create;
  FSelectedFont.OnChange := DoRefresh;
  FFocusedFont  := TFont.Create;
  FFocusedFont.OnChange := DoRefresh;

  ColCount := 1;
  RowCount := 2;
  FixedRows := 1;
  DefaultRowHeight := 16;
  Options := Options - [goEditing,goRangeSelect];
end;

destructor TCROGrid.Destroy;
begin
  if Assigned(InplaceEditor)
  then TCROInplaceEdit(InplaceEditor).Properties.CursorHandle := nil;
  FProperties.Free;
  FObjects.Free;
  FFixedFont.Free;
  FSelectedFont.Free;
  FFocusedFont.Free;
  inherited;
end;

function TCROGrid.GetProperties: TCROGridProperties;
begin
  Result := FProperties;
end;

function TCROGrid.CreateEditor: TInplaceEdit;
begin
  Result := TCROInplaceEdit.Create(Self);
  TCROInplaceEdit(Result).Properties.CursorHandle := Properties.ListHandle.InternalCursorHandle;
  TCROInplaceEdit(Result).Properties.FontProperties := Properties.FontProperties;
  TCROInplaceEdit(Result).OnKeyDown := EditorKeyDown;
end;

procedure TCROGrid.SetProperties(Value: TCROGridProperties);
begin
  if Value <> nil
  then FProperties := Value;
end;

function TCROGrid.GetObjects(Index: Integer): TCROGridObject;
begin
  Result := TCROGridObject(FObjects[Index]);
end;

procedure TCROGrid.SetObjects(Index: Integer; const Value: TCROGridObject);
begin
  if FObjects.Count > Index
  then FObjects[Index] := Value
  else FObjects.Add(Value);
end;

procedure TCROGrid.UpdateColumn(const Index: Word);
begin
  if (ColCount > Index) and (Columns.Count > Index)
  then ColWidths[Index] := Columns[Index].Width;
end;

procedure TCROGrid.UpdateColumns;
var i: Integer;
begin
  if Columns.Count > 0 then
  begin
    ColCount := Columns.Count;
    for i := 0 to Columns.Count-1
    do UpdateColumn(i);
  end;
end;

procedure TCROGrid.ColumnMoved(FromIndex, ToIndex: Integer);
begin
  if (Columns.Count > FromIndex) and (Columns.Count > ToIndex)
  then Columns[FromIndex].Index := ToIndex;
  inherited ColumnMoved(FromIndex, ToIndex);
end;

procedure TCROGrid.RowMoved(FromIndex, ToIndex: Integer);
var AObject: TObject;
begin
  AObject := FObjects.Extract(FObjects.Items[FromIndex]);
  FObjects.Insert(ToIndex,AObject);
  inherited RowMoved(FromIndex, ToIndex);
end;

procedure TCROGrid.ColWidthsChanged;
var i: Integer;
begin
  if not FResizingColumns then
  begin
    for i := 0 to Columns.Count-1 do
    begin
      if Columns[i].Width <> ColWidths[i]
      then Columns[i].Width := ColWidths[i];
    end;

    FFixedWidth := 0;
    FOriginalWidth := 0;
    FUsefullWidth := 0;
    FAvailableWidth := 0;
    FScrollAreaWidth := 0;

    for i := 0 to Columns.Count-1 do
    begin
      FUsefullWidth := FUsefullWidth + Columns[i].Width;
      if not Columns[i].AutoSize
      then FFixedWidth := FFixedWidth + Columns[i].Width;
    end;
    FOriginalWidth := Width;
    FScrollAreaWidth := Width - FUsefullWidth;
    FAvailableWidth := FOriginalWidth - FFixedWidth - FScrollAreaWidth;
  end;
  inherited;
end;

procedure TCROGrid.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
var S: String; X,Y: Integer; CheckBoxState: TCheckBoxState;
begin
  inherited; //there is no defaultdrawing property in this component - its always true

  //draw titles
  if (ARow = 0) and (Columns.Count > ACol) then
  begin
    S := Columns[ACol].DisplayName;
    Canvas.TextOut(ARect.Left+2,ARect.Top+2,S);
  end
  else
  //draw items
  begin
    if (Columns.Count > ACol) and (ObjectCount > ARow-1) then
    begin
      //select current index to cursor
      if Assigned(Properties.ListHandle) then
      begin
        if (FObjects.Count > ARow-1) and
           (gdSelected in AState) and (gdFocused in AState) and
           (Properties.ListHandle.ItemIndex <> Objects[ARow-1].ItemIndex) then
        begin
          Properties.ListHandle.DisableUpdate := True;
          Properties.ListHandle.ItemIndex := Objects[ARow-1].ItemIndex;
          Properties.ListHandle.DisableUpdate := False;
        end;
      end;

      //draw components
      case Columns[ACol].FEditorTypes.IndexOf(Columns[ACol].EditorType) of
        //default editor
        EDITOR_DEFAULT_INDEX:
        begin
          S := Objects[ARow-1].GetValueFromExpression(ACol);
          Canvas.TextOut(ARect.Left+2,ARect.Top+2,S);
        end;
        //checkbox
        EDITOR_CHECKBOX_INDEX:
        begin
          X := ARect.Left + ((ColWidths[ACol] - Columns[ACol].Editor.Width) div 2);
          Y := ARect.Top + ((RowHeights[ARow] - Columns[ACol].Editor.Height) div 2);
          if (gdSelected in AState) and (gdFocused in AState) then
          begin
            Columns[ACol].Editor.TabStop := False;
            Columns[ACol].Editor.Top := Y;
            Columns[ACol].Editor.Left := X;
            Columns[ACol].Editor.Visible := True;
            Columns[ACol].Editor.BringToFront;
          end
          else
          begin
            if Assigned(Columns[ACol].Editor)
            then Columns[ACol].Editor.Visible := False;
            //draw checkbox image
            S := Objects[ARow-1].GetValueFromExpression(ACol);
            if S = 'True'
            then CheckBoxState := cbChecked
            else CheckBoxState := cbUnchecked;
            TCROCheckBox(Columns[ACol].Editor).PaintButton(Canvas,X,Y,CheckBoxState, True);
          end;
        end;
        //combobox
        EDITOR_COMBOBOX_INDEX:
        begin
          if Assigned(Columns[ACol].Editor) and
             Columns[ACol].Editor.Visible and
             (not Columns[ACol].Editor.Focused)
          then Columns[ACol].Editor.Visible := False;

          S := Objects[ARow-1].GetValueFromExpression(ACol);
          Canvas.TextOut(ARect.Left+2,ARect.Top+2,S);
        end;
        //lookupcombo
        EDITOR_LOOKUPCOMBO_INDEX:
        begin
          if Assigned(Columns[ACol].Editor) and
             Columns[ACol].Editor.Visible and
             (not Columns[ACol].Editor.Focused)
          then Columns[ACol].Editor.Visible := False;

          S := Objects[ARow-1].GetValueFromExpression(ACol);
          Canvas.TextOut(ARect.Left+2,ARect.Top+2,S);
        end;
      end;
    end;
  end;

  //redraw cells
  if gdFixed in AState then
  begin
    Canvas.Brush.Color := FixedColor;
    Canvas.Font.Assign(FFixedFont);
  end
  else
  begin
    Canvas.Font.Assign(Font);
    Canvas.Brush.Color := Color;
  end;
  Canvas.FillRect(ARect);
  Canvas.TextOut(ARect.Left+2,ARect.Top+2, S);
  //format fixed cells
  if gdFixed in AState then
  begin
    Canvas.Font.Assign(FFixedFont);
    if (goFixedVertLine in Options) and (goFixedHorzLine in Options) then
    begin
      Canvas.Pen.Color := FBorderColorLeft;
      Canvas.MoveTo(ARect.Left,ARect.Bottom);
      Canvas.LineTo(ARect.Left,ARect.Top);

      Canvas.Pen.Color := FBorderColorTop;
      Canvas.MoveTo(ARect.Left,ARect.Top);
      Canvas.LineTo(ARect.Right,ARect.Top);

      Canvas.Pen.Color := FBorderColorRight;
      Canvas.MoveTo(ARect.Right,ARect.Top);
      Canvas.LineTo(ARect.Right,ARect.Bottom);

      Canvas.Pen.Color := FBorderColorBottom;
      Canvas.MoveTo(ARect.Right,ARect.Bottom);
      Canvas.LineTo(ARect.Left,ARect.Bottom);
    end
    else
    if ((not(goFixedVertLine in Options)) and (goFixedHorzLine in Options)) then
    begin
      Canvas.Pen.Color := FBorderColorTop;
      Canvas.MoveTo(ARect.Left,ARect.Top);
      Canvas.LineTo(ARect.Right,ARect.Top);

      Canvas.Pen.Color := FBorderColorBottom;
      Canvas.MoveTo(ARect.Right,ARect.Bottom);
      Canvas.LineTo(ARect.Left,ARect.Bottom);

      Canvas.Pixels[ARect.Left,ARect.Bottom] := FBorderColorBottom;
      Canvas.Pixels[ARect.Right,ARect.Top] := FBorderColorTop;
    end
    else
    if ((goFixedVertLine in Options) and (not(goFixedHorzLine in Options))) then
    begin
      Canvas.Pen.Color := FBorderColorLeft;
      Canvas.MoveTo(ARect.Left,ARect.Bottom);
      Canvas.LineTo(ARect.Left,ARect.Top);

      Canvas.Pen.Color := FBorderColorRight;
      Canvas.MoveTo(ARect.Right,ARect.Top);
      Canvas.LineTo(ARect.Right,ARect.Bottom);

      Canvas.Pixels[ARect.Right,ARect.Bottom] := FBorderColorBottom;
      Canvas.Pixels[ARect.Left,ARect.Top] := FBorderColorTop;
    end;
  end
  else
  begin
    if gdSelected in AState then
    begin
      if gdFocused in AState then
      begin
        Canvas.Brush.Color := FFocusedColor;
        Canvas.FillRect(ARect);
        Canvas.Font.Assign(FFocusedFont);
        Canvas.TextOut(ARect.Left+2,ARect.Top+2, S);
      end
      else
      begin
        if goDrawFocusSelected in Options
        then Canvas.Brush.Color := FFocusedColor
        else Canvas.Brush.Color := FSelectedColor;
        Canvas.FillRect(ARect);
        Canvas.Font.Assign(FSelectedFont);
        Canvas.TextOut(ARect.Left+2,ARect.Top+2, S);
      end;
    end
    else
    if not Odd(ARow) then
    begin
      Canvas.Brush.Color := FEvenColor;
      Canvas.FillRect(ARect);
      Canvas.TextOut(ARect.Left+2,ARect.Top+2, S);
    end;

    Canvas.Font.Assign(Font);
    if (goVertLine in Options) and (goHorzLine in Options) then
    begin
      Canvas.Pen.Color := FBorderColorLeft;
      Canvas.MoveTo(ARect.Left,ARect.Bottom);
      Canvas.LineTo(ARect.Left,ARect.Top);

      Canvas.Pen.Color := FBorderColorTop;
      Canvas.MoveTo(ARect.Left,ARect.Top);
      Canvas.LineTo(ARect.Right,ARect.Top);

      Canvas.Pen.Color := FBorderColorRight;
      Canvas.MoveTo(ARect.Right,ARect.Top);
      Canvas.LineTo(ARect.Right,ARect.Bottom);

      Canvas.Pen.Color := FBorderColorBottom;
      Canvas.MoveTo(ARect.Right,ARect.Bottom);
      Canvas.LineTo(ARect.Left,ARect.Bottom);
    end
    else
    if ((not(goVertLine in Options)) and (goHorzLine in Options)) then
    begin
      Canvas.Pen.Color := FBorderColorTop;
      Canvas.MoveTo(ARect.Left,ARect.Top);
      Canvas.LineTo(ARect.Right,ARect.Top);

      Canvas.Pen.Color := FBorderColorBottom;
      Canvas.MoveTo(ARect.Right,ARect.Bottom);
      Canvas.LineTo(ARect.Left,ARect.Bottom);

      Canvas.Pixels[ARect.Left,ARect.Bottom] := FBorderColorBottom;
      Canvas.Pixels[ARect.Right,ARect.Top] := FBorderColorTop;
    end
    else
    if ((goVertLine in Options) and (not(goHorzLine in Options))) then
    begin
      Canvas.Pen.Color := FBorderColorLeft;
      Canvas.MoveTo(ARect.Left,ARect.Bottom);
      Canvas.LineTo(ARect.Left,ARect.Top);

      Canvas.Pen.Color := FBorderColorRight;
      Canvas.MoveTo(ARect.Right,ARect.Top);
      Canvas.LineTo(ARect.Right,ARect.Bottom);

      Canvas.Pixels[ARect.Right,ARect.Bottom] := FBorderColorBottom;
      Canvas.Pixels[ARect.Left,ARect.Top] := FBorderColorTop;
    end;
  end;
end;

function TCROGrid.AddObject: TCROGridObject;
begin
  //insert grid line
  if (FObjects.Count >= RowCount-1)
  then RowCount := RowCount + 1;
  //insert object
  Result := TCROGridObject.Create(Self,RowCount-1);
  FObjects.Add(Result);
end;

function TCROGrid.RemoveObject: Boolean;
begin
  FObjects.Delete(FObjects.Count-1);
  if (RowCount > 2) and (RowCount-1 > FObjects.Count)
  then RowCount := RowCount-1;
  Result := True;
end;

function TCROGrid.GetOwnsObjects: Boolean;
begin
  Result := Assigned(FObjects) and FObjects.OwnsObjects;
end;

procedure TCROGrid.SetOwnsObjects(const Value: Boolean);
begin
  if Assigned(FObjects)
  then FObjects.OwnsObjects := Value;
end;

function TCROGrid.ObjectCount: Integer;
begin
  if Assigned(FObjects)
  then Result := FObjects.Count
  else Result := 0;
end;

function TCROGrid.GetEditText(ACol, ARow: Integer): string;
          procedure ShowCombo;
          var TopLeft: TPoint;
          begin
            (Columns[ACol].Editor as ICROCustomControl).UpdateValue;
            Columns[ACol].Editor.Height := RowHeights[ARow];
            TopLeft.X := CellRect(ACol,ARow).Left;
            TopLeft.Y := CellRect(ACol,ARow).Top;
            TopLeft   := ClientToParent(TopLeft);
            Columns[ACol].Editor.Top  := TopLeft.Y;
            Columns[ACol].Editor.Left := TopLeft.X;

            if Columns[ACol].Editor.InheritsFrom(TCROComboBox) then
            begin
              if TCROComboBox(Columns[ACol].Editor).ShowButton
              then TCROComboBox(Columns[ACol].Editor).Width := ColWidths[ACol] + 4
              else TCROComboBox(Columns[ACol].Editor).Width := ColWidths[ACol];
            end
            else
            begin
              if TCROLookupCombo(Columns[ACol].Editor).ShowButton
              then TCROLookupCombo(Columns[ACol].Editor).Width := ColWidths[ACol] + 4
              else TCROLookupCombo(Columns[ACol].Editor).Width := ColWidths[ACol];
            end;

            Columns[ACol].Editor.Visible := True;
            Columns[ACol].Editor.SetFocus;
          end;
begin
  if (FObjects.Count > ARow-1) and (Columns.Count > ACol) then
  begin
    if (Columns[ACol].EditorType <> EDITOR_DEFAULT_NAME) then
    begin
      if Columns[ACol].FEditorTypes.IndexOF(Columns[ACol].EditorType) in [EDITOR_COMBOBOX_INDEX, EDITOR_LOOKUPCOMBO_INDEX]
      then ShowCombo;

      Result := '';
    end
    else
    begin
      TCROInplaceEdit(Self.InplaceEditor).Properties.Expression := Columns[ACol].Expression;
      Result := Objects[ARow-1].GetValueFromExpression(ACol);
    end;

    if Assigned(OnGetEditText)
    then OnGetEditText(Self, ACol, ARow, Result);
  end;
end;

procedure TCROGrid.UpdateSkin;
begin
  if Assigned(FProperties.GridProperties) then
  begin
    Color := FProperties.GridProperties.Color;
    Font.Assign(FProperties.GridProperties.Font);
    FixedColor := FProperties.GridProperties.FixedColor;
    Options := FProperties.GridProperties.Options;

    FSelectedFont.Assign(FProperties.GridProperties.SelectedFont);
    FFocusedFont.Assign(FProperties.GridProperties.FocusedFont);
    FFixedFont.Assign(FProperties.GridProperties.FixedFont);

    FEvenColor := FProperties.GridProperties.EvenColor;
    FFocusedColor := FProperties.GridProperties.FocusedColor;
    FSelectedColor := FProperties.GridProperties.SelectedColor;
    FBorderColorRight := FProperties.GridProperties.BorderColorRight;
    FBorderColorTop := FProperties.GridProperties.BorderColorTop;
    FBorderColorBottom := FProperties.GridProperties.BorderColorBottom;
    FBorderColorLeft := FProperties.GridProperties.BorderColorLeft;

    RowHeights[0] := FProperties.GridProperties.FixedRowHeight;
    DefaultRowHeight := FProperties.GridProperties.DefaultRowHeight;

    Refresh;
  end;
end;

procedure TCROGrid.UpdateItems;
begin
  if (Assigned(FProperties.ListHandle)) and
     (Assigned(FProperties.ListHandle.Data)) then
  begin
    with FProperties.ListHandle do
    begin
      DisableUpdate := True;
      try
        //remove items a mais
        while FObjects.Count > Count
        do Self.RemoveObject;
        //insere novos itens
        while Count > FObjects.Count
        do Self.AddObject;
      finally
        DisableUpdate := False;
        UpdateIndex;
      end;
    end;
  end;
end;

procedure TCROGrid.Clear;
begin
  RowCount := 2;
  FObjects.Clear;
end;

procedure TCROGrid.SetName(const NewName: TComponentName);
var i: Integer; OldName: String;
begin
  OldName := Name;
  inherited;
  if Assigned(Columns) then
  begin
    for i := 0 to Columns.Count-1 do
    begin
      if Columns[i].Name = OldName+'Column'+IntToStr(i+1) then
      begin
        Columns[i].Name := Name+'Column'+IntToStr(i+1);
        if Assigned(Columns[i].Editor)
        then Columns[i].Editor.Name := Name+'Editor'+IntToStr(i+1);
      end;
    end;
  end;
end;

procedure TCROGrid.CheckBoxMouseDown(Sender: TObject; Button: TMouseButton;
       Shift: TShiftState; X, Y: Integer);
begin
  Self.SetFocus;
end;

procedure TCROGrid.EditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Shift = [] then
  begin
    case Key of
      13, VK_ESCAPE:
      begin
        if Key = VK_ESCAPE then
        begin
          if Assigned(FProperties.ListHandle) and
             Assigned(FProperties.ListHandle.Data) and
             (FProperties.ListHandle.Data is TROArray) and 
             (TROArray(FProperties.ListHandle.Data).Count > 0)
          then ((Sender as TComponent) as ICROCustomControl).UpdateValue;
          EditorMode := False;
        end;

        TWinControl(Sender).Visible := False;
        SetFocus;
      end;
    end;
  end;
end;

procedure TCROGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Columns.Count > Col) and
     (Columns[Col].FEditorTypes.IndexOF(Columns[Col].EditorType) <> EDITOR_DEFAULT_INDEX) and
     (not (Key in [27,13,VK_LEFT,VK_RIGHT,VK_DOWN,VK_UP]))
  then SendMessage(Columns[Col].Editor.Handle, WM_KEYDOWN, Ord(Key), 0);
end;

procedure TCROGrid.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Columns.Count > Col) and
     (Columns[Col].FEditorTypes.IndexOF(Columns[Col].EditorType) <> EDITOR_DEFAULT_INDEX) and
     (not (Key in [27,13,VK_LEFT,VK_RIGHT,VK_DOWN,VK_UP]))
  then SendMessage(Columns[Col].Editor.Handle, WM_KEYUP, Ord(Key), 0);
//  Paint;//para atualizar grid apos cancelar edição por ter mudado de celular sem postar
end;

procedure TCROGrid.EditorExit(Sender: TObject);
begin
  EditorMode := False;
  if Sender is TWinControl
  then TWinControl(Sender).Visible := False;
end;

function TCROGrid.SelectCell(ACol, ARow: Longint): Boolean;
var ARect: TRect;
begin
  if (Columns.Count > ACol) and Assigned(FProperties.ListHandle) and (FProperties.ListHandle.Count = RowCount-1) then
  begin
    if (Columns[ACol].ReadOnly) or (ObjectCount = 0)
    then Options := Options - [goEditing]
    else
    begin
      //if it has an editor, disable to edit the cell
      if (Columns[ACol].EditorType <> EDITOR_CHECKBOX_NAME) then
      begin
        if Columns[ACol].FEditorTypes.IndexOf(Columns[ACol].EditorType) in [EDITOR_COMBOBOX_INDEX, EDITOR_LOOKUPCOMBO_INDEX]
        then (Columns[ACol].Editor as ICROCustomControl).UpdateValue;

        Options := Options + [goEditing];
      end
      else
      begin   //if checkbox
        EditorMode := False;
        Options := Options - [goEditing];

        if FMouseDown then
        begin
          ARect := CellRect(ACol, ARow);
          DrawCell(ACol, ARow, ARect, []);

          PostMessage(Columns[ACol].Editor.Handle, WM_LBUTTONDOWN, 0, 0);
        end;
      end;
    end;
  end;
  Result := inherited SelectCell(ACol, ARow);

  FMouseDown := False;
end;

procedure TCROGrid.KeyPress(var Key: Char);
begin
  inherited;
  if (Columns.Count > Col) and
     (Columns[Col].FEditorTypes.IndexOF(Columns[Col].EditorType) <> EDITOR_DEFAULT_INDEX)
  then SendMessage(Columns[Col].Editor.Handle, WM_CHAR, Ord(Key), 0);
end;

procedure TCROGrid.Loaded;
begin
  inherited;
  DoUpdateHandle(nil);
  UpdateItems;
end;

procedure TCROGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var ARect: TRect;
begin
  inherited;

  if (Columns.Count > Col) and
     (Columns[Col].EditorType = EDITOR_CHECKBOX_NAME) then
  begin
    ARect := CellRect(Col, Row);
    DrawCell(Col, Row, ARect, []);

    PostMessage(Columns[Col].Editor.Handle, WM_LBUTTONDOWN, 0, 0);//debug
  end
  else
    FMouseDown := True;
end;

procedure TCROGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  //if it has an editor, disable to edit the cell
  if (Columns.Count > Col) and
     (Columns[Col].EditorType = EDITOR_CHECKBOX_NAME) then
  begin
    PostMessage(Columns[Col].Editor.Handle, WM_LBUTTONUP, 0, 0);//debug
  end;
//  Paint;//para atualizar grid apos cancelar edição por ter mudado de celular sem postar - buffer do edit já faz isso - se der paint aqui perde foco qnd clica em combo
end;

procedure TCROGrid.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  Repaint;
end;

procedure TCROGrid.UpdateIndex;
begin
  if Assigned(FProperties.ListHandle) then
  begin
    if FProperties.ListHandle.ItemIndex > -1 then
    begin
      Row := FProperties.ListHandle.ItemIndex+1;
      Paint;
    end
    else
    begin
      Row := 1;
      Paint;
    end;
  end;
end;

procedure TCROGrid.DoUpdateHandle(Sender: TObject);
var i: Integer;
begin
  if Assigned(Columns) and (not (csDestroying in ComponentState)) then
  begin
    for i := 0 to Columns.Count - 1
    do Columns[i].LoadEditorProperties;
  end;
end;

procedure TCROGrid.SetFixedFont(const Value: TFont);
begin
  FFixedFont.Assign(Value);
end;

procedure TCROGrid.DoRefresh(Sender: TObject);
begin
  Refresh;
end;

procedure TCROGrid.SetBorderColorBottom(const Value: TColor);
begin
  FBorderColorBottom := Value;
  Refresh;
end;

procedure TCROGrid.SetBorderColorLeft(const Value: TColor);
begin
  FBorderColorLeft := Value;
  Refresh;
end;

procedure TCROGrid.SetBorderColorRight(const Value: TColor);
begin
  FBorderColorRight := Value;
  Refresh;
end;

procedure TCROGrid.SetBorderColorTop(const Value: TColor);
begin
  FBorderColorTop := Value;
  Refresh;
end;

procedure TCROGrid.SetFocusedColor(const Value: TColor);
begin
  FFocusedColor := Value;
  Refresh;
end;

procedure TCROGrid.SetSelectedColor(const Value: TColor);
begin
  FSelectedColor := Value;
  Refresh;
end;

procedure TCROGrid.SetEvenColor(const Value: TColor);
begin
  FEvenColor := Value;
  Refresh;
end;

procedure TCROGrid.SetSelectedFont(const Value: TFont);
begin
  FSelectedFont.Assign(Value);
  Refresh;
end;

procedure TCROGrid.SetFFocusedFont(const Value: TFont);
begin
  FFFocusedFont.Assign(Value);
  Refresh;
end;

procedure TCROGrid.Resize;
var i,NewUsefullWidth,NewAvailableWidth: Integer;
begin
  inherited;
  //recalculate colwidths
  FResizingColumns := True;
{
      FFixedWidth
      FAvailableWidth
      FScrollAreaWidth
}
  NewUsefullWidth := Width - FScrollAreaWidth;
  NewAvailableWidth := NewUsefullWidth - FFixedWidth;
  for i := 0 to Columns.Count-1 do
  begin
    if Columns[i].AutoSize
    then ColWidths[i] := Round(NewAvailableWidth * Columns[i].Width / FAvailableWidth);
  end;
  FResizingColumns := False;
  ColWidthsChanged;
end;

end.
