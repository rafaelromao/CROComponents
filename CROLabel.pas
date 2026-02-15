unit CROLabel;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, CROOperation, Dialogs,
  CROComponentsCommon, Graphics, CROComboBox, uRODL;

type
  TCROLabelDataType = (ldtString,ldtDateTime,ldtEnumerator);

  TCROLabel = class(TCustomLabel, ICROCustomControl, ICROEnumControl, ICROSkinControl)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property Color nodefault;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Transparent default True;
    property Layout;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStartDock;
    property OnStartDrag;
  private
    { Private declarations }
    FProperties: TCROProperties;
    FAware: Boolean;
    FDataType: TCROLabelDataType;
    FEnumValues: TCROComboItems;
    procedure SetProperties(Value: TCROProperties);
    procedure SetDataType(const Value: TCROLabelDataType);
    procedure SetEnumValues(const Value: TCROComboItems);
    procedure SetAware(const Value: Boolean);
  protected
    { Protected declarations }
    procedure Loaded; override;
    procedure Paint; override;
    procedure OnUpdateExpression(Sender: TObject);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetProperties: TCROProperties;
    procedure UpdateValue;
    procedure UpdateObject; virtual; abstract;

    procedure UpdateSkin;
  published
    { Published declarations }
    property Properties: TCROProperties read GetProperties write SetProperties;
    property Aware: Boolean read FAware write SetAware default False;
    property DataType: TCROLabelDataType read FDataType write SetDataType default ldtString;
    property EnumValues: TCROComboItems read FEnumValues write SetEnumValues;
  end;

implementation

{ TCROLabel }

constructor TCROLabel.Create(AOwner: TComponent);
begin
  inherited;
  FEnumValues := TCROComboItems.Create(Self);
  FProperties := TCROProperties.Create(Self);
  FProperties.OnInternalUpdateExpression := OnUpdateExpression;
  Transparent := True;
  FAware := False;
  FDataType := ldtString;
end;

destructor TCROLabel.Destroy;
begin
  FProperties.Free;
  FEnumValues.Free;
  inherited;
end;

function TCROLabel.GetProperties: TCROProperties;
begin
  Result := FProperties;
end;

procedure TCROLabel.Loaded;
begin
  inherited;
  UpdateValue;
end;

procedure TCROLabel.OnUpdateExpression(Sender: TObject);
var
  i: Integer;
  Lib: TRODLLibrary;
  Enum: TRODLEnum;
  sProposedDisplay: String;
  EnumItems: TCROComboItems;
  EnumItem : TCROComboItem;
begin
  //Executed when Properties.Expression have been modified
  //Load ItemValues default list - its executed at design time, so CursorHandle.Data is not loaded
  if (FAware) and
     (DataType = ldtEnumerator) and
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
      EnumItems := TCROComboItems.Create(nil);
      try
        for i := 0 to Enum.Count-1 do
        begin
          sProposedDisplay := GetProposedDisplay(Enum.Items[i].Info.Name);
          EnumItems.CreateItem(Enum.Items[i].Info.Name, sProposedDisplay, True);
        end;

        // Verify if any items was deleted
        i := 0;
        while i < FEnumValues.Count do
        begin
          EnumItem := EnumItems.FindItem(FEnumValues[i].Name);

          if not Assigned(EnumItem)
          then FEnumValues.Delete(i)
          else Inc(i);
        end;

        // Verify if any items was created
        for i := 0 to EnumItems.Count - 1 do
        begin
          EnumItem := FEnumValues.FindItem(EnumItems[i].Name);

          if not Assigned(EnumItem)
          then FEnumValues.CreateItem(EnumItems[i].Name, EnumItems[i].Caption, True);
        end;
      finally
        EnumItems.Free;
      end;
    end
    else
      raise Exception.Create('CROLabel - Expression is not an Enumeration property');
  end
  else
    FEnumValues.Clear;
end;

procedure TCROLabel.Paint;
begin
  inherited;
  if (csDesigning in ComponentState) and
     (FAware) and (Caption <> Name)
  then Caption := Name;
end;

procedure TCROLabel.SetAware(const Value: Boolean);
begin
  FAware := Value;
  OnUpdateExpression(Self);
end;

procedure TCROLabel.SetDataType(const Value: TCROLabelDataType);
begin
  FDataType := Value;
  OnUpdateExpression(Self);
  UpdateValue;
end;

procedure TCROLabel.SetEnumValues(const Value: TCROComboItems);
begin
  FEnumValues := Value;
end;

procedure TCROLabel.SetProperties(Value: TCROProperties);
begin
  if Value <> nil
  then FProperties := Value;
end;

procedure TCROLabel.UpdateSkin;
begin
  if Assigned(FProperties.FontProperties) and Assigned(Font)
  then Font.Assign(FProperties.FontProperties.Font);
end;

procedure TCROLabel.UpdateValue;
var V: Variant; EnumValue: TCROComboItem;
begin
  if FAware then
  begin
    if (Assigned(FProperties.CursorHandle)) and
       (Assigned(FProperties.CursorHandle.Data)) then
    begin
      FProperties.GetValueFromExpression(V);
      case FDataType of
        ldtString: Caption := V;
        ldtDateTime: Caption := DateTimeToStr(V);
        ldtEnumerator:
        begin
          EnumValue := FEnumValues.ItemByName(V);
          if Assigned(EnumValue)
          then Caption  := EnumValue.Caption
          else Caption  := '';
        end;
      end;
    end
    else
      Caption := '';
  end;
end;

end.
