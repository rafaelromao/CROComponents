unit CRORadioButton;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, CROOperation, Messages,
  Dialogs, CROCustomCheckbox, CROComponentsCommon, Graphics, uRODL,
  TypInfo;

type
  TCRORadioButton = class(TCROCustomCheckbox, ICROCustomControl, ICROSkinControl)
  private
    { Private declarations }
    FLoading: Boolean;
    FProperties: TCROProperties;
    FAware: Boolean;
    FValues: TStrings;
    FValue: String;
    FGroupIndex: Integer;
    FEnterToTab: Boolean;

    procedure SetProperties(Value: TCROProperties);
    function GetProperties: TCROProperties;
    procedure SetValue(const AValue: String);
  protected
    { Protected declarations }
    procedure AssignTo(Dest: TPersistent); override;
    procedure Click; override;
    procedure Loaded; override;
    procedure KeyPress(var Key: Char); override;
    procedure CreateWnd; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure UpdateValue;
    procedure UpdateObject;

    procedure UpdateSkin;
    procedure OnUpdateExpression(Sender: TObject);
    property Values: TStrings read FValues;
  published
    { Published declarations }
    property Properties: TCROProperties read GetProperties write SetProperties;
    property Aware: Boolean read FAware write FAware default True;
    property Value: String read FValue write SetValue;
    property GroupIndex: Integer read FGroupIndex write FGroupIndex;
    property EnterToTab: Boolean read FEnterToTab write FEnterToTab default False;
  end;

implementation

{ TCRORadioButton }

procedure TCRORadioButton.AssignTo(Dest: TPersistent);
begin
  if Dest.ClassType = Self.ClassType then
  begin
    TCRORadioButton(Dest).Left := Self.Left;
    TCRORadioButton(Dest).Top := Self.Top;
    TCRORadioButton(Dest).Width := Self.Width;
    TCRORadioButton(Dest).Height := Self.Height;
    TCRORadioButton(Dest).Properties.CursorHandle := Self.Properties.CursorHandle;
    TCRORadioButton(Dest).Properties.Expression := Self.Properties.Expression;
    TCRORadioButton(Dest).Checked := Self.Checked;
    TCRORadioButton(Dest).OnMouseDown := Self.OnMouseDown;
    TCRORadioButton(Dest).OnKeyDown := Self.OnKeyDown;
    TCRORadioButton(Dest).OnClick := Self.OnClick;
    TCRORadioButton(Dest).OnEnter := Self.OnEnter;
    TCRORadioButton(Dest).OnExit := Self.OnExit;
    //include here the property you want to be assigned
  end;
end;

procedure TCRORadioButton.Click;
var i: Integer;
begin
  if (FAware) then
  begin
    if Assigned(FProperties.CursorHandle) then
    begin
      if (FProperties.CursorHandle.AutoEdit) or
         (FProperties.CursorHandle.State in [csEdit,csInsert]) then
      begin
        inherited;
        if (not FLoading) then
        begin
          FProperties.CursorHandle.Edit;
        end;
        UpdateObject;
      end;
    end;  
  end
  else
    inherited;
  //Update groupindex items
  for i := 0 to Owner.ComponentCount-1 do
  begin
    if (Owner.Components[i].InheritsFrom(TCRORadioButton)) and
       (TCRORadioButton(Owner.Components[i]).GroupIndex = FGroupIndex) and
       (TCRORadioButton(Owner.Components[i]) <> Self)
    then TCRORadioButton(Owner.Components[i]).Checked := False;
  end;
  if Assigned(FProperties.CursorHandle)
  then FProperties.CursorHandle.UpdateProperties;//deve atualizar os outros radiobuttons do mesmo grupo
end;

constructor TCRORadioButton.Create(AOwner: TComponent);
begin
  //ATENÇÃO: Propriedades herdadas não podem ser setadas aqui... use o CreateWnd para isso, após o Inherited do mesmo
  inherited;
  FGroupIndex := 0;
  FAware := True;
  FValues := TStringList.Create;
  FProperties := TCROProperties.Create(Self);
  FProperties.OnInternalUpdateExpression := OnUpdateExpression;
  FEnterToTab := False;
end;

procedure TCRORadioButton.CreateWnd;
begin
  inherited;
  AutoSize := True;
  inherited Style := csRadioButton;
end;

destructor TCRORadioButton.Destroy;
begin
  FProperties.Free;
  FValues.Free;
  inherited;
end;

function TCRORadioButton.GetProperties: TCROProperties;
begin
  Result := FProperties;
end;

procedure TCRORadioButton.KeyPress(var Key: Char);
begin
  inherited;
  CheckFocus(Self,Key,FEnterToTab);
end;

procedure TCRORadioButton.Loaded;
begin
  inherited;
  UpdateValue;
end;

procedure TCRORadioButton.OnUpdateExpression(Sender: TObject);
var
  i: Integer;
  Lib: TRODLLibrary;
  Enum: TRODLEnum;
begin
  //Executed when Properties.Expression have been modified
  //Load ItemValues default list - its executed at design time, so CursorHandle.Data is not loaded
  if (FAware) then
  begin
    if (FProperties.Expression <> '') and
       (FProperties.DataType <> '') and
       Assigned(FProperties.CursorHandle) and
       Assigned(FProperties.CursorHandle.SourceOperation) and
       Assigned(FProperties.CursorHandle.SourceOperation.RORemoteService) and
       FProperties.CursorHandle.SourceOperation.Attached  then
    begin
      Lib := FProperties.CursorHandle.SourceOperation.RORemoteService.GetRODLLibrary;

      Enum := FindROEnum(Lib,FProperties.DataType);
      FValues.Clear;
      if Assigned(Enum) then
      begin
        for i := 0 to Enum.Count-1
        do FValues.Add(Enum.Items[i].Info.Name);

        if FValues.IndexOf(FValue) = -1
        then Value := FValues[0];
      end
      else
        raise Exception.Create('CRORadioButton - Expression is not an Enumeration property');
    end{
    else
      if not (csLoading in ComponentState)
      then MessageDlg('CRORadioButton - Could not identify the property data type!',mtInformation,[mbOk],0)};
  end;
end;

procedure TCRORadioButton.SetProperties(Value: TCROProperties);
begin
  if Value <> nil
  then FProperties := Value;
end;

procedure TCRORadioButton.SetValue(const AValue: String);
var CanPropose: Boolean;
begin
  if Assigned(FValues) and (FValues.Count > 0) then
  begin
    CanPropose := (Caption = Name) or (Caption = GetProposedDisplay(FValue));

    if FValues.IndexOf(AValue) = -1
    then FValue := FValues[0]
    else FValue := AValue;

    if CanPropose
    then Caption := GetProposedDisplay(FValue);
  end
  else
  begin
    if csLoading in ComponentState
    then FValue := AValue
    else FValue := '';
  end;
end;

procedure TCRORadioButton.UpdateObject;
var
  Instance: TObject;
  APropInfo: PPropInfo;
  AExpression: String;
begin
  if (FAware) and
     Assigned(FProperties.CursorHandle) and
     Assigned(FProperties.CursorHandle.Data) and
     (Trim(FProperties.Expression) <> '') and
     Checked then
  begin
    Instance := FProperties.CursorHandle.Data;
    AExpression := FProperties.Expression;
    GetPropInfoFromExpression(GetTrueOwner(Self.Owner,AExpression),AExpression,Instance,APropInfo);
    FProperties.SetValueToExpression(GetEnumValue(APropInfo^.PropType^,Value));
  end;
end;

procedure TCRORadioButton.UpdateSkin;
begin
  if Assigned(FProperties.FontProperties) and Assigned(Font)
  then Font.Assign(FProperties.FontProperties.Font);
end;

procedure TCRORadioButton.UpdateValue;
var V: Variant;
begin
  if (FAware) and
     (Assigned(FProperties.CursorHandle)) and
     (Assigned(FProperties.CursorHandle.Data)) then
  begin
    FProperties.GetValueFromExpression(V);
    FLoading := True;
    Self.Checked := (V = FValue);
    FLoading := False;
  end;
end;

end.
