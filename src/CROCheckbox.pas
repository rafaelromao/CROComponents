unit CROCheckbox;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, CROOperation, Messages,
  Dialogs, CROCustomCheckbox, CROComponentsCommon, Graphics;

type
  TCROCheckbox = class(TCROCustomCheckbox, ICROCustomControl, ICROSkinControl)
  private
    { Private declarations }
    FLoading: Boolean;
    FProperties: TCROProperties;
    FAware: Boolean;
    FEnterToTab: Boolean;
    FOnDebug: TNotifyEvent;

    procedure SetProperties(Value: TCROProperties);
    function GetProperties: TCROProperties;
  protected
    { Protected declarations }
    procedure Paint; override; //debug
    procedure AssignTo(Dest: TPersistent); override;
    procedure Click; override;
    procedure Loaded; override;
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
    property Aware: Boolean read FAware write FAware default True;
    property EnterToTab: Boolean read FEnterToTab write FEnterToTab default False;
    property OnDebug: TNotifyEvent read FOnDebug write FOnDebug;
  end;

implementation

{ TCROCheckbox }

procedure TCROCheckbox.AssignTo(Dest: TPersistent);
begin
  if Dest.ClassType = Self.ClassType then
  begin
    TCROCheckBox(Dest).Left := Self.Left;
    TCROCheckBox(Dest).Top := Self.Top;
    TCROCheckBox(Dest).Width := Self.Width;
    TCROCheckBox(Dest).Height := Self.Height;
    TCROCheckBox(Dest).Properties.CursorHandle := Self.Properties.CursorHandle;
    TCROCheckBox(Dest).Properties.Expression := Self.Properties.Expression;
    TCROCheckBox(Dest).Checked := Self.Checked;
    TCROCheckBox(Dest).OnMouseDown := Self.OnMouseDown;
    TCROCheckBox(Dest).OnKeyDown := Self.OnKeyDown;
    TCROCheckBox(Dest).OnClick := Self.OnClick;
    TCROCheckBox(Dest).OnEnter := Self.OnEnter;
    TCROCheckBox(Dest).OnExit := Self.OnExit;
    //include here the property you want to be assigned
  end;
end;

procedure TCROCheckbox.Click;
begin
  if (FAware) and Assigned(FProperties.CursorHandle) and (
       (FProperties.CursorHandle.AutoEdit) or
       (FProperties.CursorHandle.State in [csEdit,csInsert])
     ) then
  begin
    inherited;
    if (not FLoading) then
    begin
      FProperties.CursorHandle.Edit;
    end;
    UpdateObject;
  end
  else
    inherited;
end;

constructor TCROCheckbox.Create(AOwner: TComponent);
begin
  inherited;
  FAware := True;
  FProperties := TCROProperties.Create(Self);
  FEnterToTab := False;
end;

destructor TCROCheckbox.Destroy;
begin
  FProperties.Free;
  inherited;
end;

function TCROCheckbox.GetProperties: TCROProperties;
begin
  Result := FProperties;
end;

procedure TCROCheckbox.KeyPress(var Key: Char);
begin
  inherited;
  CheckFocus(Self,Key,FEnterToTab);
end;

procedure TCROCheckbox.Loaded;
begin
  inherited;
  UpdateValue;
end;

procedure TCROCheckbox.Paint;
begin
  inherited;
  if Assigned(FOnDebug)
  then FOnDebug(Self);
end;

procedure TCROCheckbox.SetProperties(Value: TCROProperties);
begin
  if Value <> nil
  then FProperties := Value;
end;

procedure TCROCheckbox.UpdateObject;
var V: Variant;
begin
  if (FAware) and
     Assigned(FProperties.CursorHandle) and
     Assigned(FProperties.CursorHandle.Data) then
  begin
    FProperties.GetValueFromExpression(V);
    if V <> Self.Checked //para otimização, assim naum atualiza desnecessariamente todos os objetos
    then FProperties.SetValueToExpression(Self.Checked);
  end;
end;

procedure TCROCheckbox.UpdateSkin;
begin
  if Assigned(FProperties.FontProperties) and Assigned(Font) then
  begin
    Font.Assign(FProperties.FontProperties.Font);
    FEnterToTab := FProperties.FontProperties.EnterToTab;
  end;  
end;

procedure TCROCheckbox.UpdateValue;
var V: Variant;
begin
  if (FAware) and
     (Assigned(FProperties.CursorHandle)) and
     (Assigned(FProperties.CursorHandle.Data)) then
  begin
    FProperties.GetValueFromExpression(V);
    FLoading := True;
    Self.Checked := V;
    FLoading := False;
  end;
end;

end.
