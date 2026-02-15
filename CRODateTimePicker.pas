unit CRODateTimePicker;

interface

uses
  Windows, SysUtils, Classes, Controls, Messages,
  CROComponentsCommon, CROOperation, CROCustomDateTimePicker;

type
  TCRODateTimePicker = class(TCROCustomDateTimePicker, ICROCustomControl, ICROSkinControl)
  private
    { Private declarations }
    FLoading: Boolean;
    FProperties: TCROProperties;
    FAware: Boolean;
    FEnterToTab: Boolean;
    procedure SetProperties(Value: TCROProperties);
    function GetProperties: TCROProperties;
  protected
    { Protected declarations }
    function CanSetDate: Boolean; override;
    procedure Change; override;
    procedure DoExit; override;
    procedure Loaded; override;
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
    property Aware: Boolean read FAware write FAware default True;
    property EnterToTab: Boolean read FEnterToTab write FEnterToTab default True;
  end;

implementation

{ TCRODateTimePicker }

function TCRODateTimePicker.CanSetDate: Boolean;
begin
  //deve ser feito este controle em todos os componentes de ediçãod e dados
  Result := (not FAware) or (
              (Assigned(FProperties.CursorHandle)) and (
                (Assigned(FProperties.CursorHandle.Data)) or
                (not Assigned(FProperties.CursorHandle.ListHandle))
              ) and
              (not FLoading) and (
                (FProperties.CursorHandle.AutoEdit) or
                (FProperties.CursorHandle.State in [csEdit,csInsert])
              )
            );
end;

procedure TCRODateTimePicker.Change;
begin
  //deve ser feito este controle em todos os componentes de ediçãod e dados
  if CanSetDate
  then FProperties.CursorHandle.Edit;
  inherited;
end;

constructor TCRODateTimePicker.Create(AOwner: TComponent);
begin
  inherited;
  FAware := True;
  FProperties := TCROProperties.Create(Self);
  AutoSize := False;
  FEnterToTab := True;
end;

destructor TCRODateTimePicker.Destroy;
begin
  FProperties.Free;
  inherited;
end;

procedure TCRODateTimePicker.DoExit;
begin
  UpdateObject;
  inherited;
end;

function TCRODateTimePicker.GetProperties: TCROProperties;
begin
  Result := FProperties;
end;

procedure TCRODateTimePicker.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if FAware and Assigned(FProperties.CursorHandle) then
  begin
    if (
         (not (FProperties.CursorHandle.AutoEdit)) and
         (not (FProperties.CursorHandle.State in [csEdit,csInsert]))
       )
    then Key := 0;
  end;

  inherited;
end;

procedure TCRODateTimePicker.KeyPress(var Key: Char);
begin
  if (FAware) and Assigned(FProperties.CursorHandle) then
  begin
    if (not FProperties.CursorHandle.AutoEdit) and
       (not (FProperties.CursorHandle.State in [csEdit,csInsert]))
    then Key := #0;
  end;

  inherited;

  CheckFocus(Self,Key,FEnterToTab);
end;

procedure TCRODateTimePicker.Loaded;
begin
  inherited;
  UpdateValue;
end;

procedure TCRODateTimePicker.SetProperties(Value: TCROProperties);
begin
  if Value <> nil
  then FProperties := Value;
end;

procedure TCRODateTimePicker.UpdateObject;
var V: Variant;
begin
  if (FAware) and
     Assigned(FProperties.CursorHandle) and
     Assigned(FProperties.CursorHandle.Data) then
  begin
    FProperties.GetValueFromExpression(V);
    if V <> Self.DateTime //para otimização, assim naum atualiza desnecessariamente todos os objetos
    then FProperties.SetValueToExpression(Self.DateTime);
  end;
end;

procedure TCRODateTimePicker.UpdateSkin;
begin
  if Assigned(FProperties.FontProperties) and Assigned(Font) then
  begin
    Font.Assign(FProperties.FontProperties.Font);
    FEnterToTab := FProperties.FontProperties.EnterToTab;
  end;
end;

procedure TCRODateTimePicker.UpdateValue;
var V: Variant;
begin
  if (FAware) then
  begin
    if (Assigned(FProperties.CursorHandle)) and
       (Assigned(FProperties.CursorHandle.Data)) then
    begin
      //deve ser feito este controle em todos os componentes de edição de dados
      FLoading := True;
      FProperties.GetValueFromExpression(V);
      if V = 0 then
      begin
        Self.DateTime := Now;
        UpdateObject;
      end
      else Self.DateTime := V;
      FLoading := False;
    end
    else
      Self.DateTime := Now;
  end;
end;

end.
