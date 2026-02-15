unit CROEdit;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, CROOperation, Messages, Dialogs, Forms,
  CROComponentsCommon, Graphics;

type
  TCROCustomEdit = class(TCustomEdit, ICROCustomControl, ICROSkinControl, ICROEdit)
  private
    { Private declarations }
    FLoading: Boolean;
    FProperties: TCROProperties;
    FFrameStyle: TFrameStyle;
    FAware: Boolean;
    FEnterToTab: Boolean;
    procedure SetProperties(Value: TCROProperties);
    function GetProperties: TCROProperties;
    procedure SetFrameStyle(const Value: TFrameStyle);
  protected
    { Protected declarations }
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
    procedure DoSetMaxLength(Value: Integer); override;

    property Properties: TCROProperties read GetProperties write SetProperties;
    property FrameStyle: TFrameStyle read FFrameStyle write SetFrameStyle;
    property Aware: Boolean read FAware write FAware default True;
    property EnterToTab: Boolean read FEnterToTab write FEnterToTab default True;
  end;

  TCROEdit = class(TCROCustomEdit, ICROCustomControl, ICROSkinControl)
  published
    property Aware;
    property Properties;
    property FrameStyle;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;


implementation

{ TCROCustomEdit }

procedure TCROCustomEdit.Change;
begin
  //deve ser feito este controle em todos os componentes de ediçãod e dados
  if (FAware) and
     (Assigned(FProperties.CursorHandle)) and
     (
       (Assigned(FProperties.CursorHandle.Data)) or
       (not Assigned(FProperties.CursorHandle.ListHandle))//para o caso de naum estar ligado a um list handle e nem a um paramentro de saida
     ) and
     (not FLoading) and
     (FProperties.CursorHandle.AutoEdit) then
  begin
    FProperties.CursorHandle.Edit;
  end;
  inherited;
end;

constructor TCROCustomEdit.Create(AOwner: TComponent);
begin
  inherited;
  FAware := True;
  FProperties := TCROProperties.Create(Self);
  AutoSize := False;
  Height := 21;
  FEnterToTab := True;
end;

destructor TCROCustomEdit.Destroy;
begin
  FProperties.Free;
  inherited;
end;

procedure TCROCustomEdit.DoExit;
begin
  UpdateObject;
  inherited;
end;

procedure TCROCustomEdit.DoSetMaxLength(Value: Integer);
begin
  inherited;//only publics this method
end;

function TCROCustomEdit.GetProperties: TCROProperties;
begin
  Result := FProperties;
end;

procedure TCROCustomEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (FAware) and
      Assigned(FProperties.CursorHandle) and
     (not FProperties.CursorHandle.AutoEdit) and
     (not (FProperties.CursorHandle.State in [csEdit,csInsert]))
  then Key := 0;
  inherited;
end;

procedure TCROCustomEdit.KeyPress(var Key: Char);
begin
  if (FAware) and Assigned(FProperties.CursorHandle) then
  begin
    if (not FProperties.CursorHandle.AutoEdit) and
       (not (FProperties.CursorHandle.State in [csEdit,csInsert]))
    then Key := #0;
  end;

  if not ValidateEditKey(FProperties.DataType,Key,Self)
  then Key := #0;

  inherited;

  CheckFocus(Self,Key,FEnterToTab);
end;

procedure TCROCustomEdit.Loaded;
begin
  inherited;
  UpdateValue;
end;

procedure TCROCustomEdit.SetFrameStyle(const Value: TFrameStyle);
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

procedure TCROCustomEdit.SetProperties(Value: TCROProperties);
begin
  if Value <> nil
  then FProperties := Value;
end;

procedure TCROCustomEdit.UpdateObject;
var V: Variant;
begin
  if (FAware) and
     Assigned(FProperties.CursorHandle) and
     Assigned(FProperties.CursorHandle.Data) then
  begin
    FProperties.GetValueFromExpression(V);
    if V <> Self.Text //para otimização, assim naum atualiza desnecessariamente todos os objetos
    then FProperties.SetValueToExpression(Self.Text);
  end;
end;

procedure TCROCustomEdit.UpdateSkin;
begin
  if Assigned(FProperties.FontProperties) and Assigned(Font) then
  begin
    Font.Assign(FProperties.FontProperties.Font);
    FEnterToTab := FProperties.FontProperties.EnterToTab;
  end;
end;

procedure TCROCustomEdit.UpdateValue;
var V: Variant;
begin
  if (FAware) then
  begin
    if (Assigned(FProperties.CursorHandle)) and
       (Assigned(FProperties.CursorHandle.Data)) then
    begin
      //deve ser feito este controle em todos os componentes de ediçãod e dados
      FLoading := True;
      FProperties.GetValueFromExpression(V);
      Self.Text := V;
      FLoading := False;
    end
    else
      Self.Text := '';
  end;
end;

end.
