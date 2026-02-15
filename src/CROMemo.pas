unit CROMemo;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, CROOperation, CROComponentsCommon,
  Graphics, Forms;

type
  TCROCustomMemo = class(TCustomMemo, ICROCustomControl, ICROSkinControl)
  private
    { Private declarations }
    FLoading: Boolean;
    FProperties: TCROProperties;
    FFrameStyle: TFrameStyle;
    FAware: Boolean;
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

    property Properties: TCROProperties read GetProperties write SetProperties;
    property FrameStyle: TFrameStyle read FFrameStyle write SetFrameStyle;
    property Aware: Boolean read FAware write FAware default True;
  end;

  {TCROMemo}

  TCROMemo = class(TCROCustomMemo)
  published
    property Properties;
    property FrameStyle;
    property Aware;
    
    property Align;
    property Alignment;
    property Anchors;
    property BiDiMode;
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
    property Lines;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property WantReturns;
    property WantTabs;
    property WordWrap;
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

{ TCROCustomMemo }

procedure TCROCustomMemo.Change;
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

constructor TCROCustomMemo.Create(AOwner: TComponent);
begin
  inherited;
  FAware := True;
  FProperties := TCROProperties.Create(Self);
  AutoSize := False;
end;

destructor TCROCustomMemo.Destroy;
begin
  FProperties.Free;
  inherited;
end;

procedure TCROCustomMemo.DoExit;
begin
  UpdateObject;
  inherited;
end;

function TCROCustomMemo.GetProperties: TCROProperties;
begin
  Result := FProperties;
end;

procedure TCROCustomMemo.KeyDown(var Key: Word; Shift: TShiftState);
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

procedure TCROCustomMemo.KeyPress(var Key: Char);
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
end;

procedure TCROCustomMemo.Loaded;
begin
  inherited;
  UpdateValue;
end;

procedure TCROCustomMemo.SetFrameStyle(const Value: TFrameStyle);
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

procedure TCROCustomMemo.SetProperties(Value: TCROProperties);
begin
  if Value <> nil
  then FProperties := Value;
end;

procedure TCROCustomMemo.UpdateObject;
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

procedure TCROCustomMemo.UpdateSkin;
begin
  if Assigned(FProperties.FontProperties) and Assigned(Font)
  then Font.Assign(FProperties.FontProperties.Font);
end;

procedure TCROCustomMemo.UpdateValue;
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
      Self.Text := V;
      FLoading := False;
    end
    else
      Self.Text := '';
  end;
end;

end.
