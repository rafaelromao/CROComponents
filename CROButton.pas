unit CROButton;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, Graphics, ExtCtrls, Windows,
  Messages, Buttons, Dialogs, Forms, CROPictureSet,
  CROSkinProperties, CROSkinControl, CROComponentsCommon;

type
  TCROButtonProperties = class;

  TCROFocusStyle = (fsDefault, fsDotAsFont, fsNoFocus);

  TCROCustomButton = class(TCROCustomSkinControl, ICROSkinControl)
  private
    FPainted: array [TButtonState] of Boolean;
    IsFocused: Boolean;
    FMouseInControl: Boolean;
    FPictureDisabled: TCROPictureSplit;
    FPictureDown: TCROPictureSplit;
    FPictureUp: TCROPictureSplit;
    FPictureUpImage: TPicture;
    FPictureDownImage: TPicture;
    FPictureDisabledImage: TPicture;
    FState: TButtonState;
    FModalResult: TModalResult;
    FProperties: TCROButtonProperties;
    procedure SetState(const Value: TButtonState);
    procedure SetProperties(const Value: TCROButtonProperties);
  protected
    FFocusStyle: TCROFocusStyle;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CMTextChanged(var Message: TWMGetText); message CM_TEXTCHANGED;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMSetFocus); message WM_KILLFOCUS;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure Paint; override;
    procedure UpdateSkin; virtual;
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure SetEnabled(Value: Boolean); override;
    procedure Click; override;
    property State: TButtonState read FState write SetState;
    property Properties:TCROButtonProperties read FProperties write SetProperties;
    property FocusStyle: TCROFocusStyle read FFocusStyle;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property Caption;
    property Enabled;
    property Visible;
    property Font;
    property Action;
    property BiDiMode;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property OnClick;
    property OnContextPopup;
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
    property ModalResult: TModalResult read FModalResult write FModalResult default 0;
  end;

  TCROButton = class(TCROCustomButton)
  published
    property Properties;
  end;

  TCROButtonProperties = class(TCROSkinProperties)
  private
    FFocusStyle: TCROFocusStyle;
    FPictureDisabled: TCROPictureSplit;
    FPictureDown: TCROPictureSplit;
    FPictureUp: TCROPictureSplit;
    FFont: TFont;
    FTransparentColor: TColor;
    FDisabledFont: TFont;
    procedure SetFont(const Value: TFont);
    procedure SetFocusStyle(const Value: TCROFocusStyle);
    procedure SetTransparentColor(const Value: TColor);
    procedure SetDisabledFont(const Value: TFont);
  published
    property PictureUp: TCROPictureSplit read FPictureUp write FPictureUp;
    property PictureDown: TCROPictureSplit read FPictureDown write FPictureDown;
    property PictureDisabled: TCROPictureSplit read FPictureDisabled write FPictureDisabled;
    property Font: TFont read FFont write SetFont;
    property DisabledFont: TFont read FDisabledFont write SetDisabledFont;
    property FocusStyle: TCROFocusStyle read FFocusStyle write SetFocusStyle default fsDefault;
    property TransparentColor:TColor read FTransparentColor write SetTransparentColor default clRed;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TCROCustomButton }

procedure TCROCustomButton.Click;
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if Form <> nil then Form.ModalResult := ModalResult;
  inherited Click;
end;

procedure TCROCustomButton.CMTextChanged(var Message: TWMGetText);
begin
  inherited;
  Refresh;
end;

constructor TCROCustomButton.Create(AOwner: TComponent);
begin
  inherited;
  FPictureUpImage := TPicture.Create;
  FPictureDownImage := TPicture.Create;
  FPictureDisabledImage := TPicture.Create;

  FFocusStyle := fsDefault;
  FActivePictureSet := nil;
  FActivePicture := nil;
  TabStop := True;
  Width := 78;
  Height := 28;
  State := bsUp;
end;

destructor TCROCustomButton.Destroy;
begin
  FPictureUpImage.Free;
  FPictureDownImage.Free;
  FPictureDisabledImage.Free;
  inherited;
end;

procedure TCROCustomButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key in [Ord(' '),13] then
  begin
    Perform(WM_LBUTTONDOWN,0,0);
  end;
  inherited;
end;

procedure TCROCustomButton.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if (Key in [Ord(' '),13]) and (FMouseInControl) then
  begin
    Perform(WM_LBUTTONUP,0,0);
  end;
  inherited;
end;

procedure TCROCustomButton.Paint;
var X,Y: Word; R: TRect;
begin
  inherited;
  if Assigned(FProperties) then
  begin
    if State = bsDisabled
    then Canvas.Font.Assign(FProperties.FDisabledFont)
    else Canvas.Font.Assign(FProperties.FFont)
  end
  else
    Canvas.Font.Assign(Font);

  Canvas.Brush.Style := bsClear;

  X := (Width - Canvas.TextWidth(Caption)) div 2;
  Y := (Height - Canvas.TextHeight(Caption)) div 2;

  if IsFocused then
  begin
    R.Left   := X - 2;
    R.Top    := Y - 2;
    R.Right  := X + Canvas.TextWidth(Caption) + 2;
    R.Bottom := Y + Canvas.TextHeight(Caption) + 2;

    case FFocusStyle of
      fsDefault:
      begin
        DrawFocusRect(Canvas.Handle,R);
      end;
      fsDotAsFont:
      begin
        Canvas.Pen.Color := Font.Color;
        Canvas.Pen.Style := psDot;
        Canvas.Rectangle(R);
      end;
    end;  
  end;

  Canvas.TextOut(X,Y,Caption);
end;

procedure TCROCustomButton.SetEnabled(Value: Boolean);
begin
  inherited;
  if Enabled
  then State := bsUp
  else State := bsDisabled;
end;

procedure TCROCustomButton.SetState(const Value: TButtonState);
begin
  FState := Value;
  case FState of
    bsUp:
    if Assigned(FPictureUp) then
    begin
      FActivePictureSet := FPictureUp;
      FActivePicture := FPictureUpImage;
    end;
    bsDown:
    if Assigned(FPictureDown) then
    begin
      FActivePictureSet := FPictureDown;
      FActivePicture := FPictureDownImage;
    end;
    bsDisabled:
    if Assigned(FPictureDisabled) then
    begin
      FActivePictureSet := FPictureDisabled;
      FActivePicture := FPictureDisabledImage;
    end;
  end;

  if (not FPainted[FState]) then
  begin
    if RecreatePicture then
    begin
      FPainted[FState] := PaintTransparentBorder;
      //se já conseguiu montar alguma então verifica as outras para saber se já foram montadas
      if FPainted[FState] then
      begin
        if not FPainted[bsDisabled] then
        begin
          FActivePictureSet := FPictureDisabled;
          FActivePicture := FPictureDisabledImage;
          if RecreatePicture then
          begin
            PaintTransparentBorder;
            FPainted[bsDisabled] := True;
          end;
        end;
        if not FPainted[bsDown] then
        begin
          FActivePictureSet := FPictureDown;
          FActivePicture := FPictureDownImage;
          if RecreatePicture then
          begin
            PaintTransparentBorder;
            FPainted[bsDown] := True;
          end;
        end;
        if not FPainted[bsUp] then
        begin
          FActivePictureSet := FPictureUp;
          FActivePicture := FPictureUpImage;
          if RecreatePicture then
          begin
            PaintTransparentBorder;
            FPainted[bsUp] := True;
          end;
        end;
        SetState(FState);//retorna ao state correto
      end;  
    end;
  end;
  Refresh;
end;

procedure TCROCustomButton.WMKillFocus(var Message: TWMSetFocus);
begin
  FMouseInControl := False;
  IsFocused := False;
  if Enabled
  then State := bsUp;
  Refresh;
end;

procedure TCROCustomButton.WMSetFocus(var Message: TWMSetFocus);
begin
  IsFocused := True;
  Refresh;
end;

function TCROCustomButton.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
  if Assigned(FPictureUp) then
  begin
    if (NewWidth < FPictureUp.Picture.Width)
    then NewWidth := FPictureUp.Picture.Width;

    if (NewHeight < FPictureUp.Picture.Height)
    then NewHeight := FPictureUp.Picture.Height;
  end;
  Result := inherited CanResize(NewWidth, NewHeight)
end;

procedure TCROCustomButton.SetProperties(
  const Value: TCROButtonProperties);
begin
  if (FProperties <> Value) then
  begin
    if Assigned(FProperties) then
    begin
      FProperties.RemoveControl(Self);
      FProperties.RemoveFreeNotification(Self);
    end
    else
    if Assigned(Value) then
    begin
      Value.AddControl(Self);
      Value.FreeNotification(Self);
    end;
    FProperties := Value;
    UpdateSkin;
  end;
end;

procedure TCROCustomButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FProperties
    then FProperties := nil;
  end;
end;

procedure TCROCustomButton.UpdateSkin;
begin
  if Assigned(FProperties) then
  begin
    FPainted[bsUp] := False;
    FPainted[bsDown] := False;
    FPainted[bsDisabled] := False;
    FPictureUp := FProperties.FPictureUp;
    FPictureDown := FProperties.FPictureDown;
    FPictureDisabled := FProperties.FPictureDisabled;
    FFocusStyle := FProperties.FocusStyle;
    TransparentColor := FProperties.TransparentColor;
    Font.Assign(FProperties.Font);
    SetState(State);
  end;
end;

procedure TCROCustomButton.WMLButtonDown(var Message: TWMLButtonDown);
begin
  FMouseInControl := True;
  State := bsDown;
  SetFocus;
  inherited;
end;

procedure TCROCustomButton.WMLButtonUp(var Message: TWMLButtonUp);
begin
  FMouseInControl := False;
  State := bsUp;
  inherited;
end;

procedure TCROCustomButton.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  Perform(WM_LBUTTONDOWN,0,0);
  inherited;
end;

{ TCROButtonProperties }

constructor TCROButtonProperties.Create(AOwner: TComponent);
begin
  inherited;
  FTransparentColor := clRed;
  FFocusStyle := fsDefault;
  FFont := TFont.Create;
  FFont.OnChange := DoChange;
  FDisabledFont := TFont.Create;
  FDisabledFont.OnChange := DoChange;
  FPictureUp := TCROPictureSplit.Create(Self);
  FPictureDown := TCROPictureSplit.Create(Self);
  FPictureDisabled := TCROPictureSplit.Create(Self);
  FPictureUp.OnChange := DoChange;
  FPictureDown.OnChange := DoChange;
  FPictureDisabled.OnChange := DoChange;
end;

destructor TCROButtonProperties.Destroy;
begin
  FreeAndNil(FPictureUp);
  FreeAndNil(FPictureDown);
  FreeAndNil(FPictureDisabled);
  FreeAndNil(FFont);
  FreeAndNil(FDisabledFont);
  inherited;
end;

procedure TCROButtonProperties.SetDisabledFont(const Value: TFont);
begin
  FDisabledFont.Assign(Value);
  DoChange(Self);
end;

procedure TCROButtonProperties.SetFocusStyle(const Value: TCROFocusStyle);
begin
  FFocusStyle := Value;
  DoChange(Self);
end;

procedure TCROButtonProperties.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  DoChange(Self);
end;

procedure TCROButtonProperties.SetTransparentColor(const Value: TColor);
begin
  FTransparentColor := Value;
  DoChange(Self);
end;

end.

