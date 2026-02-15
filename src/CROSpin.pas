unit CROSpin;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, CROEdit, CROButton, Buttons,
  CROComponentsCommon, Spin, ExtCtrls, Graphics, Messages, Windows,
  CROOperation;

type
  TCROSpinEdit = class;
  TCROSpinButton = class;
  TCROTimerButton = class;

  TCROTimerButton = class(TCROCustomButton, ICROSkinControl)
  private
    FRepeatTimer: TTimer;
    procedure TimerExpired(Sender: TObject);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure UpdateSkin; override;
//    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TCROSpinButton = class (TWinControl)
  private
    FButtonUp: TCROTimerButton;
    FButtonDown: TCROTimerButton;
    FFocusedButton: TCROTimerButton;
    FFocusControl: TWinControl;
    FOnUpClick: TNotifyEvent;
    FOnDownClick: TNotifyEvent;
    function CreateButton(const AName:String): TCROTimerButton;
    procedure BtnClick(Sender: TObject);
    procedure BtnMouseDown (Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetFocusBtn (Btn: TCROTimerButton);
    procedure AdjustSize (var W, H: Integer); reintroduce;
    procedure WMSize(var Message: TWMSize);  message WM_SIZE;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure SetPropertiesDown(const Value: TCROButtonProperties);
    procedure SetPropertiesUp(const Value: TCROButtonProperties);
    function GetPropertiesDown: TCROButtonProperties;
    function GetPropertiesUp: TCROButtonProperties;
  protected
    procedure SetEnabled(Value: Boolean); override;
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property ButtonUp: TCROTimerButton read FButtonUp;
    property ButtonDown: TCROTimerButton read FButtonDown;
  published
    property Align;
    property Anchors;
    property Enabled;
    property Visible;
    property Font;
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
    property OnDownClick: TNotifyEvent read FOnDownClick write FOnDownClick;
    property OnUpClick: TNotifyEvent read FOnUpClick write FOnUpClick;
    property PropertiesUp: TCROButtonProperties read GetPropertiesUp write SetPropertiesUp;
    property PropertiesDown: TCROButtonProperties read GetPropertiesDown write SetPropertiesDown;
  end;

  TCROSpinEdit = class(TCROCustomEdit, ICROCustomControl, ICROSkinControl)
  private
    FMinValue: LongInt;
    FMaxValue: LongInt;
    FIncrement: LongInt;
    FButton: TCROSpinButton;
    FEditorEnabled: Boolean;
    FEnterToTab: Boolean;
    function GetMinHeight: Integer;
    function GetValue: LongInt;
    function CheckValue (NewValue: LongInt): LongInt;
    procedure SetValue (NewValue: LongInt);
    procedure SetEditRect;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMExit);   message CM_EXIT;
    procedure WMPaste(var Message: TWMPaste);   message WM_PASTE;
    procedure WMCut(var Message: TWMCut);   message WM_CUT;
    function GetButtonPropertiesDown: TCROButtonProperties;
    function GetButtonPropertiesUp: TCROButtonProperties;
    procedure SetButtonPropertiesDown(const Value: TCROButtonProperties);
    procedure SetButtonPropertiesUp(const Value: TCROButtonProperties);
  protected
    procedure SetEnabled(Value: Boolean); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function IsValidChar(Key: Char): Boolean; virtual;
    procedure UpClick (Sender: TObject); virtual;
    procedure DownClick (Sender: TObject); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Button: TCROSpinButton read FButton;
  published
    property Value: LongInt read GetValue write SetValue;
    property MaxValue: LongInt read FMaxValue write FMaxValue;
    property MinValue: LongInt read FMinValue write FMinValue;
    property Increment: LongInt read FIncrement write FIncrement default 1;
    property EditorEnabled: Boolean read FEditorEnabled write FEditorEnabled default True;
    property ButtonPropertiesUp: TCROButtonProperties read GetButtonPropertiesUp write SetButtonPropertiesUp;
    property ButtonPropertiesDown: TCROButtonProperties read GetButtonPropertiesDown write SetButtonPropertiesDown;
    property EnterToTab: Boolean read FEnterToTab write FEnterToTab default True;
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

uses CROPictureSet;

{ TCROSpinEdit }

function TCROSpinEdit.CheckValue(NewValue: Integer): LongInt;
begin
  Result := NewValue;
  if (FMaxValue <> FMinValue) then
  begin
    if NewValue < FMinValue then
      Result := FMinValue
    else if NewValue > FMaxValue then
      Result := FMaxValue;
  end;
end;

procedure TCROSpinEdit.CMEnter(var Message: TCMGotFocus);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
  inherited;
end;

procedure TCROSpinEdit.CMExit(var Message: TCMExit);
begin
  inherited;
  if CheckValue (Value) <> Value then
    SetValue (Value);
end;

constructor TCROSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton := TCROSpinButton.Create(Self);
  FButton.SetSubComponent(True);
  FButton.Width := 15;
  FButton.Height := 17;
  FButton.Visible := True;
  FButton.Parent := Self;
  FButton.TabStop := False;
  FButton.OnUpClick := UpClick;
  FButton.OnDownClick := DownClick;
  ControlStyle := ControlStyle - [csSetCaption];
  FMaxValue := MaxInt;
  FIncrement := 1;
  Text := '0';
  FEditorEnabled := True;
  ParentBackground := False;
  FEnterToTab := True;
end;

procedure TCROSpinEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

procedure TCROSpinEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

destructor TCROSpinEdit.Destroy;
begin
  FButton := nil;
  inherited Destroy;
end;

procedure TCROSpinEdit.DownClick(Sender: TObject);
begin
  if Assigned(Properties.CursorHandle) and (
       (Properties.CursorHandle.AutoEdit) or
       (Properties.CursorHandle.State in [csEdit,csInsert])
     ) then
  begin
    if ReadOnly then MessageBeep(0)
    else Value := Value - FIncrement;
  end;  
end;

procedure TCROSpinEdit.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

function TCROSpinEdit.GetMinHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  I := SysMetrics.tmHeight;
  if I > Metrics.tmHeight then I := Metrics.tmHeight;
  Result := Metrics.tmHeight + I div 4 + GetSystemMetrics(SM_CYBORDER) * 4 + 2;
end;

function TCROSpinEdit.GetValue: LongInt;
begin
  try
    Result := StrToInt (Text);
  except
    Result := FMinValue;
  end;
end;

function TCROSpinEdit.IsValidChar(Key: Char): Boolean;
begin
  Result := (Key in [DecimalSeparator, '+', '-', '0'..'9']) or
    ((Key < #32) and (Key <> Chr(VK_RETURN)));
  if not FEditorEnabled and Result and ((Key >= #32) or
      (Key = Char(VK_BACK)) or (Key = Char(VK_DELETE))) then
    Result := False;
end;

procedure TCROSpinEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_UP then UpClick (Self)
  else if Key = VK_DOWN then DownClick (Self);
  inherited KeyDown(Key, Shift);
end;

procedure TCROSpinEdit.KeyPress(var Key: Char);
var AKey: Char;
begin
  AKey := Key;
  if (not IsValidChar(Key)) then
  begin
    Key := #0;
    MessageBeep(0)
  end;
  if Key <> #0
  then inherited KeyPress(Key);

  CheckFocus(Self,AKey,FEnterToTab);
end;

procedure TCROSpinEdit.SetButtonPropertiesDown(
  const Value: TCROButtonProperties);
begin
  FButton.FButtonDown.Properties := Value;
end;

procedure TCROSpinEdit.SetButtonPropertiesUp(
  const Value: TCROButtonProperties);
begin
  FButton.FButtonUp.Properties := Value;
  if Assigned(Value)
  then FButton.Width := Value.PictureUp.Width;
end;

function TCROSpinEdit.GetButtonPropertiesDown: TCROButtonProperties;
begin
  Result := FButton.FButtonDown.Properties;
end;

function TCROSpinEdit.GetButtonPropertiesUp: TCROButtonProperties;
begin
  Result := FButton.FButtonUp.Properties;
end;

procedure TCROSpinEdit.SetEditRect;
var
  Loc: TRect;
begin
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@Loc));
  Loc.Bottom := ClientHeight + 1;  {+1 is workaround for windows paint bug}
  Loc.Right := ClientWidth - FButton.Width - 2;
  Loc.Top := 0;  
  Loc.Left := 0;  
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Loc));
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@Loc));  {debug}
end;

procedure TCROSpinEdit.SetValue(NewValue: Integer);
begin
  Text := IntToStr (CheckValue (NewValue));
end;

procedure TCROSpinEdit.UpClick(Sender: TObject);
begin
  if Assigned(Properties.CursorHandle) and (
       (Properties.CursorHandle.AutoEdit) or
       (Properties.CursorHandle.State in [csEdit,csInsert])
     ) then
  begin
    if ReadOnly then MessageBeep(0)
    else Value := Value + FIncrement;
  end;  
end;

procedure TCROSpinEdit.WMCut(var Message: TWMCut);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TCROSpinEdit.WMPaste(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TCROSpinEdit.WMSize(var Message: TWMSize);
var
  MinHeight: Integer;
begin
  inherited;
  MinHeight := GetMinHeight;
    { text edit bug: if size to less than minheight, then edit ctrl does
      not display the text }
  if Height < MinHeight then   
    Height := MinHeight
  else if FButton <> nil then
  begin
    if NewStyleControls and Ctl3D then
      FButton.SetBounds(Width - FButton.Width - 5, 0, FButton.Width, Height - 5)
    else FButton.SetBounds (Width - FButton.Width, 1, FButton.Width, Height - 3);
    SetEditRect;
  end;
end;



procedure TCROSpinEdit.SetEnabled(Value: Boolean);
begin
  inherited;
  FButton.Enabled := Value;
end;

{ TCROSpinButton }

procedure TCROSpinButton.AdjustSize(var W, H: Integer);
begin
  if (FButtonUp = nil) or (csLoading in ComponentState)
  then Exit;
//  FButtonUp.SetBounds(0, 1, W, H div 2);
//  FButtonDown.SetBounds(0, {H - (H div 2)}FButtonUp.Height+1, W, H div 2{H - FButtonUp.Height});
  FButtonUp.SetBounds(0, 1, W, H div 2);
  FButtonDown.SetBounds(0, {H - (H div 2)}FButtonUp.Height+1, W, H div 2{H - FButtonUp.Height});
end;

procedure TCROSpinButton.BtnClick(Sender: TObject);
begin
  if Sender = FButtonUp then
  begin
    if Assigned(FOnUpClick) then FOnUpClick(Self);
  end
  else
    if Assigned(FOnDownClick) then FOnDownClick(Self);
end;

procedure TCROSpinButton.BtnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    SetFocusBtn (TCROTimerButton (Sender));
    if (FFocusControl <> nil) and FFocusControl.TabStop and 
        FFocusControl.CanFocus and (GetFocus <> FFocusControl.Handle) then
      FFocusControl.SetFocus
    else if TabStop and (GetFocus <> Handle) and CanFocus then
      SetFocus;
  end;
end;

constructor TCROSpinButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] + [csOpaque];
  FButtonUp := CreateButton('ButtonDown');
  FButtonDown := CreateButton('ButtonUp');

  Width := 20;
  Height := 25;
  FFocusedButton := FButtonUp;
end;

function TCROSpinButton.CreateButton(const AName:String): TCROTimerButton;
begin
  Result := TCROTimerButton.Create(Self);
  Result.Name := AName;
  Result.Caption := '';
  Result.SetSubComponent(True); 
  Result.OnClick := BtnClick;
  Result.OnMouseDown := BtnMouseDown;
  Result.Visible := True;
  Result.Enabled := True;
  Result.Parent := Self;
end;

procedure TCROSpinButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP:
      begin
        SetFocusBtn (FButtonUp);
        FButtonUp.Click;
      end;
    VK_DOWN:
      begin
        SetFocusBtn (FButtonDown);
        FButtonDown.Click;
      end;
    VK_SPACE:
      FFocusedButton.Click;
  end;
end;

procedure TCROSpinButton.Loaded;
var
  W, H: Integer;
begin
  inherited Loaded;
  W := Width;
  H := Height;
  AdjustSize (W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds (Left, Top, W, H);
end;

procedure TCROSpinButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FFocusControl := nil;
end;

procedure TCROSpinButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  AdjustSize(W, H);
  inherited SetBounds(ALeft, ATop, W, H);
end;

procedure TCROSpinButton.SetFocusBtn(Btn: TCROTimerButton);
begin
  if TabStop and CanFocus and  (Btn <> FFocusedButton) then
  begin
    FFocusedButton := Btn;
    if (GetFocus = Handle) then
    begin
      Invalidate;
    end;
  end;
end;

procedure TCROSpinButton.SetPropertiesDown(
  const Value: TCROButtonProperties);
begin
  FButtonDown.Properties := Value;
end;

procedure TCROSpinButton.SetPropertiesUp(
  const Value: TCROButtonProperties);
begin
  FButtonUp.Properties := Value;
end;

function TCROSpinButton.GetPropertiesDown: TCROButtonProperties;
begin
  Result := FButtonDown.Properties;
end;

function TCROSpinButton.GetPropertiesUp: TCROButtonProperties;
begin
  Result := FButtonUp.Properties;
end;

procedure TCROSpinButton.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TCROSpinButton.WMSize(var Message: TWMSize);
var
  W, H: Integer;
begin
  inherited;
  { check for minimum size }
  W := Width;
  H := Height;
  AdjustSize(W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds(Left, Top, W, H);
  Message.Result := 0;
end;

procedure TCROSpinButton.SetEnabled(Value: Boolean);
begin
  inherited;
  FButtonUp.Enabled := Value;
  FButtonDown.Enabled := Value;
end;

{ TCROTimerButton }

constructor TCROTimerButton.Create(AOwner: TComponent);
begin
  inherited;
  TabStop := False;
end;

destructor TCROTimerButton.Destroy;
begin
  if FRepeatTimer <> nil then
    FRepeatTimer.Free;
  inherited Destroy;
end;
{
procedure TCROTimerButton.KeyPress(var Key: Char);
begin
  inherited;
  if Assigned(Owner.Owner) and (Owner.Owner.InheritsFrom(TCROSpinEdit))
  then CheckFocus(TCROSpinEdit(Owner.Owner),Key,TCROSpinEdit(Owner.Owner).EnterToTab);
end;
}
procedure TCROTimerButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown (Button, Shift, X, Y);
  if FRepeatTimer = nil then
    FRepeatTimer := TTimer.Create(Self);

  FRepeatTimer.OnTimer := TimerExpired;
  FRepeatTimer.Interval := InitRepeatPause;
  FRepeatTimer.Enabled  := True;
end;

procedure TCROTimerButton.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp (Button, Shift, X, Y);
  if FRepeatTimer <> nil then
    FRepeatTimer.Enabled  := False;
end;

procedure TCROTimerButton.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval := RepeatPause;
  if (State = bsDown) and MouseCapture then
  begin
    try
      Click;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
  end;
end;

procedure TCROTimerButton.UpdateSkin;
begin
  inherited;
  FFocusStyle := fsNoFocus;
end;

end.
