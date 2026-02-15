unit CROCustomCheckBox;

{
  This Code was used as base to TCROCustomCheckBox(CATI Informatica 2005)

  (C)opyright 2000 Version 1.02
  Autor : Simon Reinhardt
  eMail : reinhardt@picsoft.de
  Internet : http://www.picsoft.de

  Die Komponente ist eine Checkbox-Komponente mit Autosize-,
  Transparent- und WordWrap-Eigenschaften. Außerdem wird kein OnClick-Ereignis
  abgefeuert, wenn die Checked-Eigenschaft per Programmcode geändert wird.

  Die Komponente ist abgeleteitet von TGraphicControl und ist Public Domain,
  das Urheberrecht liegt aber beim Autor.
}

interface

uses
  Windows, Classes, Graphics, Controls, SysUtils, Messages, StdCtrls,
  Forms;

type
  TCtrl = class(TWinControl);

  TCheckStyle = (csCheckBox, csDiamond, csPushButton, csRadioButton, csTrafficLight);

  TCROCustomCheckBox = class(TCustomControl)
  private
    FPainting:         Boolean;
    FAlignment:        TLeftRight;
    FAllowGrayed,
    FAutoSize:         boolean;
    FColor:            TColor;
    FChecked:          boolean;
    FCheckSize:        integer;
    FMouseDown:        boolean;
    FSpaceKeyDown:     boolean;
    FSpacing:          integer;
    FState:            TCheckBoxState;
    FStateChanged:     boolean;
    FStyle:            TCheckStyle;
    FTransparent,
    FWordWrap:         boolean;
    FMouseOut:         boolean;
    FReadOnly:         Boolean;

    FOnChange,
    FOnClick,
    FOnDblClick:       TNotifyEvent;
    FOnMouseDown:      TMouseEvent;
    FOnMouseMove:      TMouseMoveEvent;
    FOnMouseUp:        TMouseEvent;

    procedure SetReadOnly(Value: Boolean);

    function IsPushed: Boolean;      

    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure CNCtlColorEdit(var Message: TWMCtlColorEdit); message CN_CTLCOLOREDIT;
    procedure CNCtlColorStatic(var Message: TWMCtlColorStatic); message CN_CTLCOLORSTATIC;
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMSetFocus); message WM_KILLFOCUS;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKeyUp(var Message: TWMKeyUp); message WM_KEYUP;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;

    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CMDialogChar(var Message: TCMDialogChar);message CM_DIALOGCHAR;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var msg: TMessage);message CM_TEXTCHANGED;
  protected
    procedure RepaintWindow;
    procedure CreateWindowHandle(const Params: TCreateParams); override;

    procedure AdjustBounds;
    procedure Change; dynamic;
    procedure DblClick; override;
    procedure DrawTextComp(AText:string; var ARect:TRect; AFormat:Word);
    function GetTextRect(ARect: TRect): TRect;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure PaintButton; overload;
    procedure PaintCaption;

    procedure SetAlignment(newValue: TLeftRight);
    procedure SetAutosize(newValue: boolean);override;
    procedure SetColor(newColor: TColor);
    procedure SetChecked(newValue: boolean);
    procedure SetCheckSize(newValue: integer);
    procedure SetSpacing(newValue: integer);
    procedure SetState(newValue: TCheckBoxState);
    procedure SetStyle(newValue: TCheckStyle);
    procedure SetTransparent(newValue: boolean);
    procedure SetWordWrap(newValue: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PaintButton(ACanvas: TCanvas; ALeft, ATop: Integer;
      AState: TCheckBoxState; IgnoreIsPushed: Boolean = False);overload;
  published
    property Action;
    property Alignment: TLeftRight read FAlignment write SetAlignment;
    property AllowGrayed: boolean read FAllowGrayed write FAllowGrayed;
    property Anchors;
    property AutoSize: boolean read FAutoSize write SetAutoSize;
    property BiDiMode;
    property Caption;
    property Checked: boolean read FChecked write SetChecked;
    property CheckSize: integer read FCheckSize write SetCheckSize;
    property Color: TColor read FColor write SetColor;
    property Constraints;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Spacing: integer read FSpacing write SetSpacing;
    property State: TCheckBoxState read FState write SetState;
    property Style: TCheckStyle read FStyle write SetStyle;
    property Transparent: boolean read FTransparent write SetTransparent default True;
    property Visible;
    property WordWrap: boolean read FWordWrap write SetWordWrap;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnContextPopup;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnKeyDown;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property TabOrder;
    property TabStop default True;
  end;

implementation

uses Types;

function GetScreenClient(Control: TControl): TPoint;
var
 p: TPoint;
begin
 p := Control.ClientOrigin;
 ScreenToClient(Control.Parent.Handle, p);
 Result := p;
end;

function IsAccellerator(VK: Word; const Str: string): Boolean;
var
  P : Integer;
begin
  P := Pos('&', Str);
  Result := (P <> 0) and (P < Length(Str)) and
    (Upcase(Str[P + 1])=Upcase(Char(VK)));
end;

procedure DrawDiamond(ACanvas:TCanvas; ARect:TRect; AState:TCheckBoxState);
var Offset        : integer;
    OldBrushStyle : TBrushStyle;
begin
  Offset:=(ARect.Right-ARect.Left) div 2;
  with ACanvas do begin
    Pen.Width:=1;
    if AState=cbUnChecked then
      Pen.Color:=clBtnHighlight
    else
      Pen.Color:=clBtnShadow;
    MoveTo(ARect.Left+Offset, ARect.Top);
    LineTo(ARect.Left, ARect.Top+Offset);
    LineTo(ARect.Left+Offset, ARect.Bottom-1);
    if AState=cbUnChecked then
      Pen.Color:=clBtnShadow
    else
      Pen.Color:=clBtnHighlight;
    LineTo(ARect.Right-1, ARect.Top+Offset);
    LineTo(ARect.Left+Offset, ARect.Top);
    if AState<>cbUnchecked then begin
      OldBrushStyle:=Brush.Style;
      Pen.Color:=Brush.Color;
      Brush.Style:=bsSolid;
      if AState=cbChecked then
        Brush.Color:=clBlack
      else
        Brush.Color:=clGray;
      Polygon([Point(ARect.Left+Offset, ARect.Top+1),
              Point(ARect.Right-2, ARect.Top+Offset),
              Point(ARect.Left+Offset, ARect.Bottom-2),
              Point(ARect.Left+1, ARect.Top+Offset)]);
      Brush.Color:=Pen.Color;
      Brush.Style:=OldBrushStyle;
    end;
  end;
end;

procedure DrawTrafficLight(ACanvas:TCanvas; ARect:TRect; AState:TCheckBoxState);
const LightColors : array[TCheckBoxState] of TColor =
  (clLime, clRed, clYellow);
var OldColor      : TColor;
    OldBrushStyle : TBrushStyle;
begin
  with ACanvas do begin
    Pen.Color:=clBlack;
    Pen.Width:=1;
    OldColor:=Brush.Color;
    OldBrushStyle:=Brush.Style;
    Brush.Color:=LightColors[AState];
    Brush.Style:=bsSolid;
    InflateRect(ARect, -1, -1);
    Ellipse(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
    InflateRect(ARect, 1, 1);
    Pen.Color:=clBtnShadow;
    Arc(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom,
        ARect.Right, ARect.Top, ARect.Left, ARect.Bottom);
    Pen.Color:=clBtnHighlight;
    Arc(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom,
        ARect.Left, ARect.Bottom, ARect.Right, ARect.Top);
    Brush.Color:=OldColor;
    Brush.Style:=OldBrushStyle;
  end;
end;

{ Komponente TCROCustomCheckBox }
constructor TCROCustomCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

//  ControlStyle := ControlStyle - [csOpaque];

  {Vorgabewerte setzen}
  FAlignment:=taLeftJustify;
  FAllowGrayed:=false;
  FAutosize:=false;
  FColor:=clBtnFace;
  FChecked:=false;
  FCheckSize:=13;
  FSpacing:=4;
  FState:=cbUnchecked;
  FTransparent:=True;
  FWordWrap:=false;
  FPainting := False;
  FSpaceKeyDown := False;
  FMouseOut     := False;
  Width:=90;
  Height:=15;

  FMouseDown:=False;
  AdjustBounds;

  TabStop := True;
end;

destructor  TCROCustomCheckBox.Destroy;
begin
  inherited Destroy;
end;

procedure TCROCustomCheckBox.AdjustBounds;
var ARect : TRect;
begin
  if FAutoSize then begin
    ARect:=GetTextRect(ClientRect);
    ARect.Right:=ARect.Right+FCheckSize+FSpacing;
    InflateRect(ARect, 1, 1);
    SetBounds(Left, Top, ARect.Right, ARect.Bottom);
  end;
end;

procedure TCROCustomCheckBox.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCROCustomCheckBox.CMDialogChar(var Message: TCMDialogChar);
var AState : integer;
begin
  with Message do begin
    if IsAccellerator(CharCode, Caption) then begin
      AState:=ord(FState);
      inc(AState);
      if (FAllowGrayed and (AState=3)) or (not FAllowGrayed and (AState=2)) then
        AState:=0;
      SetState(TCheckBoxState(AState));
      if Enabled
      then Click;
      Result:=1;
    end
    else
      inherited;
  end;
end;

procedure TCROCustomCheckBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  AdjustBounds;
end;

procedure TCROCustomCheckBox.CMTextChanged(var msg: TMessage);
begin
  inherited;
  AdjustBounds;
  Invalidate;//*
end;

procedure TCROCustomCheckBox.DblClick;
begin
  if Enabled then
    if Assigned(FOnDblClick) then
      FOnDblClick(Self);
end;

procedure TCROCustomCheckBox.DrawTextComp(AText:string; var ARect:TRect; AFormat:Word);
begin
  DrawText(Canvas.Handle, PChar(AText), Length(AText), ARect, AFormat);

  if Focused
  then Canvas.DrawFocusRect(Rect(ARect.Left, ARect.Top, ARect.Left+Canvas.TextWidth(AText)+1, ARect.Bottom));
end;

function TCROCustomCheckBox.GetTextRect(ARect: TRect): TRect;
const WordWraps : array[Boolean] of Word = (0, DT_WORDBREAK);
var AText     : string;
    DC        : HDC;
    OldHandle : THandle;
begin
  Result:=ARect;
  AText:=Self.Caption;
  if (AText='') or ((AText[1]='&') and (AText[2]=#0)) then
    AText:=AText+' ';
  OldHandle:=Canvas.Handle;
  DC:=GetDC(0);
  Canvas.Handle:=DC;
  Canvas.Font:=Font;
  DrawTextComp(AText, Result, (DT_EXPANDTABS or DT_CALCRECT) or WordWraps[FWordWrap]);
  Canvas.Handle:=OldHandle;
  ReleaseDC(0, DC);
end;

procedure TCROCustomCheckBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Enabled then
  begin
    if Visible
    then SetFocus;

    if Button=mbLeft then
    begin
      FMouseDown:=true;
      FStateChanged := False;
    end;
    if Assigned(FOnMouseDown) then
      FOnMouseDown(Self, Button, Shift, X, Y);

    PaintButton;

    Mouse.Capture := WindowHandle;
  end;
end;

procedure TCROCustomCheckBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Enabled and FMouseDown and IsPushed and not FReadOnly then
  begin
    { State-Eigenschaft berechnen }
    if not FStateChanged then
    begin
      if FState=cbChecked then
      begin
        if FAllowGrayed
        then SetState(cbGrayed)
        else SetState(cbUnChecked);
      end
      else
      begin
        if FState=cbGrayed
        then SetState(cbUnChecked)
        else SetState(cbChecked);
      end;
      Click;
    end;
    FStateChanged:=false;

    { OnClick-Ereignis abfeuern }
    if Assigned(FOnMouseUp) then
      FOnMouseUp(Self, Button, Shift, X, Y);
  end;
  FMouseDown    := false;
  FSpaceKeyDown := False;
  PaintButton;
end;

procedure TCROCustomCheckBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Shift, X, Y);
end;

procedure TCROCustomCheckBox.Paint;
var ARect : TRect;
begin
  { Hintergrund zeichnen }
  with Canvas do begin
    if FTransparent then
      Brush.Style:=bsClear
    else begin
      Brush.Style:=bsSolid;
      Brush.Color:=Color;
      ARect:=GetClientRect;
      FillRect(ARect);
    end;
  end;

  { Den Rest zeichnen }
  PaintButton;
  PaintCaption;
end;

procedure TCROCustomCheckBox.PaintCaption;
const
  Alignments: array[TAlignment] of Word =
   (DT_Left, DT_Right, DT_Center);
  WordWraps: array[Boolean] of Word =
   (0, DT_WordBreak);
  Lines: array[Boolean] of Word =
   (DT_SingleLine, 0);
var ARect     : TRect;
    DrawStyle : Integer;
begin
  { Ausgaberechteck für Caption ermitteln }
  ARect:=GetClientRect;
  if FAlignment=taLeftJustify then
    ARect.Left:=ARect.Left+FCheckSize+FSpacing
  else
    ARect.Right:=ARect.Right-FCheckSize-FSpacing;

  { Caption zeichnen }
  Canvas.Font.Assign(Font);
  if not Enabled then begin
    Canvas.Font.Color:=clInactiveCaption;
    OffsetRect(ARect, 1, 1);
  end;
  DrawStyle:=DT_ExpandTabs or DT_VCenter or Lines[FWordWrap] or WordWraps[FWordWrap] or Alignments[FAlignment];
  if not Enabled then
  begin
    Canvas.Brush.Style := bsClear;
    OffsetRect(ARect, 1, 1);
    Canvas.Font.Color := clBtnHighlight;
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), ARect, DrawStyle);
    OffsetRect(ARect, -1, -1);
    Canvas.Font.Color := clBtnShadow;
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), ARect, DrawStyle);
  end
  else
    DrawTextComp(Caption, ARect, DrawStyle);
end;

procedure TCROCustomCheckBox.SetAlignment(newValue: TLeftRight);
begin
  if FAlignment<>newValue then begin
    FAlignment:=newValue;
    Invalidate;
  end;
end;

procedure TCROCustomCheckBox.SetAutosize(newValue: boolean);
begin
  if FAutosize<>newValue then begin
    FAutosize:=newValue;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TCROCustomCheckBox.SetColor(newColor: TColor);
begin
  if FColor<>newColor then begin
    FColor:=newColor;
    Invalidate;
  end;
end;

procedure TCROCustomCheckBox.SetChecked(newValue: boolean);
begin
  if FChecked<>newValue then begin
    FChecked:=newValue;
    if FChecked then
      SetState(cbChecked)
    else
      SetState(cbUnChecked);
    Invalidate;
  end;
end;

procedure TCROCustomCheckBox.SetCheckSize(newValue: integer);
begin
  if FCheckSize<>newValue then begin
    FCheckSize:=newValue;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TCROCustomCheckBox.SetSpacing(newValue: integer);
begin
  if FSpacing<>newValue then begin
    FSpacing:=newValue;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TCROCustomCheckBox.SetState(newValue: TCheckBoxState);
begin
  if FState<>newValue then begin
    FState:=newValue;
    if FState=cbChecked then
      FChecked:=true;
    if FState=cbUnChecked then
      FChecked:=false;
    Change;
    Invalidate;
  end;
end;

procedure TCROCustomCheckBox.SetStyle(newValue: TCheckStyle);
begin
  if FStyle<>newValue then begin
    FStyle:=newValue;
    Invalidate;
  end;
end;

procedure TCROCustomCheckBox.SetTransparent(newValue: boolean);
begin
  if FTransparent <> newValue then
  begin
    FTransparent := newValue;
{    if FTransparent
    then ControlStyle := ControlStyle - [csOpaque]
    else ControlStyle := ControlStyle + [csOpaque];}
    Invalidate;
  end;                         
end;

procedure TCROCustomCheckBox.SetWordWrap(newValue: boolean);
begin
  if FWordWrap<>newValue then begin
    FWordWrap:=newValue;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TCROCustomCheckBox.CreateWindowHandle(const Params: TCreateParams);
begin
  with Params do
    WindowHandle := CreateWindowEx(ExStyle{ or WS_EX_TRANSPARENT}, WinClassName, Caption, Style,//** com WS_EX_TRANSPARENT da problema ao mover um graphiccontrol q esteja atras - veja Pen Programer's Help
      X, Y, Width, Height, WndParent, 0, WindowClass.hInstance, Param);
end;

procedure TCROCustomCheckBox.WMPaint(var Message: TWMPaint);
begin
 inherited;
 if FTransparent then
 if not FPainting then
 RepaintWindow;
end;

procedure TCROCustomCheckBox.WMEraseBkGnd(var Message: TWMEraseBkGnd);
var
 DC: hDC;
 i: integer;
 p: TPoint;
begin
 if FTransparent then
 begin
  if Assigned(Parent) then
  begin
   DC := Message.DC;
   i := SaveDC(DC);
   p := GetScreenClient(self);
   p.x := -p.x;
   p.y := -p.y;
   MoveWindowOrg(DC, p.x, p.y);
   SendMessage(Parent.Handle, $0014, DC, 0);
   TCtrl(Parent).PaintControls(DC, nil);
   RestoreDC(DC, i);
  end;
 end else inherited;
end;

procedure TCROCustomCheckBox.RepaintWindow;
var
 DC: hDC;
 TmpBitmap, Bitmap: hBitmap;
begin
 if FTransparent then
 begin
  FPainting := true;
  HideCaret(Handle);
  DC := CreateCompatibleDC(GetDC(Handle));
  TmpBitmap := CreateCompatibleBitmap(GetDC(Handle), Succ(ClientWidth), Succ(ClientHeight));
  Bitmap := SelectObject(DC, TmpBitmap);
  PaintTo(DC, 0, 0);
  BitBlt(GetDC(Handle), 1, 1, ClientWidth-1, ClientHeight-1, DC, 1, 1, SRCCOPY);
  SelectObject(DC, Bitmap);
  DeleteDC(DC);
  ReleaseDC(Handle, GetDC(Handle));
  DeleteObject(TmpBitmap);
  ShowCaret(Handle);
  FPainting := false;
 end;
end;

procedure TCROCustomCheckBox.CMParentColorChanged(var Message: TMessage);
begin
 inherited;
 if FTransparent then
 Invalidate;
end;

procedure TCROCustomCheckBox.CNCtlColorEdit(var Message: TWMCtlColorEdit);
begin
 inherited;
 if FTransparent then
 SetBkMode(Message.ChildDC, 1);
end;

procedure TCROCustomCheckBox.CNCtlColorStatic(
  var Message: TWMCtlColorStatic);
begin
 inherited;
 if FTransparent then
 SetBkMode(Message.ChildDC, 1);
end;

procedure TCROCustomCheckBox.WMMove(var Message: TWMMove);
begin
 inherited;
 Invalidate;
end;

procedure TCROCustomCheckBox.WMSize(var Message: TWMSize);
begin
 inherited;
 Invalidate;
end;

procedure TCROCustomCheckBox.WMKillFocus(var Message: TWMSetFocus);
begin
  inherited;
  Paint;
end;

procedure TCROCustomCheckBox.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  Paint;
end;

procedure TCROCustomCheckBox.WMKeyDown(var Message: TWMKeyDown);
begin
  inherited;

  if Message.CharCode = VK_SPACE then
  begin
    FSpaceKeyDown := True;
//    MouseDown(mbLeft, [], Self.Left, Self.Top);
    PostMessage(WindowHandle, WM_LBUTTONDOWN, Self.Left+1, Self.Top+1);
  end;
end;

procedure TCROCustomCheckBox.WMKeyUp(var Message: TWMKeyUp);
begin
  inherited;

  if Message.CharCode = VK_SPACE then
  begin
//    MouseUp(mbLeft, [], Self.Left, Self.Top);
    PostMessage(WindowHandle, WM_LBUTTONUP, Self.Left+1, Self.Top+1);
  end;
end;

function TCROCustomCheckBox.IsPushed: Boolean;
begin
  Result := FMouseDown;

  if FMouseDown and (not FSpaceKeyDown) then
  begin
    Result := not FMouseOut;
  end;
end;

procedure TCROCustomCheckBox.WMMouseMove(var Message: TWMMouseMove);
begin
  inherited;

  if not (Message.XPos in [0..Width]) or
     not (Message.YPos in [0..Height])
  then FMouseOut := True
  else FMouseOut := False;

  PaintButton;
end;

procedure TCROCustomCheckBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Paint;
end;

procedure TCROCustomCheckBox.SetReadOnly(Value: Boolean);
begin
  FReadOnly := Value;
end;

procedure TCROCustomCheckBox.PaintButton;
var ARect : TRect;
begin
  ARect:=GetClientRect;

  if FAlignment<>taLeftJustify
  then ARect.Left:=ARect.Right-FCheckSize;
  ARect.Top:=(ARect.Bottom-ARect.Top-FCheckSize) div 2;

  PaintButton(Canvas, ARect.Left, ARect.Top, FState);
end;

procedure TCROCustomCheckBox.PaintButton(ACanvas: TCanvas; ALeft, ATop: Integer;
  AState: TCheckBoxState; IgnoreIsPushed: Boolean = False);
var ARect : TRect;
    iPushed, dx, dy: Integer;
begin
  ARect:=GetClientRect;

  dx := ALeft - ARect.Left;
  dy := ATop - ARect.Top;
  OffsetRect(ARect, dx, dy);

  if FAlignment=taLeftJustify then
    ARect.Right:=ARect.Left+FCheckSize
  else
    ARect.Left:=ARect.Right-FCheckSize;
  ARect.Bottom:=ARect.Top+FCheckSize;

  with ACanvas do begin
    if FStyle=csCheckBox then
    begin
      if ((not IgnoreIsPushed and IsPushed) and not FReadOnly) or not Enabled
      then iPushed := DFCS_PUSHED
      else iPushed := 0;

      if AState=cbUnchecked then
        DrawFrameControl(Handle, ARect, DFC_Button, DFCS_ButtonCheck or iPushed);
      if AState=cbChecked then
        DrawFrameControl(Handle, ARect, DFC_Button, DFCS_ButtonCheck or DFCS_Checked or iPushed);
      if AState=cbGrayed then
        DrawFrameControl(Handle, ARect, DFC_Button, DFCS_ButtonCheck or DFCS_Checked or DFCS_Inactive or iPushed);
    end;

    if FStyle=csRadioButton then begin
      if (not IgnoreIsPushed and IsPushed) or not Enabled
      then iPushed := DFCS_Inactive
      else iPushed := 0;

      if AState=cbUnchecked then
        DrawFrameControl(Handle, ARect, DFC_Button, DFCS_ButtonRadio or iPushed);
      if AState=cbChecked then
        DrawFrameControl(Handle, ARect, DFC_Button, DFCS_ButtonRadio or DFCS_Checked or iPushed);
      if AState=cbGrayed then
        DrawFrameControl(Handle, ARect, DFC_Button, DFCS_ButtonRadio or DFCS_Checked or DFCS_Inactive or iPushed);
    end;

    if FStyle=csPushButton then begin
      if (not IgnoreIsPushed and IsPushed) or not Enabled
      then iPushed := DFCS_Flat
      else iPushed := 0;

      if AState=cbUnchecked then
        DrawFrameControl(Handle, ARect, DFC_Button, DFCS_ButtonPush or iPushed);
      if AState=cbChecked then
        DrawFrameControl(Handle, ARect, DFC_Button, DFCS_ButtonPush or DFCS_Pushed or iPushed);
      if AState=cbGrayed then
        DrawFrameControl(Handle, ARect, DFC_Button, DFCS_ButtonPush or DFCS_Flat or iPushed);
    end;

    if FStyle=csDiamond then
      DrawDiamond(Canvas, ARect, AState);

    if FStyle=csTrafficLight then
      DrawTrafficLight(Canvas, ARect, AState);
  end;
end;

end.
