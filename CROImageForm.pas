unit CROImageForm;

interface

uses
  SysUtils, Classes, Controls, CROSkinImage, Forms, Windows, Messages, Graphics,
  StdCtrls, CROComponentsCommon, Dialogs, CROSkinProperties, CROSkinControl;

type
  TObjectRect = class(TPersistent)
  private
    FTop: Word;
    FLeft: Word;
    FRight: Word;
    FBottom: Word;
    FOnChange: TNotifyEvent;
    procedure DoChange;
    procedure SetBottom(const Value: Word);
    procedure SetLeft(const Value: Word);
    procedure SetRight(const Value: Word);
    procedure SetTop(const Value: Word);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Left: Word read FLeft write SetLeft;
    property Top: Word read FTop write SetTop;
    property Right: Word read FRight write SetRight;
    property Bottom: Word read FBottom write SetBottom;
  public
    function Rect: TRect;
  end;

  TTaskButton = (tbNone, tbClose,tbRestore,tbMinimize);

  TCROImageForm = class(TCROCustomSkinImage, ICROSkinControl)
  private
    FTaskButton: TTaskButton;
    FPicture: TPicture;
    FCaptionRect: TObjectRect;
    FShowDesign: Boolean;
    FRegion: HRgn;
    FProperties: TCROImageProperties;
    FButtonRestoreRect: TObjectRect;
    FButtonCloseRect: TObjectRect;
    FButtonMinimizeRect: TObjectRect;
    FTransparentColor: TColor;
//    FWindowState: TWindowState;
    procedure SetProperties(const Value: TCROImageProperties);
    procedure SetShowDesign(const Value: Boolean);
    //fc i
    procedure ReadRegions(Reader: TStream);
    procedure WriteRegions(Writer: TStream);
    //fc f
  protected
    //fc i
    LastFocusRect: TRect;
    DraggingForm: Boolean;
    procedure DoChangeRects(Sender: TObject);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function GetTransparentColor: TColor;
    procedure DestroyWnd;
    procedure WndProc(var Message: TMessage); override;
    procedure DefineProperties(Filer: TFiler);override;
    procedure SetParent(Value:TWinControl); override;
    procedure AfterFormWndProc(var Message: TMessage); virtual;
    procedure MouseLoop(X, Y: Integer); virtual;
    procedure MouseLoop_MouseMove(X, Y: Integer; ACursorPos: TPoint;  var FirstTime: Boolean; var FocusRect: TRect; OriginalRect:TRect); virtual;
    procedure MouseLoop_MouseUp(X, Y: Integer; ACursorPos: TPoint;
      OriginalRect, FocusRect: TRect); virtual;
    //fc f
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateSkin;
    procedure Paint; override;
  published
    property Visible;
    property TransparentColor: TColor read FTransparentColor write FTransparentColor default clRed;
    property ShowDesign: Boolean read FShowDesign write SetShowDesign default False;
    property CaptionRect: TObjectRect read FCaptionRect write FCaptionRect;
    property ButtonCloseRect: TObjectRect read FButtonCloseRect write FButtonCloseRect;
    property ButtonRestoreRect: TObjectRect read FButtonRestoreRect write FButtonRestoreRect;
    property ButtonMinimizeRect: TObjectRect read FButtonMinimizeRect write FButtonMinimizeRect;
    property Properties: TCROImageProperties read FProperties write SetProperties;
//    property WindowState: TWindowState read FWindowState write FWindowState;
  public
    //fc i
    Patch: Variant;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ApplyBitmapRegion; virtual;
    procedure ScaleRegion(xFact, yFact:Single);virtual;
    property RegionData: HRgn read FRegion stored True;
    //fc f
  end;

implementation

const
  DragTolerance = 5;

constructor TCROImageForm.Create(AOwner: TComponent);
begin
  if not AOwner.InheritsFrom(TCustomForm)
  then raise Exception.Create('Owner must be a form!');

  inherited;
  Transparent := False;
  Align := alClient;
  TForm(Owner).Position := poScreenCenter;
  //fc i
  FPicture := TPicture.Create;
  FActivePicture := FPicture;
  FCaptionRect := TObjectRect.Create;
  FButtonRestoreRect := TObjectRect.Create;
  FButtonCloseRect := TObjectRect.Create;
  FButtonMinimizeRect := TObjectRect.Create;
  FRegion := 0;
  FTransparentColor := clRed;

  FCaptionRect.Left := 0;
  FCaptionRect.Right := 0;
  FCaptionRect.Top := 0;
  FCaptionRect.Bottom := 0;
  FCaptionRect.OnChange := DoChangeRects;

  FButtonRestoreRect.Left := 0;
  FButtonRestoreRect.Right := 0;
  FButtonRestoreRect.Top := 0;
  FButtonRestoreRect.Bottom := 0;
  FButtonRestoreRect.OnChange := DoChangeRects;

  FButtonCloseRect.Left := 0;
  FButtonCloseRect.Right := 0;
  FButtonCloseRect.Top := 0;
  FButtonCloseRect.Bottom := 0;
  FButtonCloseRect.OnChange := DoChangeRects;

  FButtonMinimizeRect.Left := 0;
  FButtonMinimizeRect.Right := 0;
  FButtonMinimizeRect.Top := 0;
  FButtonMinimizeRect.Bottom := 0;
  FButtonMinimizeRect.OnChange := DoChangeRects;
  //fc f

  TForm(Owner).WindowState := wsNormal; //never accept other value due to region definition
end;

procedure TCROImageForm.Paint;
begin
  if FShowDesign or (not (csDesigning in ComponentState))
  then inherited Paint
  else
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.FillRect(DestRect);
  end;


  if (FCaptionRect.Right > FCaptionRect.Left) and
     (FCaptionRect.Bottom > FCaptionRect.Top) then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Color := Font.Color;
    Canvas.Pen.Style := psDot;
    Canvas.Rectangle(FCaptionRect.Rect);
  end;

  if (FButtonCloseRect.Right > FButtonCloseRect.Left) and
     (FButtonCloseRect.Bottom > FButtonCloseRect.Top) then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Color := Font.Color;
    Canvas.Pen.Style := psDot;
    Canvas.Rectangle(FButtonCloseRect.Rect);
  end;

  if (FButtonRestoreRect.Right > FButtonRestoreRect.Left) and
     (FButtonRestoreRect.Bottom > FButtonRestoreRect.Top) then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Color := Font.Color;
    Canvas.Pen.Style := psDot;
    Canvas.Rectangle(FButtonRestoreRect.Rect);
  end;

  if (FButtonMinimizeRect.Right > FButtonMinimizeRect.Left) and
     (FButtonMinimizeRect.Bottom > FButtonMinimizeRect.Top) then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Color := Font.Color;
    Canvas.Pen.Style := psDot;
    Canvas.Rectangle(FButtonMinimizeRect.Rect);
  end;
end;

procedure TCROImageForm.SetShowDesign(const Value: Boolean);
begin
  FShowDesign := Value;
  TForm(Owner).Refresh;
end;

//fc i
procedure TCROImageForm.AfterFormWndProc(var Message: TMessage);
begin
  if not (csDesigning in componentstate) then
  case Message.Msg of
    WM_DESTROY: DestroyWnd;
  end;
end;

procedure TCROImageForm.ApplyBitmapRegion;
begin
  SetWindowRgn(GetParentForm(self).Handle, 0, False);
  if FRegion <> 0 then DeleteObject(FRegion);
  FRegion := CreateRegionFromBitmap(Picture.Bitmap, GetTransparentColor);
  if not (csDesigning in ComponentState)
  then SetWindowRgn(GetParentForm(self).Handle, FRegion, True);
end;

procedure TCROImageForm.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('RegionData', ReadRegions, WriteRegions, True);
end;

destructor TCROImageForm.Destroy;
begin
  FPicture.Free;
  if FRegion <> 0 then DeleteObject(FRegion);
  FCaptionRect.Free;
  FButtonRestoreRect.Free;
  FButtonCloseRect.Free;
  FButtonMinimizeRect.Free;
  inherited Destroy;
end;

procedure TCROImageForm.DestroyWnd;
begin
  if FRegion <> 0 then
  begin
    SetWindowRgn(GetParentForm(self).Handle, 0, False);
    DeleteObject(FRegion);
    FRegion := 0;
  end;
end;

procedure TCROImageForm.MouseLoop(X, Y: Integer);
var ACursor: TPoint;
    Msg: TMsg;
    FirstTime: Boolean;
    OriginalRect, FocusRect: TRect;
begin
  FirstTime := True;
  with Parent do OriginalRect := Rect(Left, Top, Left + Width, Top + Height);
  FocusRect := Rect(0, 0, 0, 0);
  with GetParentForm(self) do
  begin
    SetCapture(Handle);
    try
      while GetCapture = Handle do
      begin
        GetCursorPos(ACursor);
        case Integer(GetMessage(Msg, 0, 0, 0)) of
          -1: Break;
          0: begin
            PostQuitMessage(Msg.WParam);
            Break;
          end;
        end;
        case Msg.Message of
          WM_MOUSEMOVE: MouseLoop_MouseMove(X, Y, ACursor, FirstTime, FocusRect, OriginalRect);
          WM_LBUTTONUP: begin
            MouseLoop_MouseUp(X, Y, ACursor, OriginalRect, FocusRect);

            TranslateMessage(Msg);   // So OnMouseUp fires
            DispatchMessage(Msg);

            if GetCapture = Handle then ReleaseCapture;
          end;
          else begin // 12/07/98 - Following code needed to prevent eating of messages.
            TranslateMessage(Msg);
            DispatchMessage(Msg);
          end;
        end;
      end;
    finally
      if GetCapture = Handle then ReleaseCapture;
    end;
  end;
end;

procedure TCROImageForm.MouseLoop_MouseMove(X, Y: Integer;
  ACursorPos: TPoint; var FirstTime: Boolean; var FocusRect: TRect;
  OriginalRect: TRect);
var p: TPoint;
    Msg: TMsg;
begin
  p := ClientToScreen(Point(x, y));
  if (Abs(ACursorPos.X - p.x) <= DragTolerance) and
     (Abs(ACursorPos.Y - p.y) <= DragTolerance)
  then Exit;

  with GetParentForm(self) do
  begin
    DraggingForm := True;
    sleep(10);
    while PeekMessage(Msg, Handle, WM_MOUSEMOVE, WM_MOUSEMOVE, PM_REMOVE) do;
    GetCursorPos(ACursorPos);
    SetWindowPos(Handle, 0, ACursorPos.x - x, ACursorPos.y - y, 0, 0,
         SWP_NOZORDER or SWP_NOSIZE or SWP_NOACTIVATE);
  end;
end;

procedure TCROImageForm.MouseLoop_MouseUp(X, Y: Integer;
  ACursorPos: TPoint; OriginalRect, FocusRect: TRect);
begin
  if not DraggingForm
  then Exit;
  DraggingForm := False;
end;

procedure TCROImageForm.ScaleRegion(xFact, yFact: Single);
var
   size:integer;
   rgndata: pRGNData;
   newregion,existingrgn:HRgn;
   xform:TXForm;
begin
  existingrgn := CreateRectRgn(0,0,1,1);
  GetWindowRgn(GetParentForm(self).handle,existingrgn);
  Size := GetRegionData(existingrgn, 0, nil);
  if Size > 0 then
  begin
    Getmem(RgnData,size);
    try
      GetRegionData(existingrgn, Size, RgnData);
      FillChar(Xform,sizeof(xform),0);
      xform.eM11 := xfact;
      xform.em22 := yfact;
      newRegion := ExtCreateRegion(@xform,size,rgndata^);

      SetWindowRgn(GetParentForm(self).Handle, 0, False);
      if FRegion <> 0 then DeleteObject(FRegion);

      SetWindowRgn(GetParentForm(self).Handle,newRegion,true)
    finally
      FreeMem(RgnData);
      DeleteObject(existingrgn);
    end;
  end;
end;

procedure TCROImageForm.SetParent(Value: TWinControl);
begin
  if (Value <> nil) and not (Value is TCustomForm)
  then Value := GetParentForm(Value);

  inherited SetParent(value);

  if Parent <> nil
  then SetWindowLong(Parent.Handle, GWL_STYLE, GetWindowLong(Parent.Handle, GWL_STYLE) and not WS_CLIPCHILDREN);

  if Value <> Nil
  then TForm(Value).BorderStyle:= bsNone;
end;

procedure TCROImageForm.WndProc(var Message: TMessage);
begin
  inherited;
end;

procedure TCROImageForm.WriteRegions(Writer: TStream);
var
   size:integer;
   rgndata: pRGNData;
   stat: integer;
begin
  ApplyBitmapRegion;
  if (FRegion <> 0) then
  begin
    Size := GetRegionData(FRegion, 0, nil);
    Writer.Write(Size, SizeOf(Size));
    if Size > 0 then
    begin
      Getmem(RgnData,size);
      try
        Stat := GetRegionData(FRegion, Size, RgnData);
        if Stat > 0 then Writer.Write(RgnData^, Size);
      finally
        FreeMem(RgnData);
      end;
    end;
  end else begin
    Size := 0;
    Writer.Write(Size, SizeOf(Size));
  end;
end;
//fc f

function TCROImageForm.GetTransparentColor: TColor;
begin
  result := TransparentColor;
  if TransparentColor = clNone then
  begin
    if (Picture.Bitmap <> nil)
    then result := Picture.Bitmap.Canvas.Pixels[0,Picture.Bitmap.height-1]
  end
  else
    result := TransparentColor;
end;

procedure TCROImageForm.ReadRegions(Reader: TStream);
var
   rgnsize:integer;
   rgndata: pRGNData;
begin
  Reader.Read(RgnSize, 4);

  if RgnSize <> 0 then
  begin
    GetMem(RgnData, RgnSize);
    try
      Reader.Read(RgnData^,rgnSize);
      FRegion := ExtCreateRegion(nil, RgnSize, RgnData^);

      if not (csDesigning in ComponentState) and (FRegion<>0) then
        SetWindowRgn(parent.handle,Fregion,true)
    finally
      FreeMem(RgnData);
    end;
  end else begin
    FRegion := 0;
    ApplyBitmapRegion;
  end
end;

procedure TCROImageForm.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if ((Button = mbLeft) and (
       (X in [FCaptionRect.Left..FCaptionRect.Right]) and
       (Y in [FCaptionRect.Top..FCaptionRect.Bottom])
     ) or (
       (FCaptionRect.Right <= FCaptionRect.Left) or
       (FCaptionRect.Bottom <= FCaptionRect.Top)
     )) and not (
       (
         (X in [FButtonCloseRect.Left..FButtonCloseRect.Right]) and
         (Y in [FButtonCloseRect.Top..FButtonCloseRect.Bottom])
       ) or
       (
         (X in [FButtonRestoreRect.Left..FButtonRestoreRect.Right]) and
         (Y in [FButtonRestoreRect.Top..FButtonRestoreRect.Bottom])
       ) or
       (
         (X in [FButtonMinimizeRect.Left..FButtonMinimizeRect.Right]) and
         (Y in [FButtonMinimizeRect.Top..FButtonMinimizeRect.Bottom])
       )
     ) then
  begin
    ReleaseCapture;
    PostMessage(TWinControl(Owner).Handle, WM_SYSCOMMAND, SC_MOVE + 2, 0);
  end
  else
  begin
    if (X in [FButtonCloseRect.Left..FButtonCloseRect.Right]) and
       (Y in [FButtonCloseRect.Top..FButtonCloseRect.Bottom])
    then FTaskButton := tbClose
    else
    if (X in [FButtonRestoreRect.Left..FButtonRestoreRect.Right]) and
       (Y in [FButtonRestoreRect.Top..FButtonRestoreRect.Bottom])
    then FTaskButton := tbRestore
    else
    if (X in [FButtonMinimizeRect.Left..FButtonMinimizeRect.Right]) and
       (Y in [FButtonMinimizeRect.Top..FButtonMinimizeRect.Bottom])
    then FTaskButton := tbMinimize
    else FTaskButton := tbNone;
  end;
  inherited;
end;

procedure TCROImageForm.DoChangeRects(Sender: TObject);
begin
  Refresh;
end;

procedure TCROImageForm.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FProperties
    then FProperties := nil;
  end;
end;

procedure TCROImageForm.SetProperties(const Value: TCROImageProperties);
begin
  if (FProperties <> Value) then
  begin
    if Assigned(Value) then
    begin
      if Assigned(FProperties) then
      begin
        FProperties.RemoveControl(Self);
        FProperties.RemoveFreeNotification(Self);
      end
      else
      begin
        Value.AddControl(Self);
        Value.FreeNotification(Self);
      end;
    end
    else
    begin
      FRegion := 0;
      ApplyBitmapRegion;
    end;
    FProperties := Value;
    UpdateSkin;
  end;
end;

procedure TCROImageForm.UpdateSkin;
begin
  if Assigned(FProperties) then
  begin
    FActivePictureSet := FProperties.PictureSet;
  end
  else
    FActivePictureSet := nil;
  DoPictureSetChange(Self);
  SendToBack;
end;

{ TObjectRect }

procedure TObjectRect.DoChange;
begin
  if Assigned(FOnChange)
  then FOnChange(Self);
end;

function TObjectRect.Rect: TRect;
begin
  Result.Left := FLeft;
  Result.Top := FTop;
  Result.Right := FRight;
  Result.Bottom := FBottom;
end;

procedure TObjectRect.SetBottom(const Value: Word);
begin
  FBottom := Value;
  DoChange;
end;

procedure TObjectRect.SetLeft(const Value: Word);
begin
  FLeft := Value;
  DoChange;
end;

procedure TObjectRect.SetRight(const Value: Word);
begin
  FRight := Value;
  DoChange;
end;

procedure TObjectRect.SetTop(const Value: Word);
begin
  FTop := Value;
  DoChange;
end;

procedure TCROImageForm.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbLeft) then
  begin
    if (X in [FButtonCloseRect.Left..FButtonCloseRect.Right]) and
       (Y in [FButtonCloseRect.Top..FButtonCloseRect.Bottom]) and
       (FTaskButton = tbClose)
    then TForm(Owner).Close
    else
    if (X in [FButtonRestoreRect.Left..FButtonRestoreRect.Right]) and
       (Y in [FButtonRestoreRect.Top..FButtonRestoreRect.Bottom]) and
       (FTaskButton = tbRestore) then
    begin
      if TForm(Owner).WindowState = wsMaximized
      then TForm(Owner).WindowState := wsNormal
      else TForm(Owner).WindowState := wsMaximized;
    end
    else
    if (X in [FButtonMinimizeRect.Left..FButtonMinimizeRect.Right]) and
       (Y in [FButtonMinimizeRect.Top..FButtonMinimizeRect.Bottom]) and
       (FTaskButton = tbMinimize)
    then Application.Minimize;
  end;
  FTaskButton := tbNone;
  inherited;
end;
{
procedure TCROImageForm.Loaded;
begin
  inherited;
  TForm(Owner).WindowState := FWindowState;
end;

function TCROImageForm.BitmapHeight: Word;
begin
  if (FWindowState = wsMaximized) and (not (csDesigning in ComponentState))
  then Result := Screen.Height
  else Result := inherited BitmapHeight;
end;

function TCROImageForm.BitmapWidth: Word;
begin
  if (FWindowState = wsMaximized) and (not (csDesigning in ComponentState))
  then Result := Screen.Width
  else Result := inherited BitmapWidth;
end;
}
end.
