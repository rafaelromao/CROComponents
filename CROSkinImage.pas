unit CROSkinImage;

interface

uses
  SysUtils, Classes, Controls, Graphics, Types, Windows, Forms, Messages,
  Dialogs, Math, CROComponentsCommon, TypInfo, Contnrs, CROPictureSet, CROSkinProperties,
  CROCustomCheckBox;

type
  TCROImageProperties = class;

  TCROCustomSkinImage = class(TGraphicControl)//NoWindowed - veja em CROSkinControl.pas a versão Windowed
  private
    FDrawing: Boolean;
    FTransparent: Boolean;
    FOnDebug: TNotifyEvent;
    function GetCanvas: TCanvas;
    procedure SetTransparent(const Value: Boolean);
  protected
    FActivePicture: TPicture;
    FActivePictureSet: TCROPictureSplit;
    procedure PictureChanged(Sender: TObject); virtual;
    function DoPaletteChange: Boolean;
    function DestRect: TRect;
    function GetPalette: HPALETTE; override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure Resize; override;
    procedure Paint; override;
    procedure DoPictureSetChange(Sender: TObject); virtual;
    function RecreatePicture:Boolean;
    property Transparent: Boolean read FTransparent write SetTransparent;
    property Align;
    property Anchors;
    property Visible;
  public
    property Picture: TPicture read FActivePicture;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas: TCanvas read GetCanvas;
    property OnMouseDown;
    property OnMouseUp;
    property OnDebug: TNotifyEvent read FOnDebug write FOnDebug;
  end;

  TCROSkinImage = class(TCROCustomSkinImage, ICROSkinControl)
  private
    FPicture: TPicture;
    FProperties: TCROImageProperties;
    procedure SetProperties(const Value: TCROImageProperties);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateSkin;
  published
    property Align;
    property Anchors;
    property Visible;
    property Properties: TCROImageProperties read FProperties write SetProperties;
    property OnDebug;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TCROImageProperties = class(TCROSkinProperties)
  private
    FPictureSet: TCROPictureSplit;
    FTransparent: Boolean;
    procedure SetTransparent(const Value: Boolean);
  published
    property PictureSet: TCROPictureSplit read FPictureSet write FPictureSet;
    property Transparent: Boolean read FTransparent write SetTransparent default True;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TCROCustomSkinImage }

function TCROCustomSkinImage.CanAutoSize(var NewWidth,
  NewHeight: Integer): Boolean;
begin
  Result := True;
  if not (csDesigning in ComponentState) or
         (FActivePicture.Width > 0) and (FActivePicture.Height > 0) then
  begin
    if Align in [alNone, alLeft, alRight]
    then NewWidth := FActivePicture.Width;
    if Align in [alNone, alTop, alBottom]
    then NewHeight := FActivePicture.Height;
  end;
end;

constructor TCROCustomSkinImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FActivePictureSet := nil;
  Height := 40;
  Width := 40;
  FTransparent := True;
  if Owner.InheritsFrom(TWinControl)
  then TWinControl(Owner).DoubleBuffered := True;
end;

destructor TCROCustomSkinImage.Destroy;
begin
  inherited Destroy;
end;

procedure TCROCustomSkinImage.Paint;
var Save: Boolean; //i:Integer;
begin
  if (csDesigning in ComponentState) then
  begin
    with Canvas do
    begin
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;
  end;
  Save := FDrawing;
  if Assigned(FActivePictureSet) and Assigned(FActivePicture) then
  begin
    FDrawing := True;
    try
      Canvas.StretchDraw(DestRect, FActivePicture.Graphic);
    finally
      FDrawing := Save;
    end;
  end;

{  //debug
  if Owner.InheritsFrom(TWinControl) then
  begin
    for i := 0 to TWinControl(Owner).ControlCount-1 do
    begin
      if TWinControl(Owner).Controls[i].InheritsFrom(TCROCustomCheckBox)
      then TWinControl(Owner).Controls[i].Repaint;
    end;
  end;}

  if Assigned(FOnDebug)
  then FOnDebug(Self);
end;

procedure TCROCustomSkinImage.PictureChanged(Sender: TObject);
var
  G: TGraphic;
  D : TRect;
begin
  if (not (Assigned(FActivePicture))) or (csDesigning in ComponentState)
  then Exit;

  if AutoSize and (FActivePicture.Width > 0) and (FActivePicture.Height > 0)
  then SetBounds(Left, Top, FActivePicture.Width, FActivePicture.Height);

  G := FActivePicture.Graphic;
  if G <> nil then
  begin
    if not ((G is TMetaFile) or (G is TIcon))
    then G.Transparent := FTransparent;
    D := DestRect;
    if (not G.Transparent) and (D.Left <= 0) and (D.Top <= 0) and
       (D.Right >= Width) and (D.Bottom >= Height)
    then ControlStyle := ControlStyle + [csOpaque]
    else ControlStyle := ControlStyle - [csOpaque];  // picture might not cover entire clientrect
    if DoPaletteChange and FDrawing
    then Update;
  end
  else
    ControlStyle := ControlStyle - [csOpaque];

  if not FDrawing
  then Invalidate;
end;

function TCROCustomSkinImage.DoPaletteChange: Boolean;
var
  ParentForm: TCustomForm;
  Tmp: TGraphic;
begin
  Result := False;
  Tmp := FActivePicture.Graphic;
  if Visible and (not (csLoading in ComponentState)) and (Tmp <> nil) and
	  (Tmp.PaletteModified) then
  begin
    if (Tmp.Palette = 0)
    then Tmp.PaletteModified := False
    else
    begin
      ParentForm := GetParentForm(Self);
      if Assigned(ParentForm) and ParentForm.Active and Parentform.HandleAllocated then
      begin
        if FDrawing
        then ParentForm.Perform(wm_QueryNewPalette, 0, 0)
        else PostMessage(ParentForm.Handle, wm_QueryNewPalette, 0, 0);
        Result := True;
        Tmp.PaletteModified := False;
      end;
    end;
  end;
end;


function TCROCustomSkinImage.GetPalette: HPALETTE;
begin
  Result := 0;
  if FActivePicture.Graphic <> nil
  then Result := FActivePicture.Graphic.Palette;
end;

procedure TCROCustomSkinImage.DoPictureSetChange(Sender: TObject);
begin
  RecreatePicture;
end;

function TCROCustomSkinImage.DestRect: TRect;
var
  w, h, cw, ch: Integer;
  xyaspect: Double;
begin
  w := FActivePicture.Width;
  h := FActivePicture.Height;
  cw := ClientWidth;
  ch := ClientHeight;
  if (w > cw) or (h > ch) then
  begin
    if (w > 0) and (h > 0) then
    begin
      xyaspect := w / h;
      if w > h then
      begin
        w := cw;
        h := Trunc(cw / xyaspect);
        if h > ch then  // woops, too big
        begin
          h := ch;
          w := Trunc(ch * xyaspect);
        end;
      end
      else
      begin
        h := ch;
        w := Trunc(ch * xyaspect);
        if w > cw then  // woops, too big
        begin
          w := cw;
          h := Trunc(cw / xyaspect);
        end;
      end;
    end
    else
    begin
      w := cw;
      h := ch;
    end;
  end;

  with Result do
  begin
    Left := 0;
    Top := 0;
    Right := w;
    Bottom := h;
  end;
end;

function TCROCustomSkinImage.GetCanvas: TCanvas;
begin
  Result := inherited Canvas;
end;

function TCROCustomSkinImage.RecreatePicture: Boolean;
var SourceRect, Rect: TRect; K:Word; P: TPicture;
begin
  Result := False;
  //draw image in FActivePicture object
  if (Assigned(FActivePictureSet)) and (Assigned(FActivePicture)) then
  begin
    if (
        (FActivePictureSet.LeftTop.Width = 0) and (FActivePictureSet.RightTop.Width = 0) and
        (FActivePictureSet.RightBottom.Width = 0) and (FActivePictureSet.LeftBottom.Width = 0) and
        (FActivePictureSet.LeftMiddle.Width = 0) and (FActivePictureSet.RightMiddle.Width = 0) and
        (FActivePictureSet.CenterTop.Width = 0) and (FActivePictureSet.CenterBottom.Width = 0) and
        (FActivePictureSet.CenterMiddle.Width = 0)
       ) then
    begin
      P := TPicture.Create;
      try
        FActivePicture.Assign(P);
      finally
        P.Free;
      end;
    end
    else
    begin
      FActivePicture.Bitmap.Width := Width;
      FActivePicture.Bitmap.Height := Height;
      //draw left top
      if FActivePictureSet.LeftTop.Width > 0 then
      begin
        SourceRect.Top := 0;
        SourceRect.Left := 0;
        SourceRect.Right := FActivePictureSet.LeftTop.Width;
        SourceRect.Bottom := FActivePictureSet.LeftTop.Height;
        Rect.Top := 0;
        Rect.Left := 0;
        Rect.Right := FActivePictureSet.LeftTop.Width;
        Rect.Bottom := FActivePictureSet.LeftTop.Height;
        FActivePicture.Bitmap.Canvas.CopyRect(Rect,FActivePictureSet.LeftTop.Bitmap.Canvas,SourceRect);
      end;
      //draw left bottom
      if FActivePictureSet.LeftBottom.Width > 0 then
      begin
        SourceRect.Top := 0;
        SourceRect.Left := 0;
        SourceRect.Right := FActivePictureSet.LeftBottom.Width;
        SourceRect.Bottom := FActivePictureSet.LeftBottom.Height;
        Rect.Top := FActivePicture.Bitmap.Height - FActivePictureSet.LeftBottom.Height;
        Rect.Left := 0;
        Rect.Right := FActivePictureSet.LeftBottom.Width;
        Rect.Bottom := FActivePicture.Bitmap.Height;
        FActivePicture.Bitmap.Canvas.CopyRect(Rect,FActivePictureSet.LeftBottom.Bitmap.Canvas,SourceRect);
      end;
      //draw left middle
      if FActivePictureSet.LeftMiddle.Width > 0 then
      begin
        SourceRect.Top := 0;
        SourceRect.Left := 0;
        SourceRect.Right := FActivePictureSet.LeftMiddle.Width;
        SourceRect.Bottom := FActivePictureSet.LeftMiddle.Height;
        Rect.Top := FActivePictureSet.LeftTop.Height;
        Rect.Left := 0;
        Rect.Right := FActivePictureSet.LeftMiddle.Width;
        Rect.Bottom := (FActivePicture.Bitmap.Height - FActivePictureSet.LeftTop.Height - FActivePictureSet.LeftBottom.Height) + Rect.Top;
        FActivePicture.Bitmap.Canvas.CopyRect(Rect,FActivePictureSet.LeftMiddle.Bitmap.Canvas,SourceRect);
      end;

      //draw right top
      if FActivePictureSet.RightTop.Width > 0 then
      begin
        SourceRect.Top := 0;
        SourceRect.Left := 0;
        SourceRect.Right := FActivePictureSet.RightTop.Width;
        SourceRect.Bottom := FActivePictureSet.RightTop.Height;
        Rect.Top := 0;
        Rect.Left := FActivePicture.Bitmap.Width - FActivePictureSet.RightTop.Width;
        Rect.Right := FActivePicture.Bitmap.Width;
        Rect.Bottom := FActivePictureSet.RightTop.Height;
        FActivePicture.Bitmap.Canvas.CopyRect(Rect,FActivePictureSet.RightTop.Bitmap.Canvas,SourceRect);
      end;
      //draw right bottom
      if FActivePictureSet.RightBottom.Width > 0 then
      begin
        SourceRect.Top := 0;
        SourceRect.Left := 0;
        SourceRect.Right := FActivePictureSet.RightBottom.Width;
        SourceRect.Bottom := FActivePictureSet.RightBottom.Height;
        Rect.Top := Height - FActivePictureSet.RightBottom.Height;
        Rect.Left := FActivePicture.Bitmap.Width - FActivePictureSet.RightBottom.Width;
        Rect.Right := FActivePicture.Bitmap.Width;
        Rect.Bottom := FActivePicture.Bitmap.Height;
        FActivePicture.Bitmap.Canvas.CopyRect(Rect,FActivePictureSet.RightBottom.Bitmap.Canvas,SourceRect);
      end;
      //draw right middle
      if FActivePictureSet.RightMiddle.Width > 0 then
      begin
        SourceRect.Top := 0;
        SourceRect.Left := 0;
        SourceRect.Right := FActivePictureSet.RightMiddle.Width;
        SourceRect.Bottom := FActivePictureSet.RightMiddle.Height;
        Rect.Top := FActivePictureSet.RightTop.Height;
        Rect.Left := FActivePicture.Bitmap.Width - FActivePictureSet.RightMiddle.Width;
        Rect.Right := FActivePicture.Bitmap.Width;
        Rect.Bottom := (FActivePicture.Bitmap.Height - FActivePictureSet.RightTop.Height - FActivePictureSet.RightBottom.Height) + Rect.Top;
        FActivePicture.Bitmap.Canvas.CopyRect(Rect,FActivePictureSet.RightMiddle.Bitmap.Canvas,SourceRect);
      end;

      //draw center top
      if FActivePictureSet.CenterTop.Width > 0 then
      begin
        SourceRect.Top := 0;
        SourceRect.Left := 0;
        SourceRect.Right := FActivePictureSet.CenterTop.Width;
        SourceRect.Bottom := FActivePictureSet.CenterTop.Height;
        Rect.Top := 0;
        Rect.Left := FActivePictureSet.LeftTop.Width;
        Rect.Right := (FActivePicture.Bitmap.Width - FActivePictureSet.LeftTop.Width - FActivePictureSet.RightTop.Width) + Rect.Left;
        Rect.Bottom := FActivePictureSet.CenterTop.Height;
        FActivePicture.Bitmap.Canvas.CopyRect(Rect,FActivePictureSet.CenterTop.Bitmap.Canvas,SourceRect);
      end;
      //draw center bottom
      if FActivePictureSet.CenterBottom.Width > 0 then
      begin
        SourceRect.Top := 0;
        SourceRect.Left := 0;
        SourceRect.Right := FActivePictureSet.CenterBottom.Width;
        SourceRect.Bottom := FActivePictureSet.CenterBottom.Height;
        Rect.Top := FActivePicture.Bitmap.Height - FActivePictureSet.CenterBottom.Height;
        Rect.Left := FActivePictureSet.LeftBottom.Width;
        Rect.Right := (FActivePicture.Bitmap.Width - FActivePictureSet.LeftBottom.Width - FActivePictureSet.RightBottom.Width) + Rect.Left;
        Rect.Bottom := FActivePicture.Bitmap.Height;
        FActivePicture.Bitmap.Canvas.CopyRect(Rect,FActivePictureSet.CenterBottom.Bitmap.Canvas,SourceRect);
      end;
      //draw center middle
      if FActivePictureSet.CenterMiddle.Width > 0 then
      begin
        SourceRect.Top := 0;
        SourceRect.Left := 0;
        SourceRect.Right := FActivePictureSet.CenterMiddle.Width;
        SourceRect.Bottom := FActivePictureSet.CenterMiddle.Height;

        K := FActivePictureSet.LeftTop.Height;
        K := Min(K,FActivePictureSet.CenterTop.Height);
        K := Min(K,FActivePictureSet.RightTop.Height);
        Rect.Top := K;

        K := FActivePictureSet.LeftTop.Width;
        K := Min(K,FActivePictureSet.LeftMiddle.Width);
        K := Min(K,FActivePictureSet.LeftBottom.Width);
        Rect.Left := K;

        K := FActivePictureSet.RightTop.Width;
        K := Min(K,FActivePictureSet.RightMiddle.Width);
        K := Min(K,FActivePictureSet.RightBottom.Width);
        Rect.Right := (FActivePicture.Bitmap.Width - K);

        K := FActivePictureSet.LeftBottom.Height;
        K := Min(K,FActivePictureSet.CenterBottom.Height);
        K := Min(K,FActivePictureSet.RightBottom.Height);
        Rect.Bottom := (FActivePicture.Bitmap.Height - K);
        FActivePicture.Bitmap.Canvas.CopyRect(Rect,FActivePictureSet.CenterMiddle.Bitmap.Canvas,SourceRect);
      end;
    end;
    PictureChanged(Self);
    Result := True;
  end;
end;

procedure TCROCustomSkinImage.Resize;
begin
  inherited;
  if Assigned(FActivePictureSet)
  then FActivePictureSet.DoChange;
end;

procedure TCROCustomSkinImage.SetTransparent(const Value: Boolean);
begin
 	FTransparent := Value;
 	PictureChanged(Self);
end;

{ TCROImageProperties }

constructor TCROImageProperties.Create(AOwner: TComponent);
begin
  inherited;
  FTransparent := True;
  FPictureSet := TCROPictureSplit.Create(Self);
  FPictureSet.OnChange := DoChange;
end;

destructor TCROImageProperties.Destroy;
begin
  FreeAndNil(FPictureSet);
  inherited;
end;

procedure TCROImageProperties.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
  DoChange(Self);
end;

{ TCROSkinImage }

constructor TCROSkinImage.Create(AOwner: TComponent);
begin
  inherited;
  FPicture := TPicture.Create;
  FActivePicture := FPicture;
end;

destructor TCROSkinImage.Destroy;
begin
  FPicture.Free;
  inherited;
end;

procedure TCROSkinImage.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FProperties
    then FProperties := nil;
  end;
end;

procedure TCROSkinImage.SetProperties(const Value: TCROImageProperties);
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

procedure TCROSkinImage.UpdateSkin;
begin
  if Assigned(FProperties) then
  begin
    FActivePictureSet := FProperties.PictureSet;
    FTransparent := FProperties.Transparent;
  end
  else
    FActivePictureSet := nil;
  DoPictureSetChange(Self);
end;

end.
