unit CROSkinControl;

interface

uses
  SysUtils, Classes, Controls, Windows, Graphics, CROPictureSet, Types,
  CROComponentsCommon, TypInfo, Math, CROSkinImage, CROSkinProperties;

type
  TCROCustomSkinControl = class(TCustomControl)//Windowed
  private
    FRegion: HRgn;
    FDrawing: Boolean;
    FTransparentColor: TColor;
    procedure SetTransparentColor(const Value: TColor);
    function GetCanvas: TCanvas;
  protected
    FActivePicture: TPicture;
    FActivePictureSet: TCROPictureSplit;
    procedure PictureChanged(Sender: TObject); virtual;
    function DestRect: TRect;
    function GetPalette: HPALETTE; override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure Resize; override;
    procedure Paint; override;
    procedure DoPictureSetChange(Sender: TObject); virtual;
    function PaintTransparentBorder:Boolean;
    function RecreatePicture:Boolean;

    procedure ApplyRegions;

    property TransparentColor: TColor read FTransparentColor write SetTransparentColor;
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
  end;

implementation

{ TCROCustomSkinControl }

function TCROCustomSkinControl.CanAutoSize(var NewWidth,
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

constructor TCROCustomSkinControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FActivePictureSet := nil;
  Height := 40;
  Width := 40;
  FTransparentColor := clRed;
  DoubleBuffered := True;
end;

destructor TCROCustomSkinControl.Destroy;
begin
  if FRegion <> 0
  then DeleteObject(FRegion);
  inherited Destroy;
end;

procedure TCROCustomSkinControl.Paint;
var Save: Boolean;
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
end;

procedure TCROCustomSkinControl.SetTransparentColor(const Value: TColor);
begin
 	FTransparentColor := Value;
 	PictureChanged(Self);
end;

procedure TCROCustomSkinControl.PictureChanged(Sender: TObject);
begin
  if not (Assigned(FActivePicture))
  then Exit;

  if AutoSize and (FActivePicture.Width > 0) and (FActivePicture.Height > 0)
  then SetBounds(Left, Top, FActivePicture.Width, FActivePicture.Height);

  if not FDrawing
  then Invalidate;
end;

function TCROCustomSkinControl.GetPalette: HPALETTE;
begin
  Result := 0;
  if FActivePicture.Graphic <> nil
  then Result := FActivePicture.Graphic.Palette;
end;

procedure TCROCustomSkinControl.DoPictureSetChange(Sender: TObject);
begin
  if RecreatePicture
  then PaintTransparentBorder;
end;

function TCROCustomSkinControl.DestRect: TRect;
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

function TCROCustomSkinControl.GetCanvas: TCanvas;
begin
  Result := inherited Canvas;
end;

function TCROCustomSkinControl.PaintTransparentBorder:Boolean;
begin
  Result := True;

  if (TransparentColor = clNone)
  then Exit;

  if (csDesigning in ComponentState)
  then Exit;

  Result := False;

  if not Assigned(FActivePicture)
  then Exit;

  ApplyRegions;
  Result := True;
end;

function TCROCustomSkinControl.RecreatePicture: Boolean;
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
        Rect.Top := Height - FActivePictureSet.LeftBottom.Height;
        Rect.Left := 0;
        Rect.Right := FActivePictureSet.LeftBottom.Width;
        Rect.Bottom := Height;
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
        Rect.Bottom := (Height - FActivePictureSet.LeftTop.Height - FActivePictureSet.LeftBottom.Height) + Rect.Top;
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
        Rect.Left := Width - FActivePictureSet.RightTop.Width;
        Rect.Right := Width;
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
        Rect.Left := Width - FActivePictureSet.RightBottom.Width;
        Rect.Right := Width;
        Rect.Bottom := Height;
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
        Rect.Left := Width - FActivePictureSet.RightMiddle.Width;
        Rect.Right := Width;
        Rect.Bottom := (Height - FActivePictureSet.RightTop.Height - FActivePictureSet.RightBottom.Height) + Rect.Top;
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
        Rect.Right := (Width - FActivePictureSet.LeftTop.Width - FActivePictureSet.RightTop.Width) + Rect.Left;
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
        Rect.Top := Height - FActivePictureSet.CenterBottom.Height;
        Rect.Left := FActivePictureSet.LeftBottom.Width;
        Rect.Right := (Width - FActivePictureSet.LeftBottom.Width - FActivePictureSet.RightBottom.Width) + Rect.Left;
        Rect.Bottom := Height;
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
        Rect.Right := (Width - K);

        K := FActivePictureSet.LeftBottom.Height;
        K := Min(K,FActivePictureSet.CenterBottom.Height);
        K := Min(K,FActivePictureSet.RightBottom.Height);
        Rect.Bottom := (Height - K);
        FActivePicture.Bitmap.Canvas.CopyRect(Rect,FActivePictureSet.CenterMiddle.Bitmap.Canvas,SourceRect);
      end;
    end;
    PictureChanged(Self);
    Result := True;
  end;
end;

procedure TCROCustomSkinControl.Resize;
begin
  inherited;
  if Assigned(FActivePictureSet)
  then FActivePictureSet.DoChange;
end;


procedure TCROCustomSkinControl.ApplyRegions;
begin
  if not Assigned(FActivePicture)
  then Exit;

  if FRegion <> 0
  then DeleteObject(FRegion);
  FRegion := BitmapToRegion(FActivePicture.Bitmap,FTransparentColor,0);
  SetWindowRgn(Handle, FRegion, True);
end;

end.
