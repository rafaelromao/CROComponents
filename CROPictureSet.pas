unit CROPictureSet;

interface

uses
  SysUtils, Classes, Graphics, Math, Types, Dialogs;

type
  TCROCustomPictureSet = class;
  TCROPictureSet = class;
  TCROPictureSplit = class;

  TCROCustomPictureSet = class(TPersistent)
  private
    FOwner: TComponent;
    FLeftBottom: TPicture;
    FLeftMiddle: TPicture;
    FRightMiddle: TPicture;
    FLeftTop: TPicture;
    FRightBottom: TPicture;
    FCenterMiddle: TPicture;
    FCenterTop: TPicture;
    FCenterBottom: TPicture;
    FRightTop: TPicture;
    FOnChange: TNotifyEvent;
    procedure SetCenterBottom(const Value: TPicture);
    procedure SetCenterMiddle(const Value: TPicture);
    procedure SetCenterTop(const Value: TPicture);
    procedure SetLeftBottom(const Value: TPicture);
    procedure SetLeftMiddle(const Value: TPicture);
    procedure SetLeftTop(const Value: TPicture);
    procedure SetRightBottom(const Value: TPicture);
    procedure SetRightMiddle(const Value: TPicture);
    procedure SetRightTop(const Value: TPicture);
  private
    property Owner: TComponent read FOwner write FOwner;
  protected
    procedure DoChange; virtual;
    property LeftTop: TPicture read FLeftTop write SetLeftTop;
    property LeftMiddle: TPicture read FLeftMiddle write SetLeftMiddle;
    property LeftBottom: TPicture read FLeftBottom write SetLeftBottom;
    property CenterTop: TPicture read FCenterTop write SetCenterTop;
    property CenterMiddle: TPicture read FCenterMiddle write SetCenterMiddle;
    property CenterBottom: TPicture read FCenterBottom write SetCenterBottom;
    property RightTop: TPicture read FRightTop write SetRightTop;
    property RightMiddle: TPicture read FRightMiddle write SetRightMiddle;
    property RightBottom: TPicture read FRightBottom write SetRightBottom;
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
    function Width: Word;
    function Height: Word;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TCROPictureSplit = class(TCROCustomPictureSet)
  private
    FPicture: TPicture;
    FLeftWidth: Word;
    FBottomHeight: Word;
    FTopHeight: Word;
    FRightWidth: Word;
    procedure SetBottomHeight(const Value: Word);
    procedure SetLeftWidth(const Value: Word);
    procedure SetRightWidth(const Value: Word);
    procedure SetTopHeight(const Value: Word);
  public
    procedure SetPicture(const Value: TPicture);
    procedure DoChange; override;
    property LeftTop;
    property LeftMiddle;
    property LeftBottom;
    property CenterTop;
    property CenterMiddle;
    property CenterBottom;
    property RightTop;
    property RightMiddle;
    property RightBottom;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  published
    property Picture: TPicture read FPicture write SetPicture;
    property TopHeight: Word read FTopHeight write SetTopHeight;
    property BottomHeight: Word read FBottomHeight write SetBottomHeight;
    property LeftWidth: Word read FLeftWidth write SetLeftWidth;
    property RightWidth: Word read FRightWidth write SetRightWidth;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TCROPictureSet = class(TCROCustomPictureSet)
  published
    property LeftTop;
    property LeftMiddle;
    property LeftBottom;
    property CenterTop;
    property CenterMiddle;
    property CenterBottom;
    property RightTop;
    property RightMiddle;
    property RightBottom;
  end;

implementation

{ TCROCustomPictureSet }

constructor TCROCustomPictureSet.Create(AOwner: TComponent);
begin
  if Assigned(AOwner)
  then Owner := AOwner
  else raise Exception.Create('Owner cannot be null');
  FLeftBottom := TPicture.Create;
  FLeftMiddle := TPicture.Create;
  FRightMiddle := TPicture.Create;
  FLeftTop := TPicture.Create;
  FRightBottom := TPicture.Create;
  FCenterMiddle := TPicture.Create;
  FCenterTop := TPicture.Create;
  FCenterBottom := TPicture.Create;
  FRightTop := TPicture.Create;
end;

destructor TCROCustomPictureSet.Destroy;
begin
  FreeAndNil(FLeftBottom);
  FreeAndNil(FLeftMiddle);
  FreeAndNil(FRightMiddle);
  FreeAndNil(FLeftTop);
  FreeAndNil(FRightBottom);
  FreeAndNil(FCenterMiddle);
  FreeAndNil(FCenterTop);
  FreeAndNil(FCenterBottom);
  FreeAndNil(FRightTop);
  inherited;
end;

procedure TCROCustomPictureSet.DoChange;
begin
  if Assigned(FOnChange)
  then FOnChange(Self);
end;

function TCROCustomPictureSet.Height: Word;
var K: Word;
begin
  K := FCenterMiddle.Height;
  K := Max(K,FCenterTop.Height);
  K := Max(K,FCenterBottom.Height);
  Result := K;
  K := FLeftBottom.Height;
  K := Max(K,FLeftMiddle.Height);
  K := Max(K,FLeftTop.Height);
  Result := Result + K;
  K := FRightBottom.Height;
  K := Max(K,FRightMiddle.Height);
  K := Max(K,FRightTop.Height);
  Result := Result + K;
end;

function TCROCustomPictureSet.Width: Word;
var K: Word;
begin
  K := FCenterMiddle.Width;
  K := Max(K,FCenterTop.Width);
  K := Max(K,FCenterBottom.Width);
  Result := K;
  K := FLeftBottom.Width;
  K := Max(K,FLeftMiddle.Width);
  K := Max(K,FLeftTop.Width);
  Result := Result + K;
  K := FRightBottom.Width;
  K := Max(K,FRightMiddle.Width);
  K := Max(K,FRightTop.Width);
  Result := Result + K;
end;

procedure TCROCustomPictureSet.SetCenterBottom(const Value: TPicture);
begin
  FCenterBottom.Assign(Value);
  DoChange;
end;

procedure TCROCustomPictureSet.SetCenterMiddle(const Value: TPicture);
begin
  FCenterMiddle.Assign(Value);
  DoChange;
end;

procedure TCROCustomPictureSet.SetCenterTop(const Value: TPicture);
begin
  FCenterTop.Assign(Value);
  DoChange;
end;

procedure TCROCustomPictureSet.SetLeftBottom(const Value: TPicture);
begin
  FLeftBottom.Assign(Value);
  DoChange;
end;

procedure TCROCustomPictureSet.SetLeftMiddle(const Value: TPicture);
begin
  FLeftMiddle.Assign(Value);
  DoChange;
end;

procedure TCROCustomPictureSet.SetLeftTop(const Value: TPicture);
begin
  FLeftTop.Assign(Value);
  DoChange;
end;

procedure TCROCustomPictureSet.SetRightBottom(const Value: TPicture);
begin
  FRightBottom.Assign(Value);
  DoChange;
end;

procedure TCROCustomPictureSet.SetRightMiddle(const Value: TPicture);
begin
  FRightMiddle.Assign(Value);
  DoChange;
end;

procedure TCROCustomPictureSet.SetRightTop(const Value: TPicture);
begin
  FRightTop.Assign(Value);
  DoChange;
end;

{ TCROPictureSplit }

constructor TCROPictureSplit.Create(AOwner: TComponent);
begin
  inherited;
  FPicture := TPicture.Create;
  FLeftWidth := 1;
  FBottomHeight := 1;
  FTopHeight := 1;
  FRightWidth := 1;
end;

destructor TCROPictureSplit.Destroy;
begin
  FreeAndNil(FPicture);
  inherited;
end;

procedure TCROPictureSplit.DoChange;
var SourceRect: TRect; P: TPicture;
          procedure SetPicturePart(Part:TPicture);
          var Rect: TRect;
          begin
            Rect.Left := 0;
            Rect.Top := 0;
            Rect.Right := Max(SourceRect.Right - SourceRect.Left,0);
            Rect.Bottom := Max(SourceRect.Bottom - SourceRect.Top,0);
            Part.Bitmap.Width := Rect.Right;
            Part.Bitmap.Height := Rect.Bottom;
            Part.Bitmap.Canvas.CopyRect(Rect,FPicture.Bitmap.Canvas,SourceRect);
          end;
begin
  if (Assigned(FPicture)) and
     (Assigned(FPicture.Bitmap)) and
     (FPicture.Bitmap.Width > 0)  then
  begin
    //LeftTop
    SourceRect.Left := 0;
    SourceRect.Top := 0;
    SourceRect.Right := FLeftWidth;
    SourceRect.Bottom := FTopHeight;
    SetPicturePart(LeftTop);
    //RightTop
    SourceRect.Left := FPicture.Bitmap.Width - FRightWidth;
    SourceRect.Top := 0;
    SourceRect.Right := FPicture.Bitmap.Width;
    SourceRect.Bottom := FTopHeight;
    SetPicturePart(RightTop);
    //RightBottom
    SourceRect.Left := FPicture.Bitmap.Width - FRightWidth;
    SourceRect.Top := FPicture.Bitmap.Height - FBottomHeight;
    SourceRect.Right := FPicture.Bitmap.Width;
    SourceRect.Bottom := FPicture.Bitmap.Height;
    SetPicturePart(RightBottom);
    //LeftBottom
    SourceRect.Left := 0;
    SourceRect.Top := FPicture.Bitmap.Height - FBottomHeight;
    SourceRect.Right := FLeftWidth;
    SourceRect.Bottom := FPicture.Bitmap.Height;
    SetPicturePart(LeftBottom);
    //LeftMiddle
    SourceRect.Left := 0;
    SourceRect.Top := FTopHeight;
    SourceRect.Right := FLeftWidth;
    SourceRect.Bottom := FPicture.Bitmap.Height - FBottomHeight;
    SetPicturePart(LeftMiddle);
    //RightMiddle
    SourceRect.Left := FPicture.Bitmap.Width - FRightWidth;
    SourceRect.Top := FTopHeight;
    SourceRect.Right := FPicture.Bitmap.Width;
    SourceRect.Bottom := FPicture.Bitmap.Height - FBottomHeight;
    SetPicturePart(RightMiddle);
    //CenterTop
    SourceRect.Left := FLeftWidth;
    SourceRect.Top := 0;
    SourceRect.Right := FPicture.Bitmap.Width - FRightWidth;
    SourceRect.Bottom := FTopHeight;
    SetPicturePart(CenterTop);
    //CenterBottom
    SourceRect.Left := FLeftWidth;
    SourceRect.Top := FPicture.Bitmap.Height - FBottomHeight;
    SourceRect.Right := FPicture.Bitmap.Width - FRightWidth;
    SourceRect.Bottom := FPicture.Bitmap.Height;
    SetPicturePart(CenterBottom);
    //CenterMiddle
    SourceRect.Left := FLeftWidth;
    SourceRect.Top := FTopHeight;
    SourceRect.Right := FPicture.Bitmap.Width - FRightWidth;
    SourceRect.Bottom := FPicture.Bitmap.Height - FBottomHeight;
    SetPicturePart(CenterMiddle);
  end
  else
  begin
    P := TPicture.Create;
    try
      LeftTop.Assign(P);
      RightTop.Assign(P);
      RightBottom.Assign(P);
      LeftBottom.Assign(P);
      LeftMiddle.Assign(P);
      RightMiddle.Assign(P);
      CenterTop.Assign(P);
      CenterBottom.Assign(P);
      CenterMiddle.Assign(P);
    finally
      P.Free;
    end;
  end;
  inherited;
end;

procedure TCROPictureSplit.SetTopHeight(const Value: Word);
begin
  if (FPicture.Bitmap.Height > 0) and ((Value + FBottomHeight) >= FPicture.Bitmap.Height)
  then raise Exception.Create(Format('Heights sum must be inferior to %d!',[FPicture.Bitmap.Height]))
  else
  begin
    FTopHeight := Max(Value,1);
    DoChange;
  end;
end;

procedure TCROPictureSplit.SetBottomHeight(const Value: Word);
begin
  if (FPicture.Bitmap.Height > 0) and ((FTopHeight + Value) >= FPicture.Bitmap.Height)
  then raise Exception.Create(Format('Heights sum must be inferior to %d!',[FPicture.Bitmap.Height]))
  else
  begin
    FBottomHeight := Max(Value,1);
    DoChange;
  end;
end;

procedure TCROPictureSplit.SetLeftWidth(const Value: Word);
begin
  if (FPicture.Bitmap.Width > 0) and ((Value + FRightWidth) >= FPicture.Bitmap.Width)
  then raise Exception.Create(Format('Widths sum must be inferior to %d!',[FPicture.Bitmap.Width]))
  else
  begin
    FLeftWidth := Max(Value,1);
    DoChange;
  end;
end;

procedure TCROPictureSplit.SetRightWidth(const Value: Word);
begin
  if (FPicture.Bitmap.Width > 0) and ((FLeftWidth + Value) >= FPicture.Bitmap.Width)
  then raise Exception.Create(Format('Widths sum must be inferior to %d!',[FPicture.Bitmap.Width]))
  else
  begin
    FRightWidth := Max(Value,1);
    DoChange;
  end;
end;

procedure TCROPictureSplit.SetPicture(const Value: TPicture);
var WrongWidth,WrongHeight: Boolean;
begin
  FPicture.Assign(Value);
  if FPicture.Bitmap.Width > 0 then
  begin
    WrongWidth := ((FLeftWidth + FRightWidth) >= FPicture.Bitmap.Width);
    WrongHeight := ((FTopHeight + FBottomHeight) >= FPicture.Bitmap.Height);
    if WrongWidth or WrongHeight then
    begin
      if (WrongWidth) and (not WrongHeight)
      then MessageDlg(Format('Widths sum must be inferior to %d!',[FPicture.Bitmap.Width]),mtError,[mbOk],0)
      else
      if (not WrongWidth) and (WrongHeight)
      then MessageDlg(Format('Heights sum must be inferior to %d!',[FPicture.Bitmap.Height]),mtError,[mbOk],0)
      else MessageDlg(Format('Heights and widths sum must be inferior to %d and %d!',[FPicture.Bitmap.Height,FPicture.Bitmap.Width]),mtError,[mbOk],0);
    end;

{    if (FLeftWidth = 1) and (FRightWidth = 1) and (FTopHeight = 1) and (FBottomHeight = 1) then //calcula automaticamente ao setar imagem
    begin
      WrongWidth := True;
      WrongHeight := True;
    end;}

    if WrongWidth then
    begin
      FLeftWidth := Max((FPicture.Bitmap.Width div 2) - 1,1);
      FRightWidth := FLeftWidth;
    end;
    if WrongHeight then
    begin
      FTopHeight := Max((FPicture.Bitmap.Height div 2) - 1,1);
      FBottomHeight := FTopHeight;
    end;
  end;
  DoChange;
end;

procedure TCROPictureSplit.AssignTo(Dest: TPersistent);
begin
  if Assigned(Dest) then
  begin
    TCROPictureSplit(Dest).FPicture.Assign(FPicture);
    TCROPictureSplit(Dest).FLeftWidth := FLeftWidth;
    TCROPictureSplit(Dest).FBottomHeight := FBottomHeight;
    TCROPictureSplit(Dest).FTopHeight := FTopHeight;
    TCROPictureSplit(Dest).FRightWidth := FRightWidth;
    TCROPictureSplit(Dest).DoChange;
  end;
end;


end.
 