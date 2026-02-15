unit CROSkinProperties;

interface

uses
  SysUtils, Classes, Contnrs, Controls, Graphics, CROComponentsCommon, Grids;

type
  TCROSkinProperties = class(TComponent)
  private
    FControls: TObjectList;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure DoChange(Sender: TObject); 
    function AddControl(AControl: TControl):Integer;
    function RemoveControl(AControl: TControl):Boolean;
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
  end;

  TCROFontStyleProperties = class(TCROSkinProperties)
  private
    FFont: TFont;
    FEnterToTab: Boolean;
    procedure SetFont(const Value: TFont);
  published
    property Font: TFont read FFont write SetFont;
    property EnterToTab: Boolean read FEnterToTab write FEnterToTab default True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TCROGridStyleProperties = class(TCROSkinProperties)
  private
    FBorderColorRight: TColor;
    FEvenColor: TColor;
    FFocusedColor: TColor;
    FBorderColorTop: TColor;
    FBorderColorBottom: TColor;
    FSelectedColor: TColor;
    FBorderColorLeft: TColor;
    FSelectedFont: TFont;
    FFixedFont: TFont;
    FFont: TFont;
    FFixedColor: TColor;
    FOptions: TGridOptions;
    FFocusedFont: TFont;
    FColor: TColor;
    FFixedRowHeight: Word;
    FDefaultRowHeight: Word;
    procedure SetEvenColor(const Value: TColor);
    procedure SetBorderColorBottom(const Value: TColor);
    procedure SetBorderColorLeft(const Value: TColor);
    procedure SetBorderColorRight(const Value: TColor);
    procedure SetBorderColorTop(const Value: TColor);
    procedure SetFixedFont(const Value: TFont);
    procedure SetFocusedColor(const Value: TColor);
    procedure SetSelectedColor(const Value: TColor);
    procedure SetSelectedFont(const Value: TFont);
    procedure SetFont(const Value: TFont);
    procedure SetFixedColor(const Value: TColor);
    procedure SetOptions(const Value: TGridOptions);
    procedure SetFocusedFont(const Value: TFont);
    procedure SetColor(const Value: TColor);
    procedure SetDefaultRowHeight(const Value: Word);
    procedure SetFixedRowHeight(const Value: Word);
  published
    property FixedRowHeight: Word read FFixedRowHeight write SetFixedRowHeight default 17;
    property DefaultRowHeight: Word read FDefaultRowHeight write SetDefaultRowHeight default 17;
    property Color: TColor read FColor write SetColor;
    property Font: TFont read FFont write SetFont;
    property FixedFont: TFont read FFixedFont write SetFixedFont;
    property FixedColor: TColor read FFixedColor write SetFixedColor;
    property BorderColorTop: TColor read FBorderColorTop write SetBorderColorTop;
    property BorderColorLeft: TColor read FBorderColorLeft write SetBorderColorLeft;
    property BorderColorRight: TColor read FBorderColorRight write SetBorderColorRight;
    property BorderColorBottom: TColor read FBorderColorBottom write SetBorderColorBottom;
    property FocusedColor: TColor read FFocusedColor write SetFocusedColor;
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor;
    property EvenColor: TColor read FEvenColor write SetEvenColor;
    property SelectedFont: TFont read FSelectedFont write SetSelectedFont;
    property FocusedFont: TFont read FFocusedFont write SetFocusedFont;
    property Options: TGridOptions read FOptions write SetOptions;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

const
  clBorderColorTop: Integer = clWhite;
  clBorderColorLeft: Integer = clWhite;
  clBorderColorRight: Integer = clGray;
  clBorderColorBottom: Integer = clGray;//$0099A8AC;
  clFocusedColor: Integer = $00C8D0D4;//clBtnFace;
  clSelectedColor: Integer = clBtnFace;
  clEvenColor: Integer = clWindow;
  clFixedColor: Integer = clBtnFace;
  clColor: Integer = clWindow;

implementation

{ TCROSkinProperties }

function TCROSkinProperties.AddControl(AControl: TControl): Integer;
begin
  Result := FControls.IndexOf(AControl);
  if Assigned(AControl) then
  begin
    if (Result = -1) then
    begin
      Result := FControls.Add(AControl);
      AControl.FreeNotification(Self);
    end;
  end;
end;

constructor TCROSkinProperties.Create(AOwner: TComponent);
begin
  inherited;
  FControls := TObjectList.Create;
  FControls.OwnsObjects := False;
end;

destructor TCROSkinProperties.Destroy;
begin
  FreeAndNil(FControls);
  inherited;
end;

procedure TCROSkinProperties.DoChange(Sender: TObject);
var i: Word;
begin
  if Assigned(FControls) then
  begin
    if FControls.Count > 0 then
    begin
      for i := 0 to FControls.Count-1 do
      begin
        try
          (TComponent(FControls[i]) as ICROSkinControl).UpdateSkin;
        except end;
      end;
    end;
  end;
end;

procedure TCROSkinProperties.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if Assigned(FControls) then
    begin
      if FControls.IndexOf(AComponent) > -1
      then FControls.Extract(AComponent);
    end;
  end;
end;

function TCROSkinProperties.RemoveControl(AControl: TControl): Boolean;
begin
  Result := FControls.IndexOf(AControl) > -1;
  if Result
  then FControls.Extract(AControl);
end;

{ TCROFontStyleProperties }

constructor TCROFontStyleProperties.Create(AOwner: TComponent);
begin
  inherited;
  FFont := TFont.Create;
  FFont.OnChange := DoChange;
  FEnterToTab := True;
end;

destructor TCROFontStyleProperties.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TCROFontStyleProperties.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  DoChange(Self);
end;

{ TCROGridStyleProperties }

constructor TCROGridStyleProperties.Create(AOwner: TComponent);
begin
  inherited;
  FSelectedFont := TFont.Create;
  FFixedFont := TFont.Create;
  FFont := TFont.Create;
  FFocusedFont := TFont.Create;

  FColor := clColor;
  FBorderColorTop := clBorderColorTop;
  FBorderColorLeft := clBorderColorLeft;
  FBorderColorRight := clBorderColorRight;
  FBorderColorBottom := clBorderColorBottom;
  FFocusedColor := clFocusedColor;
  FSelectedColor := clSelectedColor;
  FEvenColor := clEvenColor;
  FSelectedFont := TFont.Create;
  FFixedColor := clFixedColor;
  FFixedRowHeight := 17;
  FDefaultRowHeight := 17;

  FOptions := [goFixedVertLine,goFixedHorzLine,goVertLine,goHorzLine];
end;

destructor TCROGridStyleProperties.Destroy;
begin
  FSelectedFont.Free;
  FFixedFont.Free;
  FFont.Free;
  FFocusedFont.Free;
  inherited;
end;

procedure TCROGridStyleProperties.SetEvenColor(const Value: TColor);
begin
  FEvenColor := Value;
  DoChange(Self);
end;

procedure TCROGridStyleProperties.SetBorderColorBottom(const Value: TColor);
begin
  FBorderColorBottom := Value;
  DoChange(Self);
end;

procedure TCROGridStyleProperties.SetBorderColorLeft(const Value: TColor);
begin
  FBorderColorLeft := Value;
  DoChange(Self);
end;

procedure TCROGridStyleProperties.SetBorderColorRight(const Value: TColor);
begin
  FBorderColorRight := Value;
  DoChange(Self);
end;

procedure TCROGridStyleProperties.SetBorderColorTop(const Value: TColor);
begin
  FBorderColorTop := Value;
  DoChange(Self);
end;

procedure TCROGridStyleProperties.SetFixedFont(const Value: TFont);
begin
  FFixedFont.Assign(Value);
  DoChange(Self);
end;

procedure TCROGridStyleProperties.SetFocusedColor(const Value: TColor);
begin
  FFocusedColor := Value;
  DoChange(Self);
end;

procedure TCROGridStyleProperties.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  DoChange(Self);
end;

procedure TCROGridStyleProperties.SetSelectedColor(const Value: TColor);
begin
  FSelectedColor := Value;
  DoChange(Self);
end;

procedure TCROGridStyleProperties.SetSelectedFont(const Value: TFont);
begin
  FSelectedFont.Assign(Value);
  DoChange(Self);
end;

procedure TCROGridStyleProperties.SetFixedColor(const Value: TColor);
begin
  FFixedColor := Value;
  DoChange(Self);
end;

procedure TCROGridStyleProperties.SetOptions(const Value: TGridOptions);
begin
  FOptions := Value;
  DoChange(Self);
end;

procedure TCROGridStyleProperties.SetFocusedFont(const Value: TFont);
begin
  FFocusedFont.Assign(Value);
  DoChange(Self);
end;

procedure TCROGridStyleProperties.SetColor(const Value: TColor);
begin
  FColor := Value;
  DoChange(Self);
end;

procedure TCROGridStyleProperties.SetDefaultRowHeight(const Value: Word);
begin
  FDefaultRowHeight := Value;
  DoChange(Self);
end;

procedure TCROGridStyleProperties.SetFixedRowHeight(const Value: Word);
begin
  FFixedRowHeight := Value;
  DoChange(Self);
end;

end.
