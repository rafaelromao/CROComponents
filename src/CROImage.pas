unit CROImage;

interface

uses
  SysUtils, Classes, Controls, ExtCtrls, CROOperation, uROTypes, jpeg, Dialogs,
  Graphics, CROComponentsCommon;

type
  TCROImage = class(TImage, ICROCustomControl, ICROSkinControl)
  private
    { Private declarations }
    FProperties: TCROProperties;
    FPicture: TPicture;
    FLoadingImage: Boolean;

    procedure OnChangePicture(Sender: TObject);
    procedure SetProperties(Value: TCROProperties);
    function GetProperties: TCROProperties;
    procedure SetPicture(const Value: TPicture);
  protected
    { Protected declarations }
    procedure Loaded; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure UpdateValue;
    procedure UpdateObject;
    procedure UpdateSkin;
  published
    { Published declarations }
    property Picture: TPicture read FPicture write SetPicture;
    property Properties: TCROProperties read GetProperties write SetProperties;
  end;

implementation


{ TCROImage }

constructor TCROImage.Create(AOwner: TComponent);
begin
  inherited;
  FLoadingImage := False;
  FPicture := TPicture.Create;
  FPicture.OnChange := OnChangePicture;
  FProperties := TCROProperties.Create(Self);
end;

destructor TCROImage.Destroy;
begin
  FProperties.Free;
  FPicture.Free;

  inherited;
end;

function TCROImage.GetProperties: TCROProperties;
begin
  Result := FProperties;
end;

procedure TCROImage.Loaded;
begin
  inherited;
  UpdateValue;
end;

procedure TCROImage.OnChangePicture(Sender: TObject);
begin
  inherited Picture.Assign(FPicture);

  if not FLoadingImage then
  begin
    if Assigned(FProperties.CursorHandle) and (FProperties.CursorHandle.AutoEdit)
    then FProperties.CursorHandle.Edit;
    UpdateObject;
  end;
end;

procedure TCROImage.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
  inherited Picture.Assign(Value);
end;

procedure TCROImage.SetProperties(Value: TCROProperties);
begin
  if Value <> nil
  then FProperties := Value;
end;

procedure TCROImage.UpdateObject;
var ImageStream: TROBinaryMemoryStream;
    obj: TObject;
    jpg : TJpegImage;
begin
  if Assigned(FProperties.CursorHandle) and
     Assigned(FProperties.CursorHandle.Data) then
  begin
    FProperties.GetValueFromExpression(obj);
    ImageStream := TROBinaryMemoryStream(obj);
    if not Assigned(ImageStream)
    then ImageStream := TROBinaryMemoryStream.create;

    ImageStream.Clear;

    jpg := TJpegImage.create;
    jpg.Assign(Self.Picture);
    jpg.SaveToStream(ImageStream);

    ImageStream.position := 0;
    FProperties.SetValueToExpression(ImageStream);

    jpg.Free;
  end;
end;

procedure TCROImage.UpdateSkin;
begin
  if Assigned(FProperties.FontProperties) and Assigned(Font)
  then Font.Assign(FProperties.FontProperties.Font);
end;

procedure TCROImage.UpdateValue;
var obj: TObject;
    ImageStream: TROBinaryMemoryStream;
    jpg : TJpegImage;
begin
  if (Assigned(FProperties.CursorHandle)) and
     (Assigned(FProperties.CursorHandle.Data)) then
  begin
    jpg := TJpegImage.create;
    try
      FLoadingImage := True;
      FProperties.GetValueFromExpression(obj);

      if Assigned(obj) then
      begin
        ImageStream := TROBinaryMemoryStream(obj);

        ImageStream.Position := 0;
        jpg.LoadFromStream(ImageStream);
        FPicture.Assign(jpg);
      end
      else
      begin
        FPicture.Bitmap.FreeImage;
      end;
    finally
      FLoadingImage := False;
      jpg.Free;
    end;
  end;
end;

end.
