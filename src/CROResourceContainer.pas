unit CROResourceContainer;

interface

uses
  SysUtils, Classes;

type
  TCROResourceItem = class;
  TCROResourceItems = class;
  TCROResourceContainer = class;

  TGetMessageBodyEvent = procedure (Sender: TObject; const AMessageCode: Integer; AMessageID: String; var AMessageBody: TStrings) of object;

  TCROResourceItem  = class(TCollectionItem)
  private
    FMessageID: String;
    FMessageBody: TStrings;
    FOnGetMessageBody: TGetMessageBodyEvent;
    FMessageCode: Integer;
    procedure SetMessageBody(const Value: TStrings);
    procedure SetMessageID(const Value: String);
    procedure SetMessageCode(const Value: Integer);
  protected
    function GetMessageBody: TStrings; virtual;
    function GetDisplayName: string; override;
  published
    property MessageCode: Integer read FMessageCode write SetMessageCode;
    property MessageID: String read FMessageID write SetMessageID;
    property MessageBody: TStrings read GetMessageBody write SetMessageBody;
    property OnGetMessageBody: TGetMessageBodyEvent read FOnGetMessageBody write FOnGetMessageBody;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  end;

  TCROResourceItems = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TCROResourceItem;
    procedure SetItem(Index: Integer; Value: TCROResourceItem);
  public
    constructor Create(AOwner: TComponent); overload;
    function FindMessage(const AMessageID: String): TCROResourceItem;
    function GetMessageText(const AMessageID: String): String;
    property Items[Index: Integer]: TCROResourceItem read GetItem write SetItem; default;
  end;

  TCROResourceContainer = class(TComponent)
  private
    FItems: TCROResourceItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Items: TCROResourceItems read FItems write FItems;
  end;


implementation

{ TCROResourceItems }

constructor TCROResourceItems.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TCROResourceItem);
end;

function TCROResourceItems.FindMessage(
  const AMessageID: String): TCROResourceItem;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
  begin
    if Items[i].MessageID = AMessageID then
    begin
      Result := Items[i];
      Break;
    end;
  end;
end;

function TCROResourceItems.GetItem(Index: Integer): TCROResourceItem;
begin
  Result := TCROResourceItem(inherited Items[Index]);
end;

function TCROResourceItems.GetMessageText(
  const AMessageID: String): String;
var AItem: TCROResourceItem;
begin
  Result := '';
  AItem := FindMessage(AMessageID);
  if Assigned(AItem)
  then Result := AItem.FMessageBody.Text;
end;

procedure TCROResourceItems.SetItem(Index: Integer;
  Value: TCROResourceItem);
begin
  inherited SetItem(Index, TCollectionItem(Value));
end;

{ TCROResourceContainer }

constructor TCROResourceContainer.Create(AOwner: TComponent);
begin
  inherited;
  Items := TCROResourceItems.Create(Self);
end;

destructor TCROResourceContainer.Destroy;
begin
  Items.Free;
  inherited;
end;

{ TCROResourceItem }

constructor TCROResourceItem.Create(Collection: TCollection);
begin
  inherited;
  FMessageBody := TStringList.Create;
  FMessageID := inherited GetDisplayName + IntToStr(ID);
  FMessageCode := ID;
end;

destructor TCROResourceItem.Destroy;           
begin
  FMessageBody.Free;
  inherited;
end;

function TCROResourceItem.GetDisplayName: string;
begin
  Result := FMessageID;
end;

function TCROResourceItem.GetMessageBody: TStrings;
var ABuffer: TStrings;
begin
  Result := FMessageBody;
  if Assigned(FOnGetMessageBody) then
  begin
    ABuffer := TStringList.Create;
    try
      ABuffer.Assign(FMessageBody);
      FOnGetMessageBody(Self,MessageCode,MessageID,ABuffer);
      Result := ABuffer;
    finally
      ABuffer.Free;
    end;
  end;
end;

procedure TCROResourceItem.SetMessageBody(const Value: TStrings);
begin
  FMessageBody.Assign(Value);
end;

procedure TCROResourceItem.SetMessageCode(const Value: Integer);
var i: Integer;
begin
  if not (csLoading in TComponent(Collection.Owner).ComponentState) then
  begin
    for i := 0 to Collection.Count-1 do
    begin
      if TCROResourceItem(Collection.Items[i]).MessageCode = Value
      then raise Exception.Create('There is another message using this code!')
    end;
    FMessageCode := Value;
  end;  
end;

procedure TCROResourceItem.SetMessageID(const Value: String);
var AItem: TCROResourceItem;
begin
  AItem := TCROResourceItems(Collection).FindMessage(Value);
  if Assigned(AItem) and (AItem <> Self)
  then raise Exception.Create('There is another message using this title!')
  else FMessageID := Value;
end;

end.
