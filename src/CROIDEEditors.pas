unit CROIDEEditors;

{$D-,L-,S-}

interface

uses
  Classes, DesignIntf, DesignEditors, ColnEdit, uRORemoteService, SysUtils,
  Contnrs, TypInfo;

type
  TCRORadioButtonValuesProperty = class(TStringProperty)
  protected
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TCROServiceNameProperty = class(TStringProperty)
  protected
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TCROOperationNameProperty = class(TStringProperty)
  protected
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TCROGridColumnEditorProperty = class(TStringProperty)
  protected
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TQueryCROOperationParamsProperty = class(TCollectionProperty)
  public
    function GetColOptions: TColOptions; override;
  end;

  TQueryCROGridColumnProperty = class(TCollectionProperty)
  public
    function GetColOptions: TColOptions; override;
  end;

  TQueryCROEnumItemsProperty = class(TCollectionProperty)
  public
    function GetColOptions: TColOptions; override;
  end;

  TQueryCROActionItemsProperty = class(TCollectionProperty)
  public
    function GetColOptions: TColOptions; override;
  end;

  TCROResourceIDProperty = class(TStringProperty)
  protected
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;


implementation

uses
  CROOperation, CROGrid, uROClasses, CRORadioButton, CROActions,
  CROResourceContainer;

{ TCROOperationNameProperty }

function TCROOperationNameProperty.GetAttributes: TPropertyAttributes;
begin
  result := [paValueList]
end;

procedure TCROOperationNameProperty.GetValues(Proc: TGetStrProc);
var i : integer;
    ss : TStrings;
    CROOperation: TPersistent;
begin
  CROOperation := GetComponent(0);
  if Assigned(CROOperation) then
  begin
    ss := TCROCustomOperation(CROOperation).GetOperationNames;
    for i := 0 to (ss.Count-1)
    do Proc(ss[i]);
  end;
end;

{ TQueryCROOperationParamsProperty }

function TQueryCROOperationParamsProperty.GetColOptions: TColOptions;
begin
  Result := [];
end;

{ TQueryCROGridColumnProperty }

function TQueryCROGridColumnProperty.GetColOptions: TColOptions;
begin
  Result := [coAdd, coDelete, coMove];
end;

{ TCROGridColumnEditorProperty }

function TCROGridColumnEditorProperty.GetAttributes: TPropertyAttributes;
begin
  result := [paValueList];
end;

procedure TCROGridColumnEditorProperty.GetValues(Proc: TGetStrProc);
var i : integer;
    CROGridColumn: TPersistent;
begin
  CROGridColumn := GetComponent(0);
  if Assigned(CROGridColumn) then
  begin
    for i := 0 to (TCROGridColumn(CROGridColumn).EditorTypes.Count-1)
    do Proc(TCROGridColumn(CROGridColumn).EditorTypes[i]);
  end;
end;

{ TCROServiceNameProperty }

function TCROServiceNameProperty.GetAttributes: TPropertyAttributes;
begin
  result := [paValueList]
end;

procedure TCROServiceNameProperty.GetValues(Proc: TGetStrProc);
var CROOperation : TCROCustomOperation;
    i : integer;
    ss : IROStrings;
begin
  CROOperation := TCROCustomOperation(GetComponent(0));
  if Assigned(CROOperation.RORemoteService) then
  begin
    ss := CROOperation.RORemoteService.GetServiceNames;
    for i := 0 to (ss.Count-1)
    do Proc(ss[i]);
  end;
end;

{ TCRORadioButtonValuesProperty }

function TCRORadioButtonValuesProperty.GetAttributes: TPropertyAttributes;
begin
  result := [paValueList];
end;

procedure TCRORadioButtonValuesProperty.GetValues(Proc: TGetStrProc);
var CRORadioButton : TCRORadioButton;
    i : integer;
    ss : TStrings;
begin
  CRORadioButton := TCRORadioButton(GetComponent(0));
  if Trim(CRORadioButton.Properties.Expression) <> '' then
  begin
    ss := CRORadioButton.Values;
    for i := 0 to (ss.Count-1)
    do Proc(ss[i]);
  end;
end;

{ TQueryCROEnumItemsProperty }

function TQueryCROEnumItemsProperty.GetColOptions: TColOptions;
begin
  Result := [];
end;

{ TQueryCROActionItemsProperty }

function TQueryCROActionItemsProperty.GetColOptions: TColOptions;
begin
  Result := [coAdd, coDelete, coMove];
end;

{ TCROResourceIDProperty }

function TCROResourceIDProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TCROResourceIDProperty.GetValues(Proc: TGetStrProc);
var
  i: integer;
  Action: TCROCustomAction;
  Container: TCROResourceContainer;
begin
  if Assigned(GetComponent(0)) and (GetComponent(0).InheritsFrom(TCROCustomAction)) then
  begin
    Action := TCROCustomAction(GetComponent(0));
    Container := Action.ResourceContainer;
    if Assigned(Container) then
    begin
      for i := 0 to (Container.Items.Count-1)
      do Proc(Container.Items[i].MessageID);
    end;  
  end;
end;

end.
