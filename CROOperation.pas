unit CROOperation;

interface

uses
  SysUtils, Classes, uRORemoteService, uRODL, DB, uROProxy, uROTypes,
  Variants, TypInfo, CROComponentsCommon, ContNrs, uROWinInetHttpChannel,
  uROBINMessage, uROClient, CROSkinProperties, Controls, Math,
  Dialogs;

type
  TCROHandle = class;
  TCROListHandle   = class;
  TCROCursorHandle = class;
  TCROCustomProperties = class;
  TCROOperationParams = class;
  TCROOperationParam  = class;
  TCROCustomOperation = class;
  TCROOperation = class;
  TCROServiceOperation = class;
  TCROProperties = class;

  TCROOperationAction = (oaSelect,oaInsert,oaUpdate,oaDelete,oaCreate);

  ICROEnumControl = interface
  ['{261C72BD-3129-41E2-BE31-A20B1DC12341}']
    function GetProperties: TCROProperties;
  end;

  TCROServiceHandle = class(TComponent)
  end;

  TCROServiceManager = class(TCROServiceHandle)
  private
    FOperationForSelect: TCROCustomOperation;
    FOperationForInsert: TCROCustomOperation;
    FOperationForUpdate: TCROCustomOperation;
    FOperationForDelete: TCROCustomOperation;
    FOperationForCreate: TCROCustomOperation;
  published
    property OperationForSelect: TCROCustomOperation read FOperationForSelect;
    property OperationForInsert: TCROCustomOperation read FOperationForInsert;
    property OperationForDelete: TCROCustomOperation read FOperationForDelete;
    property OperationForUpdate: TCROCustomOperation read FOperationForUpdate;
    property OperationForCreate: TCROCustomOperation read FOperationForCreate;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TCROCustomOperation = class(TCROServiceHandle)
  private
    { Private declarations }
    fOperationName: String;
    fParamUpdated: Boolean;
    FAttached: Boolean;
    FOnConnectionSuccess: TNotifyEvent;
    FOnConnectionFailure: TNotifyEvent;
    FConnected: Boolean;
    FAction: TCROOperationAction;

    procedure SetOperationName(Value: String);
    procedure SetParams(Value: TCROOperationParams);
  protected
    { Protected declarations }
    fRORemoteService: TRORemoteService;
    fRODLLibrary: TRODLLibrary;
    fServiceGUID: TGUID;
    fRODLService: TRODLService;
    fRODLOperation: TRODLOperation;
    fROProxy      : TROProxy;
    fOperationNames: TStrings;
    fParams: TCROOperationParams;
    procedure SetRORemoteService(Value: TRORemoteService);
    procedure Loaded; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    function GetOperationNames: TStrings;
    function Execute: Boolean;
    property RORemoteService: TRORemoteService read fRORemoteService write SetRORemoteService;
    property OperationName: String read fOperationName write SetOperationName;
    property Params: TCROOperationParams read fParams write SetParams;
    property Attached: Boolean read FAttached;
    property Connected: Boolean read FConnected;
    property OnConnectionFailure: TNotifyEvent read FOnConnectionFailure write FOnConnectionFailure;
    property OnConnectionSuccess: TNotifyEvent read FOnConnectionSuccess write FOnConnectionSuccess;
    property RODLLibrary: TRODLLibrary read FRODLLibrary;
    property Action: TCROOperationAction read FAction write FAction; 
  end;

  TCROOperation = class(TCROCustomOperation)
  published
    property RORemoteService;
    property OperationName;
    property Params;
    property OnConnectionFailure;
    property OnConnectionSuccess;
    property Action;
  end;

  TCROServiceOperation = class(TCROCustomOperation)
  private
    function GetServiceName: String;
    procedure SetServiceName(const Value: String);
  protected
    FROChannel: TROTransportChannel;
    FROMessage: TROMessage;       
  published
    property ROMessage:TROMessage read FROMessage;
    property ROChannel:TROTransportChannel read FROChannel;
    property RORemoteService: TRORemoteService read FRORemoteService;
    property ServiceName: String read GetServiceName write SetServiceName;
    property OperationName;
    property Params;
    property OnConnectionFailure;
    property OnConnectionSuccess;
    property Action;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TCROOperationParam = class(TCollectionItem)
  private
    FName: String;
    FDataType: TRODataType;
    FParamType: TRODLParamFlag;
    FParamClass: String;
    FData: Variant;
    FDataAsObject: TObject;
    FExpression: String;
    FExpressionHandle: TCROHandle;

    function GetValue: Variant;
    procedure SetValue(Value: Variant);
    function GetValueAsObject: TObject;
    procedure SetValueAsObject(Value: TObject);
    procedure SetParamClass(Value: String);
    procedure SetDataType(Value: TRODataType);
    procedure SetParamType(Value: TRODLParamFlag);
    procedure SetName(Value: String);
    procedure SetExpression(Value: String);
  protected
    procedure AssignParam(Param: TCROOperationParam);
  public
    constructor Create(Collection: TCollection); overload; override;
    constructor Create(Collection: TCollection; AName: String; ADataType:TRODataType;
      AParamType:TRODLParamFlag; AParamClass: String); reintroduce; overload;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    procedure FixParamExpression;
  published
    property DataType: TRODataType read FDataType write SetDataType;
    property ParamClass: String read FParamClass write SetParamClass;
    property Name: string read FName write SetName;
    property ParamType: TRODLParamFlag read fParamType write SetParamType;
    property Value: Variant read GetValue write SetValue;
    property ValueAsObject: TObject read GetValueAsObject write SetValueAsObject;
    property Expression: String read FExpression write SetExpression;
  end;

{ TParams }

  TCROOperationParams = class(TCollection)
  private
    FOwner: TPersistent;
    function GetItem(Index: Integer): TCROOperationParam;
    procedure SetItem(Index: Integer; Value: TCROOperationParam);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetOwner: TPersistent; override;
  public
    constructor Create(Owner: TCROCustomOperation); overload;
    procedure AssignValues(Value: TCROOperationParams);
    { Create, AddParam, RemoveParam and CreateParam are in for backward compatibility }
    constructor Create; overload;
    procedure AddParam(Value: TCROOperationParam);
    procedure RemoveParam(Value: TCROOperationParam);
    function CreateParam(DataType: TRODataType; const ParamName: string;
      ParamType: TRODLParamFlag; ParamClass: String; Operation: TCROCustomOperation): TCROOperationParam;
    procedure GetParamList(List: TList; const ParamNames: string);
    function ParamByName(const Value: string): TCROOperationParam;
    function FindParam(const Value: string): TCROOperationParam;
    procedure FixParamExpressions;
    property Items[Index: Integer]: TCROOperationParam read GetItem write SetItem; default;
  end;

  TParamInfo = record
    OperationParam: TCROOperationParam;
    Instance: TObject;
    PropInfo: PPropInfo;
  end;
  PParamInfo = ^TParamInfo;

  TParamInfoArray = Array of PParamInfo;


  //---

  TCursorState = (csBrowse, csInsert, csEdit, csCancel);

  TStateChangeEvent = procedure (Sender:TObject; const AState:TCursorState) of object;

  TCROHandle = class(TComponent)
  private
    { Private declarations }
    FHandleUpdateTime: TDateTime;
    FCanceling: Boolean;
    FState: TCursorState;
    FIsLinkedToParam: Boolean;
    FOperation: TCROCustomOperation;
    FHandleList: TList;
    FLinkedHandle: TCROHandle;
    FExpression: String;
    FHandleDataType: String;
    FServiceManager: TCROServiceManager;
    FOnStateChange: TStateChangeEvent;
    FAutoEdit: Boolean;
    FDisableUpdate: Boolean;
    FIdentifierExpression: String;
    procedure SetServiceManager(const Value: TCROServiceManager);
  protected
    { Protected declarations }
    FData: TROComplexType;
    procedure SetDisableUpdate(const Value: Boolean); virtual;
    procedure SetAutoEdit(const Value: Boolean); virtual; abstract;
    procedure SetState(const Value: TCursorState); virtual; abstract;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetData(Value: TROComplexType); virtual;
    procedure UpdateDataType; virtual; abstract;
    procedure InsertProperties(AProperties: TCROCustomProperties);
    procedure DeleteProperties(AProperties: TCROCustomProperties);
    procedure SetExpression(Value: String);
    procedure SetOperation(Value: TCROCustomOperation);
    procedure EvaluateExpression; virtual;
    procedure Loaded; override;
    procedure SetDataType(const Value: String); virtual;
  public
    { Public declarations }
    FListProperties: TList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Post: Boolean; virtual;
    procedure UpdateProperties; virtual;
    procedure EvaluateHandleList; virtual;

    function Insert: Boolean; virtual; abstract;
    function Delete: Boolean; virtual; abstract;
    procedure Edit; virtual; abstract;
    procedure Cancel; virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure FixExpression; virtual;

    property DataType: String read FHandleDataType write SetDataType;
    property SourceOperation: TCROCustomOperation read FOperation write SetOperation;
    property State: TCursorState read FState;
    property DisableUpdate: Boolean read FDisableUpdate write SetDisableUpdate;
  published
    { Published declarations }
    property Data: TROComplexType read FData write SetData;
    property Expression: String read FExpression write SetExpression;
    property ServiceManager: TCROServiceManager read FServiceManager write SetServiceManager;
    property OnStateChange: TStateChangeEvent read FOnStateChange write FOnStateChange;
    property AutoEdit: Boolean read FAutoEdit write SetAutoEdit default False;
    property IdentifierExpression: String read FIdentifierExpression write FIdentifierExpression;
  end;

  TCROCursorHandle = class(TCROHandle)
  private
    { Private declarations }
    FBuffer: TROComplexType;
    FListHandle: TCROListHandle;
    FGeneratingIdentifier: Boolean;
    procedure SetListHandle(Value: TCROListHandle);
  protected
    { Protected declarations }
    procedure SetAutoEdit(const Value: Boolean); override;
    procedure SetState(const Value: TCursorState); override;
    procedure SetData(Value: TROComplexType); override;
    procedure UpdateDataType; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure FixExpression; override;
    procedure UpdateProperties; override;
    function Insert: Boolean; override;
    function Delete: Boolean; override;
    procedure Edit; override;
    procedure Cancel; override;
    procedure Clear; override;
    procedure SetDataProperty(Expression: String; Value: Variant); overload;
    procedure SetDataProperty(Expression: String; Value: TObject); overload;
    function GetDataProperty(Expression: String): Variant;
    function GetDataPropertyAsObject(Expression: String): TObject;
  published
    { Published declarations }
    property ListHandle: TCROListHandle read FListHandle write SetListHandle;
  end;

  TCROListHandle = class(TCROHandle)
  private
    FItems: TList;
    FIndexMap: TStringList;
    FItemIndex: Integer;
    FInserting: Boolean;
    FCursorHandleList: TObjectList;
    FFiltered: Boolean;
    FKeepCursorPosition: Boolean;
    procedure SetItemIndex(const Value: Integer);
    function GetItem(Index: Integer): TROComplexType;
    procedure SetItem(Index: Integer; const Value: TROComplexType);
    function GetDataIndex(const AIndex: Integer):Integer;
    { Private declarations }
  protected
    { Protected declarations }
    procedure EvaluateExpression; override;
    procedure SetAutoEdit(const Value: Boolean); override;
    procedure SetState(const Value: TCursorState); override;
    procedure SetDataType(const Value: String); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetData(Value: TROComplexType); override;
    procedure AddCursorHandle(ACursorHandle: TCROCursorHandle);
    procedure UpdateDataType; override;
  public
    { Public declarations }
    InternalCursorHandle: TCROCursorHandle;
    BOF: Boolean;
    EOF: Boolean;
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    procedure EvaluateHandleList; override;
    procedure NotifyIndexUpdate;
    procedure UpdateCursorHandle;
    procedure Refresh;
    function Next: Boolean;
    function Prior: Boolean;
    function Insert: Boolean; override;
    function Delete: Boolean; override;
    function Count: Integer;
    procedure Clear; override;
    procedure Cancel; override;
    procedure Edit; override;
    function Post: Boolean; override;
    procedure AddObject(Obj: TROComplexType);
    function CopyObject(const Index:Integer; Dest: TCROListHandle): Boolean;
    function MoveObject(const Index:Integer; Dest: TCROListHandle): Boolean;
    function MoveUp(const Index: Integer): Boolean;
    function MoveDown(const Index: Integer): Boolean;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property Items[Index: Integer]: TROComplexType read GetItem write SetItem;
  published
    { Published declarations }
    property Filtered: Boolean read FFiltered write FFiltered default False;
    property KeepCursorPosition: Boolean read FKeepCursorPosition write FKeepCursorPosition default False;
  end;

  TCROCustomProperties = class(TPersistent)
  private
    { Private declarations }
    FPropertiesUpdateTime: TDateTime;
    FExpression: String;
    FOwner: TComponent;
    FOnInternalUpdateExpression: TNotifyEvent;
    FOnInternalUpdateHandle: TNotifyEvent;
    FPropertiesDataType: String;
    FFontProperties: TCROFontStyleProperties;
    FGridProperties: TCROGridStyleProperties;
    procedure SetListHandle(Value: TCROListHandle);
    procedure SetCursorHandle(Value: TCROCursorHandle);
    procedure SetExpression(Value: String);
    procedure SetOnInternalUpdateExpression(const Value: TNotifyEvent);
    procedure SetOnInternalUpdateHandle(const Value: TNotifyEvent);
    function GetListHandle: TCROListHandle;
    function GetCursorHandle: TCROCursorHandle;
    procedure SetFontProperties(const Value: TCROFontStyleProperties);
    procedure SetGridProperties(const Value: TCROGridStyleProperties);
  protected
    { Protected declarations }
    FHandle: TCROHandle;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
    function HandleAssigned: Boolean;
    procedure FixDataType;
    procedure UpdateValue; virtual; abstract;
    procedure GetValueFromExpression(var AResult: Variant); overload;
    procedure GetValueFromExpression(var AResult: TObject); overload;
    procedure SetValueToExpression(const Value: Variant); overload;
    procedure SetValueToExpression(Value: TObject; ClassName: String = ''); overload;
    property OnInternalUpdateExpression: TNotifyEvent read FOnInternalUpdateExpression write SetOnInternalUpdateExpression;
    property OnInternalUpdateHandle: TNotifyEvent read FOnInternalUpdateHandle write SetOnInternalUpdateHandle;
    property DataType: String read FPropertiesDataType;
    property FontProperties: TCROFontStyleProperties read FFontProperties write SetFontProperties;
    property GridProperties: TCROGridStyleProperties read FGridProperties write SetGridProperties;
    //
    property ListHandle: TCROListHandle read GetListHandle write SetListHandle;
    property CursorHandle: TCROCursorHandle read GetCursorHandle write SetCursorHandle;
    property Expression: String read FExpression write SetExpression;
  published
    { Published declarations }
  end;

  TCROProperties = class(TCROCustomProperties)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure UpdateValue; override;
  published
    { Published declarations }
    property CursorHandle;
    property Expression;
    property FontProperties;
  end;

  TCROLookupProperties = class(TCROCustomProperties)
  private
    { Private declarations }
    FIdentifierExpression: String;
    procedure SetIdentifierExpression(Value: String);
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure UpdateValue; override;
  published
    { Published declarations }
    property ListHandle;
    property Expression;
    property IdentifierExpression: String read FIdentifierExpression write SetIdentifierExpression;
    property FontProperties;
  end;

  TCROIndexedProperties = class(TCROCustomProperties)
  protected
    procedure NotifyIndexUpdate; virtual; abstract;
  end;

  TCROGridProperties = class(TCROIndexedProperties)
  public
    { Public declarations }
    procedure NotifyIndexUpdate; override;
    procedure UpdateValue; override;
  published
    { Published declarations }
    property ListHandle;
    property GridProperties;
  end;

  TCROActionProperties = class(TCROIndexedProperties)
  protected
    property Handle: TCROHandle read FHandle write FHandle;
    procedure NotifyIndexUpdate; override;
  public  
    procedure UpdateValue; override;
  end;

var
  IsDesignTime: Boolean;

const
  FILTER_PROPERTY_NAME = 'Visible';
  FILTER_PROPERTY_VALUE = 'True'; 

implementation


{ TCROCustomOperation }

constructor TCROCustomOperation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAttached := False;
  FConnected := False;
  FAction := oaSelect;

  IsDesignTime := csDesigning in Self.ComponentState;

  // Create a list to store the operatios name for the service
  fOperationNames := TStringList.Create;
  fParams := TCROOperationParams.Create(Self);

  fParamUpdated := False;
end;

destructor TCROCustomOperation.Destroy;
begin
  fParams.Free;
  fOperationNames.Free;

  inherited;
end;

function TCROCustomOperation.Execute: Boolean;


      // Prepare an array with the params information
      procedure PrepareParams(var ParamInfoList: TParamInfoArray);
      var ParamInfo: PParamInfo;
          i: Integer;
          size: Integer;
          Instance: TObject;
          PropInfo: PPropInfo;
          PropName,AExpression: String;

          obj: TObject;
          value: Variant;

          AParamInfo: TParamInfoArray;
      begin
        // Loop to check all params
        for i := 0 to Self.Params.Count - 1 do
        begin
          Instance := nil;
          PropInfo := nil;
          New(ParamInfo);

          // Set the information for the param
          ParamInfo^.OperationParam := Params[i];
          
          if (Params[i].Expression <> '') then
          begin
            // Call the function to get the instance, pointer and propinfo
            AExpression := Params[i].Expression;
            GetPropInfoFromExpression(GetTrueOwner(Self.Owner,AExpression), AExpression, Instance, PropInfo);
            ParamInfo^.Instance := Instance;
            ParamInfo^.PropInfo := PropInfo;

            //skip non input parameters
            if (Params[i].FParamType in [fIn,fInOut]) then
            begin
              // Get the property name assigned in the expression
              PropName := ParamInfo^.PropInfo^.Name;

              // Verify if the property is an object
              if ParamInfo^.PropInfo^.PropType^.Kind = tkClass then
              begin
                // Get the property
                obj := GetObjectProp(Instance, PropName);

                // Set the object
                Params[i].ValueAsObject := obj;
              end
              else if ParamInfo^.PropInfo^.PropType^.Kind in [tkInteger, tkChar, tkFloat,
                        tkString, tkWChar, tkLString, tkWString, tkVariant, tkInt64] then
              begin
                // Get the property
                value := GetPropValue(Instance, PropName);

                // Set the value
                Params[i].Value := value;
              end;
            end;
          end;

          // Get the current size of the array
          size := Length(AParamInfo);
          // Set the new size for the array
          SetLength(AParamInfo, size+1);
          // Store the paramInfo in the array
          AParamInfo[size] := ParamInfo;
        end;

        // Return the array
        ParamInfoList := AParamInfo;
      end;

      // Set the values of the params in the array to the property in expression and
      // free the pointer of the param values
      procedure UnprepareParams(var ParamInfoList: TParamInfoArray);
      var i: Integer;
          size: Integer;
          Instance: TObject;
          PropName: String;
          obj: TObject;
          value: Variant;
      begin
        // Get the size of the array
        size := Length(ParamInfoList)-1;

        // Loop to get all params
        for i := 0 to size do
        begin
          //skip non output parameters or expression is empty
          if (not (ParamInfoList[i].OperationParam.FParamType in [fOut,fInOut,fResult])) or
             (ParamInfoList[i].OperationParam.Expression = '')
          then Continue;

          // Get the instance of the object owner of the property assigned in the
          // expression
          Instance  := ParamInfoList[i].Instance;
          // Get the property name assigned in the expression
          PropName := ParamInfoList[i].PropInfo^.Name;

          // Verify if the property is an object
          if ParamInfoList[i].PropInfo^.PropType^.Kind = tkClass then
          begin
            // Get the object stored in the pointer
            obj := ParamInfoList[i].OperationParam.ValueAsObject;

            // Set the property
            SetObjectProp(Instance, PropName, obj);
          end
          else if ParamInfoList[i].PropInfo^.PropType^.Kind in [tkInteger, tkChar, tkFloat,
                    tkString, tkWChar, tkLString, tkWString, tkVariant, tkInt64] then
          begin
            // Get the value stored in the pointer
            value := ParamInfoList[i].OperationParam.Value;

            // Set the property
            SetPropValue(Instance, PropName, value);
          end;
        end;
      end;

      // Write the params values to send to the server
      procedure WriteInputParams(const ParamInfoList: TParamInfoArray);

            procedure WriteInteger(ParamInfo: PParamInfo);
            var tmp: Integer;
            begin
              if ParamInfo^.OperationParam.Value <> null
              then tmp := ParamInfo^.OperationParam.Value;

              FROProxy.__Message.Write(ParamInfo^.OperationParam.FName,
                TypeInfo(Integer), tmp, []);
            end;

            procedure WriteDateTime(ParamInfo: PParamInfo);
            var tmp: TDateTime;
            begin
              if ParamInfo^.OperationParam.Value <> null
              then tmp := ParamInfo^.OperationParam.Value;

              FROProxy.__Message.Write(ParamInfo^.OperationParam.FName,
                TypeInfo(TDateTime), tmp, []);
            end;

            procedure WriteDouble(ParamInfo: PParamInfo);
            var tmp: Double;
            begin
              if ParamInfo^.OperationParam.Value <> null
              then tmp := ParamInfo^.OperationParam.Value;

              FROProxy.__Message.Write(ParamInfo^.OperationParam.FName,
                TypeInfo(Double), tmp, []);
            end;

            procedure WriteCurrency(ParamInfo: PParamInfo);
            var tmp: Currency;
            begin
              if ParamInfo^.OperationParam.Value <> null
              then tmp := ParamInfo^.OperationParam.Value;

              FROProxy.__Message.Write(ParamInfo^.OperationParam.FName,
                TypeInfo(Currency), tmp, []);
            end;

            procedure WriteWidestring(ParamInfo: PParamInfo);
            var tmp: Widestring;
            begin
              if ParamInfo^.OperationParam.Value <> null
              then tmp := ParamInfo^.OperationParam.Value;

              FROProxy.__Message.Write(ParamInfo^.OperationParam.FName,
                TypeInfo(Widestring), tmp, []);
            end;

            procedure WriteString(ParamInfo: PParamInfo);
            var tmp: String;
            begin
              if ParamInfo^.OperationParam.Value <> null
              then tmp := ParamInfo^.OperationParam.Value;

              FROProxy.__Message.Write(ParamInfo^.OperationParam.FName,
                TypeInfo(String), tmp, []);
            end;

            procedure WriteInt64(ParamInfo: PParamInfo);
            var tmp: Int64;
            begin
              if ParamInfo^.OperationParam.Value <> null
              then tmp := ParamInfo^.OperationParam.Value;

              FROProxy.__Message.Write(ParamInfo^.OperationParam.FName,
                TypeInfo(Int64), tmp, []);
            end;

            procedure WriteBoolean(ParamInfo: PParamInfo);
            var tmp: Boolean;
            begin
              if ParamInfo^.OperationParam.Value <> null
              then tmp := ParamInfo^.OperationParam.Value;

              FROProxy.__Message.Write(ParamInfo^.OperationParam.FName,
                TypeInfo(Boolean), tmp, []);
            end;

            procedure WriteVariant(ParamInfo: PParamInfo);
            var tmp: Variant;
            begin
              tmp := ParamInfo^.OperationParam.Value;

              FROProxy.__Message.Write(ParamInfo^.OperationParam.FName,
                TypeInfo(Variant), tmp, []);
            end;

            procedure WriteObject(ParamInfo: PParamInfo);
            var tmp: TObject;
            begin
              tmp := ParamInfo^.OperationParam.ValueAsObject;

              FROProxy.__Message.Write(ParamInfo^.OperationParam.FName,
                FindROClass(ParamInfo^.OperationParam.ParamClass).ClassInfo, tmp, []);
            end;

      var i: integer;
          size: Integer;
      begin
        // Get the size of the array
        size := Length(ParamInfoList)-1;
        for i := 0 to size do
        begin
          // Verify the flag of the param
          if ParamInfoList[i].OperationParam.ParamType in [fIn, fInOut] then
          begin
            case ParamInfoList[i].OperationParam.DataType of
              rtInteger    : WriteInteger(ParamInfoList[i]);
              rtDateTime   : WriteDateTime(ParamInfoList[i]);
              rtDouble     : WriteDouble(ParamInfoList[i]);
              rtCurrency   : WriteCurrency(ParamInfoList[i]);
              rtWidestring : WriteWidestring(ParamInfoList[i]);
              rtString     : WriteString(ParamInfoList[i]);
              rtInt64      : WriteInt64(ParamInfoList[i]);
              rtBoolean    : WriteBoolean(ParamInfoList[i]);
              rtVariant    : WriteVariant(ParamInfoList[i]);
              rtBinary     : ;
              rtUserDefined: WriteObject(ParamInfoList[i]);
            end;
          end;
        end;
      end;

      // Read the params values received from the server
      procedure ReadOutputParams(var ParamInfoList: TParamInfoArray);

            procedure ReadInteger(ParamInfo: PParamInfo);
            var tmp: Integer;
            begin
              FROProxy.__Message.Read(ParamInfo^.OperationParam.FName,
                TypeInfo(Integer), tmp, []);

              ParamInfo^.OperationParam.Value := tmp;
            end;

            procedure ReadDateTime(ParamInfo: PParamInfo);
            var tmp: TDateTime;
            begin
              FROProxy.__Message.Read(ParamInfo^.OperationParam.FName,
                TypeInfo(TDateTime), tmp, []);

              ParamInfo^.OperationParam.Value := tmp;
            end;

            procedure ReadDouble(ParamInfo: PParamInfo);
            var tmp: Double;
            begin
              FROProxy.__Message.Read(ParamInfo^.OperationParam.FName,
                TypeInfo(Double), tmp, []);

              ParamInfo^.OperationParam.Value := tmp;
            end;

            procedure ReadCurrency(ParamInfo: PParamInfo);
            var tmp: Currency;
            begin
              FROProxy.__Message.Read(ParamInfo^.OperationParam.FName,
                TypeInfo(Currency), tmp, []);

              ParamInfo^.OperationParam.Value := tmp;
            end;

            procedure ReadWidestring(ParamInfo: PParamInfo);
            var tmp: Widestring;
            begin
              FROProxy.__Message.Read(ParamInfo^.OperationParam.FName,
                TypeInfo(Widestring), tmp, []);

              ParamInfo^.OperationParam.Value := tmp;
            end;

            procedure ReadString(ParamInfo: PParamInfo);
            var tmp: String;
            begin
              FROProxy.__Message.Read(ParamInfo^.OperationParam.FName,
                TypeInfo(String), tmp, []);

              ParamInfo^.OperationParam.Value := tmp;
            end;

            procedure ReadInt64(ParamInfo: PParamInfo);
            var tmp: Int64;
            begin
              FROProxy.__Message.Read(ParamInfo^.OperationParam.FName,
                TypeInfo(Int64), tmp, []);

              ParamInfo^.OperationParam.Value := tmp;
            end;

            procedure ReadBoolean(ParamInfo: PParamInfo);
            var tmp: Boolean;
            begin
              FROProxy.__Message.Read(ParamInfo^.OperationParam.FName,
                TypeInfo(Boolean), tmp, []);

              ParamInfo^.OperationParam.Value := tmp;
            end;

            procedure ReadVariant(ParamInfo: PParamInfo);
            var tmp: Variant;
            begin
              FROProxy.__Message.Read(ParamInfo^.OperationParam.FName,
                TypeInfo(Variant), tmp, []);

              ParamInfo^.OperationParam.Value := tmp;
            end;

            procedure ReadObject(ParamInfo: PParamInfo);
            var tmp: TObject;
            begin
              FROProxy.__Message.Read(ParamInfo^.OperationParam.FName,
                FindROClass(ParamInfo^.OperationParam.ParamClass).ClassInfo, tmp, []);

              ParamInfo^.OperationParam.ValueAsObject := tmp;
            end;

      var i: integer;
          size: Integer;
      begin
        // Get the size of the array
        size := Length(ParamInfoList)-1;
        for i := 0 to size do
        begin
          if ParamInfoList[i].OperationParam.ParamType in [fOut, fInOut, fResult] then
          begin
            case ParamInfoList[i].OperationParam.DataType of
              rtInteger    : ReadInteger(ParamInfoList[i]);
              rtDateTime   : ReadDateTime(ParamInfoList[i]);
              rtDouble     : ReadDouble(ParamInfoList[i]);
              rtCurrency   : ReadCurrency(ParamInfoList[i]);
              rtWidestring : ReadWidestring(ParamInfoList[i]);
              rtString     : ReadString(ParamInfoList[i]);
              rtInt64      : ReadInt64(ParamInfoList[i]);
              rtBoolean    : ReadBoolean(ParamInfoList[i]);
              rtVariant    : ReadVariant(ParamInfoList[i]);
              rtBinary     : ;
              rtUserDefined: ReadObject(ParamInfoList[i]);
            end;
          end;
        end;
      end;

var __request, __response : TMemoryStream;
    __ParamInfoList: TParamInfoArray;
    i: Integer;
begin
  Result := False;
  if not FAttached
  then Exit;

  __request  := TMemoryStream.Create;
  __response := TMemoryStream.Create;

  try
    try
      __ParamInfoList := nil;

      PrepareParams(__ParamInfoList);

      fROProxy.__Message.Initialize(fROProxy.__TransportChannel, fRODLLibrary.Info.Name, fROProxy.__InterfaceName, fOperationName);

      WriteInputParams(__ParamInfoList);

      fROProxy.__Message.Finalize;

      fROProxy.__Message.WriteToStream(__request);
      fROProxy.__TransportChannel.Dispatch(__request, __response);
      fROProxy.__Message.ReadFromStream(__response);

      ReadOutputParams(__ParamInfoList);

      UnprepareParams(__ParamInfoList);

      Result := True;
      FConnected := True;
    except
      on E:Exception do
      begin
        raise Exception.Create('Execute Error'#10#13+E.Message);
        Result := False;
      end;
    end;
  finally
    __request.Free;
    __response.Free;

    for i := 0 to Length(__ParamInfoList)-1
    do Dispose(__ParamInfoList[i]);

    Finalize(__ParamInfoList);
    FreeMem(__ParamInfoList);
  end;
end;

function TCROCustomOperation.GetOperationNames: TStrings;
begin
  Result := fOperationNames;
end;

procedure TCROCustomOperation.Loaded;
begin
  inherited;
  Params.FixParamExpressions;
end;

procedure TCROCustomOperation.SetOperationName(Value: String);
var i: Integer;
    param: TRODLOperationParam;
begin
  if fOperationName <> Trim(Value)
  then fParamUpdated := False;

  fOperationName := Trim(Value);

  if not (csLoading in Self.ComponentState) then
  begin
    if Assigned(fRODLService) then
    begin
      Self.Params.Clear;

      fRODLOperation := fRODLService.Default.FindOperation(fOperationName);

      if Assigned(fRODLOperation) then
      begin
        for i := 0 to fRODLOperation.Count - 1 do
        begin
          param := fRODLOperation.Items[i];

          if StrToDataType(param.Info.DataType) = rtUserDefined
          then Self.Params.CreateParam(StrToDataType(param.Info.DataType), param.Info.Name, param.Info.Flag, param.Info.DataType, Self)
          else Self.Params.CreateParam(StrToDataType(param.Info.DataType), param.Info.Name, param.Info.Flag, '', Self);
        end;
      end;

      fParamUpdated := True;
    end
    else
      Self.Params.Clear;
  end;

  //update FOperator and FDataType in Handle Objects
  for i := 0 to Self.Params.Count - 1
  do Self.Params[i].SetExpression(Self.Params[i].FExpression);
end;

procedure TCROCustomOperation.SetParams(Value: TCROOperationParams);
begin
  fParams := Value;
end;

procedure TCROCustomOperation.SetRORemoteService(Value: TRORemoteService);
var i: Integer;
    fROProxyClass: TROProxyClass;
    obj: TObject;
begin
  fRORemoteService := Value;

  // Clear the list to refresh the operation names
  fOperationNames.Clear;

  // Verify if a Remote Service is assigned
  if Assigned(fRORemoteService) then
  begin
    // Get the Library information
    try
      fRODLLibrary := fRORemoteService.GetRODLLibrary;
      FAttached := True;
      if Assigned(FOnConnectionSuccess)
      then FOnConnectionSuccess(Self);
    except
      FAttached := False;

      if csDesigning in ComponentState then
      begin
        MessageDlg('Could not attach to server. Close the project, start the server and try again!'#10#13+
                   'Design without a server running may cause lost of property values.',mtInformation,[mbOk],0);
      end;

      if Assigned(FOnConnectionFailure)
      then FOnConnectionFailure(Self);
    end;
  end;

  if FAttached then
  begin
    // Get the service in the library
    fRODLService := fRODLLibrary.FindService(fRORemoteService.ServiceName);

    // Get GUID from the service
    fServiceGUID := fRODLService.Default.Info.UID;

    // Loop to store the operations names
    for i := 0 to fRODLService.Default.Count - 1
    do fOperationNames.Add(fRODLService.Default.Items[i].Info.Name);

    if not IsDesignTime then
    begin
      fROProxyClass := FindProxyClass(fServiceGUID, False);

      if Assigned(fROProxyClass) then
      begin
        fROProxy := fROProxyClass.Create(fRORemoteService.Message, fRORemoteService.Channel);
        fROProxy.GetInterface(fServiceGUID, obj);
      end;
    end;
  end
  else
  begin
    fRODLLibrary := nil;
    fRODLService := nil;
    fROProxy     := nil;

    fOperationName := '';
  end;

  if not fParamUpdated
  then SetOperationName(fOperationName);
end;

{ TCROOperationParam }

procedure TCROOperationParam.Assign(Source: TPersistent);
begin
  if Source is TCROOperationParam then
    AssignParam(TCROOperationParam(Source))
  else
    inherited Assign(Source);
end;

procedure TCROOperationParam.AssignParam(Param: TCROOperationParam);
begin
  if Param <> nil then
  begin
    FDataType := Param.DataType;
    FParamClass := Param.FParamClass;
    FName := Param.Name;
    FParamType := Param.ParamType;
    FData := Param.Value;
    FDataAsObject := Param.ValueAsObject;
  end;
end;

procedure TCROOperationParam.Clear;
begin
  FData := null;
  FDataAsObject := nil;
end;

constructor TCROOperationParam.Create(Collection: TCollection; AName: String; ADataType:TRODataType;
  AParamType:TRODLParamFlag; AParamClass: String);
begin
  inherited Create(Collection);

  FName       := AName;
  FDataType   := ADataType;
  FParamType  := AParamType;
  FParamClass := AParamClass;
end;

constructor TCROOperationParam.Create(Collection: TCollection);
begin
  inherited;
end;

destructor TCROOperationParam.Destroy;
begin
  inherited;
end;

procedure TCROOperationParam.FixParamExpression;
begin
  SetExpression(FExpression);
end;

function TCROOperationParam.GetValue: Variant;
begin
  if FDataType <> rtUserDefined
  then Result := FData
  else Result := null;
end;

function TCROOperationParam.GetValueAsObject: TObject;
begin
  if FDataType = rtUserDefined
  then Result := FDataAsObject
  else Result := nil;
end;

procedure TCROOperationParam.SetDataType(Value: TRODataType);
begin
  FDataType := Value;
end;

procedure TCROOperationParam.SetExpression(Value: String);
var K,L: Byte; ExpressionHandleName,OwnerName: String; AComp: TComponent; AOperation: TPersistent;
begin
  FExpression := Value;
  //if expression is a CROHandle.data  
  if (FDataType = rtUserDefined) then
  begin
    //if FExpressionHandle is not nil, turn it null
    if Assigned(FExpressionHandle) then
    begin
      try
        FExpressionHandle.FIsLinkedToParam := False;
        FExpressionHandle.FOperation := nil;
        FExpressionHandle.FHandleDataType := '';
        FExpressionHandle.UpdateDataType;
        FExpressionHandle := nil;
      except
      end;  
    end;
    //reload FExpressionHandle
    K := Pos('.',FExpression); //supoe q o primeiro ponto identifica a Handle, abaixo trata o caso contrario
    if (K > 0) then
    begin
      ExpressionHandleName := Copy(FExpression,1,K-1);

      //verifica se o primeiro identificador é o verdadeiro owner do handle
      L := Pos('.',Copy(FExpression,K+1,MaxInt));  //FExpression = Form9.ListHandle1.Data
      if L > 0 then
      begin
        OwnerName := ExpressionHandleName; //Form9
        ExpressionHandleName := Copy(FExpression,K+1,L-1); //ListHandle1
      end
      else
        OwnerName := '';

      AOperation := Collection.Owner;
      if Assigned(AOperation) then
      begin
        if Trim(TCROCustomOperation(AOperation).fOperationName) <> '' then
        begin
          AComp := TCROCustomOperation(AOperation).Owner;
          AComp := GetTrueOwner(AComp,OwnerName).FindComponent(ExpressionHandleName);
          if Assigned(AComp) then
          begin
            FExpressionHandle := TCROHandle(AComp);
            if Assigned(FExpressionHandle) then
            begin
              FExpressionHandle.FIsLinkedToParam := True;
              FExpressionHandle.FExpression := '';
              FExpressionHandle.FOperation := TCROCustomOperation(AOperation);//must call SetOperation
              FExpressionHandle.FOperation.FreeNotification(FExpressionHandle);
              FExpressionHandle.FHandleDataType := Self.ParamClass;
              FExpressionHandle.UpdateDataType;
            end;
          end;
        end;  
      end;
    end;
  end;
end;

procedure TCROOperationParam.SetName(Value: String);
begin
  FName := Value;
end;

procedure TCROOperationParam.SetParamClass(Value: String);
begin
  FParamClass := Value;
end;

procedure TCROOperationParam.SetParamType(Value: TRODLParamFlag);
begin
  FParamType := Value;
end;

procedure TCROOperationParam.SetValue(Value: Variant);
begin
  if FDataType <> rtUserDefined
  then FData := Value;
end;

procedure TCROOperationParam.SetValueAsObject(Value: TObject);
begin
  if FDataType = rtUserDefined
  then FDataAsObject := Value;
end;

{ TCROOperationParams }

procedure TCROOperationParams.AddParam(Value: TCROOperationParam);
begin
  Value.Collection := Self;
end;

procedure TCROOperationParams.AssignTo(Dest: TPersistent);
begin
  if Dest is TCROOperationParams then TCROOperationParams(Dest).Assign(Self)
  else inherited AssignTo(Dest);
end;

procedure TCROOperationParams.AssignValues(
  Value: TCROOperationParams);
var
  I: Integer;
  P: TCROOperationParam;
begin
  for I := 0 to Value.Count - 1 do
  begin
    P := FindParam(Value[I].Name);
    if P <> nil then
      P.Assign(Value[I]);
  end;
end;

constructor TCROOperationParams.Create;
begin
  FOwner := nil;
  inherited Create(TCROOperationParam);
end;

constructor TCROOperationParams.Create(Owner: TCROCustomOperation);
begin
  inherited Create(TCROOperationParam);
  FOwner := Owner;
end;

function TCROOperationParams.CreateParam(DataType: TRODataType;
  const ParamName: string;
  ParamType: TRODLParamFlag; ParamClass: String; Operation: TCROCustomOperation): TCROOperationParam;
begin
  Result := TCROOperationParam.Create(Self, ParamName, DataType, ParamType, ParamClass);
end;

function TCROOperationParams.FindParam(
  const Value: string): TCROOperationParam;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := TCROOperationParam(inherited Items[I]);
    if AnsiCompareText(Result.Name, Value) = 0 then Exit;
  end;
  Result := nil;
end;

procedure TCROOperationParams.FixParamExpressions;
var i: Integer;
begin
  for i := 0 to Count-1
  do TCROOperationParam(Items[i]).FixParamExpression;
end;

function TCROOperationParams.GetItem(
  Index: Integer): TCROOperationParam;
begin
  Result := TCROOperationParam(inherited Items[Index]);
end;

function TCROOperationParams.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TCROOperationParams.GetParamList(List: TList;
  const ParamNames: string);
var
  Pos: Integer;
begin
  Pos := 1;
  while Pos <= Length(ParamNames) do
    List.Add(ParamByName(ExtractFieldName(ParamNames, Pos)));
end;

function TCROOperationParams.ParamByName(
  const Value: string): TCROOperationParam;
begin
  Result := FindParam(Value);
end;

procedure TCROOperationParams.RemoveParam(Value: TCROOperationParam);
begin
  Value.Collection := nil;
end;

procedure TCROOperationParams.SetItem(Index: Integer;
  Value: TCROOperationParam);
begin
  inherited SetItem(Index, TCollectionItem(Value));
end;

//----

{ TCROHandle }

constructor TCROHandle.Create(AOwner: TComponent);
begin
  inherited;
  FAutoEdit := False;
  FListProperties := TList.Create;
  FHandleList     := TList.Create;
  FIsLinkedToParam := False;
  FState := csBrowse;
end;

procedure TCROHandle.DeleteProperties(AProperties: TCROCustomProperties);
var iIndex: Integer;
begin
  iIndex := FListProperties.IndexOf(AProperties);
  if iIndex > -1
  then FListProperties.Delete(iIndex);
end;

destructor TCROHandle.Destroy;
begin
  while FListProperties.Count > 0
  do TCROCustomProperties(FListProperties[0]).CursorHandle := nil;
  FListProperties.Free;
  FHandleList.Free;
  inherited;
end;

procedure TCROHandle.EvaluateExpression;
var
  Instance: TObject;
  PropInfo: PPropInfo;
  AExpression: String;
begin
  if Expression <> '' then
  begin
    Instance := nil;
    // Call the function to get the instance, pointer and propinfo
    AExpression := Expression;
    GetPropInfoFromExpression(GetTrueOwner(Self.Owner,AExpression), AExpression, Instance, PropInfo);

    // Verify if the property is an object
    if PropInfo^.PropType^.Kind = tkClass then
    begin
      // Set the property
      Data := TROComplexType(GetObjectProp(Instance, PropInfo^.Name));
    end
    else
      MessageDlg(Name+'.EvaluateExpression - The Property is not from the expected type.', mtError, [mbOk], 0);
  end;
end;

procedure TCROHandle.InsertProperties(AProperties: TCROCustomProperties);
begin
  if FListProperties.IndexOf(AProperties) = -1
  then FListProperties.Add(AProperties);
end;

procedure TCROHandle.Loaded;
begin
  inherited;
  SetExpression(FExpression);
end;

procedure TCROHandle.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FOperation
    then FOperation := nil;
  end;
end;

function TCROHandle.Post: Boolean;
var AOperation: TCROCustomOperation; 
begin
  if (FState in [csEdit,csInsert]) then
  begin
    AOperation := nil;
    case FState of
      csEdit:
      begin
        if Assigned(FServiceManager)
        then AOperation := FServiceManager.FOperationForUpdate
        else
        begin
          if Assigned(FOperation) and (FOperation.Action = oaUpdate)
          then AOperation := FOperation;
        end;
      end;
      csInsert:
      begin
        if Assigned(FServiceManager)
        then AOperation := FServiceManager.FOperationForInsert
        else
        begin
          if Assigned(FOperation) and (FOperation.Action = oaInsert)
          then AOperation := FOperation;
        end;
      end;
    end;

    //executa serviço responsavel por enviar os dados ao servidor
    if Assigned(AOperation) then
    begin
      try
        AOperation.Execute;
      except
        on E:Exception do
        begin
          MessageDlg(E.Message,mtInformation,[mbOk],0);
          Result := False;
          Exit;
        end;
      end;
    end;

    SetState(csBrowse);
    UpdateProperties;
  end;
  Result := True;
end;

procedure TCROHandle.SetData(Value: TROComplexType);
begin
  FHandleUpdateTime := Now;
  FData := Value;
  EvaluateHandleList;
end;

procedure TCROHandle.SetDataType(const Value: String);
begin
  FHandleDataType := Value;
end;

procedure TCROHandle.SetExpression(Value: String);
var k,j:Integer; FOldExpression:String;
      // The expression must be in the format: <Handle>.Data.<atribute>[.<atribute>]
      procedure CheckExpression;
      var sExpression: String;
          sHandleName: String;
          Handle: TCROHandle;
          Struct: TRODLStruct;
          sElementName: String;
          i: Integer;
          bFoundElement: Boolean;
          AComp: TComponent;
      begin
        Handle := nil;
        sExpression := Value;

        if (sExpression <> '') then
        begin
          try
            AComp := GetTrueOwner(Owner,sExpression);

            sHandleName := GetNextIdentifier(sExpression);
            if CompareText(GetNextIdentifier(sExpression),'DATA') <> 0
            then raise Exception.Create('A handle data or handle data property is not specified!');

            AComp := AComp.FindComponent(sHandleName);
            if Assigned(AComp) and AComp.InheritsFrom(TCROHandle) then
            begin
              Handle := TCROHandle(AComp);
              if Assigned(Handle.FOperation) and (Handle.FOperation.FAttached) then
              begin
                //objeto carregado no CURSORhandle passado no expression - naum faz sentido usar listhandle, pois naum possui objeto q contenha propriedades q possam conter objetos
                Struct := Handle.FOperation.fRODLLibrary.FindStruct(Handle.FHandleDataType);
                DataType := Handle.FHandleDataType;

                FLinkedHandle := Handle;
                SourceOperation := Handle.SourceOperation;
                FLinkedHandle.FHandleList.Add(Self);

                //busca o objeto a ser carregado no Self.DATA, caso ja naum seja um data passado
                while sExpression <> '' do
                begin

                  sElementName := GetNextIdentifier(sExpression);
                  bFoundElement := False;

                  for i := 0 to Struct.Count - 1 do
                  begin
                    if CompareText(Struct.Items[i].Info.Name, sElementName) = 0 then
                    begin
                      DataType := Struct.Items[i].Info.DataType;
                      Struct := Handle.FOperation.fRODLLibrary.FindStruct(DataType);
                      bFoundElement := True;
                      Break;
                    end;
                  end;

                  if not bFoundElement
                  then raise Exception.CreateFmt('Invalid Expression. Element "%s" not found!',[sElementName]);
                end;
              end;
            end
            else
              raise Exception.Create('');

          except
            on E:Exception do
            begin
              if (E.Message = '') and ((not Assigned(Handle) or (not Assigned(Handle.SourceOperation))))
              then MessageDlg('No Operation found to perform validation!', mtError, [mbOk], 0)
              else MessageDlg(E.Message+#10#13'The Expression has not the expected format.', mtError, [mbOk], 0);
              FExpression := FOldExpression;
            end;
          end;
        end
        else
        begin
          if Assigned(FLinkedHandle) then
          begin
            FLinkedHandle.FHandleList.Delete(FLinkedHandle.FHandleList.IndexOf(Self));
            FLinkedHandle := nil;
          end;
        end;
      end;

begin
  if (not (csLoading in Self.ComponentState)) and (Trim(Value) <> '') then
  begin
    if FIsLinkedToParam then
    begin
      MessageDlg('The Expression can not be used because this handle is controlled by an operation.', mtError, [mbOk], 0);
      Exit;
    end;

    FOldExpression := FExpression;
    FExpression := Value;
    CheckExpression;
  end
  else
    FExpression := Trim(Value);

//neste momento os FListProperties podem naum estar carregados ainda, neste caso o datatype naum estará corretamente definido
//Property owners devem chamar o SetExpression dos handles em seu metodo loaded para reexecutar este evento
  for k := 0 to FListProperties.Count-1 do
  begin
    if Assigned(TCROCustomProperties(FListProperties[k]).FOnInternalUpdateExpression) then
    begin
      TCROCustomProperties(FListProperties[k]).FixDataType;
      TCROCustomProperties(FListProperties[k]).FOnInternalUpdateExpression(FListProperties[k]);
    end;
  end;

  if Self.InheritsFrom(TCROListHandle) then
  begin
    for j := 0 to TCROListHandle(Self).FCursorHandleList.Count-1 do
    begin
      for k := 0 to TCROCursorHandle(TCROListHandle(Self).FCursorHandleList[j]).FListProperties.Count-1 do     //**
      begin
        if Assigned(TCROCustomProperties(TCROCursorHandle(TCROListHandle(Self).FCursorHandleList[j]).FListProperties[k]).FOnInternalUpdateExpression) then
        begin
          TCROCustomProperties(TCROCursorHandle(TCROListHandle(Self).FCursorHandleList[j]).FListProperties[k]).FixDataType;
          TCROCustomProperties(TCROCursorHandle(TCROListHandle(Self).FCursorHandleList[j]).FListProperties[k]).FOnInternalUpdateExpression(TCROCursorHandle(TCROListHandle(Self).FCursorHandleList[j]).FListProperties);
        end;
      end;
    end;
  end;
end;

procedure TCROHandle.SetOperation(Value: TCROCustomOperation);
begin
  if FOperation <> Value
  then FOperation := Value;

  if Assigned(FOperation) then
  begin
    if Self.InheritsFrom(TCROListHandle) then
    begin
      if Assigned(TCROListHandle(Self).InternalCursorHandle)
      then TCROListHandle(Self).InternalCursorHandle.SourceOperation := FOperation;
    end;
  end;
end;

procedure TCROHandle.SetServiceManager(const Value: TCROServiceManager);
begin
  FServiceManager := Value;
  if Assigned(FServiceManager) then
  begin
    if not Assigned(FOperation)
    then FOperation := FServiceManager.FOperationForSelect;//tem q ter algum operation
  end;
end;

procedure TCROHandle.UpdateProperties;
var i: Integer;
begin
  if (not DisableUpdate) or (FCanceling) then
  begin
    for i := 0 to FListProperties.Count-1
    do TCROCustomProperties(FListProperties[i]).UpdateValue;

    for i := 0 to FHandleList.Count - 1
    do TCROHandle(FHandleList[i]).UpdateProperties;
  end;
end;

procedure TCROHandle.FixExpression;
begin
  SetExpression(FExpression);
end;

procedure TCROHandle.SetDisableUpdate(const Value: Boolean);
begin
  FDisableUpdate := Value;
end;

procedure TCROHandle.EvaluateHandleList;
var i:Integer;
begin
  if (not DisableUpdate) then
  begin
    if Assigned(FData) then
    begin
      for i := 0 to FHandleList.Count - 1
      do TCROHandle(FHandleList[i]).EvaluateExpression;
    end
    else
    begin
      for i := 0 to FHandleList.Count - 1
      do TCROHandle(FHandleList[i]).Clear;
    end;
  end;
end;

{ TCROListHandle }

procedure TCROListHandle.AddCursorHandle(
  ACursorHandle: TCROCursorHandle);
begin
  if Assigned(ACursorHandle) and (FCursorHandleList.IndexOf(ACursorHandle) = -1) then
  begin
    ACursorHandle.AutoEdit := FAutoEdit;
    FCursorHandleList.Add(ACursorHandle);
    ACursorHandle.FreeNotification(Self);
  end;
end;

function TCROListHandle.Count: Integer;
begin
  Result := FItems.Count;
end;

constructor TCROListHandle.Create(AOwner: TComponent);
begin
  inherited;
  FIndexMap := TStringList.Create;
  FItems := TList.Create;
  FItemIndex := -1;
  FCursorHandleList := TObjectList.Create;
  FCursorHandleList.OwnsObjects := False;

  InternalCursorHandle := TCROCursorHandle.Create(Self);
  InternalCursorHandle.Name := 'InternalCursorHandle';
  InternalCursorHandle.ListHandle := Self;
end;

destructor TCROListHandle.Destroy;
begin
  FCursorHandleList.Free;
  InternalCursorHandle.Free;
  FItems.Free;
  FIndexMap.Free;
  inherited;
end;

function TCROListHandle.Prior: Boolean;
begin
  Result := False;
  case FState of
    csInsert: //if inserting then does nothing
    begin
      Cancel;
      Exit;
    end;
    csEdit: Cancel; //if editing then cancel before go on
  end;

  if Assigned(Self.FData) and (FItemIndex > 0) then
  begin
    ItemIndex := ItemIndex-1;
    Result := True;
    EOF := False;
  end
  else
    BOF := True;
  SetState(csBrowse);
end;

function TCROListHandle.Next: Boolean;
begin
  Result := False;
{  case FState of
    csInsert: Exit; //if inserting then does nothing
    csEdit: Cancel; //if editing then cancel before go on
  end;}
  case FState of
    csInsert: //if inserting then does nothing
    begin
      Cancel;
      Exit;
    end;
    csEdit: Cancel; //if editing then cancel before go on
  end;

  if Assigned(Self.FData) and (FItemIndex < Count-1) then
  begin
    ItemIndex := ItemIndex+1;
    Result := True;
    BOF := False;
  end
  else
    EOF := True;
  SetState(csBrowse);
end;

procedure TCROListHandle.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    //removes destroyed cursor from cursorlist
    if (AComponent is TCROCursorHandle) then
    begin
      if FCursorHandleList.IndexOf(AComponent) > -1
      then FCursorHandleList.Extract(AComponent);
    end;
  end;
end;

procedure TCROListHandle.SetData(Value: TROComplexType);
begin
  inherited;
  if not DisableUpdate then
  begin
    FState := csBrowse;
    Refresh;
    if FKeepCursorPosition
    then FItemIndex := Min(FItemIndex,Count-1)
    else
    begin
      if Count > 0
      then FItemIndex := 0
      else FItemIndex := -1;
    end;
  end;
  BOF := False;
  EOF := False;
  UpdateProperties;
  UpdateCursorHandle;
end;

procedure TCROListHandle.SetItemIndex(const Value: Integer);
begin
  if (FItemIndex <> Value) then
  begin
    if (FState in [csEdit,csInsert])   
    then Cancel;
    FItemIndex := Value;
    NotifyIndexUpdate;
    UpdateCursorHandle;
  end;
  EOF := False;
  BOF := False;
end;

procedure TCROListHandle.UpdateCursorHandle;
var i: Integer;
begin
  if FCursorHandleList.Count > 0 then
  begin
    for i := 0 to FCursorHandleList.Count-1 do
    begin
      if Assigned(FCursorHandleList[i]) then
      begin
        if FItemIndex < 0
        then TCROCursorHandle(FCursorHandleList[i]).Data := nil
        else
        begin
          if FItemIndex < Self.Count
          then TCROCursorHandle(FCursorHandleList[i]).Data := Items[FItemIndex];
        end;  
      end;
    end;
  end;
end;

procedure TCROListHandle.UpdateDataType;
var i: Integer;
begin
  for i := 0 to FCursorHandleList.Count - 1
  do TCROCursorHandle(FCursorHandleList[i]).UpdateDataType;
end;

procedure TCROListHandle.NotifyIndexUpdate;
var i: Integer;
begin
  if (not DisableUpdate) then
  begin
    if FListProperties.Count > 0 then
    begin
      for i := 0 to FListProperties.Count-1 do
      begin
        if (TObject(FListProperties[i]).InheritsFrom(TCROIndexedProperties))
        then TCROIndexedProperties(FListProperties[i]).NotifyIndexUpdate;
      end;
    end;
  end;
end;

function TCROListHandle.Insert: Boolean;
var obj: TROComplexType;
    iDataCount: Integer;
    AOperation: TCROCustomOperation;
begin
  FInserting := True;
  Result := False;
  if not (Assigned(FOperation) and (FOperation.FConnected or (FOperation.Action <> oaSelect)))
  then Exit;

  case FState of
    csEdit: Cancel;
    csInsert: Exit;
  end;

  obj := CreateROObject(InternalCursorHandle.FHandleDataType, SourceOperation.fRODLLibrary);

  iDataCount := TROArray(Data).Count;
  TROArray(Data).Resize(iDataCount+1);
  TROArray(Data).SetItemRef(iDataCount, obj);
  Refresh;//atualiza Items

  DisableUpdate := True;//desabilita atualização dos componentes até concluir o processo de inserção
  ItemIndex := Count-1;
  AOperation := nil;
  if Assigned(FServiceManager)
  then AOperation := FServiceManager.FOperationForCreate
  else
  if Assigned(FOperation) and (FOperation.Action = oaCreate)
  then AOperation := FOperation;

  if Assigned(AOperation)
  then AOperation.Execute; //este serviço deverá setar o novo valor para o Data do cursor handle - setData do cursor handle trata descarga de referencias perdidas
  DisableUpdate := False;//habilita atualização dos componentes

  //Atualiza componentes
  UpdateProperties;
  UpdateCursorHandle;
  NotifyIndexUpdate;

  if Trim(FIdentifierExpression) <> '' then
  begin
    InternalCursorHandle.FGeneratingIdentifier := True;
    try
      try
        InternalCursorHandle.SetDataProperty(FIdentifierExpression,GenerateIdentifier);//*
      except
        MessageDlg('Invalid IdentifierExpression!',mtError,[mbOk],0);
      end;
    finally
      InternalCursorHandle.FGeneratingIdentifier := False;
    end;
  end;
  Result := True;

  SetState(csInsert);
  FInserting := False;
end;

function TCROListHandle.Delete: Boolean;
begin
  Result := False;
  if Count = 0
  then Exit;

  case FState of
    csEdit: Cancel;
    csInsert:
    begin
      Cancel;
      Exit;
    end;
  end;

  try
    if Assigned(FServiceManager)
    then FServiceManager.FOperationForDelete.Execute
    else
    begin
      if Assigned(FOperation) and (FOperation.Action = oaDelete)
      then FOperation.Execute;
    end;
  except
    on E:Exception do
    begin
      MessageDlg(E.Message,mtInformation,[mbOk],0);
      Result := False;
      Exit;
    end;
  end;

  TROArray(Data).Delete(GetDataIndex(FItemIndex));

  Refresh;

  if (FItemIndex >= Count)
  then FItemIndex := Count - 1;

  SetState(csBrowse);

  NotifyIndexUpdate;
  UpdateCursorHandle;
  UpdateProperties;

  Result := True;
end;

procedure TCROListHandle.SetDataType(const Value: String);
begin
  inherited;
  UpdateDataType;
end;

procedure TCROListHandle.Cancel;
begin
  if (FState in [csEdit,csInsert]) then
  begin
    FCanceling := True;

    if FState = csInsert
    then TROArray(Data).Delete(GetDataIndex(FItemIndex));

    SetState(csCancel);

    Refresh;

    if ItemIndex >= Count
    then ItemIndex := Count-1;

    UpdateCursorHandle;
    UpdateProperties;
    FCanceling := False;
  end;
end;

procedure TCROListHandle.Edit;
begin
  if (FState <> csInsert) and (Count > 0)
  then SetState(csEdit);
end;

procedure TCROListHandle.Clear;
begin
  while Count > 0
  do TROArray(Data).Delete(GetDataIndex(FItemIndex));
  Refresh;
  NotifyIndexUpdate;
  UpdateCursorHandle;
  UpdateProperties;
end;

procedure TCROListHandle.SetState(const Value: TCursorState);
var i: Integer;
begin
  if FState <> Value then
  begin
    FState := Value;

    for i := 0 to FCursorHandleList.Count - 1 do
    begin
      if TCROCursorHandle(FCursorHandleList[i]).FState <> FState
      then TCROCursorHandle(FCursorHandleList[i]).SetState(FState);
    end;

    if (FState = csBrowse) and (not FCanceling)
    then UpdateProperties;

    if Assigned(FOnStateChange)
    then FOnStateChange(Self,FState);
  end;
end;

procedure TCROListHandle.SetAutoEdit(const Value: Boolean);
var i: Integer;
begin
  FAutoEdit := Value;
  for i := 0 to FCursorHandleList.Count - 1 do
  begin
    if TCROCursorHandle(FCursorHandleList[i]).AutoEdit <> FAutoEdit
    then TCROCursorHandle(FCursorHandleList[i]).AutoEdit := FAutoEdit;
  end;
end;

procedure TCROListHandle.EvaluateExpression;
var AItemIndex: Integer;
begin
  DisableUpdate := True;
  AItemIndex := ItemIndex;
  inherited;
  if AItemIndex < Count
  then ItemIndex := AItemIndex
  else ItemIndex := Count-1;
  DisableUpdate := False;
end;

procedure TCROListHandle.EvaluateHandleList;
var i: Integer;
begin
  for i := 0 to FCursorHandleList.Count - 1
  do TCROCursorHandle(FCursorHandleList[i]).EvaluateHandleList;
  inherited;
end;

procedure TCROListHandle.AddObject(Obj: TROComplexType);
begin
  DisableUpdate := True;
  Insert;
  AssignROObject(Obj,InternalCursorHandle.Data);
  Post;
  Refresh;
  DisableUpdate := False;
  UpdateProperties;
end;

function TCROListHandle.CopyObject(const Index: Integer;
  Dest: TCROListHandle): Boolean;
var Obj: TROComplexType;
begin
  Result := False;
  if (Index > -1) and (Index < Count) then
  begin
    Obj := Items[Index];
    Dest.AddObject(Obj);
    Result := True;
  end;
end;

function TCROListHandle.MoveObject(const Index: Integer;
  Dest: TCROListHandle): Boolean;
var AItemIndex: Integer;
begin
  Result := False;
  if (Index > -1) and (Index < Count) then
  begin
    CopyObject(Index,Dest);
    AItemIndex := ItemIndex;
    DisableUpdate := True;
    ItemIndex := Index;
    Delete;
    ItemIndex := Min(AItemIndex,Count-1);
    DisableUpdate := False;
    UpdateProperties;
    Result := True;
  end;
end;

function TCROListHandle.MoveDown(const Index: Integer): Boolean;
var CurrentObj,NextObj: TROComplexType;
begin
  Result := False;
  if Index < Count-1 then
  begin
    DisableUpdate := True;
    CurrentObj := TROArray(Data).GetItemRef(GetDataIndex(Index));
    NextObj := TROArray(Data).GetItemRef(GetDataIndex(Index+1));
    TROArray(Data).SetItemRef(Index+1,CurrentObj);
    TROArray(Data).SetItemRef(Index,NextObj);
    DisableUpdate := False;
    Result := True;
    Refresh;//atualiza Items
  end;
  UpdateProperties;
end;

function TCROListHandle.MoveUp(const Index: Integer): Boolean;
var CurrentObj,PreviousObj: TROComplexType;
begin
  Result := False;
  if Index > 0 then
  begin
    DisableUpdate := True;
    CurrentObj := TROArray(Data).GetItemRef(GetDataIndex(Index));
    PreviousObj := TROArray(Data).GetItemRef(GetDataIndex(Index-1));
    TROArray(Data).SetItemRef(Index-1,CurrentObj);
    TROArray(Data).SetItemRef(Index,PreviousObj);
    DisableUpdate := False;
    Result := True;
    Refresh;//atualiza Items
  end;
  UpdateProperties;
end;

function TCROListHandle.GetItem(Index: Integer): TROComplexType;
begin
  Result := TROComplexType(FItems[Index]);
end;

procedure TCROListHandle.SetItem(Index: Integer;
  const Value: TROComplexType);
var AItemIndex: Integer;  
begin
  FItems[Index] := Value;
  AItemIndex := GetDataIndex(Index);
  (Self.FData as TROArray).SetItemRef(AItemIndex,Value);
end;

procedure TCROListHandle.Refresh;
var i: Integer; Obj: TObject; V: Variant;
begin
  FItems.Clear;
  FIndexMap.Clear;
  for i := 0 to TROArray(Data).Count-1 do
  begin
    Obj := TROArray(Data).GetItemRef(i);
    V := FILTER_PROPERTY_VALUE;
    if IsPublishedProp(Obj.ClassType,FILTER_PROPERTY_NAME)
    then V := GetPropValue(Obj,FILTER_PROPERTY_NAME);
    if (not Filtered) or
       (V = FILTER_PROPERTY_VALUE) or
       ((i = TROArray(Data).Count-1) and (FInserting)) then
    begin
      FItems.Add(Obj);
      FIndexMap.Values[IntToStr(FItems.Count-1)] := IntToStr(i);
    end;
  end;
  FItemIndex := Min(FItemIndex,FItems.Count-1);
end;

function TCROListHandle.GetDataIndex(const AIndex: Integer): Integer;
begin
  Result := StrToIntDef(FIndexMap.Values[IntToStr(AIndex)],-1);
end;

function TCROListHandle.Post: Boolean;
begin
  Result := inherited Post;
  if Result then
  begin
    Refresh;
    UpdateProperties;
  end;
end;

{ TCROCursorHandle }

constructor TCROCursorHandle.Create(AOwner: TComponent);
begin
  inherited;
  DisableUpdate := False;
end;

function TCROCursorHandle.Delete: Boolean;
begin
  Result := False;
  case FState of
    csEdit: Cancel;
    csInsert:
    begin
      Cancel;
      Exit;
    end;
  end;

  if Assigned(ListHandle)
  then Result := ListHandle.Delete
  else
  begin
    Data.Free;
    Data := nil;
    Result := True;
  end;

  if Result then
  begin
    if Assigned(FServiceManager)
    then FServiceManager.FOperationForDelete.Execute
    else
    begin
      if Assigned(FOperation) and (FOperation.Action = oaDelete)
      then FOperation.Execute;
    end;
  end;
end;

destructor TCROCursorHandle.Destroy;
begin
  inherited;
end;

function TCROCursorHandle.GetDataProperty(Expression: String): Variant;
var
  Instance: TObject;
  PropInfo: PPropInfo;
  AExpression: String;
begin
  Instance := Self.Data;
  AExpression := Expression;

  // Call the function to get the instance, pointer and propinfo
  GetPropInfoFromExpression(GetTrueOwner(Self.Owner,AExpression), AExpression, Instance, PropInfo);

  if (Expression <> '') and (Instance <> nil) then
  begin
    if PropInfo^.PropType^.Kind in [tkInteger, tkChar, tkFloat,
              tkString, tkWChar, tkLString, tkWString, tkVariant, tkInt64, tkEnumeration] then
    begin
      // Set the property
      Result := GetPropValue(Instance, PropInfo^.Name);
    end
    else
    begin
      MessageDlg(Name+'GetDataProperty - The Property is not from the expected type.', mtError, [mbOk], 0);
    end;
  end;
end;

function TCROCursorHandle.GetDataPropertyAsObject(Expression: String): TObject;
var
  Instance: TObject;
  PropInfo: PPropInfo;
  AExpression: String;
begin
  Result := nil;
  Instance := Self.Data;
  AExpression := Expression;

  // Call the function to get the instance, pointer and propinfo
  GetPropInfoFromExpression(GetTrueOwner(Self.Owner,AExpression), AExpression, Instance, PropInfo);

  // Verify if the property is an object
  if PropInfo^.PropType^.Kind = tkClass then
  begin
    // Set the property
    Result := GetObjectProp(Instance, PropInfo^.Name);
  end
  else
  begin
    MessageDlg(Name+'GetDataPropertyAsObject - The Property is not from the expected type.', mtError, [mbOk], 0);
  end;
end;

procedure TCROCursorHandle.Loaded;
begin
  inherited;
end;

function TCROCursorHandle.Insert: Boolean;
var obj: TROComplexType; AOperation: TCROCustomOperation;
begin
  Result := False;
  if not (Assigned(FOperation) and (FOperation.FConnected or (FOperation.Action <> oaSelect)))
  then Exit;

  case FState of
    csEdit: Cancel;
    csInsert: Exit;
  end;

  if Assigned(ListHandle)
  then Result := ListHandle.Insert
  else
  begin
    obj := CreateROObject(FHandleDataType, SourceOperation.fRODLLibrary); //aloca memoria para o novo objeto
    Data := obj;

    AOperation := nil;
    if Assigned(FServiceManager)
    then AOperation := FServiceManager.FOperationForCreate
    else
    if Assigned(FOperation) and (FOperation.Action = oaCreate)
    then AOperation := FOperation;

    if Assigned(AOperation)
    then AOperation.Execute; //este serviço deverá setar o novo valor para o Data do cursor handle - setData do cursor handle trata descarga de referencias perdidas

    if Trim(FIdentifierExpression) <> '' then
    begin
      FGeneratingIdentifier := True;
      try
        try
          SetDataProperty(FIdentifierExpression,GenerateIdentifier);//*
        except
          MessageDlg('Invalid IdentifierExpression!',mtError,[mbOk],0);
        end;
      finally
        FGeneratingIdentifier := False;
      end;
    end;

    Result := True;
  end;

  SetState(csInsert);
end;

procedure TCROCursorHandle.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = ListHandle
    then ListHandle := nil;
  end;
end;

procedure TCROCursorHandle.SetData(Value: TROComplexType);
var ABuffer: TObject; 
begin
  inherited;
  if Assigned(ListHandle) and
     Assigned(ListHandle.Data) and
     Assigned(Data) and
     (TROArray(ListHandle.Data).Count > ListHandle.ItemIndex) then
  begin
    ABuffer := ListHandle.Items[ListHandle.ItemIndex]; //cria referencia para Data antigo
    if Assigned(ABuffer) and (ABuffer <> Data) then
    begin
      //atualiza o listhandle e destroy a referencia antiga
      ListHandle.Items[ListHandle.ItemIndex] := Data;
      ListHandle.UpdateCursorHandle;//remove referencias ao objeto nos CursorHandles
      FreeAndNil(ABuffer); //destroy Data antigo
    end;
  end;
  UpdateProperties;
end;

procedure TCROCursorHandle.SetDataProperty(Expression: String; Value: TObject);
var
  Instance: TObject;
  PropInfo: PPropInfo;
  AExpression: String;
begin
  if FState in [csEdit,csInsert] then //só permite alterar o valor do objeto se for edit ou insert mode
  begin
    Instance := Self.Data;
    AExpression := Expression;

    // Call the function to get the instance, pointer and propinfo
    GetPropInfoFromExpression(GetTrueOwner(Self.Owner,AExpression), AExpression, Instance, PropInfo);

    // Verify if the property is an object
    if PropInfo^.PropType^.Kind = tkClass then
    begin
      // Set the property
      SetObjectProp(Instance, PropInfo^.Name, Value);
      FHandleUpdateTime := Now;
    end
    else
    begin
      MessageDlg(Name+'SetDataProperty(Obj) - The Property is not from the expected type.', mtError, [mbOk], 0);
      Exit;
    end;

    if Assigned(Self.ListHandle)
    then Self.ListHandle.UpdateCursorHandle
    else UpdateProperties;
  end;  
end;

procedure TCROCursorHandle.SetDataProperty(Expression: String; Value: Variant);
var
  Instance: TObject;
  PropInfo: PPropInfo;
  AExpression: String;
begin
  if (FState in [csEdit,csInsert]) or (FGeneratingIdentifier) then //só permite alterar o valor do objeto se for edit ou insert mode
  begin
    Instance := Self.Data;
    AExpression := Expression;

    // Call the function to get the instance, pointer and propinfo
    GetPropInfoFromExpression(GetTrueOwner(Self.Owner,AExpression), AExpression, Instance, PropInfo);

    if PropInfo^.PropType^.Kind in [tkInteger, tkChar, tkFloat,
              tkString, tkWChar, tkLString, tkWString, tkVariant, tkInt64, tkEnumeration] then
    begin
      SetPropValue(Instance, PropInfo^.Name, Value);
      FHandleUpdateTime := Now;
    end
    else
    begin
      MessageDlg(Name+'SetDataProperty - The Property is not from the expected type.', mtError, [mbOk], 0);
      Exit;
    end;

    if Assigned(Self.ListHandle)
    then Self.ListHandle.UpdateCursorHandle
    else UpdateProperties;
  end;
end;

procedure TCROCursorHandle.SetListHandle(Value: TCROListHandle);
          procedure SetDataType;
          var Lib: TRODLLibrary; Typ: TRODLArray;
          begin
            //get DataType for Self.Data
            if Assigned(FOperation) and
               Assigned(FListHandle) and
               Assigned(FOperation.fRORemoteService) and
               (FListHandle.FHandleDataType <> '') then
            begin
              try
                Lib := FOperation.fRORemoteService.GetRODLLibrary;
              except
                Lib := nil;//could not attach to server
              end;

              if Assigned(Lib) then
              begin
                Typ := FindROArray(Lib,FListHandle.FHandleDataType);
                if Assigned(Typ)
                then FHandleDataType := Typ.ElementType;
              end;
            end;
          end;
begin
  //clear cursor data
  FListHandle := Value;

  FOperation := nil;
  FHandleDataType := '';
  //load cursor data
  if Assigned(FListHandle) then
  begin
    if not Assigned(FOperation) then
    begin
      FOperation := FListHandle.SourceOperation;
      if Assigned(FOperation)
      then FOperation.FreeNotification(Self);
    end;
    FListHandle.AddCursorHandle(Self);
    FListHandle.FreeNotification(Self);//Ensures that Self is notified FListHandle is going to be destroyed.
    //get datatype of list item
    SetDataType; //**
  end;
end;

procedure TCROCursorHandle.UpdateDataType;
begin
  if Assigned(FListHandle)
  then Self.SetListHandle(FListHandle);
end;

procedure TCROCursorHandle.Cancel;
begin
  if Assigned(ListHandle)
  then ListHandle.Cancel
  else SetState(csCancel);

  UpdateProperties;
end;

procedure TCROCursorHandle.Edit;
begin
  if (FState <> csInsert) then
  begin
    if (not Assigned(ListHandle)) and (not Assigned(Data))
    then Insert
    else SetState(csEdit);
  end;
end;

procedure TCROCursorHandle.UpdateProperties;
begin
  inherited;
  if Assigned(FListHandle)
  then FListHandle.NotifyIndexUpdate;
end;

procedure TCROCursorHandle.SetState(const Value: TCursorState);
begin
  if FState <> Value then
  begin
    FState := Value;

    if Assigned(ListHandle) then
    begin
      if ListHandle.FState <> Value
      then ListHandle.SetState(Value);
    end;    //naum coloque um Exit dentro deste if... causará um erro dificil de se perceber no setdataindex

    case FState of
      csEdit:
      begin
        FBuffer := CreateROObject(FHandleDataType,SourceOperation.fRODLLibrary);
        if Assigned(Data)
        then AssignROObject(Data,FBuffer);//FBuffer.Assign(Data);
      end;
      csCancel:
      begin
        if Assigned(FBuffer)
        then AssignROObject(FBuffer,Data);//Data.Assign(FBuffer);
        SetState(csBrowse);
      end;
      csBrowse:
      begin
        if Assigned(FBuffer)
        then FreeAndNil(FBuffer);
      end;
    end;

//    if (FState = csBrowse)
//    then UpdateProperties; //causes access violation when deletes an unique item just inserted

    if Assigned(FOnStateChange)
    then FOnStateChange(Self,FState);
  end;
end;

procedure TCROCursorHandle.SetAutoEdit(const Value: Boolean);
begin
  FAutoEdit := Value;
  if Assigned(ListHandle) and (ListHandle.AutoEdit <> FAutoEdit)
  then ListHandle.AutoEdit := FAutoEdit;
end;

procedure TCROCursorHandle.FixExpression;
begin
  inherited;
  if Assigned(FListHandle)
  then FListHandle.FixExpression;
end;

procedure TCROCursorHandle.Clear;
begin
  inherited;
  Delete;
end;

{ TCROCustomProperties }

constructor TCROCustomProperties.Create(AOwner: TComponent);
begin
  FOwner := AOwner;
end;

procedure TCROCustomProperties.GetValueFromExpression(var AResult: Variant);
begin
  AResult := Self.CursorHandle.GetDataProperty(Self.Expression);
end;

destructor TCROCustomProperties.Destroy;
begin
  FOnInternalUpdateExpression := nil;
  FOnInternalUpdateHandle := nil;
  CursorHandle := nil;//naum troquei por FHandle SetCursorHandle executa ações necessárias... mesmo se FHandle for um ListHandle
  inherited;
end;

procedure TCROCustomProperties.GetValueFromExpression(var AResult: TObject);
begin
  AResult := Self.CursorHandle.GetDataPropertyAsObject(Self.Expression);
end;

procedure TCROCustomProperties.SetExpression(Value: String);

          procedure SetDataType;

                    function GetDataTypeFromStruct(Lib: TRODLLibrary;
                      StructName: String; PropName: String): String;
                    var i: Integer;
                        Struct: TRODLStruct;
                    begin
                      Result := '';

                      Struct := Lib.FindStruct(StructName);

                      repeat
                        for i := 0 to Struct.Count-1 do
                        begin
                          if AnsiUpperCase(Struct.Items[i].Info.Name) = AnsiUpperCase(PropName) then
                          begin
                            Result := Struct.Items[i].Info.DataType;
                            Break;
                          end;
                        end;
                        Struct := Lib.FindStruct(Struct.GetAncestor);//nil when no ancestor found
                      until (Struct = nil);
                    end;

          var Lib: TRODLLibrary;

              StructName  : String;
              PropName    : String;
              cExpression : String;

              HandleArray: TRODLArray;
          begin
            FPropertiesDataType := '';
            if Assigned(FHandle) and
               Assigned(FHandle.FOperation) and
               (FHandle.FOperation.FAttached) and
               (FHandle.FHandleDataType <> '') then
            begin
              Lib := FHandle.FOperation.fRODLLibrary;

              if Assigned(Lib) then
              begin
                cExpression := FExpression;

                FPropertiesDataType   := FHandle.FHandleDataType;

                // Try to get the struct as an array
                HandleArray := FindROArray(Lib, FPropertiesDataType);

                // If got the array, get the data type of the element
                if Assigned(HandleArray)
                then FPropertiesDataType := HandleArray.ElementType;

                while cExpression <> '' do
                begin
                  StructName := FPropertiesDataType;
                  PropName   := GetNextIdentifier(cExpression);
                  FPropertiesDataType := GetDataTypeFromStruct(Lib, StructName, PropName);
                end;
              end;

              if (FExpression <> '') and (FPropertiesDataType = '') then
              begin
                MessageDlg('Could not find expression data type!', mtError, [mbOk], 0);
                Exit;
              end;
            end;
          end;
begin
  FExpression := Value;
  SetDataType;
  if Assigned(FOnInternalUpdateExpression)
  then FOnInternalUpdateExpression(Self);
end;

procedure TCROCustomProperties.SetValueToExpression(const Value: Variant);
begin
  Self.CursorHandle.SetDataProperty(Self.Expression,Value)
end;

procedure TCROCustomProperties.SetValueToExpression(Value: TObject; ClassName: String = '');
var Struct: TRODLStruct;
    i: Integer;
    PropInfo: PPropInfo;
    obj: TObject;
    v :Variant;
begin
  if Expression <> '' then
  begin
    Self.CursorHandle.SetDataProperty(Self.Expression,Value);
  end
  else
  begin
    Struct := CursorHandle.SourceOperation.fRODLLibrary.FindStruct(ClassName);

    while (Assigned(Struct)) and (Struct.Ancestor <> ClassName) and (Struct.Ancestor <> '')
    do Struct := CursorHandle.SourceOperation.fRODLLibrary.FindStruct(Struct.Ancestor);

    if Assigned(Struct) then
    begin
      for i := 0 to Struct.Count - 1 do
      begin
        // Get the information of the property
        PropInfo := GetPropInfo(Value, Struct.Items[i].Info.Name);

        if PropInfo^.PropType^.Kind in [tkInteger, tkChar, tkFloat,
              tkString, tkWChar, tkLString, tkWString, tkVariant, tkInt64, tkEnumeration] then
        begin
          // Set the property
          V := GetPropValue(Value, PropInfo^.Name);
          CursorHandle.SetDataProperty(Struct.Items[i].Info.Name,V);
        end
        else if PropInfo^.PropType^.Kind = tkClass then
        begin
          // Set the property
          obj := GetObjectProp(Value, PropInfo^.Name);
          Self.CursorHandle.SetDataProperty(Struct.Items[i].Info.Name,Obj);
        end;
      end;
    end;
  end;
end;

procedure TCROCustomProperties.SetOnInternalUpdateExpression(
  const Value: TNotifyEvent);
begin
  FOnInternalUpdateExpression := Value;
end;

procedure TCROCustomProperties.SetCursorHandle(Value: TCROCursorHandle);
begin
  if (FHandle <> nil) and (Value = nil) then
  begin
    FHandle.DeleteProperties(Self);
  end;

  FHandle := Value;

  //insert property object in cursor handle property list
  //in order to know which objects refers to it, just if the property is updated
  if Assigned(FHandle) then
  begin
    FHandle.InsertProperties(Self);
  end;

  FixDataType;

  if Assigned(FOnInternalUpdateHandle)
  then FOnInternalUpdateHandle(Self);
end;

procedure TCROCustomProperties.SetListHandle(Value: TCROListHandle);
begin
  if Assigned(FHandle) and not(Assigned(Value))
  then FHandle.DeleteProperties(Self);

  FHandle := Value;

  //insert property object in cursor handle property list
  //in order to know which objects refers to it, just if the property is updated
  if Assigned(FHandle) then
  begin
    FHandle.InsertProperties(Self);
  end;

  FixDataType;

  if Assigned(FOnInternalUpdateHandle)
  then FOnInternalUpdateHandle(Self);
end;

function TCROCustomProperties.GetCursorHandle: TCROCursorHandle;
begin
  Result := nil;
  if Assigned(FHandle) and not(csDestroying in FHandle.ComponentState) then
  begin
    if FHandle.InheritsFrom(TCROListHandle)
    then Result := TCROListHandle(FHandle).InternalCursorHandle
    else if FHandle.InheritsFrom(TCROCursorHandle)
    then Result := TCROCursorHandle(FHandle);
  end;
end;

function TCROCustomProperties.GetListHandle: TCROListHandle;
begin
  Result := nil;
  if Assigned(FHandle) and not(csDestroying in FHandle.ComponentState) then
  begin
    if FHandle.InheritsFrom(TCROListHandle)
    then Result := TCROListHandle(FHandle);
  end;
end;

procedure TCROCustomProperties.SetOnInternalUpdateHandle(
  const Value: TNotifyEvent);
begin
  FOnInternalUpdateHandle := Value;
end;

procedure TCROCustomProperties.SetFontProperties(const Value: TCROFontStyleProperties);
begin
  if (FFontProperties <> Value) then
  begin
    if Assigned(FFontProperties) then
    begin
      FFontProperties.RemoveControl(TControl(Self.FOwner));
      FFontProperties.RemoveFreeNotification(TControl(Self.FOwner));
    end
    else
    if Assigned(Value) then
    begin
      Value.AddControl(TControl(Self.FOwner));
      Value.FreeNotification(TControl(Self.FOwner));
    end;
    FFontProperties := Value;
    if Assigned(FFontProperties)
    then FFontProperties.DoChange(FFontProperties);
  end;
end;

procedure TCROCustomProperties.FixDataType;
begin
  SetExpression(FExpression);
end;

procedure TCROCustomProperties.SetGridProperties(
  const Value: TCROGridStyleProperties);
begin
  if (FGridProperties <> Value) then
  begin
    if Assigned(FGridProperties) then
    begin
      FGridProperties.RemoveControl(TControl(Self.FOwner));
      FGridProperties.RemoveFreeNotification(TControl(Self.FOwner));
    end
    else
    if Assigned(Value) then
    begin
      Value.AddControl(TControl(Self.FOwner));
      Value.FreeNotification(TControl(Self.FOwner));
    end;
    FGridProperties := Value;
    if Assigned(FGridProperties)
    then FGridProperties.DoChange(FGridProperties);
  end;
end;

function TCROCustomProperties.HandleAssigned: Boolean;
begin
  Result := Assigned(FHandle);
end;

{ TCROProperties }

procedure TCROProperties.UpdateValue;
begin
  //evita atualizações desnecessárias
  if FPropertiesUpdateTime <= FHandle.FHandleUpdateTime then
  begin
    FPropertiesUpdateTime := FHandle.FHandleUpdateTime;
    (Self.FOwner as ICROCustomControl).UpdateValue;
  end;
end;

{ TCROLookupProperties }

procedure TCROLookupProperties.SetIdentifierExpression(Value: String);
begin
  FIdentifierExpression := Value;
end;

procedure TCROLookupProperties.UpdateValue;
begin
  if FPropertiesUpdateTime <= FHandle.FHandleUpdateTime then
  begin
    FPropertiesUpdateTime := FHandle.FHandleUpdateTime;
    (Self.FOwner as ICROCustomListControl).UpdateItems;
  end;
end;

{ TCROGridProperties }

procedure TCROGridProperties.NotifyIndexUpdate;
begin
  (Self.FOwner as ICROCustomListControl).UpdateIndex;
end;

procedure TCROGridProperties.UpdateValue;
begin
  if (FPropertiesUpdateTime <= FHandle.FHandleUpdateTime) then
  begin
    FPropertiesUpdateTime := FHandle.FHandleUpdateTime;
    (Self.FOwner as ICROCustomListControl).UpdateItems;
  end;
end;

{ TCROServiceOperation }

constructor TCROServiceOperation.Create(AOwner: TComponent);
begin
  inherited;
  FRORemoteService := TRORemoteService.Create(Self);
  FRORemoteService.SetSubComponent(True);
  FRORemoteService.Name := 'RemoteService';
  FROChannel := TROWinInetHTTPChannel.Create(Self);
  FROChannel.SetSubComponent(True);
  FROChannel.Name := 'Channel';
  FROMessage := TROBinMessage.Create(Self);
  FROMessage.SetSubComponent(True);
  FROMessage.Name := 'Message';
  FRORemoteService.Channel := FROChannel;
  FRORemoteService.Message := FROMessage;
end;

destructor TCROServiceOperation.Destroy;
begin
  FRORemoteService.Free;
  FROChannel.Free;
  FROMessage.Free;
  inherited;
end;

function TCROServiceOperation.GetServiceName: String;
begin
  if Assigned(FRORemoteService)
  then Result := FRORemoteService.ServiceName
  else Result := '';
end;

procedure TCROServiceOperation.SetServiceName(const Value: String);
begin
  if Assigned(FRORemoteService) then
  begin
    if FRORemoteService.ServiceName <> Value
    then OperationName := '';
    FRORemoteService.ServiceName := Value;
    if Trim(Value) <> ''
    then SetRORemoteService(FRORemoteService);
  end;
end;

{ TCROServiceManager }

constructor TCROServiceManager.Create(AOwner: TComponent);
begin
  inherited;
  FOperationForSelect := TCROServiceOperation.Create(Self);
  FOperationForInsert := TCROServiceOperation.Create(Self);
  FOperationForUpdate := TCROServiceOperation.Create(Self);
  FOperationForDelete := TCROServiceOperation.Create(Self);
  FOperationForCreate := TCROServiceOperation.Create(Self);
  FOperationForSelect.Name := 'OperationForSelect';
  FOperationForInsert.Name := 'OperationForInsert';
  FOperationForUpdate.Name := 'OperationForUpdate';
  FOperationForDelete.Name := 'OperationForDelete';
  FOperationForCreate.Name := 'OperationForCreate';
  FOperationForSelect.Action := oaSelect;
  FOperationForInsert.Action := oaInsert;
  FOperationForUpdate.Action := oaUpdate;
  FOperationForDelete.Action := oaDelete;
  FOperationForCreate.Action := oaCreate;
  FOperationForSelect.SetSubComponent(True);
  FOperationForInsert.SetSubComponent(True);
  FOperationForUpdate.SetSubComponent(True);
  FOperationForDelete.SetSubComponent(True);
  FOperationForCreate.SetSubComponent(True);
end;

destructor TCROServiceManager.Destroy;
begin
  FOperationForSelect.Free;
  FOperationForInsert.Free;
  FOperationForUpdate.Free;
  FOperationForDelete.Free;
  FOperationForCreate.Free;
  inherited;
end;

{ TCROActionProperties }

procedure TCROActionProperties.NotifyIndexUpdate;
begin
  UpdateValue;
end;

procedure TCROActionProperties.UpdateValue;
begin
  //evita atualizações desnecessárias
  if FPropertiesUpdateTime <= FHandle.FHandleUpdateTime then
  begin
    FPropertiesUpdateTime := FHandle.FHandleUpdateTime;
    (Self.FOwner as ICROCustomControl).UpdateValue;
  end;
end;

end.




