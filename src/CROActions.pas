unit CROActions;

interface

uses
  Forms, SysUtils, Classes, ActnList, CROOperation, uROTypes, CROComponentsCommon,
  Controls, CROResourceContainer, Dialogs, CROListBox, ExtCtrls;

type

  (* Please refers to file CROPatterns.txt to know more about CROActions usage *)

  TActionItemExecuteTime = (ecBeforeExecute,ecAfterExecute);
  TActionExecuteResult = (erNone,erOk,erCancel,erAbort,erRetry,erIgnore,erYes,erNo,erAll,erNoToAll,erYesToAll);
  TActionExecuteResults = set of TActionExecuteResult;

  { CRO Action Item}
  TCROActionItem  = class(TCollectionItem)
  private
    FAction: TCustomAction;
    FExecuteCondiction: TActionExecuteResults;
    FExecuteTime: TActionItemExecuteTime;
    procedure SetAction(const Value: TCustomAction);
  protected
    function GetDisplayName: String; override;
  published
    property Action: TCustomAction read FAction write SetAction;
    property ExecuteTime: TActionItemExecuteTime read FExecuteTime write FExecuteTime;
    property ExecuteCondiction: TActionExecuteResults read FExecuteCondiction write FExecuteCondiction; //used only for ecAfterExecute
  end;

  { CRO Action Item Collecion}
  TCROActionItems = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TCROActionItem;
    procedure SetItem(Index: Integer; Value: TCROActionItem);
  public
    constructor Create(AOwner: TComponent); overload;
    function FindAction(AAction: TCustomAction): TCROActionItem;
    property Items[Index: Integer]: TCROActionItem read GetItem write SetItem; default;
  end;

  TCROExecuteEvent = procedure (Sender: TObject; var Result: TModalResult) of object;

  { CROCustomActions }
  TCROCustomAction = class(TCustomAction)
  private
    FActions: TCROActionItems;
    FExecuteResult: TActionExecuteResult;
    FOnExecute: TCROExecuteEvent;
    FResourceID: String;
    FResourceContainer: TCROResourceContainer;
    FConfirmExecute: Boolean;
    procedure SetActions(const Value: TCROActionItems);
    procedure SetResourceContainer(const Value: TCROResourceContainer);
    procedure SetResourceID(const Value: String);
  protected
    function ModalResultToActionExecuteResult(const mr:TModalResult):TActionExecuteResult;
    function ExecuteConfirmation: Boolean; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure ExecuteBackwardActions;
    procedure ExecuteAfterwardActions;
    function ExecuteMainAction: TActionExecuteResult; virtual;
    procedure ExecuteTarget(Target: TObject); override;
    function HandlesTarget(Target: TObject): Boolean; override;
    function Execute: Boolean; override;

    property ConfirmExecute: Boolean read FConfirmExecute write FConfirmExecute;
    property ResourceContainer: TCROResourceContainer read FResourceContainer write SetResourceContainer;
    property ResourceID: String read FResourceID write SetResourceID;
    property ExecuteResult: TActionExecuteResult read FExecuteResult;
    property OnExecute: TCROExecuteEvent read FOnExecute write FOnExecute;//hide and reintroduce inherited OnExecute

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Actions: TCROActionItems read FActions write SetActions;
    property AutoCheck;
    property Caption;
    property Checked;
    property GroupIndex;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property ImageIndex;
    property ShortCut;
    property SecondaryShortCuts;
    property Visible;
    property OnHint;
  end;

  { CROAction }
  TCROAction = class(TCROCustomAction)
  published
    property Enabled;
    property OnExecute;
    property OnUpdate;
  end;

  { CROFormClose Action}
  TCROFormClose = class(TCROCustomAction)
  public
    constructor Create(AOwner:TComponent); override;
    function ExecuteMainAction: TActionExecuteResult; override;
  end;

  { CROOperation Execute}
  TCROOperationExecute = class(TCROCustomAction)
  private
    FServiceOperation: TCROServiceOperation;
    procedure SetServiceOperation(const Value: TCROServiceOperation);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner:TComponent); override;
    function ExecuteMainAction: TActionExecuteResult; override;
    procedure UpdateTarget(Target: TObject); override;
    function HandlesTarget(Target: TObject): Boolean; override;
  published
    property ServiceOperation: TCROServiceOperation read FServiceOperation write SetServiceOperation;
  end;

  { CROServiceManager Action}
  TCROServiceManagerAction = class(TCROCustomAction)
  private
    FServiceManager: TCROServiceManager;
    procedure SetServiceManager(const Value: TCROServiceManager);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    function HandlesTarget(Target: TObject): Boolean; override;
  published
    property ServiceManager: TCROServiceManager read FServiceManager write SetServiceManager;
  end;

  { CROServiceManager Select}
  TCROServiceManagerSelect = class(TCROServiceManagerAction)
  public
    constructor Create(AOwner:TComponent); override;
    function ExecuteMainAction: TActionExecuteResult; override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  { CROServiceManager Insert}
  TCROServiceManagerInsert = class(TCROServiceManagerAction)
  public
    constructor Create(AOwner:TComponent); override;
    function ExecuteMainAction: TActionExecuteResult; override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  { CROServiceManager Delete}
  TCROServiceManagerDelete = class(TCROServiceManagerAction)
  public
    constructor Create(AOwner:TComponent); override;
    function ExecuteMainAction: TActionExecuteResult; override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  { CROServiceManager Update}
  TCROServiceManagerUpdate = class(TCROServiceManagerAction)
  public
    constructor Create(AOwner:TComponent); override;
    function ExecuteMainAction: TActionExecuteResult; override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  { CROHandle Actions }
  TCROHandleAction = class(TCROCustomAction)
  private
    FHandle: TCROHandle;
    procedure SetHandle(const Value: TCROHandle);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    function HandlesTarget(Target: TObject): Boolean; override;
  published
    property Handle: TCROHandle read FHandle write SetHandle;
  end;

  { CROListHandle Actions }
  TCROListHandleAction = class(TCROCustomAction)
  private
    FListHandle: TCROListHandle;
    procedure SetListHandle(const Value: TCROListHandle);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    function HandlesTarget(Target: TObject): Boolean; override;
  published
    property ListHandle: TCROListHandle read FListHandle write SetListHandle;
  end;

  { CROListHandle Next }
  TCROHandleNext = class(TCROListHandleAction)
  public
    constructor Create(AOwner:TComponent); override;
    function ExecuteMainAction: TActionExecuteResult; override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  { CROListHandle Prior }
  TCROHandlePrior = class(TCROListHandleAction)
  public
    constructor Create(AOwner:TComponent); override;
    function ExecuteMainAction: TActionExecuteResult; override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  { CROHandle Insert }
  TCROHandleInsert = class(TCROHandleAction)
  public
    constructor Create(AOwner:TComponent); override;
    function ExecuteMainAction: TActionExecuteResult; override;
    procedure UpdateTarget(Target: TObject); override;
  published
    property ConfirmExecute;
    property ResourceContainer;
    property ResourceID;
  end;

  { CROHandle Edit }
  TCROHandleEdit = class(TCROHandleAction)
  public
    constructor Create(AOwner:TComponent); override;
    function ExecuteMainAction: TActionExecuteResult; override;
    procedure UpdateTarget(Target: TObject); override;
  published
    property ConfirmExecute;
    property ResourceContainer;
    property ResourceID;
  end;

  { CROHandle Cancel }
  TCROHandleCancel = class(TCROHandleAction)
  public
    constructor Create(AOwner:TComponent); override;
    function ExecuteMainAction: TActionExecuteResult; override;
    procedure UpdateTarget(Target: TObject); override;
  published
    property ConfirmExecute;
    property ResourceContainer;
    property ResourceID;
  end;

  { CROHandle Delete }
  TCROHandleDelete = class(TCROHandleAction)
  public
    constructor Create(AOwner:TComponent); override;
    function ExecuteMainAction: TActionExecuteResult; override;
    procedure UpdateTarget(Target: TObject); override;
  published
    property ConfirmExecute;
    property ResourceContainer;
    property ResourceID;
  end;

  { CROHandle Post }
  TCROHandlePost = class(TCROHandleAction)
  public
    constructor Create(AOwner:TComponent); override;
    function ExecuteMainAction: TActionExecuteResult; override;
    procedure UpdateTarget(Target: TObject); override;
  published
    property ConfirmExecute;
    property ResourceContainer;
    property ResourceID;
  end;

  { CRONotebook Actions }
  TCRONotebookAction = class(TCROAction)
  private
    FNotebook: TNotebook;
    procedure SetNotebook(const Value: TNotebook);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property Notebook: TNotebook read FNotebook write SetNotebook;
    property ConfirmExecute;
    property ResourceContainer;
    property ResourceID;
  end;

  { CRONotebook Next }
  TCRONotebookNext = class(TCRONotebookAction)
  public
    constructor Create(AOwner:TComponent); override;
    function ExecuteMainAction: TActionExecuteResult; override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  { CRONotebook Prior }
  TCRONotebookPrior = class(TCRONotebookAction)
  public
    constructor Create(AOwner:TComponent); override;
    function ExecuteMainAction: TActionExecuteResult; override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  { CROListHandle HandleObject }
  TCROHandleHandleObject = class(TCROCustomAction)
  private
    FFromHandle: TCROListHandle;
    FToHandle: TCROListHandle;
    procedure SetFromHandle(const Value: TCROListHandle);
    procedure SetToHandle(const Value: TCROListHandle);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property FromHandle: TCROListHandle read FFromHandle write SetFromHandle;
    property ToHandle: TCROListHandle read FToHandle write SetToHandle;
  public
    procedure UpdateTarget(Target: TObject); override;
  end;

  { CROListHandle CopyObject }
  TCROHandleCopyObject = class(TCROHandleHandleObject)
  public
    constructor Create(AOwner:TComponent); override;
    function ExecuteMainAction: TActionExecuteResult; override;
  end;

  { CROListHandle MoveObject }
  TCROHandleMoveObject = class(TCROHandleHandleObject)
  public
    constructor Create(AOwner:TComponent); override;
    function ExecuteMainAction: TActionExecuteResult; override;
  end;

  { CROListBox HandleItem}
  TCROListBoxHandleItem = class(TCROCustomAction)
  private
    FFromListBox: TCROListBox;
    FToListBox: TCROListBox;
    procedure SetFromListBox(const Value: TCROListBox);
    procedure SetToListBox(const Value: TCROListBox);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property FromListBox: TCROListBox read FFromListBox write SetFromListBox;
    property ToListBox: TCROListBox read FToListBox write SetToListBox;
  public
    procedure UpdateTarget(Target: TObject); override;
  end;

  { CROListBox CopyItem }
  TCROListBoxCopyItem = class(TCROListBoxHandleItem)
  public
    constructor Create(AOwner:TComponent); override;
    function ExecuteMainAction: TActionExecuteResult; override;
  end;

  { CROListBox MoveItem }
  TCROListBoxMoveItem = class(TCROListBoxHandleItem)
  public
    constructor Create(AOwner:TComponent); override;
    function ExecuteMainAction: TActionExecuteResult; override;
  end;

  { CROHandle AssignObject }
  TAssingmentType = (atReference,atValue);

  TCROHandleAssignObject = class(TCROHandleHandleObject)
  private
    FAssingmentType: TAssingmentType;
  public
    constructor Create(AOwner:TComponent); override;
    function ExecuteMainAction: TActionExecuteResult; override;
    procedure UpdateTarget(Target: TObject); override;
  published
    property AssingmentType: TAssingmentType read FAssingmentType write FAssingmentType;
    property ConfirmExecute;
    property ResourceContainer;
    property ResourceID;
  end;


implementation

{ TCROHandleAction }

function TCROHandleAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := (Target <> nil) and (FHandle <> nil);
end;

{ TCROListHandleNext }

constructor TCROHandleNext.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Next';
end;

function TCROHandleNext.ExecuteMainAction: TActionExecuteResult;
begin
  Result := inherited ExecuteMainAction;
  if Assigned(FListHandle) and
     Assigned(TROArray(FListHandle.Data)) and
     (not FListHandle.EOF)
  then FListHandle.Next;
end;

procedure TCROHandleNext.UpdateTarget(Target: TObject);
begin
  Enabled := Assigned(FListHandle) and
             Assigned(FListHandle.Data) and
             (FListHandle.Count > 0) and 
             (not FListHandle.EOF);
end;

{ TCROListHandlePrior }

constructor TCROHandlePrior.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Prior';
end;

function TCROHandlePrior.ExecuteMainAction: TActionExecuteResult;
begin
  Result := inherited ExecuteMainAction;
  if Assigned(FListHandle) and
     Assigned(TROArray(FListHandle.Data)) and
     (not FListHandle.BOF)
  then FListHandle.Prior;
end;

procedure TCROHandlePrior.UpdateTarget(Target: TObject);
begin
  Enabled := Assigned(FListHandle) and
             Assigned(FListHandle.Data) and
             (FListHandle.Count > 0) and
             (not FListHandle.BOF);
end;

procedure TCROHandleAction.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FHandle)
  then FHandle := nil;
end;

procedure TCROHandleAction.SetHandle(const Value: TCROHandle);
begin
  FHandle := Value;
  if Assigned(FHandle)
  then FHandle.FreeNotification(Self);
end;

{ TCROListHandleAction }

function TCROListHandleAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := (Target <> nil) and (FListHandle <> nil);
end;

procedure TCROListHandleAction.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FListHandle)
  then FListHandle := nil;
end;

procedure TCROListHandleAction.SetListHandle(const Value: TCROListHandle);
begin
  FListHandle := Value;
  if Assigned(FListHandle)
  then FListHandle.FreeNotification(Self);
end;

{ TCROHandleInsert }

constructor TCROHandleInsert.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Insert';
end;

function TCROHandleInsert.ExecuteMainAction: TActionExecuteResult;
begin
  Result := inherited ExecuteMainAction;
  if Assigned(FHandle) and
     Assigned(TROArray(FHandle.Data)) and
     (not (FHandle.State in [csEdit,csInsert]))
  then FHandle.Insert;
end;

procedure TCROHandleInsert.UpdateTarget(Target: TObject);
begin
  Enabled := Assigned(FHandle) and
             Assigned(FHandle.Data) and
             (not (FHandle.State in [csEdit,csInsert]));
end;

{ TCROHandleCancel }

constructor TCROHandleCancel.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Cancel';
end;

function TCROHandleCancel.ExecuteMainAction: TActionExecuteResult;
begin
  Result := inherited ExecuteMainAction;
  if Assigned(FHandle) and
     Assigned(TROArray(FHandle.Data)) and
     (FHandle.State in [csEdit,csInsert])
  then FHandle.Cancel;
end;

procedure TCROHandleCancel.UpdateTarget(Target: TObject);
begin
  Enabled := Assigned(FHandle) and
             Assigned(FHandle.Data) and
             (FHandle.State in [csEdit,csInsert]);
end;

{ TCROHandleEdit }

constructor TCROHandleEdit.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Edit';
end;

function TCROHandleEdit.ExecuteMainAction: TActionExecuteResult;
begin
  Result := inherited ExecuteMainAction;
  if Assigned(FHandle) and
     Assigned(TROArray(FHandle.Data)) and
     (not (FHandle.State in [csEdit,csInsert]))
  then FHandle.Edit;
end;

procedure TCROHandleEdit.UpdateTarget(Target: TObject);
begin
  Enabled := Assigned(FHandle) and
             Assigned(FHandle.Data) and (
               (FHandle.InheritsFrom(TCROCursorHandle)) or (
                 (FHandle.InheritsFrom(TCROListHandle)) and
                 (TCROListHandle(FHandle).Count > 0)
               )
             ) and (
               not (FHandle.State in [csEdit,csInsert])
             );
end;

{ TCROHandleDelete }

constructor TCROHandleDelete.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Delete';
end;

function TCROHandleDelete.ExecuteMainAction: TActionExecuteResult;
begin
  Result := inherited ExecuteMainAction;
  if Assigned(FHandle) and
     Assigned(TROArray(FHandle.Data)) and
     (not (FHandle.State in [csEdit,csInsert]))
  then FHandle.Delete;
end;

procedure TCROHandleDelete.UpdateTarget(Target: TObject);
begin
  Enabled := Assigned(FHandle) and
             Assigned(FHandle.Data) and (
               (FHandle.InheritsFrom(TCROCursorHandle)) or (
                 (FHandle.InheritsFrom(TCROListHandle)) and
                 (TCROListHandle(FHandle).Count > 0)
               )
             ) and (
               not (FHandle.State in [csEdit,csInsert])
             );
end;

{ TCROHandlePost }

constructor TCROHandlePost.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Post';
end;

function TCROHandlePost.ExecuteMainAction: TActionExecuteResult;
begin
  Result := inherited ExecuteMainAction;
  if Assigned(FHandle) and//TODO colocar um try para mandar erNone se der erro 
     Assigned(TROArray(FHandle.Data)) and
     (FHandle.State in [csEdit,csInsert])
  then FHandle.Post;
end;

procedure TCROHandlePost.UpdateTarget(Target: TObject);
begin
  Enabled := Assigned(FHandle) and
             Assigned(FHandle.Data) and
             (FHandle.State in [csEdit,csInsert]);
end;

{ TCROActionItems }

constructor TCROActionItems.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TCROActionItem);
end;

function TCROActionItems.FindAction(
  AAction: TCustomAction): TCROActionItem;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
  begin
    if Items[i].Action = AAction then
    begin
      Result := Items[i];
      Break;
    end;
  end;
end;

function TCROActionItems.GetItem(Index: Integer): TCROActionItem;
begin
  Result := TCROActionItem(inherited Items[Index]);
end;

procedure TCROActionItems.SetItem(Index: Integer; Value: TCROActionItem);
begin
  inherited SetItem(Index, TCollectionItem(Value));
end;

{ TCROCustomAction }

constructor TCROCustomAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DisableIfNoHandler := True;
  FExecuteResult := erNone;
  FResourceID := '';
  FActions := TCROActionItems.Create(Self);
end;

destructor TCROCustomAction.Destroy;
begin
  FActions.Free;
  inherited;
end;

function TCROCustomAction.Execute: Boolean;
begin
  ExecuteTarget(nil);
  Result := True; 
end;

procedure TCROCustomAction.ExecuteBackwardActions;
var i:Integer;
begin
  for i := 0 to FActions.Count-1 do
  begin
    if Assigned(FActions[i].Action) then
    begin
      FActions[i].Action.Update;
      if (FActions[i].ExecuteTime = ecBeforeExecute) and
         (FActions[i].Action.Enabled)
      then FActions[i].Action.Execute;
    end;  
  end;
end;

procedure TCROCustomAction.ExecuteAfterwardActions;
var i:Integer;
begin
  for i := 0 to FActions.Count-1 do
  begin
    if (Assigned(FActions[i].Action)) then
    begin
      FActions[i].Action.Update;
      if (FActions[i].ExecuteTime = ecAfterExecute) and (
           (FExecuteResult in FActions[i].ExecuteCondiction) or
           (FActions[i].ExecuteCondiction = [])
         ) and (
           (FActions[i].Action.Enabled)
         ) then
      begin
        FActions[i].Action.Execute;
        if FActions[i].Action.InheritsFrom(TCROCustomAction)
        then FExecuteResult := TCROCustomAction(FActions[i].Action).ExecuteResult;
      end;
    end;
  end;
end;

function TCROCustomAction.ExecuteConfirmation: Boolean;
var AItem: TCROResourceItem;
begin
  Result := True;
  if Assigned(FResourceContainer) and
     (Trim(FResourceID) <> '') and 
     FConfirmExecute then
  begin
    AItem := TCROResourceItem(FResourceContainer.Items.FindMessage(FResourceID));
    if Assigned(AItem) and (Trim(AItem.MessageBody.Text) <> '') then
    begin
      if MessageDlg(AItem.MessageBody.Text,mtConfirmation,[mbYes,mbNo],0) = mrNo
      then Result := False;
    end;  
  end;
end;

function TCROCustomAction.ExecuteMainAction: TActionExecuteResult;
begin
  Result := erOk;
end;

procedure TCROCustomAction.ExecuteTarget(Target: TObject);
var mr: TModalResult;
begin
  FExecuteResult := erNone;
  if ExecuteConfirmation then  //does not inherits so does not call TComponent.ExecuteTarget and inherited OnExecute
  begin
    ExecuteBackwardActions;
    if Assigned(FOnExecute) then
    begin
      FOnExecute(Self,mr);
      FExecuteResult := ModalResultToActionExecuteResult(mr);
    end
    else
      FExecuteResult := ExecuteMainAction;
    ExecuteAfterwardActions;
  end;
end;

function TCROCustomAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := Target <> nil;
end;

function TCROCustomAction.ModalResultToActionExecuteResult(
  const mr: TModalResult): TActionExecuteResult;
begin
  case mr of
    mrOk       : Result := erOk;
    mrCancel   : Result := erCancel;
    mrAbort    : Result := erAbort;
    mrRetry    : Result := erRetry;
    mrIgnore   : Result := erIgnore;
    mrYes      : Result := erYes;
    mrNo       : Result := erNo;
    mrAll      : Result := erAll;
    mrNoToAll  : Result := erNoToAll;
    mrYesToAll : Result := erYesToAll;
    else Result := erNone;
  end;
end;

procedure TCROCustomAction.SetActions(const Value: TCROActionItems);
begin
  FActions.Assign(Value);
end;

procedure TCROCustomAction.Notification(AComponent: TComponent;
  Operation: TOperation);
var AItem: TCROActionItem;
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if (Assigned(AComponent)) and
       (Assigned(Actions)) and
       (not (csDestroying in ComponentState)) and
       (AComponent.InheritsFrom(TCustomAction)) then
    begin
      AItem := Actions.FindAction(TCustomAction(AComponent));
      if Assigned(AItem)
      then AItem.Action := nil;
    end
    else
    if (AComponent = FResourceContainer)
    then FResourceContainer := nil;
  end;
end;

procedure TCROCustomAction.SetResourceContainer(
  const Value: TCROResourceContainer);
begin
  FResourceContainer := Value;
  if Assigned(FResourceContainer)
  then FResourceContainer.FreeNotification(Self);
end;

procedure TCROCustomAction.SetResourceID(const Value: String);
begin
  if (Trim(Value) = '')
  then FResourceID := ''
  else
  begin
    if (Value <> FResourceID) then
    begin
      if Assigned(FResourceContainer) then
      begin
        if Assigned(FResourceContainer.Items.FindMessage(Value))
        then FResourceID := Value
        else
        begin
          if not Assigned(FResourceContainer.Items.FindMessage(FResourceID))
          then FResourceID := '';
          raise Exception.Create('Invalid Resource ID!');
        end;
      end
      else
        FResourceID := Value;
    end;
  end;
end;

{ TCROOperationExecute }

constructor TCROOperationExecute.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Execute';
end;

function TCROOperationExecute.ExecuteMainAction: TActionExecuteResult;
begin
  Result := inherited ExecuteMainAction;
  if Assigned(FServiceOperation)
  then FServiceOperation.Execute;
end;

function TCROOperationExecute.HandlesTarget(Target: TObject): Boolean;
begin
  Result := (Target <> nil) and (FServiceOperation <> nil);
end;

procedure TCROOperationExecute.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FServiceOperation)
  then FServiceOperation := nil;
end;

procedure TCROOperationExecute.SetServiceOperation(
  const Value: TCROServiceOperation);
begin
  FServiceOperation := Value;
  if Assigned(FServiceOperation)
  then FServiceOperation.FreeNotification(Self);
end;

procedure TCROOperationExecute.UpdateTarget(Target: TObject);
begin
  Enabled := Assigned(FServiceOperation) and FServiceOperation.Attached;
end;

{ TCROServiceManagerSelect }

constructor TCROServiceManagerSelect.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Execute';
end;

function TCROServiceManagerSelect.ExecuteMainAction: TActionExecuteResult;
begin
  Result := inherited ExecuteMainAction;
  if Assigned(FServiceManager) and
     Assigned(FServiceManager.OperationForSelect) and
     (FServiceManager.OperationForSelect.Attached)
  then FServiceManager.OperationForSelect.Execute;
end;

procedure TCROServiceManagerSelect.UpdateTarget(Target: TObject);
begin
  Enabled := Assigned(FServiceManager) and
             Assigned(FServiceManager.OperationForSelect) and
             (FServiceManager.OperationForSelect.Attached);
end;

{ TCROServiceManagerAction }

function TCROServiceManagerAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := (Target <> nil) and (FServiceManager <> nil);
end;

procedure TCROServiceManagerAction.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FServiceManager)
  then FServiceManager := nil;
end;

procedure TCROServiceManagerAction.SetServiceManager(
  const Value: TCROServiceManager);
begin
  FServiceManager := Value;
  if Assigned(FServiceManager)
  then FServiceManager.FreeNotification(Self);
end;

{ TCROServiceManagerInsert }

constructor TCROServiceManagerInsert.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Execute';
end;

function TCROServiceManagerInsert.ExecuteMainAction: TActionExecuteResult;
begin
  Result := inherited ExecuteMainAction;
  if Assigned(FServiceManager) and
     Assigned(FServiceManager.OperationForInsert) and
     (FServiceManager.OperationForInsert.Attached)
  then FServiceManager.OperationForInsert.Execute;
end;

procedure TCROServiceManagerInsert.UpdateTarget(Target: TObject);
begin
  Enabled := Assigned(FServiceManager) and
             Assigned(FServiceManager.OperationForInsert) and
             (FServiceManager.OperationForInsert.Attached);
end;

{ TCROServiceManagerDelete }

constructor TCROServiceManagerDelete.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Execute';
end;

function TCROServiceManagerDelete.ExecuteMainAction: TActionExecuteResult;
begin
  Result := inherited ExecuteMainAction;
  if Assigned(FServiceManager) and
     Assigned(FServiceManager.OperationForDelete) and
     (FServiceManager.OperationForDelete.Attached)
  then FServiceManager.OperationForDelete.Execute;
end;

procedure TCROServiceManagerDelete.UpdateTarget(Target: TObject);
begin
  Enabled := Assigned(FServiceManager) and
             Assigned(FServiceManager.OperationForDelete) and
             (FServiceManager.OperationForDelete.Attached);
end;

{ TCROServiceManagerUpdate }

constructor TCROServiceManagerUpdate.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Execute';
end;

function TCROServiceManagerUpdate.ExecuteMainAction: TActionExecuteResult;
begin
  Result := inherited ExecuteMainAction;
  if Assigned(FServiceManager) and
     Assigned(FServiceManager.OperationForUpdate) and
     (FServiceManager.OperationForUpdate.Attached)
  then FServiceManager.OperationForUpdate.Execute;
end;

procedure TCROServiceManagerUpdate.UpdateTarget(Target: TObject);
begin
  Enabled := Assigned(FServiceManager) and
             Assigned(FServiceManager.OperationForUpdate) and
             (FServiceManager.OperationForUpdate.Attached);
end;

{ TCROActionItem }

function TCROActionItem.GetDisplayName: String;
begin
  if Assigned(FAction)
  then Result := FAction.Name
  else Result := 'Unassigned';
end;

procedure TCROActionItem.SetAction(const Value: TCustomAction);
begin
  FAction := Value;
  if Assigned(FAction)
  then FAction.FreeNotification(TComponent(Collection.Owner));
end;

{ TCROFormClose }

constructor TCROFormClose.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Close';
end;

function TCROFormClose.ExecuteMainAction: TActionExecuteResult;
begin
  Result := inherited ExecuteMainAction;
  if Assigned(ActionList) and ActionList.Owner.InheritsFrom(TForm)
  then TForm(ActionList.Owner).Close;
end;

{ TCRONotebookNext }

constructor TCRONotebookNext.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Next';
end;

function TCRONotebookNext.ExecuteMainAction: TActionExecuteResult;
begin
  Result := inherited ExecuteMainAction;
  if Assigned(FNotebook) and (FNotebook.PageIndex < FNotebook.Pages.Count)
  then FNotebook.PageIndex := FNotebook.PageIndex + 1;
end;

procedure TCRONotebookNext.UpdateTarget(Target: TObject);
begin
  Enabled := Assigned(FNotebook) and (FNotebook.PageIndex < FNotebook.Pages.Count);
end;

{ TCRONotebookPrior }

constructor TCRONotebookPrior.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Prior';
end;

function TCRONotebookPrior.ExecuteMainAction: TActionExecuteResult;
begin
  Result := inherited ExecuteMainAction;
  if Assigned(FNotebook) and (FNotebook.PageIndex > 0)
  then FNotebook.PageIndex := FNotebook.PageIndex - 1;
end;

procedure TCRONotebookPrior.UpdateTarget(Target: TObject);
begin
  Enabled := Assigned(FNotebook) and (FNotebook.PageIndex > 0);
end;

{ TCROHandleMoveObject }

constructor TCROHandleMoveObject.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Move';
end;

function TCROHandleMoveObject.ExecuteMainAction: TActionExecuteResult;
begin
  Result := inherited ExecuteMainAction;
  if Assigned(FFromHandle) and
     Assigned(FToHandle) and
     Assigned(FFromHandle.Data) and
     Assigned(FToHandle.Data) and
     (FFromHandle.Count > 0)
  then FFromHandle.MoveObject(FFromHandle.ItemIndex,FToHandle);
end;

{ TCROHandleCopyObject }

constructor TCROHandleCopyObject.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Copy';
end;

function TCROHandleCopyObject.ExecuteMainAction: TActionExecuteResult;
begin
  Result := inherited ExecuteMainAction;
  if Assigned(FFromHandle) and
     Assigned(FToHandle) and
     Assigned(FFromHandle.Data) and
     Assigned(FToHandle.Data) and
     (FFromHandle.Count > 0)
  then FFromHandle.CopyObject(FFromHandle.ItemIndex,FToHandle);
end;

{ TCROHandleHandleObject }

procedure TCROHandleHandleObject.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if (AComponent = FFromHandle)
    then FFromHandle := nil
    else
    if (AComponent = FToHandle)
    then FToHandle := nil
  end;
end;

procedure TCROHandleHandleObject.SetFromHandle(
  const Value: TCROListHandle);
begin
  FFromHandle := Value;
  if Assigned(FFromHandle)
  then FFromHandle.FreeNotification(Self);
end;

procedure TCROHandleHandleObject.SetToHandle(const Value: TCROListHandle);
begin
  FToHandle := Value;
  if Assigned(FToHandle)
  then FToHandle.FreeNotification(Self);
end;

procedure TCROHandleHandleObject.UpdateTarget(Target: TObject);
begin
  Enabled := Assigned(FFromHandle) and
             Assigned(FToHandle) and
             Assigned(FFromHandle.Data) and
             Assigned(FToHandle.Data) and
             (FFromHandle.Count > 0);
end;

{ TCROListBoxHandleItem }

procedure TCROListBoxHandleItem.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if (AComponent = FFromListBox)
    then FFromListBox := nil
    else
    if (AComponent = FToListBox)
    then FToListBox := nil
  end;
end;

procedure TCROListBoxHandleItem.SetFromListBox(const Value: TCROListBox);
begin
  FFromListBox := Value;
  if Assigned(FFromListBox)
  then FFromListBox.FreeNotification(Self);
end;

procedure TCROListBoxHandleItem.SetToListBox(const Value: TCROListBox);
begin
  FToListBox := Value;
  if Assigned(FToListBox)
  then FToListBox.FreeNotification(Self);
end;

procedure TCROListBoxHandleItem.UpdateTarget(Target: TObject);
begin
  Enabled := Assigned(FFromListBox) and
             Assigned(FToListBox) and
             (FFromListBox.Count > 0);
end;

{ TCROListBoxMoveItem }

constructor TCROListBoxMoveItem.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Move';
end;

function TCROListBoxMoveItem.ExecuteMainAction: TActionExecuteResult;
begin
  Result := inherited ExecuteMainAction;
  if Assigned(FFromListBox) and
     Assigned(FToListBox) and
     (FFromListBox.Count > 0)
  then FFromListBox.MoveItem(FFromListBox.ItemIndex,FToListBox);
end;

{ TCROListBoxCopyItem }

constructor TCROListBoxCopyItem.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Copy';
end;

function TCROListBoxCopyItem.ExecuteMainAction: TActionExecuteResult;
begin
  Result := inherited ExecuteMainAction;
  if Assigned(FFromListBox) and
     Assigned(FToListBox) and
     (FFromListBox.Count > 0)
  then FFromListBox.CopyItem(FFromListBox.ItemIndex,FToListBox);
end;

{ TCROHandleAssignObject }

constructor TCROHandleAssignObject.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Assign';
end;

function TCROHandleAssignObject.ExecuteMainAction: TActionExecuteResult;
var AData: TObject;
begin
  Result := inherited ExecuteMainAction;
  Update;
  if Enabled then
  begin
    case FAssingmentType of
      atValue:
      begin
        if FFromHandle.ClassType = FToHandle.ClassType
        then AssignROObject(FFromHandle.Data,FToHandle.Data)
        else
        if FFromHandle.InheritsFrom(TCROCursorHandle) and
           FToHandle.InheritsFrom(TCROListHandle) then
        begin
          AData := TCROListHandle(FToHandle).Items[TCROListHandle(FToHandle).ItemIndex];
          AssignROObject(FFromHandle.Data,AData);
        end
        else
        if FFromHandle.InheritsFrom(TCROListHandle) and
           FToHandle.InheritsFrom(TCROCursorHandle) then
        begin
          AData := TCROListHandle(FFromHandle).Items[TCROListHandle(FFromHandle).ItemIndex];
          AssignROObject(AData,FToHandle.Data);
        end;
      end;
      atReference:
      begin
        if FFromHandle.ClassType = FToHandle.ClassType
        then FToHandle.Data := FFromHandle.Data
        else
        if FFromHandle.InheritsFrom(TCROCursorHandle) and
           FToHandle.InheritsFrom(TCROListHandle)
        then TCROListHandle(FToHandle).Items[TCROListHandle(FToHandle).ItemIndex] := FFromHandle.Data
        else
        if FFromHandle.InheritsFrom(TCROListHandle) and
           FToHandle.InheritsFrom(TCROCursorHandle) then
        begin
          AData := TCROListHandle(FFromHandle).Items[TCROListHandle(FFromHandle).ItemIndex];
          FToHandle.Data := TROComplexType(AData);
        end;
      end;
    end;
  end;
end;

procedure TCROHandleAssignObject.UpdateTarget(Target: TObject);
begin
  Enabled := Assigned(FFromHandle) and
             Assigned(FToHandle) and
             Assigned(FFromHandle.Data) and
             Assigned(FToHandle.Data);
             
  if Enabled then
  begin
    if FFromHandle.ClassType = FToHandle.ClassType
    then Enabled := FFromHandle.Data.InheritsFrom(FToHandle.Data.ClassType)
    else
    if FFromHandle.InheritsFrom(TCROCursorHandle) and
       FToHandle.InheritsFrom(TCROListHandle)
    then Enabled := FFromHandle.Data.InheritsFrom(TROArray(FToHandle.Data).GetItemClass)
    else
    if FFromHandle.InheritsFrom(TCROListHandle) and
       FToHandle.InheritsFrom(TCROCursorHandle)
    then Enabled := TROArray(FFromHandle.Data).GetItemClass.InheritsFrom(FToHandle.Data.ClassType);
  end;
end;

{ TCRONotebookAction }

procedure TCRONotebookAction.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FNotebook)
  then FNotebook := nil;
end;

procedure TCRONotebookAction.SetNotebook(const Value: TNotebook);
begin
  FNotebook := Value;
  if Assigned(FNotebook)
  then FNotebook.FreeNotification(Self);
end;

end.
