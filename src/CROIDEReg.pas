unit CROIDEReg;

interface

const
  CRO_TOOLS_PAGE_NAME = 'CRO Tools';
  CRO_AWARE_PAGE_NAME = 'CRO Aware';
  CRO_VISUAL_PAGE_NAME = 'CRO Visual';
  CRO_ACTION_CATEGORY_NAME = 'CRO Actions';

  prop_ServiceName = 'ServiceName';
  prop_OPERATION_NAME = 'OperationName';
  prop_PARAMS = 'Params';
  prop_COLUMNS = 'Columns';
  prop_EDITORTYPE = 'EditorType';
  prop_ValueName = 'Value';
  prop_ComboItemValues = 'ItemValues';
  prop_LabelEnumValues = 'EnumValues';
  prop_ActionItems = 'Actions';
  prop_Action = 'Action';
  prop_ResourceID = 'ResourceID';

procedure Register;

implementation

uses DesignIntf, Classes, ActnList, CROOperation, CROIDEEditors,
  CROEdit, CROCheckbox, CROActions, CROLookupCombo, CROComboBox,
  CROGrid, CROImage, CROSkinImage, CROButton, CROImageForm, CROLabel, CROSkinProperties,
  CROSpin, CROMemo, CRORadioButton, CROListBox, CRODateTimePicker, 
  CROResourceContainer;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(string), TCROCustomOperation, prop_OPERATION_NAME, TCROOperationNameProperty);
  RegisterPropertyEditor(TypeInfo(TCROOperationParams), TCROCustomOperation, prop_PARAMS, TQueryCROOperationParamsProperty);
  RegisterPropertyEditor(TypeInfo(TCROGridColumns), TCROGrid, prop_COLUMNS, TQueryCROGridColumnProperty);
  RegisterPropertyEditor(TypeInfo(string), TCROGridColumn, prop_EDITORTYPE, TCROGridColumnEditorProperty);
  RegisterPropertyEditor(TypeInfo(string), TCROServiceOperation, prop_ServiceName, TCROServiceNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TCRORadioButton, prop_ValueName, TCRORadioButtonValuesProperty);
  RegisterPropertyEditor(TypeInfo(TCROComboItems), TCROComboBox, prop_ComboItemValues, TQueryCROEnumItemsProperty);
  RegisterPropertyEditor(TypeInfo(TCROComboItems), TCROLabel, prop_LabelEnumValues, TQueryCROEnumItemsProperty);
  RegisterPropertyEditor(TypeInfo(TCROActionItems), TCROAction, prop_ActionItems, TQueryCROActionItemsProperty);
  RegisterPropertyEditor(TypeInfo(string), TCROCustomAction, prop_ResourceID, TCROResourceIDProperty);

  RegisterComponents(CRO_TOOLS_PAGE_NAME, [TCROServiceOperation]);
  RegisterComponents(CRO_TOOLS_PAGE_NAME, [TCROListHandle]);
  RegisterComponents(CRO_TOOLS_PAGE_NAME, [TCROCursorHandle]);
  RegisterComponents(CRO_TOOLS_PAGE_NAME, [TCROServiceManager]);

  RegisterComponents(CRO_AWARE_PAGE_NAME, [TCROEdit]);
  RegisterComponents(CRO_AWARE_PAGE_NAME, [TCROCheckbox]);
  RegisterComponents(CRO_AWARE_PAGE_NAME, [TCROLookupCombo]);
  RegisterComponents(CRO_AWARE_PAGE_NAME, [TCROComboBox]);
  RegisterComponents(CRO_AWARE_PAGE_NAME, [TCROGrid]);
  RegisterComponents(CRO_AWARE_PAGE_NAME, [TCROImage]);
  RegisterComponents(CRO_AWARE_PAGE_NAME, [TCROLabel]);
  RegisterComponents(CRO_AWARE_PAGE_NAME, [TCROSpinEdit]);
  RegisterComponents(CRO_AWARE_PAGE_NAME, [TCROMemo]);
  RegisterComponents(CRO_AWARE_PAGE_NAME, [TCRORadioButton]);
  RegisterComponents(CRO_AWARE_PAGE_NAME, [TCROListBox]);
  RegisterComponents(CRO_AWARE_PAGE_NAME, [TCRODateTimePicker]);

  RegisterComponents(CRO_VISUAL_PAGE_NAME, [TCROSkinImage]);
  RegisterComponents(CRO_VISUAL_PAGE_NAME, [TCROButton]);
  RegisterComponents(CRO_VISUAL_PAGE_NAME, [TCROImageForm]);
  RegisterComponents(CRO_VISUAL_PAGE_NAME, [TCROButtonProperties]);
  RegisterComponents(CRO_VISUAL_PAGE_NAME, [TCROImageProperties]);
  RegisterComponents(CRO_VISUAL_PAGE_NAME, [TCROFontStyleProperties]);
  RegisterComponents(CRO_VISUAL_PAGE_NAME, [TCROGridStyleProperties]);
  RegisterComponents(CRO_VISUAL_PAGE_NAME, [TCROSpinButton]);
  RegisterComponents(CRO_VISUAL_PAGE_NAME, [TCROResourceContainer]);

  RegisterActions(CRO_ACTION_CATEGORY_NAME, [TCROHandleNext], nil);
  RegisterActions(CRO_ACTION_CATEGORY_NAME, [TCROHandlePrior], nil);
  RegisterActions(CRO_ACTION_CATEGORY_NAME, [TCROHandleInsert], nil);
  RegisterActions(CRO_ACTION_CATEGORY_NAME, [TCROHandleEdit], nil);
  RegisterActions(CRO_ACTION_CATEGORY_NAME, [TCROHandleCancel], nil);
  RegisterActions(CRO_ACTION_CATEGORY_NAME, [TCROHandleDelete], nil);
  RegisterActions(CRO_ACTION_CATEGORY_NAME, [TCROHandlePost], nil);
  RegisterActions(CRO_ACTION_CATEGORY_NAME, [TCROOperationExecute], nil);
  RegisterActions(CRO_ACTION_CATEGORY_NAME, [TCROServiceManagerSelect], nil);
  RegisterActions(CRO_ACTION_CATEGORY_NAME, [TCROServiceManagerInsert], nil);
  RegisterActions(CRO_ACTION_CATEGORY_NAME, [TCROServiceManagerDelete], nil);
  RegisterActions(CRO_ACTION_CATEGORY_NAME, [TCROServiceManagerUpdate], nil);
  RegisterActions(CRO_ACTION_CATEGORY_NAME, [TCROAction], nil);
  RegisterActions(CRO_ACTION_CATEGORY_NAME, [TCROFormClose], nil);
  RegisterActions(CRO_ACTION_CATEGORY_NAME, [TCROHandleCopyObject], nil);
  RegisterActions(CRO_ACTION_CATEGORY_NAME, [TCROHandleMoveObject], nil);
  RegisterActions(CRO_ACTION_CATEGORY_NAME, [TCRONotebookNext], nil);
  RegisterActions(CRO_ACTION_CATEGORY_NAME, [TCRONotebookPrior], nil);
  RegisterActions(CRO_ACTION_CATEGORY_NAME, [TCROListBoxCopyItem], nil);
  RegisterActions(CRO_ACTION_CATEGORY_NAME, [TCROListBoxMoveItem], nil);
  RegisterActions(CRO_ACTION_CATEGORY_NAME, [TCROHandleAssignObject], nil);


end;

end.
