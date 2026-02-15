# CROComponents - RemObjects Remotting SDK Component Library

## Overview

CROComponents is a Delphi component library that bridges RemObjects Remotting (RPC) with VCL data-aware controls. It enables rapid development of distributed applications by binding UI controls directly to remote service operations.

**Technology**: Delphi 7, [RemObjects Remoting SDK](https://www.remobjects.com/ro/)  
**Purpose**: RPC client component library for building distributed applications

## Architecture

### Core RPC Layer (`CROOperation.pas`)

The foundation implements remote operation handling:

```
TCROCustomOperation
├── Manages TRORemoteService connection
├── Maintains TRODLLibrary (service definition)
├── Handles TROProxy for communication
└── Executes remote method calls via binary messaging

TCROOperationParams / TCROOperationParam
├── Parameter collection for RPC calls
├── Supports data binding via Expression (e.g., "Form1.Edit1.Text")
├── Type-safe parameter handling (TRODataType, TRODLParamFlag)
└── Variant-based value storage with object support
```

### Data Model: Cursor Handles

The library implements a **cursor-based data model** similar to TDataSet:

```
TCROHandle (abstract base)
├── Expression-based property binding
├── State management (csBrowse, csInsert, csEdit, csCancel)
├── Auto-edit functionality
└── Event-driven updates

TCROCursorHandle
├── Single record buffer (TROComplexType)
├── Edit/Insert/Delete/Cancel operations
├── Linked to TCROListHandle for list navigation
└── Property access via expressions

TCROListHandle  
├── Collection of TROComplexType records
├── BOF/EOF navigation (Next/Prior)
├── Multiple cursor handle support
├── Expression evaluation for filtering
└── Index mapping for lookups
```

### Data-Aware Controls

UI components bind to cursor handles:

| Component | Base Class | Purpose |
|-----------|-----------|---------|
| `CROEdit` | TCustomEdit | Text input bound to cursor fields |
| `CROGrid` | TCustomGrid | Tabular data with expression columns |
| `CROComboBox` | TCustomComboBox | Dropdown with data binding |
| `CROCheckbox` | TCustomCheckbox | Boolean field binding |
| `CRODateTimePicker` | TDateTimePicker | Date/time field binding |
| `CROLookupCombo` | TCustomComboBox | Lookup field support |
| `CROListBox` | TCustomListBox | List selection binding |
| `CROButton` | TButton | Action triggers |

Each implements `ICROCustomControl` interface for value synchronization:
- `UpdateValue` - Refresh UI from cursor data
- `UpdateObject` - Save UI changes to cursor

### Expression Binding System (`CROComponentsCommon.pas`)

Property binding via string expressions:
```pascal
// Syntax: ComponentName.PropertyName
Expression := 'CustomerEdit.Text'  // Binds to CustomerEdit's Text property
Expression := 'OrderGrid.Data'     // Binds to grid's data source
```

Key functions:
- `GetPropInfoFromExpression` - Resolves expression to object/property
- `GetNextIdentifier` - Parses dot-separated paths
- `GetTrueOwner` - Finds owning Form/DataModule

### Action Framework (`CROActions.pas`)

Operation execution via Delphi Actions:

```
TCROCustomAction
├── Execute conditions (before/after)
├── Confirmation dialogs
└── Backward action chains

TCROOperationExecute (Select/Insert/Update/Delete)
├── Binds to TCROServiceManager
├── Automatic operation routing
└── Result handling
```

### Skin System

Optional visual customization:
- `CROSkinControl` - Skin update notifications
- `CROSkinProperties` - Visual property storage
- `CROSkinImage` - Image-based skins
- `BitmapToRegion` - Transparent image regions

## Design Patterns

### 1. Interface-Based Communication
```pascal
ICROCustomControl = interface
  ['{F27BD8D2-E4BA-4A40-BF84-E1289E2CD561}']
  procedure UpdateValue;
end;
```

### 2. Component Aggregation
```pascal
TCROCustomEdit = class(TCustomEdit, ICROCustomControl, ICROSkinControl)
  FProperties: TCROProperties;  // Links to cursor handle
```

### 3. Expression Parser
Dynamic property binding at design/runtime without code coupling.

### 4. Lazy Initialization
RORemoteService, RODLLibrary resolved on first access.

## Key Implementation Details

### RPC Communication Flow
1. Set OperationName on TCROOperation
2. Configure Parameters via Expression or Value
3. Call Execute → serializes to binary via TROBinMessage
4. TROTransportChannel dispatches to server
5. Response deserialized back to parameters

### Expression Evaluation
```pascal
// Runtime binding resolution
procedure TField.SetValue(Value: Variant);
begin
  Instance := nil;
  GetPropInfoFromExpression(Owner, Expression, Instance, PropInfo);
  SetPropValue(Instance, PropInfo^.Name, Value);
end;
```

### State Management
```
csBrowse → csEdit (user types) → Post → csBrowse
         → csInsert (Insert)   → Post → csBrowse
         → csCancel (Escape)  → csBrowse
```

## Key Skills Demonstrated

- **Component Development**: Full VCL component lifecycle, IDE integration
- **RPC/Distributed Systems**: Binary serialization, transport channels, async operations
- **Design Patterns**: Interface-based, aggregation, expression evaluation
- **Data Binding**: Cursor model, property binding, automatic synchronization
- **Framework Design**: Extensible architecture with skin support
- **Delphi Mastery**: RTTI, streaming, component editors, design-time registration

## Dependencies

- `rtl` - Delphi runtime
- `RemObjects_Core_D7` - RemObjects core
- `designide` - IDE integration
- `vcl` - Visual component library
- `vcldb` - Database VCL
