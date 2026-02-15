unit CROComponentsCommon;

interface

uses
  Windows, Dialogs, SysUtils, Classes, uRORemoteService, uRODL, DB, uROProxy, uROTypes,
  Variants, TypInfo, Graphics, Forms, Controls, StdCtrls, Messages;

type
  TFrameStyle = (fs3D, fs2D, fsFramed, fsNone);

  ICROCustomControl = interface
    ['{F27BD8D2-E4BA-4A40-BF84-E1289E2CD561}']
    procedure UpdateValue;
  end;

  ICROCustomListControl = interface
    ['{C37BFE3A-F935-43D8-B8DD-A6A39E0BF4F2}']
    procedure UpdateItems;
    procedure UpdateIndex;
  end;

  ICROSkinControl = interface
    ['{76EF587B-D9DF-454E-8C2F-F90DD89545DC}']
    procedure UpdateSkin;
  end;

  ICROEdit = interface
    ['{0C7F3BEB-82DD-42DB-9462-2B97DB47B19E}']
    procedure DoSetMaxLength(Value: Integer);
  end;

procedure GetPropInfoFromExpression(Owner: TComponent; const Expression: String; var Instance: TObject;
  var PropInfo: PPropInfo);
function GetNextIdentifier(var AExpression: String): String;
function FindROStruct(Lib:TRODLLibrary; const StructName: String): TRODLStruct;
function FindROArray(Lib:TRODLLibrary; const ArrayName: String): TRODLArray;
function FindROEnum(Lib:TRODLLibrary; const EnumName: String): TRODLEnum;
function CreateROObject(ADataType: String; Lib:TRODLLibrary): TROComplexType;
function CreateRegionFromBitmap(ABitmap: Graphics.TBitmap; TransColor: TColor): HRgn;
function IsTrueColorBitmap(Bitmap: Graphics.TBitmap): boolean;
function ThisThat(const Clause: Boolean; TrueVal, FalseVal: Integer): Integer;
function IsClass(ClassType: TClass; const Name: string): Boolean;
function GetTrueOwner(Owner: TComponent; var Expression:String): TComponent;
function ValidateEditKey(const ADataType: String; AKey: Char; Edit: TCustomEdit): Boolean;
function GenerateIdentifier: Int64;
procedure AssignROObject(src, dst: TObject);
function BitmapToRegion( hBmp: TBitmap; TransColor: TColor; Tolerance: Integer): HRGN;
function GetProposedDisplay(const AName: String):String;
procedure CheckFocus(ASelf: TControl; const Key: Char; const AEnterToTab:Boolean);
function FindInROEnum(Lib: TRODLLibrary; const EnumName, EnumValue: String): Boolean;


implementation

var
  LastIdentifier: Int64; //global and unique to ensure that no indentifier created by cursor or list handles will be duplicated

const
  BitMask: array[0..7] of byte = (128, 64, 32, 16, 8, 4, 2, 1);
  IntegerChars = ['-','0','1','2','3','4','5','6','7','8','9',#8,#9,#13,#27];


function FindInROEnum(Lib: TRODLLibrary; const EnumName, EnumValue: String): Boolean;
var Enum: TRODLEnum; i: Integer;
begin
  Result := False;
  if Assigned(Lib) then
  begin
    Enum := FindROEnum(Lib,EnumName);
    if Assigned(Enum) then
    begin
      for i := 0 to Enum.Count-1 do
      begin
        if Enum.Items[i].Info.Name = EnumValue then
        begin
          Result := True;
          Break;
        end;
      end;
    end;
  end;
end;


procedure CheckFocus(ASelf: TControl; const Key: Char; const AEnterToTab:Boolean);
begin
  if (Key = #13) and (AEnterToTab)
  then TControl(ASelf.Owner).Perform(WM_NEXTDLGCTL,0,0);
end;

function GetProposedDisplay(const AName: String): String;
var j: Integer;
begin
  Result := AName;
  //remove sufix
  for j := 1 to Length(Result) do
  begin
    if Ord(Result[j]) in [Ord('A')..Ord('Z')] then
    begin
      Result := Copy(Result,j,MaxByte);
      Break;
    end;
  end;
  //insert spaces
  j := 4;
  while j < Length(Result) do
  begin
    if Ord(Result[j]) in [Ord('A')..Ord('Z')] then
    begin
      Insert(' ',Result,j);
      Inc(j);
    end;
    Inc(j);
  end;
end;


//creates a region from a bitmap
function BitmapToRegion( hBmp: TBitmap; TransColor: TColor;
                         Tolerance: Integer): HRGN;

  function MinByte(B1, B2: byte): byte;
  begin
    if B1 < B2 then
      Result := B1
    else
      Result := B2;
  end;

const
  ALLOC_UNIT = 100;
var
  MemDC, DC: HDC;
  BitmapInfo: TBitmapInfo;
  hbm32, holdBmp, holdMemBmp: HBitmap;
  pbits32 : Pointer;
  bm32 : BITMAP;
  maxRects: DWORD;
  hData: HGLOBAL;
  pData: PRgnData;
  b, LR, LG, LB, HR, HG, HB: Byte;
  p32: pByte;
  x, x0, y: integer;
  p: pLongInt;
  pr: PRect;
  h: HRGN;
begin
  Result := 0;
  if hBmp <> nil then
  begin
    { Create a memory DC inside which we will scan the bitmap contents }
    MemDC := CreateCompatibleDC(0);
    if MemDC <> 0 then
    begin
     { Create a 32 bits depth bitmap and select it into the memory DC }
      with BitmapInfo.bmiHeader do
      begin
        biSize := sizeof(TBitmapInfoHeader);
        biWidth := hBmp.Width;
        biHeight := hBmp.Height;
        biPlanes := 1;
        biBitCount := 32;
        biCompression := BI_RGB; { (0) uncompressed format }
        biSizeImage := 0;
        biXPelsPerMeter := 0;
        biYPelsPerMeter := 0;
        biClrUsed := 0;
        biClrImportant := 0;
      end;
      hbm32 := CreateDIBSection(MemDC, BitmapInfo, DIB_RGB_COLORS, pbits32,0, 0);
      if hbm32 <> 0 then
      begin
        holdMemBmp := SelectObject(MemDC, hbm32);
        {
          Get how many bytes per row we have for the bitmap bits
          (rounded up to 32 bits)
        }
        GetObject(hbm32, SizeOf(bm32), @bm32);
        while (bm32.bmWidthBytes mod 4) > 0 do
          inc(bm32.bmWidthBytes);
        DC := CreateCompatibleDC(MemDC);
        { Copy the bitmap into the memory DC }
        holdBmp := SelectObject(DC, hBmp.Handle);
        BitBlt(MemDC, 0, 0, hBmp.Width, hBmp.Height, DC, 0, 0, SRCCOPY);
        {
          For better performances, we will use the ExtCreateRegion() function
          to create the region. This function take a RGNDATA structure on
          entry. We will add rectangles by
          amount of ALLOC_UNIT number in this structure
        }
        maxRects := ALLOC_UNIT;
        hData := GlobalAlloc(GMEM_MOVEABLE, sizeof(TRgnDataHeader) +
           SizeOf(TRect) * maxRects);
        pData := GlobalLock(hData);
        pData^.rdh.dwSize := SizeOf(TRgnDataHeader);
        pData^.rdh.iType := RDH_RECTANGLES;
        pData^.rdh.nCount := 0;
        pData^.rdh.nRgnSize := 0;
        SetRect(pData^.rdh.rcBound, MaxInt, MaxInt, 0, 0);
        { Keep on hand highest and lowest values for the "transparent" pixel }
        LR := GetRValue(ColorToRGB(TransColor));
        LG := GetGValue(ColorToRGB(TransColor));
        LB := GetBValue(ColorToRGB(TransColor));
        { Add the value of the tolerance to the "transparent" pixel value }
        HR := MinByte($FF, LR + Tolerance);
        HG := MinByte($FF, LG + Tolerance);
        HB := MinByte($FF, LB + Tolerance);
        {
          Scan each bitmap row from bottom to top,
          the bitmap is inverted vertically
        }
        p32 := bm32.bmBits;
        inc(PChar(p32), (bm32.bmHeight - 1) * bm32.bmWidthBytes);
        for y := 0 to hBmp.Height-1 do
        begin
          { Scan each bitmap pixel from left to right }
          x := -1;
          while x+1 < hBmp.Width do
          begin
            inc(x);
            { Search for a continuous range of "non transparent pixels" }
            x0 := x;
            p := PLongInt(p32);
            inc(PChar(p), x * SizeOf(LongInt));
            while x < hBmp.Width do
            begin
              b := GetBValue(p^); // Changed from GetRValue(p^)
              if (b >= LR) and (b <= HR) then
              begin
                b := GetGValue(p^); // Left alone
                if (b >= LG) and (b <= HG) then
                begin
                  b := GetRValue(p^); // Changed from GetBValue(p^)
                  if (b >= LB) and (b <= hb) then
                    { This pixel is "transparent" }
                    break;
                end;
              end;
              inc(PChar(p), SizeOf(LongInt));
              inc(x);
            end;
            if x > x0 then
            begin
              {
                Add the pixels (x0, y) to (x, y+1) as a new rectangle in
                the region
              }
              if pData^.rdh.nCount >= maxRects then
              begin
                GlobalUnlock(hData);
                inc(maxRects, ALLOC_UNIT);
                hData := GlobalReAlloc(hData, SizeOf(TRgnDataHeader) +
                   SizeOf(TRect) * maxRects, GMEM_MOVEABLE);
                pData := GlobalLock(hData);
                Assert(pData <> NIL);
              end;
              pr := @pData^.Buffer[pData^.rdh.nCount * SizeOf(TRect)];
              SetRect(pr^, x0, y, x, y+1);
              if x0 < pData^.rdh.rcBound.Left then
                pData^.rdh.rcBound.Left := x0;
              if y < pData^.rdh.rcBound.Top then
                pData^.rdh.rcBound.Top := y;
              if x > pData^.rdh.rcBound.Right then
                pData^.rdh.rcBound.Left := x;
              if y+1 > pData^.rdh.rcBound.Bottom then
                pData^.rdh.rcBound.Bottom := y+1;
              inc(pData^.rdh.nCount);
              {
               On Windows98, ExtCreateRegion() may fail if the number of
               rectangles is too large (ie: > 4000). Therefore, we have to
               create the region by multiple steps
              }
              if pData^.rdh.nCount = 2000 then
              begin
                h := ExtCreateRegion(NIL, SizeOf(TRgnDataHeader) +
                   (SizeOf(TRect) * maxRects), pData^);
                Assert(h <> 0);
                if Result <> 0 then
                begin
                  CombineRgn(Result, Result, h, RGN_OR);
                  DeleteObject(h);
                end else
                  Result := h;
                pData^.rdh.nCount := 0;
                SetRect(pData^.rdh.rcBound, MaxInt, MaxInt, 0, 0);
              end;
            end;
          end;
          {
            Go to next row (remember, the bitmap is inverted vertically)
            that is why we use DEC!
          }
          Dec(PChar(p32), bm32.bmWidthBytes);
        end;
        { Create or extend the region with the remaining rectangle }
        h := ExtCreateRegion(NIL, SizeOf(TRgnDataHeader) +
           (SizeOf(TRect) * maxRects), pData^);
        Assert(h <> 0);
        if Result <> 0 then
        begin
          CombineRgn(Result, Result, h, RGN_OR);
          DeleteObject(h);
        end else
          Result := h;
        { Clean up }
        GlobalFree(hData);
        SelectObject(DC, holdBmp);
        DeleteDC(DC);
        DeleteObject(SelectObject(MemDC, holdMemBmp));
      end;
    end;
    DeleteDC(MemDC);
  end;
end;

//assigns an rocomplextype to another
procedure AssignROObject(src, dst: TObject);
var i, iItems, fPropCountSrc, fPropCountDst: Integer;
    fPropName: String;
    fPropListSrc, fPropListDst: PPropList;
    fPropInfo: PPropInfo;
    fChildObj, fNewChildObj, fItem: TObject;
    fPropNameListDst: TStringList;
begin
  fPropNameListDst := TStringList.Create;

  fPropCountDst := GetPropList(PTypeInfo(dst.ClassInfo), fPropListDst);

  if fPropCountDst > 0 then
  begin
    GetMem(fPropListDst, fPropCountDst*SizeOf(Pointer));
    GetPropList(dst.ClassInfo, tkProperties, fPropListDst);

    for i := 0 to fPropCountDst-1 do
    begin
      fPropInfo := fPropListDst^[i];
      fPropNameListDst.Add(fPropInfo^.Name);
    end;
  end;

  fPropCountSrc := GetPropList(PTypeInfo(src.ClassInfo), fPropListSrc);

  if fPropCountSrc > 0 then
  begin
    GetMem(fPropListSrc, fPropCountSrc*SizeOf(Pointer));
    GetPropList(src.ClassInfo, tkProperties, fPropListSrc);

    // loop to verify all fields
    for i := 0 to fPropCountSrc-1 do
    begin
      fPropInfo := fPropListSrc^[i];

      // get the field name
      fPropName := fPropInfo^.Name;

      if fPropNameListDst.IndexOf(fPropName) = -1
      then Continue;

      // verify if the field is another object

      if (fPropInfo^.PropType^.Kind = tkClass) {and (fPropName <> 'photo') }then
      begin
        // load the objects from the fields
        fChildObj := GetObjectProp(src, fPropName);

        if fChildObj <> nil then
        begin
          if fChildObj.InheritsFrom(TROArray) then
          begin
            fNewChildObj := TObject(fChildObj.ClassType.Create);

            for iItems := 0 to TROArray(fChildObj).Count - 1 do
            begin
              fItem := TObject(TROArray(fChildObj).GetItemClass.Create);
              AssignROObject(TObject(TROArray(fChildObj).GetItemRef(iItems)), fItem);

              TROArray(fNewChildObj).Resize(iItems+1);
              TROArray(fNewChildObj).SetItemRef(iItems, Pointer(fItem));
            end;
          end
          else if fChildObj.InheritsFrom(TROBinaryMemoryStream) then
          begin
            fNewChildObj := TObject(TROBinaryMemoryStream.Create);
            TROBinaryMemoryStream(fNewChildObj).Assign(TROBinaryMemoryStream(fChildObj));
          end
          else
          begin
            // if the field has an object, create another and assign the values
            fNewChildObj := TObject(fChildObj.ClassType.Create);
            AssignROObject(fChildObj, fNewChildObj);
          end;

          SetOrdProp(dst, fPropName, Integer(fNewChildObj));
        end;
      end
      else
        SetPropValue(dst, fPropName, GetPropValue(src, fPropName));
    end;

    FreeMem(fPropListSrc);
    FreeMem(fPropListDst);
  end;
end;

//generate new negative and temporary identifiers for objects created by cursor and list handles
function GenerateIdentifier: Int64;
begin
  Dec(LastIdentifier);
  Result := LastIdentifier;
end;

//validate keys for edit inputs according to expression datatype
function ValidateEditKey(const ADataType: String; AKey: Char; Edit: TCustomEdit): Boolean;
var FormatSettings: TFormatSettings;
begin
  GetLocaleFormatSettings(0,FormatSettings);
  Result := True;
  if UpperCase(ADataType) = UpperCase('Integer') then
  begin
    Result := (AKey in IntegerChars + [FormatSettings.ThousandSeparator]) and
              ((Edit.SelStart = 0) or (AKey <> '-'));
  end
  else
  if UpperCase(ADataType) = UpperCase('Char') then
  begin
    Edit.Text := Edit.Text[1];
    (Edit as ICROEdit).DoSetMaxLength(1);
    Result := True;
  end
  else
  if Pos(UpperCase(ADataType),UpperCase('Real,Double,Extended,Real48,Single,Comp,Currency')) > 0 then
  begin
{    if AKey in [',','.']
    then AKey := FormatSettings.DecimalSeparator;removido por causa do uso de separador de milhar}

    Result := (AKey in IntegerChars + [FormatSettings.DecimalSeparator,FormatSettings.ThousandSeparator]) and
              ((AKey <> FormatSettings.DecimalSeparator) or (Pos(FormatSettings.DecimalSeparator,Edit.Text) = 0)) and
              ((Edit.SelStart = 0) or (AKey <> '-'));
  end
  else
  if UpperCase(ADataType) = UpperCase('TDateTime') then
  begin
    Result := (AKey in IntegerChars + [FormatSettings.DateSeparator,FormatSettings.TimeSeparator]);
  end;
end;


function GetTrueOwner(Owner: TComponent; var Expression:String): TComponent;
var AExpression, SupposedOwnerName: String; AOwner: TComponent; AForceFlag: Char;
begin
  if (Length(Expression) > 0)
  then AForceFlag := Expression[1]
  else AForceFlag := #0;

  //verifica se o owner deve ser um form ou datamodule indicado pela propria expression, corrige owner e expression //Ex: Form9.Edit1.Text
  if (AForceFlag <> '-') then  //"-" deve ser usado em caso de expressions q referenciam uma propriedade de objeto que tenha o mesmo nome de um form ou dm do application
  begin
    AExpression := Expression;
    SupposedOwnerName := GetNextIdentifier(AExpression);
    if AForceFlag in ['+','-']
    then SupposedOwnerName := Copy(SupposedOwnerName,2,MaxInt);
    AOwner := Application.FindComponent(SupposedOwnerName);

    if (Assigned(AOwner)) and (
         (Owner.InheritsFrom(TDataModule)) or
         (Owner.InheritsFrom(TCustomFrame)) or
         (Owner.InheritsFrom(TCustomForm))
       ) or (
         (AForceFlag = '+')  //"+" deve ser usado para forçar o interpretador a considerar o primeiro identificador de um expression como sendo o Owner do segundo identificador
       ) then
    begin
      Expression := AExpression;
      Result := AOwner;
      Exit;
    end;
  end;

  //localiza o primeiro Owner de um dos tipos abaixo como sendo o owner desejado
  while (not Owner.InheritsFrom(TDataModule)) and
        (not Owner.InheritsFrom(TCustomFrame)) and
        (not Owner.InheritsFrom(TCustomForm)) do
  begin
    if (Owner.ClassType <> TComponent)
    then Owner := Owner.Owner
    else Break;
  end;
  Result := Owner;
end;

function IsClass(ClassType: TClass; const Name: string): Boolean;
begin
  Result := True;
  while ClassType <> nil do
  begin
    if uppercase(ClassType.ClassName)=uppercase(Name) then Exit;
    ClassType := ClassType.ClassParent;
  end;
  Result := False;
end;

function ThisThat(const Clause: Boolean; TrueVal, FalseVal: Integer): Integer;
begin
  if Clause then result := TrueVal else Result := FalseVal;
end;

function IsTrueColorBitmap(Bitmap: Graphics.TBitmap): boolean;
begin
  result:= Bitmap.PixelFormat = Graphics.pf24bit;
end;

function CreateRegionFromBitmap(ABitmap: Graphics.TBitmap; TransColor: TColor): HRgn;
var
  TempBitmap: Graphics.TBitmap;
  Rgn1, Rgn2: HRgn;
  Col, StartCol, Row: integer;
  Line: PByteArray;

  function ColToColor(Col: integer): TColor;
  begin
    if IsTrueColorBitmap(TempBitmap) then
      result:= Line[Col * 3] * 256 * 256 + Line[Col * 3 + 1] * 256 + Line[Col * 3 + 2]
    else result := TColor(ThisThat((Line[Col div 8] and BitMask[Col mod 8]) <> 0, clBlack, clWhite));
  end;
begin
  result := 0;
  if (ABitmap <> nil) and (ABitmap.Width = 0) or (ABitmap.Height = 0) then Exit;
  Rgn1 := 0;

  TempBitmap := Graphics.TBitmap.Create;

  TempBitmap.Assign(ABitmap);
  if not IsTrueColorBitmap(TempBitmap) then
  begin
    TempBitmap.Mask(TransColor);
    TransColor := clBlack;
  end;

  with TempBitmap do
  begin
    for Row := 0 to TempBitmap.height-1 do
    begin
      Line:= scanLine[row];

      Col := 0;
      while Col < TempBitmap.Width do
      begin
        while (Col < TempBitmap.Width) and (ColToColor(Col) = TransColor) do inc(Col);
        if Col >= TempBitmap.Width then Continue;

        StartCol := Col;
        while (Col < TempBitmap.Width) and (ColToColor(Col) <> TransColor) do inc(Col);
        if Col >= TempBitmap.Width then Col := TempBitmap.Width;

        if Rgn1 = 0 then Rgn1 := CreateRectRgn(StartCol, Row, Col, Row + 1)
        else begin
          Rgn2 := CreateRectRgn(StartCol, Row, Col, Row + 1);
          if (Rgn2 <> 0) then CombineRgn(Rgn1,Rgn1,Rgn2,RGN_OR);
            Deleteobject(Rgn2);
        end;
      end;
    end;
  end;
  result := Rgn1;
  TempBitmap.Free;
end;

//Find an TRODLStruct in a TRODLLibrary
function FindROStruct(Lib:TRODLLibrary; const StructName: String): TRODLStruct;
var i: Integer;
begin
  Result := nil;
  with Lib do
  begin
    for i := 0 to StructCount - 1 do
    begin
      if CompareText(Structs[i].Info.Name, StructName) = 0 then
      begin
        Result := Structs[i];
        Break;
      end;
    end;
  end;
end;

//Find an TRODLArray in a TRODLLibrary
function FindROArray(Lib:TRODLLibrary; const ArrayName: String): TRODLArray;
var i: Integer;
begin
  Result := nil;
  with Lib do
  begin
    for i := 0 to ArrayCount-1 do
    begin
      if CompareText(Arrays[i].Info.Name,ArrayName) = 0 then
      begin
        Result := Arrays[i];
        Break;
      end;
    end;
  end;  
end;

//Find an TRODLEnum in a TRODLLibrary
function FindROEnum(Lib:TRODLLibrary; const EnumName: String): TRODLEnum;
var i: Integer;
begin
  Result := nil;
  with Lib do
  begin
    for i := 0 to EnumCount-1 do
    begin
      if CompareText(Enums[i].Info.Name,EnumName) = 0 then
      begin
        Result := Enums[i];
        Break;
      end;
    end;
  end;
end;

// Get the first identifier in the expression and remove it
function GetNextIdentifier(var AExpression: String): String;
var iPos: Integer;
begin
  Result := '';

  // Verify if the expression has more identifiers
  if Trim(AExpression) <> '' then
  begin
    // Get the position of the identifier separator "."
    iPos := Pos('.', AExpression);

    // Verify if the separator have been found
    if iPos > 0 then
    begin
      // Copy the first identifier
      Result := Copy(AExpression, 1, iPos-1);
      // Remove the identifier from the expression
      Delete(AExpression, 1, iPos);
    end
    else
    begin
      // if the expression has no more separator, the identifier is the
      // expression
      Result      := Trim(AExpression);
      // Clear the expression
      AExpression := '';
    end;
  end;
end;

// Get the instance of the object owner of the property, the pointer where is
// the value of the property, and the pointer with information about the property
// assigned in the expression.
procedure GetPropInfoFromExpression(Owner: TComponent; const Expression:String; var Instance:
  TObject; var PropInfo: PPropInfo);
var cExpression: String;
    cPropName: String;

    obj: TObject;
    value: Variant;
    AOwner: TComponent;
begin
  cExpression := Expression;

  if Instance = nil then
  begin
    // The first identifier must be a name of some component
    AOwner := GetTrueOwner(Owner,cExpression);

    if (Length(cExpression) > 0) and (
         (cExpression[1] = '+') or (cExpression[1] = '-')
       )
    then Delete(cExpression,1,1);

    obj := AOwner.FindComponent(GetNextIdentifier(cExpression));
  end
  else
  begin
    obj := Instance;
  end;

  if Assigned(obj) then
  begin
    // Loop to get the property assigned in the expression
    while cExpression <> '' do
    begin
      // Set the instance of the property owner
      Instance := obj;

      if not Assigned(Instance)
      then Exit;//Raise Exception.Create('Property not assigned.');

      // Get the property name in the expression
      cPropName := GetNextIdentifier(cExpression);

      // Get the information of the property
      PropInfo := GetPropInfo(obj, cPropName);

      if cExpression <> '' then
      begin
        // Verify if the property is an Object
        if PropInfo^.PropType^.Kind = tkClass then
        begin
          // Get the object of the property
          obj := GetObjectProp(obj, cPropName);
        end
        else if PropInfo^.PropType^.Kind in [tkInteger, tkChar, tkFloat,
                  tkString, tkWChar, tkLString, tkWString, tkVariant, tkInt64] then
        begin
          // if the property isn't an object, gets its value
          value := GetPropValue(obj, cPropName);
        end
        else
          Raise Exception.Create('Property type not expected.');
      end;
    end;
  end
  else
    Raise Exception.Create('Component not found.');
end;

function CreateROObject(ADataType: String; Lib:TRODLLibrary): TROComplexType;
var obj, objChild: TROComplexType;
    Struct: TRODLStruct;
    i: Integer;
begin
  obj := FindROClass(ADataType).Create;
  {.$DEFINE CREATE_INTERNAL_OBJECTS}
  {$IFDEF CREATE_INTERNAL_OBJECTS}
  if IsStruct(ADataType, Lib) then
  begin
    repeat
      Struct := Lib.FindStruct(ADataType);
      for i := 0 to Struct.Count - 1 do
      begin
        if FindROClass(Struct.Items[i].Info.DataType) <> nil then
        begin
          objChild := CreateROObject(Struct.Items[i].Info.DataType, Lib);
          // Set the property
          SetObjectProp(obj, Struct.Items[i].Info.Name, objChild);
        end;
      end;
      ADataType := Struct.GetAncestor;
    until ADataType = '';
  end;
  {$ELSE}
  if IsStruct(ADataType, Lib) then
  begin
    Struct := Lib.FindStruct(ADataType);

    for i := 0 to Struct.Count - 1 do
    begin
      if FindROClass(Struct.Items[i].Info.DataType) <> nil then
      begin
        objChild := CreateROObject(Struct.Items[i].Info.DataType, Lib);
        // Set the property
        SetObjectProp(obj, Struct.Items[i].Info.Name, objChild);
      end;
    end;
  end;
  {$ENDIF}
  Result := obj;
end;

initialization
  LastIdentifier := 0;//will be decreased - see GenerateIdentifier;

finalization

end.
