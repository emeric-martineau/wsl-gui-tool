{
 /***************************************************************************
                             wslconfig.pas
                             -------------
 ***************************************************************************/
}
{
@abstract(Provide a class compatible with ini file to read wslconfig)
@author(Emeric MARTINEAU)
@created(2021)
}
unit WslConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  TWslConfigEntryType = (WslconfigSection, WslconfigKeyValue, WslconfigDiscardLine);

  TWslConfigItem = class(TObject)
  private
    FType: TWslConfigEntryType;
    // Name of section or key
    FName: String;
    // Value of key
    FValue: string;
    // Line number in file where store section or key
    FLineNumber: integer;
    // Start in line of key or section
    FKeyNameStart: integer;
    // End in line of key or section
    FKeyNameEnd: integer;
    // Start in line of value
    FKeyValueStart: integer;
    // End in line of value
    FKeyValueEnd: integer;
    // If true, value or name changed
    FIsChanged: boolean;
    // If key is delete
    FIsDeleted: boolean;
    // If value is quoted when read. For new value is always false
    FQuoted: boolean;

    procedure SetName(aName: string);
    procedure SetValue(aValue: string);

    constructor Create(aType: TWslConfigEntryType; aName: String; aValue: string;
      aLineNumber: integer; aKeyNameStart: integer; aKeyNameEnd: integer;
      aKeyValueStart: integer; aKeyValueEnd: integer; aQuoted: boolean);
  public
    constructor CreateSection(aName: String; aLineNumber: integer;
      aKeyNameStart: integer; aKeyNameEnd: integer);

    constructor CreateNewSection(aName: String);

    constructor CreateKey(aName: String; aValue: string;
      aLineNumber: integer; aKeyNameStart: integer; aKeyNameEnd: integer;
      aKeyValueStart: integer; aKeyValueEnd: integer; aQuoted: boolean);

    constructor CreateNewKey(aName: String; aValue: string);

    constructor CreateDiscardLine(aLineNumber: integer);

    property EntryType: TWslConfigEntryType read FType;
    property Name: string read FName write SetName;
    property Value: string read FValue write SetValue;
    property LineNumber: integer read FLineNumber;
    property KeyNameStart: integer read FKeyNameStart;
    property KeyNameEnd: integer read FKeyNameEnd;
    property KeyValueStart: integer read FKeyValueStart;
    property KeyValueEnd: integer read FKeyValueEnd;
    property IsChanged: boolean read FIsChanged;
    property IsDeleted: boolean read FIsDeleted write FIsDeleted;
    property Quoted: boolean read FQuoted;
  end;

  TWslConfigItemList = specialize TFPGObjectList<TWslConfigItem>;

  (****************************************************************************
   * This class is like TIniFile cause .wslconfig file is same format except for
   * comment.
   * Comment is # and not ;
   *
   * This class load all file in TStrings (FLines).
   * Then read file and create a list of TWslConfigItem to store data type,
   * name/value, position in file.
   *
   * The class have FLines with original data and FItemList like this:
   * {Name: '<section>', LineNumber, Start, End}              <- section
   * {LineNumber}                                             <- empty line or comment
   * {Name: 'key', Value: '<value>', LineNumber, Start, End}  <- key/value
   *
   * With two list, it's easy to update.
   * This behavior allow to keep original file, just change necessary data.
   ****************************************************************************)
  TWslconfigFile = class(TObject)
  private
    // Content of Wslconfig file
    FLines: TStrings;
    // Filename to load/store
    FFileName: string;
    // List of item in file
    FItemList: TWslConfigItemList;

    // Reload file from FLines
    procedure ReloadFile;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    function ReadString(const Section, Ident, Default: string): string;
    procedure WriteString(const Section, Ident, Value: String);
    procedure ReadSection(const Section: string; Strings: TStrings);
    procedure ReadSections(Strings: TStrings);
    procedure ReadSectionValues(const Section: string; Strings: TStrings);
    procedure EraseSection(const Section: string);
    procedure DeleteKey(const Section, Ident: String);
    procedure UpdateFile;
    function ReadInteger(const Section, Ident: string; Default: integer): integer;
    procedure WriteInteger(const Section, Ident: string; Value: integer);
    function ReadBool(const Section, Ident: string; Default: boolean): boolean;
    procedure WriteBool(const Section, Ident: string; Value: boolean);
  end;

  TKeyData = record
    Data: string;
    PosStart: integer;
    PosEnd: integer;
    Quoted: boolean;
  end;

const
  WSLCONFIG_NOT_SET = -1;
  WSLCONFIG_VALUE_NOT_SET = '';
  WSLCONFIG_KEY_VALUE_SEPARATOR = '=';
  WSLCONFIG_SECTION_OPEN = '[';
  WSLCONFIG_SECTION_CLOSE = ']';
  WSLCONFIG_COMMENT = '#';
  WSLCONFIG_VALUE_QUOTE = '"';
  WSLCONFIG_VALUE_TRUE = 'true';
  WSLCONFIG_VALUE_FALSE = 'false';

implementation

{ TWslConfigItem }

constructor TWslConfigItem.Create(aType: TWslConfigEntryType; aName: String; aValue: string;
  aLineNumber: integer; aKeyNameStart: integer; aKeyNameEnd: integer;
  aKeyValueStart: integer; aKeyValueEnd: integer; aQuoted: boolean);
begin
  FIsChanged := false;
  FIsDeleted := false;

  FQuoted := aQuoted;
  FType := aType;
  FName := aName;
  FValue := aValue;
  FLineNumber := aLineNumber;
  FKeyNameStart := aKeyNameStart;
  FKeyNameEnd := aKeyNameEnd;
  FKeyValueStart := aKeyValueStart;
  FKeyValueEnd := aKeyValueEnd;
end;

constructor TWslConfigItem.CreateSection(aName: String; aLineNumber: integer;
  aKeyNameStart: integer; aKeyNameEnd: integer);
begin
  Create(
    WslconfigSection,
    aName,
    WSLCONFIG_VALUE_NOT_SET,
    aLineNumber,
    aKeyNameStart,
    aKeyNameEnd,
    WSLCONFIG_NOT_SET,
    WSLCONFIG_NOT_SET,
    false);
end;

constructor TWslConfigItem.CreateNewSection(aName: String);
begin
  CreateSection(
    aName,
    WSLCONFIG_NOT_SET,
    WSLCONFIG_NOT_SET,
    WSLCONFIG_NOT_SET);
end;

constructor TWslConfigItem.CreateKey(aName: String; aValue: string;
  aLineNumber: integer; aKeyNameStart: integer; aKeyNameEnd: integer;
  aKeyValueStart: integer; aKeyValueEnd: integer; aQuoted: boolean);
begin
  Create(
    WslconfigKeyValue,
    aName,
    aValue,
    aLineNumber,
    aKeyNameStart,
    aKeyNameEnd,
    aKeyValueStart,
    aKeyValueEnd,
    aQuoted);
end;

constructor TWslConfigItem.CreateNewKey(aName: String; aValue: string);
begin
  CreateKey(
    aName,
    aValue,
    WSLCONFIG_NOT_SET,
    WSLCONFIG_NOT_SET,
    WSLCONFIG_NOT_SET,
    WSLCONFIG_NOT_SET,
    WSLCONFIG_NOT_SET,
    false);
end;

constructor TWslConfigItem.CreateDiscardLine(aLineNumber: integer);
begin
  Create(
    WslconfigDiscardLine,
    WSLCONFIG_VALUE_NOT_SET,
    WSLCONFIG_VALUE_NOT_SET,
    aLineNumber,
    WSLCONFIG_NOT_SET,
    WSLCONFIG_NOT_SET,
    WSLCONFIG_NOT_SET,
    WSLCONFIG_NOT_SET,
    false);
end;

procedure TWslConfigItem.SetName(aName: string);
begin
  FName := aName;
  FIsChanged := true;
end;

procedure TWslConfigItem.SetValue(aValue: string);
begin
  FValue := aValue;
  FIsChanged := true;
end;

{ TWslconfigFile - Helper function }

// Find a section and return index in list
function FindSection(Section: string; ItemList: TWslConfigItemList): integer;
var
  IndexItem: integer;
begin
  Result := -1;

  for IndexItem := 0 to ItemList.Count - 1 do
  begin
    if (ItemList[IndexItem].EntryType = WslconfigSection) and
      (ItemList[IndexItem].Name = Section)
    then begin
      Exit(IndexItem);
    end;
  end;
end;

// Find a section, then find a key and return index in list
function FindKey(Section: string; Key: string; ItemList: TWslConfigItemList): integer;
var
  IndexItem: integer;
  KeyIndex: integer;
begin
  Result := -1;

  IndexItem := FindSection(Section, ItemList);

  if IndexItem <> -1
  then begin
    for KeyIndex := IndexItem + 1 to ItemList.Count - 1 do
    begin
      if ItemList[KeyIndex].EntryType = WslconfigSection
      then begin
        break;
      end else if (ItemList[KeyIndex].EntryType = WslconfigKeyValue) and
        (ItemList[KeyIndex].Name = Key)
      then begin
        Exit(KeyIndex);
      end;
    end;
  end;
end;

// Find a section and return last key index in list
function FindLastKey(Section: string; ItemList: TWslConfigItemList; Start: integer = -1): integer;
var
  IndexItem: integer;
  KeyIndex: integer;
  LastLine: integer;
begin
  Result := -1;

  if Start = -1
  then begin
    IndexItem := FindSection(Section, ItemList);
  end else begin
    IndexItem := Start;
  end;

  if IndexItem <> -1
  then begin
    LastLine := ItemList.Count - 1;

    for KeyIndex := IndexItem + 1 to LastLine do
    begin
      if (ItemList[KeyIndex].EntryType = WslconfigSection) or (KeyIndex = LastLine)
      then begin
        Exit(KeyIndex);
      end;
    end;
  end;
end;

// Return all key name start by index in list
procedure ReadAllKeyName(Start: integer; ItemList: TWslConfigItemList; Strings: TStrings);
var
  IndexItem: integer;
begin
  for IndexItem := Start to ItemList.Count - 1 do
  begin
    // Stop when found a new section
    if ItemList[IndexItem].EntryType = WslconfigSection
    then begin
      Exit;
    end;

    if (ItemList[IndexItem].EntryType = WslconfigKeyValue) and
      (not ItemList[IndexItem].IsDeleted)
    then begin
      Strings.Add(ItemList[IndexItem].Name);
    end;
  end;
end;

// Return all key value start by index in list
procedure ReadAllKeyValue(Start: integer; ItemList: TWslConfigItemList; Strings: TStrings);
var
  IndexItem: integer;
begin
  for IndexItem := Start to ItemList.Count - 1 do
  begin
    // Stop when found a new section
    if ItemList[IndexItem].EntryType = WslconfigSection
    then begin
      Exit;
    end;

    if (ItemList[IndexItem].EntryType = WslconfigKeyValue) and
      (not ItemList[IndexItem].IsDeleted)
    then begin
      Strings.Add(ItemList[IndexItem].Value);
    end;
  end;
end;

// Check if is white space
function IsWhiteSpace(Data: string; Offset: integer): boolean;
begin
  Result := Data[Offset] in [' ', #9, #10, #11, #12, #13];
end;

// Find last non with char. If not found, return last index of char + 1
function FindNextNoWhiteChar(Data: string; Offset: integer = 1): integer;
var
  IndexChar: integer;
  Len: integer;
begin
  Len := Length(Data);

  for IndexChar := Offset to Len do
  begin
    if not IsWhiteSpace(Data, IndexChar)
    then begin
      Exit(IndexChar);
    end;
  end;

  Result := Len + 1;
end;

// Find last white char. If not found, return last index of char + 1
function FindNextWhiteChar(Data: string; Offset: integer = 1): integer;
var
  IndexChar: integer;
  Len: integer;
begin
  Len := Length(Data);

  for IndexChar := Offset to Len do
  begin
    if IsWhiteSpace(Data, IndexChar)
    then begin
      Exit(IndexChar);
    end;
  end;

  Result := Len + 1;
end;

// Find a char. If not found, return last index of char + 1
function FindNextChar(Search: Char; Data: string; Offset: integer = 1): integer;
var
  IndexChar: integer;
  Len: integer;
begin
  Len := Length(Data);

  for IndexChar := Offset to Len do
  begin
    if Data[IndexChar] = Search
    then begin
      Exit(IndexChar);
    end;
  end;

  Result := Len + 1;
end;

// Find '=' of a white char. If not found, return last index of char
function FindNextEqualOrWhiteChar(Data: string; Offset: integer = 1): integer;
var
  IndexChar: integer;
  Len: integer;
begin
  Len := Length(Data);

  for IndexChar := Offset to Len do
  begin
    if (Data[IndexChar] = WSLCONFIG_KEY_VALUE_SEPARATOR) or IsWhiteSpace(Data, IndexChar)
    then begin
      Exit(IndexChar);
    end;
  end;

  Result := Len + 1;
end;

// Extract name of property and return, name, start/stop pos
function ExtractName(CurrentLine: string; KeyNameStart: integer): TKeyData;
begin
  Result.PosStart := KeyNameStart;

  Result.PosEnd := FindNextEqualOrWhiteChar(CurrentLine, KeyNameStart);

  Result.Data := Copy(CurrentLine, KeyNameStart, Result.PosEnd - KeyNameStart);
end;

// Extract value after = sign
function ExtractValue(CurrentLine: string; KeyValueStart: integer): TKeyData;
var
  FirstCharPos: integer;
  LastCharPos: integer;
begin
  // First search no-white chr
  FirstCharPos := FindNextNoWhiteChar(CurrentLine, KeyValueStart);

  if CurrentLine[FirstCharPos] = WSLCONFIG_VALUE_QUOTE
  then begin
    LastCharPos := FindNextChar(WSLCONFIG_VALUE_QUOTE, CurrentLine, FirstCharPos + 1);

    Result.PosStart := FirstCharPos + 1;
    Result.PosEnd := LastCharPos;
    Result.Data := Copy(CurrentLine, FirstCharPos + 1, Result.PosEnd - Result.PosStart);
    Result.Quoted := true;
  end else begin
    LastCharPos := FindNextChar(WSLCONFIG_COMMENT, CurrentLine, FirstCharPos);

    Result.PosStart := FirstCharPos;
    Result.Data := Trim(Copy(CurrentLine, FirstCharPos, LastCharPos - FirstCharPos));
    Result.PosEnd := FirstCharPos + Length(Result.Data);
    Result.Quoted := false;
  end;
end;

// Extract section from line, create a section object and add it in ItemList
procedure ExtractSection(ItemList: TWslConfigItemList; CurrentLine: string; LineIndex: integer);
var
  SectionStart: integer;
  SectionEnd: integer;
  SectionName: string;
  SectionNameSize: integer;
  Item: TWslConfigItem;
begin
  SectionStart := Pos(WSLCONFIG_SECTION_OPEN, CurrentLine);
  SectionEnd := Pos(WSLCONFIG_SECTION_CLOSE, CurrentLine, SectionStart + 1);

  if SectionEnd <> -1
  then begin
    SectionNameSize := SectionEnd - SectionStart - 1;

    SectionName := Trim(Copy(CurrentLine, SectionStart + 1, SectionNameSize));

    Item := TWslConfigItem.CreateSection(
      SectionName,
      LineIndex,
      SectionStart,
      SectionEnd);

    ItemList.Add(Item);
  end;
end;

// Extract key/value from line, create a key object and add it in ItemList
// Key can be:
// a=eeee
// b   =   eeee
// c=   eeeee
// d  =eeeee
// e="eeee"
// f=eeee # comment
procedure ExtractKeyValue(ItemList: TWslConfigItemList; CurrentLine: string; LineIndex: integer);
var
  // To find key name
  EqualSignPos: integer;
  // To find value
  CurrentValue: TKeyData;
  // To extract keyname
  KeyNameStart: integer;
  CurrentKey: TKeyData;
  // Item to add in FItemList
  Item: TWslConfigItem;
begin
  EqualSignPos := FindNextChar(WSLCONFIG_KEY_VALUE_SEPARATOR, CurrentLine);

  if EqualSignPos <> WSLCONFIG_NOT_SET
  then begin
    KeyNameStart := FindNextNoWhiteChar(CurrentLine);
    CurrentKey := ExtractName(CurrentLine, KeyNameStart);
    CurrentValue := ExtractValue(CurrentLine, EqualSignPos + 1);

    Item := TWslConfigItem.CreateKey(
      CurrentKey.Data,
      CurrentValue.Data,
      LineIndex,
      CurrentKey.PosStart,
      CurrentKey.PosEnd,
      CurrentValue.PosStart,
      CurrentValue.PosEnd,
      CurrentValue.Quoted);

    ItemList.Add(Item);
  end;

end;

function CreateLineFromNewItem(Item: TWslConfigItem): string;
begin
  Result := '<oh, oh, this is an error>';

  case Item.EntryType of
    WslconfigKeyValue:
      begin
        Result := Format('%s=%s', [Item.Name, Item.Value]);
      end;
    WslconfigSection:
      begin
        Result := Format('[%s]', [Item.Name]);
      end;
  end;
end;

// Existing section/key with modification
function CreateLineFromExistingItem(FLines: TStrings; Item: TWslConfigItem): string;
var
  SizeData: integer;
  Line: string;
begin
  Line := FLines[Item.LineNumber];

  // Copy part before keyname
  Result := Copy(Line, 1, Item.KeyNameStart - 1);

  // Add keyname
  Result := Result + Item.Name;

  // Copy part between keyname and value
  SizeData := Item.KeyValueStart - Item.KeyNameEnd;
  Result := Result + Copy(Line, Item.KeyNameEnd, SizeData);

  // If quoted, don't check value
  if Item.Quoted
  then begin
    Result := Result + Item.Value;
  end else begin
    // Check if data contains comment
    if Pos(Item.Value, WSLCONFIG_COMMENT) = 0
    then begin
      // No comment found
      Result := Result + Item.Value;
    end else begin
      Result := Result + WSLCONFIG_VALUE_QUOTE + Item.Value + WSLCONFIG_VALUE_QUOTE;
    end;
  end;

  // Copy rest of data
  // Item.KeyValueEnd is position after last char of value
  // We need do a -1 to get it
  SizeData := Length(Line) - (Item.KeyValueEnd - 1);
  Result := Result + Copy(Line, Item.KeyValueEnd, SizeData);

  Result := Result + '';
end;

{ TWslconfigFile }

constructor TWslconfigFile.Create(const AFileName: string);
begin
  FLines := TStringList.Create;

  FFileName := AFileName;
  FItemList := TWslConfigItemList.Create();

  if FileExists(FFileName)
  then begin
    FLines.LoadFromFile(FFileName);

    ReloadFile;
  end;
end;

destructor TWslconfigFile.Destroy;
begin
  inherited Destroy;

  FItemList.Free;
  FLines.Free;
end;

function TWslconfigFile.ReadString(const Section, Ident, Default: string): string;
var
  IndexItem: integer;
begin
  IndexItem := FindKey(Section, Ident, FItemList);

  if (IndexItem = -1) or (FItemList[IndexItem].IsDeleted)
  then begin
    Result := Default;
  end else begin
    Result := FItemList[IndexItem].Value;
  end;
end;

procedure TWslconfigFile.WriteString(const Section, Ident, Value: String);
var
  IndexItem: integer;
  LastIndexItem: integer;
  Item: TWslConfigItem;
begin
  IndexItem := FindKey(Section, Ident, FItemList);

  if IndexItem = -1
  then begin
    // Key not found, search if section exists to add in this section
    IndexItem := FindSection(Section, FItemList);

    if IndexItem = -1
    then begin
      // Section and key don't exists, create all
      Item := TWslConfigItem.CreateNewSection(Section);

      FItemList.Add(Item);

      Item := TWslConfigItem.CreateNewKey(Ident, Value);

      FItemList.Add(Item);
    end else begin
      // Section exists but not the key

      // If section was previously deleted, restore it.
      FItemList[IndexItem].IsDeleted := false;

      // Add key at end of section
      LastIndexItem := FindLastKey(Section, FItemList, IndexItem + 1);

      Item := TWslConfigItem.CreateNewKey(Ident, Value);

      FItemList.Insert(LastIndexItem + 1, Item);
    end;
  end else begin
    // If key was previously deleted, restore it and restore section.
    if FItemList[IndexItem].IsDeleted
    then begin
      FItemList[FindSection(Section, FItemList)].IsDeleted := false ;
      FItemList[IndexItem].IsDeleted := false;
    end;

    FItemList[IndexItem].Value := Value;
  end;
end;

procedure TWslconfigFile.ReadSection(const Section: string; Strings: TStrings);
var
  IndexItem: integer;
begin
  IndexItem := FindSection(Section, FItemList);

  if IndexItem <> -1
  then begin
    ReadAllKeyName(IndexItem + 1, FItemList, Strings);
  end;
end;

procedure TWslconfigFile.ReadSections(Strings: TStrings);
var
  IndexItem: integer;
begin
  for IndexItem := 0 to FItemList.Count - 1 do
  begin
    if (FItemList[IndexItem].EntryType = WslconfigSection) and
      not FItemList[IndexItem].IsDeleted
    then begin
      Strings.Add(FItemList[IndexItem].Name);
    end;
  end;
end;

procedure TWslconfigFile.ReadSectionValues(const Section: string; Strings: TStrings);
var
  IndexItem: integer;
begin
  IndexItem := FindSection(Section, FItemList);

  if IndexItem <> -1
  then begin
    ReadAllKeyValue(IndexItem + 1, FItemList, Strings);
  end;
end;

procedure TWslconfigFile.EraseSection(const Section: string);
var
  IndexItem: integer;
  KeyIndex: integer;
begin
  IndexItem := FindSection(Section, FItemList);

  if IndexItem <> -1
  then begin
    // Delete section
    FItemList[IndexItem].IsDeleted := true;

    for KeyIndex := IndexItem + 1 to FItemList.Count - 1 do
    begin
      if FItemList[KeyIndex].EntryType = WslconfigSection
      then begin
        break;
      end else begin
        FItemList[KeyIndex].IsDeleted := true;
      end;
    end;
  end;
end;

procedure TWslconfigFile.DeleteKey(const Section, Ident: String);
var
  IndexItem: integer;
begin
  IndexItem := FindKey(Section, Ident, FItemList);

  if IndexItem <> -1
  then begin
    FItemList[IndexItem].IsDeleted := true;
  end;
end;

procedure TWslconfigFile.UpdateFile;
var
  // The new lines
  NewLines: TStrings;
  // Index of current item to copy or update
  ItemIndex: integer;
  // Current item
  Item: TWslConfigItem;
begin
  // We create a new StringsList, to copy data from previous to the new List.
  NewLines := TStringList.Create;

  for ItemIndex := 0 to FItemList.Count - 1 do
  begin
    Item := FItemList[ItemIndex];

    if not Item.IsDeleted
    then begin
      if Item.LineNumber = WSLCONFIG_NOT_SET
      then begin
        NewLines.Add(CreateLineFromNewItem(Item));
      end else if Item.IsChanged
      then begin
        // Existing section/key with modification
        NewLines.Add(CreateLineFromExistingItem(FLines, Item));
      end else begin
        // Existing section/key without modification
        NewLines.Add(FLines[Item.LineNumber]);
      end;
    end;
  end;

  FLines.AddStrings(NewLines, true);
  NewLines.SaveToFile(FFileName);
  NewLines.Free;

  ReloadFile;
end;

// Reload data from Fline
procedure TWslconfigFile.ReloadFile;
var
  // Current line index
  LineIndex: integer;
  // Clear line
  ClearLine: string;
  Item: TWslConfigItem;
begin
  FItemList.Clear;

  for LineIndex := 0 to FLines.Count - 1 do
  begin
    ClearLine := Trim(FLines[LineIndex]);

    if Length(ClearLine) = 0
    then begin
      Item := TWslConfigItem.CreateDiscardLine(LineIndex);

      FItemList.Add(Item);
    end else begin
      case ClearLine[1] of
        WSLCONFIG_SECTION_OPEN: ExtractSection(FItemList, FLines[LineIndex], LineIndex);
        WSLCONFIG_COMMENT:
          begin
            Item := TWslConfigItem.CreateDiscardLine(LineIndex);

            FItemList.Add(Item);
          end;
        else ExtractKeyValue(FItemList, FLines[LineIndex], LineIndex);
      end;
    end;
  end;
end;

function TWslconfigFile.ReadInteger(const Section, Ident: string; Default: integer): integer;
var
  Value: string;
begin
  Value := ReadString(Section, Ident, '');

  if Value = ''
  then begin
    Result := Default;
  end else begin
    Result := StrToInt(Value);
  end;
end;

procedure TWslconfigFile.WriteInteger(const Section, Ident: string; Value: integer);
begin
  WriteString(Section, Ident, IntToStr(Value));
end;

function TWslconfigFile.ReadBool(const Section, Ident: string; Default: boolean): boolean;
var
  Value: string;
begin
  Value := ReadString(Section, Ident, '');

  if Value = ''
  then begin
    Result := Default;
  end else if Value = WSLCONFIG_VALUE_TRUE
  then begin
    Result := true;
  end else begin
    Result := false;
  end;
end;

procedure TWslconfigFile.WriteBool(const Section, Ident: string; Value: boolean);
begin
  if Value
  then begin
    WriteString(Section, Ident, WSLCONFIG_VALUE_TRUE);
  end else begin
    WriteString(Section, Ident, WSLCONFIG_VALUE_FALSE);
  end;
end;

end.

