{
 /***************************************************************************
                        wslconfigparameterctrl.pas
                        --------------------------
 ***************************************************************************/
}
{
@abstract(Provide a GUI component to display wslconfig)
@author(Emeric MARTINEAU)
@created(2021)
}
unit WslconfigParameterCtrl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, ExtCtrls, Graphics, Controls, fgl;

const
  SPACE_BETWEEN_ITEM = 5;

type
  { TWslconfigEntryType }

  TWslconfigEntryType = (EntryString, EntrySize, EntryNumber, EntryBoolean, EntryPath, EntryHeader);

  { TWslconfigEntryParameter }

  TWslconfigEntryParameter = class(TObject)
  public
    Caption: string;
    Key: string;
    Section: string;
    EntryType: TWslconfigEntryType;
    Help: string;
    DefaultValue: string;
    WinMajorVersion: integer;
    WinMinorVersion: integer;

    constructor Create(aCaption: String; aKey: string; aSection: string;
      aType: TWslconfigEntryType; aHelp: string; aDefault: string;
      aWinMajorVersion: integer; aWinMinorVersion: integer);
  end;

  { TWslconfigEntry }

  TWslconfigEntry = class(TObject)
  public
    Caption: string;
    Key: string;
    Section: string;
    EntryType: TWslconfigEntryType;
    Help: string;
    DefaultValue: string;
    WinMajorVersion: integer;
    WinMinorVersion: integer;
    Value: string;

    constructor Create(aCaption: String; aKey: string; aSection: string;
      aValue: string; aType: TWslconfigEntryType; aHelp: string; aDefault: string;
      aWinMajorVersion: integer; aWinMinorVersion: integer);
  end;

  TWslValue = record
    Value: string;
    Changed: boolean;
    Found: boolean;
  end;

  { TWslconfigEntryPanel }

  TWslconfigEntryPanel = class(TObject)
    FPanel: TPanel;
    FCaption: TLabel;
    FKey: string;
    FType: TWslconfigEntryType;
    FHelp: string;
    FDefault: string;
    FWinMajorVersion: integer;
    FWinMinorVersion: integer;
    FValue: string;

    FChanged: boolean;
    FOnChange: TNotifyEvent;

    EditValue: TEdit; // for EntryNumber, EntryString, EntryPath, EntrySize
    ComboValue: TComboBox; // for EntryBoolean, EntrySize
    HelpImage: TImage;
  private
    function GetCaption: string;
    function GetHeight: integer;
    function GetWidth: integer;
    function GetTop: integer;
    procedure SetTop(NewTop: integer);
    function GetCaptionSize: integer;
    procedure SetCaptionSize(NewSize: integer);
    function GetEnable: boolean;
    procedure SetEnable(NewEnable: boolean);
    function GetValue: string;

    procedure OnResizeInput(Sender: TObject);
    procedure OnValueChange(Sender: TObject);
  public
    constructor Create(TheOwner: TWinControl; aCaption: String; aKey: string;
      aValue: string; aType: TWslconfigEntryType; aHelp: string; aDefault: string;
      aWinMajorVersion: integer; aWinMinorVersion: integer; Images: TImageList;
      IndexImage: integer);
    destructor Destroy; override;

    procedure Reset;

    property Caption: string read GetCaption;
    property Key: string read FKey;
    property EntryType: TWslconfigEntryType read FType;
    property Help: string read FHelp;
    property WinMajorVersion: integer read FWinMajorVersion;
    property WinMinorVersion: integer read FWinMinorVersion;
    property DefaultValue: string read FDefault;
    property CaptionSize: integer read GetCaptionSize write SetCaptionSize;
    property Height: integer read GetHeight;
    property Width: integer read GetWidth;
    property Top: integer read GetTop write SetTop;
    property Enable: boolean read GetEnable write SetEnable;
    property Value: string read GetValue;
    property Changed: boolean read FChanged default false;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TWslconfigEntry*List }

  TWslconfigEntryParameterList = specialize TFPGObjectList<TWslconfigEntryParameter>;
  TWslconfigEntryPanelList = specialize TFPGObjectList<TWslconfigEntryPanel>;
  TWslconfigEntryList  = specialize TFPGObjectList<TWslconfigEntry>;

  { TWslConfigPanel }

  TWslConfigPanel = class(TObject)
  private
    FScrollBox: TScrollBox;
    FImages: TImageList;
    FHelpImageIndex: integer;
    FOnChange: TNotifyEvent;
    FOnReset: TNotifyEvent;

    ListWslEntryPanel: TWslconfigEntryPanelList;

    procedure OnValueChange(Sender: TObject);

    function GetParent: TWinControl;
    procedure SetParent(NewParent: TWinControl);
    function GetVisible: boolean;
    procedure SetVisible(NewVisibility: boolean);
    function GetAlign: TAlign;
    procedure SetAlign(NewAlign: TAlign);
    function GetBorderSpacing: TControlBorderSpacing;
    procedure TControlBorderSpacing(NewBorderSpacing: TControlBorderSpacing);
    function GetBorderStyle: TBorderStyle;
    procedure SetBorderStyle(NewStyle: TBorderStyle);
    function GetWidth: integer;
  public
    constructor Create(AOwner: TWinControl);
    destructor Destroy; override;

    procedure AddItems(List: TWslconfigEntryList);
    procedure Reset;
    function GetValue(Key: string): TWslValue;

    property Parent: TWinControl read GetParent write SetParent;
    property Visible: boolean read GetVisible write SetVisible default true;
    property Align: TAlign read GetAlign write SetAlign default alNone;
    property BorderSpacing: TControlBorderSpacing read GetBorderSpacing write TControlBorderSpacing;
    property BorderStyle: TBorderStyle read GetBorderStyle write SetBorderStyle default bsNone;
    property Images: TImageList read FImages write FImages default nil;
    property HelpImageIndex: integer read FHelpImageIndex write FHelpImageIndex default -1;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnReset: TNotifyEvent read FOnReset write FOnReset;
    property Width: integer read GetWidth;
  end;

implementation

{ TWslconfigEntryParameter }

constructor TWslconfigEntryParameter.Create(aCaption: String; aKey: string;
  aSection: string; aType: TWslconfigEntryType; aHelp: string; aDefault: string;
  aWinMajorVersion: integer; aWinMinorVersion: integer);
begin
  Caption := aCaption;
  Key := aKey;
  EntryType := aType;
  Help := aHelp;
  Section := aSection;
  WinMajorVersion := aWinMajorVersion;
  WinMinorVersion := aWinMinorVersion;
  DefaultValue := aDefault;
end;

{ TWslconfigEntry }

constructor TWslconfigEntry.Create(aCaption: String; aKey: string;
  aSection: string; aValue: string; aType: TWslconfigEntryType; aHelp: string;
  aDefault: string; aWinMajorVersion: integer; aWinMinorVersion: integer);
begin
  Caption := aCaption;
  Key := aKey;
  Section := aSection;
  EntryType := aType;
  Help := aHelp;
  DefaultValue := aDefault;
  WinMajorVersion := aWinMajorVersion;
  WinMinorVersion := aWinMinorVersion;
  Value := aValue;
end;

{ TWslconfigEntryPanel - Helper function }

function GenerateItemHint(aHelp: string; aDefault: string; aSupported: boolean;
  aWinMajorVersion: integer; aWinMinorVersion: integer): string;
begin
  if aSupported
  then begin
    Result := Format('%s' + #10 + #13 + 'Default value: %s',
      [aHelp, aDefault]);
  end else begin
    Result := Format('%s' + #10 + #13 + 'Default value: %s' + #10 + #13 +
      'Only available on Windows %d.%d.',
      [aHelp, aDefault, aWinMajorVersion, aWinMinorVersion]);
  end;
end;

{ TWslconfigEntryPanel }

constructor TWslconfigEntryPanel.Create(TheOwner: TWinControl; aCaption: String;
  aKey: string; aValue: string; aType: TWslconfigEntryType; aHelp: string;
  aDefault: string; aWinMajorVersion: integer; aWinMinorVersion: integer;
  Images: TImageList; IndexImage: integer);
begin
  FPanel := TPanel.Create(TheOwner);
  FPanel.Parent := TheOwner;
  FPanel.AutoSize := true;
  FPanel.Visible := true;
  FPanel.Align := alTop;
  FPanel.BorderStyle := bsNone;
  FPanel.BorderWidth := 2;
  FPanel.BevelOuter := bvNone;

  FCaption := TLabel.Create(FPanel);
  FCaption.Parent := FPanel;
  FCaption.AutoSize := false;
  FCaption.Visible := true;
  FCaption.Caption := aCaption;
  FCaption.Width := FCaption.Canvas.TextWidth(aCaption);
  FCaption.Height := FCaption.Canvas.TextHeight(aCaption);
  FCaption.Alignment := taRightJustify;
  FCaption.Layout := tlCenter;

  FKey := aKey;
  FType := aType;
  FHelp := aHelp;
  FDefault := aDefault;
  FWinMajorVersion := aWinMajorVersion;
  FWinMinorVersion := aWinMinorVersion;
  FValue := aValue;
  FOnChange := nil;

  EditValue := TEdit.Create(FPanel);
  EditValue.Parent := FPanel;
  EditValue.Left := FCaption.Width + SPACE_BETWEEN_ITEM;
  EditValue.Visible := false;
  EditValue.BorderStyle := bsSingle;
  EditValue.OnChange := @OnValueChange;

  ComboValue := TComboBox.Create(FPanel);
  ComboValue.Parent := FPanel;
  ComboValue.Left := FCaption.Width + SPACE_BETWEEN_ITEM;
  ComboValue.Visible := false;
  ComboValue.AutoSize := true;
  ComboValue.AutoSelect := true;
  ComboValue.Style := csDropDownList;
  ComboValue.OnChange := @OnValueChange;

  HelpImage := TImage.Create(FPanel);
  HelpImage.Parent := FPanel;
  HelpImage.Picture.Bitmap := nil; // clear previous image
  HelpImage.Hint := GenerateItemHint(aHelp, aDefault, true, FWinMajorVersion, FWinMinorVersion);
  HelpImage.ShowHint := true;

  if (Images = nil) or (IndexImage = -1)
  then begin
    HelpImage.Visible := false;
  end else begin
    HelpImage.Width := Images.Width;
    HelpImage.Height := Images.Height;
    Images.GetBitmap(IndexImage, HelpImage.Picture.Bitmap);
  end;

  if FType in [EntryNumber, EntryString, EntryPath]
  then begin
    EditValue.NumbersOnly := (aType = EntryNumber);
    EditValue.Visible := true;
    EditValue.OnResize := @OnResizeInput; // Need to have same size of Label and Edit
  end else if FType = EntryBoolean
  then begin
    ComboValue.Items.Add('');
    ComboValue.Items.Add('true');
    ComboValue.Items.Add('false');
    ComboValue.Visible := true;
    ComboValue.OnResize := @OnResizeInput; // Need to have same size of Label and Edit
  end else if FType = EntrySize
  then begin
    EditValue.NumbersOnly := true;
    EditValue.Visible := true;

    ComboValue.Left := EditValue.Left + EditValue.Width;

    ComboValue.Items.Add('KB');
    ComboValue.Items.Add('MB');
    ComboValue.Items.Add('GB');
    ComboValue.Items.Add('TB');
    ComboValue.Items.Add('PB');
    ComboValue.OnResize := @OnResizeInput; // Need to have same size of Label and Edit
    ComboValue.Visible := true;
  end else begin
    // EntryHeader
    EditValue.Visible := false;
    ComboValue.Visible := false;
    HelpImage.Visible := false;
    FCaption.Font.Style := [fsBold];
  end;

  Reset;
end;

destructor TWslconfigEntryPanel.Destroy;
begin
  EditValue.Free;
  HelpImage.Free;
  ComboValue.Free;
end;

procedure TWslconfigEntryPanel.OnResizeInput(Sender: TObject);
begin
  FCaption.Height := TControl(Sender).Height;
  HelpImage.Left := TControl(Sender).Left + TControl(Sender).Width + SPACE_BETWEEN_ITEM;
  HelpImage.Top := (TControl(Sender).Height - HelpImage.Height) div 2;
end;

procedure TWslconfigEntryPanel.OnValueChange(Sender: TObject);
begin
  FChanged := true;

  if FOnChange <> nil
  then begin
    FOnChange(Self);
  end;
end;

function TWslconfigEntryPanel.GetCaption: string;
begin
  Result := FCaption.Caption;
end;

function TWslconfigEntryPanel.GetHeight: integer;
begin
  Result := FPanel.Height;
end;

function TWslconfigEntryPanel.GetWidth: integer;
begin
  Result := FPanel.Width;
end;

function TWslconfigEntryPanel.GetTop: integer;
begin
  Result := FPanel.Top;
end;

procedure TWslconfigEntryPanel.SetTop(NewTop: integer);
begin
  FPanel.Top := NewTop;
end;

function TWslconfigEntryPanel.GetCaptionSize: integer;
begin
  Result := FCaption.Width;
end;

procedure TWslconfigEntryPanel.SetCaptionSize(NewSize: integer);
begin
  FCaption.Width := NewSize;

  if FType in [EntryNumber, EntryString, EntryPath]
  then begin
    EditValue.Left := NewSize + SPACE_BETWEEN_ITEM;

    HelpImage.Left := EditValue.Left + EditValue.Width + SPACE_BETWEEN_ITEM;
  end else if FType = EntryBoolean
  then begin
    ComboValue.Left := NewSize + SPACE_BETWEEN_ITEM;

    HelpImage.Left := ComboValue.Left + ComboValue.Width + SPACE_BETWEEN_ITEM;
  end else if FType = EntrySize
  then begin
    EditValue.Left := NewSize + SPACE_BETWEEN_ITEM;
    ComboValue.Left := EditValue.Left + EditValue.Width;

    HelpImage.Left := ComboValue.Left + ComboValue.Width;
  end;
end;

function TWslconfigEntryPanel.GetEnable: boolean;
begin
  Result := FCaption.Enabled;
end;

procedure TWslconfigEntryPanel.SetEnable(NewEnable: boolean);
begin
  FCaption.Enabled := NewEnable;
  EditValue.Enabled := NewEnable;
  ComboValue.Enabled := NewEnable;

  HelpImage.Hint := GenerateItemHint(FHelp, FDefault, NewEnable,
    FWinMajorVersion, FWinMinorVersion);
end;

procedure TWslconfigEntryPanel.Reset;
begin
  if FType in [EntryNumber, EntryString, EntryPath]
  then begin
    EditValue.Text := FValue;
  end else if FType = EntryBoolean
  then begin
    ComboValue.Text := LowerCase(FValue);
  end else if FType = EntrySize
  then begin
    if Length(FValue) > 2
    then begin
      EditValue.Text := Copy(FValue, 1, Length(FValue) - 2);
      ComboValue.Text := Copy(FValue, Length(FValue) - 1, 2);
    end else begin
      EditValue.Text := FValue;
    end;
  end;
end;

function TWslconfigEntryPanel.GetValue: string;
begin
  if FType in [EntryNumber, EntryString, EntryPath]
  then begin
    Result := Trim(EditValue.Text);
  end else if FType = EntryBoolean
  then begin
    Result := ComboValue.Text;
  end else if FType = EntrySize
  then begin
    Result := Trim(EditValue.Text);

    if Length(Result) > 0
    then begin
      Result := Result + ComboValue.Text;
    end;
  end;
end;

{ TWslConfigPanel - Helper function }

function ComputeNewTop(TheList: TWslconfigEntryPanelList): integer;
var LastItem: TWslconfigEntryPanel;
begin
  Result := 0;

  if TheList.Count > 0
  then begin
    LastItem := TheList[TheList.Count - 1];
    Result := LastItem.Top + LastItem.Height;
  end;
end;

{ TWslConfigPanel }

constructor TWslConfigPanel.Create(AOwner: TWinControl);
begin
  FScrollBox := TScrollBox.Create(AOwner);
  FScrollBox.Width := 20;
  FScrollBox.Height := 20;
  FScrollBox.Visible := true;
  FScrollBox.AutoScroll := true;
  FOnChange := nil;
  FOnReset := nil;

  ListWslEntryPanel := TWslconfigEntryPanelList.Create();
end;

destructor TWslConfigPanel.Destroy;
begin
  ListWslEntryPanel.Free;
  FScrollBox.Free;
end;

procedure TWslConfigPanel.SetParent(NewParent: TWinControl);
begin
  FScrollBox.Parent := NewParent;
end;

function TWslConfigPanel.GetParent: TWinControl;
begin
  Result := FScrollBox.Parent;
end;

procedure TWslConfigPanel.SetVisible(NewVisibility: boolean);
begin
  FScrollBox.Visible := NewVisibility;
end;

function TWslConfigPanel.GetVisible: boolean;
begin
  Result := FScrollBox.Visible;
end;

procedure TWslConfigPanel.SetAlign(NewAlign: TAlign);
begin
  FScrollBox.Align := NewAlign;
end;

function TWslConfigPanel.GetAlign: TAlign;
begin
  Result := FScrollBox.Align;
end;

function TWslConfigPanel.GetBorderSpacing: TControlBorderSpacing;
begin
  Result := FScrollBox.BorderSpacing;
end;

procedure TWslConfigPanel.TControlBorderSpacing(NewBorderSpacing: TControlBorderSpacing);
begin
  FScrollBox.BorderSpacing := NewBorderSpacing;
end;

function TWslConfigPanel.GetBorderStyle: TBorderStyle;
begin
  Result := FScrollBox.BorderStyle;
end;

procedure TWslConfigPanel.SetBorderStyle(NewStyle: TBorderStyle);
begin
  FScrollBox.BorderStyle := NewStyle;
end;

procedure TWslConfigPanel.AddItems(List: TWslconfigEntryList);
var
  Item: TWslconfigEntryPanel;
  CurrentItem: TWslconfigEntry;
  Index: integer;
  TextCaptionSize: integer;
begin
  ListWslEntryPanel.Clear;

  TextCaptionSize := 0;

  for Index := List.Count -1 downto 0 do
  begin
    CurrentItem := List[Index];

    Item := TWslconfigEntryPanel.Create(
      FScrollBox,
      CurrentItem.Caption,
      CurrentItem.Key,
      CurrentItem.Value,
      CurrentItem.EntryType,
      CurrentItem.Help,
      CurrentItem.DefaultValue,
      CurrentItem.WinMajorVersion,
      CurrentItem.WinMinorVersion,
      FImages,
      FHelpImageIndex);

    Item.OnChange := @OnValueChange;

    if (Win32MajorVersion < CurrentItem.WinMajorVersion) or
       (Win32MinorVersion < CurrentItem.WinMinorVersion)
    then begin
      Item.Enable := false;
    end;

    if Item.CaptionSize > TextCaptionSize
    then begin
      TextCaptionSize := Item.CaptionSize;
    end;

    ListWslEntryPanel.Add(Item);
  end;

  for Index := 0 to ListWslEntryPanel.Count - 1 do
  begin
    ListWslEntryPanel[Index].CaptionSize := TextCaptionSize;
  end;
end;

procedure TWslConfigPanel.Reset;
var
  Index: integer;
begin
  for Index := 0 to ListWslEntryPanel.Count - 1 do
  begin
    ListWslEntryPanel[Index].Reset;
  end;

  if FOnReset <> nil
  then begin
    FOnReset(Self);
  end;
end;

function TWslConfigPanel.GetValue(Key: string): TWslValue;
var
  Index: integer;
begin
  Result.Value := '';
  Result.Changed := false;
  Result.Found := false;

  for Index := 0 to ListWslEntryPanel.Count - 1 do
  begin
    if Key = ListWslEntryPanel[Index].Key
    then begin
      Result.Value := ListWslEntryPanel[Index].Value;
      Result.Changed := ListWslEntryPanel[Index].Changed;
      Result.Found := true;
    end;
  end;
end;

procedure TWslConfigPanel.OnValueChange(Sender: TObject);
begin
  if FOnChange <> nil
  then begin
    FOnChange(Self);
  end;
end;

function TWslConfigPanel.GetWidth: integer;
begin
  Result := FScrollBox.Width;
end;

end.

