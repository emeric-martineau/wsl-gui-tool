unit distributionpropertieswindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ValEdit,
  ExtCtrls, Buttons, WslRegistry, WslApi, fgl, Grids, MaskEdit,
  // For MB_xxxx dialog flags
  LCLType;

type
  TMapStrings = specialize TFPGMap<String, String>;

  { TFormDistributionProperties }

  TFormDistributionProperties = class(TForm)
    ButtonCancel: TButton;
    ButtonSave: TButton;
    ButtonReset: TButton;
    CheckBoxDriveMounting: TCheckBox;
    CheckBoxInterop: TCheckBox;
    CheckBoxAppendNtPath: TCheckBox;
    ComboBoxVersion: TComboBox;
    EditName: TEdit;
    GroupBoxFlags: TGroupBox;
    ImageListEnv: TImageList;
    Label1: TLabel;
    LabelUserID: TLabel;
    LabelVersion: TLabel;
    LabelName: TLabel;
    LabelEnv: TLabel;
    EditUserID: TMaskEdit;
    Panel1: TPanel;
    PanelUserID: TPanel;
    PanelButtonCancel: TPanel;
    PanelSeparatorVersion: TPanel;
    PanelSeparatorVersion1: TPanel;
    PanelVersion: TPanel;
    PanelSeparatorName: TPanel;
    PanelUpper: TPanel;
    PanelEnv: TPanel;
    PanelButtonOk: TPanel;
    PanelButtonReset: TPanel;
    PanelButtons: TPanel;
    SpeedButtonAdd: TSpeedButton;
    SpeedButtonDuplicate: TSpeedButton;
    SpeedButtonDelete: TSpeedButton;
    ValueListEditorEnv: TValueListEditor;
    procedure ButtonResetClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure CheckWslConfigChange(Sender: TObject);
    procedure EditNameChange(Sender: TObject);
    procedure EditNameKeyPress(Sender: TObject; var Key: char);
    procedure SpeedButtonAddClick(Sender: TObject);
    procedure SpeedButtonDeleteClick(Sender: TObject);
    procedure SpeedButtonDuplicateClick(Sender: TObject);
    procedure ValueListEditorEnvEnter(Sender: TObject);
    procedure ValueListEditorEnvExit(Sender: TObject);
    procedure ValueListEditorEnvSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
  private
    WslDistribution: TWslRegistryDistribution;
    procedure InitScreen();
  public
    constructor Create(AOwner: TComponent; DistributionName: string; DistributionVersion: integer); overload;
    destructor destroy; override;
  end;

var
  FormDistributionProperties: TFormDistributionProperties;

implementation

{$R *.lfm}

function ConvertStringListToMap(Data: TStrings): TMapStrings;
var
  i: integer;
  CurrentData: array of string;
begin
  Result := TMapStrings.Create;

  for i := 0 to Data.Count -1 do
  begin
    CurrentData := Data[i].Split('=', 2);

    Result.Add(CurrentData[0], CurrentData[1]);
  end;
end;

function CompareEnvData(Data1: TStrings; Data2: TStrings): boolean;
var
  Map1: TMapStrings;
  Map2: TMapStrings;
  i: integer;
  CurrentKey: string;
  Map1CurrentValue: string;
  Map2CurrentValue: string;
begin
  Result := Data1.Count = Data2.Count;

  if Result
  then begin
    Map1 := ConvertStringListToMap(Data1);
    Map2 := ConvertStringListToMap(Data2);

    for i := 0 to Map1.Count - 1 do
    begin
      CurrentKey := Map1.Keys[i];

      Map1.TryGetData(CurrentKey, Map1CurrentValue);

      if Map2.TryGetData(CurrentKey, Map2CurrentValue)
      then begin
        if Map1CurrentValue <> Map2CurrentValue
        then begin
          Result := false;
          break;
        end;
      end else begin
        Result := false;
        break;
      end;
    end;

    Map1.Free;
    Map2.Free;
  end;
end;

procedure TFormDistributionProperties.InitScreen();
var
  i: integer;
begin
  if WslDistribution = nil
    then begin
      EditName.Text := '';
      EditName.Enabled := false;
      ComboBoxVersion.Enabled := false;
      ValueListEditorEnv.Enabled := false;

      CheckBoxInterop.Enabled := false;
      CheckBoxAppendNtPath.Enabled := false;
      CheckBoxDriveMounting.Enabled := false;

      ButtonSave.Enabled := false;
    end else begin
      EditName.Text := WslDistribution.Name;
      ComboBoxVersion.ItemIndex := WslDistribution.Version - 1;
      EditUserID.Text := IntToStr(WslDistribution.DefaultUID);

      ValueListEditorEnv.Strings.Clear;

      for i := 0 to WslDistribution.Env.Count - 1 do
      begin
        ValueListEditorEnv.Strings.Add(WslDistribution.Env.Strings[i]);
      end;

      CheckBoxInterop.Checked := (WslDistribution.Flags and WSL_DISTRIBUTION_FLAGS_ENABLE_INTEROP) > 0;
      CheckBoxAppendNtPath.Checked := (WslDistribution.Flags and WSL_DISTRIBUTION_FLAGS_APPEND_NT_PATH) > 0;
      CheckBoxDriveMounting.Checked := (WslDistribution.Flags and WSL_DISTRIBUTION_FLAGS_ENABLE_DRIVE_MOUNTING) > 0;

      CheckWslConfigChange(Self);
    end;
end;

procedure TFormDistributionProperties.SpeedButtonDeleteClick(Sender: TObject);
begin
  ValueListEditorEnv.DeleteRow(
    ValueListEditorEnv.Row);
end;

procedure TFormDistributionProperties.SpeedButtonDuplicateClick(Sender: TObject
  );
begin
  ValueListEditorEnv.Strings.Add(
    ValueListEditorEnv.Strings[ValueListEditorEnv.Row - 1]);
end;

procedure TFormDistributionProperties.ValueListEditorEnvEnter(Sender: TObject);
begin
  SpeedButtonAdd.Enabled := true;
  SpeedButtonDelete.Enabled := true;
  SpeedButtonDuplicate.Enabled := true;
end;

procedure TFormDistributionProperties.ValueListEditorEnvExit(Sender: TObject);
begin
  SpeedButtonAdd.Enabled := false;
  SpeedButtonDelete.Enabled := false;
  SpeedButtonDuplicate.Enabled := false;
end;

procedure TFormDistributionProperties.ValueListEditorEnvSetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  CheckWslConfigChange(Self);
end;

procedure TFormDistributionProperties.SpeedButtonAddClick(Sender: TObject);
begin
  ValueListEditorEnv.Strings.Add(
    Format('<key_%d>=<value>', [ValueListEditorEnv.Row]));
end;

procedure TFormDistributionProperties.CheckWslConfigChange(Sender: TObject);
var
  IdenticalName: boolean;
  IdenticalVersion: boolean;
  IdenticalInterop: boolean;
  IdenticalAppendNtPath: boolean;
  IdenticalDriveMounting: boolean;
begin
  IdenticalName := Trim(EditName.Text) = WslDistribution.Name;
  IdenticalVersion := (ComboBoxVersion.ItemIndex + 1) = WslDistribution.Version;
  IdenticalInterop := CheckBoxInterop.Checked = ((WslDistribution.Flags and WSL_DISTRIBUTION_FLAGS_ENABLE_INTEROP) > 0);
  IdenticalAppendNtPath := CheckBoxAppendNtPath.Checked = ((WslDistribution.Flags and WSL_DISTRIBUTION_FLAGS_APPEND_NT_PATH) > 0);
  IdenticalDriveMounting := CheckBoxDriveMounting.Checked = ((WslDistribution.Flags and WSL_DISTRIBUTION_FLAGS_ENABLE_DRIVE_MOUNTING) > 0);

  ButtonSave.Enabled := not (
    IdenticalName and
    IdenticalVersion and
    IdenticalInterop and
    IdenticalAppendNtPath and
    IdenticalDriveMounting and
    CompareEnvData(ValueListEditorEnv.Strings, WslDistribution.Env)
    );
end;

procedure TFormDistributionProperties.EditNameChange(Sender: TObject);
begin
  CheckWslConfigChange(Sender);
end;

procedure TFormDistributionProperties.EditNameKeyPress(Sender: TObject;
  var Key: char);
begin
  // #8 is Backspace
  if not (Key in [#8, '0'..'9', 'a'..'z', 'A'..'Z', '_'])
  then begin
    Key := #0;
  end;
end;

procedure TFormDistributionProperties.ButtonResetClick(Sender: TObject);
begin
  InitScreen();
end;

procedure TFormDistributionProperties.ButtonSaveClick(Sender: TObject);
var
  Flags: LongWord;
begin
  Flags := WslDistribution.Flags;

  if CheckBoxInterop.Checked
  then begin
    Flags := Flags or WSL_DISTRIBUTION_FLAGS_ENABLE_INTEROP;
  end else begin
    Flags := Flags and( not WSL_DISTRIBUTION_FLAGS_ENABLE_INTEROP);
  end;

  if CheckBoxAppendNtPath.Checked
  then begin
    Flags := Flags or WSL_DISTRIBUTION_FLAGS_APPEND_NT_PATH;
  end else begin
    Flags := Flags and( not WSL_DISTRIBUTION_FLAGS_APPEND_NT_PATH);
  end;

  if CheckBoxDriveMounting.Checked
  then begin
    Flags := Flags or WSL_DISTRIBUTION_FLAGS_ENABLE_DRIVE_MOUNTING;
  end else begin
    Flags := Flags and( not WSL_DISTRIBUTION_FLAGS_ENABLE_DRIVE_MOUNTING);
  end;

  if (Trim(EditName.Text) <> WslDistribution.Name) and
    (Application.MessageBox('Wsl distribution name change! Are you sur ?', 'Caution!', MB_YESNO + MB_ICONWARNING) = mrNo)
  then begin
    exit;
  end;

  WslDistribution.DefaultUID := StrToInt(Trim(EditUserID.Text));
  WslDistribution.Flags := Flags;
  WslDistribution.Name := Trim(EditName.Text);

  SaveDistributionToRegistry(WslDistribution);

  Self.ModalResult := mrOK;
  Self.Close();

  {if WslSetConfigurationOfDistribution(
    WslDistribution.Name,
    StrToInt(
      Trim(EditUserID.Text)),
    Flags)
  then begin
    // Now save env var in registry
    if CompareEnvData(ValueListEditorEnv.Strings, WslDistribution.Env) = false
    then begin

    end;

    Self.ModalResult := mrOK;
    Self.Close();
  end else begin
    Application.MessageBox('Cannot save config!', 'Error', MB_OK + MB_ICONERROR);
  end;}
end;

Constructor TFormDistributionProperties.Create(AOwner: TComponent; DistributionName: string; DistributionVersion: integer);
begin
  inherited Create(AOwner);

  WslDistribution := LoadWslOneDistributionFromRegistryByName(DistributionName);

  // Fucking bug https://github.com/microsoft/WSL/issues/6924
  if DistributionVersion = 1
  then begin
    WslDistribution.Version := DistributionVersion;
  end;

  InitScreen();
end;

destructor TFormDistributionProperties.Destroy;
begin
   if WslDistribution <> nil
   then begin
     WslDistribution.Free;
   end;

   inherited;
end;

end.

