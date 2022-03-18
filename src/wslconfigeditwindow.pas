unit WslConfigEditWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  MaskEdit, EditBtn, Menus, SpinEx, WslConfig, WslconfigParameterCtrl;

type
  { TFormWslconfigEdit }

  TFormWslconfigEdit = class(TForm)
    ButtonCancel: TButton;
    ButtonReset: TButton;
    ButtonSave: TButton;
    ImageListWslconfig: TImageList;
    PanelButtonCancel: TPanel;
    PanelButtonOk: TPanel;
    PanelButtonReset: TPanel;
    PanelButtons: TPanel;
    procedure ButtonResetClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    WslconfigPropertiesPanel: TWslConfigPanel;
    WslList: TWslconfigEntryList;
    Wslconfig: TWslconfigFile;
    WslParameters: TWslconfigEntryParameterList;
    ConfigFilename: string;
    procedure OnValueChange(Sender: TObject);
    procedure OnValueReset(Sender: TObject);
  public
    constructor CreateWslConfigForm(TheOwner: TComponent; aConfigFilename: string; aParameters: TWslconfigEntryParameterList);
  end;

var
  FormWslconfigEdit: TFormWslconfigEdit;



implementation

{$R *.lfm}


{ TFormWslconfigEdit }

constructor TFormWslconfigEdit.CreateWslConfigForm(TheOwner: TComponent; aConfigFilename: string; aParameters: TWslconfigEntryParameterList);
begin
  Inherited Create(TheOwner);

  WslParameters := aParameters;
  ConfigFilename := aConfigFilename;
end;

function InitList(Wslconfig: TWslconfigFile; aParameters: TWslconfigEntryParameterList): TWslconfigEntryList;
var
  Param: TWslconfigEntryParameter;
  Item: TWslconfigEntry;
  Index: integer;
begin
  Result := TWslconfigEntryList.Create;

  for Index := 0 to aParameters.Count - 1 do
  begin
    Param := aParameters[Index];

    Item := TWslconfigEntry.Create(
      Param.Caption,
      Param.Key,
      Param.Section,
      Wslconfig.ReadString(Param.Section, Param.Key, ''),
      Param.EntryType,
      Param.Help,
      Param.DefaultValue,
      Param.WinMajorVersion,
      Param.WinMinorVersion);

    Result.Add(Item);
  end;
end;

procedure TFormWslconfigEdit.FormCreate(Sender: TObject);
begin
  Wslconfig := TWslconfigFile.Create(ConfigFilename);

  WslList := InitList(Wslconfig, WslParameters);

  WslconfigPropertiesPanel := TWslConfigPanel.Create(Self);
  WslconfigPropertiesPanel.Parent := Self;
  WslconfigPropertiesPanel.Align := alClient;
  WslconfigPropertiesPanel.Images := ImageListWslconfig;
  WslconfigPropertiesPanel.HelpImageIndex := 0;
  WslconfigPropertiesPanel.OnChange := @OnValueChange;
  WslconfigPropertiesPanel.OnReset := @OnValueReset;

  WslconfigPropertiesPanel.AddItems(WslList);
end;

procedure TFormWslconfigEdit.ButtonResetClick(Sender: TObject);
begin
  WslconfigPropertiesPanel.Reset;
end;

procedure TFormWslconfigEdit.ButtonSaveClick(Sender: TObject);
var
  Index: integer;
  Key: string;
  Section: string;
  Value: TWslValue;
begin
  // TODO for /etc/wsl.conf, display content of file, not save
  for Index := 0 to WslList.Count - 1 do
  begin
    Key := WslList[Index].Key;
    Section := WslList[Index].Section;
    Value := WslconfigPropertiesPanel.GetValue(Key);

    if Value.Found and Value.Changed
    then begin
      if Length(Value.Value) > 0
      then begin
        Wslconfig.WriteString(Section, Key, Value.Value);
      end else begin
        Wslconfig.DeleteKey(Section, Key);
      end;
    end;
  end;

  Wslconfig.UpdateFile;
end;

procedure TFormWslconfigEdit.FormDestroy(Sender: TObject);
begin
  WslconfigPropertiesPanel.Free;
  Wslconfig.Free;
  WslList.Free;
end;

procedure TFormWslconfigEdit.OnValueChange(Sender: TObject);
begin
  ButtonSave.Enabled := true;
end;

procedure TFormWslconfigEdit.OnValueReset(Sender: TObject);
begin
  ButtonSave.Enabled := false;
end;

end.
