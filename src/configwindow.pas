unit ConfigWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  EditBtn, Buttons, Spin, ShellApi, WslRegistry, Math, IniPropStorage,
  ApplicationConfig;

type

  { TFormSetup }

  TFormSetup = class(TForm)
    ButtonCancel: TButton;
    ButtonReset: TButton;
    ButtonSave: TButton;
    ComboBoxWslDefaultVersion: TComboBox;
    DirectoryEditDefaultWSLStartFolder: TDirectoryEdit;
    DirectoryEditText: TEdit;
    Image1: TImage;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LabelApplicationConfigFolder: TLabel;
    PanelEditLabelApplicationConfigFolder: TPanel;
    PanelUpperLabelApplicationConfigFolder: TPanel;
    PanelButtonCancel: TPanel;
    PanelButtonOk: TPanel;
    PanelButtonReset: TPanel;
    PanelButtons: TPanel;
    DirectoryEditButton: TSpeedButton;
    SpinEditTimerInterval: TSpinEdit;
    procedure ButtonSaveClick(Sender: TObject);
    procedure DirectoryEditButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    // Don't free it!
    AppProps: TIniPropStorage;

  public
    constructor Create(TheOwner: TComponent; TheAppProps: TIniPropStorage); overload;

  end;

var
  FormSetup: TFormSetup;

implementation

{$R *.lfm}

{ TFormSetup }
constructor TFormSetup.Create(TheOwner: TComponent; TheAppProps: TIniPropStorage);
begin
  Inherited Create(TheOwner);
  AppProps := TheAppProps;
end;

procedure LoadOpenFolderIcon(aApplication: TApplication; aImageList: TImageList;
  aSpeedButton: TSpeedButton; DefaultWSLStartFolder: TDirectoryEdit);
var
  OpenFolderIcon : TIcon;
begin
  OpenFolderIcon := TIcon.Create;

  OpenFolderIcon.Handle := ExtractIcon(
    aApplication.Handle,
    PChar('C:\Windows\System32\imageres.dll'),
    3);

  aImageList.Width := 16 ;
  aImageList.Height := 16;
  aImageList.AddIcon(OpenFolderIcon);

  aSpeedButton.ImageIndex := 0;
  aSpeedButton.Images := aImageList;

  openFolderIcon.Free;

  DefaultWSLStartFolder.ImageIndex := 0;
  DefaultWSLStartFolder.Images := aImageList;
end;

procedure SetAppConfig(aEditText: TEdit);
var
  CfgFile: string;
begin
  CfgFile := GetAppConfigFile(False, False);
  aEditText.Text := ExtractFileDir(CfgFile);
end;

procedure TFormSetup.FormCreate(Sender: TObject);
begin
  LoadOpenFolderIcon(
    Application,
    ImageList1,
    DirectoryEditButton,
    DirectoryEditDefaultWSLStartFolder);
  SetAppConfig(DirectoryEditText);
  ComboBoxWslDefaultVersion.ItemIndex := Max(GetDefaultWslVersion - 1, 0);
  SpinEditTimerInterval.Value := AppProps.ReadInteger(
    TIMER_VALUE_KEY,
    DEFAULT_TIMER_VALUE);
  DirectoryEditDefaultWSLStartFolder.Directory := AppProps.ReadString(
    WSL_START_FOLDER_KEY,
    DEFAULT_WSL_START_FOLDER_VALUE);
end;

procedure TFormSetup.DirectoryEditButtonClick(Sender: TObject);
begin
  SysUtils.ExecuteProcess(Pchar('explorer.exe'), PChar(DirectoryEditText.Text), []);
end;

procedure TFormSetup.ButtonSaveClick(Sender: TObject);
begin
  WslRegistry.SetDefaultWslVersion(ComboBoxWslDefaultVersion.ItemIndex + 1);
  AppProps.WriteInteger(
    ApplicationConfig.TIMER_VALUE_KEY,
    SpinEditTimerInterval.Value);
  Close;
  AppProps.WriteString(
    WSL_START_FOLDER_KEY,
    DirectoryEditDefaultWSLStartFolder.Directory);
end;

end.

