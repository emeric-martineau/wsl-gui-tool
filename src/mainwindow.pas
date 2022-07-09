unit MainWindow;

{$mode objfpc}{$H+}

// Icon from https://icons8.com/icon/set/stop/windows

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  AboutWindow, ActnList, DistributionPropertiesWindow, importdistributionwindow,
  RunCommandWithUserWindow, PromptWindow,
  // For MB_xxxx dialog flags
  LCLType, Menus, IniPropStorage,
  // Wsl interface
  WslCommandLine, WslRegistry,
  // Process
  BackgroundProcessProgressBar, Process, processresultdisplay,
  // Wslconfig
  WslConfigGlobal, WslconfigParameterCtrl, WslConfigEditWindow,
  // /etc/wsl.conf
  WslConfigDistribution;

type

  { TWslGuiToolMainWindow }

  TWslGuiToolMainWindow = class(TForm)
    IconListWslDistributionList: TImageList;
    ImageListStatusbar: TImageList;
    ImageListPopupMenu: TImageList;
    AppProps: TIniPropStorage;
    PopupMenuEditEtcWslConf: TMenuItem;
    PopupMenuRunCommandWithUser: TMenuItem;
    PopupMenuProperties: TMenuItem;
    PopupMenuDefault: TMenuItem;
    PopupMenuRun: TMenuItem;
    PopupMenuStop: TMenuItem;
    PopupMenu1: TPopupMenu;
    ExportDialog: TSaveDialog;
    TimerRefreshDistributionList: TTimer;
    ToolButton1: TToolButton;
    ToolButtonWslconfigEdit: TToolButton;
    ToolButtonDuplicate: TToolButton;
    ToolButtonUnregisterDistribution: TToolButton;
    ToolButtonExport: TToolButton;
    ToolButtonImport: TToolButton;
    ToolButtonAbout: TToolButton;
    ToolButtonProperties: TToolButton;
    ToolButton2: TToolButton;
    ToolButtonGeneralProperties: TToolButton;
    ToolButtonStop: TToolButton;
    WslDistributionList: TListView;
    IconListToolbar: TImageList;
    ToolBar1: TToolBar;
    ToolButtonRun: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure PopupMenuDefaultClick(Sender: TObject);
    procedure PopupMenuEditEtcWslConfClick(Sender: TObject);
    procedure PopupMenuRunCommandWithUserClick(Sender: TObject);
    procedure StatusBar1DrawPanel(StatusBar: TStatusBar; {%H-}Panel: TStatusPanel;
      const Rect: TRect);
    procedure TimerRefreshDistributionListTimer(Sender: TObject);
    procedure ToolButtonWslconfigEditClick(Sender: TObject);
    procedure ToolButtonAboutClick(Sender: TObject);
    procedure ToolButtonDuplicateClick(Sender: TObject);
    procedure ToolButtonExportClick(Sender: TObject);
    procedure ToolButtonImportClick(Sender: TObject);
    procedure ToolButtonPropertiesClick(Sender: TObject);
    procedure ToolButtonRunClick(Sender: TObject);
    procedure ToolButtonStopClick(Sender: TObject);
    procedure ToolButtonUnregisterDistributionClick(Sender: TObject);
    procedure WslDistributionListCompare(Sender: TObject; Item1,
      Item2: TListItem; {%H-}Data: Integer; var Compare: Integer);
    procedure WslDistributionListEdited(Sender: TObject; Item: TListItem;
      var AValue: string);
    procedure WslDistributionListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    BackgroundProcessProgressBar: TBackgroundProcessProgressBar;

    // To manage statusbar message
    StatusbarMessage: string;
    StatusbarImageIndex: integer;
    procedure SetStatusbarExport(DistributionName: string);
    procedure SetStatusbarImport(DistributionName: string);
    procedure SetStatusbarError(Message: string);
    procedure SetStatusbarInfo(Message: string);
    procedure SetStatusbarUnregister(DistributionName: string);

    procedure ActivateAppProps;

    // Refresh distribution list
    procedure RefreshWslDistributionInList(Sender: TObject);

    // When only one distribution is selected
    procedure ManageOneSelectedItemInListView(Item: TListItem;
      Selected: Boolean);

    // When many distributions is selected
    procedure ManageManySelectedItemInListView();

    // Manage action of distribution
    procedure ManageOneDistributionActionWithoutState(enable: boolean);
    procedure ManageOneDistributionActionWithState(running: boolean);

    // Job interaction
    procedure ActivateBackgroundProcessButon(Status: boolean);
    procedure BackgroundProcessProgressBarRun();
    procedure BackgroundProcessProgressBarFinished(ExitStatus: integer; Canceled: boolean);
    procedure BackgroundProcessProgressBarStatusbarDblClick(Sender: TObject);
  public
  end;

var
  WslGuiToolMainWindow: TWslGuiToolMainWindow;

const
  IMAGE_INDEX_RUNNING = 0;
  IMAGE_INDEX_STOP = 1;
  IMAGE_INDEX_RUNNING_DEFAULT = 2;
  IMAGE_INDEX_STOP_DEFAULT = 3;

  DISTRIBUTION_TLISTVIEW_COLUMN_NAME = 0;
  DISTRIBUTION_TLISTVIEW_COLUMN_VERSION = 1;
  DISTRIBUTION_TLISTVIEW_COLUMN_BASEREF = 2;

  STATUSBAR_MARGIN_LEFT = 5;
  STATUSBAR_EXPORT_IMAGE_INDEX = 0;
  STATUSBAR_IMPORT_IMAGE_INDEX = 1;
  STATUSBAR_ERROR_IMAGE_INDEX = 2;
  STATUSBAR_OK_IMAGE_INDEX = 3;
  STATUSBAR_UNREGISTER_IMAGE_INDEX = 4;
  STATUSBAR_CANCEL_IMAGE_INDEX = 5;
  STATUSBAR_INFO_IMAGE_INDEX = 6;

implementation

{$R *.lfm}

{ TWslGuiToolMainWindow }

function FindDistributionInListView(WslDistributionList: TListView; DistributionName: string): TListItem;
var
  i: integer;
begin
  Result := nil;

  for i := 0 to WslDistributionList.Items.Count -1 do
  begin
    if WslDistributionList.Items[i].Caption = DistributionName
    then begin
      exit(WslDistributionList.Items[i]);
    end;
  end;
end;

function IsExistsInOutput(DistributionName: string; WslDistList: TWslCommandLineDistributionList): boolean;
var
  i: integer;
begin
  Result := false;

  for i := 0 to WslDistList.Count - 1 do
  begin
    if WslDistList[i].Name = DistributionName
    then begin
      exit(true);
    end;
  end;
end;

procedure AddDistributionInListView(WslDistributionList: TListView; WslDistribution: TWslCommandLineDistribution; BasePath: string);
var
  CurrentDistribution: TListItem;
begin
  CurrentDistribution := WslDistributionList.Items.Add;

  // TODO use CurrentDistribution.Data to know if running or not ?
  if WslDistribution.IsDefault
  then begin
    if WslDistribution.IsRunning
    then begin
      CurrentDistribution.ImageIndex := IMAGE_INDEX_RUNNING_DEFAULT;
    end else begin
      CurrentDistribution.ImageIndex := IMAGE_INDEX_STOP_DEFAULT;
    end;
  end else begin
    if WslDistribution.IsRunning
    then begin
      CurrentDistribution.ImageIndex := IMAGE_INDEX_RUNNING;
    end else begin
      CurrentDistribution.ImageIndex := IMAGE_INDEX_STOP;
    end;
  end;

  CurrentDistribution.Caption := WslDistribution.Name;
  CurrentDistribution.SubItems.Add('%d', [WslDistribution.Version]);
  CurrentDistribution.SubItems.Add(BasePath);
end;

procedure UpdateDistributionInListView(Distribution: TListItem; WslDistribution: TWslCommandLineDistribution; BasePath: string);
begin
  // TODO use CurrentDistribution.Data to know if running or not ?
  if WslDistribution.IsRunning
  then begin
    Distribution.ImageIndex := IMAGE_INDEX_RUNNING;
  end else begin
    Distribution.ImageIndex := IMAGE_INDEX_STOP;
  end;

  if WslDistribution.IsDefault
  then begin
    if WslDistribution.IsRunning
    then begin
      Distribution.ImageIndex := IMAGE_INDEX_RUNNING_DEFAULT;
    end else begin
      Distribution.ImageIndex := IMAGE_INDEX_STOP_DEFAULT;
    end;
  end else begin
      if WslDistribution.IsRunning
      then begin
        Distribution.ImageIndex := IMAGE_INDEX_RUNNING;
      end else begin
        Distribution.ImageIndex := IMAGE_INDEX_STOP;
      end;
  end;

  Distribution.Caption := WslDistribution.Name;
  Distribution.SubItems[DISTRIBUTION_TLISTVIEW_COLUMN_VERSION - 1] := Format('%d', [WslDistribution.Version]);
  Distribution.SubItems[DISTRIBUTION_TLISTVIEW_COLUMN_BASEREF - 1] := BasePath;
end;

function NumberItemSelected(List: TListView): integer;
var idx: integer;
begin
  Result := 0;
  for idx := 0 to List.Items.Count - 1 do
  begin
    if List.Items[idx].Selected
    then begin
      Inc(Result);
    end;
  end;
end;

procedure TWslGuiToolMainWindow.FormCreate(Sender: TObject);
begin
  ActivateAppProps;

  BackgroundProcessProgressBar := TBackgroundProcessProgressBar.Create(Self);

  BackgroundProcessProgressBar.OnRun := @BackgroundProcessProgressBarRun;
  BackgroundProcessProgressBar.OnFinished := @BackgroundProcessProgressBarFinished;
  BackgroundProcessProgressBar.OnDrawPanel := @StatusBar1DrawPanel;
  BackgroundProcessProgressBar.OnDblClick := @BackgroundProcessProgressBarStatusbarDblClick;

  StatusbarImageIndex := -1;
  StatusbarMessage := '';
end;

procedure TWslGuiToolMainWindow.FormDestroy(Sender: TObject);
begin
  BackgroundProcessProgressBar.Free;
end;

procedure TWslGuiToolMainWindow.FormHide(Sender: TObject);
begin
  TimerRefreshDistributionList.Enabled := false;
end;

procedure TWslGuiToolMainWindow.FormShow(Sender: TObject);
begin
  TimerRefreshDistributionList.Enabled := true;
end;

procedure TWslGuiToolMainWindow.FormWindowStateChange(Sender: TObject);
begin
  case WindowState of
    wsNormal: TimerRefreshDistributionList.Enabled := true;
    wsMaximized: TimerRefreshDistributionList.Enabled := true;
    wsMinimized: TimerRefreshDistributionList.Enabled := false;
  end;
end;

procedure TWslGuiToolMainWindow.PopupMenuDefaultClick(Sender: TObject);
begin
  SetDistributionAsDefault(WslDistributionList.Selected.Caption);

  TimerRefreshDistributionList.Enabled := true;
end;

procedure TWslGuiToolMainWindow.PopupMenuEditEtcWslConfClick(Sender: TObject);
var
  WslConfigForm: TFormWslconfigEdit;
  WslParameters: TWslconfigEntryParameterList;
begin
  WslParameters := GenerateWslConfigForDistribution;

  WslConfigForm := TFormWslconfigEdit.CreateWslConfigForm(
    Self,
    Format('/etc/wsl.conf of %s', [WslDistributionList.Selected.Caption]),
    '\\wsl$\' + WslDistributionList.Selected.Caption + '\etc\wsl.conf',
    WslParameters,
    false);

  if WslConfigForm.ShowModal = mrOk
  then begin
    UpdateEtcWslConf(WslDistributionList.Selected.Caption, WslConfigForm.Data.Text);
    SetStatusbarInfo('Content of wsl.conf file was copied in /etc/wsl.conf.');
  end;

  WslConfigForm.Free;

  WslParameters.Free;
end;

procedure TWslGuiToolMainWindow.PopupMenuRunCommandWithUserClick(Sender: TObject
  );
var RunForm : TFormRunCommandWithUser;
begin
  RunForm := TFormRunCommandWithUser.Create(Self);

  RunForm.ShowModal;

  if RunForm.ModalResult = mrOK
  then begin
    StartDistribution(
      WslDistributionList.Selected.Caption,
      RunForm.LabeledEditUsername.Text,
      RunForm.LabeledEditCommandLine.Text,
      RunForm.DirectoryEditDefault.Text,
      RunForm.CheckBoxDefaultShell.Checked);
  end;

  RunForm.Free;
end;

procedure TWslGuiToolMainWindow.StatusBar1DrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
var
  StatusbarImageTop: integer;
  StatusbarTextTop: integer;
begin
  if StatusbarImageIndex > -1
  then begin
    // Center text
    StatusbarTextTop := (Rect.Height - StatusBar.Canvas.TextHeight('Txt')) div 2;

    // Center image
    StatusbarImageTop := (Rect.Height - ImageListStatusbar.Height) div 2;

    ImageListStatusbar.Draw(
      StatusBar.Canvas, Rect.Left + STATUSBAR_MARGIN_LEFT,
      StatusbarImageTop,
      StatusbarImageIndex);

    StatusBar.Canvas.Brush.Style := bsClear; // To have transparent background

    StatusBar.Canvas.TextOut(
      Rect.left + STATUSBAR_MARGIN_LEFT +
      STATUSBAR_MARGIN_LEFT + ImageListStatusbar.Width,
      StatusbarTextTop,
      StatusbarMessage);
  end;
end;

procedure TWslGuiToolMainWindow.TimerRefreshDistributionListTimer(
  Sender: TObject);
begin
  RefreshWslDistributionInList(Sender);
end;

procedure TWslGuiToolMainWindow.ToolButtonWslconfigEditClick(Sender: TObject);
var
  WslConfigForm: TFormWslconfigEdit;
  WslParameters: TWslconfigEntryParameterList;
begin
  WslParameters := GenerateWslConfigForUser;

  WslConfigForm := TFormWslconfigEdit.CreateWslConfigForm(
    Self,
    'Global WSL2 configuration',
    GetUserDir + '.wslconfig',
    WslParameters);
  WslConfigForm.ShowModal;
  WslConfigForm.Free;

  WslParameters.Free;
end;

procedure TWslGuiToolMainWindow.RefreshWslDistributionInList(Sender: TObject);
var
  CurrentDistribution: TListItem;
  WslDistList: TWslCommandLineDistributionList;
  WslRegistryDistList: TWslRegistryDistributionList;
  RegistryDistribution: TWslRegistryDistribution;
  i : integer;
  CurrentName: string;
begin
  WslDistList := ListDistribution();

  WslRegistryDistList := LoadWslFromRegistry();

  WslDistributionList.BeginUpdate;

  for i := 0 to WslDistList.Count - 1 do
  begin
    CurrentDistribution := FindDistributionInListView(WslDistributionList, WslDistList[i].Name);

    RegistryDistribution := FindDistribution(WslRegistryDistList, WslDistList[i].Name);

    if CurrentDistribution = nil
    then begin
      // Add new distribution
      AddDistributionInListView(WslDistributionList, WslDistList[i], RegistryDistribution.BasePath);
    end else begin
      UpdateDistributionInListView(CurrentDistribution, WslDistList[i], RegistryDistribution.BasePath);
    end;
  end;

  // Remove all entry in viewlist that not found in output of command
  i := 0;

  while i < WslDistributionList.Items.Count do
  begin
    CurrentName := WslDistributionList.Items[i].Caption;

    if IsExistsInOutput(CurrentName, WslDistList)
    then begin
      i := i + 1;
    end else begin
      WslDistributionList.Items.Delete(i); // Delete call free() of object
    end;
  end;

  WslDistributionList.EndUpdate;

  if WslDistributionList.SelCount = 1
  then begin
    ManageOneSelectedItemInListView(WslDistributionList.Selected, true);
  end else begin
    ManageOneSelectedItemInListView(WslDistributionList.Selected, false);
  end;

  WslDistList.Free;
  WslRegistryDistList.Free;
end;

procedure TWslGuiToolMainWindow.ToolButtonAboutClick(Sender: TObject);
var
  About: TFormAbout;
begin
  About := TFormAbout.Create(Self);

  About.ShowModal;

  About.Free;
end;

function CheckDistributionName(DistributionName: string; var ErrorMessage: string): boolean;
begin
  if not IsValidDistributionName(DistributionName)
  then begin
    ErrorMessage := 'Distribution name is not valid! Only chars allowed "0-9, "a-z", "A-Z, ".-"';
    Result := false;
  end else if IsDistributionExists(DistributionName)
  then begin
    ErrorMessage := 'Distribution name still exists';
    Result := false;
  end;
end;

procedure TWslGuiToolMainWindow.ToolButtonDuplicateClick(Sender: TObject);
var
  DistributionName: string;
  TempFilename: string;
  CurrentDistribution: TWslRegistryDistribution;
  // For UUID Generator
  GUID: TGuid;
  GUIDResult: HResult;
  UUID: string;
  // Import process
  ExportProcess: TProcess;
  ImportProcess: TProcess;
begin
  DistributionName := Format('%s-Copy', [WslDistributionList.Selected.Caption]);

  if Prompt(
    Self,
    Format('Clone distribution "%s"', [WslDistributionList.Selected.Caption]),
    'Please enter the name of distribution:',
    DistributionName,
    @CheckDistributionName)
  then begin
    TempFilename := GetTempFileName(
      GetTempDir(false),
      'wslguitool-clone-distribution-');

    SetStatusbarExport(WslDistributionList.Selected.Caption);

    ExportProcess := ExportDistribution(
        WslDistributionList.Selected.Caption, TempFilename);

    BackgroundProcessProgressBar.Run(ExportProcess);

    CurrentDistribution := LoadWslOneDistributionFromRegistryByName(WslDistributionList.Selected.Caption);

    GUIDResult := CreateGuid(GUID);

    if GUIDResult = S_OK
    then begin
      // {B54ED86E-211F-4803-AF46-0586DA66C583}
      UUID := GuidToString(GUID);
      // B54ED86E-211F-4803-AF46-0586DA66C583
      UUID := Copy(UUID, 2, Length(UUID) - 2);

      SetStatusbarImport(DistributionName);

      ImportProcess := WslCommandLine.ImportDistribution(
        DistributionName,
        Format('%s-%s', [CurrentDistribution.BasePath, UUID]),
        CurrentDistribution.Version,
        TempFilename);

      DeleteFile(TempFilename);

      BackgroundProcessProgressBar.Run(ImportProcess);
    end else begin
      Application.MessageBox(
        'Cannot generate UUID to clone distribution! Sorry :(',
        'Error',
        MB_OK + MB_ICONERROR);
    end;
  end;
end;

procedure TWslGuiToolMainWindow.ToolButtonExportClick(Sender: TObject);
var ExportProcess: TProcess;
begin
  if ExportDialog.Execute
  then begin
    SetStatusbarExport(WslDistributionList.Selected.Caption);

    ExportProcess := ExportDistribution(
      WslDistributionList.Selected.Caption, ExportDialog.FileName);

    BackgroundProcessProgressBar.Run(ExportProcess);
  end;
end;

procedure TWslGuiToolMainWindow.ToolButtonImportClick(Sender: TObject);
var
  FormImportDistribution: TFormImportDistribution;
  ImportProcess: TProcess;
begin
  FormImportDistribution := TFormImportDistribution.Create(Self);

  if FormImportDistribution.ShowModal = mrOk
  then begin
    SetStatusbarImport(FormImportDistribution.DistributionName);

    // Check if DistributionName still exists done in previous form.
    ImportProcess := WslCommandLine.ImportDistribution(
      FormImportDistribution.DistributionName,
      FormImportDistribution.InstallLocationPath,
      FormImportDistribution.Version,
      FormImportDistribution.Filename);

    BackgroundProcessProgressBar.Run(ImportProcess);
  end;

  FormImportDistribution.Free;
end;

procedure TWslGuiToolMainWindow.ToolButtonPropertiesClick(Sender: TObject);
var
  DistributionProperties : TFormDistributionProperties;
begin
  DistributionProperties := TFormDistributionProperties.Create(Self,
    WslDistributionList.Selected.Caption);

  DistributionProperties.ShowModal;

  DistributionProperties.Free;
end;

procedure TWslGuiToolMainWindow.ToolButtonRunClick(Sender: TObject);
var idx: integer;
begin
  for idx := 0 to WslDistributionList.Items.Count -1 do
  begin
    if WslDistributionList.Items[idx].Selected
    then begin
      // TODO check return of StartDistribution
      StartDistribution(
        WslDistributionList.Items[idx].Caption);
    end;
  end;
end;

procedure TWslGuiToolMainWindow.ToolButtonStopClick(Sender: TObject);
var idx: integer;
begin
  for idx := 0 to WslDistributionList.Items.Count -1 do
  begin
    if WslDistributionList.Items[idx].Selected
    then begin
      // TODO check return of StopDistribution
      StopDistribution(
        WslDistributionList.Items[idx].Caption);
    end;
  end;
end;

procedure TWslGuiToolMainWindow.ToolButtonUnregisterDistributionClick(
  Sender: TObject);
var
  DistributionName: string;
  QuestionMessage: string;
  UnregisterProcess: TProcess;
begin
  DistributionName := WslDistributionList.Selected.Caption;
  QuestionMessage := Format('Do you really want remove distribution "%s"', [DistributionName]);

  if Application.MessageBox(PChar(QuestionMessage), 'Delete', MB_YESNO + MB_ICONQUESTION) = mrYes
  then begin
    SetStatusbarUnregister(DistributionName);

    UnregisterProcess := UnregisterDistribution(DistributionName);

    BackgroundProcessProgressBar.Run(UnregisterProcess);
  end;
end;

procedure TWslGuiToolMainWindow.WslDistributionListCompare(Sender: TObject;
  Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
var
  MyList: TlistView;
begin
  MyList := (Sender as TlistView);

  if MyList.SortColumn = 0
  then begin
    Compare := CompareText(Item1.Caption, Item2.Caption);
  end else if MyList.SortColumn = DISTRIBUTION_TLISTVIEW_COLUMN_VERSION
  then begin
    Compare := StrToInt(Item1.SubItems[DISTRIBUTION_TLISTVIEW_COLUMN_VERSION - 1]) - StrToInt(Item2.SubItems[DISTRIBUTION_TLISTVIEW_COLUMN_VERSION - 1])
  end else if MyList.SortColumn = DISTRIBUTION_TLISTVIEW_COLUMN_BASEREF
  then begin
    Compare := CompareText(Item1.SubItems[DISTRIBUTION_TLISTVIEW_COLUMN_VERSION - 1], Item2.SubItems[DISTRIBUTION_TLISTVIEW_COLUMN_VERSION - 1])
  end;

  if MyList.SortDirection = sdDescending
  then begin
    Compare := - Compare;
  end;
end;

procedure TWslGuiToolMainWindow.WslDistributionListEdited(Sender: TObject;
  Item: TListItem; var AValue: string);
var WslDistribution: TWslRegistryDistribution;
begin
  // If edit is canceled
  if Item.Caption = AValue
  then begin
    exit;
  end;

  if IsDistributionExists(AValue)
  then begin
    Application.MessageBox(
      PChar(
        Format('The "%s" distribution name already exists!', [AValue])),
      'Error',
      MB_OK + MB_ICONERROR);

    AValue := Item.Caption;
  end else begin
    WslDistribution := LoadWslOneDistributionFromRegistryByName(Item.Caption);
    WslDistribution.Name := AValue;

    SaveDistributionToRegistry(WslDistribution);

    WslDistribution.Free;
  end;
end;

procedure TWslGuiToolMainWindow.WslDistributionListSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  TimerRefreshDistributionList.Enabled := false;

  if NumberItemSelected(TListView(Sender)) > 1
  then begin
    ManageManySelectedItemInListView();
  end else begin
    ManageOneSelectedItemInListView(Item, Selected);
  end;

  TimerRefreshDistributionList.Enabled := true;
end;

procedure TWslGuiToolMainWindow.ManageOneSelectedItemInListView(Item: TListItem;
  Selected: Boolean);
begin
  if Selected
  then begin
    // Event fire when item selected AND unselected
    ManageOneDistributionActionWithState(Item.ImageIndex in [IMAGE_INDEX_RUNNING, IMAGE_INDEX_RUNNING_DEFAULT]);
  end else begin
    ToolButtonRun.Enabled := false;
    PopupMenuRun.Enabled := false;
    PopupMenuRunCommandWithUser.Enabled := false;
    ToolButtonStop.Enabled := false;
    PopupMenuStop.Enabled := false;
    ToolButtonDuplicate.Enabled := false;
    PopupMenuEditEtcWslConf.Enabled := false;
  end;

  ManageOneDistributionActionWithoutState(Selected);
end;

procedure TWslGuiToolMainWindow.ManageManySelectedItemInListView();
begin
  ToolButtonRun.Enabled := true;
  PopupMenuRun.Enabled := true;
  ToolButtonStop.Enabled := true;
  PopupMenuStop.Enabled := true;
  PopupMenuRunCommandWithUser.Enabled := false;
  ToolButtonDuplicate.Enabled := false;

  ManageOneDistributionActionWithoutState(false);
end;

procedure TWslGuiToolMainWindow.ManageOneDistributionActionWithoutState(enable: boolean);
begin
  ToolButtonProperties.Enabled := enable;
  PopupMenuDefault.Enabled := enable;
  PopupMenuProperties.Enabled := enable;

  if not BackgroundProcessProgressBar.Running
  then begin
    ToolButtonExport.Enabled := enable;
    ToolButtonUnregisterDistribution.Enabled := enable;
    ToolButtonDuplicate.Enabled := enable;
  end;
end;

procedure TWslGuiToolMainWindow.ManageOneDistributionActionWithState(running: boolean);
begin
  ToolButtonRun.Enabled := not running;
  PopupMenuRun.Enabled := not running;
  ToolButtonStop.Enabled := running;
  PopupMenuStop.Enabled := running;
  PopupMenuRunCommandWithUser.Enabled := not running;
  PopupMenuEditEtcWslConf.Enabled := running;
end;

procedure TWslGuiToolMainWindow.SetStatusbarExport(DistributionName: string);
begin
  StatusbarMessage := Format('Export distribution "%s" in progress...', [DistributionName]);
  StatusbarImageIndex := STATUSBAR_EXPORT_IMAGE_INDEX;
  BackgroundProcessProgressBar.Refresh;
end;

procedure TWslGuiToolMainWindow.SetStatusbarImport(DistributionName: string);
begin
  StatusbarMessage := Format('Import distribution "%s" in progress...', [DistributionName]);
  StatusbarImageIndex := STATUSBAR_IMPORT_IMAGE_INDEX;
  BackgroundProcessProgressBar.Refresh;
end;

procedure TWslGuiToolMainWindow.SetStatusbarError(Message: string);
begin
  StatusbarMessage := Message;
  StatusbarImageIndex := STATUSBAR_ERROR_IMAGE_INDEX;
  BackgroundProcessProgressBar.Refresh;
end;

procedure TWslGuiToolMainWindow.SetStatusbarInfo(Message: string);
begin
  StatusbarMessage := Message;
  StatusbarImageIndex := STATUSBAR_INFO_IMAGE_INDEX;
  BackgroundProcessProgressBar.Refresh;
end;

procedure TWslGuiToolMainWindow.SetStatusbarUnregister(DistributionName: string);
begin
  StatusbarMessage := Format('Unregister distribution "%s" in progress...', [DistributionName]);
  StatusbarImageIndex := STATUSBAR_UNREGISTER_IMAGE_INDEX;
  BackgroundProcessProgressBar.Refresh;
end;

procedure TWslGuiToolMainWindow.ActivateAppProps;
var
  CfgFile: string;
begin
  CfgFile := GetAppConfigFile(False, False);
  if ForceDirectories(ExtractFileDir(CfgFile)) then
     with AppProps do begin
       IniFileName := CfgFile;
       Active := True;
     end;
end;

procedure TWslGuiToolMainWindow.BackgroundProcessProgressBarFinished(ExitStatus: integer; Canceled: boolean);
begin
  if Canceled
  then begin
    StatusbarImageIndex := STATUSBAR_CANCEL_IMAGE_INDEX;
    StatusbarMessage := 'Job canceled';
  end else if ExitStatus = 0
  then begin
    StatusbarImageIndex := STATUSBAR_OK_IMAGE_INDEX;
    StatusbarMessage := 'Job successed';
  end else begin
    StatusbarImageIndex := STATUSBAR_ERROR_IMAGE_INDEX;
    StatusbarMessage := 'Job error';
  end;

  ActivateBackgroundProcessButon(true);

  BackgroundProcessProgressBar.Refresh;
end;

procedure TWslGuiToolMainWindow.BackgroundProcessProgressBarStatusbarDblClick(Sender: TObject);
var Log: TFormProcessResultDisplay;
begin
  if BackgroundProcessProgressBar.Running
  then begin
    if Application.MessageBox(
      'Do you want stop the job?', 'Job running',
      MB_YESNO + MB_ICONQUESTION) = mrYes
    then begin
      BackgroundProcessProgressBar.Terminate();
    end;
  end else if not BackgroundProcessProgressBar.Canceled
  then begin
    Log := TFormProcessResultDisplay.Create(Self);

    Log.SetLog(
      BackgroundProcessProgressBar.Executable,
      BackgroundProcessProgressBar.Parameters,
      BackgroundProcessProgressBar.ExitStatus,
      BackgroundProcessProgressBar.Stdout,
      BackgroundProcessProgressBar.Stderr);

    Log.ShowModal;

    Log.Free;
  end;
end;

procedure TWslGuiToolMainWindow.BackgroundProcessProgressBarRun();
begin
  ActivateBackgroundProcessButon(false);
end;

procedure TWslGuiToolMainWindow.ActivateBackgroundProcessButon(Status: boolean);
begin
  ToolButtonExport.Enabled := Status;
  ToolButtonImport.Enabled := Status;
  ToolButtonDuplicate.Enabled := Status;
  ToolButtonUnregisterDistribution.Enabled := Status;
end;

end.

