unit mainwindow;

{$mode objfpc}{$H+}

// Icon from https://icons8.com/icon/set/stop/windows

// TODO sort tlistview

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  AboutWindow, ActnList, DistributionPropertiesWindow, ImportDistribution,
  // For MB_xxxx dialog flags
  LCLType, Menus,
  // Wsl interface
  WslCommandLine, WslRegistry;

type

  { TWslGuiToolMainWindow }

  TWslGuiToolMainWindow = class(TForm)
    IconListWslDistributionList: TImageList;
    ImageListPopupMenu: TImageList;
    PopupMenuProperties: TMenuItem;
    PopupMenuDefault: TMenuItem;
    PopupMenuRun: TMenuItem;
    PopupMenuStop: TMenuItem;
    PopupMenu1: TPopupMenu;
    ExportDialog: TSaveDialog;
    TimerRefreshDistributionList: TTimer;
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
    procedure CheckIfWslIsInstalledExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure PopupMenuDefaultClick(Sender: TObject);
    procedure TimerRefreshDistributionListTimer(Sender: TObject);
    procedure ToolButtonAboutClick(Sender: TObject);
    procedure ToolButtonExportClick(Sender: TObject);
    procedure ToolButtonImportClick(Sender: TObject);
    procedure ToolButtonPropertiesClick(Sender: TObject);
    procedure ToolButtonRunClick(Sender: TObject);
    procedure ToolButtonStopClick(Sender: TObject);
    procedure WslDistributionListCompare(Sender: TObject; Item1,
      Item2: TListItem; Data: Integer; var Compare: Integer);
    procedure WslDistributionListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private

  public
    procedure RefreshWslDistributionInList(Sender: TObject);
  end;

var
  WslGuiToolMainWindow: TWslGuiToolMainWindow;

const
  IMAGE_INDEX_RUNNING = 0;
  IMAGE_INDEX_STOP = 1;

implementation

{$R *.lfm}

{ TWslGuiToolMainWindow }

function ExtractDistributionName(Name: string): string;
begin
  Result := Copy(Name, 4, Length(Name) - 3);
end;

function FindDistributionInListView(WslDistributionList: TListView; DistributionName: string): TListItem;
var
  i: integer;
begin
  Result := nil;

  for i := 0 to WslDistributionList.Items.Count -1 do
  begin
    if ExtractDistributionName(WslDistributionList.Items[i].Caption) = DistributionName
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

procedure AddDistributionInListView(WslDistributionList: TListView; WslDistribution: TWslCommandLineDistribution);
var
  CurrentDistribution: TListItem;
begin
  CurrentDistribution := WslDistributionList.Items.Add;

  // TODO use CurrentDistribution.Data to know if running or not ?
  if WslDistribution.IsRunning
  then begin
    CurrentDistribution.ImageIndex := IMAGE_INDEX_RUNNING;
  end else begin
    CurrentDistribution.ImageIndex := IMAGE_INDEX_STOP;
  end;

  if WslDistribution.IsDefault
  then begin
    CurrentDistribution.Caption := ' * ' + WslDistribution.Name;
  end else begin
    CurrentDistribution.Caption := '   ' + WslDistribution.Name;
  end;

  CurrentDistribution.SubItems.Add('%d', [WslDistribution.Version]);
end;

procedure UpdateDistributionInListView(Distribution: TListItem; WslDistribution: TWslCommandLineDistribution);
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
    Distribution.Caption := ' * ' + WslDistribution.Name;
  end else begin
    Distribution.Caption := '   ' + WslDistribution.Name;
  end;

  Distribution.SubItems[0] := Format('%d', [WslDistribution.Version]);
end;

procedure TWslGuiToolMainWindow.CheckIfWslIsInstalledExecute(Sender: TObject);
begin
  if not WslCommandLine.IsWslInstalled()
  then begin
    Application.MessageBox(
      'WSL seems to be not installed!',
      'Error',
      MB_OK + MB_ICONERROR);
    Application.Terminate;
  end;
end;

procedure TWslGuiToolMainWindow.FormCreate(Sender: TObject);
begin
  Self.CheckIfWslIsInstalledExecute(Sender);
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
  SetDistributionAsDefault(
    ExtractDistributionName(
      WslDistributionList.Selected.Caption));

  TimerRefreshDistributionList.Enabled := true;
end;

procedure TWslGuiToolMainWindow.TimerRefreshDistributionListTimer(
  Sender: TObject);
begin
  RefreshWslDistributionInList(Sender);
end;

procedure TWslGuiToolMainWindow.RefreshWslDistributionInList(Sender: TObject);
var
  CurrentDistribution: TListItem;
  WslDistList: TWslCommandLineDistributionList;
  i : integer;
  CurrentName: string;
begin
  WslDistList := ListDistribution();

  WslDistributionList.BeginUpdate;

  for i := 0 to WslDistList.Count - 1 do
  begin
    CurrentDistribution := FindDistributionInListView(WslDistributionList, WslDistList[i].Name);

    if CurrentDistribution = nil
    then begin
      // Add new distribution
      AddDistributionInListView(WslDistributionList, WslDistList[i]);
    end else begin
      UpdateDistributionInListView(CurrentDistribution, WslDistList[i]);
    end;
  end;

  // Remove all entry in viewlist that not found in output of command
  i := 0;

  while i < WslDistributionList.Items.Count do
  begin
    CurrentName := ExtractDistributionName(WslDistributionList.Items[i].Caption);

    if IsExistsInOutput(CurrentName, WslDistList)
    then begin
      i := i + 1;
    end else begin
      WslDistributionList.Items.Delete(i); // Delete call free
    end;
  end;

  WslDistributionList.EndUpdate;

  WslDistList.Free;
end;

procedure TWslGuiToolMainWindow.ToolButtonAboutClick(Sender: TObject);
var
  About: TFormAbout;
begin
  About := TFormAbout.Create(Self);

  About.ShowModal;

  About.Free;
end;

procedure TWslGuiToolMainWindow.ToolButtonExportClick(Sender: TObject);
begin
  if ExportDialog.Execute
  then begin
    ExportDistribution(
      ExtractDistributionName(
        WslDistributionList.Selected.Caption), ExportDialog.FileName);
  end;
end;

procedure TWslGuiToolMainWindow.ToolButtonImportClick(Sender: TObject);
var FormImportDistribution: TFormImportDistribution;
begin
  FormImportDistribution := TFormImportDistribution.Create(Self);

  if FormImportDistribution.ShowModal = mrOk
  then begin

    // TODO check if DistributionName still exists. If yes, display error box.
    WslCommandLine.ImportDistribution(
      FormImportDistribution.DistributionName,
      FormImportDistribution.InstallLocationPath,
      FormImportDistribution.Version,
      FormImportDistribution.Filename);
  end;

  FormImportDistribution.Free;
end;

procedure TWslGuiToolMainWindow.ToolButtonPropertiesClick(Sender: TObject);
var
  DistributionProperties : TFormDistributionProperties;
begin
  DistributionProperties := TFormDistributionProperties.Create(Self,
    ExtractDistributionName(WslDistributionList.Selected.Caption),
    WslDistributionList.Selected.SubItems[0].ToInteger
    );

  DistributionProperties.ShowModal;

  DistributionProperties.Free;
end;

procedure TWslGuiToolMainWindow.ToolButtonRunClick(Sender: TObject);
begin
  StartDistribution(
    ExtractDistributionName(
      WslDistributionList.Selected.Caption));
end;

procedure TWslGuiToolMainWindow.ToolButtonStopClick(Sender: TObject);
begin
  StopDistribution(
    ExtractDistributionName(
      WslDistributionList.Selected.Caption));
end;

procedure TWslGuiToolMainWindow.WslDistributionListCompare(Sender: TObject;
  Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
var
  MyList: TlistView;
begin
  MyList := (Sender as TlistView);

  if MyList.SortColumn = 0
  then begin
    Compare := CompareText(
      ExtractDistributionName(Item1.Caption),
      ExtractDistributionName(Item2.Caption));
  end else if MyList.SortColumn = 1
  then begin
    Compare := StrToInt(Item1.SubItems[0]) - StrToInt(Item2.SubItems[0])
  end;

  if MyList.SortDirection = sdDescending
  then begin
    Compare := - Compare;
  end;
end;

procedure TWslGuiToolMainWindow.WslDistributionListSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected
  then begin
    // Event fire when item selected AND unselected
    if Item.ImageIndex = IMAGE_INDEX_RUNNING
    then begin
      ToolButtonRun.Enabled := false;
      PopupMenuRun.Enabled := false;
      ToolButtonStop.Enabled := true;
      PopupMenuStop.Enabled := true;
    end else begin
      ToolButtonRun.Enabled := true;
      PopupMenuRun.Enabled := true;
      ToolButtonStop.Enabled := false;
      PopupMenuStop.Enabled := false;
    end;
  end else begin
    ToolButtonRun.Enabled := false;
    PopupMenuRun.Enabled := false;
    ToolButtonStop.Enabled := false;
    PopupMenuStop.Enabled := false;
  end;

  ToolButtonProperties.Enabled := Selected;
  PopupMenuDefault.Enabled := Selected;
  PopupMenuProperties.Enabled := Selected;
  ToolButtonExport.Enabled := Selected;
end;

end.

