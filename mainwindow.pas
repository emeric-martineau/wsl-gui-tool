unit mainwindow;

{$mode objfpc}{$H+}

// Icon from https://icons8.com/icon/set/stop/windows

// TODO sort tlistview

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  AboutWindow, ActnList, DistributionPropertiesWindow,
  // For MB_xxxx dialog flags
  LCLType,
  // Wsl interface
  WslCommandLine;

type

  { TWslGuiToolMainWindow }

  TWslGuiToolMainWindow = class(TForm)
    CheckIfWslIsInstalled: TAction;
    ActionList1: TActionList;
    IconListWslDistributionList: TImageList;
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
    procedure LoadWslDistributionInList(Sender: TObject);
    procedure ToolButtonAboutClick(Sender: TObject);
    procedure ToolButtonPropertiesClick(Sender: TObject);
    procedure ToolButtonRunClick(Sender: TObject);
    procedure ToolButtonStopClick(Sender: TObject);
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
  Self.LoadWslDistributionInList(Sender);
end;

procedure TWslGuiToolMainWindow.LoadWslDistributionInList(Sender: TObject);
var
  WslDistList: TWslCommandLineDistributionList;
  i : integer;
begin
  WslDistList := ListDistribution();

  WslDistributionList.Items.Clear;

  for i := 0 to WslDistList.Count - 1 do
  begin
    AddDistributionInListView(WslDistributionList, WslDistList[i]);
  end;
end;

procedure TWslGuiToolMainWindow.RefreshWslDistributionInList(Sender: TObject);
var
  CurrentDistribution: TListItem;
  WslDistList: TWslCommandLineDistributionList;
  i : integer;
  CurrentName: string;
begin
  WslDistList := ListDistribution();

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
       WslDistributionList.Items.Delete(i);
    end;
  end;
end;

procedure TWslGuiToolMainWindow.ToolButtonAboutClick(Sender: TObject);
var
  About: TFormAbout;
begin
  About := TFormAbout.Create(Self);

  About.ShowModal;

  About.Free;
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

  RefreshWslDistributionInList(Sender);
end;

procedure TWslGuiToolMainWindow.ToolButtonRunClick(Sender: TObject);
begin
  StartDistribution(
    ExtractDistributionName(
      WslDistributionList.Selected.Caption));

  RefreshWslDistributionInList(Sender);
end;

procedure TWslGuiToolMainWindow.ToolButtonStopClick(Sender: TObject);
begin
  StopDistribution(
    ExtractDistributionName(
      WslDistributionList.Selected.Caption));

  RefreshWslDistributionInList(Sender);
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
      ToolButtonStop.Enabled := true;
    end else begin
      ToolButtonRun.Enabled := true;
      ToolButtonStop.Enabled := false;
    end;
  end;

  ToolButtonProperties.Enabled := Selected;
end;

end.

