unit ImportDistributionWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  EditBtn, WslRegistry;

type

  { TFormImportDistribution }

  TFormImportDistribution = class(TForm)
    ButtonCancel: TButton;
    ButtonImport: TButton;
    ComboBoxVersion: TComboBox;
    DirectoryEditInstallLocationPath: TDirectoryEdit;
    EditFileNameFileName: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LabeledEditDistributionName: TLabeledEdit;
    PanelButtonCancel: TPanel;
    PanelButtonOk: TPanel;
    PanelButtonReset: TPanel;
    PanelButtons: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure LabeledEditDistributionNameChange(Sender: TObject);
  private
    function ReadFilename: string;
    function ReadDistributionName: string;
    function ReadInstallLocationPath: string;
    procedure WriteInstallLocationPath(Path: string);
    function ReadVersion: Integer;
    function ReadInitialDir: string;
    procedure WriteInitialDir(Path: string);
  public
    property InitialDir: string read ReadInitialDir write WriteInitialDir;
    property Filename: string read ReadFilename;
    property DistributionName: string read ReadDistributionName;
    property InstallLocationPath: string read ReadInstallLocationPath write WriteInstallLocationPath;
    property Version: Integer read ReadVersion;
  end;

var
  FormImportDistribution: TFormImportDistribution;

implementation

{$R *.lfm}

{ TFormImportDistribution }

// TODO read default version and set in ComboBox

procedure TFormImportDistribution.LabeledEditDistributionNameChange(Sender: TObject);
begin
  ButtonImport.Enabled := (Trim(LabeledEditDistributionName.Text) <> '') and
     (Trim(Self.DirectoryEditInstallLocationPath.Text) <> '') and
     (Trim(Self.EditFileNameFilename.Text) <> '');
end;

procedure TFormImportDistribution.FormCreate(Sender: TObject);
begin
  ComboBoxVersion.ItemIndex := GetDefaultWslVersion - 1;
end;

function TFormImportDistribution.ReadFilename: string;
begin
  Result := Trim(EditFileNameFileName.Text);
end;

function TFormImportDistribution.ReadDistributionName: string;
begin
  Result := Trim(LabeledEditDistributionName.Text);
end;

function TFormImportDistribution.ReadInstallLocationPath: string;
begin
  Result := Trim(DirectoryEditInstallLocationPath.Text);
end;

procedure TFormImportDistribution.WriteInstallLocationPath(Path: string);
begin
  DirectoryEditInstallLocationPath.Text := Path;
end;

function TFormImportDistribution.ReadVersion: Integer;
begin
  Result := ComboBoxVersion.ItemIndex + 1;
end;

function TFormImportDistribution.ReadInitialDir: string;
begin
  Result := EditFileNameFileName.InitialDir;
end;

procedure TFormImportDistribution.WriteInitialDir(Path: string);
begin
  EditFileNameFileName.InitialDir := Path;
end;

end.

