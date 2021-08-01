unit importdistribution;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  EditBtn;

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
    procedure LabeledEditDistributionNameChange(Sender: TObject);
  private
    function ReadFilename: string;
    function ReadDistributionName: string;
    function ReadInstallLocationPath: string;
    function ReadVersion: Integer;
  public
    property Filename: string read ReadFilename;
    property DistributionName: string read ReadDistributionName;
    property InstallLocationPath: string read ReadInstallLocationPath;
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

function TFormImportDistribution.ReadVersion: Integer;
begin
  Result := ComboBoxVersion.ItemIndex + 1;
end;

end.

