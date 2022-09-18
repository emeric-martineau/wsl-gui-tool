unit ConfigWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  EditBtn, Buttons, ShellApi;

type

  { TFormSetup }

  TFormSetup = class(TForm)
    ButtonCancel: TButton;
    ButtonReset: TButton;
    ButtonSave: TButton;
    DirectoryEditText: TEdit;
    ImageList1: TImageList;
    LabelApplicationConfigFolder: TLabel;
    PanelEditLabelApplicationConfigFolder: TPanel;
    PanelUpperLabelApplicationConfigFolder: TPanel;
    PanelButtonCancel: TPanel;
    PanelButtonOk: TPanel;
    PanelButtonReset: TPanel;
    PanelButtons: TPanel;
    DirectoryEditButton: TSpeedButton;
    procedure DirectoryEditButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  FormSetup: TFormSetup;

implementation

{$R *.lfm}

{ TFormSetup }
procedure LoadOpenFolderIcon(aApplication: TApplication; aImageList: TImageList; aSpeedButton: TSpeedButton);
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
  LoadOpenFolderIcon(Application, ImageList1, DirectoryEditButton);
  SetAppConfig(DirectoryEditText);
end;

procedure TFormSetup.DirectoryEditButtonClick(Sender: TObject);
begin
  SysUtils.ExecuteProcess(Pchar('explorer.exe'), PChar(DirectoryEditText.Text), []);
end;

end.

