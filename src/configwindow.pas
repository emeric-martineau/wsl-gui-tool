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
    Label1: TLabel;
    Panel1: TPanel;
    PanelUpper: TPanel;
    PanelButtonCancel: TPanel;
    PanelButtonOk: TPanel;
    PanelButtonReset: TPanel;
    PanelButtons: TPanel;
    DirectoryEditButton: TSpeedButton;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  FormSetup: TFormSetup;

implementation

{$R *.lfm}

{ TFormSetup }

procedure TFormSetup.FormCreate(Sender: TObject);
var
  myIcon : TIcon;

begin
//  DirectoryEdit.RootDir := GetAppConfigFile(False, False);
  //DirectoryEdit.Directory := DirectoryEdit1.RootDir;

  myIcon :=   TIcon.Create;

  myIcon.Handle := ExtractIcon(
    Application.Handle,
    PChar('C:\Windows\System32\imageres.dll'),
    3);

  ImageList1.Width := 16 ;
  ImageList1.Height := 16;
  ImageList1.AddIcon(myIcon);
  DirectoryEditButton.ImageIndex := 0;
  DirectoryEditButton.Images := ImageList1;

  myIcon.Free;
end;

end.

