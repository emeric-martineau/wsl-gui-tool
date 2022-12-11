unit runcommandwithuserwindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  EditBtn;

type

  { TFormRunCommandWithUser }

  TFormRunCommandWithUser = class(TForm)
    ButtonRun: TButton;
    ButtonCancel: TButton;
    CheckBoxDefaultShell: TCheckBox;
    DirectoryEditDefault: TDirectoryEdit;
    Label1: TLabel;
    LabeledEditUsername: TLabeledEdit;
    LabeledEditCommandLine: TLabeledEdit;
  private

  public
    constructor Create(TheOwner: TComponent; StartFolder: string);
  end;

var
  FormRunCommandWithUser: TFormRunCommandWithUser;

implementation

{$R *.lfm}

constructor TFormRunCommandWithUser.Create(TheOwner: TComponent; StartFolder: string);
begin
  Inherited Create(TheOwner);
  DirectoryEditDefault.Text := StartFolder;
end;

end.

