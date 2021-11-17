unit aboutwindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ApplicationInfo;

type

  { TFormAbout }

  TFormAbout = class(TForm)
    ImageLogo: TImage;
    LabelLogo: TLabel;
    LabelIcons: TLabel;
    LabelVersion: TLabel;
    LabelTitle: TLabel;
    LabelCopyright: TLabel;
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  FormAbout: TFormAbout;

implementation

{$R *.lfm}

{ TFormAbout }

procedure TFormAbout.FormShow(Sender: TObject);
var
  f: TForm;
begin
  f := TForm(Sender);

  ImageLogo.Left := (f.Width - ImageLogo.Width) div 2;
  LabelTitle.Left := (f.Width - LabelTitle.Width) div 2;
  LabelVersion.Left := (f.Width - LabelVersion.Width) div 2;
  LabelLogo.Left := (f.Width - LabelLogo.Width) div 2;
  LabelIcons.Left := (f.Width - LabelIcons.Width) div 2;

  LabelCopyright.Left := (f.Width - LabelCopyright.Width) div 2;

  LabelVersion.Caption := 'Version ' + GetAppVersionStr;
end;

end.

