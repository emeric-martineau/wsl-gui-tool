unit prompt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TFormPrompt }

  TFormPrompt = class(TForm)
    ButtonCancel: TButton;
    ButtonOk: TButton;
    EditValue: TEdit;
    LabelError: TLabel;
    LabelPrompt: TLabel;
    PanelSeparator: TPanel;
    PanelPromptValue: TPanel;
    PanelButtons: TPanel;
    PanelButtonsBottom: TPanel;
    procedure FormShow(Sender: TObject);
  private
  public
    constructor Create(AOwner: TComponent; ACaption: string; APrompt: string; ADefault: string); overload;
  end;

  // Callback method. Return false to cancle ok and display error message.
  TPromptCallbackCheck = function(AValue: string; var AErrorMsg: string): boolean;

// Create a prompt window
function Prompt(AOwner: TComponent; ACaption: string; APrompt: string; var ADefault: string; ACallback: TPromptCallbackCheck = nil): boolean;

var
  FormPrompt: TFormPrompt;

const
  MinimumWindowSize: integer = 492;

implementation

{$R *.lfm}

{ TFormPrompt }

procedure TFormPrompt.FormShow(Sender: TObject);
begin
  if LabelPrompt.Width > EditValue.Width
  then begin
    EditValue.AutoSize := false;
    EditValue.Width := LabelPrompt.Width;
  end else if LabelPrompt.Width < EditValue.Width
  then begin
    LabelPrompt.AutoSize := false;
    LabelPrompt.Width := EditValue.Width;
  end;

  if LabelPrompt.Width < MinimumWindowSize
  then begin
    EditValue.AutoSize := false;
    EditValue.Width := MinimumWindowSize;
    LabelPrompt.AutoSize := false;
    LabelPrompt.Width := MinimumWindowSize;
  end;

  Self.Position := poOwnerFormCenter;
end;

Constructor TFormPrompt.Create(AOwner: TComponent; ACaption: string; APrompt: string; ADefault: string); overload;
begin
  inherited Create(AOwner);

  LabelPrompt.Caption := APrompt;
  EditValue.Text := ADefault;
  Self.Caption := ACaption;
  LabelError.Caption := ' ';
end;

function Prompt(AOwner: TComponent; ACaption: string; APrompt: string; var ADefault: string; ACallback: TPromptCallbackCheck = nil): boolean;
var
  p: TFormPrompt;
  e: string;
begin
  p := TFormPrompt.Create(AOwner, ACaption, APrompt, ADefault);

  Result := false;

  while true do
  begin
    if p.ShowModal = mrOk
    then begin
      e := '';

      if (ACallback = nil) or ACallback(p.EditValue.Text, e)
      then begin
        ADefault := p.EditValue.Text;
        Result := True;
        break;
      end else begin
        p.LabelError.Caption := e;
      end;
    end else begin
      break;
    end;
  end;

  p.Free;
end;

end.

