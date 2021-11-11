unit processresultdisplay;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TFormProcessResultDisplay }

  TFormProcessResultDisplay = class(TForm)
    ButtonClose: TButton;
    Memo1: TMemo;
    PanelButton: TPanel;
  private

  public
    procedure SetLog(Executable: string; Parameters: TStrings; ExistStatus: integer; StdOut: TStrings; StdErr: TStrings);

  end;

var
  FormProcessResultDisplay: TFormProcessResultDisplay;

implementation

{$R *.lfm}

{ TFormProcessResultDisplay }

procedure TFormProcessResultDisplay.SetLog(Executable: string; Parameters: TStrings; ExistStatus: integer; StdOut: TStrings; StdErr: TStrings);
var
  params: string;
  i: integer;
begin
  Memo1.Clear;

  if Parameters.Count > 0
  then begin
    params := Parameters[0];
  end else begin
    params := '';
  end;

  for i := 1 to Parameters.Count - 1 do
  begin
    params := Format('%s %s', [params, Parameters[i]]);
  end;

  Memo1.Lines.Add(Format('$ %s %s', [Executable, params]));
  Memo1.Lines.Add(Format('Exit: %d', [ExistStatus]));
  Memo1.Lines.Add('');
  Memo1.Lines.Add('Stdout');
  Memo1.Lines.Add('--------------------------------------------------------------------------------');
  Memo1.Lines.AddStrings(Stdout);
  Memo1.Lines.Add('--------------------------------------------------------------------------------');
  Memo1.Lines.Add('Stderr');
  Memo1.Lines.Add('--------------------------------------------------------------------------------');
  Memo1.Lines.AddStrings(Stderr);
  Memo1.Lines.Add('--------------------------------------------------------------------------------');
end;

end.

