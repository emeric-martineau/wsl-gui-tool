unit WslConfigEditWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, WslConfig;

type

  { TFormWslconfigEdit }

  TFormWslconfigEdit = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

  TWslconfigEntryType = (EntryString, EntrySize, EntryNumber, EntryBoolean);

  TWslconfigEntry = class(TObject)
    FCaption: string;
    FKey: string;
    FType: TWslconfigEntryType;
    FHelp: string;
    FPreview: boolean;
  end;

var
  FormWslconfigEdit: TFormWslconfigEdit;

implementation

{$R *.lfm}

{ TFormWslconfigEdit }

procedure TFormWslconfigEdit.FormCreate(Sender: TObject);
var
  Wslconfig: TWslconfigFile;
begin
  Wslconfig := TWslconfigFile.Create(GetUserDir + 'wslconfig.test');

  Memo1.Lines.Clear;

  Wslconfig.WriteString('wsl4', 'test_emeric', '123');

  Memo1.Lines.Add('--- All section ----');
  Wslconfig.ReadSections(Memo1.Lines);

  Memo1.Lines.Add('--- All [wsl2] keys ----');
  Wslconfig.ReadSection('wsl2', Memo1.Lines);

  Memo1.Lines.Add('--- Delete key "test_quote" ----');
  Wslconfig.DeleteKey('wsl2', 'test_quote');
  Wslconfig.ReadSectionValues('wsl2', Memo1.Lines);

  Memo1.Lines.Add('--- All [test] values ----');
  Wslconfig.ReadSectionValues('test', Memo1.Lines);

  Memo1.Lines.Add('--- Delete section "test" ----');
  Wslconfig.EraseSection('test');

  Memo1.Lines.Add('--- All [test] values ----');
  Wslconfig.ReadSectionValues('test', Memo1.Lines);

  Memo1.Lines.Add('--- All section ----');
  Wslconfig.ReadSections(Memo1.Lines);

  Memo1.Lines.Add('--- All [test] values ----');
  Wslconfig.WriteString('test', 'truc', '<restored>');
  Wslconfig.ReadSectionValues('test', Memo1.Lines);
  Memo1.Lines.Add(Wslconfig.ReadString('test', 'cool', '<deleted>'));

  // Delete key
  Wslconfig.DeleteKey('wsl2', 'processors');

  // Erase section
  Wslconfig.EraseSection('test');

  // Update key
  Wslconfig.WriteString('wsl2', 'processors', '568');

  // Create new section and new key
  Wslconfig.WriteString('wsl2', 'test_quote', 'hello every body!');

  Wslconfig.UpdateFile;

  Wslconfig.Free;
end;

end.

