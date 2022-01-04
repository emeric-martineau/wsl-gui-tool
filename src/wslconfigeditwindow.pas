unit WslConfigEditWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  MaskEdit, EditBtn, SpinEx, WslConfig, WslconfigParameterCtrl;

type
  { TFormWslconfigEdit }

  TFormWslconfigEdit = class(TForm)
    ButtonCancel: TButton;
    ButtonReset: TButton;
    ButtonSave: TButton;
    ImageListWslconfig: TImageList;
    PanelButtonCancel: TPanel;
    PanelButtonOk: TPanel;
    PanelButtonReset: TPanel;
    PanelButtons: TPanel;
    procedure ButtonResetClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    WslconfigPropertiesPanel: TWslConfigPanel;
    WslList: TWslconfigEntryList;
    Wslconfig: TWslconfigFile;
    procedure OnValueChange(Sender: TObject);
    procedure OnValueReset(Sender: TObject);
  public

  end;

var
  FormWslconfigEdit: TFormWslconfigEdit;

const
  WSL2_SECTION = 'wsl2';
  WSL2_KERNEL_PROPERTY = 'kernel';
  WSL2_MEMORY_PROPERTY = 'memory';
  WSL2_PROCESSORS_PROPERTY = 'processors';
  WSL2_LOCALHOST_FORWARDING_PROPERTY = 'localhostForwarding';
  WSL2_KERNEL_COMMANDLINE_PROPERTY = 'kernelCommandLine';
  WSL2_SWAP_PROPERTY = 'swap';
  WSL2_SWAP_FILE_PROPERTY = 'swapFile';
  WSL2_PAGE_REPORTING_PROPERTY = 'pageReporting';
  WSL2_GUI_APPLICATIONS_PROPERTY = 'guiApplications';
  WSL2_DEBUG_CONSOLE_PROPERTY = 'debugConsole';
  WSL2_NESTED_VIRTUALIZATION_PROPERTY = 'nestedVirtualization';
  WSL2_VM_IDLE_TIMEOUT_PROPERTY = 'vmIdleTimeout';

implementation

{$R *.lfm}


{ TFormWslconfigEdit }

procedure TFormWslconfigEdit.FormCreate(Sender: TObject);
begin
  Wslconfig := TWslconfigFile.Create(GetUserDir + '.wslconfig');

  WslconfigPropertiesPanel := TWslConfigPanel.Create(Self);
  WslconfigPropertiesPanel.Parent := Self;
  WslconfigPropertiesPanel.Align := alClient;
  WslconfigPropertiesPanel.Images := ImageListWslconfig;
  WslconfigPropertiesPanel.HelpImageIndex := 0;
  WslconfigPropertiesPanel.OnChange := @OnValueChange;
  WslconfigPropertiesPanel.OnReset := @OnValueReset;

  WslList := TWslconfigEntryList.Create();

  WslList.Add(TWslconfigEntry.Create(
    'Linux kernel path:',
    WSL2_KERNEL_PROPERTY,
    Wslconfig.ReadString(WSL2_SECTION, WSL2_KERNEL_PROPERTY, ''),
    EntryString,
    'An absolute Windows path to a custom Linux kernel.',
    'The Microsoft built kernel provided inbox',
    10, 0));
  WslList.Add(TWslconfigEntry.Create(
    'Maximum memory size:',
    WSL2_MEMORY_PROPERTY,
    Wslconfig.ReadString(WSL2_SECTION, WSL2_MEMORY_PROPERTY, ''),
    EntrySize,
    'How much memory to assign to the WSL 2 VM.',
    '50% of total memory on Windows or 8GB, whichever is less; on builds before 20175: 80% of your total memory on Windows',
    10, 0));
  WslList.Add(TWslconfigEntry.Create(
    'Number of processor:',
    WSL2_PROCESSORS_PROPERTY,
    Wslconfig.ReadString(WSL2_SECTION, WSL2_PROCESSORS_PROPERTY, ''),
    EntryNumber,
    'How many processors to assign to the WSL 2 VM.',
    'The same number of processors on Windows',
    10, 0));
  WslList.Add(TWslconfigEntry.Create(
    'Port bound wildcard:',
    WSL2_LOCALHOST_FORWARDING_PROPERTY,
    Wslconfig.ReadString(WSL2_SECTION, WSL2_LOCALHOST_FORWARDING_PROPERTY, ''),
    EntryBoolean,
    'Boolean specifying if ports bound to wildcard or localhost in the WSL 2 VM should be connectable from the host via localhost:port.',
    'true',
    10, 0));
  WslList.Add(TWslconfigEntry.Create(
    'Kernel commande line:',
    WSL2_KERNEL_COMMANDLINE_PROPERTY,
    Wslconfig.ReadString(WSL2_SECTION, WSL2_KERNEL_COMMANDLINE_PROPERTY, ''),
    EntryString,
    'Additional kernel command line arguments.',
    '',
    10, 0));
  WslList.Add(TWslconfigEntry.Create(
    'Swap size:',
    WSL2_SWAP_PROPERTY,
    Wslconfig.ReadString(WSL2_SECTION, WSL2_SWAP_PROPERTY, ''),
    EntryPath,
    'How much swap space to add to the WSL 2 VM, 0 for no swap file.',
    '25% of memory size on Windows rounded up to the nearest GB',
    10, 0));
  WslList.Add(TWslconfigEntry.Create(
    'Swap filename:',
    WSL2_SWAP_FILE_PROPERTY,
    Wslconfig.ReadString(WSL2_SECTION, WSL2_SWAP_FILE_PROPERTY, ''),
    EntryString,
    'An absolute Windows path to the swap virtual hard disk.',
    '%USERPROFILE%\AppData\Local\Temp\swap.vhdx',
    10, 0));
  WslList.Add(TWslconfigEntry.Create(
    'Reclaim unused memory:',
    WSL2_PAGE_REPORTING_PROPERTY,
    Wslconfig.ReadString(WSL2_SECTION, WSL2_PAGE_REPORTING_PROPERTY, ''),
    EntryBoolean,
    'Default true setting enables Windows to reclaim unused memory allocated to WSL 2 virtual machine.',
    'true',
    10, 0));
  WslList.Add(TWslconfigEntry.Create(
    'GUI support:',
    WSL2_GUI_APPLICATIONS_PROPERTY,
    Wslconfig.ReadString(WSL2_SECTION, WSL2_GUI_APPLICATIONS_PROPERTY, ''),
    EntryBoolean,
    'Boolean to turn on or off support for GUI applications (WSLg) in WSL.',
    'true',
    11, 0));
  WslList.Add(TWslconfigEntry.Create(
    'Enable debug console:',
    WSL2_DEBUG_CONSOLE_PROPERTY,
    Wslconfig.ReadString(WSL2_SECTION, WSL2_DEBUG_CONSOLE_PROPERTY, ''),
    EntryBoolean,
    'Boolean to turn on an output console Window that shows the contents of dmesg upon start of a WSL 2 distro instance.',
    'false',
    11, 0));
  WslList.Add(TWslconfigEntry.Create(
    'Need virtualization:',
    WSL2_NESTED_VIRTUALIZATION_PROPERTY,
    Wslconfig.ReadString(WSL2_SECTION, WSL2_NESTED_VIRTUALIZATION_PROPERTY, ''),
    EntryBoolean,
    'Boolean to turn on or off nested virtualization for WSL2.',
    'true',
    11, 0));
  WslList.Add(TWslconfigEntry.Create(
    'Timeout before shutdown:',
    WSL2_VM_IDLE_TIMEOUT_PROPERTY,
    Wslconfig.ReadString(WSL2_SECTION, WSL2_VM_IDLE_TIMEOUT_PROPERTY, ''),
    EntryNumber,
    'The number of milliseconds that a VM is idle, before it is shut down.',
    '60000',
    11, 0));

   WslconfigPropertiesPanel.AddItems(WslList);
end;

procedure TFormWslconfigEdit.ButtonResetClick(Sender: TObject);
begin
  WslconfigPropertiesPanel.Reset;
end;

procedure TFormWslconfigEdit.ButtonSaveClick(Sender: TObject);
var
  Index: integer;
  Key: string;
  Value: TWslValue;
begin
  for Index := 0 to WslList.Count - 1 do
  begin
    Key := WslList[Index].Key;
    Value := WslconfigPropertiesPanel.GetValue(Key);

    if Value.Found and Value.Changed
    then begin
      if Length(Value.Value) > 0
      then begin
        Wslconfig.WriteString(WSL2_SECTION, Key, Value.Value);
      end else begin
        Wslconfig.DeleteKey(WSL2_SECTION, Key);
      end;
    end;
  end;

  Wslconfig.UpdateFile;
end;

procedure TFormWslconfigEdit.FormDestroy(Sender: TObject);
begin
  WslconfigPropertiesPanel.Free;
  WslList.Free;
  Wslconfig.Free;
end;

procedure TFormWslconfigEdit.OnValueChange(Sender: TObject);
begin
  ButtonSave.Enabled := true;
end;

procedure TFormWslconfigEdit.OnValueReset(Sender: TObject);
begin
  ButtonSave.Enabled := false;
end;

end.
