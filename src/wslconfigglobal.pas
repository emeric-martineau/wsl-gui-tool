unit WslConfigGlobal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WslconfigParameterCtrl;

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

function GenerateWslConfigForUser(): TWslconfigEntryParameterList;

implementation

function GenerateWslConfigForUser(): TWslconfigEntryParameterList;
begin
  Result := TWslconfigEntryParameterList.Create();

  Result.Add(TWslconfigEntryParameter.Create(
    'Linux kernel path:',
    WSL2_KERNEL_PROPERTY,
    WSL2_SECTION,
    EntryString,
    'An absolute Windows path to a custom Linux kernel.',
    'The Microsoft built kernel provided inbox',
    10, 0));
  Result.Add(TWslconfigEntryParameter.Create(
    'Maximum memory size:',
    WSL2_MEMORY_PROPERTY,
    WSL2_SECTION,
    EntrySize,
    'How much memory to assign to the WSL 2 VM.',
    '50% of total memory on Windows or 8GB, whichever is less; on builds before 20175: 80% of your total memory on Windows',
    10, 0));
  Result.Add(TWslconfigEntryParameter.Create(
    'Number of processor:',
    WSL2_PROCESSORS_PROPERTY,
    WSL2_SECTION,
    EntryNumber,
    'How many processors to assign to the WSL 2 VM.',
    'The same number of processors on Windows',
    10, 0));
  Result.Add(TWslconfigEntryParameter.Create(
    'Port bound wildcard:',
    WSL2_LOCALHOST_FORWARDING_PROPERTY,
    WSL2_SECTION,
    EntryBoolean,
    'Boolean specifying if ports bound to wildcard or localhost in the WSL 2 VM should be connectable from the host via localhost:port.',
    'true',
    10, 0));
  Result.Add(TWslconfigEntryParameter.Create(
    'Kernel commande line:',
    WSL2_KERNEL_COMMANDLINE_PROPERTY,
    WSL2_SECTION,
    EntryString,
    'Additional kernel command line arguments.',
    '',
    10, 0));
  Result.Add(TWslconfigEntryParameter.Create(
    'Swap size:',
    WSL2_SWAP_PROPERTY,
    WSL2_SECTION,
    EntryPath,
    'How much swap space to add to the WSL 2 VM, 0 for no swap file.',
    '25% of memory size on Windows rounded up to the nearest GB',
    10, 0));
  Result.Add(TWslconfigEntryParameter.Create(
    'Swap filename:',
    WSL2_SWAP_FILE_PROPERTY,
    WSL2_SECTION,
    EntryString,
    'An absolute Windows path to the swap virtual hard disk.',
    '%USERPROFILE%\AppData\Local\Temp\swap.vhdx',
    10, 0));
  Result.Add(TWslconfigEntryParameter.Create(
    'Reclaim unused memory:',
    WSL2_PAGE_REPORTING_PROPERTY,
    WSL2_SECTION,
    EntryBoolean,
    'Default true setting enables Windows to reclaim unused memory allocated to WSL 2 virtual machine.',
    'true',
    10, 0));
  Result.Add(TWslconfigEntryParameter.Create(
    'GUI support:',
    WSL2_GUI_APPLICATIONS_PROPERTY,
    WSL2_SECTION,
    EntryBoolean,
    'Boolean to turn on or off support for GUI applications (WSLg) in WSL.',
    'true',
    11, 0));
  Result.Add(TWslconfigEntryParameter.Create(
    'Enable debug console:',
    WSL2_DEBUG_CONSOLE_PROPERTY,
    WSL2_SECTION,
    EntryBoolean,
    'Boolean to turn on an output console Window that shows the contents of dmesg upon start of a WSL 2 distro instance.',
    'false',
    11, 0));
  Result.Add(TWslconfigEntryParameter.Create(
    'Need virtualization:',
    WSL2_NESTED_VIRTUALIZATION_PROPERTY,
    WSL2_SECTION,
    EntryBoolean,
    'Boolean to turn on or off nested virtualization for WSL2.',
    'true',
    11, 0));
  Result.Add(TWslconfigEntryParameter.Create(
    'Timeout before shutdown:',
    WSL2_VM_IDLE_TIMEOUT_PROPERTY,
    WSL2_SECTION,
    EntryNumber,
    'The number of milliseconds that a VM is idle, before it is shut down.',
    '60000',
    11, 0));
end;

end.

