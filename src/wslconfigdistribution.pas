unit WslConfigDitribution;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WslconfigParameterCtrl;

const
  WSL2_AUTOMOUNT_SECTION = 'automount';
  WSL2_NETWORK_SECTION = 'network';
  WSL2_INTEROP_SECTION = 'interop';
  WSL2_USER_SECTION = 'user';
  WSL2_BOOT_SECTION = 'boot';

  WSL2_AUTOMOUNT_ENABLE_PROPERTY = 'enabled';
  WSL2_AUTOMOUNT_MOUNTFSTAB_PROPERTY = 'mountFsTab';
  WSL2_AUTOMOUNT_ROOT_PROPERTY = 'root';
  WSL2_AUTOMOUNT_OPTIONS_PROPERTY = 'options';

  WSL2_NETWORK_GENERATE_HOSTS_PROPERTY = 'generateHosts';
  WSL2_NETWORK_GENERATE_RESOLV_HOSTS_PROPERTY = 'generateResolvConf';
  WSL2_NETWORK_GENERATE_HOSTNAME_PROPERTY = 'hostname';

  WSL2_INTEROP_ENABLED = 'enabled';
  WSL2_INTEROP_APPEND_WINDOWS_PATH = 'appendWindowsPath';

  WSL2_USER_DEFAULT = 'default';

  WSL2_BOOT_COMMAND = 'command';

function GenerateWslConfigForDistribution(): TWslconfigEntryParameterList;

implementation

function GenerateWslConfigForDistribution(): TWslconfigEntryParameterList;
begin
  Result := TWslconfigEntryParameterList.Create();

  Result.Add(TWslconfigEntryParameter.Create(
    'Automount',
    '',
    '',
    EntryHeader,
    '',
    '',
    10, 0));

  Result.Add(TWslconfigEntryParameter.Create(
    'Enable:',
    WSL2_AUTOMOUNT_ENABLE_PROPERTY,
    WSL2_AUTOMOUNT_SECTION,
    EntryBoolean,
    'true causes fixed drives (i.e C:/ or D:/) to be automatically mounted with DrvFs under /mnt. false means drives won''t be mounted automatically, but you could still mount them manually or via fstab.',
    'true',
    10, 0));
  Result.Add(TWslconfigEntryParameter.Create(
    'Fs tab:',
    WSL2_AUTOMOUNT_MOUNTFSTAB_PROPERTY,
    WSL2_AUTOMOUNT_SECTION,
    EntryBoolean,
    'true sets /etc/fstab to be processed on WSL start. /etc/fstab is a file where you can declare other filesystems, like an SMB share. Thus, you can mount these filesystems automatically in WSL on start up.',
    'true',
    10, 0));
  Result.Add(TWslconfigEntryParameter.Create(
    'Root folder:',
    WSL2_AUTOMOUNT_ROOT_PROPERTY,
    WSL2_AUTOMOUNT_SECTION,
    EntryString,
    'Sets the directory where fixed drives will be automatically mounted. By default this is set to /mnt/, so your Windows file system C-drive is mounted to /mnt/c/. If you change /mnt/ to /windir/, you should expect to see your fixed C-drive mounted to /windir/c.',
    '/mnt/',
    10, 0));
  Result.Add(TWslconfigEntryParameter.Create(
    'Options:',
    WSL2_AUTOMOUNT_OPTIONS_PROPERTY,
    WSL2_AUTOMOUNT_SECTION,
    EntryString,
    'The automount option values are listed below and are appended to the default DrvFs mount options string. Only DrvFs-specific options can be specified.',
    '',
    10, 0));

  Result.Add(TWslconfigEntryParameter.Create(
    'Network',
    '',
    '',
    EntryHeader,
    '',
    '',
    10, 0));

  Result.Add(TWslconfigEntryParameter.Create(
    'Generates "/etc/hosts" file:',
    WSL2_NETWORK_GENERATE_HOSTS_PROPERTY,
    WSL2_NETWORK_SECTION,
    EntryBoolean,
    'The hosts file contains a static map of hostnames corresponding IP address.',
    'true',
    10, 0));
  Result.Add(TWslconfigEntryParameter.Create(
    'Generates "/etc/resolv.conf" file:',
    WSL2_NETWORK_GENERATE_RESOLV_HOSTS_PROPERTY,
    WSL2_NETWORK_SECTION,
    EntryBoolean,
    'The resolv.conf contains a DNS list that are capable of resolving a given hostname to its IP address.',
    'true',
    10, 0));
  Result.Add(TWslconfigEntryParameter.Create(
    'Hostname:',
    WSL2_NETWORK_GENERATE_HOSTNAME_PROPERTY,
    WSL2_NETWORK_SECTION,
    EntryString,
    'Hostname of WSL distribution.',
    'true',
    10, 0));

  Result.Add(TWslconfigEntryParameter.Create(
    'Interop',
    '',
    '',
    EntryHeader,
    '',
    '',
    10, 0));
  Result.Add(TWslconfigEntryParameter.Create(
    'Run Windows and Linux tools and commands interchangeably:',
    WSL2_INTEROP_ENABLED,
    WSL2_INTEROP_SECTION,
    EntryBoolean,
    'Setting this key will determine whether WSL will support launching Windows processes.',
    'true',
    10, 0));
  Result.Add(TWslconfigEntryParameter.Create(
    'RShare environment variables between Linux and Windows:',
    WSL2_INTEROP_APPEND_WINDOWS_PATH,
    WSL2_INTEROP_SECTION,
    EntryBoolean,
    'Setting this key will determine whether WSL will add Windows path elements to the $PATH environment variable.',
    'true',
    10, 0));

  Result.Add(TWslconfigEntryParameter.Create(
    'User',
    '',
    '',
    EntryHeader,
    '',
    '',
    10, 0));
  Result.Add(TWslconfigEntryParameter.Create(
    'Usename:',
    WSL2_USER_DEFAULT,
    WSL2_USER_SECTION,
    EntryString,
    'Default user to run WSL distribution. By default, the initial username created on first run.',
    '',
    10, 0));

  Result.Add(TWslconfigEntryParameter.Create(
    'Boot',
    '',
    '',
    EntryHeader,
    '',
    '',
    11, 0));
  Result.Add(TWslconfigEntryParameter.Create(
    'Usename:',
    WSL2_BOOT_COMMAND,
    WSL2_BOOT_SECTION,
    EntryString,
    'A string of the command that you would like to run when the WSL instance starts. This command is run as the root user. e.g: "service docker start"',
    '',
    11, 0));
end;

end.

