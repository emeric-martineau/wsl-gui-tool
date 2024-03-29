unit wslcommandLine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, fgl, Regexpr, ComObj, ShellApi, StrUtils;

type
  // A WSL distribution
  TWslCommandLineDistribution = class(TObject)
      Name: string;
      Version: Integer;
      IsRunning: boolean;
      IsDefault: boolean;
  end;

  TWslCommandLineDistributionList = specialize TFPGObjectList<TWslCommandLineDistribution>;

// Retrun is WSL seems to be installed
function IsWslInstalled(): boolean;
// Return list of current WSL distribution
function ListDistribution: TWslCommandLineDistributionList;
// Find a distribution in list
function FindDistribution(Distributions: TWslCommandLineDistributionList; Name: string): TWslCommandLineDistribution;
// Start WSL distribution
function StartDistribution(Name: string; User: string = ''; Command: string = ''; Directory: string = ''; UseShell: boolean = true): boolean;
function StartDistributionFromFolder(Name: string; Directory: string = ''): boolean;
// Stop WSL distribution
function StopDistribution(Name: string): boolean;
// Set WSL1 or WSL2 for one distribution
function SetDistributionVersion(Name: string; Version: integer): boolean;
// Export a distribution
function ExportDistribution(Name: string; ExportFileName: string): TProcess;
// Import a distribution
function ImportDistribution(Name: string; InstallLocationPath: string; Version: Integer; FileName: string): TProcess;
// Unregister a distribution
function UnregisterDistribution(Name: string): TProcess;
// Update wsl.conf file
function UpdateEtcWslConf(DistributionName: string; Data: string): boolean;

implementation

function IsWslInstalled(): boolean;
var
  wslOutput : string;
begin
  Result := RunCommand('wsl', ['--list'], wslOutput);
end;

// Run WSL and return raw output
function ListDistributionRunWslList: TStringList;
var
  // Use to run WSL binary
  WslProcess: TProcess;
begin
  WslProcess := TProcess.Create(nil);

  WslProcess.Executable := 'wsl';
  WslProcess.Parameters.Add('--list');
  WslProcess.Parameters.Add('--verbose');

  WslProcess.Options := WslProcess.Options + [poWaitOnExit, poUsePipes, poStderrToOutPut];
  WslProcess.ShowWindow := swoHIDE;

  WslProcess.Execute;

  Result := TStringList.Create;
  Result.LoadFromStream(WslProcess.Output, TEncoding.Unicode);

  // Now that the output from the process is processed, it can be freed.
  WslProcess.Free;
end;

procedure ListDistributionClearWslOutput(WslOutput: TStringList);
var
  i: Integer;
  NewOutputLine: string;
begin
  // Remove first line with title
  WslOutput.Delete(0);

  i := 0;

  while i < WslOutput.Count do
  begin
    NewOutputLine := WslOutput.Strings[i];

    if NewOutputLine = ''
    then begin
      WslOutput.Delete(i);
    end else begin
      WslOutput.Strings[i] := ReplaceRegExpr('\s+', NewOutputLine, ' ', True);
      i := i + 1;
    end;
  end;
end;

function FindDistribution(Distributions: TWslCommandLineDistributionList; Name: string): TWslCommandLineDistribution;
var
  i: Integer;
begin
  for i := 0 to Distributions.Count - 1 do
  begin
    if Distributions[i].Name = Name
    then begin
      Exit(Distributions[i]);
    end;
  end;
end;

function ListDistribution: TWslCommandLineDistributionList;
var
  WslProcessOutput: TStringList;
  i : Integer;
  CurrentWslDistribution: TWslCommandLineDistribution;
  // Result of split a output line
  CurrentWslDistributionLineSplit: TStringArray;
begin
  Result := TWslCommandLineDistributionList.Create();

  WslProcessOutput := ListDistributionRunWslList;
  ListDistributionClearWslOutput(WslProcessOutput);

  // Now output is like:
  // * Ubuntu-20.04 Running  2
  //  docker-desktop Stopped 2
  //  docker-desktop-data Stopped 2
  for i := 0 to WslProcessOutput.Count -1 do
  begin
    CurrentWslDistributionLineSplit := WslProcessOutput.Strings[i].Split(' ');

    CurrentWslDistribution := TWslCommandLineDistribution.Create();
    CurrentWslDistribution.IsDefault := (CurrentWslDistributionLineSplit[0] = '*');
    CurrentWslDistribution.Name := CurrentWslDistributionLineSplit[1];
    CurrentWslDistribution.Version := StrToInt(CurrentWslDistributionLineSplit[3]);
    CurrentWslDistribution.IsRunning :=
      (LowerCase(CurrentWslDistributionLineSplit[2]) = 'running');

    Result.Add(CurrentWslDistribution);
  end;

  WslProcessOutput.Free;
end;

function StartDistributionFromFolder(Name: string; Directory: string = ''): boolean;
begin
  Result := StartDistribution(Name, '', '', Directory, true)
end;

function StartDistribution(Name: string; User: string = ''; Command: string = ''; Directory: string = ''; UseShell: boolean = true): boolean;
var
  args: string;
  dir: PChar;
begin
  args := '--distribution ' + Name;

  if User <> ''
  then begin
    args := args + ' --user ' + User;
  end;

  if Command <> ''
  then begin
    if UseShell
    then begin
      args := args + ' -- ' + Command;
    end else begin
      args := args + ' --exec ' + Command;
    end;
  end;

  if Directory = ''
  then begin
    dir := nil;
  end else begin
    dir := PChar(Directory);
  end;

  // TODO allow use windows terminal, powershell....
  Result := ShellExecute(0, nil, PChar('wsl'),PChar(args), dir, 1) = 0;
end;

function StopDistribution(Name: string): boolean;
begin
  Result := ShellExecute(0, nil, PChar('wsl'),PChar('--terminate ' + Name), nil, 1) = 0;
end;

function SetDistributionVersion(Name: string; Version: integer): boolean;
var
  // Use to run WSL binary
  WslProcess: TProcess;
begin
  Result := false;

  WslProcess := TProcess.Create(nil);

  WslProcess.Executable := 'wsl';
  WslProcess.Parameters.Add('--set-version');
  WslProcess.Parameters.Add(Name);
  WslProcess.Parameters.Add(IntToStr(Version));

  // TODO return stderr

  WslProcess.Options := WslProcess.Options + [poWaitOnExit];
  WslProcess.ShowWindow := swoHIDE;

  WslProcess.Execute;

  if WslProcess.ExitStatus = 0
  then begin
    Result := true;
  end;

  WslProcess.Free;
end;

function ExportDistribution(Name: string; ExportFileName: string): TProcess;
var
  // Use to run WSL binary
  WslProcess: TProcess;
begin
  WslProcess := TProcess.Create(nil);

  WslProcess.Executable := 'wsl';
  WslProcess.Parameters.Add('--export');
  WslProcess.Parameters.Add(Name);
  WslProcess.Parameters.Add(ExportFileName);

  WslProcess.Options := WslProcess.Options + [poUsePipes] - [poWaitOnExit];
  WslProcess.ShowWindow := swoHIDE;

  Result := WslProcess;
end;

function ImportDistribution(Name: string; InstallLocationPath: string; Version: Integer; FileName: string): TProcess;
var
  // Use to run WSL binary
  WslProcess: TProcess;
begin
  WslProcess := TProcess.Create(nil);

  WslProcess.Executable := 'wsl';
  WslProcess.Parameters.Add('--import');
  WslProcess.Parameters.Add(Name);
  WslProcess.Parameters.Add(InstallLocationPath);
  WslProcess.Parameters.Add(FileName);
  WslProcess.Parameters.Add('--version');
  WslProcess.Parameters.Add(IntToStr(Version));

  WslProcess.Options := WslProcess.Options + [poUsePipes] - [poWaitOnExit];
  WslProcess.ShowWindow := swoHIDE;

  Result := WslProcess;
end;

function UnregisterDistribution(Name: string): TProcess;
var
  // Use to run WSL binary
  WslProcess: TProcess;
begin
  WslProcess := TProcess.Create(nil);

  WslProcess.Executable := 'wsl';
  WslProcess.Parameters.Add('--unregister');
  WslProcess.Parameters.Add(Name);

  WslProcess.Options := WslProcess.Options + [poUsePipes] - [poWaitOnExit];
  WslProcess.ShowWindow := swoHIDE;

  Result := WslProcess;
end;

function UpdateEtcWslConf(DistributionName: string; Data: string): boolean;
var
  // Use to run WSL binary
  WslProcess: TProcess;
begin
  Result := false;

  // Remove CR
  Data := ReplaceStr(Data, #13, '');

  WslProcess := TProcess.Create(nil);

  WslProcess.Executable := 'wsl';
  WslProcess.Parameters.Add('--user');
  WslProcess.Parameters.Add('root');
  WslProcess.Parameters.Add('--distribution');
  WslProcess.Parameters.Add(DistributionName);
  WslProcess.Parameters.Add('eval');
  WslProcess.Parameters.Add('echo '''+ Data + ''' > /etc/wsl.conf');

  // TODO return stderr

  WslProcess.Options := WslProcess.Options + [poWaitOnExit];
  WslProcess.ShowWindow := swoHIDE;

  WslProcess.Execute;

  if WslProcess.ExitStatus = 0
  then begin
    Result := true;
  end;

  WslProcess.Free;

end;

end.

