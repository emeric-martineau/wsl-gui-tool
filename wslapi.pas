unit wslapi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Regexpr, ComObj, Windows, ActiveX;

Type
  // A WSL distribution
  TWslApiDistribution = class(TObject)
      Name: String;
      Version: Integer;
      DefaultUID: Longword;
      Flags: Longword;
      Env: TStrings;
      destructor destroy; override;
  end;

// Return configuration of distribution of nil if not found
function WslGetConfigurationOfDistribution(DistributionName: string): TWslApiDistribution;
// Set a new configuration
function WslSetConfigurationOfDistribution(DistributionName: string; DefaultUID: Longword; Flags: Longword): boolean;

const
  WSL_DISTRIBUTION_FLAGS_NONE = 0;
  WSL_DISTRIBUTION_FLAGS_ENABLE_INTEROP = 1;
  WSL_DISTRIBUTION_FLAGS_APPEND_NT_PATH = 2;
  WSL_DISTRIBUTION_FLAGS_ENABLE_DRIVE_MOUNTING = 4;

implementation

Type
  TFunctionWslGetDistributionConfiguration = function(
    DistributionName: PWideChar;
    DistributionVersion: PLongWord;
    DefaultUID: PLongWord;
    WslDistributionFlags: PLongWord;
    DefaultEnvironmentVariables: PPChar;
    DefaultEnvironmentVariableCount: PLongWord
    ): HRESULT;  stdcall;

  WSL_DISTRIBUTION_FLAGS = LongWord;

  TFunctionWslConfigureDistribution = function(
    DistributionName: PWideChar;
    DefaultUID: LongWord;
    WslDistributionFlags: WSL_DISTRIBUTION_FLAGS
  ): HRESULT;  stdcall;

const
  RPC_C_AUTHN_LEVEL_DEFAULT = 0;
  RPC_C_IMP_LEVEL_IMPERSONATE = 3;

destructor TWslApiDistribution.Destroy;
begin
   Self.Env.Free;
   inherited;
end;

var
  // Creates a suitable variable (data field) for the DLL subroutine
  WslGetDistributionConfiguration : TFunctionWslGetDistributionConfiguration;
  WslDistributionConfiguration: TFunctionWslConfigureDistribution;
  // Creates a handle for the DLL
  LibHandle : THandle;

function CopyString(pstr: PChar): string;
var
  IndexOfChar: Integer;
begin
  IndexOfChar := 0;
  Result := '';

  while pstr[IndexOfChar] <> #0 do
  begin
    Result := Result + pstr[IndexOfChar];
    IndexOfChar := IndexOfChar + 1;
  end;
end;

function ConvertWslApiEnvVarToStringsList(
  DefaultEnvironmentVariables: PPChar;
  DefaultEnvironmentVariableCount: LongWord): TStrings;
var
  StringIndex: Integer;
  CurrentStringPointer: PChar;
begin
  Result := TStringList.Create();

  for StringIndex := 0 to DefaultEnvironmentVariableCount - 1 do
  begin
    CurrentStringPointer := DefaultEnvironmentVariables[StringIndex];

    Result.Add(CopyString(CurrentStringPointer));
  end;
end;

procedure ClearWslApiEnvVar(
  DefaultEnvironmentVariables: PPChar;
  DefaultEnvironmentVariableCount: LongWord);
var
  StringIndex: Integer;
  CurrentStringPointer: PChar;
begin
  for StringIndex := 0 to DefaultEnvironmentVariableCount - 1 do
  begin
    CurrentStringPointer := DefaultEnvironmentVariables[StringIndex];

    CoTaskMemFree(CurrentStringPointer)
  end;

  CoTaskMemFree(DefaultEnvironmentVariables);
end;

function WslGetConfigurationOfDistribution(DistributionName: string): TWslApiDistribution;
var
    DistributionVersion: LongWord;
    DefaultUID: LongWord;
    WslDistributionFlags: LongWord;
    DefaultEnvironmentVariables: PPChar;
    DefaultEnvironmentVariableCount: LongWord;
    ret: HRESULT;
    DistribNameLen: integer;
    DistribName: PWideChar;
    EnvVars: TStrings;
begin
  DistribNameLen := (Length(DistributionName)+1) * SizeOf(WideChar);

  GetMem(DistribName, DistribNameLen);

  StringToWideChar(DistributionName, PWideChar(DistribName), DistribNameLen);

  ret := WslGetDistributionConfiguration(
      DistribName,
      @DistributionVersion,
      @DefaultUID,
      @WslDistributionFlags,
      @DefaultEnvironmentVariables,
      @DefaultEnvironmentVariableCount);

  FreeMem(DistribName);

  if Succeeded(ret)
  then begin
    EnvVars := ConvertWslApiEnvVarToStringsList(DefaultEnvironmentVariables, DefaultEnvironmentVariableCount);

    ClearWslApiEnvVar(DefaultEnvironmentVariables, DefaultEnvironmentVariableCount);

    Result := TWslApiDistribution.Create();

    Result.Version := DistributionVersion;
    Result.Name := DistributionName;
    Result.Flags := WslDistributionFlags;
    Result.DefaultUID := DefaultUID;
    Result.Env := EnvVars;
  end else begin
    Result := nil;
  end;
end;

function WslSetConfigurationOfDistribution(DistributionName: string; DefaultUID: LongWord; Flags: LongWord): boolean;
var
    ret: HRESULT;
    DistribNameLen: integer;
    DistribName: PWideChar;
begin
  DistribNameLen := (Length(DistributionName)+1) * SizeOf(WideChar);

  GetMem(DistribName, DistribNameLen);

  StringToWideChar(DistributionName, PWideChar(DistribName), DistribNameLen);

  ret := WslDistributionConfiguration(
      DistribName,
      DefaultUID,
      Flags);

  FreeMem(DistribName);

  Result := Succeeded(ret)
end;

initialization
  CoInitialize(nil);

  // Get the handle of the library to be used
  LibHandle := LoadLibrary(PChar('Api-ms-win-wsl-api-l1-1-0.dll'));

  // Checks whether loading the DLL was successful
  if LibHandle <> 0
  then begin
    Pointer(WslGetDistributionConfiguration) := GetProcAddress(LibHandle, 'WslGetDistributionConfiguration');
    Pointer(WslDistributionConfiguration) := GetProcAddress(LibHandle, 'WslConfigureDistribution');

    // Checks whether a valid address has been returned
    if (@WslGetDistributionConfiguration <> nil) and (@WslDistributionConfiguration <> nil)
    then begin
       if Failed(CoInitializeSecurity(
         nil,
         -1,
         nil,
         nil,
         RPC_C_AUTHN_LEVEL_DEFAULT,
         RPC_C_IMP_LEVEL_IMPERSONATE,
         nil,
         EOAC_STATIC_CLOAKING,
         nil))
       then begin
         // release memory and use registry only
         WslGetDistributionConfiguration := nil;
         WslDistributionConfiguration := nil;
       end;
    end;
  end;

finalization
   // release memory
   WslGetDistributionConfiguration := nil;
   WslDistributionConfiguration := nil;

   FreeLibrary(LibHandle);

   CoUninitialize();
end.

