unit wslregistry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, Regexpr, ComObj, Registry, WslApi;

Type
  // A WSL distribution
  TWslRegistryDistribution = class(TWslApiDistribution)
      Id: string;
      BasePath: string;
      KernelCommandLine: string;
      PackageFamilyName: string;
      IsDefault: boolean;
  end;

  TWslRegistryDistributionList = specialize TFPGObjectList<TWslRegistryDistribution>;

// Return list of distribution for registry
function LoadWslFromRegistry(): TWslRegistryDistributionList;
// Return a distribution for registry by this id
function LoadWslOneDistributionFromRegistryById(DistributionKey: string): TWslRegistryDistribution;
// Find a distribution in list return by LoadWslFromRegistry()
function FindDistribution(Distributions: TWslRegistryDistributionList; Name: string): TWslRegistryDistribution;
// Return a distribution by name
function LoadWslOneDistributionFromRegistryByName(DistributionName: string): TWslRegistryDistribution;
// Save a distribution in registry
procedure SaveDistributionToRegistry(Distribution: TWslRegistryDistribution);
// Set distribution as default distribution
procedure SetDistributionAsDefault(DistributionName: string);

const
  LXSS_REG_KEY = '\SOFTWARE\Microsoft\Windows\CurrentVersion\Lxss';

implementation

function LoadWslOneDistributionFromRegistryById(DistributionKey: string): TWslRegistryDistribution;
var
  Registry : TRegistry;
begin
  Result := TWslRegistryDistribution.Create();

  Registry := TRegistry.Create;
  Registry.RootKey := HKEY_CURRENT_USER;

  Registry.OpenKey(LXSS_REG_KEY + '\\' + DistributionKey, False);

  Result.Id:= DistributionKey;
  Result.Version := Registry.ReadInteger('Version');
  Result.Name := Registry.Readstring('DistributionName');
  Result.BasePath := Registry.Readstring('BasePath');
  Result.Flags := Registry.ReadInteger('Flags');
  Result.DefaultUID := Longword(Registry.ReadInteger('DefaultUid'));

  Result.Env := TStringList.Create();

  Registry.ReadStringList('DefaultEnvironment', Result.Env);

  Result.KernelCommandLine := Registry.Readstring('KernelCommandLine');
  Result.PackageFamilyName := Registry.Readstring('PackageFamilyName');

  Registry.CloseKey;

  Registry.Free;
end;

function LoadWslFromRegistry(): TWslRegistryDistributionList;
var
  Registry : TRegistry;
  DefaultDistribution: string;
  DistributionsIdList : Tstrings;
  Distribution: TWslRegistryDistribution;
  i: integer;
begin
  Result := TWslRegistryDistributionList.Create();

  Registry := TRegistry.Create;
  Registry.RootKey := HKEY_CURRENT_USER;

  if Registry.KeyExists(LXSS_REG_KEY)
  then begin
    Registry.OpenKey(LXSS_REG_KEY, False);

    DefaultDistribution := Registry.Readstring('DefaultDistribution');

    DistributionsIdList := TstringList.Create();

    Registry.GetKeyNames(DistributionsIdList);

    for i := 0 to DistributionsIdList.Count - 1 do
    begin
      Distribution := LoadWslOneDistributionFromRegistryById(DistributionsIdList[i]);

      if Distribution.Id = DefaultDistribution
      then begin
        Distribution.IsDefault := true;
      end;

      Result.Add(Distribution);
    end;

    DistributionsIdList.Free;

    Registry.CloseKey;
  end;

  Registry.Free;
end;

function FindDistribution(Distributions: TWslRegistryDistributionList; Name: string): TWslRegistryDistribution;
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

function CheckDistributionName(DistributionKey: string; DistributionName: string): boolean;
var
  Registry : TRegistry;
begin
  Registry := TRegistry.Create;
  Registry.RootKey := HKEY_CURRENT_USER;

  Registry.OpenKey(LXSS_REG_KEY + '\\' + DistributionKey, False);

  Result := Registry.Readstring('DistributionName') = DistributionName;

  Registry.CloseKey;

  Registry.Free;
end;

function LoadWslOneDistributionFromRegistryByName(DistributionName: string): TWslRegistryDistribution;
var
  Registry : TRegistry;
  DistributionsIdList : Tstrings;
  i: integer;
begin
  Registry := TRegistry.Create;
  Registry.RootKey := HKEY_CURRENT_USER;
  Result := nil;

  if Registry.KeyExists(LXSS_REG_KEY)
  then begin
    Registry.OpenKey(LXSS_REG_KEY, False);

    DistributionsIdList := TstringList.Create();

    Registry.GetKeyNames(DistributionsIdList);

    for i := 0 to DistributionsIdList.Count - 1 do
    begin
      if CheckDistributionName(DistributionsIdList[i], DistributionName)
      then begin
        Result := LoadWslOneDistributionFromRegistryById(DistributionsIdList[i]);
        break;
      end;

    end;

    DistributionsIdList.Free;

    Registry.CloseKey;
  end;

  Registry.Free;
end;

procedure SaveDistributionToRegistry(Distribution: TWslRegistryDistribution);
var
  Registry : TRegistry;
begin
  Registry := TRegistry.Create;
  Registry.RootKey := HKEY_CURRENT_USER;

  Registry.OpenKey(LXSS_REG_KEY + '\\' + Distribution.Id, False);

  Registry.WriteString('DistributionName', Distribution.Name);
  Registry.WriteInteger('Flags', Distribution.Flags);
  Registry.WriteInteger('DefaultUid', Distribution.DefaultUID);
  Registry.WriteStringList('DefaultEnvironment', Distribution.Env);
// TODO
//  Result.Version := Registry.ReadInteger('Version');
//  Result.BasePath := Registry.Readstring('BasePath');
//  Result.KernelCommandLine := Registry.Readstring('KernelCommandLine');
//  Result.PackageFamilyName := Registry.Readstring('PackageFamilyName');


  Registry.CloseKey;

  Registry.Free;
end;

procedure SetDistributionAsDefault(DistributionName: string);
var
  Distribution: TWslRegistryDistribution;
  Registry : TRegistry;
begin
  Distribution := LoadWslOneDistributionFromRegistryByName(DistributionName);

  if Distribution <> nil
  then begin
    Registry := TRegistry.Create;
    Registry.RootKey := HKEY_CURRENT_USER;

    if Registry.KeyExists(LXSS_REG_KEY)
    then begin
      Registry.OpenKey(LXSS_REG_KEY, False);

      Registry.Writestring('DefaultDistribution', Distribution.Id);

      Registry.CloseKey;
    end;

    Registry.Free;

    Distribution.Free;
  end;
end;

end.

