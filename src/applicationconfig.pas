unit ApplicationConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const DEFAULT_TIMER_VALUE: integer = 300;
const TIMER_VALUE_KEY: string = 'RefreshTimerInterval';
const WSL_START_FOLDER_KEY: string = 'WslStartFolder';
const DEFAULT_WSL_START_FOLDER_VALUE = '';
const DEFAULT_REMEMBER_EXPORT_PATH_VALUE: boolean = true;
const REMEMBER_EXPORT_PATH_KEY = 'RememberExportPath';
const DEFAULT_REMEMBER_IMPORT_PATH_VALUE: boolean = true;
const REMEMBER_IMPORT_PATH_KEY = 'RememberImportPath';
const EXPORT_PATH_KEY = 'ExportPath';
const DEFAULT_EXPORT_PATH: string = '';
const IMPORT_PATH_KEY = 'ImportPath';
const DEFAULT_IMPORT_PATH: string = '';
const IMPORT_INSTALL_LOCATION_KEY = 'ImportInstallLocation';

implementation

end.

