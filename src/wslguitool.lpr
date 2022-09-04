program wslguitool;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, MainWindow, WslApi, WslCommandLine, WslRegistry,
  ApplicationInfo, AboutWindow, DistributionPropertiesWindow,
  ImportDistributionWindow, RunCommandWithUserWindow, PromptWindow,
  BackgroundProcessProgressBar, ProcessResultDisplay, WslConfigEditWindow,
  Wslconfig, WslconfigParameterCtrl, WslConfigGlobal, WslConfigDistribution,
  LCLType, ConfigWindow;

{$R *.res}

begin
  RequireDerivedFormResource:=True;

  Application.Scaled:=True;
  Application.Initialize;

  if WslCommandLine.IsWslInstalled() then
  begin
  Application.CreateForm(TWslGuiToolMainWindow, WslGuiToolMainWindow);
    Application.Run;
  end
  else begin
    Application.MessageBox(
      'WSL seems to be not installed!',
      'Error',
      MB_OK + MB_ICONERROR
    );
    Application.Terminate;
  end;
end.

