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
  Wslconfig, WslconfigParameterCtrl;


{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TWslGuiToolMainWindow, WslGuiToolMainWindow);
  Application.Run;
end.

