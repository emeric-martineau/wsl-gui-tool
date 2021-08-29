program wslguitool;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, mainwindow, WslApi, WslCommandLine, WslRegistry, ApplicationInfo,
  aboutwindow, distributionpropertieswindow, importdistribution, runcommandwithuser;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TWslGuiToolMainWindow, WslGuiToolMainWindow);
  Application.Run;
end.

