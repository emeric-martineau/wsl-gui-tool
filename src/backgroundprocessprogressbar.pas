{
 /***************************************************************************
                        backgroundprocessprogressbar.pas
                        --------------------------------
 ***************************************************************************/
}
{
@abstract(Provide a Statusbar that check if a TProcess running or finish and call)
@author(Emeric MARTINEAU)
@created(2021)
}
unit BackgroundProcessProgressBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, ExtCtrls, Forms, ComCtrls;

type

  TBackgroundProcessProgressBarFinished = procedure(ExitStatus: integer; Canceled: boolean) of object;
  TBackgroundProcessProgressBarRun = procedure() of object;

  // Background process
  TBackgroundProcessProgressBar = class(TObject)
      FProcess: TProcess;
      FTimer: TTimer;
      FOwner: TForm;
      FCallbackFinished: TBackgroundProcessProgressBarFinished;
      FCallbackRun: TBackgroundProcessProgressBarRun;
      FStdout: TStringList;
      FStderr: TStringList;
      Statusbar: TStatusBar;
      FExecutable: TProcessString;
      FParameters: TProcessStrings;
      FCanceled: boolean;
      FExitStatus: integer;

      procedure CheckProcessStatus(Sender: TObject);
      procedure SetOnDrawPanel(OnDrawPanel: TDrawPanelEvent);
      function GetOnDrawPanel: TDrawPanelEvent;
      function GetOnDblClick: TNotifyEvent;
      procedure SetOnDblClick(OnDblClick: TNotifyEvent);
  public
      Constructor Create(Owner: TForm);
      Constructor Create(Owner: TForm; Intervale: integer);
      Destructor Destroy; override;
      procedure Run(Process: TProcess);
      procedure Refresh;
      function Running: boolean;
      procedure Terminate();
  published
      property Process: TProcess write FProcess;
      property OnRun: TBackgroundProcessProgressBarRun read FCallbackRun write FCallbackRun;
      property OnFinished: TBackgroundProcessProgressBarFinished read FCallbackFinished write FCallbackFinished;
      property OnDrawPanel: TDrawPanelEvent read GetOnDrawPanel write SetOnDrawPanel;
      property OnDblClick: TNotifyEvent read GetOnDblClick write SetOnDblClick;
      property Stdout: TStringList read FStdout;
      property Stderr: TStringList read FStderr;
      property Executable: TProcessString read FExecutable;
      property Parameters: TProcessStrings read FParameters;
      property Canceled: boolean read FCanceled;
      property ExitStatus: integer read FExitStatus;
  end;

implementation

constructor TBackgroundProcessProgressBar.Create(Owner: TForm);
begin
  Create(Owner, 500);
end;

constructor TBackgroundProcessProgressBar.Create(Owner: TForm; Intervale: integer);
var PanelOne: TStatusPanel;
begin
  FOwner := Owner;

  FTimer := TTimer.Create(FOwner);
  FTimer.Enabled := false;
  FTimer.OnTimer := @Self.CheckProcessStatus;
  FTimer.Interval := Intervale;

  FCallbackFinished := nil;
  FCallbackRun := nil;

  Statusbar := TStatusBar.Create(Owner);
  Statusbar.Parent := Owner;
  Statusbar.SimplePanel := false;
  PanelOne := Statusbar.Panels.Add;
  PanelOne.Style := psOwnerDraw;
  PanelOne.Width := 50;

  FStdout := TStringList.Create;
  FStderr := TStringList.Create;

  FParameters := TProcessStringList.Create;
end;

destructor TBackgroundProcessProgressBar.Destroy;
begin
  if FProcess <> nil
  then begin
    FProcess.Terminate(0);
    FProcess.Free;
    FProcess := nil;
  end;

  FTimer.Free;
  FTimer := nil;

  Statusbar.Free;
  Statusbar := nil;

  FStdout.Free;
  FStdout := nil;

  FStderr.Free;
  FStderr := nil;

  FParameters.Free;
  FParameters := nil;
end;

procedure TBackgroundProcessProgressBar.CheckProcessStatus(Sender: TObject);
begin
  if (not FProcess.Running) and (FCallbackFinished <> nil)
  then begin
    FTimer.Enabled := False;

    FStdout.LoadFromStream(FProcess.Output, TEncoding.Unicode);
    FStderr.LoadFromStream(FProcess.Stderr, TEncoding.Unicode);

    FExitStatus := FProcess.ExitStatus;

    FProcess.Free;
    FProcess := nil;

    FCallbackFinished(FExitStatus, FCanceled);
  end;
end;

procedure TBackgroundProcessProgressBar.Run(Process: TProcess);
begin
  FProcess := Process;

  FCanceled := false;
  FExitStatus := 0;

  FStdout.Clear;
  FStderr.Clear;

  FExecutable :=  FProcess.Executable;
  FParameters.AddStrings(FProcess.Parameters, true);

  if (FCallbackRun <> nil)
  then begin
    FCallbackRun();
  end;

  FProcess.Execute;
  FTimer.Enabled := True;
end;

procedure TBackgroundProcessProgressBar.Refresh;
begin
  Statusbar.Refresh;
end;

procedure TBackgroundProcessProgressBar.SetOnDrawPanel(OnDrawPanel: TDrawPanelEvent);
begin
  Statusbar.OnDrawPanel := OnDrawPanel;
end;

function TBackgroundProcessProgressBar.GetOnDrawPanel: TDrawPanelEvent;
begin
  Result := Statusbar.OnDrawPanel;
end;

function TBackgroundProcessProgressBar.GetOnDblClick: TNotifyEvent;
begin
  Result := Statusbar.OnDblClick;
end;

procedure TBackgroundProcessProgressBar.SetOnDblClick(OnDblClick: TNotifyEvent);
begin
  Statusbar.OnDblClick := OnDblClick;
end;

function TBackgroundProcessProgressBar.Running: boolean;
begin
   if FProcess = nil
   then begin
     Result := false;
   end else begin
     Result := FProcess.Running;
   end;
end;

procedure TBackgroundProcessProgressBar.Terminate();
begin
   if FProcess <> nil
   then begin
     FCanceled := true;
     FProcess.Terminate(0);
   end;
end;

end.
