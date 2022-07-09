unit helpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;

// As size of the screen can change in time this procedure
// ensures the form will be always visible despite on
// the screen configuration.
procedure RearrangeFormToVisible(CustomForm: TCustomForm);

implementation

function SetRectInsideScreen(Rect: TRect): TRect;
var
  Width, Height: Integer;
begin
  Result := Rect;
  Width := Rect.Right - Rect.Left;
  Height := Rect.Bottom - Rect.Top;
  if Result.Left < Screen.DesktopLeft then begin
    Result.Left := Screen.DesktopLeft;
    Result.Right := Screen.DesktopLeft + Width;
  end;
  if Result.Right > (Screen.DesktopLeft + Screen.DesktopWidth) then begin
    Result.Left := Screen.DesktopLeft + Screen.DesktopWidth - Width;
    Result.Right := Screen.DesktopLeft + Screen.DesktopWidth;
  end;
  if Result.Top < Screen.DesktopTop then begin
    Result.Top := Screen.DesktopTop;
    Result.Bottom := Screen.DesktopTop + Height;
  end;
  if Result.Bottom > (Screen.DesktopTop + Screen.DesktopHeight) then begin
    Result.Top := Screen.DesktopTop + Screen.DesktopHeight - Height;
    Result.Bottom := Screen.DesktopTop + Screen.DesktopHeight;
  end;
end;

procedure RearrangeFormToVisible(CustomForm: TCustomForm);
var
  NewRect: TRect;
begin
  with CustomForm do begin
    NewRect := SetRectInsideScreen(Bounds(Left, Top, Width, Height));
    Left := NewRect.Left;
    Top := NewRect.Top;
    Width := NewRect.Right - NewRect.Left;
    Height := NewRect.Bottom - NewRect.Top;
  end;
end;

end.

