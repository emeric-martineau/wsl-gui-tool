unit applicationinfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows;


function GetAppVersionStr: string;

implementation

function GetAppVersionStr: string;
var
   Exe: string;
   Size: DWord;
   Buffer: Pointer;
   FixedFileInfo: PVSFixedFileInfo;
   FixedFileInfoSize: DWord;
begin
 Result := '?.?.?.?';
 Exe := ParamStr(0);
 Size := 0;

 Size := GetFileVersionInfoSize(PChar(Exe), Size);

 if Size > 0 then
 try
    GetMem(Buffer, Size);

    GetFileVersionInfo(PChar(Exe),0, Size, Buffer);

    FixedFileInfoSize := 0;
    FixedFileInfo := nil;

    if VerQueryValue(Buffer, '\', FixedFileInfo, FixedFileInfoSize)
    then begin
      Result := Format('%d.%d.%d.%d',
        [LongRec(FixedFileInfo^.dwFileVersionMS).Hi,  //major
         LongRec(FixedFileInfo^.dwFileVersionMS).Lo,  //minor
         LongRec(FixedFileInfo^.dwFileVersionLS).Hi,  //release
         LongRec(FixedFileInfo^.dwFileVersionLS).Lo]) //build
    end;
 finally
    FreeMem(Buffer, Size);
 end;

end;


end.

