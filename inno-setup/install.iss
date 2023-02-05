[Setup]
AppName=WslGuiTool
AppVerName=WslGuiTool 0.9
DefaultDirName={commonpf}\WslGuiTool
DefaultGroupName=WslGuiTool
UninstallDisplayIcon={uninstallexe}
LicenseFile=..\LICENSE
WizardImageFile=WizModernImage-IS.bmp
WizardSmallImageFile=WizModernSmallImage-IS.bmp
LanguageDetectionMethod=uilanguage
PrivilegesRequired=lowest
OutputDir=..\..
OutputBaseFilename=wslguitool-setup

[Files]
Source: "..\src\wslguitool.exe"; DestDir: "{app}"; DestName: "wslguitool.exe"

[Icons]
Name: "{group}\WSL GUI Tool"; Filename: "{app}\wslguitool.exe"; WorkingDir: "{app}"

// {localappdata} = C:\Users\XXXXX\AppData\Local\
[UninstallDelete]
Type: filesandordirs; Name: "{localappdata}\wslguitool"
