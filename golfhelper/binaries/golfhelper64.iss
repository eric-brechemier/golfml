[Setup]
AppName=Golf Helper (64-bit)
AppVersion=0.91
AppCopyright=GNU General Licence
PrivilegesRequired=none
AppId={{3819CFD0-305C-4A8C-982A-19FEF99B3C9C}
RestartIfNeededByRun=False
DefaultDirName={pf}\GolfHelper
DefaultGroupName=Golfml Applications
OutputDir=E:\LAZARUS\golfml\golfhelper\binaries
OutputBaseFilename=GolfHelperSetup64_V091
SourceDir=E:\LAZARUS\svnprojects\golfapp
SolidCompression=True
ShowLanguageDialog=auto
SetupIconFile=E:\LAZARUS\svnprojects\golfapp\golfml.ico
UninstallDisplayName=Golf Helper (64-bit) golfml Application
UninstallDisplayIcon={app}\golfhelperwin64.ico
VersionInfoVersion=0.9
UsePreviousGroup=False
MinVersion=0,6.0
ArchitecturesInstallIn64BitMode=x64

[Files]
Source: "E:\LAZARUS\svnprojects\golfapp\golfhelperwin64.exe"; DestDir: "{app}"; Permissions: authusers-readexec
Source: "E:\LAZARUS\golfml\coursewriter\source\2.x\golfmlcoursewriterwin64.exe"; DestDir: "{app}"; Permissions: authusers-readexec
Source: "E:\LAZARUS\svnprojects\golfapp\tenerife_amarillagolf.xml"; DestDir: "{app}"; Permissions: everyone-modify
Source: "E:\LAZARUS\svnprojects\golfapp\golfhelperwin64.ico"; DestDir: "{app}"
Source: "E:\LAZARUS\golfml\coursewriter\courses\scorecard_tenerife_abamagolf.xml"; DestDir: "{app}"; Flags: ignoreversion
Source: "E:\LAZARUS\golfml\coursewriter\courses\tenerife_abamagolf.xml"; DestDir: "{app}"; Flags: ignoreversion
Source: "E:\LAZARUS\golfml\coursewriter\courses\tenerife_buenavistagolf.xml"; DestDir: "{app}"; Flags: ignoreversion
Source: "E:\LAZARUS\golfml\coursewriter\courses\tenerife_costaadejegolf.xml"; DestDir: "{app}"; Flags: ignoreversion
Source: "E:\LAZARUS\golfml\coursewriter\courses\tenerife_golfdelsur.xml"; DestDir: "{app}"; Flags: ignoreversion
Source: "E:\LAZARUS\golfml\coursewriter\courses\tenerife_golflasamericas.xml"; DestDir: "{app}"; Flags: ignoreversion

[Icons]
Name: "{group}\GolfHelper"; Filename: "{app}\golfhelperwin64.ico"

[Run]
Filename: "{app}\golfhelperwin64.exe"; WorkingDir: "{app}"; Flags: postinstall runasoriginaluser 64bit; Description: "Run Golf Helper now"
