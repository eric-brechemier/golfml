[Setup]
AppName=Golf Helper (32-bit)
AppVersion=0.9
AppCopyright=GNU General Licence
PrivilegesRequired=none
AppId={{94E24DDE-6039-4FFA-A289-713FE3484E0E}
RestartIfNeededByRun=False
DefaultDirName={pf}\GolfHelper
DefaultGroupName=Golfml Applications
OutputDir=E:\LAZARUS\golfml\golfhelper\binaries
OutputBaseFilename=GolfHelperSetup32_V09
SourceDir=E:\LAZARUS\svnprojects\golfapp
SolidCompression=True
ShowLanguageDialog=auto
SetupIconFile=E:\LAZARUS\svnprojects\golfapp\golfml.ico
UninstallDisplayName=Golf Helper (32-bit) golfml Application
UninstallDisplayIcon={app}\golfhelperwin32.ico
VersionInfoVersion=0.9
UsePreviousGroup=False
MinVersion=0,5.01

[Files]
Source: "E:\LAZARUS\svnprojects\golfapp\golfhelperwin32.exe"; DestDir: "{app}"; Permissions: authusers-readexec
Source: "E:\LAZARUS\golfml\coursewriter\source\2.x\golfmlcoursewriterwin32.exe"; DestDir: "{app}"; Permissions: authusers-readexec
Source: "E:\LAZARUS\svnprojects\golfapp\tenerife_amarillagolf.xml"; DestDir: "{app}"; Permissions: everyone-modify
Source: "E:\LAZARUS\svnprojects\golfapp\golfhelperwin32.ico"; DestDir: "{app}"
Source: "E:\LAZARUS\golfml\coursewriter\courses\scorecard_tenerife_abamagolf.xml"; DestDir: "{app}"; Flags: ignoreversion
Source: "E:\LAZARUS\golfml\coursewriter\courses\tenerife_abamagolf.xml"; DestDir: "{app}"; Flags: ignoreversion
Source: "E:\LAZARUS\golfml\coursewriter\courses\tenerife_buenavistagolf.xml"; DestDir: "{app}"; Flags: ignoreversion
Source: "E:\LAZARUS\golfml\coursewriter\courses\tenerife_costaadejegolf.xml"; DestDir: "{app}"; Flags: ignoreversion
Source: "E:\LAZARUS\golfml\coursewriter\courses\tenerife_golfdelsur.xml"; DestDir: "{app}"; Flags: ignoreversion
Source: "E:\LAZARUS\golfml\coursewriter\courses\tenerife_golflasamericas.xml"; DestDir: "{app}"; Flags: ignoreversion

[Icons]
Name: "{app}\Golf Helper Icon"; Filename: "{app}\golfhelperwin32.ico"

[Run]
Filename: "{app}\golfhelperwin32.exe"; WorkingDir: "{app}"; Flags: postinstall runasoriginaluser 32bit; Description: "Run Golf Helper now"
