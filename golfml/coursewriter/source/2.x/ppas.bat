@echo off
SET THEFILE=golfmlcoursewriterwin64.exe
echo Linking %THEFILE%
C:\lazarus32\fpc\2.6.1\bin\x86_64-win64\ld.exe -b pei-x86-64  --gc-sections  -s --subsystem windows --entry=_WinMainCRTStartup    -o golfmlcoursewriterwin64.exe link.res
if errorlevel 1 goto linkend
goto end
:asmend
echo An error occured while assembling %THEFILE%
goto end
:linkend
echo An error occured while linking %THEFILE%
:end
