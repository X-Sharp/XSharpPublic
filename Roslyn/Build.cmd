@echo off
rem call "C:\Program Files (x86)\Microsoft Visual Studio\2019\Enterprise\Common7\Tools\VsDevCmd.bat"
rem set COREHOST_TRACE=1
powershell -noprofile -executionPolicy RemoteSigned -file "%~dp0\build\scripts\build.ps1" -build -skipBuildExtras %*
