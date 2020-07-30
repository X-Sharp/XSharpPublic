@echo off
call "C:\Program Files (x86)\Microsoft Visual Studio\2019\Enterprise\Common7\Tools\VsDevCmd.bat"
powershell -noprofile -executionPolicy RemoteSigned -file "%~dp0\build\scripts\build.ps1" -restore %*
echo Start building compilers
msbuild "%~dp0\Compilers.sln" 
