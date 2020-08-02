@echo off
if "%VSSDKINSTALL%" == "" call "C:\Program Files (x86)\Microsoft Visual Studio\2019\Enterprise\Common7\Tools\VsDevCmd.bat"
set path=%~dp0\..\Roslyn\Binaries\Tools\dotnet;%path%
dotnet pack Compiler.sln