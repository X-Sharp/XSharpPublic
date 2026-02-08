@echo off
rem First remove old templates when any
dotnet new uninstall XSharp.Templates >>install.log 2>>NUL
rem Then install new templates
for %%i in (*.nupkg) do dotnet new install %%i >>install.log