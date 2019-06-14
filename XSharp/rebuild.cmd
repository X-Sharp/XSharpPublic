@echo off
Echo Cleaning Binaries folder
taskkill  /f /t /fi "IMAGENAME eq XSCompiler.exe" >nul
if exist Binaries\Debug     	rd Binaries\Debug  /s /q
if exist Binaries\Release    	rd Binaries\Release  /s /q
if exist Binaries\Obj			rd Binaries\Obj /s /q
call restore.cmd
call build.cmd Debug
call build.cmd Release

