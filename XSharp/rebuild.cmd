@echo off
Echo Cleaning Binaries folder
taskkill  /f /t /fi "IMAGENAME eq XSCompiler.exe"
if exist Binaries\Debug_AnyCPU 		rd Binaries\Debug_AnyCPU /s /q
if exist Binaries\Release_AnyCPU 	rd Binaries\Release_AnyCPU /s /q
if exist Binaries\Obj				rd Binaries\Obj /s /q
call build.cmd
