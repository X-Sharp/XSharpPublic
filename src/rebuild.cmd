@echo off
Echo Rebuilding for Debug and Release configuration
Echo Cleaning Binaries folder
SET XSHARPDEV=c:\XSharp\Dev\XSharp\artifacts\bin\xsc\Debug\net472
Echo Using Compiler in location %XSHARPDEV%
if exist Binaries\Debug  rd Binaries\Debug /s /q
if exist Binaries\Release rd Binaries\Release /s /q
if exist Binaries\Obj rd Binaries\Obj /s /q
call build debug
call build release

