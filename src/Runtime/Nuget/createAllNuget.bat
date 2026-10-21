@echo off
if "%1" == "Debug" goto Ok
if "%1" == "Release" goto Ok
Goto Error
:Ok
copy ..\..\..\Artifacts\%1\Net8.0-windows\*.dll ..\..\..\Artifacts\%1\Net8.0 /y
xsi XSPackNuget.prgx Core %1
xsi XSPackNuget.prgx RT %1
xsi XSPackNuget.prgx VO %1
xsi XSPackNuget.prgx VFP %1
xsi XSPackNuget.prgx XPP %1
xsi XSPackNuget.prgx Harbour %1
xsi XSPackNuget.prgx VOSDK %1
xsi XSPackNuget.prgx VOSDKTyped %1
Goto End
:Error
Echo Syntax: CreateAllNuget debug or CreateAllNuget Release
:End
