@echo off
set msbuilddir=c:\Program Files (x86)\Microsoft Visual Studio\2017\Enterprise\MSBuild\15.0\Bin\
if exist "%msbuilddir%msbuild.exe" goto found
set msbuilddir=c:\Program Files (x86)\Microsoft Visual Studio\2017\Professional\MSBuild\15.0\Bin\
if exist "%msbuilddir%msbuild.exe" goto found
set msbuilddir=c:\Program Files (x86)\Microsoft Visual Studio\2017\Community\MSBuild\15.0\Bin\
if exist "%msbuilddir%msbuild.exe" goto found
set msbuilddir=d:\Program Files (x86)\Microsoft Visual Studio\2017\Enterprise\MSBuild\15.0\Bin\
if exist "%msbuilddir%msbuild.exe" goto found
set msbuilddir=d:\Program Files (x86)\Microsoft Visual Studio\2017\Professional\MSBuild\15.0\Bin\
if exist "%msbuilddir%msbuild.exe" goto found
set msbuilddir=d:\Program Files (x86)\Microsoft Visual Studio\2017\Community\MSBuild\15.0\Bin\
if exist "%msbuilddir%msbuild.exe" goto found
goto notfound
:found
goto end
:notfound
set msbuilddir=
echo Cannot locate VS 2017 MsBuild.exe
goto end
:end