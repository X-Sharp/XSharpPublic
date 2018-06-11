@echo off
Echo Cleaning Master Solution
msbuild Master.sln /fl1 /p:Configuration=Debug	/t:Clean /m /v:q  /nologo
msbuild Master.sln /fl2 /p:Configuration=Release	/t:Clean /m /v:q /nologo
if exist clean-debug.log del clean-debug.log
if exist clean-release.log del clean-release.log
rename msbuild1.log clean-debug.log
rename msbuild2.log clean-release.log
if exist binaries\debug\*.* rd binaries\debug /s /q
if exist binaries\obj\*.* rd binaries\obj /s /q
if exist binaries\release\*.* rd binaries\release /s /q