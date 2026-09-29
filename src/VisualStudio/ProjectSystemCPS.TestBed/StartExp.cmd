@echo off
rem Starts the Visual Studio 2022 experimental instance for testing the CPS based X# project system (WP-1).
rem
rem The CPS project type needs the X# MSBuild files of this repository (XSharp.DesignTime.targets and the
rem XSharpCpsProjectSystem import in XSharp.CurrentVersion.targets). They are not installed yet, so this script
rem copies the installed X# MsBuild folder to Artifacts\CpsMsBuild, overlays the changed files from the
rem repository and starts devenv with XSharpMsBuildDir pointing to that folder. The installed X# is not changed.
rem
rem Usage: StartExp.cmd [solution]   (default CpsTestBed.sln; SelectorTestBed.sln tests the project selector:
rem        all projects use the classic X# project type GUID, the SDK-style ones must be loaded by CPS:
rem        CpsTestBed, WinForms\WinFormsTestBed (shadow WinForms designer), VO\VOTestBed (VO designers);
rem        Legacy\LegacyTestBed stays on MPFproj)
rem
rem Prerequisite: a Debug build of VisualStudio\ProjectPackage\ProjectPackage2022.csproj, which deploys the
rem VSIX (including XSharp.ProjectSystemCPS) to the experimental instance.
setlocal
for %%i in ("%~dp0..\..\..") do set REPO=%%~fi
set BUILDTASK=%REPO%\src\Compiler\src\Compiler\XSharpBuildTask
set OVERLAY=%REPO%\Artifacts\CpsMsBuild

if not defined XSharpMsBuildDir (
    echo XSharpMsBuildDir is not set. Is X# installed?
    exit /b 1
)

xcopy /y /e /q /i "%XSharpMsBuildDir%" "%OVERLAY%" >nul
copy /y "%BUILDTASK%\XSharp.CurrentVersion.targets" "%OVERLAY%" >nul
copy /y "%BUILDTASK%\XSharp.CrossTargeting.targets" "%OVERLAY%" >nul
copy /y "%BUILDTASK%\XSharp.DesignTime.targets" "%OVERLAY%" >nul
copy /y "%BUILDTASK%\XSharp.SDK.Props" "%OVERLAY%" >nul
xcopy /y /e /q /i "%BUILDTASK%\Rules" "%OVERLAY%\Rules" >nul

for /f "usebackq delims=" %%i in (`"%ProgramFiles(x86)%\Microsoft Visual Studio\Installer\vswhere.exe" -version [17.0^,18.0^) -latest -property productPath`) do set DEVENV=%%i
if not defined DEVENV (
    echo Visual Studio 2022 not found.
    exit /b 1
)

rem A Debug build deploys the VSIX to the experimental instance without refreshing its image library cache,
rem so new image monikers (XSharp.ProjectSystemCPS.imagemanifest: project and file icons) would not be found.
rem VS rebuilds the cache when it is missing.
for /d %%h in ("%LOCALAPPDATA%\Microsoft\VisualStudio\17.0_*Exp") do if exist "%%h\ImageLibrary\ImageLibrary.cache" del /q "%%h\ImageLibrary\ImageLibrary.cache"

set XSharpMsBuildDir=%OVERLAY%
echo XSharpMsBuildDir=%XSharpMsBuildDir%
set SOLUTION=%~1
if "%SOLUTION%" == "" set SOLUTION=CpsTestBed.sln
start "" "%DEVENV%" /rootSuffix Exp "%~dp0%SOLUTION%"
