rem use this cmd file to generate the pkgdef with the VS2017 tools
set vsdir=c:\Program Files (x86)\Microsoft Visual Studio\2017\Enterprise
set langservicedir=%1
set currentdir=%cd%
rem echo The langservicedir is: %langservicedir%
rem echo The currentdir is: %currentdir%
"%vsdir%\VSSDK\VisualStudioIntegration\Tools\Bin\CreatePkgDef.exe" c:\XSharp\DevPublic\Binaries\2019\Debug\XSharpLanguageService.dll  /out=c:\XSharp\DevPublic\Binaries\2019\Debug\XSharpLanguageService.pkgdef /verbose /codebase
copy %cd%\XSharpLanguageService.pkgdef  %1XSharpLanguageService.pkgdef
copy %cd%\XSharpLanguageService.pkgdef  %1..\ProjectPackage\XSharpLanguageService.pkgdef
