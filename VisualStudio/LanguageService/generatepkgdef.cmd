rem use this cmd file to generate the pkgdef with the VS2017 tools
set vsdir=c:\Program Files (x86)\Microsoft Visual Studio\2017\Enterprise
set langservicedir=%1
set config=%2
set currentdir=%cd%
rem echo The langservicedir is: %langservicedir%
rem echo The currentdir is: %currentdir%
"%vsdir%\VSSDK\VisualStudioIntegration\Tools\Bin\CreatePkgDef.exe" c:\XSharp\DevPublic\Binaries\2019\%config%\XSharp.LanguageService.dll  /out=c:\XSharp\DevPublic\Binaries\2019\%config%\XSharp.LanguageService.pkgdef /verbose /codebase
copy %cd%\XSharp.LanguageService.pkgdef  %1XSharp.LanguageService.pkgdef
copy %cd%\XSharp.LanguageService.pkgdef  %1..\ProjectPackage\XSharp.LanguageService.pkgdef
