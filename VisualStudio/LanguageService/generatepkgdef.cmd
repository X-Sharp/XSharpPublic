rem If the build process fails to generate the Languageservice PkgDef file call:
"c:\Program Files (x86)\Microsoft Visual Studio\2017\Enterprise\VSSDK\VisualStudioIntegration\Tools\Bin\CreatePkgDef.exe" c:\XSharp\DevPublic\Binaries\2019\Debug\XSharpLanguageService.dll  /out=c:\XSharp\DevPublic\Binaries\2019\Debug\XSharpLanguageService.pkgdef /verbose /codebase
copy c:\XSharp\DevPublic\Binaries\2019\Debug\XSharpLanguageService.pkgdef  XSharpLanguageService.pkgdef
copy c:\XSharp\DevPublic\Binaries\2019\Debug\XSharpLanguageService.pkgdef  ..\ProjectPackage\XSharpLanguageService.pkgdef
pause