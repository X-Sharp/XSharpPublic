set KEYFILE=c:\XSharp\Dev\XSharp\src\Common\XSharp.snk
cd c:\XSharp\Dev\XSharp\artifacts\bin\FullMacroCompiler\Debug
sn -R XSharp.MacroCompiler.Full.dll   %KEYFILE%
sn -R XSharp.Scripting.dll   %KEYFILE%
sn -R XSharp.CodeAnalysis.dll   %KEYFILE%
cd c:\XSharp\Dev\XSharp\artifacts\bin\FullMacroCompiler\Release
sn -R XSharp.MacroCompiler.Full.dll   %KEYFILE%
sn -R XSharp.Scripting.dll   %KEYFILE%
sn -R XSharp.CodeAnalysis.dll   %KEYFILE%
set KEYFILE=