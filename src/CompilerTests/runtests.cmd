SET ROOT=%~dp0
SET BINARIESDIR=%ROOT%..\..\Artifacts\
SET TESTDIR=%BINARIESDIR%Tests
SET XSCOMPILER=%BINARIESDIR%Bin\xsc\Release\net472\xsc.exe 
SET XSCONFIG=Release
SET XSTESTPROJECT=%ROOT%xSharp Tests30.viproj
SET XSRUNTIMEFOLDER=%ROOT%Runtime
SET XSFIXEDTESTS=TRUE
SET XSLOGFILE=%TESTDIR%\LogFixed.Log
SET INCLUDE=%ROOT%..\Common;%ROOT%Include
IF NOT EXIST %ROOT%Bin MKDIR %ROOT%Bin
IF NOT EXIST %ROOT%Bin\Debug MKDIR %ROOT%Bin\Debug
IF NOT EXIST %ROOT%Bin\Release MKDIR %ROOT%Bin\Release
COPY %XSRUNTIMEFOLDER%\*.* %ROOT%Bin\Debug
COPY %XSRUNTIMEFOLDER%\*.* %ROOT%Bin\Release
IF NOT EXIST %TESTDIR% MKDIR %TESTDIR%
REM Analyzer for tests that use /analyzer (C990). It is built against the XSharp.CodeAnalysis.dll of the tested compiler
dotnet build %ROOT%Analyzers\XSharpTestAnalyzer\XSharpTestAnalyzer.csproj -c Release -o %ROOT%Bin\Analyzers -nologo -v:m
%XSCOMPILER% Automated\CompilerTests.prg /out:%TESTDIR%\CompilerTests.exe /nowarn:165,9101 
%TESTDIR%\CompilerTests.exe
SET XSFIXEDTESTS=False
SET XSLOGFILE=%TESTDIR%\LogBroken.Log
%TESTDIR%\CompilerTests.exe
