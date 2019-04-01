copy xsharp.msha \xsharp\dev\xsharp\binaries\help
cd \xsharp\dev\xsharp\binaries\help
makecab xsharp.mshc xsharp.cab /V1
cd \xsharp\devrt\binaries\help
makecab xsruntime.mshc xsruntime.cab /V1
copy xsruntime.mshc \xsharp\dev\xsharp\binaries\help
copy xsruntime.cab \xsharp\dev\xsharp\binaries\help
cd \xsharp\devrt\binaries\Vohelp
makecab XsVoSdk.mshc XsVoSdk.cab /V1
copy XsVoSdk.mshc \xsharp\dev\xsharp\binaries\help
copy XsVoSdk.cab  \xsharp\dev\xsharp\binaries\help
cd c:\XSharp\Dev\XSharp\src\Setup

