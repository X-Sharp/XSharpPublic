; Please note that the "deregistering" of the XSharp association is done in a script step at the end of this file

#define Product         "XSharp"
#define ProdVer         "XSharp 0.2.5"
#define ProdBuild       "XSharp Beta 5"
#define Company         "XSharp BV"
#define RegCompany      "XSharpBV"
#define XSharpURL       "http://www.xsharp.info"
#define CopyRight       "Copyright © 2015-2016 XSharp B.V."
#define VIVersion       "0.2.5.2502"
#define VITextVersion   "0.2.5.2502 (Beta 5)"
#define TouchDate       "2016-06-20"
#define TouchTime       "02:05:02"
#define SetupExeName    "XSharpSetup025Final"
#define InstallPath     "XSharpPath"

;Folders
#define BinFolder       "D:\Xsharp\Dev\XSharp\Binaries\Debug\"
#define BinPFolder      "D:\Xsharp\DevPublic\Binaries\Debug\"
#define CommonFolder    "D:\Xsharp\Dev\XSharp\src\Common\"
#define ToolsFolder     "d:\Xsharp\Dev\XSharp\src\Tools\"
#define VSProjectFolder "d:\Xsharp\Dev\XSharp\src\VisualStudio\XSharp.ProjectType\"
#define ExamplesFolder  "D:\Xsharp\DevPublic\Samples\"
#define OutPutFolder    "D:\XSharp\Dev\XSharp\Binaries\Setup"
#define DocFolder       "D:\Xsharp\Dev\XSharp\Binaries\Help\"
#define XIDEFolder      "D:\Xsharp\Dev\XSharp\Xide\"
#define XIDESetup       "XIDE_Set_up_1.04.exe"
#define XIDEVersion     "1.04"
#define StdFlags        "ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname"
#define GACInstall      "gacinstall sharedfile uninsnosharedfileprompt uninsrestartdelete"
#define ProviderVersion "XSharp.CodeDom.XSharpCodeDomProvider, Version=1.0.0.0, Culture=neutral, PublicKeyToken=31c59c566fa38f21"
#define ImmutableVersion "System.Collections.Immutable, Version=1.1.37.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a"
#define MetadataVersion  "System.Reflection.Metadata, Version=1.1.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a"
#define Compression     "lzma2/ultra64"
;#define Compression     "none"

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{32EB192A-B120-4055-800E-74B48B80DA06}
DisableWelcomePage=no
DisableStartupPrompt=yes
DisableReadyMemo=yes
DisableFinishedPage=no
InfoBeforeFile=Baggage\ReadmeShort.rtf
AppName={#Product}
AppVersion={#VIVersion}
AppCopyright={# CopyRight}
AppVerName={#ProdVer}
AppPublisher={#Company}
AppPublisherURL={#XSharpURL}
AppSupportURL={#XSharpURL}
AppUpdatesURL={#XSharpURL}
DefaultDirName={pf}\{#Product}
DefaultGroupName={#Product}
LicenseFile=Baggage\License.txt
OutputDir={#OutPutFolder} 
OutputBaseFilename={#SetupExeName}
OutputManifestFile=Setup-Manifest.txt
SetupIconFile=Baggage\XSharp.ico
Compression={#Compression}
SolidCompression=yes
SetupLogging=yes

; Version Info for Installer and uninstaller
VersionInfoVersion={#= VIVersion}
VersionInfoDescription={# ProdBuild}
VersionInfoCompany={# Company}
VersionInfoTextVersion={#= VITextVersion}
VersionInfoCopyRight={# CopyRight}
VersionInfoProductName={# Product}
VersionInfoProductVersion={# VIVersion}
Wizardsmallimagefile=Baggage\XSharp_Bmp_Banner.bmp 
WizardImagefile=Baggage\XSharp_Bmp_Dialog.bmp

;Uninstaller
UninstallFilesDir={app}\uninst
UninstallDisplayName={#=ProdBuild}
UninstallDisplayIcon={app}\Images\XSharp.ico;
UninstallLogMode=overwrite


TouchDate={#=TouchDate}
TouchTime={#=TouchTime}


; Make sure they are admins
PrivilegesRequired=admin
; Make sure they are running on Windows 2000 Or Higher
Minversion=6.0.600


[Components]
Name: "main";             Description: "The XSharp Compiler and Build System";        Types: full compact custom; Flags: fixed; 
Name: "vs2015";           Description: "Visual Studio 2015 Integration";              Types: full custom;         Check: Vs2015IsInstalled;
Name: "vs2015\vulcanprg"; Description: "Keep Vulcan associated with PRG files";       Types: full custom;         Check: VulcanPrgAssociated;
Name: "vs2015\help";      Description: "Install VS documentation";                    Types: full custom;         Check: HelpViewer22Found;
Name: "vsnext";           Description: "Visual Studio 15 Preview Integration";        Types: full custom;         Check: VsNextIsInstalled;
Name: "vsnext\help";      Description: "Install VS documentation";                    Types: full custom;         Check: HelpViewer23Found;
Name: "xide";             Description: "Include the XIDE {# XIDEVersion} installer";  Types: full custom;                  


[Dirs]
Name: "{app}\Assemblies";
Name: "{app}\Bin";
Name: "{app}\Help";
Name: "{app}\Images";
Name: "{app}\Include";
Name: "{app}\ProjectSystem";                                
Name: "{app}\Redist"
Name: "{app}\Snippets"
Name: "{app}\Tools";
Name: "{app}\Uninst";
Name: "{app}\Xide";
Name: "{code:GetVs2015IdeDir}\Extensions\XSharp";                              Components: vs2015; 
Name: "{userdocs}\Visual Studio 2015\Code Snippets\XSharp\My Code Snippets";   Components: vs2015; 
Name: "{code:GetVsNextIdeDir}\Extensions\XSharp";                              Components: vsNext; 
Name: "{userdocs}\Visual Studio 15\Code Snippets\XSharp\My Code Snippets";     Components: vsNext; 


[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Files]
; Main program
Source: "{#BinFolder}xsc.exe";                            DestDir: "{app}\bin"; Flags: {#StdFlags}; Components: main
Source: "{#BinFolder}xsc.rsp";                            DestDir: "{app}\bin"; Flags: {#StdFlags}; Components: main
Source: "{#BinFolder}XSCompiler.exe";                     DestDir: "{app}\bin"; Flags: {#StdFlags}; Components: main
Source: "{#BinFolder}XSharp.CodeAnalysis.dll";            DestDir: "{app}\bin"; Flags: {#StdFlags}; Components: main
Source: "{#BinPFolder}baggage\rc.exe";                    DestDir: "{app}\bin"; Flags: {#StdFlags}; Components: main
Source: "{#BinPFolder}baggage\rcdll.dll";                 DestDir: "{app}\bin"; Flags: {#StdFlags}; Components: main


; PDB files
Source: "{#BinFolder}XSharp.CodeAnalysis.pdb";            DestDir: "{app}\bin"; Flags: {#StdFlags}; Components: main
Source: "{#BinFolder}xsc.pdb";                            DestDir: "{app}\bin"; Flags: {#StdFlags}; Components: main
Source: "{#BinFolder}XSCompiler.pdb";                     DestDir: "{app}\bin"; Flags: {#StdFlags}; Components: main

; GAC files in Bin folder
Source: "{#BinFolder}System.Collections.Immutable.dll";   DestDir: "{app}\bin"; StrongAssemblyName: "{#ImmutableVersion}"; Flags: {#StdFlags} {#GACInstall}; components: main
Source: "{#BinFolder}System.Reflection.Metadata.dll";     DestDir: "{app}\bin"; StrongAssemblyName: "{#MetadataVersion}";  Flags: {#StdFlags} {#GACInstall}; components: main

; Support files
Source: "Baggage\Readme.rtf";                             DestDir: "{app}"    ; Flags: isreadme {#StdFlags}; Components: main
Source: "Baggage\Redist.txt";                             DestDir: "{app}\Redist" ; Flags: {#StdFlags}; Components: main
Source: "Baggage\XSharp.ico";                             DestDir: "{app}\Images"; Flags: touch {#StdFlags}; Components: main
;Source: "Baggage\License.rtf";                            DestDir: "{app}";        Flags: touch {#StdFlags}; Components: main
Source: "Baggage\License.txt";                            DestDir: "{app}";        Flags: touch {#StdFlags}; Components: main

; Include Files
Source: "{#CommonFolder}*.xh";                            DestDir: "{app}\Include"; Flags: touch {#StdFlags}; Components: main

;MsBuild Files
Source: "{#BinPFolder}Xaml\*.*";                          DestDir: "{pf}\MsBuild\{#Product}\Rules";  Flags: {#StdFlags} uninsneveruninstall; Components: main
Source: "{#BinPFolder}Targets\*.*";                       DestDir: "{pf}\MsBuild\{#Product}";        Flags: {#StdFlags} uninsneveruninstall; Components: main
Source: "{#BinPFolder}XSharp.Build.dll";                  DestDir: "{pf}\MsBuild\{#Product}";        Flags: {#StdFlags} uninsneveruninstall; Components: main

;Documentation
Source: "{#DocFolder}\XSharp.pdf";                        DestDir: "{app}\Help";        Flags: touch {#StdFlags}; Components: main;
Source: "{#DocFolder}\XSharp.chm";                        DestDir: "{app}\Help";        Flags: touch {#StdFlags}; Components: main;
Source: "{#DocFolder}\XSVulcan.chm";                      DestDir: "{app}\Help";        Flags: touch {#StdFlags}; Components: main;
Source: "{#DocFolder}\XSharp.msha";                       DestDir: "{app}\Help";        Flags: touch {#StdFlags}; Components: vs2015\help or vsnext\help;     
Source: "{#DocFolder}\XSharp.mshc";                       DestDir: "{app}\Help";        Flags: touch {#StdFlags}; Components: vs2015\help or vsnext\help;
Source: "{#DocFolder}\XSVulcan.mshc";                     DestDir: "{app}\Help";        Flags: touch {#StdFlags}; Components: vs2015\help or vsnext\help;

;XIDE
Source: "{#XIDEFolder}{#XIDESetup}";                      DestDir: "{app}\Xide";        Flags: touch {#StdFlags}; Components: Xide

;VsProjectSystem
Source: "{#BinPFolder}XSharpProject2015.vsix";            DestDir: "{app}\ProjectSystem"; Flags: {#StdFlags}; Components: vs2015 or vsnext

Source: "{#BinFolder}XSharp.CodeAnalysis.dll";            DestDir: "{code:GetVs2015IdeDir}\PrivateAssemblies"; Flags: {#StdFlags}; Components: vs2015
Source: "{#BinFolder}XSharp.CodeAnalysis.pdb";            DestDir: "{code:GetVs2015IdeDir}\PrivateAssemblies"; Flags: {#StdFlags}; Components: vs2015
Source: "{#BinPFolder}XSharpCodeDomProvider.dll";         DestDir: "{code:GetVs2015IdeDir}\PrivateAssemblies"; Flags: {#StdFlags} {#GACInstall}; StrongAssemblyName: "{#ProviderVersion}"; Components: vs2015
Source: "{#BinPFolder}XSharpCodeDomProvider.pdb";         DestDir: "{code:GetVs2015IdeDir}\PrivateAssemblies"; Flags: {#StdFlags}; Components: vs2015
Source: "{#BinPFolder}XSharpColorizer2015.dll";           DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: {#StdFlags}; Components: vs2015
Source: "{#BinPFolder}XSharpColorizer2015.pdb";           DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: {#StdFlags}; Components: vs2015

; ItemTemplates per folder
Source: "{#BinPFolder}Itemtemplates\Wpf*.Zip";            DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\ItemTemplates\WPF";        Flags: recursesubdirs {#StdFlags}; Components: vs2015
Source: "{#BinPFolder}Itemtemplates\VO*.Zip";             DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\ItemTemplates\VO";         Flags: recursesubdirs {#StdFlags}; Components: vs2015
Source: "{#BinPFolder}Itemtemplates\*Internal.Zip";       DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\ItemTemplates\Internal";   Flags: recursesubdirs {#StdFlags}; Components: vs2015
Source: "{#BinPFolder}Itemtemplates\Wcf*.Zip";            DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\ItemTemplates\WCF";        Flags: recursesubdirs {#StdFlags}; Components: vs2015
Source: "{#BinPFolder}Itemtemplates\Form*.Zip";           DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\ItemTemplates\Windows Forms"; Flags: recursesubdirs {#StdFlags}; Components: vs2015
Source: "{#BinPFolder}Itemtemplates\C*.Zip";              DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\ItemTemplates\Code";          Flags: recursesubdirs {#StdFlags}; Components: vs2015
Source: "{#BinPFolder}Itemtemplates\H*.Zip";              DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\ItemTemplates\Code";          Flags: recursesubdirs {#StdFlags}; Components: vs2015
Source: "{#BinPFolder}Itemtemplates\T*.Zip";              DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\ItemTemplates\Code";          Flags: recursesubdirs {#StdFlags}; Components: vs2015
Source: "{#BinPFolder}Itemtemplates\R*.Zip";              DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\ItemTemplates\Resources";     Flags: recursesubdirs {#StdFlags}; Components: vs2015

Source: "{#BinPFolder}ProjectTemplates\*.*";              DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\ProjectTemplates";  Flags: recursesubdirs {#StdFlags}; Components: vs2015

Source: "{#BinPFolder}XSharpProject2015.dll";             DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: {#StdFlags}; Components: vs2015
Source: "{#BinPFolder}XSharpProject2015.dll.config";      DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: {#StdFlags}; Components: vs2015
Source: "{#BinPFolder}XSharpProject2015.pdb";             DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: {#StdFlags}; Components: vs2015
Source: "{#BinPFolder}XSharpProject2015.pkgdef";          DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: {#StdFlags}; Components: vs2015; AfterInstall: AdjustPkgDef;
Source: "{#BinPFolder}extension.vsixmanifest";            DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: {#StdFlags}; Components: vs2015

Source: "{#BinPFolder}XSharp.ico ";                             DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp";        Flags: {#StdFlags}; Components: vs2015
Source: "{#BinPFolder}XSharpVSIXLogo.png ";                DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp";  Flags: {#StdFlags}; Components: vs2015
Source: "{#BinFolder}XSharp.CodeAnalysis.dll";                  DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp";         Flags: {#StdFlags}; Components: vs2015 
;Source: "{#VsProjectFolder}Images\XSharpImages.imagemanifest";  DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\Images";  Flags: {#StdFlags}; Components: vs2015


; private snippets


; VsNext
Source: "{#BinFolder}XSharp.CodeAnalysis.dll";            DestDir: "{code:GetVsNextIdeDir}\PrivateAssemblies"; Flags: {#StdFlags}; Components: vsnext
Source: "{#BinFolder}XSharp.CodeAnalysis.pdb";            DestDir: "{code:GetVsNextIdeDir}\PrivateAssemblies"; Flags: {#StdFlags}; Components: vsnext
Source: "{#BinPFolder}XSharpCodeDomProvider.dll";         DestDir: "{code:GetVsNextIdeDir}\PrivateAssemblies"; Flags: {#StdFlags} {#GACInstall}; StrongAssemblyName: "{#ProviderVersion}"; Components: vsnext
Source: "{#BinPFolder}XSharpCodeDomProvider.pdb";         DestDir: "{code:GetVsNextIdeDir}\PrivateAssemblies"; Flags: {#StdFlags}; Components: vsnext
Source: "{#BinPFolder}XSharpColorizer2015.dll";           DestDir: "{code:GetVsNextIdeDir}\Extensions\XSharp"; Flags: {#StdFlags}; Components: vsnext
Source: "{#BinPFolder}XSharpColorizer2015.pdb";           DestDir: "{code:GetVsNextIdeDir}\Extensions\XSharp"; Flags: {#StdFlags}; Components: vsnext

; ItemTemplates per folder
Source: "{#BinPFolder}Itemtemplates\Wpf*.Zip";            DestDir: "{code:GetVsNextIdeDir}\Extensions\XSharp\ItemTemplates\WPF";        Flags: recursesubdirs {#StdFlags}; Components: vsnext
Source: "{#BinPFolder}Itemtemplates\VO*.Zip";             DestDir: "{code:GetVsNextIdeDir}\Extensions\XSharp\ItemTemplates\VO";         Flags: recursesubdirs {#StdFlags}; Components: vsnext
Source: "{#BinPFolder}Itemtemplates\*Internal.Zip";       DestDir: "{code:GetVsNextIdeDir}\Extensions\XSharp\ItemTemplates\Internal";   Flags: recursesubdirs {#StdFlags}; Components: vsnext
Source: "{#BinPFolder}Itemtemplates\Wcf*.Zip";            DestDir: "{code:GetVsNextIdeDir}\Extensions\XSharp\ItemTemplates\WCF";        Flags: recursesubdirs {#StdFlags}; Components: vsnext
Source: "{#BinPFolder}Itemtemplates\Form*.Zip";           DestDir: "{code:GetVsNextIdeDir}\Extensions\XSharp\ItemTemplates\Windows Forms"; Flags: recursesubdirs {#StdFlags}; Components: vsnext
Source: "{#BinPFolder}Itemtemplates\C*.Zip";              DestDir: "{code:GetVsNextIdeDir}\Extensions\XSharp\ItemTemplates\Code";          Flags: recursesubdirs {#StdFlags}; Components: vsnext
Source: "{#BinPFolder}Itemtemplates\H*.Zip";              DestDir: "{code:GetVsNextIdeDir}\Extensions\XSharp\ItemTemplates\Code";          Flags: recursesubdirs {#StdFlags}; Components: vsnext
Source: "{#BinPFolder}Itemtemplates\T*.Zip";              DestDir: "{code:GetVsNextIdeDir}\Extensions\XSharp\ItemTemplates\Code";          Flags: recursesubdirs {#StdFlags}; Components: vsnext
Source: "{#BinPFolder}Itemtemplates\R*.Zip";              DestDir: "{code:GetVsNextIdeDir}\Extensions\XSharp\ItemTemplates\Resources";     Flags: recursesubdirs {#StdFlags}; Components: vsnext

Source: "{#BinPFolder}ProjectTemplates\*.*";              DestDir: "{code:GetVsNextIdeDir}\Extensions\XSharp\ProjectTemplates";  Flags: recursesubdirs {#StdFlags}; Components: vsnext

Source: "{#BinPFolder}XSharpProject2015.dll";             DestDir: "{code:GetVsNextIdeDir}\Extensions\XSharp";  Flags: {#StdFlags}; Components: vsnext
Source: "{#BinPFolder}XSharpProject2015.dll.config";      DestDir: "{code:GetVsNextIdeDir}\Extensions\XSharp";  Flags: {#StdFlags}; Components: vsnext
Source: "{#BinPFolder}XSharpProject2015.pdb";             DestDir: "{code:GetVsNextIdeDir}\Extensions\XSharp";  Flags: {#StdFlags}; Components: vsnext
Source: "{#BinPFolder}XSharpProject2015.pkgdef";          DestDir: "{code:GetVsNextIdeDir}\Extensions\XSharp";  Flags: {#StdFlags}; Components: vsnext
Source: "{#BinPFolder}extension.vsixmanifest";            DestDir: "{code:GetVsNextIdeDir}\Extensions\XSharp";  Flags: {#StdFlags}; Components: vsnext

Source: "{#BinPFolder}XSharp.ico ";                             DestDir: "{code:GetVsNextIdeDir}\Extensions\XSharp";        Flags: {#StdFlags}; Components: vsnext
Source: "{#BinPFolder}XSharpVSIXLogo.png ";                DestDir: "{code:GetVsNextIdeDir}\Extensions\XSharp";        Flags: {#StdFlags}; Components: vsnext
Source: "{#BinFolder}XSharp.CodeAnalysis.dll";                  DestDir: "{code:GetVsNextIdeDir}\Extensions\XSharp";        Flags: {#StdFlags}; Components: vsnext 
;Source: "{#VsProjectFolder}Images\XSharpImages.imagemanifest";  DestDir: "{code:GetVsNextIdeDir}\Extensions\XSharp\Images"; Flags: {#StdFlags}; Components: vsnext

; Examples
Source: "{#ExamplesFolder}*.prg";                             DestDir: "{commondocs}\XSharp\Examples";    Flags: recursesubdirs {#StdFlags};
Source: "{#ExamplesFolder}*.txt";                             DestDir: "{commondocs}\XSharp\Examples";    Flags: recursesubdirs {#StdFlags};
Source: "{#ExamplesFolder}*.vh";                              DestDir: "{commondocs}\XSharp\Examples";    Flags: recursesubdirs {#StdFlags};
Source: "{#ExamplesFolder}*.sln";                             DestDir: "{commondocs}\XSharp\Examples";    Flags: recursesubdirs {#StdFlags};
Source: "{#ExamplesFolder}*.xsproj";                          DestDir: "{commondocs}\XSharp\Examples";    Flags: recursesubdirs {#StdFlags};


; update machine.config
Source:"{#ToolsFolder}Various\RegisterProvider.exe";          DestDir: "{app}\Tools";                     Flags: {#StdFlags}

[Icons]
Name: "{group}\{cm:ProgramOnTheWeb,{#Product}}"; Filename: "{#XSharpURL}";IconFilename:{app}\Images\XSharp.ico;
Name: "{group}\{cm:UninstallProgram,{#Product}}"; Filename: "{uninstallexe}"; 
Name: "{group}\{#Product} Documenation (CHM)"; Filename: "{app}\Help\XSharp.chm"; 
Name: "{group}\{#Product} Documenation (PDF)"; Filename: "{app}\Help\XSharp.pdf"; 
Name: "{group}\{#Product} Vulcan Runtime Reference (CHM)"; Filename: "{app}\Help\XSVulcan.chm"; 
Name: "{group}\{cm:UninstallProgram,{#Product}}"; Filename: "{uninstallexe}"; 
Name: "{group}\{#Product} Examples"; Filename: "{commondocs}\XSharp\Examples";
Name: "{app}\Examples";  Filename: "{commondocs}\XSharp\Examples";


[Registry]
Root: HKLM; Subkey: "Software\{#RegCompany}"; Flags: uninsdeletekeyifempty 
Root: HKLM; Subkey: "Software\{#RegCompany}\{#Product}"; Flags: uninsdeletekey 
Root: HKLM; Subkey: "Software\{#RegCompany}\{#Product}"; ValueName: "{#InstallPath}"; ValueType: string; ValueData: "{app}" ;
Root: HKLM; Subkey: "Software\{#RegCompany}\{#Product}"; ValueName: "Help22Installed"; ValueType: string; ValueData: "yes" ;  Components: vs2015\help;
Root: HKLM; Subkey: "Software\{#RegCompany}\{#Product}"; ValueName: "Help23Installed"; ValueType: string; ValueData: "yes" ;  Components: vsnext\help;

; set the VSHelp to Offline
Root: HKCU; Subkey: "Software\Microsoft\VisualStudio\14.0\Help"; ValueName:"UseOnlineHelp"; ValueType: dword; ValueData: 0; Components: vs2015\help; Flags: noerror;
Root: HKCU; Subkey: "Software\Microsoft\VisualStudio\15.0\Help"; ValueName:"UseOnlineHelp"; ValueType: dword; ValueData: 0; Components: vsnext\help; Flags: noerror;


[Ini]
Filename: "{code:GetVs2015IdeDir}\Extensions\extensions.configurationchanged"; Section:"XSharp"; Key: "Installed"; String: "{#VIVersion}"; Flags: uninsdeletesection; Components: vs2015;
Filename: "{code:GetVsNextIdeDir}\Extensions\extensions.configurationchanged"; Section:"XSharp"; Key: "Installed"; String: "{#VIVersion}"; Flags: uninsdeletesection; Components: vsnext;

[Run]
Filename: "{app}\Tools\RegisterProvider.exe";

; Remove old Help contents
Filename: "{code:GetHelp22Dir}\HlpCtntMgr.exe"; Parameters: "/silent /operation uninstall /catalogname VisualStudio14 /locale en-us /vendor ""XSharp"" /productname ""X#"" /booklist ""X# Documentation"" /wait 0";   Components: vs2015\help; StatusMsg:"UnInstalling VS Help for VS2015";        Flags: waituntilidle;
Filename: "{code:GetHelp23Dir}\HlpCtntMgr.exe"; Parameters: "/silent /operation uninstall /catalogname VisualStudio15 /locale en-us /vendor ""XSharp"" /productname ""X#"" /booklist ""X# Documentation"" /wait 0";   Components: vsnext\help; StatusMsg:"UnInstalling VS Help for VS 15"; Flags: waituntilidle;

Filename: "{code:GetHelp22Dir}\HlpCtntMgr.exe"; Parameters: "/operation install /catalogname VisualStudio14 /locale en-us /sourceuri ""{app}\help\XSharp.msha"" /wait 0";     Components: vs2015\help; StatusMsg:"Installing VS Help for VS2015"; Flags: waituntilidle;
Filename: "{code:GetHelp23Dir}\HlpCtntMgr.exe"; Parameters: "/operation install /catalogname VisualStudio15 /locale en-us /sourceuri ""{app}\help\XSharp.msha"" /wait 0";     Components: vsnext\help; StatusMsg:"Installing VS Help for VS 15";  Flags: waituntilidle;
Filename:  "{app}\Xide\{#XIDESetup}"; Description:"Run XIDE {# XIDEVersion} Installer"; Flags: postInstall;  Components: XIDE;

[UninstallRun]
; This XSharp program deletes the templates cache folder and the extensionmanager key in the registry
;Filename: "{app}\uninst\XsVsUnInst.exe"; Flags: runhidden;  Components: vs2015 ;

Filename: "{code:GetHelp22Dir}\HlpCtntMgr.exe"; Parameters: "/silent /operation uninstall /catalogname VisualStudio14 /locale en-us /vendor ""XSharp"" /productname ""X#"" /booklist ""X# Documentation"" /wait 0";   Components: vs2015\help; StatusMsg:"UnInstalling VS Help for VS2015";        Flags: waituntilidle;
Filename: "{code:GetHelp23Dir}\HlpCtntMgr.exe"; Parameters: "/silent /operation uninstall /catalogname VisualStudio15 /locale en-us /vendor ""XSharp"" /productname ""X#"" /booklist ""X# Documentation"" /wait 0";   Components: vsnext\help; StatusMsg:"UnInstalling VS Help for VS 15"; Flags: waituntilidle;


[InstallDelete]
; The old License.rtf file.
; Template cache, component cache and previous installation of our project system
; vs2015
Type: files;          Name: "{app}\License.rtf"; 

Type: filesandordirs; Name: "{localappdata}\Microsoft\VisualStudio\14.0\vtc"    ; Components: vs2015
Type: filesandordirs; Name: "{localappdata}\Microsoft\VisualStudio\14.0\ComponentModelCache"    ; Components: vs2015
Type: filesandordirs; Name: "{code:GetVs2015IdeDir}\Extensions\XSharp";       Components: vs2015
Type: files;          Name: "{code:GetVs2015IdeDir}\XSharp.CodeAnalysis.dll"; Components: vs2015
Type: files;          Name: "{code:GetVs2015IdeDir}\XSharp.CodeAnalysis.pdb"; Components: vs2015
; vsnext
Type: filesandordirs; Name: "{localappdata}\Microsoft\VisualStudio\15.0\vtc"    ; Components: vsnext
Type: filesandordirs; Name: "{localappdata}\Microsoft\VisualStudio\15.0\ComponentModelCache"    ; Components: vsnext
Type: filesandordirs; Name: "{code:GetVsNextIdeDir}\Extensions\XSharp";       Components: vsnext
Type: files;          Name: "{code:GetVsNextIdeDir}\XSharp.CodeAnalysis.dll"; Components: vsnext
Type: files;          Name: "{code:GetVsNextIdeDir}\XSharp.CodeAnalysis.pdb"; Components: vsnext

; remove the old uninstaller because the uninstall file format has changed in one of the previous builds
Type: filesandordirs; Name: "{app}\Uninst"

[UninstallDelete]
Type: filesandordirs; Name: "{app}\Assemblies"                    ; Components: main
Type: filesandordirs; Name: "{app}\Bin"                           ; Components: main
Type: filesandordirs; Name: "{app}\Help"                          ; Components: main
Type: filesandordirs; Name: "{app}\Images"                        ; Components: main
Type: filesandordirs; Name: "{app}\ProjectSystem"                 ; Components: main
Type: filesandordirs; Name: "{app}\Redist"                        ; Components: main
Type: filesandordirs; Name: "{app}\Tools"                         ; Components: main
Type: filesandordirs; Name: "{app}\Uninst"                        ; Components: main
Type: filesandordirs; Name: "{pf}\MsBuild\{#Product}"             ; Components: main
Type: filesandordirs; Name: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Components: vs2015;  
Type: filesandordirs; Name: "{code:GetVsNextIdeDir}\Extensions\XSharp"; Components: vsnext;  
Type: filesandordirs; Name: "{commondocs}\XSharp\Examples";
Type: dirifempty;     Name: "{app}\Include"; 
Type: dirifempty;     Name: "{app}\Xide"; 
Type: dirifempty;     Name: "{app}"; 

Type: dirifempty;     Name: "{commondocs}\XSharp"; 

; Template cache and component cache
;vs2015
Type: filesandordirs; Name: "{localappdata}\Microsoft\VisualStudio\14.0\vtc"; 			Components: vs2015
Type: filesandordirs; Name: "{localappdata}\Microsoft\VisualStudio\14.0\ComponentModelCache"; 	Components: vs2015
;vsnext
Type: filesandordirs; Name: "{localappdata}\Microsoft\VisualStudio\15.0\vtc"; 			Components: vsnext
Type: filesandordirs; Name: "{localappdata}\Microsoft\VisualStudio\15.0\ComponentModelCache"; 	Components: vsnext

[Messages]
WelcomeLabel1=Welcome to {# Product} (X#)
WelcomeLabel2=This installer will install {#ProdBuild} on your computer.%n%nIt is recommended that you close all other applications before continuing, especially all running copies of Visual Studio.
WizardInfoBefore=Warning
InfoBeforeLabel=You are about to install Beta software
InfoBeforeClickLabel=Only continue the installation if you are aware of the following:


[Code]
Program setup;
var
  PrintButton: TButton;
  Vs2015Path : String;
  Vs2015Installed: Boolean;
  Vs2015BaseDir: String;
  VulcanInstalled: Boolean;
  VulcanBaseDir: String;
  VsNextPath : String;
  VsNextInstalled: Boolean;
  VsNextBaseDir: String;
  VulcanPrgAssociation: Boolean;
  VulcanGuid : String;
  HelpViewer22Installed : Boolean;
  HelpViewer22Dir : String;
  OurHelp22Installed: Boolean;
  HelpViewer23Installed : Boolean;
  HelpViewer23Dir : String;
  OurHelp23Installed: Boolean;

procedure PrintButtonClick(Sender: TObject);
var ResultCode :integer;
begin
ExtractTemporaryFile('license.txt');
if not ShellExec('Print', ExpandConstant('{tmp}\license.txt'),
     '', '', SW_SHOW, ewNoWait, ResultCode) then
//if not ShellExec('', ExpandConstant('{tmp}\license.txt'),
//     '', '', SW_SHOW, ewNoWait, ResultCode) then
end;
procedure DetectVS();
var temp : String;
begin
  VulcanInstalled := RegQueryStringValue(HKEY_LOCAL_MACHINE,'SOFTWARE\Grafx\Vulcan.NET','InstallPath',VulcanBaseDir) ;
  Vs2015Installed := RegQueryStringValue(HKEY_LOCAL_MACHINE,'SOFTWARE\Microsoft\VisualStudio\SxS\VS7','14.0',Vs2015BaseDir) ;
  if Vs2015Installed then Vs2015Path := Vs2015BaseDir+'\Common7\Ide\';
  VsNextInstalled := RegQueryStringValue(HKEY_LOCAL_MACHINE,'SOFTWARE\Microsoft\VisualStudio\SxS\VS7','15.0',VsNextBaseDir) ;
  HelpViewer22Installed := RegQueryStringValue(HKEY_LOCAL_MACHINE,'SOFTWARE\Microsoft\Help\v2.2','AppRoot',HelpViewer22Dir) ;
  HelpViewer23Installed := RegQueryStringValue(HKEY_LOCAL_MACHINE,'SOFTWARE\Microsoft\Help\v2.3','AppRoot',HelpViewer23Dir) ;
  if VsNextInstalled then VsNextPath := VsNextBaseDir+'\Common7\Ide\';
  VulcanPrgAssociation := false;
  if Vs2015Installed then
  begin
  VulcanPrgAssociation := RegQueryStringValue(HKEY_LOCAL_MACHINE, 'SOFTWARE\WOW6432Node\Microsoft\VisualStudio\14.0\Languages\File Extensions\.prg', '', VulcanGuid) ;
  if VulcanPrgAssociation then 
      begin
      VulcanPrgAssociation := (UpperCase(VulcanGuid) = '{8D3F6D25-C81C-4FD8-9599-2F72B5D4B0C9}');
      end
  end
  OurHelp22Installed := RegQueryStringValue(HKEY_LOCAL_MACHINE,'Software\{#RegCompany}\{#Product}','Help22Installed',temp) ;
  OurHelp23Installed := RegQueryStringValue(HKEY_LOCAL_MACHINE,'Software\{#RegCompany}\{#Product}','Help23Installed',temp) ;
end;


{function ResetExtensionManager: Boolean;
begin
  RegDeleteKeyIncludingSubKeys(HKEY_CURRENT_USER,'SOFTWARE\Microsoft\VisualStudio\14.0\ExtensionManager');
  result := True;
end;
}

function VulcanIsInstalled: Boolean;
begin
  result := VulcanInstalled;
end;

function GetVulcanDir(Param: String): String;
begin
  result := VulcanBaseDir;
end;

function VulcanPrgAssociated: Boolean;
begin
  result := Vs2015Installed and VulcanPrgAssociation;
end;

function HelpViewer22Found: Boolean;
begin
  result := HelpViewer22Installed;
end;

function HelpViewer23Found: Boolean;
begin
  result := HelpViewer23Installed;
end;


function Vs2015IsInstalled: Boolean;
begin
  result := Vs2015Installed;
end;

function VsNextIsInstalled: Boolean;
begin
  result := VsNextInstalled;
end;

function GetVs2015IdeDir(Param: String): String;
begin
  result := Vs2015Path;
end;

function GetVsNextIdeDir(Param: String): String;
begin
  result := VsNextPath;
end;


function OurHelp22IsInstalled: Boolean;
begin
  result := OurHelp22Installed;
end;

function OurHelp23IsInstalled: Boolean;
begin
  result := OurHelp23Installed;
end;

function GetHelp22Dir(Param: String): String;
begin
  result := HelpViewer22Dir;
end;

function GetHelp23Dir(Param: String): String;
begin
  result := HelpViewer23Dir;
end;

Procedure CurPageChanged(CurPage: Integer);

begin
  PrintButton.Visible := CurPage = wpLicense;
end;

procedure InitializeWizard();
begin
    PrintButton := TButton.Create(WizardForm);
    PrintButton.Caption := '&Print...';
    PrintButton.Left := WizardForm.InfoAfterPage.Left + 96;
    PrintButton.Top := WizardForm.InfoAfterPage.Height + 88;
    PrintButton.OnClick := @PrintButtonClick;
    PrintButton.Parent := WizardForm.NextButton.Parent;
end;

function InitializeSetup(): Boolean;
var
  ErrorCode: Integer;
begin
  DetectVS();
  result := true;
  if not Vs2015Installed and not VsNextInstalled then
  begin
    if MsgBox('Visual Studio 2015 has not been detected, do you want to download the free Visual Studio Community Edition ?', mbConfirmation, MB_YESNO) = IDYES then
    begin
    ShellExec('open','https://www.visualstudio.com/en-us/downloads/download-visual-studio-vs.aspx','','',SW_SHOW,ewWaitUntilIdle, ErrorCode);
    result := false;
    end
  end;
  
end;

procedure AdjustPkgDef();
var pkgdeffile: String;
var section: String;
begin
  if IsComponentSelected('vs2015\vulcanprg') then
  begin
        pkgdeffile := Vs2015Path+'\Extensions\XSharp\XSharpProject2015.pkgdef'
        section    := '$RootKey$\Languages\File Extensions\';
        DeleteIniSection(section+'.prg', pkgdeffile);
        DeleteIniSection(section+'.ppo', pkgdeffile);
        DeleteIniSection(section+'.vh',  pkgdeffile);
        { There are duplicate sections in the file. We want to delete the 1st 3 sections for the extensions prg, ppo and vh}
        section := '$RootKey$\Editors\{b4829761-2bfa-44b7-8f8f-d2625ebcf218}\Extensions';
        DeleteIniSection(section,pkgdeffile);
        DeleteIniSection(section,pkgdeffile);
        DeleteIniSection(section,pkgdeffile);
        //{ Another section is in there 5 times. We can remove 4 of them }
        //section := '$RootKey$\Editors\{b4829761-2bfa-44b7-8f8f-d2625ebcf218}';
        //DeleteIniSection(section,pkgdeffile);
        //DeleteIniSection(section,pkgdeffile);
        //DeleteIniSection(section,pkgdeffile);
        //DeleteIniSection(section,pkgdeffile);

  end;
end;

#expr SaveToFile(AddBackslash(SourcePath) + "Preprocessed.iss")