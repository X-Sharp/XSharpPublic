
;#define Compression     "lzma2/ultra64"
#define Compression     "none"

;
; preprocess the help cab files
;
#expr Exec('c:\xsharp\dev\xsharp\src\setup\makecabs.cmd')



#define FOX
#ifdef FOX
#define SetupExeName    "XSharpSetup0212Fox"
#else
#define SetupExeName    "XSharpSetup0212Public"
#endif


#define Product         "XSharp"
#define ProdVer         "XSharp 0.2.12.0"
#define ProdBuild       "XSharp Beta 12"
#define Company         "XSharp BV"
#define RegCompany      "XSharpBV"
#define XSharpURL       "http://www.xsharp.info"
#define CopyRight       "Copyright © 2015-2017 XSharp B.V."
#define VIVersion       "0.2.12.2112"
#define VITextVersion   "0.2.12.2112 (Beta 12)"                                                                                            
#define TouchDate       "2017-05-15"
#define TouchTime       "02:12:00"
#define InstallPath     "XSharpPath"


; Code Signing
#define KeyFile         "c:\XSharp\Dev\XSharp\build\Signatures\XSharpCert.pfx"
#define TimeStampURL    "http://timestamp.globalsign.com/scripts/timstamp.dll"
#define KeyPassword     "J1O39dGG6FPLXWj"
#define Description     "XSharp, xBase compiler for .Net"

;Folders
#define BinDFolder      "\Xsharp\Dev7\XSharp\Binaries\Debug_AnyCPU\"
#define BinRFolder      "\Xsharp\Dev7\XSharp\Binaries\Release_AnyCPU\"
#define BinPFolder      "\Xsharp\DevPublic\Binaries\Debug\"
#define CommonFolder    "\Xsharp\Dev\XSharp\src\Common\"
#define ToolsFolder     "\Xsharp\Dev\XSharp\src\Tools\"
#define ExamplesFolder  "\Xsharp\DevPublic\Samples\"
#define ScriptFolder    "\XSharp\Dev\XSharp\src\Scripting\"
#define OutPutFolder    "\XSharp\Dev\XSharp\Binaries\Setup"
#define DocFolder       "\Xsharp\Dev\XSharp\Binaries\Help\"
#define XIDEFolder      "\Xsharp\Dev\XSharp\Xide\"
#define XIDESetup       "XIDE_Set_up_1.09.exe"
#define XIDEVersion     "1.09"
#define StdFlags        "ignoreversion overwritereadonly sortfilesbyextension sortfilesbyname touch uninsremovereadonly"
#define GACInstall      "gacinstall sharedfile uninsnosharedfileprompt uninsrestartdelete"
#define ProviderVersion "XSharp.CodeDom.XSharpCodeDomProvider, Version=1.0.0.0, Culture=neutral, PublicKeyToken=31c59c566fa38f21"
#define VulcanEditorGuid "Editors\{{e6787d5e-718e-4810-9c26-7cc920baa335}\Extensions"
#define VS14RegPath      "Software\Microsoft\VisualStudio\14.0"
#define VS15RegPath      "Software\Microsoft\VisualStudio\15.0"
#define VS14LocalDir     "{localappdata}\Microsoft\VisualStudio\14.0"
#define VS15LocalDir     "{localappdata}\Microsoft\VisualStudio\15.0_"
#define SnippetsPath     "\Snippets\1033"
#define SnippetsSource   "\XSharp\DevPublic\VisualStudio\ProjectPackage\Snippets"

#define HelpInstall1  "/operation install /catalogname "
#define HelpInstall2  "/locale en-us /sourceuri """"{app}\help\XSharp.msha"""" /wait 0"
#define HelpUninstall1 "/silent /operation uninstall /catalogname"
#define HelpUninstall2 "/locale en-us /vendor """"XSharp"""" /productname """"X#"""" /booklist """"X# Documentation"""" /wait 0"

#define XSScript            "XSharpScript"

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{32EB192A-B120-4055-800E-74B48B80DA06}
DisableWelcomePage=no
DisableStartupPrompt=yes
DisableReadyMemo=yes
DisableFinishedPage=no
#ifdef FOX
InfoBeforeFile=Baggage\ReadmeShortFox.rtf
#else
InfoBeforeFile=Baggage\ReadmeShortPublic.rtf
#endif
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
UninstallDisplayIcon={app}\Images\XSharp.ico
UninstallLogMode=overwrite


TouchDate={#=TouchDate}
TouchTime={#=TouchTime}                               


; Make sure they are admins
PrivilegesRequired=admin
; Make sure they are running on Windows 2000 Or Higher
Minversion=6.0.600

; Code Signing
Signtool=mssigntool sign /f {# KeyFile} /p {# Keypassword}  /t {# TimeStampURL}  /d "{# Description}" $f        
SignToolRetryCount=5
; Tell windows that we associte the prgx extension
ChangesAssociations=yes


[Components]
Name: "main";             Description: "The XSharp Compiler and Build System";        Types: full compact custom; Flags: fixed; 
Name: "main\script";      Description: "Register .prgx as X# Script extension";       Types: full compact custom;  
Name: "main\ngen";        Description: "Optimize performance by generating native images";       Types: full compact custom;  
Name: "vs2015";           Description: "Visual Studio 2015 Integration";              Types: full custom;         Check: Vs2015IsInstalled;
Name: "vs2015\help";      Description: "Install VS documentation";                    Types: full custom;         Check: HelpViewer22Found;
Name: "vs2017";           Description: "Visual Studio 2017 Integration";              Types: full custom;         Check: vs2017IsInstalled;
Name: "vs2017\help";      Description: "Install VS documentation";                    Types: full custom;         Check: vs2017IsInstalled;
Name: "xide";             Description: "Include the XIDE {# XIDEVersion} installer";  Types: full custom;                  

;[Tasks]
;Name: desktopicon; Description: "Create a &desktop icon"; GroupDescription: "Additional icons:"; Components: main
;Name: desktopicon\common; Description: "For all users"; GroupDescription: "Additional icons:"; Components: main; Flags: exclusive
;Name: desktopicon\user; Description: "For the current user only"; GroupDescription: "Additional icons:"; Components: main; Flags: exclusive unchecked
;Name: quicklaunchicon; Description: "Create a &Quick Launch icon"; GroupDescription: "Additional icons:"; Components: main; Flags: unchecked
;Name: associate; Description: "&Associate files"; GroupDescription: "Other tasks:"; Flags: unchecked

[Dirs]
Name: "{app}\Assemblies";
Name: "{app}\Bin";
#ifdef FOX
Name: "{app}\Bin\Debug";
Name: "{app}\Bin\Release";
#endif
Name: "{app}\Help";
Name: "{app}\Images";
Name: "{app}\Include";
Name: "{app}\ProjectSystem";                                
Name: "{app}\Redist"
Name: "{app}\Snippets"
Name: "{app}\Tools";
Name: "{app}\Uninst";
Name: "{app}\Xide";
Components: vs2015; Name: "{code:GetVs2015IdeDir}\Extensions\XSharp";                              
Components: vs2015; Name: "{code:GetVs2015IdeDir}\Extensions\XSharp\{# SnippetsPath}";             
Components: vs2015; Name: "{userdocs}\Visual Studio 2015\Code Snippets\XSharp\My Code Snippets";   
Components: vs2017; Name: "{code:Getvs2017IdeDir}\Extensions\XSharp";                              
Components: vs2017; Name: "{code:GetVs2017IdeDir}\Extensions\XSharp\{# SnippetsPath}";             
Components: vs2017; Name: "{userdocs}\Visual Studio 2017\Code Snippets\XSharp\My Code Snippets";   
; user template folders
Components: vs2015; Name: "{userdocs}\Visual Studio 2015\Templates\ProjectTemplates\XSharp";   
Components: vs2015; Name: "{userdocs}\Visual Studio 2015\Templates\ItemTemplates\XSharp";   
Components: vs2017; Name: "{userdocs}\Visual Studio 2017\Templates\ProjectTemplates\XSharp";   
Components: vs2017; Name: "{userdocs}\Visual Studio 2017\Templates\ItemTemplates\XSharp";   


[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Files]
; Main program
#ifdef FOX
Source: "{#BinRFolder}xsc.exe";                            DestDir: "{app}\bin"; Flags: {#StdFlags} signonce ; 
Source: "{#BinRFolder}xsc.rsp";                            DestDir: "{app}\bin"; Flags: {#StdFlags}; 
Source: "{#BinRFolder}XSCompiler.exe";                     DestDir: "{app}\bin"; Flags: {#StdFlags} signonce ; BeforeInstall: TaskKill('xscompiler.exe');
Source: "{#BinRFolder}XSharp.CodeAnalysis.dll";            DestDir: "{app}\bin"; Flags: {#StdFlags} signonce ; 
Source: "{#BinRFolder}XSharp.CodeAnalysis.pdb";            DestDir: "{app}\bin"; Flags: {#StdFlags}; 
Source: "{#BinRFolder}xsc.pdb";                            DestDir: "{app}\bin"; Flags: {#StdFlags}; 
Source: "{#BinRFolder}XSCompiler.pdb";                     DestDir: "{app}\bin"; Flags: {#StdFlags}; 
Source: "{#BinRFolder}xsc.exe.config";                     DestDir: "{app}\bin"; Flags: {#StdFlags}; 
Source: "{#BinRFolder}XSCompiler.exe.config ";             DestDir: "{app}\bin"; Flags: {#StdFlags}; 
Source: "{#BinRFolder}xsi.exe";                            DestDir: "{app}\bin"; Flags: {#StdFlags} signonce ; 
Source: "{#BinRFolder}xsi.pdb";                            DestDir: "{app}\bin"; Flags: {#StdFlags}; 
Source: "{#BinRFolder}xsi.exe.config";                     DestDir: "{app}\bin"; Flags: {#StdFlags}; 
Source: "{#BinRFolder}XSharp.Scripting.dll";               DestDir: "{app}\bin"; Flags: {#StdFlags} signonce ; 
Source: "{#BinRFolder}XSharp.Scripting.pdb";               DestDir: "{app}\bin"; Flags: {#StdFlags}; 

Source: "Baggage\DebugRelease.txt";                        DestDir: "{app}\bin"; Flags: {#StdFlags}; 
; Release Folder
Source: "{#BinRFolder}xsc.exe";          DestDir: "{app}\bin\Release"; Flags: {#StdFlags} signonce ; 
Source: "{#BinRFolder}XSCompiler.exe";                     DestDir: "{app}\bin\Release"; Flags: {#StdFlags} signonce; 
Source: "{#BinRFolder}XSharp.CodeAnalysis.dll";            DestDir: "{app}\bin\Release"; Flags: {#StdFlags} signonce; 
Source: "{#BinRFolder}XSharp.CodeAnalysis.pdb";            DestDir: "{app}\bin\Release"; Flags: {#StdFlags}; 
Source: "{#BinRFolder}xsc.pdb";                            DestDir: "{app}\bin\Release"; Flags: {#StdFlags}; 
Source: "{#BinRFolder}XSCompiler.pdb";                     DestDir: "{app}\bin\Release"; Flags: {#StdFlags}; 
Source: "{#BinRFolder}xsc.exe.config";                     DestDir: "{app}\bin\Release"; Flags: {#StdFlags};  
Source: "{#BinRFolder}XSCompiler.exe.config";              DestDir: "{app}\bin\Release"; Flags: {#StdFlags}; 
Source: "{#BinRFolder}xsi.exe";                            DestDir: "{app}\bin\Release"; Flags: {#StdFlags} signonce ; 
Source: "{#BinRFolder}xsi.pdb";                            DestDir: "{app}\bin\Release"; Flags: {#StdFlags}; 
Source: "{#BinRFolder}xsi.exe.config";                     DestDir: "{app}\bin\Release"; Flags: {#StdFlags}; 
Source: "{#BinRFolder}XSharp.Scripting.dll";               DestDir: "{app}\bin\Release"; Flags: {#StdFlags} signonce ; 
Source: "{#BinRFolder}XSharp.Scripting.pdb";               DestDir: "{app}\bin\Release"; Flags: {#StdFlags}; 

; Debug Folder
Source: "{#BinDFolder}xsc.exe";                            DestDir: "{app}\bin\Debug"; Flags: {#StdFlags} signonce; 
Source: "{#BinDFolder}XSCompiler.exe";                     DestDir: "{app}\bin\Debug"; Flags: {#StdFlags} signonce; 
Source: "{#BinDFolder}XSharp.CodeAnalysis.dll";            DestDir: "{app}\bin\Debug"; Flags: {#StdFlags} signonce; 
Source: "{#BinDFolder}XSharp.CodeAnalysis.pdb";            DestDir: "{app}\bin\Debug"; Flags: {#StdFlags}; 
Source: "{#BinDFolder}xsc.pdb";                            DestDir: "{app}\bin\Debug"; Flags: {#StdFlags}; 
Source: "{#BinDFolder}XSCompiler.pdb";                     DestDir: "{app}\bin\Debug"; Flags: {#StdFlags}; 
Source: "{#BinDFolder}xsc.exe.config";                     DestDir: "{app}\bin\Debug"; Flags: {#StdFlags}; 
Source: "{#BinDFolder}XSCompiler.exe.config ";             DestDir: "{app}\bin\Debug"; Flags: {#StdFlags}; 
Source: "{#BinDFolder}xsi.exe";                            DestDir: "{app}\bin\Debug"; Flags: {#StdFlags} signonce ; 
Source: "{#BinDFolder}xsi.pdb";                            DestDir: "{app}\bin\Debug"; Flags: {#StdFlags}; 
Source: "{#BinDFolder}xsi.exe.config";                     DestDir: "{app}\bin\Debug"; Flags: {#StdFlags}; 
Source: "{#BinDFolder}XSharp.Scripting.dll";               DestDir: "{app}\bin\Debug"; Flags: {#StdFlags} signonce ; 
Source: "{#BinDFolder}XSharp.Scripting.pdb";               DestDir: "{app}\bin\Debug"; Flags: {#StdFlags}; 

#else
Source: "{#BinDFolder}xsc.exe";                            DestDir: "{app}\bin"; Flags: {#StdFlags} signonce; 
Source: "{#BinDFolder}xsc.rsp";                            DestDir: "{app}\bin"; Flags: {#StdFlags}; 
Source: "{#BinDFolder}XSCompiler.exe";                     DestDir: "{app}\bin"; Flags: {#StdFlags} signonce; 
Source: "{#BinDFolder}XSharp.CodeAnalysis.dll";            DestDir: "{app}\bin"; Flags: {#StdFlags} signonce; 
Source: "{#BinDFolder}XSharp.CodeAnalysis.pdb";            DestDir: "{app}\bin"; Flags: {#StdFlags}; 
Source: "{#BinDFolder}xsc.pdb";                            DestDir: "{app}\bin"; Flags: {#StdFlags}; 
Source: "{#BinDFolder}XSCompiler.pdb";                     DestDir: "{app}\bin"; Flags: {#StdFlags}; 
Source: "{#BinDFolder}xsc.exe.config";                     DestDir: "{app}\bin"; Flags: {#StdFlags}  
Source: "{#BinDFolder}XSCompiler.exe.config ";             DestDir: "{app}\bin"; Flags: {#StdFlags}; 
Source: "{#BinDFolder}xsi.exe";                            DestDir: "{app}\bin"; Flags: {#StdFlags} signonce ; 
Source: "{#BinDFolder}xsi.pdb";                            DestDir: "{app}\bin"; Flags: {#StdFlags}; 
Source: "{#BinDFolder}xsi.exe.config";                     DestDir: "{app}\bin"; Flags: {#StdFlags}  
Source: "{#BinDFolder}XSharp.Scripting.dll";               DestDir: "{app}\bin"; Flags: {#StdFlags} signonce ; 
Source: "{#BinDFolder}XSharp.Scripting.pdb";               DestDir: "{app}\bin"; Flags: {#StdFlags}; 

#endif

; Ms Runtime DLLs
Source: "{#BinDFolder}System*.Dll";                        DestDir: "{app}\bin"; Flags: {#StdFlags} ; 
Source: "{#BinDFolder}Microsoft*.Dll";                     DestDir: "{app}\bin"; Flags: {#StdFlags} ; 

Source: "{#BinPFolder}baggage\rc.exe";                    DestDir: "{app}\bin"; Flags: {#StdFlags}; 
Source: "{#BinPFolder}baggage\rcdll.dll";                 DestDir: "{app}\bin"; Flags: {#StdFlags}; 
Source: "{#BinPFolder}xporter.exe";                       DestDir: "{app}\bin"; Flags: {#StdFlags} signonce; 
Source: "{#BinPFolder}xporter.pdb";                       DestDir: "{app}\bin"; Flags: {#StdFlags}; 


; Support files
#ifdef FOX
Source: "Baggage\ReadmeFox.rtf";                          DestDir: "{app}";   DestName:"Readme.rtf" ; Flags: isreadme {#StdFlags}; 
#else
Source: "Baggage\ReadmePublic.rtf";                       DestDir: "{app}";   DestName:"Readme.rtf" ; Flags: isreadme {#StdFlags}; 
#endif
Source: "Baggage\Whatsnew.rtf";                           DestDir: "{app}";   DestName:"Whatsnew.rtf" ; Flags: isreadme {#StdFlags}; 

Source: "Baggage\Redist.txt";                             DestDir: "{app}\Redist" ; Flags: {#StdFlags}; 
Source: "Baggage\XSharp.ico";                             DestDir: "{app}\Images"; Flags: touch {#StdFlags}; 
Source: "Baggage\License.txt";                            DestDir: "{app}";        Flags: touch {#StdFlags}; 

; Include Files
Source: "{#CommonFolder}*.xh";                            DestDir: "{app}\Include"; Flags: touch {#StdFlags}; 

;MsBuild Files
Components: vs2015; Source: "{#BinPFolder}Xaml\*.*";                          DestDir: "{pf}\MsBuild\{#Product}\Rules";  Flags: {#StdFlags}; 
Components: vs2015; Source: "{#BinPFolder}Targets\*.*";                       DestDir: "{pf}\MsBuild\{#Product}";        Flags: {#StdFlags}; 
Components: vs2015; Source: "{#BinPFolder}XSharp.Build.dll";                  DestDir: "{pf}\MsBuild\{#Product}";        Flags: {#StdFlags}; 
Components: vs2015; Source: "{#BinPFolder}XSharp.Build.pdb";                  DestDir: "{pf}\MsBuild\{#Product}";        Flags: {#StdFlags}; 

; MsBuild files for VS2017 in a private directory per installation
Components: vs2017; Source: "{#BinPFolder}Xaml\*.*";                          DestDir: "{code:Getvs2017BaseDir}\MsBuild\{#Product}\Rules";  Flags: {#StdFlags}; 
Components: vs2017; Source: "{#BinPFolder}Targets\*.*";                       DestDir: "{code:Getvs2017BaseDir}\MsBuild\{#Product}";        Flags: {#StdFlags}; 
Components: vs2017; Source: "{#BinPFolder}XSharp.Build.dll";                  DestDir: "{code:Getvs2017BaseDir}\MsBuild\{#Product}";        Flags: {#StdFlags}; 
Components: vs2017; Source: "{#BinPFolder}XSharp.Build.pdb";                  DestDir: "{code:Getvs2017BaseDir}\MsBuild\{#Product}";        Flags: {#StdFlags}; 

;Documentation
Source: "{#DocFolder}XSharp.pdf";                        DestDir: "{app}\Help";        Flags: touch {#StdFlags}; 
Source: "{#DocFolder}XSharp.chm";                        DestDir: "{app}\Help";        Flags: touch {#StdFlags}; 
Source: "{#DocFolder}XSVulcan.chm";                      DestDir: "{app}\Help";        Flags: touch {#StdFlags}; 
Source: "{#DocFolder}XSharp.msha";                       DestDir: "{app}\Help";        Flags: touch {#StdFlags}; 
Source: "{#DocFolder}XSharp.cab";                        DestDir: "{app}\Help";        Flags: touch {#StdFlags} signonce; 
Source: "{#DocFolder}XSVulcan.cab";                      DestDir: "{app}\Help";        Flags: touch {#StdFlags} signonce; 

;XIDE
Components: Xide; Source: "{#XIDEFolder}{#XIDESetup}";                      DestDir: "{app}\Xide";        Flags: touch {#StdFlags}; 

;VsProjectSystem
; Note that CodeAnalysis.DLL and CodeDomProvider must both go to the PrivateAssemblies folder.
; We register CodeDomProvider also in the GAC because some processes read machine.config and that file does
; not have location information

Components: vs2015; Source: "{#BinPFolder}XSharpProject.vsix";            DestDir: "{app}\ProjectSystem"; Flags: {#StdFlags}
; CodeAnalysis is used by project system and CodeDom provider
#ifdef FOX
Components: vs2015; Source: "{#BinRFolder}XSharp.CodeAnalysis.dll";           DestDir: "{code:GetVs2015IdeDir}\PrivateAssemblies"; Flags: {#StdFlags}; BeforeInstall: DeleteOldFiles(2015);
Components: vs2015; Source: "{#BinRFolder}XSharp.CodeAnalysis.pdb";           DestDir: "{code:GetVs2015IdeDir}\PrivateAssemblies"; Flags: {#StdFlags}; 
#else
Components: vs2015; Source: "{#BinDFolder}XSharp.CodeAnalysis.dll";           DestDir: "{code:GetVs2015IdeDir}\PrivateAssemblies"; Flags: {#StdFlags}; BeforeInstall: DeleteOldFiles(2015);
Components: vs2015; Source: "{#BinDFolder}XSharp.CodeAnalysis.pdb";           DestDir: "{code:GetVs2015IdeDir}\PrivateAssemblies"; Flags: {#StdFlags}; 
#endif

Components: vs2015; Source: "{#BinPFolder}XSharpCodeDomProvider.dll";         DestDir: "{code:GetVs2015IdeDir}\PrivateAssemblies"; Flags: {#StdFlags} {#GACInstall}; StrongAssemblyName: "{#ProviderVersion}"; 
Components: vs2015; Source: "{#BinPFolder}XSharpCodeDomProvider.pdb";         DestDir: "{code:GetVs2015IdeDir}\PrivateAssemblies"; Flags: {#StdFlags}; 
Components: vs2015; Source: "{#BinPFolder}XSharpColorizer.dll";               DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: {#StdFlags}; 
Components: vs2015; Source: "{#BinPFolder}XSharpColorizer.pdb";               DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: {#StdFlags}; 
Components: vs2015; Source: "{#BinPFolder}XSharpModel.dll";                   DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: {#StdFlags}; 
Components: vs2015; Source: "{#BinPFolder}XSharpModel.pdb";                   DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: {#StdFlags}; 
; ItemTemplates per folder
Components: vs2015; Source: "{#BinPFolder}Itemtemplates\Wpf*.Zip";            DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\ItemTemplates\WPF";        Flags: recursesubdirs {#StdFlags}; 
;Source: "{#BinPFolder}Itemtemplates\VO*.Zip";             DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\ItemTemplates\VO";         Flags: recursesubdirs {#StdFlags}; Components: vs2015
Components: vs2015; Source: "{#BinPFolder}Itemtemplates\*Internal.Zip";       DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\ItemTemplates\Internal";   Flags: recursesubdirs {#StdFlags}; 
Components: vs2015; Source: "{#BinPFolder}Itemtemplates\Wcf*.Zip";            DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\ItemTemplates\WCF";        Flags: recursesubdirs {#StdFlags}; 
Components: vs2015; Source: "{#BinPFolder}Itemtemplates\Form*.Zip";           DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\ItemTemplates\Windows Forms"; Flags: recursesubdirs {#StdFlags}; 
Components: vs2015; Source: "{#BinPFolder}Itemtemplates\U*.Zip";              DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\ItemTemplates\Windows Forms"; Flags: recursesubdirs {#StdFlags}; 
Components: vs2015; Source: "{#BinPFolder}Itemtemplates\C*.Zip";              DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\ItemTemplates\Code";          Flags: recursesubdirs {#StdFlags}; 
Components: vs2015; Source: "{#BinPFolder}Itemtemplates\H*.Zip";              DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\ItemTemplates\Code";          Flags: recursesubdirs {#StdFlags}; 
Components: vs2015; Source: "{#BinPFolder}Itemtemplates\T*.Zip";              DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\ItemTemplates\Code";          Flags: recursesubdirs {#StdFlags}; 
Components: vs2015; Source: "{#BinPFolder}Itemtemplates\R*.Zip";              DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\ItemTemplates\Resources";     Flags: recursesubdirs {#StdFlags}; 

Components: vs2015; Source: "{#BinPFolder}ProjectTemplates\*.*";              DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\ProjectTemplates";  Flags: recursesubdirs {#StdFlags}; 
; Snippets
Components: vs2015; Source: "{#SnippetsSource}\*.*";                          DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp\{# SnippetsPath}";  Flags: recursesubdirs {#StdFlags}; 


Components: vs2015; Source: "{#BinPFolder}XSharpProject.dll";                 DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: {#StdFlags}; 
Components: vs2015; Source: "{#BinPFolder}XSharpProject.dll.config";          DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: {#StdFlags}; 
Components: vs2015; Source: "{#BinPFolder}XSharpProject.pdb";                 DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: {#StdFlags}; 
Components: vs2015; Source: "{#BinPFolder}XSharpProject.pkgdef";              DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: {#StdFlags}; 
Components: vs2015; Source: "{#BinPFolder}extension.vsixmanifest";            DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; DestName: "extension.vsixmanifest"; Flags: {#StdFlags}; 
Components: vs2015; Source: "{#BinPFolder}Designers.pkgdef";                  DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp"; Flags: {#StdFlags}; 

Components: vs2015; Source: "{#BinPFolder}XSharp.ico ";                       DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp";  Flags: {#StdFlags}; 
Components: vs2015; Source: "{#BinPFolder}XSharpVSIXLogo.png ";               DestDir: "{code:GetVs2015IdeDir}\Extensions\XSharp";  Flags: {#StdFlags}; 



; vs2017
;VsProjectSystem
; Note that CodeAnalysis.DLL and CodeDomProvider must both go to the PrivateAssemblies folder.
; We register CodeDomProvider also in the GAC because some processes read machine.config and that file does
; not have location information

Components: vs2017; Source: "{#BinPFolder}SetupCheck2017.exe";          DestDir: "{tmp}";      Flags: signonce {#StdFlags};
Components: vs2017; Source: "{#BinPFolder}XSharpProject.vsix";          DestDir: "{app}\ProjectSystem"; Flags: {#StdFlags}

; CodeAnalysis is used by project system and CodeDom provider
#ifdef FOX
Components: vs2017; Source: "{#BinRFolder}XSharp.CodeAnalysis.dll";           DestDir: "{code:GetVs2017IdeDir}\PrivateAssemblies"; Flags: {#StdFlags}; BeforeInstall: DeleteOldFiles(2017);
Components: vs2017; Source: "{#BinRFolder}XSharp.CodeAnalysis.pdb";           DestDir: "{code:GetVs2017IdeDir}\PrivateAssemblies"; Flags: {#StdFlags}; 
#else
Components: vs2017; Source: "{#BinDFolder}XSharp.CodeAnalysis.dll";           DestDir: "{code:GetVs2017IdeDir}\PrivateAssemblies"; Flags: {#StdFlags}; BeforeInstall: DeleteOldFiles(2017);
Components: vs2017; Source: "{#BinDFolder}XSharp.CodeAnalysis.pdb";           DestDir: "{code:GetVs2017IdeDir}\PrivateAssemblies"; Flags: {#StdFlags}; 
#endif

Components: vs2017; Source: "{#BinPFolder}XSharpCodeDomProvider.dll";         DestDir: "{code:Getvs2017IdeDir}\PrivateAssemblies"; Flags: {#StdFlags} {#GACInstall}; StrongAssemblyName: "{#ProviderVersion}"; 
Components: vs2017; Source: "{#BinPFolder}XSharpCodeDomProvider.pdb";         DestDir: "{code:Getvs2017IdeDir}\PrivateAssemblies"; Flags: {#StdFlags}; 
Components: vs2017; Source: "{#BinPFolder}XSharpColorizer.dll";               DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp"; Flags: {#StdFlags}; 
Components: vs2017; Source: "{#BinPFolder}XSharpColorizer.pdb";               DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp"; Flags: {#StdFlags}; 
Components: vs2017; Source: "{#BinPFolder}XSharpModel.dll";                   DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp"; Flags: {#StdFlags}; 
Components: vs2017; Source: "{#BinPFolder}XSharpModel.pdb";                   DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp"; Flags: {#StdFlags}; 

; ItemTemplates per folder
Components: vs2017; Source: "{#BinPFolder}Itemtemplates\Wpf*.Zip";            DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp\ItemTemplates\WPF";        Flags: recursesubdirs {#StdFlags}; 
;Components: vs2017; Source: "{#BinPFolder}Itemtemplates\VO*.Zip";            DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp\ItemTemplates\VO";         Flags: recursesubdirs {#StdFlags}; 
Components: vs2017; Source: "{#BinPFolder}Itemtemplates\*Internal.Zip";       DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp\ItemTemplates\Internal";   Flags: recursesubdirs {#StdFlags}; 
Components: vs2017; Source: "{#BinPFolder}Itemtemplates\Wcf*.Zip";            DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp\ItemTemplates\WCF";        Flags: recursesubdirs {#StdFlags}; 
Components: vs2017; Source: "{#BinPFolder}Itemtemplates\Form*.Zip";           DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp\ItemTemplates\Windows Forms"; Flags: recursesubdirs {#StdFlags}; 
Components: vs2017; Source: "{#BinPFolder}Itemtemplates\U*.Zip";              DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp\ItemTemplates\Windows Forms"; Flags: recursesubdirs {#StdFlags}; 
Components: vs2017; Source: "{#BinPFolder}Itemtemplates\C*.Zip";              DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp\ItemTemplates\Code";          Flags: recursesubdirs {#StdFlags}; 
Components: vs2017; Source: "{#BinPFolder}Itemtemplates\H*.Zip";              DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp\ItemTemplates\Code";          Flags: recursesubdirs {#StdFlags}; 
Components: vs2017; Source: "{#BinPFolder}Itemtemplates\T*.Zip";              DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp\ItemTemplates\Code";          Flags: recursesubdirs {#StdFlags}; 
Components: vs2017; Source: "{#BinPFolder}Itemtemplates\R*.Zip";              DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp\ItemTemplates\Resources";     Flags: recursesubdirs {#StdFlags}; 

Components: vs2017; Source: "{#BinPFolder}ProjectTemplates\*.*";              DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp\ProjectTemplates";  Flags: recursesubdirs {#StdFlags}; 

; Snippets
Components: vs2017; Source: "{#SnippetsSource}\*.*";                          DestDir: "{code:GetVs2017IdeDir}\Extensions\XSharp\{# SnippetsPath}";  Flags: recursesubdirs {#StdFlags}; 

Components: vs2017; Source: "{#BinPFolder}XSharpProject.dll";                 DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp";  Flags: {#StdFlags}; 
Components: vs2017; Source: "{#BinPFolder}XSharpProject.dll.config";          DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp";  Flags: {#StdFlags}; 
Components: vs2017; Source: "{#BinPFolder}XSharpProject.pdb";                 DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp";  Flags: {#StdFlags}; 
Components: vs2017; Source: "{#BinPFolder}XSharpProject.pkgdef";              DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp";  Flags: {#StdFlags}; 
Components: vs2017; Source: "{#BinPFolder}extension.vsixmanifest";            DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp";  DestName: "extension.vsixmanifest"; Flags: {#StdFlags}; 
Components: vs2017; Source: "{#BinPFolder}Designers.pkgdef";                  DestDir: "{code:GetVs2017IdeDir}\Extensions\XSharp"; Flags: {#StdFlags}; 

Components: vs2017; Source: "{#BinPFolder}XSharp.ico ";                             DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp";        Flags: {#StdFlags}; 
Components: vs2017; Source: "{#BinPFolder}XSharpVSIXLogo.png ";                DestDir: "{code:Getvs2017IdeDir}\Extensions\XSharp";        Flags: {#StdFlags}; 

; Examples
Source: "{#ExamplesFolder}*.prg";                             DestDir: "{commondocs}\XSharp\Examples";    Flags: recursesubdirs {#StdFlags};
Source: "{#ExamplesFolder}*.txt";                             DestDir: "{commondocs}\XSharp\Examples";    Flags: recursesubdirs {#StdFlags};
Source: "{#ExamplesFolder}*.vh";                              DestDir: "{commondocs}\XSharp\Examples";    Flags: recursesubdirs {#StdFlags};
Source: "{#ExamplesFolder}*.sln";                             DestDir: "{commondocs}\XSharp\Examples";    Flags: recursesubdirs {#StdFlags};
Source: "{#ExamplesFolder}*.dbf";                             DestDir: "{commondocs}\XSharp\Examples";    Flags: recursesubdirs {#StdFlags};
Source: "{#ExamplesFolder}*.ntx";                             DestDir: "{commondocs}\XSharp\Examples";    Flags: recursesubdirs {#StdFlags};
Source: "{#ExamplesFolder}*.xsproj";                          DestDir: "{commondocs}\XSharp\Examples";    Flags: recursesubdirs {#StdFlags};

; Scripting
Source: "{#ScriptFolder}*.*";                                 DestDir: "{commondocs}\XSharp\Scripting";    Flags: recursesubdirs {#StdFlags};

; update machine.config
Source:"{#ToolsFolder}Various\RegisterProvider.exe";          DestDir: "{app}\Tools";                     Flags: signonce {#StdFlags};

[Icons]
Name: "{group}\{cm:ProgramOnTheWeb,{#Product}}"; Filename: "{#XSharpURL}";IconFilename:{app}\Images\XSharp.ico;
Name: "{group}\{cm:UninstallProgram,{#Product}}"; Filename: "{uninstallexe}";  Parameters: "/Log";
Name: "{group}\{#Product} XPorter";  Filename: "{app}\bin\xporter.exe";
Name: "{group}\{#Product} Readme";  Filename: "{app}\Readme.rtf";
Name: "{group}\{#Product} What's New";  Filename: "{app}\Whatsnew.rtf";
Name: "{group}\{#Product} Documenation (CHM)"; Filename: "{app}\Help\XSharp.chm"; 
Name: "{group}\{#Product} Documenation (PDF)"; Filename: "{app}\Help\XSharp.pdf"; 
Name: "{group}\{#Product} Vulcan Runtime Reference (CHM)"; Filename: "{app}\Help\XSVulcan.chm"; 
Name: "{group}\{cm:UninstallProgram,{#Product}}"; Filename: "{uninstallexe}"; 
Name: "{group}\{#Product} Examples"; Filename: "{commondocs}\XSharp\Examples";
Name: "{commondesktop}\XSharp Examples";  Filename: "{commondocs}\XSharp\Examples";
Name: "{commondesktop}\XSharp Script Examples";  Filename: "{commondocs}\XSharp\Scripting";


[Registry]
Root: HKLM; Subkey: "Software\{#RegCompany}"; Flags: uninsdeletekeyifempty 
Root: HKLM; Subkey: "Software\{#RegCompany}\{#Product}"; Flags: uninsdeletekey 
Root: HKLM; Subkey: "Software\{#RegCompany}\{#Product}"; ValueName: "{#InstallPath}"; ValueType: string; ValueData: "{app}" ;
Components: vs2015\help; Root: HKLM; Subkey: "Software\{#RegCompany}\{#Product}"; ValueName: "Help22Installed"; ValueType: string; ValueData: "yes" ;  
Components: vs2017\help; Root: HKLM; Subkey: "Software\{#RegCompany}\{#Product}"; ValueName: "Help23Installed"; ValueType: string; ValueData: "yes" ;  

; set the VSHelp to Offline
Components: vs2015\help;  Root: HKCU; Subkey: "{#Vs14RegPath}\Help"; ValueName:"UseOnlineHelp"; ValueType: dword; ValueData: 0; Flags: noerror;
; Cannot set VS 2015 Online help off. It is in a private registry
;Components: vs2017\help; Root: HKCU; Subkey: "{#Vs15RegPath}\Help"; ValueName:"UseOnlineHelp"; ValueType: dword; ValueData: 0; Flags: noerror;

; When Vulcan is Installed then update its extension registration so we can handle the priorities in our project system

Components: vs2015; Root: HKLM; Subkey: "{#Vs14RegPath}\{#VulcanEditorGuid}"; ValueName: "ppo"; ValueData: 1; Check: VulcanPrgAssociated;
Components: vs2015; Root: HKLM; Subkey: "{#Vs14RegPath}\{#VulcanEditorGuid}"; ValueName: "prg"; ValueData: 1; Check: VulcanPrgAssociated;
Components: vs2015; Root: HKLM; Subkey: "{#Vs14RegPath}\{#VulcanEditorGuid}"; ValueName: "vh";  ValueData: 1; Check: VulcanPrgAssociated;

Components: vs2015; Root: HKCU; Subkey: "{#Vs14RegPath}_Config\{#VulcanEditorGuid}"; ValueName: "ppo"; ValueData: 1; Check: VulcanPrgAssociated;
Components: vs2015; Root: HKCU; Subkey: "{#Vs14RegPath}_Config\{#VulcanEditorGuid}"; ValueName: "prg"; ValueData: 1; Check: VulcanPrgAssociated;
Components: vs2015; Root: HKCU; Subkey: "{#Vs14RegPath}_Config\{#VulcanEditorGuid}"; ValueName: "vh";  ValueData: 1; Check: VulcanPrgAssociated;


; associate prgx extension

Components: main\script; Root: HKCR; Subkey: ".prgx";                            ValueData: "{#XSScript}";                Flags: uninsdeletevalue; ValueType: string;  ValueName: ""
Components: main\script; Root: HKCR; Subkey: "{#XSScript}";                      ValueData: "Program {#XSScript}";        Flags: uninsdeletekey;   ValueType: string;  ValueName: ""
Components: main\script; Root: HKCR; Subkey: "{#XSScript}\DefaultIcon";          ValueData: "{app}\Images\xsharp.ico,0";  ValueType: string;  ValueName: ""
Components: main\script; Root: HKCR; Subkey: "{#XSScript}\shell\open\command";   ValueData: """{app}\bin\xsi.exe"" ""%1"" %*";  ValueType: string;  ValueName: ""


[Ini]
Components: vs2015; Filename: "{code:GetVs2015IdeDir}\Extensions\extensions.configurationchanged"; Section:"XSharp"; Key: "Installed"; String: "{#VIVersion}"; Flags: uninsdeletesection; 
Components: vs2017; Filename: "{code:Getvs2017IdeDir}\Extensions\extensions.configurationchanged"; Section:"XSharp"; Key: "Installed"; String: "{#VIVersion}"; Flags: uninsdeletesection; 

[Run]
Filename: "{app}\Tools\RegisterProvider.exe"; Flags: runhidden;

; Remove old Help contents
Components: vs2015\help; Filename: "{code:GetHelp22Dir}\HlpCtntMgr.exe"; Parameters: "{#HelpUninstall1} VisualStudio14 {#HelpUninstall2}";   StatusMsg:"UnInstalling VS Help for VS2015"; Flags: waituntilidle;
Components: vs2017\help; Filename: "{code:GetHelp23Dir}\HlpCtntMgr.exe"; Parameters: "{#HelpUninstall1} VisualStudio15 {#HelpUninstall2}";   StatusMsg:"UnInstalling VS Help for VS2017"; Flags: waituntilidle;

Components: vs2015\help; Filename: "{code:GetHelp22Dir}\HlpCtntMgr.exe"; Parameters: "{#HelpInstall1} VisualStudio14 {#HelpInstall2}";     StatusMsg:"Installing VS Help for VS2015"; Flags: waituntilidle;
Components: vs2017\help; Filename: "{code:GetHelp23Dir}\HlpCtntMgr.exe"; Parameters: "{#HelpInstall1} VisualStudio15 {#HelpInstall2}";     StatusMsg:"Installing VS Help for VS2017";  Flags: waituntilidle;
Components: XIDE;        Filename:  "{app}\Xide\{#XIDESetup}"; Description:"Run XIDE {# XIDEVersion} Installer"; Flags: postInstall;  

Components: main\ngen;  Filename: "{app}\uninst\instngen.cmd"; Check: CreateNGenTask(); Flags: Runhidden ; StatusMsg: "Generating Native images, please wait.....";

[UninstallRun]
Components: vs2015\help; Filename: "{code:GetHelp22Dir}\HlpCtntMgr.exe"; Parameters: "{#HelpUninstall1} VisualStudio14 {#HelpUninstall2}";   StatusMsg:"UnInstalling VS Help for VS2015"; Flags: waituntilidle;
Components: vs2017\help; Filename: "{code:GetHelp23Dir}\HlpCtntMgr.exe"; Parameters: "{#HelpUninstall1} VisualStudio15 {#HelpUninstall2}";   StatusMsg:"UnInstalling VS Help for VS2017"; Flags: waituntilidle;

Components: main\ngen;  Filename: "{app}\uninst\uninstngen.cmd";  Flags: Runhidden;







[InstallDelete]
; The old License.rtf file.
; Template cache, component cache and previous installation of our project system
; vs2015
; Also the CodeDom provider from the GAC

Type: filesandordirs; Name: "{win}\Microsoft.NET\assembly\GAC_MSIL\XSharpCodeDomProvider";

Type: files;          Name: "{app}\License.rtf"; 
Type: filesandordirs; Name: "{app}\Xide"; 
Components: vs2015; Type: filesandordirs; Name: "{#Vs14LocalDir}\vtc";                            
Components: vs2015; Type: filesandordirs; Name: "{#Vs14LocalDir}\ComponentModelCache";            
Components: vs2015; Type: filesandordirs; Name: "{code:GetVs2015IdeDir}\Extensions\XSharp";       

; vs2017
Components: vs2017; Type: filesandordirs; Name: "{#Vs15LocalDir}{code:GetVs2017InstanceId}\vtc";                            
Components: vs2017; Type: filesandordirs; Name: "{#Vs15LocalDir}{code:GetVs2017InstanceId}\ComponentModelCache";            
Components: vs2017; Type: filesandordirs; Name: "{code:Getvs2017IdeDir}\Extensions\XSharp";       

; remove the old uninstaller because the uninstall file format has changed in one of the previous builds
Type: filesandordirs; Name: "{app}\Uninst"

[UninstallDelete]
Type: filesandordirs; Name: "{app}\Assemblies"                    ; 
#ifdef FOX
Type: filesandordirs; Name: "{app}\Bin\Debug"                     ; 
Type: filesandordirs; Name: "{app}\Bin\Release"                   ; 
#endif
Type: filesandordirs; Name: "{app}\Bin"                           ; 
Type: filesandordirs; Name: "{app}\Help"                          ; 
Type: filesandordirs; Name: "{app}\Images"                        ; 
Type: filesandordirs; Name: "{app}\ProjectSystem"                 ; 
Type: filesandordirs; Name: "{app}\Redist"                        ; 
Type: filesandordirs; Name: "{app}\Tools"                         ; 
Type: filesandordirs; Name: "{app}\Uninst"                        ; 
Type: filesandordirs; Name: "{pf}\MsBuild\{#Product}"             ; 
Components: vs2015; Type: filesandordirs; Name: "{code:GetVs2015IdeDir}\Extensions\XSharp";  
Components: vs2017; Type: filesandordirs; Name: "{code:Getvs2017IdeDir}\Extensions\XSharp";  
Type: filesandordirs; Name: "{commondocs}\XSharp\Examples";
Type: dirifempty;     Name: "{app}\Include"; 
Type: dirifempty;     Name: "{app}"; 
Type: filesandordirs; Name: "{app}\Xide"; 
Type: dirifempty;     Name: "{app}\Snippets"                      ; 
Type: dirifempty;     Name: "{commondocs}\XSharp"; 

; Template cache and component cache
;vs2015
Components: vs2015; Type: filesandordirs; Name: "{#Vs14LocalDir}\vtc";                            
Components: vs2015; Type: filesandordirs; Name: "{#Vs14LocalDir}\ComponentModelCache";            
;vs2017
Components: vs2017; Type: filesandordirs; Name: "{#Vs15LocalDir}{code:GetVs2017InstanceId}\vtc";                            
Components: vs2017; Type: filesandordirs; Name: "{#Vs15LocalDir}{code:GetVs2017InstanceId}\ComponentModelCache";            

[Messages]
WelcomeLabel1=Welcome to {# Product} (X#)
WelcomeLabel2=This installer will install {#ProdBuild} on your computer.%n%nIt is recommended that you close all other applications before continuing, especially all running copies of Visual Studio.
WizardInfoBefore=Warning
InfoBeforeLabel=You are about to install Beta software
InfoBeforeClickLabel=Only continue the installation if you are aware of the following:


[Code]
Program setup;
var

  vs2017VersionPage: TInputOptionWizardPage;
  PrintButton: TButton;
  Vs2015Path : String;
  Vs2015Installed: Boolean;
  Vs2015BaseDir: String;
  VulcanInstalled: Boolean;
  VulcanBaseDir: String;
  CancelSetup : Boolean;
  
  vs2017Path : String;
  vs2017InstanceId : String;
  vs2017Version: String;
  vs2017Installed: Boolean;
  vs2017BaseDir: String;

  vs2017Count: Integer;
  vs2017InstanceId1: String;
  vs2017InstanceId2: String;
  vs2017InstanceId3: String;

  vs2017BaseDir1: String;
  vs2017BaseDir2: String;
  vs2017BaseDir3: String;

  vs2017Version1: String;
  vs2017Version2: String;
  vs2017Version3: String;
  
  VulcanPrgAssociation: Boolean;
  VulcanGuid : String;
  HelpViewer22Installed : Boolean;
  HelpViewer22Dir : String;
  OurHelp22Installed: Boolean;
  HelpViewer23Installed : Boolean;
  HelpViewer23Dir : String;
  OurHelp23Installed: Boolean;
  OriginalTypeChange: TNotifyEvent;

procedure PrintButtonClick(Sender: TObject);
var ResultCode :integer;
begin
ExtractTemporaryFile('license.txt');
if not ShellExec('Print', ExpandConstant('{tmp}\license.txt'),
     '', '', SW_SHOW, ewNoWait, ResultCode) then
//if not ShellExec('', ExpandConstant('{tmp}\license.txt'),
//     '', '', SW_SHOW, ewNoWait, ResultCode) then
end;
{
VS2017.txt will contain:
InstanceId: 8aa9303a (Complete)
InstallationVersion: 15.0.25914.0 (4222126348959744)
InstallationPath: C:\Program Files (x86)\Microsoft Visual Studio\2017\Professional
Product: Microsoft.VisualStudio.Product.Professional
Workloads:
    Microsoft.VisualStudio.Workload.CoreEditor
    Microsoft.VisualStudio.Workload.ManagedDesktop
}

procedure TaskKill(FileName: String);
var
  ResultCode: Integer;
begin
    Log('Kill running task (when necessary):'+FileName);
    Exec(ExpandConstant('taskkill.exe'), '/f /im ' + '"' + FileName + '"', '', SW_HIDE,
     ewWaitUntilTerminated, ResultCode);
end;

procedure FindVS2017;
var FileContents : TArrayOfString;
  ResultCode: Integer;
  i: integer;
  j: integer;
  line: String;
  tokenLength: Integer;
  token: String;
begin
  Log('Detect 2017 start');
  ExtractTemporaryFile('SetupCheck2017.exe');
  if Exec(ExpandConstant('{tmp}\SetupCheck2017.exe'),'','',SW_HIDE, ewWaitUntilTerminated, ResultCode) then
  begin
      if ResultCode = 0 then
      begin
        if LoadStringsFromFile(ExpandConstant('{tmp}\vs2017.txt'), FileContents) then
        begin
            Log('========================================');
            Log('Installed versions of Visual Studio 2017');
            Log('========================================');
            for i:= 0 to GetArrayLength(FileContents)-1 do
            begin
               line := Trim( FileContents[i]);
               Log(line);
               token := 'InstanceId:';
               if Pos(token, line) > 0  then 
               begin
                  vs2017Count := vs2017Count + 1;
                  tokenLength := Length(token);
                  vs2017InstanceId := Trim(Copy(line, tokenLength+1, Length(line) - tokenLength));
                  j := Pos('(',vs2017InstanceId)
                  if  j > 0 then
                     vs2017InstanceId := trim(Copy(vs2017InstanceId, 1, j-1));
                  if vs2017Count = 1 then
                     vs2017InstanceId1 := vs2017InstanceId;
                  if vs2017Count = 2 then
                     vs2017InstanceId2 := vs2017InstanceId;
                  if vs2017Count = 3 then
                     vs2017InstanceId3 := vs2017InstanceId;

               end
               token := 'InstallationPath:';
               if Pos(token, line) > 0  then 
                begin
                  tokenLength := Length(token);
                  vs2017BaseDir := Trim(Copy(line, tokenLength+1, Length(line) - tokenLength));
                  if vs2017Count = 1 then
                     vs2017BaseDir1 := vs2017BaseDir;
                  if vs2017Count = 2 then
                     vs2017BaseDir2 := vs2017BaseDir;
                  if vs2017Count = 3 then
                     vs2017BaseDir3 := vs2017BaseDir;
                  vs2017Installed := true;
                end
               token := 'Product:';
               if Pos(token, line) > 0 then
                begin
                  tokenLength := Length(token);
                  vs2017Version := ExtractFileExt(line);
                  vs2017Version := Copy(vs2017Version,2,Length(vs2017Version)-1);
                  if vs2017Count = 1 then
                     vs2017Version1 := vs2017Version;
                  if vs2017Count = 2 then
                     vs2017Version2 := vs2017Version;
                  if vs2017Count = 3 then
                     vs2017Version3 := vs2017Version;
                  vs2017Installed := true;
                end
            end
        end
      end

      Log('Vs 2017 # of installations: '+IntToStr(vs2017Count));
      if vs2017Count > 0 then
      begin
        Log('Vs 2017 instanceId 1      : '+vs2017InstanceId1);
        Log('Vs 2017 installation dir 1: '+vs2017BaseDir1);
        Log('Vs 2017 product version 1 : '+vs2017Version1);
      end
      if vs2017Count > 1 then
      begin
        Log('Vs 2017 instanceId 2      : '+vs2017InstanceId2);
        Log('Vs 2017 installation dir 2: '+vs2017BaseDir2);
        Log('Vs 2017 product version 2 : '+vs2017Version2);
      end
      if vs2017Count > 2 then
      begin
        Log('Vs 2017 instanceId 3      : '+vs2017InstanceId3);
        Log('Vs 2017 installation dir 3: '+vs2017BaseDir3);
        Log('Vs 2017 product version  3: '+vs2017Version3);
      end
  end
  Log('Detect 2017 end');
end;


procedure DetectVS();
var temp : String;
begin
  Log('Start VS Detection');
  VulcanInstalled := RegQueryStringValue(HKEY_LOCAL_MACHINE,'SOFTWARE\Grafx\Vulcan.NET','InstallPath',VulcanBaseDir) ;
  Vs2015Installed := RegQueryStringValue(HKEY_LOCAL_MACHINE,'SOFTWARE\Microsoft\VisualStudio\SxS\VS7','14.0',Vs2015BaseDir) ;
  if Vs2015Installed then Vs2015Path := Vs2015BaseDir+'Common7\Ide\';
  FindVS2017;

  HelpViewer22Installed := RegQueryStringValue(HKEY_LOCAL_MACHINE,'SOFTWARE\Microsoft\Help\v2.2','AppRoot',HelpViewer22Dir) ;
  HelpViewer23Installed := RegQueryStringValue(HKEY_LOCAL_MACHINE,'SOFTWARE\Microsoft\Help\v2.3','AppRoot',HelpViewer23Dir) ;
{
  if Vs2017Installed and Not HelpViewer23Installed then
  begin
    if MsgBox('Visual Studio 2017 has been detected, but not the VS 2017 help viewer. To install the VS 2017 help you need to install the HelpViewer component. Exit installation?', mbConfirmation, MB_YESNO) = IDYES then
    begin
    CancelSetup := true;
    end
  end
 }
  VulcanPrgAssociation := false;
  if Vs2015Installed then
  begin
  VulcanPrgAssociation := RegQueryStringValue(HKEY_LOCAL_MACHINE, '{#Vs14RegPath}\Languages\File Extensions\.prg', '', VulcanGuid) ;
  if VulcanPrgAssociation then 
      begin
      VulcanPrgAssociation := (UpperCase(VulcanGuid) = '{8D3F6D25-C81C-4FD8-9599-2F72B5D4B0C9}');
      end
  end
  OurHelp22Installed := RegQueryStringValue(HKEY_LOCAL_MACHINE,'Software\{#RegCompany}\{#Product}','Help22Installed',temp) ;
  OurHelp23Installed := RegQueryStringValue(HKEY_LOCAL_MACHINE,'Software\{#RegCompany}\{#Product}','Help23Installed',temp) ;
  Log('VS2015: '+Vs2015BaseDir); 
  Log('VS2017: '+Vs2017BaseDir);
 

  Log('End VS Detection');
end;


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

function vs2017IsInstalled: Boolean;
begin
  result := vs2017Installed;
end;

function GetVs2015IdeDir(Param: String): String;
begin
  result := Vs2015Path;
end;

function Getvs2017IdeDir(Param: String): String;
begin
  result := vs2017Path;
end;

{ get instance id so we can delete the proper folder in appdata\local }
function GetVs2017InstanceId(Param: String): String;
begin
  case vs2017VersionPage.SelectedValueIndex of
    0: result := Vs2017InstanceId1;
    1: result := Vs2017InstanceId2;
    2: result := Vs2017InstanceId3;
  end;
end;

function Getvs2017Version(Param: String): String;
begin
  case vs2017VersionPage.SelectedValueIndex of
    0: result := Vs2017Version1;
    1: result := Vs2017Version2;
    2: result := Vs2017Version3;
  end;
end;


function Getvs2017BaseDir(Param: String): String;
begin
  case vs2017VersionPage.SelectedValueIndex of
    0: result := Vs2017BaseDir1;
    1: result := Vs2017BaseDir2;
    2: result := Vs2017BaseDir3;
  end;
end;

function GetHelp22Dir(Param: String): String;
begin
  result := HelpViewer22Dir;
end;

function GetHelp23Dir(Param: String): String;
begin
  result := HelpViewer23Dir;
end;

Procedure Checkvs2017Help();
var
  VsHelpItem: Integer;

begin
  if vs2015Installed and Vs2017Installed then
     vsHelpItem := 4
  else
     vsHelpItem := 2;
   
  if Vs2017Installed and Not HelpViewer23Installed then
  begin
     WizardForm.ComponentsList.ItemCaption[VsHelpItem] := 'Cannot install the VS 2017 documentation because the Help Viewer component is not installed';
     WizardForm.ComponentsList.ItemEnabled[VsHelpItem] := false;
     WizardForm.ComponentsList.Checked[VsHelpItem] := false;
  end

end;


procedure DeleteOldFiles(folder: integer);
var
  FindRec: TFindRec;
  FolderName: String;
  Ok : Boolean;
  i: Integer;
begin
  if folder = 2015 then 
  begin
      FolderName := GetVs2015IdeDir('');
      Ok := true;
  end
  else if folder = 2017 then 
  begin
     FolderName := GetVs2017IdeDir('');
     Ok := true;
  end
  if Ok then begin
    for i := 1 to 2 do
    begin
      if i = 2 then FolderName := FolderName+'PrivateAssemblies\';
      Log('Delete old files in folder '+FolderName);
      if FindFirst(FolderName+'XSharp*.dll', FindRec) then begin
        try
          repeat
            Log('Delete old file '+FindRec.Name);
            DeleteFile(FolderName+FindRec.Name)
          until not FindNext(FindRec);
        finally
          FindClose(FindRec);
        end;
      end;
      if FindFirst(FolderName+'XSharp*.pdb', FindRec) then begin
        try
          repeat
            Log('Delete old file '+FindRec.Name);
            DeleteFile(FolderName+FindRec.Name)
          until not FindNext(FindRec);
        finally
          FindClose(FindRec);
        end;
      end;
    end;
  end;
end;



Procedure CurPageChanged(CurPage: Integer);

begin
  PrintButton.Visible := CurPage = wpLicense;
  if vs2017Installed then 
  begin
    Vs2017BaseDir := Getvs2017BaseDir('');
    Vs2017Version := Getvs2017Version('');
    vs2017VersionPage.CheckListBox.ItemCaption[0] := Vs2017Version1+' in ' + Vs2017BaseDir1;
    vs2017VersionPage.CheckListBox.ItemCaption[1] := Vs2017Version2+' in ' + Vs2017BaseDir2;
    vs2017Path    := vs2017BaseDir+'\Common7\Ide\';
  end
  if vs2017Count > 2 then
    begin
    vs2017VersionPage.CheckListBox.ItemCaption[2] := Vs2017Version3+' in ' + Vs2017BaseDir3;
    vs2017VersionPage.CheckListBox.ItemEnabled[2] := true;
    end
  else
    begin
    vs2017VersionPage.CheckListBox.ItemCaption[2] := '';
    vs2017VersionPage.CheckListBox.ItemEnabled[2] := false;
    end
  Checkvs2017Help;
end;

procedure RegisterPreviousData(PreviousDataKey: Integer);
var
  Version: String;
begin
  { Store the settings so we can restore them next time }
  case vs2017VersionPage.SelectedValueIndex of
    0: Version := '1';
    1: Version:= '2';
    2: Version := '3';
  end;
  SetPreviousData(PreviousDataKey, 'VS2017Version', Version);
end;

procedure TypeChange (Sender: TObject);
begin
OriginalTypeChange(Sender);
Checkvs2017Help;
end;

procedure InitializeWizard();
begin
    
    Log('InitializeWizard start');
    TaskKill('xscompiler.exe');
    vs2017VersionPage := CreateInputOptionPage(wpSelectComponents,
                'Visual Studio 2017', 
                'Please select in which Visual Studio 2017 instance the program must be installed',
                'Select the Visual Studio 2017 Instance',True, False);
    vs2017VersionPage.Add('Option 1');
    vs2017VersionPage.Add('Option 2');
    vs2017VersionPage.Add('Option 3');
    vs2017VersionPage.Values[0] := true;
    PrintButton := TButton.Create(WizardForm);
    PrintButton.Caption := '&Print...';
    PrintButton.Left := WizardForm.InfoAfterPage.Left + 96;
    PrintButton.Top := WizardForm.InfoAfterPage.Height + 88;
    PrintButton.OnClick := @PrintButtonClick;
    PrintButton.Parent := WizardForm.NextButton.Parent;
  case GetPreviousData('VS2017Version', '') of
    '1': vs2017VersionPage.SelectedValueIndex := 0;
    '2': vs2017VersionPage.SelectedValueIndex := 1;
    '3': vs2017VersionPage.SelectedValueIndex := 2;
  else
    vs2017VersionPage.SelectedValueIndex := 0;
  end;
  OriginalTypeChange := WizardForm.TypesCombo.OnChange ;
  WizardForm.TypesCombo.OnChange := @TypeChange;
  Log('InitializeWizard end');
end;

function ShouldSkipPage(PageID: Integer): Boolean;
begin
  {Log('ShouldSkipPage start');}
  { Skip pages that shouldn't be shown }
  Result := False;
    if (PageId = vs2017VersionPage.ID)   then 
    begin
      if (vs2017Count < 2 ) then
        Result := True
      if not IsComponentSelected('vs2017') then
        Result := True
    end
  {Log('ShouldSkipPage stop');}
end;

function InitializeSetup(): Boolean;
var
  ErrorCode: Integer;
begin
  CancelSetup := false;
  Log('InitializeSetup start');
  DetectVS();
  result := not CancelSetup ;
  if result and not Vs2015Installed and not vs2017Installed then
  begin
    if MsgBox('Visual Studio 2015 has not been detected, do you want to download the free Visual Studio Community Edition ?', mbConfirmation, MB_YESNO) = IDYES then
    begin
    ShellExec('open','https://www.visualstudio.com/en-us/downloads/download-visual-studio-vs.aspx','','',SW_SHOW,ewWaitUntilIdle, ErrorCode);
    result := false;
    end
  end;
 
  { When more than 1 VS 2017 installation, ask for the instance}
  Log('InitializeSetup end');  
end;

function GetV4NetDir(version: string) : string;

var regkey, regval  : string;

begin

    // in case the target is 3.5, replace 'v4' with 'v3.5'
    // for other info, check out this link 
    // http://stackoverflow.com/questions/199080/how-to-detect-what-net-framework-versions-and-service-packs-are-installed
    regkey := 'SOFTWARE\Microsoft\NET Framework Setup\NDP\v4\Full'

    RegQueryStringValue(HKLM, regkey, 'InstallPath', regval);

    result := regval;
end; 


function GetV4Net64Dir(version: string) : string;

var regkey, regval  : string;

begin

    // in case the target is 3.5, replace 'v4' with 'v3.5'
    // for other info, check out this link 
    // http://stackoverflow.com/questions/199080/how-to-detect-what-net-framework-versions-and-service-packs-are-installed
    regkey := 'SOFTWARE\Microsoft\NET Framework Setup\NDP\v4\Full'

    RegQueryStringValue(HKLM, regkey, 'InstallPath', regval);

    StringChangeEx(regval, '\Framework', '\Framework64', false);
    result := regval;
end; 


function CreateNGenTask : Boolean;
var commands: string;
var ngenpath: string;
begin
      ngenpath := GetV4NetDir('')+'ngen.exe';
      commands := '';
      commands := commands + ngenpath + ' install "' +ExpandConstant('{app}\bin\xsc.exe')+'"' +#13+#10;
      commands := commands + ngenpath + ' install "' +ExpandConstant('{app}\bin\xsi.exe') +'"'+#13+#10;
      commands := commands + ngenpath + ' install "' +ExpandConstant('{app}\bin\xscompiler.exe') +'"'+#13+#10;
      ngenpath := GetV4Net64Dir('')+'ngen.exe';
      if FileExists(ngenpath) then 
      begin
        commands := commands + ngenpath + ' install "' +ExpandConstant('{app}\bin\xsc.exe')+'"' +#13+#10;
        commands := commands + ngenpath + ' install "' +ExpandConstant('{app}\bin\xsi.exe') +'"'+#13+#10;
        commands := commands + ngenpath + ' install "' +ExpandConstant('{app}\bin\xscompiler.exe') +'"'+#13+#10;
      end
      SaveStringToFile( ExpandConstant('{app}\uninst\instngen.cmd '), commands, False);
      StringChangeEx(commands, ' install ', ' uninstall ', false);
      SaveStringToFile( ExpandConstant('{app}\uninst\uninstngen.cmd '), commands, False);
      result := true;
end;


{ Code to run when uninstalling }
function InitializeUninstall(): Boolean;
begin
  TaskKill('xscompiler.exe');
  result := true;
end;


#expr SaveToFile(AddBackslash(SourcePath) + "Preprocessed.iss")