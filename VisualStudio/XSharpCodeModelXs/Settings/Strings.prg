// Strings.prg
// Created by    : robert
// Creation Date : 1/23/2023 1:00:15 PM
// Created for   :
// WorkStation   : NYX


USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharpModel
class DebugPropertyPagePanel
    const catGeneral := "General" as string
    const catSpecial := "Special" as string
    const captOutputPath := "Output Path" as string
    const descOutputPath := "Output Path (macros are allowed)" as string
    const captDebugType := "Generate Debug Information" as string
    const descDebugType := "Generate Debug Information (none, full, pdbonly)" as string
    const captDebuggerCommand := "Command" as string
    const descDebuggerCommand := "The debug command to execute" as string
    const captDebuggerCommandArguments := "Command Arguments" as string
    const descDebuggerCommandArguments := "The command line arguments to pass to the application" as string
    const captDebuggerWorkingDirectory := "Working Directory" as string
    const descDebuggerWorkingDirectory := "The application's working directory. By default, the directory containing the project file." as string
    const captDebuggerAttach := "Attach" as string
    const descDebuggerAttach := "Specifies whether the debugger should attempt to attach to an existing process when debugging starts." as string
    const captEnableUnmanagedDebugging := "Enable unmanaged debugging" as string
    const descEnableUnmanagedDebugging := "Enable unmanaged debugging" as string
    const captUseVSHostingProcess := "Enable the Visual Studio Hosting Process" as string
    const descUseVSHostingProcess := "Enable the Visual Studio Hosting Process" as string
end class
class DialectPropertyPagePanel
    const DialectCaption := "Dialect" as string
    const VO1Caption := "Allow Init() and Axit() as aliases for Constructor/Destructor" as string
    const VO2Caption := "Initialize strings" as string
    const VO3Caption := "All instance methods virtual" as string
    const VO4Caption := "Implicit numeric conversions" as string
    const VO5Caption := "Implicit Clipper calling convention" as string
    const VO6Caption := "Implicit pointer conversions" as string
    const VO7Caption := "Implicit casts and conversions" as string
    const VO8Caption := "Compatible preprocessor" as string
    const VO9Caption := "Handle problems with incorrect or missing return statements" as string
    const VO10Caption := "Compatible IIF Behavior" as string
    const VO11Caption := "Compatible numeric conversions" as string
    const VO12Caption := "Clipper Compatible integer divisions" as string
    const VO13Caption := "Compatible string comparisons" as string
    const VO14Caption := "Use FLOAT literals" as string
    const VO15Caption := "Treat missing types as USUAL" as string
    const VO16Caption := "Generate Clipper constructors" as string
    const VO17Caption := "Compatible BEGIN SEQUENCE .. END SEQUENCE " as string
    const XPP1Caption := "Inherit from Abstract class" as string
    const FOX1Caption := "Inherit from Custom class" as string
    const FOX2Caption := "Compatible Array Handling" as string
    const VO1Description := "Allow Init() and Axit() as aliases for Constructor/Destructor (/vo1)" as string
    const VO2Description := e"Initialize strings to empty string (String.Empty) ( /vo2). Please note that in .NET a NULL_STRING is not the same as a string with length 0. \rWhen enabled this will initialize local string variables regardless of the setting of 'initialize locals' setting from the Language page." as string
    const VO3Description := "Add the virtual modifier to all methods by default (which is the normal Visual Objects behavior) (/vo3)" as string
    const VO4Description := "Implicit conversions between numeric values, such as when assigning a DWORD to an INT or assigning a FLOAT to an INT (/vo4)" as string
    const VO5Description := e"Methods without parameters and calling convention are compiled as Clipper calling convention (/vo5). \nPlease note that without this switch all methods without parameters will be seen as STRICT. \rMethods with untyped parameters are always seen as CLIPPER calling convention." as string
    const VO6Description := "Implicit conversions between typed function PTR and PTR (/vo6)" as string
    const VO7Description := "Compatible implicit casts and Conversions (/vo7)" as string
    const VO8Description := "Makes the preprocessor case insensitive and also controls how #ifdef inspects #defines (/vo8)" as string
    const VO9Description := "Allow missing return statements or allow return statements with incorrect return values (/vo9)" as string
    const VO10Description := "Compatible IIF Behavior, allow different types of return values in TRUE and FALSE expression (/vo10)" as string
    const VO11Description := "Compatible arithmetic conversions  (/vo11)" as string
    const VO12Description := "Compatible integer divisions, integer divisions may return a float  (/vo12)" as string
    const VO13Description := "Compatible string comparisons, respects SetExact and collation table (/vo13)" as string
    const VO14Description := "Store floating point literals as FLOAT and not as System.Double (REAL8)  (/vo14)" as string
    const VO15Description := e"Missing type clauses for locals, instance variables and parameters are treated as USUAL (VO and Vulcan dialect). \rThe default = TRUE for the VO dialect and FALSE for the other dialects. \rWe strongly recommend to set this to FALSE because this will help you to find problems in your code and non optimal code. \rIf you have to use the USUAL type we recommend to explicitly declare variables and parameters as USUAL (/vo15)" as string
    const VO16Description := "Automatically create clipper calling convention constructors for classes without constructor where the parent class has a Clipper Calling convention constructor.(/vo16)" as string
    const VO17Description := "Generate code to fully implement the VO compatible BEGIN SEQUENCE .. END SEQUENCE. The compiler generates calls to the runtime functions _SequenceError and _SequenceRecover that you may override in your own code.(/vo17)" as string
    const XPP1Description := "All classes without parent class inherit from the XPP Abstract class.(/xpp1)" as string
    const FOX1Description := "All classes are assumed to inherit from the Custom class. This also affects the way in which properties are processed by the compiler.(/fox1)" as string
    const FOX2Description := "FoxPro compatible array handling (Allows parenthesized arrays and assigning a single value to an array to fill all elements). WARNING Allowing parenthesized arrays may slow down the execution of your program !(/fox2)" as string
    const CatCompatibility := "All dialects" as string
    const CatNotCore := "Not in Core dialect" as string
    const XPPCompatibility := "Xbase++ Compatibility" as string
    const FOXCompatibility := "Visual FoxPro Compatibility" as string
end class

class GeneralPropertyPagePanel
    const DefaultValue := "<default>" as string
    const captVulcanCompatibleResouces := "Vulcan Compatible Managed Resources" as string
    const descVulcanCompatibleResouces := "Use Vulcan Compatible Managed Resources (when 'True' then resources files are included in the assembly without namespace prefix. When 'False' then the resource files are prefixed with the namespace of the app, just like in other .Net languages, such as C#)" as string
    const captDialect := "Dialect" as string
    const descDialect := "Select the compiler dialect to use when compiling this project. Changing the dialect may also change the 'Allow NamedArguments' setting on the Language page." as string
    const captWin32Manifest := "Suppress default Win32 manifest" as string
    const descWin32Manifest := "Suppress default Win32 manifest. You will have to supply your own Win32 manifest if you suppress the default one. (/nowin32manifest)" as string
    const catResources := "Resources" as string
    const captUseNativeVersion := "Prefer native version resource over managed version resource" as string
    const descUseNativeVersion := "When your application includes a native version resource, use this native version resource and do not generate a resource based on the global assembly properties such as AssemblyTitle, AssemblyVersion etc (/usenativeversion)" as string
    const captPreferNative := "Prefer native version resource info over managed version info" as string
    const descPreferNative := "Prefer native version resource over managed version info, The default behavior is to create a version resource based in the various [Assembly..] attributes." as string
    const captBindingRedirects := "Auto-generate binding redirects" as string
    const descBindingRedirects := "The binding redirects are added to the output configuration (app.config) file when the app is compiled." as string
    const captStartup := "Startup object:" as string
    const captOutputType := "Output Type:" as string
    const captTargetFramework := "Target Framework:" as string
    const captAppName := "Application Name" as string
    const captNamespace := "Default Namespace:" as string
    const descNamespace := "Specifies the base namespace for files added to the project." as string
    const descAssembly := "Specifies the name of the output file that will hold the assembly manifest." as string
    const descFramework := "Specifies the version of .NET that the application targets. This option can have different values depending on which versions of .NET are installed on your computer." as string
    const descOutputType := "Specifies the type of application to build." as string
    const descStartup := "Defines the entry point to be called when the application loads. Generally this is set either to the main form in your application or to the 'Start' function that should run when the application starts. Class libraries do not define an entry point." as string
    const descIcon := "Sets the .ico file that you want to use as your program icon. Note you must specify the icon and manifest -or- a resource file." as string
    const captIcon := "Application Icon:" as string
end class

class LanguagePropertyPagePanel
    const LanguageCaption := "Language" as string
    const CMDCaption := "Extra Command Line Options" as string
    const AZCaption := "Use Zero Based Arrays" as string
    const CSCaption := "Case Sensitive" as string
    const INSCaption := "Enable Implicit Namespace lookup" as string
    const LBCaption := "Allow Late Binding" as string
    const NamedArgCaption := "Allow Named Arguments" as string
    const NSCaption := "Prefix classes with default Namespace" as string
    const OVFCaption := "Overflow Exceptions" as string
    const UnsafeCaption := "Allow Unsafe Code" as string
    const MemVarCaption := "Enable Memvar support" as string
    const UndeclaredCaption := "Enable Undeclared variables support" as string
    const InitLocalsCaption := "Initialize Local variables" as string
    const enforceSelfCaption := "Enforce SELF" as string
    const EnforceOverrideCaption := "Enforce VIRTUAL / OVERRIDE" as string
    const allowDotCaption := "Allow DOT for instance members" as string
    const allowOldStyleCaption := "Allow Old Style assignments" as string
    const CSDescription := "Enable/Disable case sensitivity (/cs)" as string
    const AZDescription := "Use Zero Based Arrays (/az)" as string
    const INSDescription := "Enable the implicit lookup of classes defined in assemblies with an Implicit Namespace attribute (/ins)" as string
    const LBDescription := "Allow property access and method calls on expressions of type OBJECT and USUAL (/lb)" as string
    const NamedArgDescription := "Allow named arguments (Default = FALSE for the Core dialect and TRUE for the other dialects). Changing the dialect may also automatically change this setting. (/namedargs)" as string
    const NSDescription := "Prefix all classes that do not have a namespace prefix and are not in a begin namespace ... end namespace block with the namespace of the assembly (/ns:<Namespace>)" as string
    const OVFDescription := "Check for Overflow and Underflow for numeric expressions, like the CHECKED keyword. (/ovf)" as string
    const UnsafeDescription := "Allow Unsafe code inside this assembly (/unsafe)" as string
    const InitLocalsDescription := "Automatically initialize local variables without initialization expression. Please note that for locals of type string the initial value will depend on the 'Initialize strings' setting from the Dialect page.(/initlocals)" as string
    const NoStdDefCaption := "Suppress standard header file" as string
    const NoStdDefDescription := "Suppress inclusion of the standard header file (XSharpDefs.xh) in every file (/nostddef)" as string
    const INCCaption := "Additional Include paths" as string
    const INCDescription := "Additional include paths for the preprocessor (it also looks through the folders set with the include environment variable) (/i)" as string
    const StdDefCaption := "Alternate standard header file" as string
    const StdDefDescription := "Name of an alternative standard header file (alternative for XSharpDefs.xh)  (/stddefs)" as string
    const MemVarDescription := e"Enable support for memory variables (MEMVAR, PUBLIC, PRIVATE & PARAMETERS). (/memvar)\rPlease note that this is NOT supported for the Core and Vulcan dialects" as string
    const UndeclaredDescription := e"Enable support for undeclared variables (these are resolved to MEMVARs). (/undeclared)\rPlease note that this requires /memvar to be enabled as well." as string
    const EnforceSelfDescription := "Enforce the use of SELF inside members of a class. This helps to prevent ambiguity in your code (/enforceself)" as string
    const EnforceOverrideDescription := "Enforce the use of VIRTUAL and OVERRIDE for members of a class. (/enforceoverride)" as string
    const allowDotDescription := "Allow the DOT operator to access instance fields, properties and methods (/allowdot)" as string
    const allowOldStyleDescription := "Allow old style assignments with the '=' operator. This defaults to TRUE for the FoxPro dialect. (/allowoldstyleassignments)" as string
    const CatGeneral := "General" as string
    const CatNamespaces := "Namespaces" as string
    const CatPreprocessor := "Preprocessor" as string
    const CatMemVars := "Memory Variables" as string
end class
class BuildPropertyPagePanel
    const catSigning := "Code Signing" as string
    const catMisc := "Miscellaneous" as string
    const catWarnings := "Warnings" as string
    const catOutput := e"\tOutput" as string
    const CatPreprocessor := "Preprocessor" as string
    const catXML := "XML Output" as string
    const captOutputPath := "Output Path" as string
    const descOutputPath := "Output Path (macros are allowed)" as string
    const captIntermediateOutputPath := "Intermediate Output Path" as string
    const descIntermediateOutputPath := "Intermediate Output Path  (macros are allowed)" as string
    const captDocumentationFile := "Generate XML doc comments file" as string
    const descDocumentationFile := "Generate XML doc comments file" as string
    const captDocumentationFileName := "XML doc comments file name" as string
    const descDocumentationFileName := "XML doc comments file name" as string
    const captOptimize := "Optimize" as string
    const descOptimize := "Should compiler optimize output? (/optimize)" as string
    const captUseSharedCompilation := "Use Shared Compiler" as string
    const descUseSharedCompilation := "Should the shared compiler be used to compile the project? (Faster, but may hide some compiler errors) (/shared)" as string
    const captDisabledWarnings := "Suppress Specific Warnings" as string
    const descDisabledWarnings := "Specify a list of warnings to suppress (/nowarn)" as string
    const captWarningLevel := "Warning Level" as string
    const descWarningLevel := "Set the warning level to a value between 0 and 4 (/warn)" as string
    const captTreatWarningsAsErrors := "Warnings As Errors" as string
    const descTreatWarningsAsErrors := "Treat warnings as errors (/warnaserror)" as string
    const captSignAssembly := "Sign the output assembly" as string
    const descSignAssembly := "Sign the assembly  (/keyfile)" as string
    const captDelaySign := "Delayed sign only" as string
    const descDelaySign := "Delayed signing (/delaysign)" as string
    const captAssemblyOriginatorKeyFile := "Code Signing KeyFile" as string
    const descAssemblyOriginatorKeyFile := "Choose a code signing key file (/keyfile)" as string
    const captRegisterForComInterop := "Register for COM Interop" as string
    const descRegisterForComInterop := "Register the output assembly for COM Interop (requires administrator rights)" as string
    const PPOCaption := "Generate preprocessor output" as string
    const PPODescription := "Save the output from the preprocessor to .ppo files  (/ppo)" as string
    const CmdLineCaption := "Extra Command Line Options" as string
    const CmdLineDescription := "User-Defined Command Line options" as string
    const DefCaption := "Defines for the preprocessor" as string
    const DefDescription := "Defines for the preprocessor (/define)" as string
    const captPrefer32Bit := e"\tPrefer 32 Bit" as string
    const descPrefer32Bit := "Prefer 32 bit when AnyCpu platform is selected. (/platform)" as string
    const SuppressRCWarningsCaption := "Suppress Resource Compiler warnings" as string
    const SuppressRCWarningsDescription := "Suppress warnings from the Native Resource Compiler about duplicate defines (RC4005)" as string
    const captPlatFormTarget := "Platform Target" as string
    const descPlatFormTarget := "Select the platform target when compiling this project. This should be AnyCPU, X86, x64,Arm or Itanium (/platform)" as string
    const defaultOutputPath := "bin\\$(Configuration)\\" as string
    const defaultIntermediatePath := "obj\\$(Configuration)\\" as string
    const descSpecificWarnings := "Specific Warnings To Treat As Errors" as string
end class


END NAMESPACE // XSharpModel.Constants
