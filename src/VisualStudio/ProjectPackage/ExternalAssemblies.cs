// The following lines ensure that the right versions of the various DLLs are loaded.
// They will be included in the generated PkgDef folder for the project system
using Microsoft.VisualStudio.Shell;

#if DEV17
[assembly: ProvideCodeBase(AssemblyName = "XSharp.AppDesigner2022")]
[assembly: ProvideCodeBase(AssemblyName = "XSharp.LanguageService2022")]
[assembly: ProvideCodeBase(AssemblyName = "XSharp.ProjectBase2022")]
[assembly: ProvideCodeBase(AssemblyName = "XSharp.Debugger2022")]
[assembly: ProvideCodeBase(AssemblyName = "XSharp.CodeDomProvider2022")]
#else
[assembly: ProvideCodeBase(AssemblyName = "XSharp.CodeDomProvider")]
[assembly: ProvideCodeBase(AssemblyName = "XSharp.AppDesigner")]
[assembly: ProvideCodeBase(AssemblyName = "XSharp.LanguageService")]
[assembly: ProvideCodeBase(AssemblyName = "XSharp.ProjectBase")]
[assembly: ProvideCodeBase(AssemblyName = "XSharp.Debugger")]
#endif
[assembly: ProvideCodeBase(AssemblyName = "XSharp.CodeAnalysis")]
[assembly: ProvideCodeBase(AssemblyName = "XSharp.CodeModel")]
[assembly: ProvideCodeBase(AssemblyName = "XSharp.MonoCecil")]
[assembly: ProvideCodeBase(AssemblyName = "XSharp.Evaluator")]
[assembly: ProvideCodeBase(AssemblyName = "XSharp.VsParser")]

[assembly: ProvideCodeBase(AssemblyName = "Community.VisualStudio.Toolkit")]
[assembly: ProvideCodeBase(AssemblyName = "Microsoft.DiaSymReader")]
[assembly: ProvideCodeBase(AssemblyName = "System.Reflection.MetaData")]
#if DEV17
[assembly: ProvideCodeBase(AssemblyName = "Microsoft.Data.Sqlite")]
[assembly: ProvideCodeBase(AssemblyName = "SQLitePCLRaw.core")]
[assembly: ProvideCodeBase(AssemblyName = "SQLitePCLRaw.batteries_v2")]
[assembly: ProvideCodeBase(AssemblyName = "SQLitePCLRaw.provider.dynamic_cdecl")]
#endif
[assembly: ProvideCodeBase(AssemblyName = "System.Data.SQLite")]

[assembly: ProvideCodeBase(AssemblyName = "Serilog.Sinks.File")]
[assembly: ProvideCodeBase(AssemblyName = "Serilog.Sinks.Debug")]
[assembly: ProvideCodeBase(AssemblyName = "Serilog")]


